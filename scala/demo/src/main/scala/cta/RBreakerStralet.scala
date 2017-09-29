package cta

import xtz.tquant.api.scala.DataApi.{Bar, MarketQuote}
import xtz.tquant.api.scala.TradeApi.{Order, Trade}
import xtz.tquant.api.scala.{DataApi, TradeApi}
import xtz.tquant.stra.stralet.{Stralet, StraletContext}
import xtz.tquant.stra.utils.CsvHelper

import scala.io.Source


// https://uqer.io/ R-Breaker策略
//
// R-Breaker 是一种短线日内交易策略，它结合了趋势和反转两种交易方式。该策略也长期被Future Thruth 杂志评为最赚钱的策略之一，尤其在标普
// 500 股指期货上效果最佳。该策略的主要特点如下：
//
// 第一、根据前一个交易日的收盘价、最高价和最低价数据通过一定方式计算出六个价位，从大到小依次为突破买入价、观察卖出价、反转卖出价、反转买
// 入价、观察买入价和突破卖出价，以此来形成当前交易日盘中交易的触发条件。通过对计算方式的调整，可以调节六个价格间的距离，进一步改变触发条
// 件。
//
// 第二、根据盘中价格走势，实时判断触发条件，具体条件如下：
// 1) 当日内最高价超过观察卖出价后，盘中价格出现回落，且进一步跌破反转卖出价构成的支撑线时，采取反转策略，即在该点位（反手、开仓）做空；
// 2) 当日内最低价低于观察买入价后，盘中价格出现反弹，且进一步超过反转买入价构成的阻力线时，采取反转策略，即在该点位（反手、开仓）做多；
// 3) 在空仓的情况下，如果盘中价格超过突破买入价，则采取趋势策略，即在该点位开仓做多；
// 4) 在空仓的情况下，如果盘中价格跌破突破卖出价，则采取趋势策略，即在该点位开仓做空。
//
// 第三、设定止损以及止盈条件；
//
// 第四、设定过滤条件；
//
// 第五、在每日收盘前，对所持合约进行平仓。
//
// 具体来看，这六个价位形成的阻力和支撑位计算过程如下：
//
// 观察卖出价 = High + 0.35 * (Close – Low)
// 观察买入价 = Low – 0.35 * (High – Close)
// 反转卖出价 = 1.07 / 2 * (High + Low) – 0.07 * Low
// 反转买入价 = 1.07 / 2 * (High + Low) – 0.07 * High
// 突破买入价 = 观察卖出价 + 0.25 * (观察卖出价 – 观察买入价)
// 突破卖出价 = 观察买入价 – 0.25 * (观察卖出价 – 观察买入价)
// 其中，High、Close、Low 分别为昨日最高价、昨日收盘价和昨日最低价。这六个价位从大到小一次是，突破买入价、观察爱出价、反转卖出价、反转
// 买入价、观察买入价和突破卖出价。

class RBreakerStralet extends Stralet {

    var account = ""
    var trade_api : TradeApi = _
    var data_api  : DataApi = _

    var contract : String = _

    case class PriceRange(sell_setup : Double,
                          buy_setup  : Double,
                          sell_enter : Double,
                          buy_enter  : Double,
                          sell_break : Double,
                          buy_break  : Double
                    )

    var price_range : PriceRange = _
    var count_1 = 0
    var count_2 = 0

    override def onInit(sc: StraletContext): Unit = {
        super.onInit(sc)

        sc.log("onInit", sc.getTime)

        trade_api = sc.getTradeApi
        data_api  = sc.getDataApi

        this.account = sc.getParameters[String]("account", "")


        // 从配置中得到要交易的商品期货，然后从主力合约映射表中得到今日交易的合约

        val code = sc.getParameters[String]("code", "")
        assert( code.nonEmpty, "no future  in config")

        contract =
            if (sc.mode == "realtime") {
                "rb1801.SHF"
            } else {
                code
            }

        sc.log("trade contract: " + contract)

        data_api.subscribe( Array(contract ))

        val (daily_bar, msg) = data_api.bar(contract, "1d")
        assert(daily_bar != null, "can't get bar1d: " + msg)
        assert(daily_bar.last.date < sc.getTradingDay)

        // 从上个交易日价格计算出今天的价格区间
        val last_day = daily_bar.last
        price_range = calcPriceRange(last_day.high, last_day.low, last_day.close)

        sc.log(price_range)
    }

    override def onFini() = {
        sc.log("onFini", sc.getTime)

        val (cur_pos, _) = trade_api.queryPosition(this.account)
        val (long_size, short_size) = cur_pos.foldLeft( (0L, 0L) ) { (v, x) =>
            if (x.side == "Long")
                ( v._1 + x.current_size, v._2)
            else
                (v._1 , v._2 + x.current_size)
        }

        if (long_size != 0 || short_size != 0)
            sc.log(s"Error: should close all positions, $contract, $long_size, $short_size")
    }

    override def onQuote(q: MarketQuote): Unit = {
        //        sc.log("quote", q.code, q.date, q.time, sc.getTimeAsInt)
    }

    def calcPriceRange(high: Double, low : Double, close :Double) : PriceRange = {

        val high_beta  = 0.35
        val low_beta   = 0.25
        val enter_beta = 0.07

        val sell_setup = high + high_beta * (close - low) //# 观察卖出价
        val buy_setup  = low  - high_beta * (high  - close) // 观察买入价
        val sell_enter = (1 + enter_beta) / 2 * (high + low) - enter_beta * low // # 反转卖出价
        val buy_enter  = (1 + enter_beta) / 2 * (high + low) - enter_beta * high // # 反转买入价
        val sell_break = buy_setup  - low_beta * (sell_setup - buy_setup) // # 突破卖出价
        val buy_break  = sell_setup + low_beta * (sell_setup - buy_setup) //# 突破买入价

        PriceRange ( sell_setup = sell_setup, buy_setup = buy_setup,
            sell_enter = sell_enter, buy_enter = buy_enter,
            sell_break = sell_break, buy_break = buy_break)

    }

    def isFinished(order : Order): Boolean = {
        order.status match {
            case "Filled"    => true
            case "Cancelled" => true
            case "Rejected"  => true
            case _ => false
        }
    }

    def cancelUnfinshedOrder() : Unit = {
        val (orders, msg) = sc.getTradeApi.queryOrders(this.account)
        if (orders == null) {
            sc.log("ERROR: queryOrders failed: " + msg)
            return
        }

        val unfinished_orders = orders.filter( x => x.code == this.contract && !isFinished(x) )
        for ( ord <- unfinished_orders) {
            sc.getTradeApi.cancelOrder(this.account, code = this.contract, entrust_no = ord.entrust_no, order_id = ord.order_id)
        }
    }

    override def onBar(cycle : String, bar : Bar): Unit = {

        if (cycle != "1m") return
        if (bar.code != this.contract) return

        if (sc.mode == "realtime") sc.log(bar)

        if ( bar.time == 93100000) {

            // 从夜盘行情中取 high, low, close计算
            val (bars, msg) = data_api.bar(this.contract, cycle="1m", align = true);
            assert(bars != null)

            var high = 0.0
            var low = 100000000.0
            var close = 0.0
            for (b <- bars) {
                if (b.high > high) high = b.high
                if (b.low < low)   low  = b.low
                close = b.close
            }
            price_range = calcPriceRange(high, low, close)
            sc.log("night data", price_range)
            return
        }

        // 只交易日盘
        if (bar.time < 90000000 || bar.time > 150000000)
            return

        val trading_day = sc.getTradingDay
        val bars = {
            val (tmp, _) = data_api.bar(this.contract, "1m", align = true)
            tmp.filter( x => x.date == trading_day && x.time > 90000000 )
        }

        if (bars.length < 2) return

        val bar_2 = bars(bars.length-2)
        val bar_1 = bars.last

        val cur_pos = trade_api.queryPosition(this.account)._1.filter( _.code == this.contract)
        val (long_size, short_size) = cur_pos.foldLeft( (0L, 0L) ) { (v, x) =>
            if (x.side == "Long")
                ( v._1 + x.current_size, v._2)
            else
                (v._1 , v._2 + x.current_size)
        }


        val last_price = data_api.quote(contract)._1.last

        if (bar.time >= 145500000) {
            if (long_size != 0)
                trade_api.placeOrder(this.account, contract, last_price, long_size, "Sell")
            if (short_size != 0)
                trade_api.placeOrder(this.account, contract, last_price, short_size, "Cover")

            return
        }
        // 趋势
        if (bar_2.close <= price_range.buy_break && bar_1.close > price_range.buy_break) {
            if (long_size == 0)
                trade_api.placeOrder(this.account, contract, last_price, 1, "Buy")

            if (short_size != 0)
                trade_api.placeOrder(this.account, contract, last_price, short_size, "Cover")
        }

        if (bar_2.close >= price_range.sell_break && bar_1.close < price_range.sell_break) {
            if (short_size == 0)
                trade_api.placeOrder(this.account, contract, last_price, 1, "Short")

            if (long_size != 0)
                trade_api.placeOrder(this.account, contract, last_price, long_size, "Sell")
        }

        // 反转
        //   多单反转
        if (bar_1.high > price_range.sell_setup && bar_1.close > price_range.sell_enter)
            count_1 = 1

        if (count_1 == 1 && bar_1.close < price_range.sell_enter) {
            if (long_size > 0) {
                trade_api.placeOrder(this.account, contract, last_price, long_size, "Sell" )
                trade_api.placeOrder(this.account, contract, last_price, 1, "Short")
            }
        }
        //   空单反转
        if (bar_1.low < price_range.buy_setup )
            count_2 = 1

        if (count_2 == 1 && bar_1.close > price_range.buy_enter) {
            if (short_size > 0) {
                trade_api.placeOrder(this.account, contract, last_price, short_size, "Cover" )
                trade_api.placeOrder(this.account, contract, last_price, 1, "Buy")
            }
        }
    }

    override def onOrderStatus(order: Order): Unit = {
        sc.log("onOrder: " + order)
    }

    override def onOrderTrade(trade: Trade): Unit = {
        sc.log("onTrade: " + trade)
    }

    override def onEvent(evt: String, data: Any): Unit = {
    }

    override def onTimer(id : Int, data : Any): Unit = {
        //        val (date, time) = sc.getTimeAsInt()
        //        sc.log(s"onCycle $date $time")
    }
}

object RBreakerStralet extends App {

    val stralet_config =
        """
          |{
          |  "stralet" : {
          |    "id": "RBreakerStralet",
          |    "stralet_class": "cta.RBreakerStralet",
          |
          |    "parameters": {
          |      "account": "simnow",
          |      "code"   : "rb.SHF"
          |    }
          |  }
          |}
        """.stripMargin

    val backtest_config =
        """
          |{
          |  "tqc" : {
          |    "addr"  : "tcp://127.0.0.1:10001"
          |  },
          |
          |  "backtest" : {
          |    "data_level" : "1m",
          |    "date_range" : [ 20170620, 20170922],
          |    "accounts"   : ["simnow"]
          |  }
          |}
        """.stripMargin

//    val realtime_config =
//        """
//          |{
//          |  "tqc" : {
//          |    "addr"  : "tcp://127.0.0.1:10001"
//          |  },
//          |
//          |  "options" : {
//          |    "sim_order_status" : true
//          |  }
//          |}
//        """.stripMargin

    if (args.isEmpty) {
        println("Usage: RBreakerStralet [backtest|realtime]")
        System.exit(0)
    }

    args(0) match {
        case "backtest" => xtz.tquant.stra.backtest.Run.runConf(stralet_config, backtest_config)

        case "realtime" => xtz.tquant.stra.realtime.Run.runConf(stralet_config, "")//realtime_config)
    }

}
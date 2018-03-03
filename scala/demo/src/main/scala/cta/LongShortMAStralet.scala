package cta

import com.acqusta.tquant.api.scala.DataApi.{Bar, MarketQuote}
import com.acqusta.tquant.api.scala.TradeApi.{Order, Trade}
import com.acqusta.tquant.api.scala.{DataApi, TradeApi}
import com.acqusta.tquant.stra.realtime.Run
import com.acqusta.tquant.stra.stralet.{Stralet, StraletContext}
import com.acqusta.tquant.stra.utils.CsvHelper
import com.tictactec.ta.lib.MInteger

import scala.io.Source

//
// https://uqer.io
//
//均线突破
//
// 均线突破策略利用均线的突破来产生交易信号，以双均线突破策略为例。
//
// 1. 双均线突破 - 无止盈止损
//
// 利用短期均线 MAS 和长期均线MAL生成开平仓信号：
//
// MAS上穿MAL，形成做多信号，买入开仓；
// MAS下穿MAL，形成做空信号，卖出开仓。
//
// 2. 双均线突破-附带止盈止损$
//
// 利用短期均线 MAS 和长期均线 MAL 生成开平仓信号：
//
// MAS 上穿 MAL，形成做多信号，买入开仓；
// MAS 上穿 MAL，形成做空信号，卖出开仓；
// 止盈 [基于 Bar 线的止盈]：浮动盈利 / 保证金 > 5%， 计算 | 浮动盈利×保证金比例 / 保证金；
// 止损 [基于 Bar 线的止损]：浮动亏损 / 保证金 < 3%， 计算 | 浮动亏损×保证金比例 / 保证金。
//
// 优矿例子是日线调仓，这里改成分钟线，用于测试

class LongShortMAStralet extends Stralet {

    val talib = new com.tictactec.ta.lib.Core()
    var account = ""
    var trade_api : TradeApi = _
    var data_api  : DataApi = _

    var contract : String = _

    override def onInit(sc: StraletContext): Unit = {
        super.onInit(sc)

        sc.log("onInit", sc.getTime)

        trade_api = sc.getTradeApi
        data_api  = sc.getDataApi

        this.account = sc.getParameters[String]("account", "")


        // 从配置中得到要交易的商品期货，然后从主力合约映射表中得到今日交易的合约

        val code = sc.getParameters[String]("code", "")
        assert( code.nonEmpty, "no code  in config")

        contract =
            if (sc.mode == "realtime") {
                "rb1801.SHF"
            } else {
                code
            }

        //println (trade_api.query(this.account, "ctp_codetable"))
        sc.log("trade contract: " + contract)

        data_api.subscribe( Array(contract ))
    }

    override def onFini() = {
        sc.log("onFini", sc.getTime)

        val (cur_pos, _) = trade_api.queryPositions(this.account)
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
            sc.getTradeApi.cancelOrder(this.account, code = this.contract, entrust_no = ord.entrust_no)
        }
    }

    def ma(prices : Array[Double], time_period: Int) : Array[Double] = {

        val ma_values = new Array[Double](prices.length)
        val beg_idx = new MInteger
        val ind_length = new MInteger

        talib.ema(0, prices.length -1 , prices, time_period, beg_idx, ind_length, ma_values)

        ma_values.splitAt(ind_length.value)._1
    }

    override def onBar(cycle : String, bar : Bar): Unit = {

        if (cycle != "1m") return

        if (bar.code != this.contract) return

        if (sc.mode == "realtime") sc.log(bar)

        // 只交易日盘
        if (bar.time < 90000000 || bar.time > 150000000)
            return

        val trading_day = sc.getTradingDay
        val bars = {
            val (tmp, _) = data_api.bar(this.contract, "1m", align = true)
            tmp.filter( x => x.date == trading_day && x.time > 90000000 )
        }

        if (bars.length <  35) return

        val close_prices = bars.map { _.close }.toArray
        val ma_short = ma(close_prices, 5)
        val ma_long  = ma(close_prices, 30)

        val cur_pos = trade_api.queryPositions(this.account)._1.filter( _.code == this.contract)
        val (long_size, short_size) = cur_pos.foldLeft( (0L, 0L) ) { (v, x) =>
            if (x.side == "Long")
                (v._1 + x.current_size, v._2)
            else
                (v._1 , v._2 + x.current_size)
        }

        // 每个 cycle 开始，取消当前的订单
        cancelUnfinshedOrder()

        val last_price = data_api.quote(contract)._1.last

        // 持仓不过夜
        if (bar.time >= 145500000) {
            if (long_size != 0)
                trade_api.placeOrder(this.account, contract, last_price, long_size, "SellToday")

            if (short_size != 0)
                trade_api.placeOrder(this.account, contract, last_price, short_size, "CoverToday")

            return
        }

        val max_open_size = 1
        if (ma_short.last > ma_long.last && ma_short(ma_short.length-2) < ma_long(ma_long.length - 2)) {
            if (short_size > 0)
                trade_api.placeOrder(this.account, contract, last_price, short_size, "CoverToday")

            if (long_size < max_open_size)
                trade_api.placeOrder(this.account, contract, last_price, max_open_size, "Buy")
        }
        else if (ma_short.last < ma_long.last && ma_short(ma_short.length-2) > ma_long(ma_long.length - 2)) {
            if (long_size > 0)
                trade_api.placeOrder(this.account, contract, last_price, long_size, "SellToday")

            if (short_size < max_open_size)
                trade_api.placeOrder(this.account, contract, last_price, max_open_size, "Short")
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

object LongShortMAStralet extends App {

    val stralet_config =
        """
          |{
          |  "stralet" : {
          |    "id": "LongShortMAStralet",
          |    "stralet_class": "cta.LongShortMAStralet",
          |
          |    "parameters": {
          |      "account": "simnow",
          |      "code"   : "rb.SHF"
          |    }
          |  }
          |}
        """.stripMargin

    //"date_range" : [ 20170620, 20170928],
    val backtest_config =
        """
          |{
          |  "tqc" : {
          |    "addr"  : "tcp://127.0.0.1:10001"
          |  },
          |
          |  "backtest" : {
          |    "data_level" : "1m",
          |    "date_range" : [ 20170101, 20170928],
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
        println("Usage: LongShortMAStralet [backtest|realtime]")
        System.exit(0)
    }

    args(0) match {
        case "backtest" => com.acqusta.tquant.stra.backtest.Run.runConf(stralet_config, backtest_config)

        case "realtime" => Run.runConf(stralet_config, "")
    }

}
package cta

import java.time.LocalDateTime

import com.acqusta.tquant.api.scala.DataApi.{Bar, MarketQuote}
import com.acqusta.tquant.api.scala.TradeApi.{Order, Trade}
import com.acqusta.tquant.api.scala.{DataApi, TradeApi}
import com.acqusta.tquant.stra.realtime.Run
import com.acqusta.tquant.stra.stralet.{Stralet, StraletContext}
import com.acqusta.tquant.stra.utils.TimeUtils._

import scala.collection.mutable


class OneTickStralet extends Stralet {

    object State extends Enumeration{
        type TradeSate = Value
        val INIT, WAITING_SIGNAL, OPENING, OPENED, CLOSING, STOP = Value
    }

    case class OrderInfo() {
        var fill_price              = 0.0
        var fill_size_of_trade      = 0L
        var entrust_size            = 0L
        var entrust_price           = 0.0
        var status                  = ""
        var entrust_time : LocalDateTime = _
        var entrust_action          = ""
        var entrust_no              = ""
        var order_id                = 0
        var fill_size_of_order      = 0L
    }

    class PositionInfo {
        var size = 0L
        var cost_price = 0.0

        def cost = size * cost_price
    }

    case class TickData(q : MarketQuote, avg_price_15s : Double)

    class PriceInfo {
        var ticks = mutable.ArrayBuffer[TickData]()
        var avg_price_30s = 0.0 // 最近一段时间平均价格
        var turnover  = 0.0 // 最近一段时间总成交量
    }

    var account = ""
    var trade_api : TradeApi = _
    var data_api  : DataApi = _
    var contract  : String = _

    val TICK_ROLLING_WINDOW_30S = 30
    //val STOP_TIME = 145000000

    var cur_order : OrderInfo = _
    val position = new PositionInfo
    val price_data    = new PriceInfo

    var state = State.INIT
    var open_time : LocalDateTime = _

    // Timer Definition
    val TIMER_CHECK = 1

    //val talib = new com.tictactec.ta.lib.Core()

    def cmp_time(dt1 : (Int, Int), dt2 : (Int, Int)) : Int = {
        if (dt1._1 < dt2._1) return -1
        if (dt1._1 == dt2._1) return dt1._2 - dt1._2
        return 1
    }

    def isInTradingTime(time: Int) : Boolean = {
            (time >  90000000 && time < 101400000) ||
            (time > 103000000 && time < 112900000) ||
            (time > 130000000 && time < 145000000)
    }

    override def onInit(sc: StraletContext): Unit = {
        super.onInit(sc)

        sc.log("onInit", sc.getTime)

        trade_api = sc.getTradeApi
        data_api  = sc.getDataApi

        this.account = sc.getParameters[String]("account", "")

        // 从配置中得到要交易的商品期货，然后从主力合约映射表中得到今日交易的合约

        val code = sc.getParameters[String]("code", "")
        assert( code.nonEmpty, "no code in config")

        contract =
            if (sc.mode == "realtime") {
                "rb1801.SHF"
            } else {
                code
            }

        sc.log("trade contract: " + contract)

        data_api.subscribe( Array(contract ))

        val begin_time = sc.getTime.minusSeconds(120).toHumanDateTime

        // 取最近的两分钟 tick，求平均
        val ticks = data_api.tick(contract)._1.filter( x => cmp_time( (x.date, x.time), begin_time) >= 0)

        for ( i <- ticks.indices) {
            val quote = ticks(i)
            val t1 = (quote.date, quote.time).toLocalDateTime.minusSeconds(TICK_ROLLING_WINDOW_30S).toHumanDateTime
            val tmp_ticks = ticks.filter( x => cmp_time( (x.date, x.time), t1) > 0)
            if (price_data.ticks.nonEmpty) {
                var avg_price_30s = (quote.turnover - tmp_ticks.head.turnover) /
                                    (quote.volume - tmp_ticks.head.volume)
                avg_price_30s /= 10
                price_data.avg_price_30s = avg_price_30s
                // 近似取30秒
                //if (tmp_ticks.size > 30) {
                val begin_pos = if (i>=29) i - 29 else 0
                val totol_price = (begin_pos to i).foldLeft(0.0) { (z, x)=> z + tmp_ticks(x).last }
                val avg_price_15s = totol_price / (i - begin_pos + 1)
                price_data.ticks.append(TickData(quote, avg_price_15s))
            } else {
                price_data.ticks.append(TickData(quote, quote.last))
                price_data.avg_price_30s = quote.last
            }
        }

        sc.setTimer(TIMER_CHECK, 5000, null)
        clearPostionAndOrder()
    }

    def clearPostionAndOrder(): Unit = {

        // 取消所有的订单
        val (orders, msg) = trade_api.queryOrders(this.account)
        if (orders==null) {
            sc.log( s"Failed to query order: $msg")
            return
        }
        var cancelling_orders = 0
        for ( ord <- orders if ord.code == this.contract && !isFinished(ord.status)) {
            trade_api.cancelOrder(this.account, this.contract, entrust_no = ord.entrust_no)
            cancelling_orders += 1
        }

        if (cancelling_orders != 0) return

        val (positions, msg2) = trade_api.queryPositions(this.account)
        if (positions == null) {
            sc.log((s"Failed to query positions: $msg2"))
            return
        }

        // 清除当前的持仓
        val (q, msg3) = this.data_api.quote(this.contract)
        if (q==null) {
            sc.log(s"Failed to get quote: $msg3")
            return
        }

        var has_close_order = false

        for (pos <- positions if pos.code == this.contract && pos.current_size > 0) {
            if (pos.side == "Long") {

                if (pos.today_size > 0) {
                    trade_api.placeOrder(this.account, this.contract, q.bid1,
                        pos.today_size, "SellToday")
                    has_close_order = true
                }

                if (pos.current_size - pos.today_size > 0 ) {
                    trade_api.placeOrder(this.account, this.contract, q.bid1,
                        pos.current_size - pos.today_size,
                        "SellYesterday")
                    has_close_order = true
                }
            } else {
                if (pos.today_size > 0) {
                    trade_api.placeOrder(this.account, this.contract, q.ask1,
                        pos.today_size, "CoverToday")
                    has_close_order = true
                }

                if (pos.current_size - pos.today_size > 0 ) {
                    trade_api.placeOrder(this.account, this.contract, q.ask1,
                        pos.current_size - pos.today_size,
                        "CoverYesterday")
                    has_close_order = true
                }
            }
        }

        if (!has_close_order) {
            this.position.size       = 0L
            this.position.cost_price = 0.0

            if (isInTradingTime(sc.getTimeAsInt._2))
                this.state = State.WAITING_SIGNAL
            else
                this.state = State.STOP
        }
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

    override def onQuote(quote: MarketQuote): Unit = {

        if (quote.code != this.contract) return

        //sc.log(s"onQuote: ${q.time} ${q.last} ${price.avg_price}")

        if (!isInTradingTime(quote.time)) return

        // 保留2分钟的数据
        {
            val begin_time = LocalDateTime.of(quote.date.toLocalDate, quote.time.toLocalTime)
                .minusSeconds(120).toHumanDateTime

            var i = price_data.ticks.indexWhere(x => cmp_time((x.q.date, x.q.time), begin_time) >= 0)
            if (i > 0)
                price_data.ticks = price_data.ticks.splitAt(i)._2
        }

        if (price_data.ticks.nonEmpty) {

            {
                // 计算30秒成交均价
                // 成交量作为权重
                if (false) {
                    val begin_time = LocalDateTime.of(quote.date.toLocalDate, quote.time.toLocalTime).minusSeconds(30).toHumanDateTime

                    val first_quote = {
                        val i = price_data.ticks.indexWhere(x => cmp_time((x.q.date, x.q.time), begin_time) >= 0)
                        if (i >= 0) price_data.ticks(i).q
                        else {
                            println(s"can't find 30s before data ${quote.time}")
                            price_data.ticks.head.q
                        }
                    }

                    var avg_price_30s = (quote.turnover - first_quote.turnover) / (quote.volume - first_quote.volume)

                    avg_price_30s /= 10
                    price_data.avg_price_30s = avg_price_30s
                } else {
                    var avg_price_30s = 0.0
//                    if (price_data.ticks.size >= 60) {
//                        val last_first_quote = price_data.ticks(price_data.ticks.size - 60).q
//                        //avg_price_30s = price_data.avg_price_30s + (quote.last - last_first_quote.last) / 60
//                    } else {
//                        //avg_price_30s = (price_data.avg_price_30s * price_data.ticks.size + quote.last) / (price_data.ticks.size + 1)
//                    }
                    val first_quote =
                        if (price_data.ticks.size >= 60)
                            price_data.ticks(price_data.ticks.size - 60).q
                        else
                            price_data.ticks.head.q
                    avg_price_30s = (quote.turnover - first_quote.turnover) / (quote.volume - first_quote.volume)
                    avg_price_30s /= 10
                    price_data.avg_price_30s = avg_price_30s
                }

            }
            if (false) {
                // 精确计算
                val begin_time_15s = LocalDateTime.of(quote.date.toLocalDate, quote.time.toLocalTime)
                    .minusSeconds(15).toHumanDateTime

                var begin_pos = price_data.ticks.lastIndexWhere(x => cmp_time((x.q.date, x.q.time), begin_time_15s) >= 0)
                if (begin_pos == -1) begin_pos = 0

                val totol_price = (begin_pos to price_data.ticks.size - 1)
                    .foldLeft(quote.last) { (z, x) => z + price_data.ticks(x).q.last }

                val avg_price_15s = totol_price / (price_data.ticks.size - begin_pos + 2) // last tick is not in ticks!
                price_data.ticks.append(TickData(quote, avg_price_15s))
            } else {
                // 快速计算, 假设每秒2个tick
                var avg_price_15s = 0.0
                if (price_data.ticks.size >= 30) {
                    val last_first_quote = price_data.ticks(price_data.ticks.size - 30).q
                    avg_price_15s = price_data.ticks.last.avg_price_15s + (quote.last - last_first_quote.last) / 30
                } else {
                    avg_price_15s = (price_data.ticks.last.avg_price_15s * price_data.ticks.size + quote.last) / (price_data.ticks.size + 1)
                }
                price_data.ticks.append(TickData(quote, avg_price_15s))
            }
        } else {
            price_data.ticks.append(TickData(quote, quote.last))
            price_data.avg_price_30s = quote.last
        }


        if (price_data.ticks.size < 120) return

        state match {
            case State.WAITING_SIGNAL => checkOpenSignal()
            case State.OPENING        => checkOrder ()
            case State.OPENED         => checkCloseSignal()
            case State.CLOSING        => checkOrder ()
            case _ => // do nothing
        }
    }

    def double_equal(v1: Double, v2: Double) : Boolean = {
        Math.abs(v1-v2) < 0.000001
    }

    // 1 -- up, -1 down, 0 -- don't know
    def getTrend() : Int = {
        // 1
        val begin_pos = price_data.ticks.size - 90
        val pos     = mutable.ArrayBuffer[Int](begin_pos)
        val prices  = mutable.ArrayBuffer[Double](price_data.ticks(begin_pos).avg_price_15s)
        val dirs    = mutable.ArrayBuffer[Int](1)

        var last_pos   = pos(0)
        var last_dir   = dirs(0)
        var last_price = prices(0)
        var high = 0.0
        var low  = 999999999999.0
        for ( i <- begin_pos + 1 until price_data.ticks.size) {
            val tick = price_data.ticks(i)
            high = Math.max(high, tick.avg_price_15s)
            low  = Math.min(low,  tick.avg_price_15s)

            if (Math.abs(tick.avg_price_15s - last_price) > 0.1) {
                if ( (tick.avg_price_15s - last_price) * last_dir > 0 ) {
                    last_price = tick.avg_price_15s
                    last_pos   = i
                } else {
                    pos     += last_pos
                    prices  += last_price
                    dirs    += last_dir
                    last_dir *= -1
                }
            }
        }
        prices += price_data.ticks.last.avg_price_15s
        pos    += price_data.ticks.size - 1
        dirs   += last_dir

        // 去掉波动不大
        if (Math.abs(high - low) < 3) return 0

        // 去掉没有均线回归走势 (?)

        var up_weight = 0.0
        var down_weight = 0.0
        for ( i <- 1 until prices.size) {
            val w = (prices(i) + prices(i-1) - 2*low)*(pos(i) - pos(i-1))
            if ( prices(i) > prices(i-1))
                up_weight += w
            else
                down_weight += w
        }

        // 力量对比要差不多
        if ( Math.abs( up_weight / ( up_weight + down_weight) - 0.5) > 0.3) return 0

        println (f"avg tops ${prices.size} ${up_weight}%.1f ${down_weight}%.1f")

        //if (prices.size < 3 ) return 0

        val weight = up_weight - down_weight
        if (weight > 0)  return 1
        else if (weight < 0) return -1
        return 0
    }

    def checkOpenSignal(): Unit = {

        //if (price_data.avg_price_30s <= 0) return

        val q = price_data.ticks.last.q

        // 模拟环境严格，用对手价作信号
        val diff = {
            var buy_diff = q.bid1 - price_data.avg_price_30s
            var sell_diff = q.ask1 - price_data.avg_price_30s

            if (buy_diff > 3.0 && buy_diff < 3.5)
                buy_diff
            else if (sell_diff > -3.5 && sell_diff < -3.0)
                sell_diff
            else
                0.0
        }

        if (Math.abs(diff) < 1) return

        // 检查挂单数量
        if (diff > 0) {
            // 做空
            // 以买价成交,并且卖得数量比买的数量多很多
            if (!(double_equal(q.last, q.bid1) && (q.ask_vol1 *1.0 / q.bid_vol1 > 5))) return
        } else {
            // 做多
            if (!(double_equal(q.last, q.ask1) && (q.bid_vol1 *1.0/ q.ask_vol1 > 5))) return
        }

        // 检查过去30秒的15秒均线趋势和diff一致
        //println(s"trend: $diff $getTrend()")
        if ( diff * getTrend() >= 0) return

        sc.log(f"open signal: ${q.time} ${q.last} ${price_data.avg_price_30s}%.1f $diff%.1f")

        val action = if (diff > 0) "Short" else "Buy"

        // 以对手价下单! 模拟撮合和Simnow 撮合方式一致, 以对价成交
        val price = if (diff > 0 ) q.bid1 else q.ask1

        val (t_order_id, msg) = trade_api.placeOrder(this.account, this.contract,
                                                     price = price, size = 1,
                                                     action = action)
        if (t_order_id != null) {
            this.open_time = LocalDateTime.now
            this.cur_order = new OrderInfo
            cur_order.entrust_time  = sc.getTime
            cur_order.order_id      = t_order_id.order_id
            cur_order.entrust_no    = t_order_id.entrust_no
            cur_order.entrust_price = q.last
            cur_order.entrust_size  = 1
            cur_order.entrust_action = action
            this.state = State.OPENING
        } else {
            sc.log(s"Failed to place order: $msg")
        }
    }

    def checkCloseSignal(): Unit = {

        assert (position.size != 0)

        val q = price_data.ticks.last.q
        // sc.log( s"check close: ${q.time} ${position.size} ${position.cost_price}  ${q.last}  ${price.avg_price}")

        // 平仓信号
        //  1. 最大允许偏离 2 个 price_tick
        //  2. 偏离 avg_price 4个 price_tick
        var should_close =
            if ( position.size > 0) {
                if (q.ask1 - position.cost_price <= - 2.0 || q.bid1 - position.cost_price >= 4)
                    true
                else {
                    val diff = q.last - price_data.avg_price_30s
                    diff < -5.0
                }
            } else {
                if (q.bid1 - position.cost_price >= 2.0 || q.ask1 - position.cost_price <= -4)
                    true
                else {
                    val diff = q.last - price_data.avg_price_30s
                    diff > 5.0
                }
            }

        if (!should_close) {
            if (sc.getTime.isAfter( this.open_time.plusSeconds(30)))
                should_close = true
            else {

            }
        }
        if (should_close) {

            sc.log(s"close signal: ${q.time} ${position.size} ${position.cost_price}  ${q.last}  ${price_data.avg_price_30s}")

            val action = if (position.size > 0) "SellToday" else "CoverToday"
            val price = q.last //if (position.size > 0 ) q.bid1 else q.ask1
            val (t_order_id, msg) = trade_api.placeOrder(this.account, this.contract,
                                price = price, size = Math.abs(position.size),
                                action = action)

            if (t_order_id != null) {

                this.cur_order = new OrderInfo
                cur_order.entrust_time   = sc.getTime
                cur_order.order_id       = t_order_id.order_id
                cur_order.entrust_no     = t_order_id.entrust_no
                cur_order.entrust_price  = price
                cur_order.entrust_size   = 1
                cur_order.entrust_action = action

                this.state = State.CLOSING
            } else {
                sc.log(s"Failed to place order: $msg")
            }
        }
    }

    def checkOrder() = {
        // 两秒没有成交, 则取消订单
        if (this.cur_order != null &&
            this.cur_order.entrust_time.plusSeconds(2).isBefore(sc.getTime))
        {
            sc.log(s"cancel order after 2 seconds ${sc.getTimeAsInt}")
            trade_api.cancelOrder(this.account, this.contract, this.cur_order.entrust_no, this.cur_order.order_id)
        }
    }

    def isFinished(status : String): Boolean = {
        status match {
            case "Filled"    => true
            case "Cancelled" => true
            case "Rejected"  => true
            case _ => false
        }
    }

    override def onBar(cycle : String, bar : Bar): Unit = {

    }

    def getActionSide(action: String) = {
        action match {
            case "Buy"              =>  1
            case "Short"            => -1
            case "Cover"            =>  1
            case "Sell"             => -1
            case "CoverToday"       =>  1
            case "SellToday"        => -1
            case "CoverYesterday"   =>  1
            case "SellYesterday"    => -1
            case _                  => assert(false); 0
        }
    }

    override def onOrderStatus(order: Order): Unit = {

        sc.log("onOrder: " + order)

        if (this.cur_order == null) return

        if (order.order_id != this.cur_order.order_id) return
        if (order.entrust_no != null)
            this.cur_order.entrust_no = order.entrust_no

        this.cur_order.status               = order.status
        this.cur_order.fill_size_of_order   = order.fill_size

        if (!isFinished(order.status)) return

        // 如果 fill_size 不匹配, 在 onTrade 中结束订单
        if (this.cur_order.fill_size_of_order == this.cur_order.fill_size_of_trade) {
            //sc.log(s"fill_size: ${this.cur_order.fill_size_of_order}")
            this.cur_order = null
            if (this.position.size != 0)
                this.state = State.OPENED
            else
                this.state = State.WAITING_SIGNAL
        }
    }

    override def onOrderTrade(trade: Trade): Unit = {

        if (trade.code != this.contract) return

        sc.log("onTrade: " + trade)

        if (this.cur_order == null) {
            sc.log("no order info!")
            return
        }

        if (this.cur_order.entrust_no != trade.entrust_no) {
            sc.log(s"unmatched entrust_no ${trade.entrust_no} ${this.cur_order.entrust_no}")
            return
        }

        val side = getActionSide(this.cur_order.entrust_action)
        val cost = this.position.cost + trade.fill_size * trade.fill_price

        this.cur_order.fill_size_of_trade += trade.fill_size

        //sc.log(s"size ${position.size} ${trade.fill_size} $side")
        this.position.size      += trade.fill_size * side
        if (this.position.size != 0)
            this.position.cost_price = Math.abs(cost / this.position.size)
        else
            this.position.cost_price = 0.0

        if (isFinished(this.cur_order.status)) {
            assert (this.cur_order.fill_size_of_trade == this.cur_order.fill_size_of_order)
            this.cur_order = null
            if (this.position.size != 0)
                this.state = State.OPENED
            else
                this.state = State.WAITING_SIGNAL
        }
    }

    override def onEvent(evt: String, data: Any): Unit = {
    }

    override def onTimer(id : Int, data : Any): Unit = {
        if (id == TIMER_CHECK) {
            if (state == State.INIT)
                clearPostionAndOrder()
            else if (state != State.STOP && !isInTradingTime(sc.getTimeAsInt._2))
                clearPostionAndOrder()
            else if (state == State.STOP && isInTradingTime(sc.getTimeAsInt._2))
                state = State.WAITING_SIGNAL
        }
    }
}

object OneTickStralet extends App {

    val stralet_config =
        """
          |{
          |  "stralet" : {
          |    "id": "OneTickStralet",
          |    "stralet_class": "cta.OneTickStralet",
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
      |    "data_level" : "tk",
      |    "date_range" : [ 20171001, 20171101],
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
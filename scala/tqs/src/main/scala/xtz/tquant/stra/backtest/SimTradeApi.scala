package xtz.tquant.stra.backtest

import java.io.ByteArrayOutputStream
import java.time.LocalDate

import com.acqusta.tquant.api.scala.TradeApi
import com.acqusta.tquant.api.scala.TradeApi.OrderID

import scala.collection.mutable
import xtz.tquant.stra.stralet.TqsTradeApi
import xtz.tquant.stra.stralet.TqsTradeApi.NetPosition
import xtz.tquant.stra.utils.CsvHelper

object SimAccount {

    class Balance {
        var init_balance = 0.0
        var enable_balance = 0.0
    }

    class TradeData {
        var trading_day    : LocalDate = _
        var cur_entrust_no : Int = 0
        val positions = mutable.HashMap[(String, String), TradeApi.Position]()
        val orders = mutable.ArrayBuffer[TradeApi.Order]()
        val trades = mutable.ListBuffer[TradeApi.Trade]()
        val balance = new Balance()
    }
}

class SimAccount(sim: SimTradeApi, account_id : String) {

    import SimAccount._

    var cur_data : TradeData = _
    var his_data = mutable.ListBuffer[TradeData]()

    private var status_ind = mutable.ListBuffer[TradeApi.Order]()
    private var trade_ind  = mutable.ListBuffer[TradeApi.Trade]()

    def getInd : (Seq[TradeApi.Order], Seq[TradeApi.Trade]) = {
        val ret = (status_ind, trade_ind)
        status_ind = mutable.ListBuffer[TradeApi.Order]()
        trade_ind  = mutable.ListBuffer[TradeApi.Trade]()
        ret
    }

    def init(init_balance: Double): Unit = {
        cur_data = new TradeData()
        cur_data.trading_day = null
        cur_data.balance.init_balance = init_balance
        cur_data.balance.enable_balance = init_balance
    }

    def moveTo(next_trading_day: LocalDate): Unit = {
        if (cur_data.trading_day == null) {
            cur_data.trading_day = next_trading_day
        }
        else {
            val data = new TradeData()
            data.trading_day = next_trading_day
            data.balance.init_balance = cur_data.balance.enable_balance
            data.balance.enable_balance =  cur_data.balance.enable_balance

            for( (code, pos) <- cur_data.positions if pos.current_size !=0) {
                val new_pos = new TradeApi.Position
                new_pos.account_id   = pos.account_id
                new_pos.code         = pos.code
                new_pos.name         = pos.name
                new_pos.current_size = pos.current_size
                new_pos.enable_size  = pos.current_size
                new_pos.init_size    = pos.current_size
                new_pos.side         = pos.side
                new_pos.cost         = pos.cost
                new_pos.cost_price   = pos.cost_price
                data.positions += code -> new_pos
            }

            his_data += cur_data
            cur_data = data

            //println(s"-- new trading day $next_trading_day")
            //println(s"enable_balance: ${data.balance.init_balance}")
//            data.positions.foreach( println _)
        }
    }

    def queryBalance() : (TradeApi.Balance, String) = {
        val bal = new TradeApi.Balance
        bal.account_id = account_id
        bal.fund_account = account_id
        bal.init_balance = cur_data.balance.init_balance
        bal.enable_balance = cur_data.balance.enable_balance
        bal.margin      = 0.0
        bal.float_pnl   = 0.0
        bal.close_pnl   = 0.0
        (bal, "")
    }

    def queryOrders() : (Seq[TradeApi.Order], String) = {
        ( cur_data.orders.toSeq, "")
    }
    def queryTrades() : (Seq[TradeApi.Trade], String) = {
        ( cur_data.trades.toSeq, "")
    }
    def queryPosition() : (Seq[TradeApi.Position], String) = {
        ( cur_data.positions.values.toSeq, "")
    }

    def buy(code : String, price : Double, size : Long, action : String) : (String, String) = {

        val cur_price = {
            val q = this.sim.st.dapi.quote(code)
            if (q._1 != null)
                q._1.ask1
            else
                -1.0
        }

        if (cur_price <= 0)
            return ("", s"no quote $code")

        if (price < cur_price) return ("", "")

        val cost = cur_price * size
        if (cur_data.balance.enable_balance >= cost) {
            cur_data.balance.enable_balance -= cost
            val pos = cur_data.positions.getOrElse( (code, "Long"), null)
            if (pos == null) {

                var pos = new TradeApi.Position
                pos.account_id  = account_id
                pos.code        = code
                pos.name        = code
                pos.current_size = size
                pos.enable_size = if (isFuture(code)) size else 0
                pos.today_size  = size
                pos.side        = "Long"
                pos.cost        = cost
                pos.cost_price  = price

                cur_data.positions += (code, "Long") -> pos
            } else {
                val pos = new TradeApi.Position

                pos.account_id  = pos.account_id
                pos.code        = pos.code
                pos.name        = pos.name
                pos.current_size = pos.current_size + size
                pos.enable_size = pos.enable_size + (if (isFuture(code)) size else 0)
                pos.init_size   = 0
                pos.frozen_size = 0
                pos.today_size  = pos.today_size + size
                pos.side        = pos.side
                pos.cost        = pos.cost + cost
                pos.cost_price  = (pos.cost + cost) / pos.current_size
                cur_data.positions += (code, "Long") -> pos
            }
            ("Filled", "")
        } else {
            ("Rejected", "no enough avail")
        }
    }

    def sell(code : String, price : Double, size : Long, action : String) : (String, String) = {

        val cur_price = {
            val q = this.sim.st.dapi.quote(code)
            if (q._1 != null)
                q._1.bid1
            else
                -1.0
        }

        if (cur_price <= 0)
            return ("", s"no quote $code")

        if (price > cur_price) return ("", "")

        val pos = cur_data.positions.getOrElse( (code, "Long"), null)
        if (pos != null && pos.enable_size >= size) {
            if (isFuture(code)) {
                val mkt = code.split('.').last
                val isSellToday = action match {
                    case "Sell"             => mkt != ".SHF"
                    case "SellToday"        => true
                    case "SellYesterday"    => false
                }

                var today_size = 0L
                if (mkt == "SHF") {
                    if (isSellToday) {
                        if (pos.today_size < size)
                            return ("Rejected", "no enough today size")
                        today_size = pos.today_size - size
                        if (today_size < 0) today_size = 0
                    } else {
                        if (pos.current_size < size)
                            return ("Rejected", "no enough size")
                    }
                } else {
                    if (pos.current_size - pos.today_size < size)
                       return ("Rejected", "no enough yesterday size")

                    today_size -= pos.today_size - size
                    if (today_size < 0) today_size = 0
                }

                val cost = cur_price * size
                val new_pos = new TradeApi.Position
                new_pos.account_id  = pos.account_id
                new_pos.code        = pos.code
                new_pos.name        = pos.name
                new_pos.current_size = pos.current_size - size
                new_pos.enable_size = pos.enable_size - size
                new_pos.init_size   = pos.init_size
                new_pos.today_size  = today_size
                new_pos.side        = pos.side
                new_pos.cost        = pos.cost - cost
                new_pos.cost_price  = pos.cost_price

                cur_data.positions += (code, "Long") -> new_pos
                cur_data.balance.enable_balance += cost
                ("Filled", "")
            } else {
                val cost = cur_price * size
                val new_pos = new TradeApi.Position
                new_pos.account_id  = pos.account_id
                new_pos.code        = pos.code
                new_pos.name        = pos.name
                new_pos.current_size = pos.current_size - size
                new_pos.enable_size = pos.enable_size - size
                new_pos.init_size   = pos.init_size
                new_pos.today_size  = pos.today_size
                new_pos.side        = pos.side
                new_pos.cost        = pos.cost - cost
                new_pos.cost_price  = pos.cost_price

                cur_data.positions += (code, "Long") -> new_pos
                cur_data.balance.enable_balance += cost
                ("Filled", "")
            }
        } else {
            ("Rejected", "no enough enable_size")
        }

    }

    def short(code : String, price : Double, size : Long, action : String) : (String, String) = {

        if (!isFuture(code)) {
            return ("Rejected", "wrong action for stock")
        }

        val cur_price = {
            val q = this.sim.st.dapi.quote(code)
            if (q._1 != null)
                q._1.bid1
            else
                -1.0
        }

        if (cur_price <= 0)
            return ("", s"no quote $code")

        if (price > cur_price) return ("", "")

        val cost = cur_price * size
        if (cur_data.balance.enable_balance >= cost) {
            cur_data.balance.enable_balance -= cost
            val pos = cur_data.positions.getOrElse( (code, "Short"), null)
            if (pos == null) {
                val new_pos = new TradeApi.Position
                new_pos.account_id  = account_id
                new_pos.code        = code
                new_pos.name        = code
                new_pos.current_size = size
                new_pos.enable_size = size
                new_pos.init_size   = 0
                new_pos.frozen_size = 0
                new_pos.today_size  = size
                new_pos.side        = "Short"
                new_pos.cost        = cost
                new_pos.cost_price  = price
                cur_data.positions += (code, "Short") -> new_pos
            } else {
                val new_pos = new TradeApi.Position
                new_pos.account_id  = pos.account_id
                new_pos.code        = pos.code
                new_pos.name        = pos.name
                new_pos.current_size = pos.current_size + size
                new_pos.enable_size = pos.enable_size + size
                new_pos.init_size   = pos.init_size
                new_pos.today_size  = pos.today_size + size
                new_pos.side        = pos.side
                new_pos.cost        = pos.cost + cost
                new_pos.cost_price  = (pos.cost + cost) / pos.current_size
                cur_data.positions += (code, "Short") -> new_pos
            }
            ("Filled", "")
        } else {
            ("Rejected", "no enough avail")
        }
    }

    def cover(code : String, price : Double, size : Long, action : String) : (String, String) = {

        if (!isFuture(code)) {
            return ("Rejected", "wrong action for stock")
        }

        val cur_price = {
            val q = this.sim.st.dapi.quote(code)
            if (q._1 != null)
                q._1.ask1
            else
                -1.0
        }

        if (cur_price <= 0)
            return ("", s"no quote $code")

        if (price < cur_price) return ("", "")

        val pos = cur_data.positions.getOrElse( (code, "Short"), null)

        val mkt = code.split('.').last
        val isCoverToday = action match {
            case "Cover"             => mkt != ".SHF"
            case "CoverToday"        => true
            case "CoverYesterday"    => mkt == ".SHF"
        }

        if (isCoverToday) {
            if (pos.today_size < size)
                return ("Rejected", "no enough short today size")
        } else {
            if (pos.current_size - pos.today_size < size)
                return ("Rejected", "no enough short yesterday size")
        }

        if (pos != null && pos.enable_size >= size) {
            val cost = cur_price * size
            val new_pos = new TradeApi.Position

            new_pos.account_id  = pos.account_id
            new_pos.code        = pos.code
            new_pos.name        = pos.name
            new_pos.current_size = pos.current_size - size
            new_pos.enable_size = pos.enable_size - size
            new_pos.init_size   = pos.init_size
            new_pos.today_size  = if (isCoverToday) pos.today_size - size else pos.today_size
            new_pos.side        = pos.side
            new_pos.cost        = pos.cost - cost
            new_pos.cost_price  = pos.cost_price

            cur_data.positions += (code, "Short") -> new_pos
            cur_data.balance.enable_balance += cost
            ("Filled", "")
        } else {
            ("Rejected", "no enough enable_size")
        }
    }

    def isFuture(code: String) : Boolean = {
        code.split('.').last match {
            case "SH" => false
            case "SZ" => false
            case _ => true
        }
    }

    /**
      * 检查是否该订单是否可以成交，如果可以更新持仓的数量和成本价。
      * 当前只支持股票。
      *
      * @return (matched, err_msg)
      */

    def isFinished(status : String): Boolean = {
        status match {
            case "Filled"    => true
            case "Cancelled" => true
            case "Rejected"  => true
            case _ => false
        }
    }

    def tryMatch(order : TradeApi.Order) : (TradeApi.Order, TradeApi.Trade) = {

        val q = this.sim.st.dapi.quote(order.code)._1
        if ( (q.date == order.entrust_date && q.time <= order.entrust_time) ||
             q.date < order.entrust_date) {
            return (null, null)
        }

        val (status, msg) = {
            order.entrust_action.toUpperCase match {
                case "BUY"              =>  buy  (order.code, order.entrust_price, order.entrust_size, order.entrust_action)
                case "SELL"             =>  sell (order.code, order.entrust_price, order.entrust_size, order.entrust_action)
                case "SHORT"            =>  short(order.code, order.entrust_price, order.entrust_size, order.entrust_action)
                case "COVER"            =>  cover(order.code, order.entrust_price, order.entrust_size, order.entrust_action)
                case "SELLTODAY"        =>  sell (order.code, order.entrust_price, order.entrust_size, order.entrust_action)
                case "SELLYESTERDAY"    =>  sell (order.code, order.entrust_price, order.entrust_size, order.entrust_action)
                case "COVERTODAY"       =>  cover(order.code, order.entrust_price, order.entrust_size, order.entrust_action)
                case "COVERYESTERDAY"   =>  cover(order.code, order.entrust_price, order.entrust_size, order.entrust_action)
                case _                  =>  ("Rejected", s"Unsupported entrust_action ${order.entrust_action}")
            }
        }

        if (status == "") return (null, null)

        var fill_price = order.fill_price
        var fill_size  = order.fill_size
        var trade : TradeApi.Trade = null
        if (status == "Filled") {
            fill_price = q.last
            fill_size  = order.entrust_size

            trade = new TradeApi.Trade
            trade.account_id = account_id
            trade.code       = order.code
            trade.name       = order.name
            trade.entrust_no = order.entrust_no
            trade.entrust_action = order.entrust_action
            trade.fill_no        = order.entrust_no + "-trade"
            trade.fill_size      = fill_size
            trade.fill_price     = fill_price
            trade.fill_date      = sim.getSimTime._1
            trade.fill_time      = sim.getSimTime._2
        }

        val new_order = new TradeApi.Order
        new_order.account_id     = order.account_id
        new_order.code           = order.code
        new_order.name           = order.name
        new_order.entrust_no     = order.entrust_no
        new_order.entrust_action = order.entrust_action
        new_order.entrust_price  = order.entrust_price
        new_order.entrust_size   = order.entrust_size
        new_order.entrust_date   = order.entrust_date
        new_order.entrust_time   = order.entrust_time
        new_order.fill_price     = fill_price
        new_order.fill_size      = fill_size
        new_order.status         = status
        new_order.status_msg     = msg
        new_order.order_id       = order.order_id
        (new_order, trade)
    }

    def tryMatchOrders() : Unit = {

        for ( i <- this.cur_data.orders.indices) {
            var ord = this.cur_data.orders(i)
            if (!isFinished(ord.status)){
                if (ord.status == "New") {
                    val tmp = new TradeApi.Order
                    tmp.account_id     = ord.account_id
                    tmp.code           = ord.code
                    tmp.name           = ord.name
                    tmp.entrust_no     = ord.entrust_no
                    tmp.entrust_action = ord.entrust_action
                    tmp.entrust_price  = ord.entrust_price
                    tmp.entrust_size   = ord.entrust_size
                    tmp.entrust_date   = ord.entrust_date
                    tmp.entrust_time   = ord.entrust_time
                    tmp.fill_price     = ord.fill_price
                    tmp.fill_size      = ord.fill_size
                    tmp.status         = "Accepted"
                    tmp.status_msg     = ""
                    tmp.order_id       = ord.order_id

                    status_ind += tmp
                    cur_data.orders(i) = tmp
                    ord = tmp
                }

                val (new_order, new_trade) = tryMatch(ord)
                if (new_order != null) {
                    cur_data.orders(i) = new_order
                    status_ind += new_order
                }

                if (new_trade != null) {
                    cur_data.trades += new_trade
                    trade_ind += new_trade
                }
            }
        }
    }

    /**
      * 保存订单，如果撮合成功，则保存成交，返回 entrust_no。
      * 注意：即使失败，也返回 不为空的entrust_no。
      *
      * @return entrust_no
      */
    def insertOrder(code : String, price : Double, size : Long, action : String) : (String, Int) = {
        cur_data.cur_entrust_no += 1
        val entrust_no = s"SIM-${cur_data.cur_entrust_no}"

        val (date, time) = sim.getSimTime

        val order = new TradeApi.Order
        order.account_id     = account_id
        order.code           = code
        order.name           = code
        order.entrust_no     = entrust_no
        order.entrust_action = action
        order.entrust_price  = price
        order.entrust_size   = size
        order.entrust_date   = date
        order.entrust_time   = time
        order.fill_price     = 0.0
        order.fill_size      = 0L
        order.status         = "New"
        order.status_msg     = ""
        order.order_id       = cur_data.cur_entrust_no
        cur_data.orders += order
        (entrust_no, cur_data.cur_entrust_no)
    }

    def placeOrder(code : String, price : Double, size : Long, action : String) : (OrderID, String) = {

        val (date, time) = this.sim.getSimTime
        val mkt = code.split('.').last

        val is_open_time = {

            val is_future =
                mkt match {
                    case "SH" => false
                    case "SZ" => false
                    case _ => true
                }

            if (is_future) {
                (time >= 90000000 && time < 101500000) ||
                (time >= 1030000 && time < 113000000) ||
                (time > 1300000 && time < 150000000)
            } else {
                (time >= 93000000 && time<113000000) || (time > 1300000 && time < 150000000)
            }
        }
        if (!is_open_time) {
            println("ERROR: place order when market is closed!")
            return (null, "market is closed")
        }

        if (code.endsWith(".SZ") && time > 145700000) {
            println("ERROR: forbid placing order to SZ market after 145700")
            return (null, "SZ market is closed")
        }

        //val (success, msg) = tryMatchDeal(code, price, size, action)
        val (entrust_no, order_id) = insertOrder(code, price, size, action)

        this.sim.log(f"place order: $date $time $code $price%.3f $size $action, $entrust_no, $order_id")

        val oid = new OrderID
        oid.entrust_no = entrust_no
        oid.order_id = order_id
        (oid, "")
    }

    def cancelOrder(code : String, entrust_no : String) : (Boolean, String) = {

        this.sim.log(f"cancel order: $code $entrust_no")

        val idx = cur_data.orders.indexWhere( _.entrust_no == entrust_no)
        if ( idx < 0) {
            (false, "unknown entrust_no")
        } else {
            val order = cur_data.orders(idx)
            if (isFinished(order.status))
                (true, "already filled")
            else {
                val new_order = new TradeApi.Order
                new_order.account_id     = order.account_id
                new_order.code           = order.code
                new_order.name           = order.name
                new_order.entrust_no     = order.entrust_no
                new_order.entrust_action = order.entrust_action
                new_order.entrust_price  = order.entrust_price
                new_order.entrust_size   = order.entrust_size
                new_order.entrust_date   = order.entrust_date
                new_order.entrust_time   = order.entrust_time
                new_order.fill_price     = order.fill_price
                new_order.fill_size      = order.fill_size
                new_order.status         = "Cancelled"
                new_order.status_msg     = ""
                new_order.order_id       = order.order_id

                cur_data.orders(idx) = new_order
                status_ind += new_order
                (true, "cancelled")
            }
        }
    }
}

class SimTradeApi(_st: StraletTest) extends TqsTradeApi {

    def log(data: Any) : Unit = _st.curSimContext.log(data)

    val accounts : Map[String, SimAccount] =
        _st.cfg.accounts.map { x=> x -> new SimAccount(this, x) }.toMap

    def init(balance: Double): Unit = {
        accounts.foreach( _._2.init(balance))
    }

    def getSimTime: (Int, Int) = _st.curSimContext.getTimeAsInt

    def st : StraletTest = _st

    def moveTo(next_tradingday: LocalDate) : Unit = {
        accounts.foreach(_._2.moveTo(next_tradingday))
    }

    def tryMatch(): Unit = {
        accounts foreach ( _._2.tryMatchOrders())
    }

    def getInd : (Seq[TradeApi.Order], Seq[TradeApi.Trade]) = {
        accounts.map( _._2.getInd)
                .foldLeft( (Seq[TradeApi.Order] (), Seq[TradeApi.Trade]() ) ) {
                    (x, i) => (x._1 ++ i._1, x._2 ++ i._2)
                }
    }

    override
    def queryAccountStatus : (Seq[TradeApi.AccountInfo], String) = {
        val status = accounts.map{ case (k, v) =>
            val act = new TradeApi.AccountInfo
            act.account_id = k
            act.broker = "sim"
            act.account_id = k
            act.status = "Connected"
            act.account_type = "sim"
            act }
        (status.toSeq, "")
    }

    override
    def queryBalance(account_id : String) : (TradeApi.Balance, String) = {
        val act = accounts.getOrElse(account_id, null)
        if (act != null)
            act.queryBalance()
        else
            (null, "unkown account")
    }

    override
    def queryOrders(account_id : String) : (Seq[TradeApi.Order], String) = {
        val act = accounts.getOrElse(account_id, null)
        if (act != null)
            act.queryOrders()
        else
            (null, "unkown account")
    }

    override
    def queryTrades(account_id : String) : (Seq[TradeApi.Trade], String) = {
        val act = accounts.getOrElse(account_id, null)
        if (act != null)
            act.queryTrades()
        else
            (null, "unkown account")
    }

    override
    def queryPositions(account_id : String) : (Seq[TradeApi.Position], String) = {
        val act = accounts.getOrElse(account_id, null)
        if (act != null)
            act.queryPosition()
        else
            (null, "unkown account")
    }

    override
    def placeOrder(account_id : String, code : String, price : Double, size : Long, action : String, order_id: Int) : (OrderID, String) = {
        val act = accounts.getOrElse(account_id, null)
        if (act != null)
            act.placeOrder(code, price, size, action)
        else
            (null, "unkown account")
    }

    override
    def cancelOrder(account_id : String, code : String, entrust_no : String) : (Boolean, String) = {
        val act = accounts.getOrElse(account_id, null)
        if (act != null)
            act.cancelOrder(code, entrust_no)
        else
            (false, "unkown account")
    }

    override
    def cancelOrder(account_id : String, code : String, order_id : Int) : (Boolean, String) = {
//        val act = accounts.getOrElse(account_id, null)
//        if (act != null)
//            act.cancelOrder(code, order_id)
//        else
//            (false, "unkown account")
        (false, "unsupported method: cancelOrder by order_id")
    }

    override
    def cancelOrder(account_id : String, code : String, entrust_no : String, order_id : Int) : (Boolean, String) = {
        if (entrust_no == null || entrust_no.isEmpty)
            (false, "empty entrust_no")
        else
            cancelOrder(account_id, code, entrust_no)
    }


    override def setCallback(callback: TradeApi.Callback): Unit = {

    }

    override def query(account_id: String, command: String, params: String): (String, String) = {
        command match {
            case "ctp_codetable" =>
                val stream = getClass.getResourceAsStream("/sim/ctp_codetable.csv")
                val baos = new ByteArrayOutputStream()
                val bytes = new Array[Byte](4096)
                var len = stream.read(bytes)
                while ( len > 0 ) {
                    baos.write(bytes, 0, len)
                    len = stream.read(bytes)
                }
                (new String(baos.toByteArray, "utf-8"), "")
            case _ =>
            (null, "-1,don't support")
        }
    }

    override def queryNetPosition(account_id: String) : (Seq[NetPosition], String) = {
        // TODO:
        assert(false, "to be implemented")
        null
    }

    override def placeAutoOrder(account_id: String, code: String, price : Double, size: Long) : (String, String) = {
        // TODO:
        assert(false, "to be implemented")
        null
    }

    override def cancelAutoOrder(account_id: String, code: String, entrust_no: String) : (Boolean, String) = {
        // TODO:
        assert(false, "to be implemented")
        null
    }

    def saveOrder(path: String): Unit = {
        val orders = mutable.ArrayBuffer[TradeApi.Order]()
        for ( (_, a) <- accounts) {
            for ( data <- a.his_data)
                orders ++= data.orders
        }

        val text = CsvHelper.serialize(orders)

        val stream = new java.io.FileOutputStream(path)
        //val writer = new FileWriter(path)
        stream.write( text.getBytes("utf8"))
    }

    def savePostion(path: String) : Unit = {

    }
}


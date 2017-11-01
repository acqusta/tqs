package xtz.tquant.stra.backtest

import java.io.ByteArrayOutputStream
import java.time.LocalDate

import scala.collection.mutable
import xtz.tquant.api.scala.TradeApi
import xtz.tquant.api.scala.TradeApi.{Callback, OrderID, OrderStatus}
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

    var status_ind = mutable.ListBuffer[TradeApi.Order]()
    var trade_ind  = mutable.ListBuffer[TradeApi.Trade]()

    def getInd() : (Seq[TradeApi.Order], Seq[TradeApi.Trade]) = {
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
                data.positions +=
                    code ->
                        TradeApi.Position(
                            account_id   = pos.account_id,
                            code         = pos.code,
                            name         = pos.name,
                            current_size = pos.current_size,
                            enable_size  = pos.current_size,
                            init_size    = pos.current_size,
                            today_size   = 0,
                            frozen_size  = 0,
                            side         = pos.side,
                            cost         = pos.cost,
                            cost_price   = pos.cost_price,
                            last_price   = 0.0,
                            float_pnl    = 0.0,
                            close_pnl    = 0.0,
                            margin       = 0.0,
                            commission   = 0.0
                        )
            }

            his_data += cur_data
            cur_data = data

            //println(s"-- new trading day $next_trading_day")
            //println(s"enable_balance: ${data.balance.init_balance}")
//            data.positions.foreach( println _)
        }
    }

    def queryBalance() : (TradeApi.Balance, String) = {

        val bal = TradeApi.Balance(account_id = account_id,
                    fund_account = account_id,
                    init_balance = cur_data.balance.init_balance,
                    enable_balance = cur_data.balance.enable_balance,
                    margin      = 0.0,
                    float_pnl   = 0.0,
                    close_pnl   = 0.0)
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
                cur_data.positions += (code, "Long") -> TradeApi.Position(
                    account_id  = account_id,
                    code        = code,
                    name        = code,
                    current_size = size,
                    enable_size = if (isFuture(code)) size else 0,
                    init_size   = 0,
                    frozen_size = 0,
                    today_size  = size,
                    side        = "Long",
                    cost        = cost,
                    cost_price  = price,
                    last_price  =  0.0,
                    float_pnl   = 0.0,
                    close_pnl   = 0.0,
                    margin      = 0.0,
                    commission  = 0.0)
            } else {
                    cur_data.positions += (code, "Long") -> TradeApi.Position (
                    account_id  = pos.account_id,
                    code        = pos.code,
                    name        = pos.name,
                    current_size = pos.current_size + size,
                    enable_size = pos.enable_size + (if (isFuture(code)) size else 0),
                    init_size   = 0,
                    frozen_size = 0,
                    today_size  = pos.today_size + size,
                    side        = pos.side,
                    cost        = pos.cost + cost,
                    cost_price  = (pos.cost + cost) / pos.current_size,
                    last_price  = 0.0,
                    float_pnl   = 0.0,
                    close_pnl   = 0.0,
                    commission  = 0.0,
                    margin      = 0.0 )
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
                cur_data.positions += (code, "Long") -> TradeApi.Position (
                    account_id  = pos.account_id,
                    code        = pos.code,
                    name        = pos.name,
                    current_size = pos.current_size - size,
                    enable_size = pos.enable_size - size,
                    init_size   = pos.init_size,
                    frozen_size = 0,
                    today_size  = today_size,
                    side        = pos.side,
                    cost        = pos.cost - cost,
                    cost_price  = pos.cost_price,
                    last_price  = 0.0,
                    float_pnl   = 0.0,
                    close_pnl   = 0.0,
                    commission  = 0.0,
                    margin      = 0.0 )

                cur_data.balance.enable_balance += cost
                ("Filled", "")
            } else {
                val cost = cur_price * size
                cur_data.positions += (code, "Long") -> TradeApi.Position (
                    account_id  = pos.account_id,
                    code        = pos.code,
                    name        = pos.name,
                    current_size = pos.current_size - size,
                    enable_size = pos.enable_size - size,
                    init_size   = pos.init_size,
                    frozen_size = 0,
                    today_size  = pos.today_size,
                    side        = pos.side,
                    cost        = pos.cost - cost,
                    cost_price  = pos.cost_price,
                    last_price  = 0.0,
                    float_pnl   = 0.0,
                    close_pnl   = 0.0,
                    commission  = 0.0,
                    margin      = 0.0 )

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
                cur_data.positions += (code, "Short") -> TradeApi.Position(
                    account_id  = account_id,
                    code        = code,
                    name        = code,
                    current_size = size,
                    enable_size = size,
                    init_size   = 0,
                    frozen_size = 0,
                    today_size  = size,
                    side        = "Short",
                    cost        = cost,
                    cost_price  = price,
                    last_price  =  0.0,
                    float_pnl   = 0.0,
                    close_pnl   = 0.0,
                    commission  = 0.0,
                    margin      = 0.0 )

            } else {
                cur_data.positions += (code, "Short") -> TradeApi.Position (
                    account_id  = pos.account_id,
                    code        = pos.code,
                    name        = pos.name,
                    current_size = pos.current_size + size,
                    enable_size = pos.enable_size + size,
                    init_size   = pos.init_size,
                    frozen_size = 0,
                    today_size  = pos.today_size + size,
                    side        = pos.side,
                    cost        = pos.cost + cost,
                    cost_price  = (pos.cost + cost) / pos.current_size,
                    last_price  =  0.0,
                    float_pnl   = 0.0,
                    close_pnl   = 0.0,
                    commission  = 0.0,
                    margin      = 0.0 )
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
            cur_data.positions += (code, "Short") -> TradeApi.Position (
                account_id  = pos.account_id,
                code        = pos.code,
                name        = pos.name,
                current_size = pos.current_size - size,
                enable_size = pos.enable_size - size,
                init_size   = pos.init_size,
                frozen_size = 0,
                today_size  = if (isCoverToday) pos.today_size - size else pos.today_size,
                side        = pos.side,
                cost        = pos.cost - cost,
                cost_price  = pos.cost_price,
                last_price  =  0.0,
                float_pnl   = 0.0,
                close_pnl   = 0.0,
                commission  = 0.0,
                margin      = 0.0 )
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

            trade = TradeApi.Trade(
                account_id = account_id,
                code       = order.code,
                name       = order.name,
                entrust_no = order.entrust_no,
                entrust_action = order.entrust_action,
                fill_no        = order.entrust_no + "-trade",
                fill_size      = fill_size,
                fill_price     = fill_price,
                fill_date      = sim.getSimTime._1,
                fill_time      = sim.getSimTime._2
            )
        }

        val new_order = TradeApi.Order(
                account_id     = order.account_id    ,
                code           = order.code          ,
                name           = order.name          ,
                entrust_no     = order.entrust_no    ,
                entrust_action = order.entrust_action,
                entrust_price  = order.entrust_price ,
                entrust_size   = order.entrust_size  ,
                entrust_date   = order.entrust_date  ,
                entrust_time   = order.entrust_time  ,
                fill_price     = fill_price    ,
                fill_size      = fill_size     ,
                status         = status,
                status_msg     = msg,
                order_id       = order.order_id
            )
        (new_order, trade)
    }

    def tryMatchOrders() : Unit = {

        for ( i <- this.cur_data.orders.indices) {
            var ord = this.cur_data.orders(i)
            if (!isFinished((ord.status))){
                if (ord.status == "New") {
                    val tmp = TradeApi.Order(
                        account_id     = ord.account_id    ,
                        code           = ord.code          ,
                        name           = ord.name          ,
                        entrust_no     = ord.entrust_no    ,
                        entrust_action = ord.entrust_action,
                        entrust_price  = ord.entrust_price ,
                        entrust_size   = ord.entrust_size  ,
                        entrust_date   = ord.entrust_date  ,
                        entrust_time   = ord.entrust_time  ,
                        fill_price     = ord.fill_price    ,
                        fill_size      = ord.fill_size     ,
                        status         = "Accepted"        ,
                        status_msg     = ""                ,
                        order_id       = ord.order_id
                    )
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

        cur_data.orders += TradeApi.Order(
            account_id     = account_id,
            code           = code,
            name           = code,
            entrust_no     = entrust_no,
            entrust_action = action,
            entrust_price  = price,
            entrust_size   = size,
            entrust_date   = date,
            entrust_time   = time,
            fill_price     = 0.0,
            fill_size      = 0L,
            status         = "New",
            status_msg     = "",
            order_id       = cur_data.cur_entrust_no
        )

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

        (OrderID(entrust_no, order_id), "")
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
                val new_order = TradeApi.Order(
                    account_id     = order.account_id    ,
                    code           = order.code          ,
                    name           = order.name          ,
                    entrust_no     = order.entrust_no    ,
                    entrust_action = order.entrust_action,
                    entrust_price  = order.entrust_price ,
                    entrust_size   = order.entrust_size  ,
                    entrust_date   = order.entrust_date  ,
                    entrust_time   = order.entrust_time  ,
                    fill_price     = order.fill_price    ,
                    fill_size      = order.fill_size     ,
                    status         = "Cancelled",
                    status_msg     = "",
                    order_id       = order.order_id
                )
                cur_data.orders(idx) = new_order
                status_ind += new_order
                (true, "cancelled")
            }
        }
    }
}

class SimTradeApi(_st: StraletTest) extends TradeApi {

    def log(data: Any) : Unit = _st.curSimContext.log(data)

    val accounts : Map[String, SimAccount] =
        _st.cfg.accounts.map { x=> x -> new SimAccount(this, x) }.toMap

    def init(balance: Double): Unit = {
        accounts.foreach( _._2.init(balance))
    }

    def getSimTime: (Int, Int) = _st.curSimContext.getTimeAsInt

    def st = _st

    def moveTo(next_tradingday: LocalDate) = {
        accounts.foreach(_._2.moveTo(next_tradingday))
    }

    def tryMatch(): Unit = {
        accounts foreach ( _._2.tryMatchOrders())
    }

    def getInd : (Seq[TradeApi.Order], Seq[TradeApi.Trade]) = {
        accounts.map( _._2.getInd())
                .foldLeft( (Seq[TradeApi.Order] (), Seq[TradeApi.Trade]() ) ) {
                    (x, i) => (x._1 ++ i._1, x._2 ++ i._2)
                }
    }

    override
    def queryAccountStatus() : (Seq[TradeApi.AccountInfo], String) = {
        val status = accounts.map{ case (k, v) => TradeApi.AccountInfo(k, "sim", k, "Connected", "", "sim") }
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
    def queryPosition(account_id : String) : (Seq[TradeApi.Position], String) = {
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
    def cancelOrder(account_id : String, code : String, entrust_no : String, order_id : Int) : (Boolean, String) = {
        val act = accounts.getOrElse(account_id, null)
        if (act != null)
            act.cancelOrder(code, entrust_no)
        else
            (false, "unkown account")
    }


    override def setCallback(callback: Callback): Unit = {

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
                (new String(baos.toByteArray(), "utf-8"), "")
            case _ =>
            (null, "-1,don't support")
        }
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


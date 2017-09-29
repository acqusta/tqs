package xtz.tquant.stra.backtest

import java.time.LocalDate

import scala.collection.mutable
import xtz.tquant.api.scala.TradeApi
import xtz.tquant.api.scala.TradeApi.{Callback, OrderID}
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
        val orders = mutable.ListBuffer[TradeApi.Order]()
        val trades = mutable.ListBuffer[TradeApi.Trade]()
        val balance = new Balance()
    }
}

class SimAccount(sim: SimTradeApi, account_id : String) {

    import SimAccount._

    var cur_data : TradeData = _
    var his_data = mutable.ListBuffer[TradeData]()

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
                            init_size    = 0,
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

    /**
      * 检查是否该订单是否可以成交，如果可以更新持仓的数量和成本价。
      * 当前只支持股票。
      *
      * @return (matched, err_msg)
      */
    def tryMatchDeal(code : String, price : Double, size : Long, action : String) : (Boolean, String) = {

        val is_future = {
            code.split('.').last match {
                case "SH" => false
                case "SZ" => false
                case _ => true
            }
        }
        action.toUpperCase match {
            case "BUY" =>
                val cost = price * size
                if (cur_data.balance.enable_balance >= cost) {
                    cur_data.balance.enable_balance -= cost
                    val pos = cur_data.positions.getOrElse( (code, "Long"), null)
                    if (pos == null) {
                        cur_data.positions += (code, "Long") -> TradeApi.Position(
                            account_id  = account_id,
                            code        = code,
                            name        = code,
                            current_size = size,
                            enable_size = if (is_future) size else 0,
                            init_size   = 0,
                            frozen_size = 0,
                            today_size  = 0,
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
                            enable_size = pos.enable_size + (if (is_future) size else 0),
                            init_size   = 0,
                            frozen_size = 0,
                            today_size  = 0,
                            side        = pos.side,
                            cost        = pos.cost + cost,
                            cost_price  = (pos.cost + cost) / pos.current_size,
                            last_price  = 0.0,
                            float_pnl   = 0.0,
                            close_pnl   = 0.0,
                            commission  = 0.0,
                            margin      = 0.0 )
                    }
                    (true, "")
                } else {
                    (false, "no enough avail")
                }
            case "SELL" =>
                val pos = cur_data.positions.getOrElse( (code, "Long"), null)
                if (pos != null && pos.enable_size >= size) {
                    val cost = price * size
                    cur_data.positions += (code, "Long") -> TradeApi.Position (
                        account_id  = pos.account_id,
                        code        = pos.code,
                        name        = pos.name,
                        current_size = pos.current_size - size,
                        enable_size = pos.enable_size - size,
                        init_size   = 0,
                        frozen_size = 0,
                        today_size  = 0,
                        side        = pos.side,
                        cost        = pos.cost - cost,
                        cost_price  = pos.cost_price,
                        last_price  =  0.0,
                        float_pnl   = 0.0,
                        close_pnl   = 0.0,
                        commission  = 0.0,
                        margin      = 0.0 )

                    cur_data.balance.enable_balance += cost
                    (true, "")
                } else {
                    (false, "no enough enable_size")
                }
            case "SHORT" =>
                val cost = price * size
                if (cur_data.balance.enable_balance >= cost) {
                    cur_data.balance.enable_balance -= cost
                    val pos = cur_data.positions.getOrElse( (code, "Short"), null)
                    if (pos == null) {
                        cur_data.positions += (code, "Short") -> TradeApi.Position(
                            account_id  = account_id,
                            code        = code,
                            name        = code,
                            current_size = size,
                            enable_size = if (is_future) size else 0,
                            init_size   = 0,
                            frozen_size = 0,
                            today_size  = 0,
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
                            enable_size = pos.enable_size + (if (is_future) size else 0),
                            init_size   = 0,
                            frozen_size = 0,
                            today_size  = 0,
                            side        = pos.side,
                            cost        = pos.cost + cost,
                            cost_price  = (pos.cost + cost) / pos.current_size,
                            last_price  =  0.0,
                            float_pnl   = 0.0,
                            close_pnl   = 0.0,
                            commission  = 0.0,
                            margin      = 0.0 )
                    }
                    (true, "")
                } else {
                    (false, "no enough avail")
                }
            case "COVER" =>
                val pos = cur_data.positions.getOrElse( (code, "Short"), null)
                if (pos != null && pos.enable_size >= size) {
                    val cost = price * size
                    cur_data.positions += (code, "Short") -> TradeApi.Position (
                        account_id  = pos.account_id,
                        code        = pos.code,
                        name        = pos.name,
                        current_size = pos.current_size - size,
                        enable_size = pos.enable_size - size,
                        init_size   = 0,
                        frozen_size = 0,
                        today_size  = 0,
                        side        = pos.side,
                        cost        = pos.cost - cost,
                        cost_price  = pos.cost_price,
                        last_price  =  0.0,
                        float_pnl   = 0.0,
                        close_pnl   = 0.0,
                        commission  = 0.0,
                        margin      = 0.0 )
                    cur_data.balance.enable_balance += cost
                    (true, "")
                } else {
                    (false, "no enough enable_size")
                }
            case _ => (false, s"unsupported action $action")
        }
    }

    /**
      * 保存订单，如果撮合成功，则保存成交，返回 entrust_no。
      * 注意：即使失败，也返回 不为空的entrust_no。
      *
      * @return entrust_no
      */
    def insertOrder(matched: Boolean, code : String, price : Double, size : Long, action : String) : String = {
        cur_data.cur_entrust_no += 1
        val entrust_no = s"SIM-${cur_data.cur_entrust_no}"

        val (date, time) = sim.getSimTime

        if (matched) {
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
                fill_price     = price,
                fill_size      = size,
                status         = "Filled",
                order_id       = 0
            )

            cur_data.trades += TradeApi.Trade(
                account_id      = account_id,
                code            = code,
                name            = code,
                entrust_no      = entrust_no,
                entrust_action  = action,
                file_date       = date,
                fill_price      = price,
                fill_size       = size,
                fill_no         = "0",
                fill_time       = time
            )
        }  else {
            cur_data.orders += TradeApi.Order(
                account_id      = account_id,
                code            = code,
                name            = code,
                entrust_no      = entrust_no,
                entrust_action  = action,
                entrust_price   = price,
                entrust_size    = size,
                entrust_date    = date,
                entrust_time    = time,
                fill_price      = price,
                fill_size       = size,
                status          = "Rejected",
                order_id        = 0
            )
        }

        entrust_no
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

        val (success, msg) = tryMatchDeal(code, price, size, action)
        val entrust_no = insertOrder(success, code, price, size, action)


        this.sim.log(f"place order: $date $time $code $price%.3f $size $action, ($entrust_no, $msg)")

        if (success)
            (OrderID(entrust_no, 0), "")
        else
            (null, msg)
    }

    def cancelOrder(code : String, entrust_no : String) : (Boolean, String) = {
        if (cur_data.orders.exists( _.entrust_no == entrust_no))
            (false, "already filled")
        else
            (false, "unknown entrust_no")
    }
}

class SimTradeApi(st: StraletTest) extends TradeApi {

    def log(data: Any) : Unit = st.curSimContext.log(data)

    val accounts : Map[String, SimAccount] =
        st.cfg.accounts.map { x=> x -> new SimAccount(this, x) }.toMap

    def init(balance: Double): Unit = {
        accounts.foreach( _._2.init(balance))
    }

    def getSimTime: (Int, Int) = st.curSimContext.getTimeAsInt

    def moveTo(next_tradingday: LocalDate) = {
        accounts.foreach(_._2.moveTo(next_tradingday))
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
        (null, "-1,don't support")
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


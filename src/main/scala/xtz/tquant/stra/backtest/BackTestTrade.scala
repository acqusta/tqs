package xtz.tquant.stra.backtest

import java.time.LocalDate

import scala.collection.mutable
import xtz.tquant.api.scala.TradeApi

object ExchangeAccount {

    class Balance {
        var init_balance = 0.0
        var enable_balance = 0.0
    }

    class TradeData {
        var trading_day    : LocalDate = _
        var cur_entrust_no : Int = 0
        val positions = mutable.HashMap[String, TradeApi.Position]()
        val orders = mutable.ListBuffer[TradeApi.Order]()
        val trades = mutable.ListBuffer[TradeApi.Trade]()
        val balance = new Balance()
    }
}

class ExchangeAccount(sim: ExchangeSimulator, account_id : String) {

    import ExchangeAccount._

    var cur_data : TradeData = _
    var his_data = mutable.ListBuffer[TradeData]()

    def init(init_balance: Double): Unit = {
        cur_data = new TradeData()
        cur_data.trading_day = null
        cur_data.balance.init_balance = init_balance
        cur_data.balance.enable_balance = init_balance
    }

    def moveOn(next_trading_day: LocalDate): Unit = {
        if (cur_data.trading_day == null) {
            cur_data.trading_day = next_trading_day
        }
        else {
            val data = new TradeData()
            data.trading_day = next_trading_day
            data.positions ++= cur_data.positions.filter(_._2.current_size != 0)
            data.balance.init_balance = cur_data.balance.enable_balance

            his_data += cur_data
            cur_data = data
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
    def tryMatchDeal(code : String, price : Double, size : Int, action : String) : (Boolean, String) = {
        action.toUpperCase match {
            case "BUY" =>
                val cost = price * size
                if (cur_data.balance.enable_balance >= cost) {
                    cur_data.balance.enable_balance -= cost
                    val pos = cur_data.positions.getOrElse( code, null)
                    if (pos == null) {
                        cur_data.positions += code -> TradeApi.Position(
                            account_id  = account_id,
                            code        = code,
                            name        = code,
                            current_size = size,
                            enable_size = 0,
                            side        = "Long",
                            cost        = cost,
                            cost_price  = price,
                            last_price  =  0.0,
                            holding_pnl = 0.0,
                            margin      = 0.0)
                    } else {
                        cur_data.positions += code -> TradeApi.Position (
                            account_id  = pos.account_id,
                            code        = pos.code,
                            name        = pos.name,
                            current_size = pos.current_size + size,
                            enable_size = pos.enable_size,
                            side        = pos.side,
                            cost        = pos.cost + cost,
                            cost_price  = (pos.cost + cost) / pos.current_size,
                            last_price  =  0.0,
                            holding_pnl = 0.0,
                            margin      = 0.0
                        )
                    }
                    (true, "")
                } else {
                    (false, "no enough avail")
                }
            case "SELL" =>
                val pos = cur_data.positions.getOrElse( code, null)
                if (pos != null && pos.enable_size > size) {
                    val cost = price * size
                    cur_data.positions += code -> TradeApi.Position (
                        account_id  = pos.account_id,
                        code        = pos.code,
                        name        = pos.name,
                        current_size = pos.current_size - size,
                        enable_size = pos.enable_size - size,
                        side        = pos.side,
                        cost        = pos.cost - cost,
                        cost_price  = pos.cost_price,
                        last_price  =  0.0,
                        holding_pnl = 0.0,
                        margin      = 0.0
                    )
                    (true, "")
                } else {
                    (false, "no enough enable_size")
                }
            case _ => (false, "unsupported action")
        }
    }

    /**
      * 保存订单，如果撮合成功，则保存成交，返回 entrust_no。
      * 注意：即使失败，也返回 不为空的entrust_no。
      *
      * @return entrust_no
      */
    def insertOrder(matched: Boolean, code : String, price : Double, size : Int, action : String) : String = {
        cur_data.cur_entrust_no += 1
        val entrust_no = s"SIM-${cur_data.cur_entrust_no}"

        val (date, time) = sim.getSimTime()

        if (matched) {
            cur_data.orders += TradeApi.Order(
                account_id      = account_id,
                code            = code,
                name            = code,
                entrust_no      = entrust_no,
                entrust_action = action,
                entrust_price  = price,
                entrust_size   = size,
                entrust_time   = time,
                fill_price     = price,
                fill_size      = size,
                status         = "Filled"
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
                entrust_time    = time,
                fill_price      = price,
                fill_size       = size,
                status          = "Rejected"
            )
        }

        entrust_no
    }

    def placeOrder(code : String, price : Double, size : Int, action : String) : (String, String) = {

        val (success, msg) = tryMatchDeal(code, price, size, action)
        val entrust_no = insertOrder(success, code, price, size, action)

        if (success)
            (entrust_no, "")
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

class ExchangeSimulator(runner: BackTestRunner) extends TradeApi {

    val accounts : Map[String, ExchangeAccount] =
        runner.accounts.map { case x=> x -> new ExchangeAccount(this, x) }.toMap

    def init(balance: Double): Unit = {
        accounts.foreach( _._2.init(balance))
    }

    def getSimTime(): (Int, Int) = runner.getSimTimeAsInt()

    def moveOn(next_tradingday: LocalDate) = {
        accounts.foreach(_._2.moveOn(next_tradingday))
    }

    def queryAccountStatus() : (Seq[TradeApi.AccountInfo], String) = {
        val status = accounts.map{ case (k, v) => TradeApi.AccountInfo(k, "sim", k, "Connected", "") }
        (status.toSeq, "")
    }

    def queryBalance(account_id : String) : (TradeApi.Balance, String) = {
        val act = accounts.getOrElse(account_id, null)
        if (act != null)
            act.queryBalance()
        else
            (null, "unkown account")
    }
    def queryOrders(account_id : String) : (Seq[TradeApi.Order], String) = {
        val act = accounts.getOrElse(account_id, null)
        if (act != null)
            act.queryOrders()
        else
            (null, "unkown account")
    }
    def queryTrades(account_id : String) : (Seq[TradeApi.Trade], String) = {
        val act = accounts.getOrElse(account_id, null)
        if (act != null)
            act.queryTrades()
        else
            (null, "unkown account")
    }
    def queryPosition(account_id : String) : (Seq[TradeApi.Position], String) = {
        val act = accounts.getOrElse(account_id, null)
        if (act != null)
            act.queryPosition()
        else
            (null, "unkown account")
    }
    def placeOrder(account_id : String, code : String, price : Double, size : Int, action : String) : (String, String) = {
        val act = accounts.getOrElse(account_id, null)
        if (act != null)
            act.placeOrder(code, price, size, action)
        else
            (null, "unkown account")
    }
    def cancelOrder(account_id : String, code : String, entrust_no : String) : (Boolean, String) = {
        val act = accounts.getOrElse(account_id, null)
        if (act != null)
            act.cancelOrder(code, entrust_no)
        else
            (false, "unkown account")
    }
}


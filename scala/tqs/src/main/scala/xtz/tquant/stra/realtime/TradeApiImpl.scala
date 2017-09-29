package xtz.tquant.stra.realtime

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import akka.actor.ActorRef
import xtz.tquant.api.scala.{TQuantApi, TradeApi}
import xtz.tquant.api.scala.TradeApi._


class TradeApiImpl(actor : ActorRef, addr : String) extends TradeApi {


    val tapi = new TQuantApi(addr).tradeApi

    tapi.setCallback(new Callback {
        override def onOrderTrade(trade: Trade): Unit = {
            actor ! trade
        }

        override def onOrderStatus(order: Order): Unit = {
            actor ! order
        }

        override def onAccountStatus(account: AccountInfo): Unit = {
            actor ! account
        }
    })

    override
    def queryAccountStatus() : (Seq[TradeApi.AccountInfo], String) = {
        tapi.queryAccountStatus()
    }

    override
    def queryBalance(account_id : String) : (TradeApi.Balance, String) = {
        tapi.queryBalance(account_id)
    }

    override
    def queryOrders(account_id : String) : (Seq[TradeApi.Order], String) = {
        tapi.queryOrders(account_id)
    }

    override
    def queryTrades(account_id : String) : (Seq[TradeApi.Trade], String) = {
        tapi.queryTrades(account_id)
    }

    override
    def queryPosition(account_id : String) : (Seq[TradeApi.Position], String) = {
        tapi.queryPosition(account_id)
    }

    private val _log_df = DateTimeFormatter.ofPattern("yyyyMMdd HHmmss.SSS")

    override
    def placeOrder(account_id : String, code : String, price : Double, size : Long, action : String, order_id: Int) : (OrderID, String) = {

        println(LocalDateTime.now.format(_log_df) + " " + s"place order($account_id, $code, $price, $size, $action, $order_id)")

        tapi.placeOrder(account_id = account_id, code = code, price = price, size = size,
            action=action, order_id=order_id)
    }

    override
    def cancelOrder(account_id : String, code : String, entrust_no : String, order_id : Int) : (Boolean, String) = {

        println(LocalDateTime.now.format(_log_df) + " " + s"cancel order($account_id, $code, $entrust_no, $order_id)")

        tapi.cancelOrder(account_id, code, entrust_no, order_id)
    }


    override def setCallback(callback: Callback): Unit = {
        // TODO:
    }

    override def query(account_id: String, command: String, params: String): (String, String) = {
        tapi.query(account_id, command, params)
    }
}


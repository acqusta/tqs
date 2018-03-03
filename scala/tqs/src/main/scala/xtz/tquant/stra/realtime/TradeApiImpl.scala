package xtz.tquant.stra.realtime

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import akka.actor.ActorRef
import com.acqusta.tquant.api.TQuantApi
import com.acqusta.tquant.api.scala.{ScalaTradeApi, TradeApi}
import com.acqusta.tquant.api.scala.TradeApi._
import xtz.tquant.stra.stralet.TqsTradeApi
import xtz.tquant.stra.stralet.TqsTradeApi.NetPosition


class TradeApiImpl(actor : ActorRef, addr : String) extends TqsTradeApi {


    private val tapi = new ScalaTradeApi(new TQuantApi(addr).getTradeApi)

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
    def queryAccountStatus : (Seq[AccountInfo], String) = {
        tapi.queryAccountStatus
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
    def queryPositions(account_id : String) : (Seq[TradeApi.Position], String) = {
        tapi.queryPositions(account_id)
    }

    private val _log_df = DateTimeFormatter.ofPattern("yyyyMMdd HHmmss.SSS")

    override
    def placeOrder(account_id : String, code : String, price : Double, size : Long, action : String, order_id: Int) : (OrderID, String) = {

        println(LocalDateTime.now.format(_log_df) + " " + s"place order($account_id, $code, $price, $size, $action, $order_id)")

        tapi.placeOrder(account_id, code, price, size, action, order_id)
    }

    override
    def cancelOrder(account_id : String, code : String, entrust_no : String) : (Boolean, String) = {

        println(LocalDateTime.now.format(_log_df) + " " + s"cancel order($account_id, $code, $entrust_no)")

        tapi.cancelOrder(account_id, code, entrust_no)
    }

    override
    def cancelOrder(account_id : String, code : String, order_id : Int) : (Boolean, String) = {

        println(LocalDateTime.now.format(_log_df) + " " + s"cancel order($account_id, $code, $order_id)")

        tapi.cancelOrder(account_id, code, order_id)
    }


    override
    def cancelOrder(account_id : String, code : String, entrust_no : String, order_id : Int) : (Boolean, String) = {

        println(LocalDateTime.now.format(_log_df) + " " + s"cancel order($account_id, $code, $order_id)")

        tapi.cancelOrder(account_id, code, entrust_no, order_id)
    }

    override def setCallback(callback: Callback): Unit = {
        // TODO:
    }

    override def query(account_id: String, command: String, params: String): (String, String) = {
        tapi.query(account_id, command, params)
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

}


package com.acqusta.tquant.api.scala

import com.acqusta.tquant.api.TradeApi.CallResult
import com.acqusta.tquant.api.{TradeApi => JavaTradeApi}


object TradeApi {
    type Callback = JavaTradeApi.Callback
    type AccountInfo = JavaTradeApi.AccountInfo
    type Balance = JavaTradeApi.Balance
    type OrderStatus = JavaTradeApi.OrderStatus
    type EntrustAction = JavaTradeApi.EntrustAction
    type Order = JavaTradeApi.Order
    type Trade = JavaTradeApi.Trade
    type Side = JavaTradeApi.Side
    type Position = JavaTradeApi.Position
    type OrderID = JavaTradeApi.OrderID
}

trait TradeApi {

    import TradeApi._

    def queryAccountStatus : (Seq[AccountInfo], String)
    def queryBalance (account_id : String) : (Balance, String)
    def queryOrders (account_id : String) : (Seq[Order], String)
    def queryTrades (account_id : String) : (Seq[Trade], String)
    def queryPositions ( account_id : String) : (Seq[Position], String)

    def placeOrder(account_id: String, code: String, price: Double, size: Long, action: String, order_id: Int = 0): (OrderID, String)
    def cancelOrder(account_id: String, code: String, order_id: Int): (Boolean, String)
    def cancelOrder(account_id: String, code: String, entrust_no: String): (Boolean, String)
    def cancelOrder(account_id: String, code: String, entrust_no: String, order_id: Int): (Boolean, String)

    def query(account_id: String, command: String, params: String): (String, String)

    def setCallback(callback: Callback)
}


class ScalaTradeApi (tapi : JavaTradeApi) extends TradeApi {

    import TradeApi._

    override
    def queryAccountStatus : (Seq[AccountInfo], String) = {
        val r = tapi.queryAccountStatus()
        (r.value, r.msg)
    }

    override
    def queryBalance(account_id : String) : (Balance, String) = {
        val r = tapi.queryBalance(account_id)
        (r.value, r.msg)
    }

    override
    def queryOrders(account_id : String) : (Seq[Order], String) = {
        val r = tapi.queryOrders(account_id)
        (r.value, r.msg)
    }

    override
    def queryTrades(account_id : String) : (Seq[Trade], String) = {
        val r = tapi.queryTrades(account_id)
        (r.value, r.msg)
    }

    override
    def queryPositions(account_id : String) : (Seq[Position], String) = {
        val r = tapi.queryPositions(account_id)
        (r.value, r.msg)
    }

    override
    def placeOrder(account_id : String, code : String, price : Double, size : Long, action : String, order_id: Int) : (OrderID, String) = {
        val r = tapi.placeOrder(account_id, code, price, size, action, order_id)
        (r.value, r.msg)
    }

    override
    def cancelOrder(account_id : String, code : String, entrust_no : String) : (Boolean, String) = {
        val r = tapi.cancelOrder(account_id, code, entrust_no)
        (r.value, r.msg)
    }

    override
    def cancelOrder(account_id : String, code : String, order_id : Int) : (Boolean, String) = {
        val r = tapi.cancelOrder(account_id, code, order_id)
        (r.value, r.msg)
    }

    override
    def cancelOrder(account_id: String, code: String, entrust_no: String, order_id: Int): (Boolean, String) = {
        var r : CallResult[java.lang.Boolean] = null
        if (entrust_no!=null && entrust_no.nonEmpty)
            r = tapi.cancelOrder(account_id, code, entrust_no)
        else if (order_id != 0)
            r = tapi.cancelOrder(account_id, code, order_id)

        if (r != null)
            (r.value, r.msg)
        else
            (false, "empty entrust_no and order_id")
    }

    def query(account_id: String, command: String, params: String): (String, String) = {
        val r = tapi.query(account_id, command, params)
        (r.value, r.msg)
    }

    def setCallback(callback: Callback): Unit = {
        tapi.setCallback(callback)
    }

}
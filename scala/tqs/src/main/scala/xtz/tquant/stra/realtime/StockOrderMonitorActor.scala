package xtz.tquant.stra.realtime

import java.time.{DayOfWeek, LocalDateTime}

import akka.actor.{Actor, ActorRef}
import xtz.tquant.api.scala.{TQuantApi, TradeApi}
import xtz.tquant.stra.utils.TimeUtils._
import xtz.tquant.stra.realtime.Config.RTConfig

import scala.concurrent.duration.{DurationInt, _}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

object StockOrderMonitorActor {

    case class RegisterStraletActor(actor: ActorRef)
    case class InitReq(rt_conf : RTConfig)

}
/**
  * Created by txu on 2017/9/6.
  */
class StockOrderMonitorActor extends Actor{

    import StockOrderMonitorActor._

    val logger = org.slf4j.LoggerFactory.getLogger(this.getClass.getSimpleName)

    var tapi : TradeApi = _

    val actors = mutable.ArrayBuffer[ActorRef]()

    class OrderInfo {
        var order : TradeApi.Order = _
        val trades = mutable.HashMap[String, TradeApi.Trade] ()
        var should_report : Boolean = false
    }

    class MyAccountInfo () {
        val account_id    : String = ""
        var connected     : Boolean = false
        var orders  = mutable.HashMap[String, OrderInfo]()
    }

    val accounts = mutable.HashMap[String, MyAccountInfo ]()

    override def receive = {

        case req: InitReq                      => onInit(req)
        case RegisterStraletActor(actor)    => actors += actor
        case trade  : TradeApi.Trade        => onTrade(trade)
        case order  : TradeApi.Order        => onOrderStatus(order)
        case account: TradeApi.AccountInfo  => onAccountStatus(account)
        case "CHECK_ORDER_TIMER"            => onCheckOrderTimer()
        case "CHECK_TRADE_TIMER"            => onCheckTradeTimer()
    }

    def onInit(req : InitReq) = {
        logger.info("Start StockOrderMonitor")

        tapi = new TQuantApi(req.rt_conf.tqc.addr).tradeApi

        tapi.setCallback(new TradeApi.Callback {
            override def onOrderTrade(trade: TradeApi.Trade): Unit = {
                self ! trade
            }

            override def onOrderStatus(order: TradeApi.Order): Unit = {
                self ! order
            }

            override def onAccountStatus(account: TradeApi.AccountInfo): Unit = {
                self ! account
            }
        })
        this.context.system.scheduler.scheduleOnce(1 seconds, self, "CHECK_ORDER_TIMER")
    }

    def onAccountStatus(account : TradeApi.AccountInfo) = {
    }

    def onTrade(trade : TradeApi.Trade) = {

    }

    def onOrderStatus( order : TradeApi.Order) = {

    }

    def updateAccountStatus() : Unit = {
        val (new_accounts, msg) = tapi.queryAccountStatus()
        if (new_accounts == null) return

        for ( new_act <- new_accounts if new_act.account_type == "stock" ) {
            var old_act = this.accounts.getOrElse(new_act.account_id, null)
            if (old_act == null) {
                old_act = new MyAccountInfo()
                this.accounts += new_act.account_id -> old_act
            }
            old_act.connected = new_act.status == "Connected"
        }

        val removed_id = accounts.keySet.diff(new_accounts map (_.account_id) toSet)
        removed_id.foreach( accounts.remove(_))
    }

    def isFinished(status : String) : Boolean = {
        status match {
            case "Filled"       => true
            case "Rejected"     => true
            case "Cancelled"    => true
            case _              => false
        }
    }

    def notifyOrderStatus(order : TradeApi.Order) = {
        actors foreach ( _ ! order)
    }

    def notifyOrderTrade(trade : TradeApi.Trade) = {
        actors foreach ( _ ! trade)
    }

    def updateOrder(act: MyAccountInfo) : Unit = {
        val (orders, msg) = tapi.queryOrders(act.account_id)
        if (orders == null) {
            logger.error("queryOrders failed: " + msg)
            return
        }

        // Notification should have the following order:
        //    OrderStatus -> New
        //    OrderStatus -> Accepted
        //    Trade
        //    OrderStatus -> Filled
        // So don't notify status here except when order is New or Rejected.

        for( ord <- orders if ord.entrust_no.nonEmpty ) {
            var tmp = act.orders.getOrElse(ord.entrust_no, null)
            if (tmp != null) {
                if (tmp.order == null) {
                    tmp.order = ord
                    tmp.should_report = true
                } else if ( tmp.order.status != ord.status ) {
                    tmp.order = ord
                    tmp.should_report = true
                }
            } else {
                tmp = new OrderInfo()
                tmp.order = ord
                act.orders += tmp.order.entrust_no -> tmp

                if (tmp.order.status == "New" ||
                    tmp.order.status == "Rejected")
                {
                    notifyOrderStatus(tmp.order)
                } else {
                    tmp.should_report = true
                }
            }
        }
    }

    def updateTrade(act : MyAccountInfo) : Unit = {
        val (all_trades, msg) = tapi.queryTrades(act.account_id)
        if (all_trades == null) {
            logger.error("queryTrades failed: " + msg)
            return
        }

        for ( (entrust_no, trades) <- all_trades.groupBy(_.entrust_no)) {
            var info = act.orders.getOrElse(entrust_no, null)
            if (info != null) {
                for (trd <- trades ) {
                    if (!info.trades.contains(trd.fill_no)) {
                        info.trades += trd.fill_no -> trd
                        notifyOrderTrade(trd)
                    }
                }
            } else {
                info = new OrderInfo
                info.trades ++= trades.map( x => x.fill_no -> x).toMap
                act.orders += entrust_no -> info
            }
        }

        for ( (_, ord) <- act.orders if ord.should_report)
            notifyOrderStatus (ord.order)
    }

    def isWorkingTime: Boolean = {

        val now = LocalDateTime.now()

        now.getDayOfWeek match{
            case DayOfWeek.SATURDAY  => false
            case DayOfWeek.SUNDAY    => false
            case _                   =>
                val t = now.toLocalTime.toHumanSecond
                if ((t >= 93000 && t < 113000) || ( t>= 130000 && t <150000) )
                    true
                else
                    false
        }
    }

    def onCheckOrderTimer() = {
        if (isWorkingTime) {
            try {
                updateAccountStatus()
                for ( act <- accounts if act._2.connected) updateOrder(act._2)

            } catch {
                case t : Throwable => t.printStackTrace()
            }
        }
        context.system.scheduler.scheduleOnce(2 seconds, self, "CHECK_TRADE_TIMER")
    }

    def onCheckTradeTimer() = {
        if (isWorkingTime) {
            try {
                for (act <- accounts if act._2.connected) updateTrade(act._2)
            } catch {
                case t : Throwable => t.printStackTrace()
            }
        }
        context.system.scheduler.scheduleOnce(1 seconds, self, "CHECK_TRADE_TIMER")
    }


}


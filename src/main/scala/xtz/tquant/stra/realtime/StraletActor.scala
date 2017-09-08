package xtz.tquant.stra.realtime

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import akka.actor.{Actor, Cancellable}
import xtz.tquant.api.scala.DataApi.{Bar, MarketQuote}
import xtz.tquant.api.scala.TradeApi.{Order, Trade}
import xtz.tquant.api.scala.{DataApi, TradeApi}
import xtz.tquant.stra.stralet.{Stralet, StraletContext}

import scala.concurrent.duration.{DurationInt, _}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable


case class StraletConfig (
    id            : String,
    stralet_class : String,
    parameters    : Map[String, Any]
)


object StraletActor {

    case class InitReq(cfg : StraletConfig)
    case class InitRsp(result: Boolean, msg : String)

    case class PostEvent(evt: String, data: Any)

    case class Timer(id: Int, delay: Int, data: Any)

    case class BarInd (cycle: String, bar : Bar)
}

/**
  * Created by txu on 2017/9/6.
  */
class StraletActor extends Actor with StraletContext{

    import StraletActor._

    var logger  : org.slf4j.Logger = _
    var stralet : Stralet          = _
    var tapi    : TradeApiImpl     = _
    var dapi    : DataApiImpl      = _
    var cfg     : StraletConfig    = _

    override def receive = {

        case InitReq(cfg : StraletConfig)             => onInitReq(cfg : StraletConfig)

        case PostEvent(evt : String, data : Any)      => onPostEvent(evt, data)

        case q     : MarketQuote                      => onMarketQuote(q)
        case ind   : BarInd                           => onBar(ind)
        case trade : Trade                            => onTrade(trade)
        case order : Order                            => onOrderStatus(order)

        case timer : Timer                            => onTimer(timer)
    }

    def onInitReq(cfg : StraletConfig) = {

        try {
            this.cfg = cfg
            logger = org.slf4j.LoggerFactory.getLogger(cfg.id)

            logger.info("create stralet: " + cfg.id + "," + cfg.stralet_class)

            val clazz = Class.forName(cfg.stralet_class).asInstanceOf[Class[Stralet]]
            val arg_classes = new Array[Class[_]](0)
            val constructor = clazz.getDeclaredConstructor(arg_classes : _*)

            stralet = constructor.newInstance()

            tapi = new TradeApiImpl(this.self, Config.conf.tqc.addr)
            dapi = new DataApiImpl(this.self, Config.conf.tqc.addr)

            stralet.onInit(this)
            sender() ! InitRsp(result = true, null)
        }catch {
            case t : Throwable =>
                t.printStackTrace()
                sender() ! InitRsp(result = false, t.getMessage)
        }
    }

    def onPostEvent(evt: String, data: Any) = {
        try {
            stralet.onEvent(evt, data)
        } catch {
            case t : Throwable => t.printStackTrace()
        }
    }

    def onMarketQuote( q : MarketQuote) = {
        try {
            stralet.onQuote(q)
        } catch {
            case t : Throwable => t.printStackTrace()
        }
    }

    def onBar( ind : BarInd) = {
        try {
            stralet.onBar(ind.cycle, ind.bar)
        } catch {
            case t : Throwable => t.printStackTrace()
        }
    }

    def onTrade(trade : Trade) = {
        try {
            stralet.onOrderTrade(trade)
        } catch {
            case t : Throwable => t.printStackTrace()
        }
    }

    def onOrderStatus( order : Order) = {
        try {
            stralet.onOrderStatus(order)
        } catch {
            case t : Throwable => t.printStackTrace()
        }
    }

    def onTimer(timer : Timer) = {
        try {
            stralet.onTimer(timer.id, timer.data)
        } catch {
            case t : Throwable => t.printStackTrace()
        }
    }

    private val _df = DateTimeFormatter.ofPattern("yyyyMMdd HHmmssSSS")

    override def getTimeAsInt : (Int, Int) = {
        val now = LocalDateTime.now

//        val str = now.format(_df)
//        val ss = str.split(" ")
//        (Integer.parseInt(ss(0)), Integer.parseInt(ss(1)))

        (now.getYear * 10000 + now.getMonthValue * 100 + now.getDayOfMonth,
            now.getHour * 10000000 + now.getMinute * 100000 + now.getSecond * 1000 + now.getNano / 1000000)
    }

    override def getTime : LocalDateTime = LocalDateTime.now

    val timer_map = mutable.HashMap[Int, Cancellable]()

    override def setTimer(id: Int, delay: Int, data: Any) : Unit = {

        val timer = Timer(id, delay, data)

        val old_handler = timer_map.remove(id).orNull
        if (old_handler!=null)
            if (!old_handler.isCancelled) old_handler.cancel()

        val timer_handler = this.context.system.scheduler.schedule( delay millis, delay millis, self, timer)
        timer_map += id -> timer_handler
    }


    override def killTimer(id: Int) : Unit = {
        val handler = timer_map.remove(id).orNull
        if (handler != null)
            if (!handler.isCancelled) handler.cancel()
    }

    override def postEvent(evt: String, data : Any) : Unit = {
        self ! PostEvent(evt, data)
    }

    override def getTradeApi : TradeApi = tapi

    override def getDataApi : DataApi = dapi

    override def log(data : Any) : Unit = {
        logger.info(data.toString)
    }

    override def getParameters[T: Manifest](name : String, def_value: T) : T = {
        cfg.parameters.getOrElse(name, def_value).asInstanceOf[T]
    }
}

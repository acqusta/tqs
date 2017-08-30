package xtz.tquant.stra.backtest

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.util

import scala.io.Source
import xtz.tquant.api.scala.{DataApi, TradeApi}
import xtz.tquant.stra.backtest.Runner.RunnerConfig
import xtz.tquant.stra.stralet.{Stralet, StraletContext}
import xtz.tquant.stra.utils.JsonHelper
import xtz.tquant.stra.utils.TimeUtils._

import scala.collection.mutable

object Runner {

    case class StraletInstanceConfig(
        id : String,
        stralet_class: String,
        parameters: Map[String, Any]
    )

    case class BackTestConfig (
        date_range: Seq[Int],
        accounts:   Seq[String]
    )
    case class BackTestStraletConfig (
        stralet : StraletInstanceConfig,
        backtest : BackTestConfig
    )

    def createFromFile(path: String) : Runner = {
        try {
            var text = Source.fromFile(path).mkString

            val config = JsonHelper.deserialize[BackTestStraletConfig](text)

            val clazz = Class.forName(config.stralet.stralet_class).asInstanceOf[Class[Stralet]]

            val date_range = config.backtest.date_range
            val first_date = if (date_range(0) != 0 ) date_range(0).toLocalDate else LocalDate.now()
            val last_date  = if (date_range(1) != 0 ) date_range(1).toLocalDate else LocalDate.now()

            val cfg = RunnerConfig(config.stralet.id, clazz, //config.stralet.universe,
                config.backtest.accounts, config.stralet.parameters, first_date, last_date)

            val runner = new Runner(cfg)
            runner.init()
            runner
        }catch{
            case t: Throwable => t.printStackTrace(); null
        }
    }

    case class RunnerConfig (
        servlet_id    : String,
        servlet_class : Class[Stralet],
//        universe : Seq[String],
        accounts   : Seq[String],
        parameters : Map[String, Any],
        first_date : LocalDate,
        last_date  : LocalDate
    )

}

class Runner (_cfg: RunnerConfig){

    import Runner._

    val tapi = new SimTradeApi(this)
    val dapi = new SimDataApi(this)

    //var trading_day: Int = _

    def cfg : RunnerConfig = _cfg

    def init(): Unit = {
        dapi.init()
        tapi.init(1000000.0) // Set init_balance
    }

    def run(): Unit = {

        val days = cfg.last_date.toEpochDay - cfg.first_date.toEpochDay

        var day = cfg.first_date
        for ( i <- 0L to days ) {
            if (dapi.calendar.contains(day))
                runOneDay( day)
            day = day.plusDays(1)
        }

        val order_file = s"SimOrder-${cfg.servlet_id}-${cfg.first_date.toHumanDate}-${cfg.last_date.toHumanDate}-${System.currentTimeMillis}.csv"
        tapi.saveOrder(order_file)
    }

    private var sc : SimContext = _

//    def getCurSimTimeAsInt : (Int, Int) = {
//        assert (sc != null, "_sc shouldn't be null")
//        sc.getTimeAsInt
//    }
//
//    def getCurTradingDay : Int = sc.trading_day

    def curSimContext : SimContext = sc

    def runOneDay(day : LocalDate): Unit = {

        val begin_time = LocalDateTime.of(day, LocalTime.of( 9, 30,0))
        val end_time   = LocalDateTime.of(day, LocalTime.of(15,  0,0))
        val time_1130  = LocalDateTime.of(day, LocalTime.of(11, 30,0))
        val time_1300  = LocalDateTime.of(day, LocalTime.of(13,  0,0))

        tapi.moveTo(day)
        dapi.moveTo(day)

        val arg_classes = new Array[Class[_]](0)
        val constructor = cfg.servlet_class.getDeclaredConstructor(arg_classes : _*)
        val stralet = constructor.newInstance()


        sc = new SimContext(this, day.toHumanDate, begin_time)

        stralet.onInit(sc)

        var bar_time = begin_time
        var timer_time = end_time

        while (sc.getTime.isBefore(end_time)) {
            for (code <- dapi.subed_codes) {
                val quote = dapi.nextQuote(code)
                if (quote != null)
                    stralet.onQuote(quote)
            }

            for (code <- dapi.subed_codes) {
                val bar = dapi.nextBar(code, "1m")
                if (bar!=null) {
                    stralet.onBar(bar)
                }
            }

            sc.executeTimer(stralet)
            sc.moveToNextSimTime()
        }

        stralet.onFini()
        sc = null
    }

}

class SimContext(runner: Runner, trading_day: Int, var sim_time: LocalDateTime) extends StraletContext {

    private val _df = DateTimeFormatter.ofPattern("yyyyMMdd HHmmssSSS")

    case class Timer(id: Int, delay: Int, data: Any, var next_time : LocalDateTime)

    private val timer_map = mutable.HashMap[Int, Timer]()

    private val evt_list = mutable.ArrayBuffer[(String, Any)]()

    def getTradingDay = trading_day

    override def getTimeAsInt : (Int, Int) = {
        val str = sim_time.format(_df)
        val ss = str.split(" ")
        (Integer.parseInt(ss(0)), Integer.parseInt(ss(1)))
    }

    override def getTime : LocalDateTime = sim_time

    override def setTimer(id: Int, delay: Int, data: Any) : Unit = {
        timer_map += id -> Timer(id, delay, data, getTime.plusNanos(delay*1000000))
    }

    override def killTimer(id: Int) : Unit = {
        timer_map.remove(id)
    }

    override def postEvent(evt: String, msg: Any) : Unit = {
        evt_list.synchronized{
            evt_list.append((evt, msg))
        }
    }

    override def getTradeApi : TradeApi = runner.tapi

    override def getDataApi : DataApi = runner.dapi

    /**
      * TODO: Save to file
      * @param data
      */
    override def log(data: Any) :Unit = {
        println(data)
    }

    override def getParameters[T: Manifest](name : String, def_value: T) : T = {

        // FIXME:
        runner.cfg.parameters.getOrElse(name, def_value).asInstanceOf[T]
    }

    def calcNextTimerTime() : LocalDateTime = {
        if (timer_map.nonEmpty)
            timer_map.reduce((a,b) => if (a._2.next_time.isBefore( b._2.next_time)) a else b)._2.next_time
        else
            null
    }

    def executeTimer(stralet : Stralet) = {

        val timer = mutable.ArrayBuffer[Timer]()
        for ( (_, t) <- timer_map) {
            if (t.next_time.isBefore(sim_time))
                timer.append(t)
        }

        for ( t <-timer) {
            if (timer_map.getOrElse(t.id, null) != null)
                stralet.onTimer(t.id, t.data)
        }
    }

    def moveToNextSimTime () : LocalDateTime = {
        val time = {
            val time1 = runner.dapi.calcNextQuoteTime()
            val time2 = calcNextTimerTime()
            if (time1 != null && time2 !=null) {
                if (time1.isBefore(time2)) time1 else time2
            } else {
                if (time1!=null) time1 else if (time2!=null) time2 else null
            }
        }

        // FIXME:
        assert(time != null, "time shouldn't be null")
        sim_time = time

        time
    }
}
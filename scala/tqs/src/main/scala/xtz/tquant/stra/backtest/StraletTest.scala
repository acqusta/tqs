package xtz.tquant.stra.backtest

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, LocalTime}

import xtz.tquant.stra.backtest.StraletTest.{BackTestConfig, _StraletTestConfig}
import xtz.tquant.stra.stralet.Stralet
import xtz.tquant.stra.utils.JsonHelper
import xtz.tquant.stra.utils.TimeUtils._

import scala.io.Source

object StraletTest {
    case class _StraletTestConfig(
        servlet_id    : String,
        servlet_class : Class[Stralet],
        accounts      : Seq[String],
        parameters    : Map[String, Any],
        first_date    : LocalDate,
        last_date     : LocalDate,
        data_level    : String
    )

    case class StraletInfo (
        id            : String,
        stralet_class : String,
        parameters    : Map[String, Any]
    )

    case class StraletConfig(
        stralet : StraletInfo
    )

    case class TqcConfig (
        addr  : String
    )

    case class BastTestOption(
         date_range  : Seq[Int],
         accounts    : Seq[String],
         data_level  : String

    )
    case class BackTestConfig(
        tqc         : TqcConfig,
        backtest    : BastTestOption
     )

    def parseTestConfig( text : String) : BackTestConfig = {
        //var text = Source.fromFile(path).mkString
        JsonHelper.deserialize[StraletTest.BackTestConfig](text)
    }
}

class StraletTest(_container: Container, _cfg: _StraletTestConfig) {

    val tapi = new SimTradeApi(this)
    val dapi = new SimDataApi(this)

    def cfg : _StraletTestConfig = _cfg

    def container : Container = _container

    private var sc : SimStraletContext = _

    def curSimContext : SimStraletContext = sc

    def init(): Unit = {
        dapi.init()
        tapi.init(1000000.0) // Set init_balance
    }

    def run(): Unit = {

        println( LocalDateTime.now())

        cfg.data_level match {
            case "tk" => runTkOr1m()
            case "1m" => runTkOr1m()
            case "1d" => run1d()
        }

        println( LocalDateTime.now())

        val df = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmssSSS")
        val time_lable = LocalDateTime.now().format(df)

        val order_file = s"Trade-${cfg.servlet_id}-${cfg.first_date.toHumanDate}-${cfg.last_date.toHumanDate}-${time_lable}.csv"
        tapi.saveOrder(order_file)
    }

    /**
      * 日内回测
      */
    private def runTkOr1m() : Unit = {
        val days = cfg.last_date.toEpochDay - cfg.first_date.toEpochDay
        var day = cfg.first_date
        for ( i <- 0L to days ) {
            if (dapi.calendar.contains(day))
                runOneDay( day)
            day = day.plusDays(1)
        }
    }

    private def runOneDay(day : LocalDate): Unit = {

        val begin_time = LocalDateTime.of(day, LocalTime.of( 0, 0,0))
        val end_time   = LocalDateTime.of(day, LocalTime.of(15, 0,0))

        tapi.moveTo(day)
        dapi.moveTo(day)

        val arg_classes = new Array[Class[_]](0)
        val constructor = cfg.servlet_class.getDeclaredConstructor(arg_classes : _*)
        val stralet = constructor.newInstance()


        sc = new SimStraletContext(this, day.toHumanDate, begin_time)

        stralet.onInit(sc)

        var bar_time   = begin_time
        var timer_time = end_time

        while (sc.moveToNextSimTime() != null && sc.getTime.isBefore(end_time)) {

            val quotes = dapi.subed_codes map { x => dapi.nextQuote(x) }
            quotes.foreach( x => if (x!=null) stralet.onQuote(x))

            val bars = dapi.subed_codes map { x => dapi.nextBar(x, "1m") }
            bars.foreach{ x => if (x!=null) stralet.onBar("1m", x) }

            sc.executeTimer(stralet)
        }

        stralet.onFini()
        sc = null
    }

    /**
      * 日线回测
      *
      * 创建一个Stralet，用日线Bar驱动该stralet，类似日内的Stralet测试。Stralet.onBar中收到 cycle="1d"的bar。
      *
      */
    def run1d() : Unit = {

        val arg_classes = new Array[Class[_]](0)
        val constructor = cfg.servlet_class.getDeclaredConstructor(arg_classes : _*)
        val stralet = constructor.newInstance()


        val date_range = cfg.first_date.toEpochDay.to(cfg.last_date.toEpochDay).map(LocalDate.ofEpochDay)
        val trade_dates = date_range.filter(dapi.calendar.contains)
        if (trade_dates.isEmpty) return


        var sim_date  = LocalDateTime.of(trade_dates.head, LocalTime.of(9,0,0))

        sc = new SimStraletContext(this, trade_dates.head.toHumanDate, sim_date)

        stralet.onInit(sc)

        var day = cfg.first_date

        for ( day <- trade_dates) {
            tapi.moveTo(day)
            sim_date  = LocalDateTime.of(day, LocalTime.of(9,0,0))
            sc.moveToNextTradingDay(day.toHumanDate, sim_date)

            val bars = dapi.subed_codes.map( x => dapi.nextBar(x, "1d"))
            for ( bar <- bars if bar != null)
                stralet.onBar("1d", bar)

            sc.executeTimer(stralet)
        }

        stralet.onFini()
        sc = null
    }
}

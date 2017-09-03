package xtz.tquant.stra.backtest

import java.time.{LocalDate, LocalDateTime, LocalTime}

import xtz.tquant.stra.backtest.TestSession.TestSessionConfig
import xtz.tquant.stra.stralet.Stralet
import xtz.tquant.stra.utils.TimeUtils._

object TestSession {

    case class TestSessionConfig(
        servlet_id    : String,
        servlet_class : Class[Stralet],
        accounts      : Seq[String],
        parameters    : Map[String, Any],
        first_date    : LocalDate,
        last_date     : LocalDate
    )

}

/**
  * Created by terryxu on 2017/9/3.
  */
class TestSession(_server: Runner, _cfg: TestSessionConfig) {

    val tapi = new SimTradeApi(this)
    val dapi = new SimDataApi(this)

    def cfg : TestSessionConfig = _cfg

    def server : Runner = _server

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

    private var sc : SimStraletContext = _

    def curSimContext : SimStraletContext = sc

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


        sc = new SimStraletContext(this, day.toHumanDate, begin_time)

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

package xtz.tquant.stra.backtest

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, LocalTime}

import scala.io.Source
import xtz.tquant.api.scala.{DataApi, TradeApi}
import xtz.tquant.stra.stralet.{Stralet, StraletContext}
import xtz.tquant.stra.utils.JsonHelper
import xtz.tquant.stra.utils.TimeUtils._

object BackTest {

    //def humanDateToDate(ymd: Int) : LocalDate = LocalDate.of( ymd / 10000, (ymd%10000)/100, (ymd%100))

    case class StraletInstanceConfig(
        id : String,
        stralet_class: String,
        universe:   Seq[String],
        parameters: Map[String, Any],
        cycle_interval: Int
    )

    case class BackTestConfig (
        date_range: Seq[Int],
        accounts:   Seq[String]
    )
    case class BackTestStraletConfig (
        stralet : StraletInstanceConfig,
        backtest : BackTestConfig
    )

    def createRunnerFromFile(path: String) : BackTestRunner = {
        try {
            var text = Source.fromFile(path).mkString

            val config = JsonHelper.deserialize[BackTestStraletConfig](text)

            val clazz = Class.forName(config.stralet.stralet_class).asInstanceOf[Class[Stralet]]

            val date_range = config.backtest.date_range
            val first_date = if (date_range(0) != 0 ) date_range(0).toLocalDate else LocalDate.now()
            val last_date  = if (date_range(1) != 0 ) date_range(1).toLocalDate else LocalDate.now()

            val runner = new BackTestRunner(config.stralet.id, clazz, config.stralet.universe,
                config.backtest.accounts, config.stralet.parameters, first_date, last_date)
            runner.init()

            runner
        }catch{
            case t: Throwable => t.printStackTrace(); null
        }
    }
}

case class BackTestRunner (
        servlet_id    : String,
        servlet_class : Class[Stralet],
        universe : Seq[String],
        accounts : Seq[String],
        parameters : Map[String, _],
        first_date : LocalDate,
        last_date:   LocalDate) {

    val exch_sim = new ExchangeSimulator(this)
    val data_sim = new BackTestDataProvider(this)

    var sim_time :   LocalDateTime = _
    var trading_day: Int = _


    def init(): Unit = {
        data_sim.init()
        exch_sim.init(1000000.0) // Set init_balance
    }

    private val _df = DateTimeFormatter.ofPattern("yyyyMMdd HHmmssSSS")

    def getSimTimeAsInt: (Int, Int) = {

        val str = sim_time.format(_df)
        val ss = str.split(" ")
        (Integer.parseInt(ss(0)), Integer.parseInt(ss(1)))
    }

    def getSimTime : LocalDateTime = sim_time

    def run(): Unit = {

        val days = last_date.toEpochDay - first_date.toEpochDay

        var day = first_date
        for ( i <- 0L to days ) {
            if (data_sim.calendar.contains(day))
                runOneDay( day)
            day = day.plusDays(1)
        }

        val order_file = s"SimOrder-$servlet_id-${first_date.toHumanDate}-${last_date.toHumanDate}-${System.currentTimeMillis}.csv"
        exch_sim.saveOrder(order_file)

    }

    def runOneDay(day : LocalDate): Unit = {

        trading_day = day.toHumanDate

        val begin_time = LocalDateTime.of(day, LocalTime.of(9, 30,0))
        val end_time   = LocalDateTime.of(day, LocalTime.of(15,0,0))
        val time_1130  = LocalDateTime.of(day, LocalTime.of(11,30,0))
        val time_1300  = LocalDateTime.of(day, LocalTime.of(13,0,0))

        sim_time = begin_time

        exch_sim.moveOn(day)
        assert(data_sim.prepare(day))

        val arg_classes = new Array[Class[_]](0)
        val constructor = servlet_class.getDeclaredConstructor(arg_classes : _*)
        val stralet = constructor.newInstance()


        val sc = new BackTestContext(this, data_sim, exch_sim)
//        val config = new StraletConfigImpl()
//        config._context = context
//        config._parameters = parameters
//        config._universe = universe

        stralet.onInit(sc)

        var bar_time = begin_time
        var timer_time = end_time

//        var cycle_time =
//            if (config.cycleInterval > 0) begin_time else end_time.plusHours(1)


        while (sim_time.isBefore(end_time)) {
            for ( code <- sc.tick_codes) {
                val quote = data_sim.nextQuote(code)
                if (quote != null)
                    stralet.onTick(quote)
            }

            for ( code <- sc.bar_codes) {
                val bar = data_sim.nextBar(code, "1m")
                if (bar!=null) {
                    stralet.onBar(bar)
                }
            }
//            if (!bar_time.isAfter(sim_time)){
//                for (code <- sc.bar_codes)
//                    data_sim.bars(sc.bar_codes)
//
//                //bar_time = bar_time.plusSeconds(60)
//                if (bar_time.isAfter(time_1130) && bar_time.isBefore(time_1300))
//                    bar_time = time_1300.plusSeconds(60)
//            }
//            if (!cycle_time.isAfter(sim_time)) {
//                stralet.onCycle()
//                cycle_time = cycle_time.plusSeconds(config.cycleInterval)
//            }

            //if (!timer_time.isAfter(sim_time)) {
            sc.executeTimer()
                timer_time = sc.nextTimerTime()
            //}

            sim_time = sc.moveToNextSimTime()
//                if (bar_time.isBefore(cycle_time))
//                    if (timer_time.isBefore(bar_time)) timer_time else bar_time
//                else
//                    if (timer_time.isBefore(cycle_time)) timer_time else cycle_time

        }

        stralet.onFini()
    }

}

class BackTestContext (runner: BackTestRunner, data_api: DataApi, trade_api: TradeApi) extends StraletContext {

    var tick_codes = runner.universe

    var bar_codes = runner.universe

    override def getTimeAsInt : (Int, Int) = runner.getSimTimeAsInt

    override def getTime : LocalDateTime = runner.getSimTime

    override def setTimer(id: Int, delay: Int, data: Any) : Unit = {

    }

    override def killTimer(id: Int) : Unit = {

    }

    override def postEvent(evt: String, msg: Any) : Unit = {

    }

    override def getTradeApi : TradeApi = trade_api

    override def getDataApi : DataApi = data_api

    /**
      * TODO: Save to file
      * @param data
      */
    override def log(data: Any) :Unit = {
        println(data)
    }

    override def getParameters(name: String, def_value: String) : String = {
        // FIXME:
        null
    }

    // return 100 days after so no timer will be executed indeed
    def nextTimerTime() : LocalDateTime = { runner.sim_time.plusDays(100) }

    def executeTimer() {}

    def moveToNextSimTime () : LocalDateTime = {
        // FIXME
        null
    }

}
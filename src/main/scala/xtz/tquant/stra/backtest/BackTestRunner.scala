package xtz.tquant.stra.backtest

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, LocalTime}

import scala.io.Source
import xtz.tquant.api.scala.{DataApi, JsonHelper, TradeApi}
import xtz.tquant.stra.stralet.impl.StraletConfigImpl
import xtz.tquant.stra.stralet.{Stralet, StraletContext}
import xtz.tquant.stra.utils.TimeUtils._

object BackTest {

    def ymdToDate(ymd: Int) : LocalDate = LocalDate.of( ymd / 10000, (ymd%10000)/100, (ymd%100))

    case class StraletInstance (
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
        stralet : StraletInstance,
        backtest : BackTestConfig
    )

    def createRunnerFromFile(path: String) : BackTestRunner = {
        try {
            var text = Source.fromFile(path).mkString

            val config = JsonHelper.deserialize[BackTestStraletConfig](text)

            val clazz = Class.forName(config.stralet.stralet_class).asInstanceOf[Class[Stralet]]

            val date_range = config.backtest.date_range
            val first_date = if (date_range(0) != 0 ) ymdToDate(date_range(0)) else LocalDate.now()
            val last_date   = if (date_range(1) != 0 ) ymdToDate(date_range(1)) else LocalDate.now()

            val runner = new BackTestRunner(clazz, config.stralet.universe,
                config.backtest.accounts, config.stralet.parameters, first_date, last_date)
            runner.init()

            return runner
        }catch{
            case t: Throwable => t.printStackTrace(); return null
        }
    }
}

case class BackTestRunner (
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


    val df = DateTimeFormatter.ofPattern("yyyyMMdd HHmmssSSS")

    def getSimTimeAsInt(): (Int, Int) = {

        val str = sim_time.format(df)
        val ss = str.split(" ")
        (Integer.parseInt(ss(0)), Integer.parseInt(ss(1)))
    }

    def getSimTime() : LocalDateTime = sim_time

    def run(): Unit = {

        val days = last_date.toEpochDay - first_date.toEpochDay

        var day = first_date
        for ( i <- 0L to days ) {
            if (data_sim.calendar.contains(day))
                runOneDay( day)
            day = day.plusDays(1)
        }

        val order_file = s"SimOrder-${servlet_class.getName}-$first_date-$last_date-${System.currentTimeMillis}.csv"
        exch_sim.saveOrder(order_file)


    }

    def runOneDay(day : LocalDate): Unit = {

        trading_day = day.toHumanDay

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


        val context = new BackTestContext(this, data_sim, exch_sim)
        val config = new StraletConfigImpl()
        config._context = context
        config._parameters = parameters
        config._universe = universe

        stralet.onInit(config)

        var bar_time = begin_time
        var timer_time = end_time

        var cycle_time =
            if (config.cycleInterval > 0) begin_time else end_time.plusHours(1)


        while (sim_time.isBefore(end_time)) {
            if (!bar_time.isAfter(sim_time)){
                stralet.onBar( data_sim.bars(context.bar_codes))
                bar_time = bar_time.plusSeconds(60)
                if (bar_time.isAfter(time_1130) && bar_time.isBefore(time_1300))
                    bar_time = time_1300.plusSeconds(60)
            }
            if (!cycle_time.isAfter(sim_time)) {
                stralet.onCycle()
                cycle_time = cycle_time.plusSeconds(config.cycleInterval)
            }

            if (!timer_time.isAfter(sim_time)) {
                context.executeTimer()
                timer_time = context.nextTimerTime()
            }

            sim_time =
                if (bar_time.isBefore(cycle_time))
                    if (timer_time.isBefore(bar_time)) timer_time else bar_time
                else
                    if (timer_time.isBefore(cycle_time)) timer_time else cycle_time

        }

        stralet.onFini()
    }

}

class BackTestContext (runner: BackTestRunner, data_api: DataApi, trade_api: TradeApi) extends StraletContext {

    var bar_codes = runner.universe

    override def getTimeAsInt() : (Int, Int) = runner.getSimTimeAsInt()

    override def getTime() : LocalDateTime = runner.getSimTime()

    override def setTimer(id: Int, delay: Int, data: Any) = {

    }

    override def killTimer(id: Int) = {

    }

    override def postMsg(msg: Any) = {

    }

    override def getTradeApi() : TradeApi = trade_api

    override def getDataApi() : DataApi = data_api

    override def subscribeQuote( codes: Seq[String]) = {

    }

    override def subscribeBar( codes: Seq[String]) = {
        bar_codes = bar_codes.union(codes).toSeq
        data_api.subscribe(bar_codes)
    }

    /**
      * TODO: Save to file
      * @param data
      */
    override def log(data: Any) :Unit = {
        println(data)
    }

    // return 100 days after so no timer will be executed indeed
    def nextTimerTime() : LocalDateTime = { runner.sim_time.plusDays(100) }

    def executeTimer() {}

}
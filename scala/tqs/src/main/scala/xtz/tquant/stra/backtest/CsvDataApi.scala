//package xtz.tquant.stra.backtest
//
//import java.time.LocalDate
//
//import com.fasterxml.jackson.annotation.{JsonIgnoreProperties, JsonPropertyOrder}
//import xtz.tquant.api.scala.DataApi
//import xtz.tquant.api.scala.DataApi.MarketQuote
//import xtz.tquant.stra.utils.CsvHelper
//
//import scala.collection.mutable
//import scala.io.Source
//
//
//object CsvDataApi {
//
//    @JsonIgnoreProperties(ignoreUnknown = true)
//    @JsonPropertyOrder(Array("DATE","TIME","OPEN","HIGH","LOW","CLOSE","VOLUME","TURNOVER"))
//    case class Bar(
//        DATE    : Int,
//        TIME    : Int,
//        OPEN    : Double,
//        HIGH    : Double,
//        LOW     : Double,
//        CLOSE   : Double,
//        VOLUME  : Long,
//        TURNOVER: Double
//    )
//
//    @JsonPropertyOrder(Array("DATE"))
//    case class CalendarItem(
//        DATE : String
//    )
//}
//
//
//class CsvDataApi(runner: Runner) extends DataApi {
//
//    import CsvDataApi._
//
//    val data_home = Config.conf.data.data_home
//
//    var _calendar : Set[LocalDate] = _
//
//    def calendar = _calendar
//
//    def init(): Unit = {
//        loadCalendar()
//    }
//
//    def loadCalendar()  = {
//        val path = s"$data_home/calendar.csv"
//
//        val text = Source.fromFile(path).mkString
//
//        _calendar = CsvHelper.deserialize[CalendarItem](text)
//            .map(x => LocalDate.parse(x.DATE))
//            .toSet
//    }
//
//    def loadBar(code : String, trading_day: Int) : Seq[DataApi.Bar] = {
//
//        val mkt = code.split('.').last
//        val path = s"$data_home/1m/$mkt/$code/$code-$trading_day.csv"
//
//        val orig_bars = CsvHelper.deserialize[Bar]( Source.fromFile(path).mkString)
//        val bars = mutable.ListBuffer[DataApi.Bar]()
//        var total_volume = 0L
//        var total_turnover = 0.0
//        for ( bar <- orig_bars) {
//            total_turnover += bar.TURNOVER
//            total_volume   += bar.VOLUME
//            bars += DataApi.Bar(code, trading_day, bar.TIME, trading_day,
//                bar.OPEN, bar.HIGH, bar.LOW, bar.CLOSE,
//                bar.VOLUME, bar.TURNOVER, oi=0)
//
//        }
//        bars
//    }
//
//    var today_bars = Map[String, Seq[DataApi.Bar]]()
//
//    def prepare(day: LocalDate) : Boolean = {
//        true
//    }
//
//    /**
//      * 取截当前交易日截至到当前时间的Bar
//      * @param codes
//      * @return
//      */
//    def bars(codes: Seq[String]) : Map[String, Seq[DataApi.Bar]] = {
//
//        val (date, time) = runner.getSimTimeAsInt
//
//        today_bars.map{ case (code, bars) => code -> bars.filter( _.time <= time) }
//    }
//
//    def nextBar(code : String, cycle: String) : Seq[DataApi.Bar] = {
//        // TO BE IMPLEMENT
//        null
//    }
//
//    def nextQuote(code : String) : DataApi.MarketQuote = {
//        null
//    }
//
//    /**
//      * 需要防止取到未来数据，不能取超过当前模拟时间的数据
//      *
//      * @param code
//      * @param cycle
//      * @param trading_day
//      * @return
//      */
//    override def bar(code : String, cycle : String, trading_day : Int, price_adj : String) : (Seq[DataApi.Bar], String) = {
//        if (cycle != "1m") return (null, "unsupported cycle " + cycle)
//
//        val (date, time) = runner.getSimTimeAsInt
//        if (trading_day > runner.trading_day ) return (null, "future trading_day")
//
//        val bars = loadBar(code, if (trading_day == 0) date else trading_day)
//
//        if (trading_day != 0 && trading_day < runner.trading_day)
//            (bars, "")
//        else
//            (bars.filter( _.time <= time), null)
//    }
//
//    /**
//      * 当使用Bar数据进行回测时，quote 为下个Bar时间区间Open价格。
//      *
//      * TODO：实现 tick 驱动
//      * @param code
//      * @return
//      */
//    override def quote(code : String) : (DataApi.MarketQuote, String) = {
//
//        val bars = today_bars.getOrElse(code, null)
//        if (bars == null) return (null, "no bar data")
//
//        val (date, time) = runner.getSimTimeAsInt
//        val future_bar = bars.filter(_.time >= time)
//        if (future_bar.isEmpty ) return (null, "market closed")
//
//        var last = 0.0
//        if (time >= 150000000)
//            last = future_bar.last.open
//        else
//            last = future_bar.head.open
//
//        val q = DataApi.MarketQuote(code, date, time, runner.trading_day,
//            0.0, 0.0, 0.0, 0.0, // fixme, OHLC
//            last,
//            0.0, 0.0,
//            0.0, // pre_close
//            0, 0.0, // volume, turnover
//            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
//            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
//            0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//            0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
//            0.0, 0.0,
//            0, 0) // oi
//
//        (q, "quote from bar!")
//    }
//
//    override def tick(code: String, trading_day: Int): (Seq[MarketQuote], String)  = {
//        (null, "-1,fix me")
//    }
//
//    override def subscribe(codes : Seq[String]) : (Seq[String], String) = {
//        //today_bars ++
////        val all_codes = this.runner.cfg.universe.union(codes)
////        today_bars --= today_bars.keys.toSeq.diff(all_codes)
////
////        today_bars ++= codes.diff(today_bars.keys.toSeq)
////                            .map(code => code -> this.loadBar(code, this.runner.trading_day))
////
////        (all_codes, "")
//        (null, "")
//    }
//
//    override def unsubscribe(codes : Seq[String]) : (Seq[String], String) = {
//        null
//    }
//
//    override def setCallback(callback : DataApi.Callback) : Unit = {
//
//    }
//}

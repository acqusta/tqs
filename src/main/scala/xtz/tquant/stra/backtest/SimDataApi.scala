package xtz.tquant.stra.backtest

import java.time.{LocalDate, LocalDateTime}

import xtz.tquant.api.scala.{DataApi, TQuantApi}
import xtz.tquant.api.scala.DataApi.MarketQuote
import xtz.tquant.stra.utils.TimeUtils._

import scala.collection.mutable

class SimDataApi(session: TestSession) extends DataApi {

    val _dapi = new TQuantApi(session.server.conf.data.tqc_addr).dataApi

    var _calendar : Set[LocalDate] = _

    def calendar : Set[LocalDate] = _calendar

    val subed_codes = mutable.HashSet[String]()

    var today_bars  = mutable.HashMap[String,  BarInfo]()
    var today_ticks = mutable.HashMap[String, TickInfo]()


    def init(): Unit = {
        loadCalendar()
    }

    def loadCalendar()  = {
        // Build calendar using 000001.SH day lines
        val (bars, msg) = _dapi.bar("000001.SH", "1d")
        assert(bars!=null && bars.nonEmpty, msg)
        _calendar = bars.map( _.date.toLocalDate).toSet
    }

    def loadBar(code : String, trading_day: Int) : Seq[DataApi.Bar] = {
        val (bars, msg) = _dapi.bar(code, "1m", trading_day=trading_day)
        assert(bars!=null, s"$code bar error:" + msg)
        bars
    }

    def loadTick(code : String, trading_day: Int) : Seq[DataApi.MarketQuote] = {
        val (ticks, msg) = _dapi.tick(code, trading_day=trading_day)
        assert(ticks!=null, s"$code ticks error:" + msg)
        ticks
    }

    case class TickInfo ( ticks : Seq[DataApi.MarketQuote], var pos : Int = -1)

    case class BarInfo ( bars : Seq[DataApi.Bar], var pos : Int = -1)

    def moveTo(day: LocalDate) : Boolean = {

        today_bars.clear()
        today_ticks.clear()

        subed_codes.clear()

        true
    }

    /**
      * 取截当前交易日截至到当前时间的Bar
      * @param codes
      * @return
      */
    def bars(codes: Seq[String]) : Map[String, Seq[DataApi.Bar]] = {

        val (date, time) = session.curSimContext.getTimeAsInt

        today_bars.map{ case (code, bi) =>
            code -> bi.bars.filter( d => d.date < date || (d.date==date && d.time <= time))
        }.toMap
    }

    /**
      *  9:31:10 -> 9:30:00，不是当前时间属于哪个Bar
      *
      * @return
      */
    def lastBartime(dt : Tuple2[Int, Int]) : Long = {

        val date = dt._1
        val time = (dt._2 / 100000) * 100000
        val bar_dt = LocalDateTime.of (date.toLocalDate, time.toLocalTime) //.plusMinutes(-1)

        bar_dt.toLocalDate.toHumanDate * 100000000L +
            bar_dt.toLocalTime.toHumanMilli
    }

    def quoteTime(date: Int, time: Int) = {
        date * 1000000000L + time
    }
    /**
      * 如果当前pos之后的bar在当前bartime之前或者相等，则表示有新的 bar
      *
      * @param code
      * @param cycle
      * @return
      */
    def nextBar(code : String, cycle: String) : Seq[DataApi.Bar] = {

        val last_bartime = lastBartime(session.curSimContext.getTimeAsInt)

        val bi = today_bars.getOrElse(code, null)
        if (bi==null) return null

        if (bi.pos + 1 >= bi.bars.length) return null

        val next_bar = bi.bars(bi.pos+1)
        if ( lastBartime(next_bar.date, next_bar.time) <= last_bartime) {
            bi.pos += 1
            bi.bars.splitAt(bi.pos + 1)._1 // include [0..pos]
        } else {
            null
        }
    }

    def nextQuote(code : String) : DataApi.MarketQuote = {

        val (date, time) = session.curSimContext.getTimeAsInt
        val cur_quote_time = quoteTime(date, time)

        val ti = today_ticks.getOrElse(code, null)
        if (ti==null) return null
        if (ti.pos + 1 >= ti.ticks.length) return null

        val next_tk = ti.ticks(ti.pos+1)
        if ( quoteTime(next_tk.date, next_tk.time) <= cur_quote_time) {
            ti.pos += 1
            next_tk
        } else {
            null
        }
    }

    def calcNextQuoteTime() : LocalDateTime = {

        // TODO:
        //   1. fix time of index
        //   2. remove ticks which is not in market trading time
        var quote_time = 99999999000000000L
        for ( (_, ti) <- this.today_ticks) {
            if (ti.pos + 1 < ti.ticks.length) {
                val tk = ti.ticks(ti.pos + 1)
                val time = quoteTime(tk.date, tk.time)
                if (time < quote_time)
                    quote_time = time
            }
        }

        //assert(quote_time >= cur_quote_time, s"wrong next quote_time $quote_time $cur_quote_time")

        if ( quote_time != 99999999000000000L) {
            val date = (quote_time / 1000000000).toInt
            val time = (quote_time % 1000000000).toInt

            LocalDateTime.of( date/10000, (date/100)%100, date%100,
                time/10000000, (time/100000)%100, (time/1000)%100,
                (time%1000)*1000000)
        } else {
            null
        }
    }

    /**
      * 需要防止取到未来数据，不能取超过当前模拟时间的数据
      *
      * @param code
      * @param cycle
      * @param trading_day
      * @return
      */
    override def bar(code: String, cycle: String, trading_day: Int, price_adj: String): (Seq[DataApi.Bar], String) = {
        if (cycle != "1m")
            return (null, "unsupported cycle " + cycle)

        val (date, time) = session.curSimContext.getTimeAsInt
        if (trading_day > session.curSimContext.getTradingDay ) return (null, "future trading_day")

        val bars = loadBar(code, if (trading_day == 0) date else trading_day)

        if (trading_day != 0 && trading_day < session.curSimContext.getTradingDay)
            (bars, "")
        else
            (bars.filter( x => x.date < date || (x.date == date &&x.time <= time)), null)
    }

    /**
      * 当使用Bar数据进行回测时，quote 为下个Bar时间区间Open价格。
      *
      * TODO：实现 tick 驱动
      * @param code
      * @return
      */
    override def quote(code : String) : (DataApi.MarketQuote, String) = {

        if (false) {
            val bi = today_bars.getOrElse(code, null)
            if (bi == null) return (null, "no bar data")

            val (date, time) = session.curSimContext.getTimeAsInt
            val future_bar = bi.bars.filter(_.time >= time)
            if (future_bar.isEmpty ) return (null, "market closed")

            var last = 0.0
            if (time >= 150000000)
                last = future_bar.last.open
            else
                last = future_bar.head.open

            val q = DataApi.MarketQuote(code, date, time, session.curSimContext.getTradingDay,
                0.0, 0.0, 0.0, 0.0, // fixme, OHLC
                last,
                0.0, 0.0,
                0.0, // pre_close
                0, 0.0, // volume, turnover
                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0.0, 0.0,
                0, 0) // oi

            (q, "0,quote from bar!")
        } else {
            val ti = today_ticks.getOrElse(code, null)
            if (ti == null) return (null, "-1,no tick data")
            if (ti.pos<0) {
                return (null, "-1,not arrive yet")
            } else {
                (ti.ticks(ti.pos), "0,")
            }
        }
    }

    override def tick(code: String, trading_day: Int): (Seq[MarketQuote], String)  = {
        (null, "-1,fix me")
    }

    override def subscribe(codes : Seq[String]) : (Seq[String], String) = {

        val (date, time) = session.curSimContext.getTimeAsInt
        val cur_bar_time   = lastBartime(date, time)
        val cur_quote_time = quoteTime(date, time)

        subed_codes ++= codes.toSet

        _dapi.subscribe(codes)

        today_bars ++= subed_codes.diff(today_bars.keys.toSet)
                            .map{ code =>
                                val bars = this.loadBar(code, this.session.curSimContext.getTradingDay)
                                var pos = bars.length
                                for ( i  <- bars.indices if pos == bars.length) {
                                    if ( lastBartime(bars(i).date, bars(i).time) > cur_bar_time) pos = i
                                }
                                pos -= 1
                                code -> BarInfo(bars, pos)
                            }

        today_ticks ++= subed_codes.diff(today_ticks.keys.toSet)
                                .map{ code =>
                                    val ticks = this.loadTick(code, this.session.curSimContext.getTradingDay)
                                    var pos = ticks.length
                                    for ( i  <- ticks.indices if pos == ticks.length) {
                                        if (ticks(i).date*1000000000L + ticks(i).time > cur_quote_time) pos = i
                                    }
                                    pos -= 1
                                    code -> TickInfo(ticks, pos)
                                }

        (subed_codes.toSeq, "")
    }

    override def unsubscribe(codes : Seq[String]) : (Seq[String], String) = {
        null
    }

    override def setCallback(callback : DataApi.Callback) : Unit = {
        // TODO: implement it in Stralet?
    }
}

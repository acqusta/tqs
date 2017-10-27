package xtz.tquant.stra.backtest

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter

import xtz.tquant.api.scala.{DataApi, TradeApi}
import xtz.tquant.stra.stralet.{Stralet, StraletContext}
import xtz.tquant.stra.utils.TimeUtils._

import scala.collection.mutable

class SimStraletContext(st: StraletTest, var trading_day: Int, var sim_time: LocalDateTime) extends StraletContext {

    private val _df = DateTimeFormatter.ofPattern("yyyyMMdd HHmmssSSS")

    case class Timer(id: Int, delay: Long, data: Any, var next_time : LocalDateTime)

    private val timer_map = mutable.HashMap[Int, Timer]()

    private val evt_list = mutable.ArrayBuffer[(String, Any)]()


    override def getTradingDay : Int = trading_day

    override def getTimeAsInt : (Int, Int) = {
        val str = sim_time.format(_df)
        val ss = str.split(" ")
        (Integer.parseInt(ss(0)), Integer.parseInt(ss(1)))
    }

    override def getTime : LocalDateTime = sim_time

    override def setTimer(id: Int, delay: Int, data: Any) : Unit = {
        assert( st.cfg.data_level != "1d")
        assert( st.cfg.data_level != "1m")

        timer_map += id -> Timer(id, delay, data, getTime.plusNanos(delay*10000000))
    }

    override def killTimer(id: Int) : Unit = {
        timer_map.remove(id)
    }

    override def postEvent(evt: String, msg: Any) : Unit = {

        assert( st.cfg.data_level != "1d")
        assert( st.cfg.data_level != "1m")

        evt_list.synchronized{
            evt_list.append((evt, msg))
        }
    }

    override def getTradeApi : TradeApi = st.tapi

    override def getDataApi : DataApi = st.dapi


    private val _log_df = DateTimeFormatter.ofPattern("yyyyMMdd HHmmss.SSS")
    /**
      * TODO: Save to file
      * @param data
      */
    override def log(data: Any) :Unit = {
        println(LocalDateTime.now.format(_log_df) + " " + data)
    }

    override def getParameters[T: Manifest](name : String, def_value: T) : T = {

        // FIXME:
        st.cfg.parameters.getOrElse(name, def_value).asInstanceOf[T]
    }

    override def mode : String = "backtest"

    def calcNextTimerTime() : LocalDateTime = {
        if (timer_map.nonEmpty)
            timer_map.reduce((a,b) => if (a._2.next_time.isBefore( b._2.next_time)) a else b)._2.next_time
        else
            null
    }

    def executeTimer(stralet : Stralet) : Unit = {

        val timer = mutable.ArrayBuffer[Timer]()
        for ( (_, t) <- timer_map) {
            if (!t.next_time.isAfter(sim_time))
                timer.append(t)
        }

        for ( t <-timer) {
            if (timer_map.getOrElse(t.id, null) != null) {
                timer_map += t.id -> Timer(t.id, t.delay, t.data, t.next_time.plusNanos(t.delay * 10000000))
                stralet.onTimer(t.id, t.data)
            }
        }
    }

    def moveToNextSimTime () : LocalDateTime = {

        assert (st.cfg.data_level != "1d")
        val time = {
            val time1 = st.dapi.calcNextTime()
            val time2 = calcNextTimerTime()
            if (time1 != null && time2 !=null) {
                if (time1.isBefore(time2)) time1 else time2
            } else {
                if (time1!=null) time1 else if (time2!=null) time2 else null
            }
        }

        if (time != null) {
            sim_time = time
            time
        } else {
            null
        }
    }

    def moveToNextTradingDay(date : Int, sim_time : LocalDateTime) = {
        this.trading_day = date
        this.sim_time = sim_time
    }
}

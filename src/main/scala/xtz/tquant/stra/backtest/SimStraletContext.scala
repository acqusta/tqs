package xtz.tquant.stra.backtest

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import xtz.tquant.api.scala.{DataApi, TradeApi}
import xtz.tquant.stra.stralet.{Stralet, StraletContext}

import scala.collection.mutable

class SimStraletContext(session: TestSession, trading_day: Int, var sim_time: LocalDateTime) extends StraletContext {

    private val _df = DateTimeFormatter.ofPattern("yyyyMMdd HHmmssSSS")

    case class Timer(id: Int, delay: Int, data: Any, var next_time : LocalDateTime)

    private val timer_map = mutable.HashMap[Int, Timer]()

    private val evt_list = mutable.ArrayBuffer[(String, Any)]()

    def getTradingDay : Int = trading_day

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

    override def getTradeApi : TradeApi = session.tapi

    override def getDataApi : DataApi = session.dapi

    /**
      * TODO: Save to file
      * @param data
      */
    override def log(data: Any) :Unit = {
        println(data)
    }

    override def getParameters[T: Manifest](name : String, def_value: T) : T = {

        // FIXME:
        session.cfg.parameters.getOrElse(name, def_value).asInstanceOf[T]
    }

    def calcNextTimerTime() : LocalDateTime = {
        if (timer_map.nonEmpty)
            timer_map.reduce((a,b) => if (a._2.next_time.isBefore( b._2.next_time)) a else b)._2.next_time
        else
            null
    }

    def executeTimer(stralet : Stralet) : Unit = {

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
            val time1 = session.dapi.calcNextQuoteTime()
            val time2 = calcNextTimerTime()
            if (time1 != null && time2 !=null) {
                if (time1.isBefore(time2)) time1 else time2
            } else {
                if (time1!=null) time1 else if (time2!=null) time2 else null
            }
        }

//        if (time == null) {
//            // FIXME:
//            assert(time != null, "time shouldn't be null")
//        }

        if (time != null) {
            sim_time = time
            time
        } else {
            null
        }
    }
}

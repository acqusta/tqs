package xtz.tquant.stra.backtest

import xtz.tquant.api.scala.DataApi.{Bar, MarketQuote}
import xtz.tquant.stra.stralet.{Stralet, StraletConfig, StraletContext}

/**
  * Created by terryxu on 2017/3/26.
  */
class DemoStralet extends Stralet {

    var cfg : StraletConfig = _
    var context : StraletContext = _

    override def onInit(cfg: StraletConfig): Unit = {
        println("DemoStralet onInit ", cfg.context.getTime())

        this.cfg = cfg
        this.context = cfg.context

        context.subscribeBar(cfg.universe)
    }

    override def onFini() = {
        println("DemoStralet onFini ", cfg.context.getTime())
    }

    override def onQuote(q: MarketQuote) = {

    }

    override def onBar(bars: Map[String, Seq[Bar]]): Unit = {
        //println("DemoStralet onBar " + context.getTimeAsInt())
        //bar foreach println _
    }

    override def onTimer(id: Int, data: Any) = {

    }

    override def onMsg(msg: Any) = {

    }

    override def onCycle(): Unit = {
        val (date, time) = context.getTimeAsInt()
        println(s"onCycle $date $time")
    }
}

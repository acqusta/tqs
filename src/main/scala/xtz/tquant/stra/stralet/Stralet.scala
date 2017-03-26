package xtz.tquant.stra.stralet

import java.time.LocalDateTime

import xtz.tquant.api.scala.DataApi._
import xtz.tquant.api.scala.{DataApi, TradeApi}


trait Stralet {

    def onInit(cfg: StraletConfig)

    def onFini()

    def onQuote(q: MarketQuote)

    def onBar(bar: Map[String,Seq[Bar]])

    def onTimer(id: Int, data: Any)

    def onMsg(msg: Any)

    def onCycle()
}

trait StraletContext {

    /**
      *
      * @return (date, time_ms)
      */
    def getTimeAsInt() : (Int, Int)

    def getTime() : LocalDateTime

    def setTimer(id: Int, delay: Int, data: Any)

    def killTimer(id: Int)

    def postMsg(msg: Any)

    def getTradeApi() : TradeApi

    def getDataApi() : DataApi

    def subscribeQuote( codes: Seq[String])

    def subscribeBar( codes: Seq[String])
}

trait StraletConfig {

    def context : StraletContext

    //def getProperty(name: String) : Any

    def parameters : Map[String, Any]

    def universe : Seq[String]

    def cycleInterval : Int

}


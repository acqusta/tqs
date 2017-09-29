package xtz.tquant.stra.realtime

import java.time.{LocalDate, LocalDateTime}

import akka.actor.ActorRef
import xtz.tquant.api.scala.DataApi.{Bar, Callback, MarketQuote}
import xtz.tquant.api.scala.{DataApi, TQuantApi}

import scala.collection.mutable

/**
  * Created by txu on 2017/9/6.
  */
class DataApiImpl (actorRef: ActorRef, tqc_addr : String) extends DataApi {

    val dapi = new TQuantApi(tqc_addr).dataApi

    dapi.setCallback( new Callback {
        override def onMarketQuote(quote: MarketQuote): Unit = {
            actorRef ! quote
        }

        override def onBar(cycle: String, bar: Bar): Unit = {
            actorRef ! StraletActor.BarInd(cycle, bar)
        }
    })

    override def bar(code: String, cycle: String, trading_day: Int, price_adj: String, align : Boolean): (Seq[DataApi.Bar], String) = {
        dapi.bar(code, cycle, trading_day, price_adj, align)
    }

    override def quote(code : String) : (DataApi.MarketQuote, String) = {
        dapi.quote(code)
    }

    override def tick(code: String, trading_day: Int): (Seq[MarketQuote], String)  = {
        dapi.tick(code, trading_day)
    }

    var subed_codes = mutable.HashSet[String]()

    override def subscribe(codes : Seq[String]) : (Seq[String], String) = {

        subed_codes ++= codes.toSet

        dapi.subscribe(codes)

        (subed_codes.toSeq, "")
    }

    override def unsubscribe(codes : Seq[String]) : (Seq[String], String) = {
        val (_, msg) = dapi.unsubscribe(codes)
        subed_codes --= codes.toSet
        (codes, msg)
    }

    override def setCallback(callback : DataApi.Callback) : Unit = {
        // TODO: log
    }
}

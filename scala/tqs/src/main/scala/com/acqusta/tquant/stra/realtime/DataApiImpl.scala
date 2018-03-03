package com.acqusta.tquant.stra.realtime

import akka.actor.ActorRef
import com.acqusta.tquant.api.scala.DataApi
import com.acqusta.tquant.api.TQuantApi
import com.acqusta.tquant.api.scala.DataApi.{Bar, Callback, DailyBar, MarketQuote}

import scala.collection.mutable

/**
  * Created by txu on 2017/9/6.
  */
class DataApiImpl (actorRef: ActorRef, tqc_addr : String) extends DataApi {

    val dapi = new TQuantApi(tqc_addr).getDataApi("")

    dapi.setCallback( new DataApi.Callback {
        override def onMarketQuote(quote: MarketQuote): Unit = {
            actorRef ! quote
        }

        override def onBar(cycle: String, bar: Bar): Unit = {
            actorRef ! StraletActor.BarInd(cycle, bar)
        }
    })


    override def bar(code: String, cycle: String, trading_day: Int, align: Boolean): (Seq[Bar], String) = {
        val r = dapi.getBar(code, cycle, trading_day, align)
        (r.value, r.msg)
    }

    override def quote(code: String): (MarketQuote, String) = {
        val r = dapi.getQuote(code)
        (r.value, r.msg)
    }

    override def tick(code: String, trading_day: Int): (Seq[MarketQuote], String) = {
        val r = dapi.getTick(code, trading_day)
        (r.value, r.msg)
    }

    var subed_codes = mutable.HashSet[String]()

    override def subscribe(codes: Seq[String]): (Seq[String], String) = {

        subed_codes ++= codes.toSet

        dapi.subscribe(codes.toArray)

        (subed_codes.toSeq, "")
    }

    override def unsubscribe(codes: Seq[String]): (Seq[String], String) = {
        val r = dapi.unsubscribe(codes.toArray)
        subed_codes --= codes.toSet
        (subed_codes.toSeq, r.msg)
    }

    override def setCallback(callback : Callback) : Unit = {
        // TODO: log
    }


    override def dailyBar(code: String, price_adj: String, align: Boolean): (Seq[DailyBar], String) = {
        val r = dapi.getDailyBar(code, price_adj, align)
        (r.value, r.msg)
    }
}

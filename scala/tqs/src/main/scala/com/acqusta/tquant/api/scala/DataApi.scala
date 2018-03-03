package com.acqusta.tquant.api.scala

import akka.actor.ActorRef
import com.acqusta.tquant.api.{TQuantApi, DataApi => JavaDataApi}
import xtz.tquant.stra.realtime.StraletActor

import scala.collection.mutable

object DataApi {
    type Callback = JavaDataApi.Callback

    type MarketQuote = JavaDataApi.MarketQuote

    type Bar = JavaDataApi.Bar

    type DailyBar = JavaDataApi.DailyBar

}

trait DataApi {

    import DataApi._

    def tick(code: String, trading_day: Int = 0): (Seq[MarketQuote], String)

    def bar(code: String, cycle: String, trading_day: Int = 0, align: Boolean = true): (Seq[Bar], String)

    def dailyBar(code: String, price_adj: String = "", align: Boolean = true): (Seq[DailyBar], String)

    def quote(code: String): (MarketQuote, String)

    def subscribe(codes: Seq[String]): (Seq[String], String)

    def unsubscribe(codes: Seq[String]): (Seq[String], String)

    def setCallback(callback: Callback): Unit
}


class ScalaDataApi (dapi : JavaDataApi) extends DataApi {

    import DataApi._

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
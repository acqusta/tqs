package com.acqusta.tquant.stra.realtime

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import Config.RTConfig
import com.acqusta.tquant.stra.utils.JsonHelper

import scala.io.Source
import scala.concurrent.Await
import scala.concurrent.duration.Duration
//import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, _}

/**
  * Created by terryxu on 2017/9/2.
  */

//object Container {
//
//    case class Config(
//        stralet  : StraletConfig
//    )
//
//}
class Container {

    val stockOrderMonitor = ActorSystem().actorOf(Props[StockOrderMonitorActor])

    var rt_conf : RTConfig = _

    def init(rt_conf: RTConfig): Unit = {
        this.rt_conf = rt_conf
    }

    def run(stralet_config  : StraletConfig): Unit = {

        if (createStralet(stralet_config)) {

            stockOrderMonitor ! StockOrderMonitorActor.InitReq(this.rt_conf)

            while (true) {
                try {
                    Thread.sleep(100)
                }catch{
                    case _: Throwable => println("Exception")
                }
            }
        }
    }

    def createStralet(stralet_config  : StraletConfig) : Boolean = {
        try {
            val actor = ActorSystem().actorOf(Props[StraletActor])

            implicit val timeout = Timeout(60 seconds)
            val rsp = Await.result((actor ? StraletActor.InitReq(stralet_config, rt_conf)).mapTo[StraletActor.InitRsp], Duration.Inf)

            if (rsp.result) {
                if (rt_conf.options.sim_order_status)
                    stockOrderMonitor ! StockOrderMonitorActor.RegisterStraletActor(actor)
            }
            rsp.result
        }catch{
            case t: Throwable => t.printStackTrace(); false
        }
    }
}

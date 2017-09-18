package xtz.tquant.stra.realtime

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import xtz.tquant.stra.utils.JsonHelper

import scala.io.Source
import scala.concurrent.Await
import scala.concurrent.duration.Duration
//import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, _}

/**
  * Created by terryxu on 2017/9/2.
  */

object Container {

    case class Config(
        stralet  : StraletConfig
    )

}
class Container {

    val stockOrderMonitor = ActorSystem().actorOf(Props[StockOrderMonitorActor])

    def init(conf_path: String): Unit = {
    }

    def run(cfg_path: String): Unit = {

        if (startStralet(cfg_path)) {

            stockOrderMonitor ! StockOrderMonitorActor.Init()

            while (true) {
                try {
                    Thread.sleep(100)
                }catch{
                    case _: Throwable => println("Exception")
                }
            }
        }
    }

    def startStralet(cfg_path: String) : Boolean = {
        try {

            var text = Source.fromFile(cfg_path).mkString

            val cfg = JsonHelper.deserialize[Container.Config](text)

            val actor = ActorSystem().actorOf(Props[StraletActor])

            implicit val timeout = Timeout(60 seconds)
            val rsp = Await.result((actor ? StraletActor.InitReq( cfg.stralet)).mapTo[StraletActor.InitRsp], Duration.Inf)

            if (rsp.result) {
                if (Config.conf.options.sim_order_status)
                    stockOrderMonitor ! StockOrderMonitorActor.RegisterStraletActor(actor)
            }
            rsp.result
        }catch{
            case t: Throwable => t.printStackTrace(); false
        }
    }
}

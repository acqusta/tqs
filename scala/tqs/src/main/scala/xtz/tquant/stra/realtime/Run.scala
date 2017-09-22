package xtz.tquant.stra.realtime

import xtz.tquant.stra.realtime.Config.RTConfig
import xtz.tquant.stra.utils.JsonHelper

import scala.io.Source

object Run {

    val default_realtime_config =
        """
          |{
          |  "tqc" : {
          |    "addr"  : "tcp://127.0.0.1:10001"
          |  },
          |
          |  "options" : {
          |    "sim_order_status" : true
          |  }
          |}
        """.stripMargin

    def runPath(stralet_conf_path : String, realtime_conf_path : String = ""): Unit = {

        val stralet_conf_text = Source.fromFile(stralet_conf_path).mkString

        val rt_conf_text =
            if ( realtime_conf_path.nonEmpty)
                Source.fromFile(realtime_conf_path).mkString
            else
                ""

        runConf(stralet_conf_text, rt_conf_text)
    }

    def runConf(stralet_conf_text : String, rt_conf_text : String = ""): Unit = {

        val stralet_conf = JsonHelper.deserialize[StraletConfig]( stralet_conf_text)

        val rt_config = JsonHelper.deserialize[RTConfig](
            if (rt_conf_text.nonEmpty) rt_conf_text else default_realtime_config )

        val container = new Container()
        container.init(rt_config)
        container.run(stralet_conf)
    }

}

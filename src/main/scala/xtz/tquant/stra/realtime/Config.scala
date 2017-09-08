package xtz.tquant.stra.realtime

import xtz.tquant.stra.utils.JsonHelper

import scala.io.Source

/**
  * Created by txu on 2017/9/6.
  */
object Config {

    case class TqcConfig( addr : String)

    case class Options (
        sim_order_status : Boolean
    )

    case class AllConfig (
        tqc      : TqcConfig,
        options  : Options
    )

    private var _config: AllConfig = null

    def conf = _config

    def load(path: String): AllConfig = {

        var text = Source.fromFile(path).mkString

        _config = JsonHelper.deserialize[AllConfig](text)
        _config
    }

}

package xtz.tquant.stra.backtest

import scala.io.Source
import xtz.tquant.stra.utils.JsonHelper



/**
  * Created by terryxu on 2017/3/26.
  */

object TestConfig {

    case class StraletConfig(
        id            : String,
        stralet_class : String,
        parameters    : Map[String, Any]
    )

    case class BackTestConfig(
        date_range  : Seq[Int],
        accounts    : Seq[String]
    )

    case class TestConfig(
        stralet  : StraletConfig,
        backtest : BackTestConfig
    )

    def load(path: String): TestConfig = {

        var text = Source.fromFile(path).mkString

        val config = JsonHelper.deserialize[TestConfig](text)
        config
    }

}

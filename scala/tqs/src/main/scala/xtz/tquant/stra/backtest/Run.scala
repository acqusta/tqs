package xtz.tquant.stra.backtest

import xtz.tquant.stra.backtest.StraletTest.BackTestConfig
import xtz.tquant.stra.backtest.StraletTest.StraletConfig
import xtz.tquant.stra.utils.JsonHelper

import scala.io.Source

/**
  * Created by txu on 2017/9/18.
  */
object Run {

    def runPath(stralet_conf_path : String, backtest_conf_path : String): Unit = {

        runConf(
            Source.fromFile(stralet_conf_path).mkString,
            Source.fromFile(backtest_conf_path).mkString)

    }

    def runConf(stralet_conf_text : String, backtest_conf_text : String) : Unit = {

        val stralet_conf = JsonHelper.deserialize[StraletConfig]( stralet_conf_text)

        val bt_config = JsonHelper.deserialize[BackTestConfig](backtest_conf_text)

        val container = new Container()
        container.init(bt_config)

        val test = container.createTest(stralet_conf)
        test.run()
    }
}

package xtz.tquant.stra.backtest

import java.time.LocalDate

import xtz.tquant.stra.backtest.Container.Config
import xtz.tquant.stra.backtest.StraletTest.StraletTestConfig
import xtz.tquant.stra.stralet.Stralet
import xtz.tquant.stra.utils.JsonHelper

import scala.io.Source
import xtz.tquant.stra.utils.TimeUtils._

object Container {

    case class DataProviderConfig (
        data_home : String,
        tqc_addr  : String
    )

    case class Config(
        data : DataProviderConfig
    )

}

/**
  * Created by terryxu on 2017/9/2.
  */
class Container {

    private var _conf : Container.Config = _

    def conf = _conf

    def init(conf_path: String): Unit = {

        val text = Source.fromFile( conf_path).mkString
        _conf = JsonHelper.deserialize[Config](text)

    }

    def createTestFromFile(path: String) : StraletTest = {
        try {
            var text = Source.fromFile(path).mkString

            val config = JsonHelper.deserialize[StraletTest.TestConfig](text)

            val clazz = Class.forName(config.stralet.stralet_class).asInstanceOf[Class[Stralet]]

            val date_range = config.backtest.date_range
            val first_date = if (date_range(0) != 0 ) date_range(0).toLocalDate else LocalDate.now()
            val last_date  = if (date_range(1) != 0 ) date_range(1).toLocalDate else LocalDate.now()
            val data_level = if (config.backtest.data_level != null) config.backtest.data_level else "tk"

            val cfg = StraletTestConfig(config.stralet.id, clazz,
                                        config.backtest.accounts, config.stralet.parameters,
                                        first_date, last_date,
                                        data_level = data_level )

            val session = new StraletTest(this, cfg)
            session.init()
            session
        }catch{
            case t: Throwable => t.printStackTrace(); null
        }
    }
}

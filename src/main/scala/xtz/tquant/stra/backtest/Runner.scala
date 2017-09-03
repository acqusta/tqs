package xtz.tquant.stra.backtest

import java.time.LocalDate

import xtz.tquant.stra.backtest.TestConfig.TestConfig
import xtz.tquant.stra.backtest.Runner.Config
import xtz.tquant.stra.backtest.TestSession.TestSessionConfig
import xtz.tquant.stra.stralet.Stralet
import xtz.tquant.stra.utils.JsonHelper

import scala.io.Source
import xtz.tquant.stra.utils.TimeUtils._

object Runner {

    case class DataProviderConfig (
        data_home : String,
        tqc_addr : String
    )

    case class Config(
        data : DataProviderConfig
    )

}

/**
  * Created by terryxu on 2017/9/2.
  */
class Runner {

    private var _conf : Runner.Config = _

    def conf = _conf

    def init(conf_path: String): Unit = {

        val text = Source.fromFile( conf_path).mkString
        _conf = JsonHelper.deserialize[Config](text)

    }

    def createSessionFromFile(path: String) : TestSession = {
        try {
            var text = Source.fromFile(path).mkString

            val config = JsonHelper.deserialize[TestConfig](text)

            val clazz = Class.forName(config.stralet.stralet_class).asInstanceOf[Class[Stralet]]

            val date_range = config.backtest.date_range
            val first_date = if (date_range(0) != 0 ) date_range(0).toLocalDate else LocalDate.now()
            val last_date  = if (date_range(1) != 0 ) date_range(1).toLocalDate else LocalDate.now()

            val cfg = TestSessionConfig(config.stralet.id, clazz,
                config.backtest.accounts, config.stralet.parameters, first_date, last_date)

            val session = new TestSession(this, cfg)
            session.init()
            session
        }catch{
            case t: Throwable => t.printStackTrace(); null
        }
    }
}

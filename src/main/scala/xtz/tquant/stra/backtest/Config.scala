package xtz.tquant.stra.backtest

import xtz.tquant.stra.utils.JsonHelper

import scala.io.Source

/**
  * Created by terryxu on 2017/3/26.
  */

object Config {

    case class DataProviderConfig (
        data_home : String,
        tqc_addr : String
    )

    case class AllConfig (
        data : DataProviderConfig
    )

    private var _conf : AllConfig = _

    def load() = {
        val text = Source.fromFile( "etc/backtest.conf").mkString

        _conf = JsonHelper.deserialize[AllConfig](text)
    }

    def conf = _conf

}

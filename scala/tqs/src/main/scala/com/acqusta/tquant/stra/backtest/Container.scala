package com.acqusta.tquant.stra.backtest

import java.time.LocalDate

import com.acqusta.tquant.stra.backtest.StraletTest._StraletTestConfig
import com.acqusta.tquant.stra.stralet.Stralet
import com.acqusta.tquant.stra.utils.JsonHelper

import scala.io.Source
import com.acqusta.tquant.stra.utils.TimeUtils._


/**
  * Created by terryxu on 2017/9/2.
  */
class Container {

    private var _bt_conf : StraletTest.BackTestConfig = _

    def conf = _bt_conf

    def init(conf_path: String): Unit = {

        val text = Source.fromFile( conf_path).mkString
        _bt_conf = JsonHelper.deserialize[StraletTest.BackTestConfig](text)

    }

    def init(conf : StraletTest.BackTestConfig): Unit = {
        _bt_conf = conf
    }

//    def createTestFromFile(path: String) : StraletTest = {
//        try {
//            var text = Source.fromFile(path).mkString
//
//            val config = JsonHelper.deserialize[StraletTest.](text)
//
//            createTest(config)
//
//        }catch{
//            case t: Throwable => t.printStackTrace(); null
//        }
//    }

    def createTest(stralet_conf : StraletTest.StraletConfig) : StraletTest = {
        try {
            val clazz = Class.forName(stralet_conf.stralet.stralet_class).asInstanceOf[Class[Stralet]]

            val date_range = _bt_conf.backtest.date_range
            val first_date = if (date_range(0) != 0 ) date_range(0).toLocalDate else LocalDate.now()
            val last_date  = if (date_range(1) != 0 ) date_range(1).toLocalDate else LocalDate.now()
            val data_level = if (_bt_conf.backtest.data_level != null) _bt_conf.backtest.data_level else "tk"

            val cfg = _StraletTestConfig(stralet_conf.stralet.id, clazz,
                                        _bt_conf.backtest.accounts, stralet_conf.stralet.parameters,
                                        first_date, last_date,
                                        data_level = data_level )

            val test = new StraletTest(this, cfg)
            test.init()
            test
        }catch{
            case t: Throwable => t.printStackTrace(); null
        }
    }

}

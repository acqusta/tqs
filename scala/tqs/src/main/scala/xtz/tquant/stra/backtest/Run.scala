package xtz.tquant.stra.backtest

/**
  * Created by txu on 2017/9/18.
  */
object Run {

    def run(stralet_conf : String, backtest_conf : String): Unit = {

        val container = new Container()
        container.init(backtest_conf)

        val test = container.createTestFromFile(stralet_conf)
        test.run()
    }

}

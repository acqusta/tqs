package xtz.tquant.stra.backtest



object Boot extends App {

    Config.load()

    val runner = BackTest.createRunnerFromFile("etc/demostralet.conf")
    runner.run()

}
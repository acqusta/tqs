package xtz.tquant.stra.backtest

import org.apache.commons.cli.{DefaultParser, HelpFormatter, Options}


object Boot extends App {

    val option = new Options()
    option.addOption("f", "config", true, "stralet config file")

    val parser = new DefaultParser()
    val line = parser.parse(option, args)

    if (!line.hasOption("f")) {
        new HelpFormatter().printHelp("backtest -f etc/demostralet.conf: ", option)
        System.exit(-1)
    }


    val stralet_conf = line.getOptionValue("f")

    val container = new Container()
    container.init("etc/backtest.conf")

    val session = container.createTestFromFile(stralet_conf)
    session.run()
}

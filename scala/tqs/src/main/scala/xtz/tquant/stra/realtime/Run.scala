package xtz.tquant.stra.realtime

object Run {

    def run(stralet_conf : String, realtime_conf : String): Unit = {
        Config.load(realtime_conf)
        val container = new Container()
        container.run(stralet_conf)
    }
}

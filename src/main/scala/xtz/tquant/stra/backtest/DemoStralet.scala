package xtz.tquant.stra.backtest

import com.tictactec.ta.lib.{MAType, MInteger}
import xtz.tquant.api.scala.DataApi.{Bar, MarketQuote}
import xtz.tquant.api.scala.{DataApi, TradeApi}
import xtz.tquant.stra.stralet.{Stralet, StraletConfig, StraletContext}

/**
  * Created by terryxu on 2017/3/26.
  */
class DemoStralet extends Stralet {

    var cfg : StraletConfig = _
    var context : StraletContext = _
    val talib = new com.tictactec.ta.lib.Core()
    var stk_account = ""
    var trade_api : TradeApi = _
    var data_api  : DataApi = _

    override def onInit(cfg: StraletConfig): Unit = {
        println("DemoStralet onInit ", cfg.context.getTime())

        this.cfg = cfg
        this.context = cfg.context
        this.trade_api = context.getTradeApi
        this.data_api = context.getDataApi
        this.stk_account = cfg.parameters.getOrElse("stk_account", "").asInstanceOf[String]

        context.subscribeBar(cfg.universe)

        val (positions, _) = trade_api.queryPosition(this.stk_account)
        val (balance, _) = trade_api.queryBalance(this.stk_account)

        var value = balance.enable_balance
        for (pos <- positions) {
            val (q, _) = data_api.quote(pos.code)
            println( pos.code, pos.current_size, q.last)
            value += q.last * pos.current_size
        }

        println(s"value: $value")

    }

    override def onFini() = {
        println("DemoStralet onFini ", cfg.context.getTime())

        val (positions, _) = trade_api.queryPosition(this.stk_account)
        val (balance, _) = trade_api.queryBalance(this.stk_account)

        var value = balance.enable_balance

        for (pos <- positions) {
            val (q, _) = data_api.quote(pos.code)
            println( pos.code, pos.current_size, q.last)
            value += q.last * pos.current_size
        }

        println(s"value: $value")
    }

    override def onQuote(q: MarketQuote) = {

    }

    override def onBar(bars: Map[String, Seq[Bar]]): Unit = {
        //println("DemoStralet onBar " + context.getTimeAsInt())
        //bar foreach println _

        // MA5 > MA60 BUY
        // MA60 < MA5 SELL
        if (bars.isEmpty || bars.head._2.size < 60) return

        val (tmp_positions, _) = trade_api.queryPosition(this.stk_account)
        val (balance, _) = trade_api.queryBalance(this.stk_account)

        val positions = tmp_positions.map ( x => x.code -> x).toMap

        var enable_balance = balance.enable_balance

        for ( (code, data) <- bars ) {

            val close_price = data.map (_.close).toArray

            val ma5 = new Array[Double](close_price.length)
            val ma30 = new Array[Double](close_price.length)
            val beg_idx = new MInteger
            val ind_length = new MInteger

            talib.sma(0, close_price.length -1 , close_price, 5, beg_idx, ind_length, ma5)
            val ma5_length = ind_length.value

            talib.sma(0, close_price.length -1 , close_price, 30, beg_idx, ind_length, ma30)
            val ma30_length = ind_length.value

            if (ma30(ma30_length -1) > ma5(ma5_length-1)) {
                if (ma30(ma30_length - 2) <= ma5(ma5_length - 2)) {
                    // up cross, buy some
                    val (q, _) = data_api.quote(code)
                    var cost = q.last * 100
                    if ( cost < enable_balance) {
                        enable_balance -= cost
                        trade_api.placeOrder(stk_account, code, q.last, 100, "Buy")
                    }
                }
            } else if (ma30(ma30_length -1) < ma5(ma5_length-1)) {
                if (ma30(ma30_length - 2) >= ma5(ma5_length - 2)) {
                    // dead cross, sell some
                    val pos = positions.getOrElse(code, null)
                    if (pos != null && pos.enable_size >= 100) {
                        val (q, _) = data_api.quote(code)
                        trade_api.placeOrder(stk_account, code, q.last, 100, "Sell")
                    }
                }
            }
        }
    }

    override def onTimer(id: Int, data: Any) = {

    }

    override def onMsg(msg: Any) = {

    }

    override def onCycle(): Unit = {
        val (date, time) = context.getTimeAsInt()
        println(s"onCycle $date $time")
    }
}

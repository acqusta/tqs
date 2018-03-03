package xtz.tquant.stra.stralet

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.acqusta.tquant.api.scala.TradeApi
import xtz.tquant.stra.stralet.TqsTradeApi.NetPosition


object TqsTradeApi {
    @JsonIgnoreProperties(ignoreUnknown = true)
    case class NetPosition (
        account_id     : String ,   // 帐号编号
        code           : String ,   // 证券代码
        name           : String ,   // 证券名称
        current_size   : Long   ,   // 当前持仓
        enable_size    : Long   ,   // 可用（可交易）持仓
        init_size      : Long   ,   // 初始持仓
        today_size     : Long   ,   // 今日持仓
        frozen_size    : Long   ,   // 冻结持仓
        cost           : Double ,   // 成本
        cost_price     : Double ,   // 成本价格
        last_price     : Double ,   // 最新价格
        close_pnl      : Double ,   // 平仓盈亏
        float_pnl      : Double ,   // 浮动盈亏
        margin         : Double ,   // 保证金
        commission     : Double     // 手续费
    )

    class EntrustOder (
        code   : String,

    )
}


trait TqsTradeApi extends TradeApi {

    def queryNetPosition(account_id: String) : (Seq[NetPosition], String)

    /**
      * 根据当前当前持仓自动选择委托动作，返回entrust_no
      *
      * @param account_id
      * @param code
      * @param price
      * @param size
      * @return
      */
    def placeAutoOrder(account_id: String, code: String, price : Double, size: Long) : (String, String)

    def cancelAutoOrder(account_id: String, code: String, entrust_no: String) : (Boolean, String)

    //def placeAlgoOrder(account_id: String, )
}

#ifndef _TQUANT_ALGO_H
#define _TQUANT_ALGO_H

#include "stralet.h"

using namespace std;

namespace tquant { namespace stra {

    class PorfolioManagerAlgoImpl;

    struct NetPosition {
        string  account_id;       // 帐号编号
        string  code;             // 证券代码
        string  name;             // 证券名称
        int64_t current_size;     // 当前持仓
                                  //int64_t enable_size;      // 可用（可交易）持仓
        int64_t init_size;        // 初始持仓
        double  cost;             // 成本
        double  cost_price;       // 成本价格
        double  last_price;       // 最新价格
        //double  float_pnl;        // 持仓盈亏
        //double  close_pnl;        // 平仓盈亏
        //double  margin;           // 保证金
        //double  commission;       // 手续费

        NetPosition()
            : current_size(0), init_size(0)
            , cost(0.0), cost_price(0.0), last_price(0.0)
        {
        }
    };


    class PorfolioManagerAlgo : public AlgoStralet {
    public:
        // interface AlgoStralet
        virtual void on_fini            () { }
        virtual void on_quote           (shared_ptr<MarketQuote> q) { }
        virtual void on_bar             (const char* cycle, shared_ptr<Bar> bar) { }
        virtual void on_timer           (int32_t id, void* data) { }
        virtual void on_event           (const string& evt, void* data) { }
        virtual void on_order_status    (shared_ptr<Order> order) { }
        virtual void on_order_trade     (shared_ptr<Trade> trade) { }
        virtual void on_account_status  (shared_ptr<AccountInfo> account) { }

        void init(const string& account_id, const vector<string>& universe);

        CallResult<vector<NetPosition>> query_net_position();
        CallResult<vector<Position>>    query_position();

        void set_target(const vector<NetPosition>& target);
        void stop_target();
        bool is_stopped();

        // return task_id, not entrust_id
        CallResult<string>  place_order (const string& code, double price, int64_t inc_size);
        CallResult<bool>    cancel_order(const string& code, const string& task_id);
    private:

        PorfolioManagerAlgoImpl* m_impl;
    };

    //class TWapAlgo : public AlgoStralet {
    //public:
    //};
    //
    //class VWapAlgo : public AlgoStralet {
    //public:
    //};


} }

#endif

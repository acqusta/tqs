#ifndef _SIM_TAPI_H
#define _SIM_TAPI_H

#include <unordered_map>
#include "stralet.h"
#include "tquant_api.h"

using namespace tquant::api;
using namespace tquant::stra;

class SimStraletContext;

struct TradeData {
    string  account_id;
    int32_t trading_day;
    double  init_balance;
    double  enable_balance;
    double  frozen_balance;

    unordered_map<string, shared_ptr<Position>> positions;     // code + side -> Position
    unordered_map<string, shared_ptr<Order>>    orders;     // entrust_no -> order
    unordered_map<string, shared_ptr<Trade>>    trades;     // fill_no -> trade
};

class SimAccount {
    friend SimStraletContext;
    friend SimTradeApi;
public:
    SimAccount(SimStraletContext* ctx, const string& account_id, double init_balance);

    CallResult<Balance>             query_balance();
    CallResult<vector<Order>>       query_orders();
    CallResult<vector<Trade>>       query_trades();
    CallResult<vector<Position>>    query_positions();
    CallResult<OrderID>             place_order(const char* code, double price, int64_t size, const char* action, int order_id);
    CallResult<bool>                cancel_order(const char* code, int order_id);
    CallResult<bool>                cancel_order(const char* code, const char* entrust_no);

    void try_match();

    void try_buy  (Order* order);
    void try_short(Order* order);
    void try_cover(Order* order);
    void try_sell (Order* order);

    void make_trade     (double price, Order* order);
    void update_position(const string& code, const string& side, int64_t size, double fill_price);
    Position* get_position(const string& code, const string& side);

    void move_to(int trading_day);
    void save_data(const string& dir);

private:
    shared_ptr<TradeData> m_tdata;
    vector<shared_ptr<TradeData>> m_his_tdata;

    list<shared_ptr<Order>> m_ord_status_ind_list;
    list<shared_ptr<Trade>> m_trade_ind_list;

    SimStraletContext* m_ctx;

    static int g_order_id;
    static int g_fill_id;;
};

class SimTradeApi : public TqsTradeApi {
    friend SimStraletContext;
public:
    SimTradeApi(SimStraletContext* ctx, vector<SimAccount*>& accounts)
        : m_ctx(ctx)
    {
        for (auto& e : accounts)
            m_accounts[e->m_tdata->account_id] = e;
    }

    // TradeApi
    virtual CallResult<vector<AccountInfo>> query_account_status();
    virtual CallResult<Balance> query_balance(const char* account_id);
    virtual CallResult<vector<Order>> query_orders(const char* account_id);
    virtual CallResult<vector<Trade>> query_trades(const char* account_id);
    virtual CallResult<vector<Position>> query_positions(const char* account_id);
    virtual CallResult<OrderID> place_order(const char* account_id, const char* code, double price, int64_t size, const char* action, int order_id);
    virtual CallResult<bool> cancel_order(const char* account_id, const char* code, int order_id);
    virtual CallResult<bool> cancel_order(const char* account_id, const char* code, const char* entrust_no);
    virtual CallResult<string> query(const char* account_id, const char* command, const char* params);
    virtual void set_callback(TradeApi_Callback* callback);

    // TqsTradeApi
    virtual CallResult<vector<NetPosition>> query_net_position(const char* account_id);
    virtual CallResult<string>  place_auto_order(const char* account_id, const char* code, int64_t size);
    virtual CallResult<bool>    cancel_auto_order(const char* account_id, const char* code, const char* entrust_no);

    SimAccount* get_account(const char* account_id) {
        auto it = m_accounts.find(account_id);
        return it != m_accounts.end() ? it->second : nullptr;
    }

    void try_match();

    void move_to(int trading_day);

    const unordered_map<string, SimAccount*> accounts() { return m_accounts; }

private:
    SimStraletContext* m_ctx;
    unordered_map<string, SimAccount*> m_accounts;
};

#endif


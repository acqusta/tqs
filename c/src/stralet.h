#ifndef _TQUANT_STRALET_H
#define _TQUANT_STRALET_H

#include <chrono>
#include <ostream>

#include "tquant_api.h"

namespace tquant { namespace stra {

    using namespace std::chrono;
    using namespace tquant::api;

    class StraletContext;

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

    struct DateTime {
        int date;
        int time;

        DateTime() : date(0), time(0)
        {}

        DateTime(int a_date, int a_time) : date(a_date), time(a_time)
        {}

        int cmp(const DateTime& dt) {
            if (this->date < dt.date) return -1;
            if (this->date == dt.date) return this->time - dt.time;
            return 1;
        }
    };


    class TqsTradeApi : public TradeApi {
    public:
        virtual CallResult<vector<NetPosition>> query_net_position(const char* account_id) = 0;
        virtual CallResult<string>  place_auto_order(const char* account_id, const char* code, int64_t size) = 0;
        virtual CallResult<bool>    cancel_auto_order(const char* account_id, const char* code, const char* entrust_no) = 0;
    };

    class Stralet {
    public:
        virtual ~Stralet() { }

        inline StraletContext* ctx() {
            return m_ctx;
        }

        virtual void on_init(StraletContext* sc) {
            m_ctx = sc;
        }

        virtual void on_fini            () { }
        virtual void on_quote           (shared_ptr<MarketQuote> q) { }
        virtual void on_bar             (const char* cycle, shared_ptr<Bar> bar) { }
        virtual void on_timer           (int32_t id, void* data) { }
        virtual void on_event           (const string& evt, void* data) { }
        virtual void on_order_status    (shared_ptr<Order> order) { }
        virtual void on_order_trade     (shared_ptr<Trade> trade) { }
        virtual void on_account_status  (shared_ptr<AccountInfo> account) { }
    protected:
        StraletContext* m_ctx;
    };

    enum LogLevel {
        INFO,
        WARNING,
        ERROR,
        FATAL
    };

    class StraletContext {
    public:
        virtual int32_t trading_day() = 0;
        virtual void cur_time(DateTime* dt) = 0;
        virtual system_clock::time_point cur_time() = 0;
        virtual void post_event(const char* evt, void* data) = 0;

        virtual void set_timer(int32_t id, int32_t delay, void* data) = 0;
        virtual void kill_timer(int32_t id) = 0;

        virtual DataApi*  data_api(const char* source = nullptr) = 0;
        virtual TradeApi* trade_api() = 0;

        virtual ostream& logger(LogLevel level = LogLevel::INFO) = 0;

        virtual string get_parameter(const char* name, const char* def_value) = 0;

        virtual string mode() = 0;
    };
} }

#endif

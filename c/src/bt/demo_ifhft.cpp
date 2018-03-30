#include <iostream>
#include <sstream>
#include <string.h>
#include <list>
#include <assert.h>
#include "stralet.h"
#include "myutils/csvparser.h"

using namespace tquant::stra;
using namespace tquant::api;


static inline int HMS(int h, int m, int s = 0, int ms = 0) { return h * 10000000 + m * 100000 + s * 1000; }

class IFHftStralet : public Stralet {

    struct PricePair {
        shared_ptr<const MarketQuote> last_index_quote[2];
        shared_ptr<const MarketQuote> last_if_quote[2];
    };
    enum State {
        S_IDLE,
        S_OPEN,
        S_CLOSE,
        S_CLEAR,
    };
    static const int TIMER_STOP_ORDER   = 1;
    static const int TIMER_CANCEL_OPEN  = 2;
    static const int TIMER_CANCEL_CLOSE = 3;

public:
    virtual void on_init  (StraletContext* sc) override;
    virtual void on_quote (shared_ptr<const MarketQuote> quote) override;
    virtual void on_timer(int32_t id, void* data) override;
    virtual void on_fini() override;

    virtual void on_order_status(shared_ptr<const Order> order);
    virtual void on_order_trade(shared_ptr<const Trade> trade);

    shared_ptr<const OrderID> place_order(const string& code, double price, int64_t size, const string action);

    void clear_data();
private:
    string m_account_id;
    shared_ptr<const MarketQuote> m_if_quote;
    shared_ptr<const MarketQuote> m_000300_quote;
    shared_ptr<const OrderID>     m_open_oid;
    shared_ptr<const OrderID>     m_close_oid;
    shared_ptr<const Order>       m_open_order;
    shared_ptr<const Order>       m_close_order;
    State m_state = S_IDLE;;
    string   m_close_action;
    double   m_close_price;
    double   m_fixed_price_diff;
    DateTime m_open_time;

    PricePair m_price;

};

void IFHftStralet::on_init(StraletContext* sc)
{
    Stralet::on_init(sc);

    sc->logger() << "on_init: " << sc->trading_day() << endl;
    m_account_id  = "sim";

    {
        auto r = sc->data_api()->subscribe(vector<string>{"IF.CFE", "000300.SH"});
        assert(r.value);
    }

    int last_tradingday = 0;
    {
        auto r = sc->data_api()->daily_bar("IF.CFE", "", true);
        assert(r.value);
        last_tradingday = r.value->at(r.value->size() - 1).date;
    }

    double avg_if_price = 0.0;
    double avg_000300_price = 0.0;
    {
        auto r = sc->data_api()->bar("IF.CFE", "1m", last_tradingday, true);
        assert(r.value && r.value->size()>=240);
        auto bars = r.value;
        double v = 0.0;
        for (int i = 1; i < bars->size() - 1; i++) v += bars->at(i).close;
        avg_if_price = v / bars->size() - 2;
    }
    {
        auto r = sc->data_api()->bar("000300.SH", "1m", last_tradingday, true);
        assert(r.value && r.value->size() >= 240);
        auto bars = r.value;
        double v = 0.0;
        for (int i = 1; i < bars->size() - 1; i++) v += bars->at(i).close;
        avg_000300_price = v / bars->size() - 2;
    }

    m_fixed_price_diff = avg_000300_price - avg_if_price;
}

void IFHftStralet::on_fini() 
{
    auto r = ctx()->data_api()->tick("IF.CFE", 0);
    if (r.value)
        ctx()->logger(INFO) <<  "on_fini: " << ctx()->trading_day() << ", tick count " << r.value->size() << endl;
    else
        ctx()->logger(FATAL) << "on_fini: " << ctx()->trading_day() << "," << r.msg << endl;
}

static bool is_finished(const Order* order)
{
    return
        order->status == OS_Filled ||
        order->status == OS_Cancelled ||
        order->status == OS_Rejected;
}

inline int time_diff(int t1, int t2)
{
    int ms1 =  t1 % 1000            , ms2 =  t2 % 1000          ;
    int s1  = (t1 / 1000) % 100     , s2  = (t2 / 1000) % 100    ;
    int m1  = (t1 / 100000) % 100   , m2  = (t2 / 100000) % 100  ;
    int h1  = (t1 / 10000000) % 100 , h2  = (t2 / 10000000) % 100;
    return ((h1 - h2) * 3600 + (m1 - m2) * 60 + s1 - s2) * 1000 + ms1 - ms2;
}

shared_ptr<const OrderID> IFHftStralet::place_order(const string& code, double price, int64_t size, const string action)
{
    DateTime dt;
    m_ctx->cur_time(&dt);
    ctx()->logger(INFO) << dt.time << " " << "place order: " << code << "," << price << "," << size << "," << action << endl;
    auto r = m_ctx->trade_api()->place_order(m_account_id, code, price, size, action, 0);
    if (!r.value)
        ctx()->logger(ERROR) << "place_order error:" << r.msg << endl;
    return r.value;
}

void IFHftStralet::on_quote(shared_ptr<const MarketQuote> quote)
{
    if (m_state != S_IDLE) return;

    DateTime now;
    m_ctx->cur_time(&now);
    if (now.time < HMS(9, 30, 30) || now.time> HMS(14, 45)) return;

    if (strcmp(quote->code, "000300.SH")==0) {
        if (!m_price.last_index_quote[0]) {
            m_price.last_index_quote[0] = quote;
            return;
        }

        // 两个tick之间价差5个点
        if (fabs(quote->last - m_price.last_index_quote[0]->last) < 3.0) {
            m_price.last_index_quote[0] = quote;
            m_price.last_if_quote[0] = nullptr;
            return;
        }
        else {
            m_price.last_index_quote[1] = quote;
        }
    }
    else if (strcmp(quote->code, "IF.CFE") == 0) {
        if (!m_price.last_index_quote[0]) return;
        if (!m_price.last_if_quote[0]) {
            if (time_diff(quote->time, m_price.last_index_quote[0]->time) > 5000) {
                // 回测中，000300.SH指数按照时间戳回放行情，效果是提前了3～5秒。
                // 000300.SH的时间戳间隔不是标准的5秒。
                // 延后5秒知道行情
                m_price.last_if_quote[0] = quote;
            }
            return;
        }
        else if (m_price.last_index_quote[1]) {
            if (time_diff(quote->time, m_price.last_index_quote[1]->time) > 5000) {
                int diff_index = m_price.last_index_quote[1]->last - m_price.last_index_quote[0]->last;
                if (diff_index > 0) {
                    int diff_if = diff_index - (quote->ask1 - m_price.last_if_quote[0]->last);
                    if (diff_if >= 4) {
                        // 低估
                        m_open_oid = place_order("IF.CFE", m_if_quote->ask1, 1, EA_Buy);
                        m_close_price = m_000300_quote->last;
                        m_close_action = EA_Sell;
                        m_ctx->cur_time(&m_open_time);
                        m_ctx->set_timer(this, TIMER_CANCEL_OPEN, 1000, nullptr);
                        m_state = S_OPEN;
                    }
                }
                else {
                    int diff_if = diff_index + (quote->bid1 - m_price.last_if_quote[0]->last);
                    if (diff_if <= -4) {
                        // 高估
                        m_open_oid = place_order("IF.CFE", m_if_quote->bid1, 1, EA_Short);
                        m_close_price = m_000300_quote->last;
                        m_close_action = EA_Cover;
                        m_ctx->cur_time(&m_open_time);
                        m_ctx->set_timer(this, TIMER_CANCEL_OPEN, 1000, nullptr);
                        m_state = S_OPEN;
                    }
                }
            }
        }
    }
}

void IFHftStralet::on_order_status(shared_ptr<const Order> order)
{
    if (order->order_id == m_open_oid->order_id) {
        m_open_order = order;
        if (order->status == OS_Rejected ||
            order->status == OS_Cancelled ) 
        {            
            m_ctx->kill_timer(this, TIMER_CANCEL_OPEN);
            clear_data();
            m_state = S_IDLE;
        }
        else if (order->status == OS_Filled ) {
            // Do nothing. Should have change state in on_order_trade
        }
    }
    else if (order->order_id == m_close_oid->order_id) {
        m_close_order = order;
        if (m_state == S_CLOSE) {
            if (order->status == OS_Rejected ||
                order->status == OS_Cancelled)
            {
                // 以当前价格立即平仓, 应该以对手价平仓
                m_close_oid = m_ctx->trade_api()->place_order(m_account_id,
                                                            "IF.CFE",
                                                            m_if_quote->last,
                                                            1,
                                                            m_close_action,
                                                            0).value;

                m_close_order = nullptr;

                m_ctx->kill_timer(this, TIMER_CANCEL_CLOSE);
                m_ctx->set_timer (this, TIMER_CANCEL_CLOSE, 1000, nullptr);
                m_state = S_CLEAR;
            }
            else if (order->status == OS_Filled) {
                // Do nothing. Should have change state in on_order_trade
            }
        }
    }
}

void IFHftStralet::on_order_trade(shared_ptr<const Trade> trade)
{
    if (m_open_order && trade->entrust_no == m_open_order->entrust_no) {
        m_close_oid = place_order("IF.CFE", m_close_price, 1, m_close_action);

        m_ctx->kill_timer(this, TIMER_CANCEL_OPEN);

        DateTime now;
        m_ctx->cur_time(&now);
        int cancel_time = 6000 - time_diff(now.time, m_open_time.time);
        m_ctx->set_timer (this, TIMER_CANCEL_CLOSE, cancel_time, nullptr);
        m_state = S_CLOSE;
    }
    else if (m_close_order && trade->entrust_no == m_close_order->entrust_no) {
        assert(m_state == S_CLOSE || S_CLEAR);
        m_ctx->kill_timer(this, TIMER_CANCEL_CLOSE);
        clear_data();
        m_state = S_IDLE;
    }
}

void IFHftStralet::clear_data()
{
    m_open_order = nullptr;
    m_open_oid = nullptr;
    m_close_order = nullptr;
    m_close_oid = nullptr;
}

void IFHftStralet::on_timer(int32_t id, void* data)
{
    auto tapi = m_ctx->trade_api();
    
    if (id ==  TIMER_CANCEL_OPEN) {
        assert(m_open_oid);
        tapi->cancel_order(m_account_id, "IF.CFE", m_open_oid->entrust_no);
        m_ctx->set_timer(this, TIMER_CANCEL_OPEN, 1000, nullptr);
    }
    else if (id == TIMER_CANCEL_CLOSE) {
        assert(m_close_oid);
        assert(m_state == S_CLOSE || m_state == S_CLEAR);
        tapi->cancel_order(m_account_id, "IF_CFE", m_close_oid->entrust_no);
        m_ctx->set_timer(this, TIMER_CANCEL_CLOSE, 1000, nullptr);
    }
}

Stralet* create_ifhft()
{
    return new IFHftStralet();
}

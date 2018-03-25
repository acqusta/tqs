#include <assert.h>
#include <algorithm>
#include <unordered_map>
#include <unordered_set>

#include "sim_data.h"
#include "sim_context.h"

using namespace tquant::api;
using namespace tquant::stra;

int cmp_time(int date_1, int time_1, int date_2, int time_2)
{
    if (date_1 < date_2) return -1;
    if (date_1 == date_2) return time_1 - time_2;
    return 1;
}

CallResult<vector<MarketQuote>> SimDataApi::tick(const char* code, int trading_day)
{
    if (trading_day == 0 || trading_day == m_ctx->trading_day()) {
        auto it = m_tick_caches.find(code);
        if (it == m_tick_caches.end())
            return CallResult<vector<MarketQuote>>("-1,no tick data");
        if (it->second.pos < 0)
            return CallResult<vector<MarketQuote>>("-1,not arrive yet");
        auto ticks = make_shared < vector<MarketQuote>>(it->second.ticks->begin(), it->second.ticks->begin() + it->second.pos);
        return CallResult<vector<MarketQuote>>(ticks);
    }
    else {
        return m_dapi->tick(code, trading_day);
    }
}

CallResult<vector<Bar>> SimDataApi::bar(const char* code, const char* cycle, int trading_day, bool align)
{
    assert(strcmp(cycle, "1m") == 0);
    if (!align)
        return CallResult<vector<Bar>>("-1,bactest's bar should be aligned");

    if (trading_day == 0 || trading_day == m_ctx->trading_day()) {
        auto it = m_bar_caches.find(code);
        if (it == m_bar_caches.end())
            return CallResult<vector<Bar>>("-1,no bar data");
        if (it->second.pos < 0)
            return CallResult<vector<Bar>>("-1,not arrive yet");
        auto bars = make_shared<vector<Bar>>(it->second.bars->begin(), it->second.bars->begin() + it->second.pos);
        return CallResult<vector<Bar>>(bars);
    }
    else if (trading_day < m_ctx->trading_day())
        return m_dapi->bar(code, cycle, trading_day, align);
    else
        return CallResult<vector<Bar>>("-1,try to get data after current trading_day");
}

CallResult<vector<DailyBar>> SimDataApi::daily_bar(const char* code, const char* price_adj, bool align)
{
    // TODO: get it from cache.
    auto r = m_dapi->daily_bar(code, price_adj, align);
    if (r.value) {
        auto today = m_ctx->trading_day();
        auto bars = r.value;
        auto it = find_if(bars->rbegin(), bars->rbegin(), [today](DailyBar& b) { return b.date < today; });
        if (it == bars->rend())
            // Shouldn't happen?
            return CallResult< vector<DailyBar> > ("-1,no data");
        else {
            auto new_bars = make_shared<vector<DailyBar>>(bars->begin(), it.base());
            return CallResult<vector<DailyBar>>(new_bars);
        }
    }
    else {
        return r;
    }
}

CallResult<MarketQuote> SimDataApi::quote(const char* code)
{
    if (m_ctx->data_level() == BT_BAR1M) {
        auto it = m_bar_caches.find(code);
        if (it == m_bar_caches.end())
            return CallResult<MarketQuote>("-1,no bar data");
        if (it->second.pos < 0)
            return CallResult<MarketQuote>("-1,not arrive yet");
        auto bar = &(it->second.bars->at(it->second.pos));
        auto q = make_shared<MarketQuote>();
        q->set_code(code);
        q->date = bar->date;
        q->time = bar->time;
        q->trading_day = bar->trading_day;
        q->last = bar->close;
        q->oi = bar->oi;
        return CallResult<MarketQuote>(q);
    }
    else if (m_ctx->data_level() == BT_TICK) {
        auto it = m_tick_caches.find(code);
        if (it == m_tick_caches.end())
            return CallResult<MarketQuote>("-1,no tick data");
        if (it->second.pos < 0)
            return CallResult<MarketQuote>("-1,not arrive yet");
        auto q = make_shared<MarketQuote>(it->second.ticks->at(it->second.pos));
        return CallResult<MarketQuote>(q);
    }
    else {
        return CallResult<MarketQuote>("-1,support quote when testing using bar1d");
    }
}

CallResult<vector<string>> SimDataApi::subscribe(const vector<string>& codes)
{
    DateTime dt;
    m_ctx->cur_time(&dt);

    if (m_ctx->data_level() == BT_BAR1M || m_ctx->data_level() == BT_TICK) {
        for (auto& code : codes) {
            if (m_bar_caches.find(code) != m_bar_caches.end()) continue;
            auto r = m_dapi->bar(code.c_str(), "1m", m_ctx->trading_day(), true);
            if (!r.value) continue;

            auto bars = r.value;
            auto it = find_if(bars->begin(), bars->end(), [&dt](Bar& a){
                return cmp_time(a.date, a.time, dt.date, dt.time) >= 0; 
            });
            int64_t pos = -1;
            if (it == bars->end()) {
                pos = bars->size() - 1;
            }
            else {
                if (cmp_time(it->date, it->time, dt.date, dt.time) == 0)
                    pos = it - bars->begin();
                else
                    pos = it - bars->begin() - 1;
            }

            BarTickCache cache;
            cache.pos = pos;
            cache.bars = bars;
            m_bar_caches[code] = cache;            
        }
    }

    if (m_ctx->data_level() == BT_TICK) {
        for (auto& code : codes) {
            if (m_tick_caches.find(code) != m_tick_caches.end()) continue;
            auto r = m_dapi->tick(code.c_str(), m_ctx->trading_day());
            if (!r.value) continue;

            auto ticks = r.value;
            auto it = find_if(ticks->begin(), ticks->end(), [&dt](MarketQuote& a) {
                return cmp_time(a.date, a.time, dt.date, dt.time) >= 0;
            });
            int64_t pos = -1;
            if (it == ticks->end()) {
                pos = (int64_t)ticks->size() - 1;
            }
            else {
                if (cmp_time(it->date, it->time, dt.date, dt.time) == 0)
                    pos = it - ticks->begin();
                else
                    pos = it - ticks->begin() - 1;
            }

            TickCache cache;
            cache.pos = pos;
            cache.ticks = ticks;
            m_tick_caches[code] = cache;
        }
    }

    for (auto& code : codes) m_codes.insert(code);

    auto ret_codes = make_shared<vector<string>>();
    for (auto& c : codes) ret_codes->push_back(c);
    sort(ret_codes->begin(), ret_codes->end());
    
    return CallResult<vector<string>>(ret_codes);
}

CallResult<vector<string>> SimDataApi::unsubscribe(const vector<string>& codes)
{
    for (auto & c : codes) {
        auto it1 = m_bar_caches.find(c);
        if (it1 != m_bar_caches.end()) m_bar_caches.erase(it1);
        auto it2 = m_tick_caches.find(c);
        if (it2 != m_tick_caches.end()) m_tick_caches.erase(it2);

        auto it3 = m_codes.find(c);
        if (it3 != m_codes.end()) m_codes.erase(it3);
    }

    auto ret_codes = make_shared<vector<string>>();
    for (auto& c : codes) ret_codes->push_back(c);
    sort(ret_codes->begin(), ret_codes->end());

    return CallResult<vector<string>>(ret_codes);
}

void SimDataApi::set_callback(DataApi_Callback* callback)
{

}

void SimDataApi::calc_nex_time(DateTime* dt)
{
    int date = 99999999, time = 0;

    for (auto& e : m_bar_caches) {
        auto cache = &e.second;
        Bar* bar;
        if (cache->pos + 1 < (int64_t)cache->bars->size())
            bar = &(*cache->bars)[cache->pos + 1];
        else
            bar = &(*cache->bars)[cache->pos];
        if (cmp_time(date, time, bar->date, bar->time) > 0) {
            date = bar->date;
            time = bar->time;
        }
    }
    for (auto& e : m_tick_caches) {
        auto cache = &e.second;
        MarketQuote* q;
        if (cache->pos + 1 < (int64_t)cache->ticks->size())
            q = &(*cache->ticks)[cache->pos + 1];
        else
            q = &(*cache->ticks)[cache->pos];
        if (cmp_time(date, time, q->date, q->time) > 0) {
            date = q->date;
            time = q->time;
        }
    }

    dt->date = date;
    dt->time = time;
}

shared_ptr<MarketQuote> SimDataApi::next_quote(const string& code)
{
    auto it = m_tick_caches.find(code);
    if (it == m_tick_caches.end())
        return nullptr;

    auto cache = &it->second;
    DateTime dt;
    m_ctx->cur_time(&dt);

    if (cache->pos + 1 >= (int64_t)cache->ticks->size()) return nullptr;
    auto q = &(*cache->ticks)[cache->pos + 1];
    if (cmp_time(q->date, q->time, dt.date, dt.time) <= 0) {
        cache->pos += 1;
        return make_shared<MarketQuote>(*q);
    }
    else {
        return nullptr;
    }
}

shared_ptr<Bar> SimDataApi::next_bar(const string& code)
{
    auto it = m_bar_caches.find(code);
    if (it == m_bar_caches.end())
        return nullptr;

    auto cache = &it->second;
    DateTime dt;
    m_ctx->cur_time(&dt);

    if (cache->pos + 1 >= (int64_t)cache->bars->size()) return nullptr;
    auto bar = &(*cache->bars)[cache->pos + 1];
    if (cmp_time(bar->date, bar->time, dt.date, dt.time) <= 0) {
        cache->pos += 1;
        return make_shared<Bar>(*bar);
    }
    else {
        return nullptr;
    }
}

const Bar* SimDataApi::last_bar(const string & code)
{
    auto it = m_bar_caches.find(code);
    if (it == m_bar_caches.end())
        return nullptr;

    auto cache = it->second;
    return cache.pos >= 0 ? &cache.bars->at(cache.pos) : nullptr;
}

void SimDataApi::move_to(int trading_day)
{
    this->m_tick_caches.clear();
    this->m_bar_caches.clear();
    this->m_codes.clear();
}
//#include "backtest.h
#include <iostream>
#include "stralet.h"


// https://uqer.io/ R-Breaker����
//
// R-Breaker ��һ�ֶ������ڽ��ײ��ԣ�����������ƺͷ�ת���ֽ��׷�ʽ���ò���Ҳ���ڱ�Future Thruth ��־��Ϊ��׬Ǯ�Ĳ���֮һ�������ڱ���
// 500 ��ָ�ڻ���Ч����ѡ��ò��Ե���Ҫ�ص����£�
//
// ��һ������ǰһ�������յ����̼ۡ���߼ۺ���ͼ�����ͨ��һ����ʽ�����������λ���Ӵ�С����Ϊͻ������ۡ��۲������ۡ���ת�����ۡ���ת��
// ��ۡ��۲�����ۺ�ͻ�������ۣ��Դ����γɵ�ǰ���������н��׵Ĵ���������ͨ���Լ��㷽ʽ�ĵ��������Ե��������۸��ľ��룬��һ���ı䴥����
// ����
//
// �ڶ����������м۸����ƣ�ʵʱ�жϴ��������������������£�
// 1) ��������߼۳����۲������ۺ����м۸���ֻ��䣬�ҽ�һ�����Ʒ�ת�����۹��ɵ�֧����ʱ����ȡ��ת���ԣ����ڸõ�λ�����֡����֣����գ�
// 2) ��������ͼ۵��ڹ۲�����ۺ����м۸���ַ������ҽ�һ��������ת����۹��ɵ�������ʱ����ȡ��ת���ԣ����ڸõ�λ�����֡����֣����ࣻ
// 3) �ڿղֵ�����£�������м۸񳬹�ͻ������ۣ����ȡ���Ʋ��ԣ����ڸõ�λ�������ࣻ
// 4) �ڿղֵ�����£�������м۸����ͻ�������ۣ����ȡ���Ʋ��ԣ����ڸõ�λ�������ա�
//
// �������趨ֹ���Լ�ֹӯ������
//
// ���ġ��趨����������
//
// ���塢��ÿ������ǰ�������ֺ�Լ����ƽ�֡�
//
// ������������������λ�γɵ�������֧��λ����������£�
//
// �۲������� = High + 0.35 * (Close �C Low)
// �۲������ = Low �C 0.35 * (High �C Close)
// ��ת������ = 1.07 / 2 * (High + Low) �C 0.07 * Low
// ��ת����� = 1.07 / 2 * (High + Low) �C 0.07 * High
// ͻ������� = �۲������� + 0.25 * (�۲������� �C �۲������)
// ͻ�������� = �۲������ �C 0.25 * (�۲������� �C �۲������)
// ���У�High��Close��Low �ֱ�Ϊ������߼ۡ��������̼ۺ�������ͼۡ���������λ�Ӵ�Сһ���ǣ�
// ͻ������ۡ��۲찮���ۡ���ת�����ۡ���ת����ۡ��۲�����ۺ�ͻ�������ۡ�

using namespace tquant::stra;
using namespace tquant::api;

struct PriceRange {
    double sell_setup;  // �۲��
    double buy_setup;   
    double sell_enter;  // ��ת��
    double buy_enter;
    double sell_break;  // ͻ�Ƽ�
    double buy_break;
};

static inline int HMS(int h, int m, int s = 0, int ms = 0) { return h * 10000000 + m * 100000 + s * 1000; }

class RBreakerStralet : public Stralet {

    PriceRange price_range;
    int count_1 = 0;
    int count_2 = 0;
    string account_id = "sim";
    string contract = "RB.SHF";

public:
    virtual void on_init(StraletContext* sc) override;
    virtual void on_bar(const char* cycle, shared_ptr<Bar> bar) override;
    virtual void on_fini() override;

    int cancel_unfinished_order();

    void place_order(const string& code, int64_t size, double price, const string action);
};

void RBreakerStralet::on_init(StraletContext* sc)
{
    Stralet::on_init(sc);

    cout << "on_init: " << sc->trading_day() << endl;
    // TODO: �������еõ�Ҫ���׵���Ʒ�ڻ���Ȼ���������Լӳ����еõ����ս��׵ĺ�Լ
    sc->data_api()->subscribe(vector<string>{contract});
    //sc->data_api()->subscribe(vector<string>{"000001.SH"});

    //data_api.subscribe(Array(contract))

    //val(daily_bar, msg) = data_api.bar(contract, "1d")
    //assert(daily_bar != null, "can't get bar1d: " + msg)
    //assert(daily_bar.last.date < sc.getTradingDay)

    //// ���ϸ������ռ۸���������ļ۸�����
    //val last_day = daily_bar.last
    //price_range = calcPriceRange(last_day.high, last_day.low, last_day.close)

    //sc.log(price_range)
}

void RBreakerStralet::on_fini() 
{
    //    sc.log("onFini", sc.getTime)

    //    val(cur_pos, _) = trade_api.queryPositions(this.account)
    //    val(long_size, short_size) = cur_pos.foldLeft((0L, 0L)) {
    //    (v, x) = >
    //        if (x.side == "Long")
    //            (v._1 + x.current_size, v._2)
    //        else
    //            (v._1 , v._2 + x.current_size)
    //}

    //if (long_size != 0 || short_size != 0)
    //    sc.log(s"Error: should close all positions, $contract, $long_size, $short_size")  
}

static PriceRange calc_price_range(double high, double low, double close ) 
{
    double high_beta = 0.35;
    double low_beta = 0.25;
    double enter_beta = 0.07;

    double sell_setup = high + high_beta * (close - low); //# �۲�������
    double buy_setup  = low - high_beta * (high - close); // �۲������
    double sell_enter = (1 + enter_beta) / 2 * (high + low) - enter_beta * low; // # ��ת������
    double buy_enter  = (1 + enter_beta) / 2 * (high + low) - enter_beta * high; // # ��ת�����
    double sell_break = buy_setup - low_beta * (sell_setup - buy_setup); // # ͻ��������
    double buy_break  = sell_setup + low_beta * (sell_setup - buy_setup); //# ͻ�������

    PriceRange range;
    range.sell_setup = sell_setup  ;
    range.buy_setup  = buy_setup   ;
    range.sell_enter = sell_enter  ;
    range.buy_enter  = buy_enter   ;
    range.sell_break = sell_break  ;
    range.buy_break  = buy_break   ;
    return range;
}

static bool is_finished(Order* order) 
{
    return
        order->status == OS_Filled ||
        order->status == OS_Cancelled ||
        order->status == OS_Rejected;
}

int RBreakerStralet::cancel_unfinished_order() 
{
    auto tapi = ctx()->trade_api();
    auto r = tapi->query_orders(account_id.c_str());
    if (!r.value) {
        ctx()->log((string("error: query_orders: ") + r.msg).c_str());
        return -1;
    }
    int count = 0;
    for (auto&ord : *r.value) {
        if (ord.code == contract && !is_finished(&ord)) {
            tapi->cancel_order(this->account_id.c_str(), ord.code.c_str(), ord.entrust_no.c_str());
            count++;
        }
    }
    return count;
}

void RBreakerStralet::place_order(const string& code, int64_t size, double price, const string action)
{
    auto r = m_ctx->trade_api()->place_order(account_id.c_str(), code.c_str(), size, price, action.c_str(), 0);
    if (!r.value)
        cerr << "place_order error:" << r.msg << endl;;
}

void RBreakerStralet::on_bar(const char* cycle, shared_ptr<Bar> bar) 
{
    if (strcmp(cycle, "1m")) return;
    if (bar->code != this->contract) return;

    auto tapi = ctx()->trade_api();
    auto dapi = ctx()->data_api();

    if (bar->time == HMS(9, 31)) {
        // ��ҹ��������ȡ high, low, close����
        double high = 0.0;
        double low = 100000000.0;
        double close = 0.0;
        auto r = ctx()->data_api()->bar(contract.c_str(), "1m", 0, true);
        for (auto & b : *r.value) {
            if (b.high > high) high = b.high;
            if (b.low < low)   low = b.low;
            close = b.close;
        }
        price_range = calc_price_range(high, low, close);
        return;
    }

    // ֻ��������, ��������bar
    if (bar->time < HMS(9, 32, 0) || bar->time > HMS(15, 0))
        return;

    // �򵥴����������;������ֱ��ȡ��
    if (cancel_unfinished_order() > 0) return;

    auto r = dapi->bar(contract.c_str(), "1m", 0, true);
    if (!r.value && r.value->size() < 2) {
        ctx()->log((string("error: dapi.bar:") + r.msg).c_str());
        return;
    }

    auto bars = r.value;
    auto bar_2 = &bars->at(bars->size() - 2);
    auto bar_1 = &bars->at(bars->size() - 1);

    int64_t long_size = 0, short_size = 0;
    {
        auto r = tapi->query_positions(account_id.c_str());
        if (!r.value) return;
        for (auto &pos : *r.value) {
            if (pos.code == this->contract) {
                if (pos.side == SD_Long)
                    long_size += pos.current_size;
                else
                    short_size += pos.current_size;
            }
        }
    }

    shared_ptr<MarketQuote> quote;
    {
        auto r = dapi->quote(contract.c_str());
        if (!r.value) return;
        quote = r.value;
    }

    if (bar->time >= HMS(14, 55)) {
        if (long_size != 0)
            place_order(contract, quote->bid1, long_size, EA_Sell);
        if (short_size != 0)
            place_order(contract, quote->bid1, short_size, EA_Cover);
        return;
    }
    // ����
    if (bar_2->close <= price_range.buy_break && bar_1->close > price_range.buy_break) {
        if (long_size == 0)
            place_order(contract, quote->ask1, 1, EA_Buy);

        if (short_size != 0)
            place_order(contract, quote->ask1, short_size, EA_Cover);
    }

    if (bar_2->close >= price_range.sell_break && bar_1->close < price_range.sell_break) {
        if (short_size == 0)
            place_order(contract, quote->bid1, 1, EA_Short);

        if (long_size != 0)
            place_order(contract, quote->bid1, long_size, EA_Sell);
    }

    // ��ת
    //   �൥��ת
    if (bar_1->high > price_range.sell_setup && bar_1->close > price_range.sell_enter)
        count_1 = 1;

    if (count_1 == 1 && bar_1->close < price_range.sell_enter) {
        if (long_size > 0) {
            place_order(contract, quote->bid1, long_size, EA_Sell);
            place_order(contract, quote->bid1, 1, EA_Short);
        }
    }
    //   �յ���ת
    if (bar_1->low < price_range.buy_setup) 
        count_2 = 1;

    if (count_2 == 1 && bar_1->close > price_range.buy_enter) {
        if (short_size > 0) {
            place_order(contract, quote->ask1, short_size, EA_Cover);
            place_order(contract, quote->ask1, 1, EA_Buy);
        }
    }
}


Stralet* create_rbreaker()
{
    return new RBreakerStralet();
}
#ifndef _TQUANT_ALGO_H
#define _TQUANT_ALGO_H

#include "stralet.h"

using namespace std;

namespace tquant { namespace stra {

    class PorfolioManagerAlgoImpl;

    struct NetPosition {
        string  account_id;       // �ʺű��
        string  code;             // ֤ȯ����
        string  name;             // ֤ȯ����
        int64_t current_size;     // ��ǰ�ֲ�
                                  //int64_t enable_size;      // ���ã��ɽ��ף��ֲ�
        int64_t init_size;        // ��ʼ�ֲ�
        double  cost;             // �ɱ�
        double  cost_price;       // �ɱ��۸�
        double  last_price;       // ���¼۸�
        //double  float_pnl;        // �ֲ�ӯ��
        //double  close_pnl;        // ƽ��ӯ��
        //double  margin;           // ��֤��
        //double  commission;       // ������

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

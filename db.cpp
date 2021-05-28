#include <bits/c++config.h>

#include <cstddef>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <mutex>
#include <string_view>
#include <unordered_map>
#include <utility>

#include "GeneratedTypes.h"
#include "HostToPuppetmasterRequest.h"
#include "PuppetToHostIndication.h"
#include "PuppetmasterToHostIndication.h"
#include "extern_cc.h"
#include "row.h"
#include "test.h"
#include "txn.h"

int run();

constexpr std::size_t objSetSize = 8;

typedef std::array<ObjectAddress, objSetSize> InputObjects;

std::mutex g_tr_map_lock;
std::unordered_map<base_query*, std::pair<InputObjects, InputObjects>>
    transactionObjects;

// Helper function to print log messages.
void print_log(std::string_view msg) {
    std::cout << "[        ] db.cpp: " << msg << std::endl;
}

HostToPuppetmasterRequestProxy* fpga;
std::mutex g_fpga_lock;

// Function called from database for scheduling transactions.
void register_txn(txn_man* m_txn, base_query* m_query, row_t* reads[], row_t* writes[],
                  std::size_t num_reads, std::size_t num_writes) {
    InputObjects readObjects;
    InputObjects writtenObjects;

    for (std::size_t i = 0; i < num_reads; i++) {
        readObjects[i] = reinterpret_cast<ObjectAddress>(reads[i]);
    }
    for (std::size_t i = 0; i < num_writes; i++) {
        writtenObjects[i] = reinterpret_cast<ObjectAddress>(writes[i]);
    }

    {
        std::scoped_lock trMapGuard(g_tr_map_lock);
        transactionObjects.insert_or_assign(
            m_query, std::make_pair(readObjects, writtenObjects));
    }

    {
        std::scoped_lock fpgaGuard(g_fpga_lock);
        fpga->enqueueTransaction(
            reinterpret_cast<TransactionId>(m_txn),
            reinterpret_cast<TransactionData>(m_query), num_reads, readObjects[0],
            readObjects[1], readObjects[2], readObjects[3], readObjects[4],
            readObjects[5], readObjects[6], readObjects[7], num_writes,
            writtenObjects[0], writtenObjects[1], writtenObjects[2], writtenObjects[3],
            writtenObjects[4], writtenObjects[5], writtenObjects[6], writtenObjects[7]);
    }
}

// Handler for messages received from the FPGA
class PuppetmasterToHostIndication : public PuppetmasterToHostIndicationWrapper {
private:
    void log_message(TransactionId tid, std::string_view verb) {
        std::ostringstream msg;
        msg << verb << " T#" << std::setw(2 * sizeof(tid)) << std::setfill('0')
            << std::hex << tid;
        print_log(msg.str());
    }

public:
    void transactionRenamed(TransactionId tid) { log_message(tid, "renamed"); }
    void transactionFreed(TransactionId tid) { log_message(tid, "freed"); }
    void transactionFailed(TransactionId tid) { log_message(tid, "failed"); }
    PuppetmasterToHostIndication(int id) : PuppetmasterToHostIndicationWrapper(id) {}
};

// Handler for messages between FPGA and puppets.
class PuppetToHostIndication : public PuppetToHostIndicationWrapper {
public:
    void startTransaction(const PuppetId pid, const TransactionId tid,
                          const TransactionData trData) {
        txn_man* m_txn = reinterpret_cast<txn_man*>(tid);
        base_query* m_query = reinterpret_cast<base_query*>(trData);

        std::pair<InputObjects, InputObjects> objects;
        {
            std::scoped_lock trMapGuard(g_tr_map_lock);
            objects = transactionObjects.at(m_query);
            transactionObjects.erase(m_query);
        }
        row_t* reads = reinterpret_cast<row_t*>(objects.first.data());
        row_t* writes = reinterpret_cast<row_t*>(objects.second.data());

        if (WORKLOAD == TEST) {
            if (g_test_case == READ_WRITE) {
                ((TestTxnMan*)m_txn)->commit_txn(g_test_case, 0, &reads, &writes);
                ((TestTxnMan*)m_txn)->commit_txn(g_test_case, 1, &reads, &writes);
                printf("READ_WRITE TEST PASSED\n");
            } else if (g_test_case == CONFLICT) {
                ((TestTxnMan*)m_txn)->commit_txn(g_test_case, 0, &reads, &writes);
            }
        } else {
            m_txn->commit_txn(m_query, &reads, &writes);
        }
    }
    PuppetToHostIndication(int id) : PuppetToHostIndicationWrapper(id) {}
};

PuppetmasterToHostIndication* pmToHost;
PuppetToHostIndication* puppetsToHost;

int main(int argc, char** argv) {
    print_log("Connectal setting up...");

    fpga = new HostToPuppetmasterRequestProxy(IfcNames_HostToPuppetmasterRequestS2H);
    print_log("Initialized the request interface to the FPGA");

    pmToHost =
        new PuppetmasterToHostIndication(IfcNames_PuppetmasterToHostIndicationH2S);
    puppetsToHost = new PuppetToHostIndication(IfcNames_PuppetToHostIndicationH2S);
    print_log("Initialized the indication interfaces");

    // Set up db.
    g_params["abort_buffer_enable"] = ABORT_BUFFER_ENABLE ? "true" : "false";
    g_params["write_copy_form"] = WRITE_COPY_FORM;
    g_params["validation_lock"] = VALIDATION_LOCK;
    g_params["pre_abort"] = PRE_ABORT;
    g_params["atomic_timestamp"] = ATOMIC_TIMESTAMP;
    if (g_thread_cnt < g_init_parallelism) {
        g_init_parallelism = g_thread_cnt;
    }

    // Call db runner.
    run();
}

#include <algorithm>
#include <chrono>
#include <condition_variable>
#include <cstddef>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <mutex>
#include <sstream>
#include <string_view>
#include <thread>
#include <unordered_map>
#include <utility>

#include "GeneratedTypes.h"
#include "HostToPuppetRequest.h"
#include "HostToPuppetmasterRequest.h"
#include "PuppetToHostIndication.h"
#include "PuppetmasterToHostIndication.h"
#include "extern_cc.h"
#include "global.h"
#include "row.h"
#include "test.h"
#include "txn.h"

#define PM_LOG(verb, tid, rest)                                                        \
    CXX_MSG(verb << " T#" << std::setw(2 * sizeof(TransactionId)) << std::setfill('0') \
                 << std::hex << tid << std::setfill(' ') << std::dec << rest)

// ----------------------------------------------------------------------------
// Forward declarations.

/// Runs configured workload. Defined in DBx1000/system/run.cpp.
int run();

// ----------------------------------------------------------------------------
// Constants and types.

/// Size of transaction read and write sets.
constexpr std::size_t objSetSize = 8;
/// Number of transactions considered by scheduler (1 represents running transactions).
constexpr std::size_t schedulingPoolSize = 8;

/// Wrapper for transaction read and write sets.
typedef std::array<ObjectAddress, objSetSize> InputObjects;

// ----------------------------------------------------------------------------
// Global variables.

/// Maps query pointer to array of associated read and written objects.
std::unordered_map<base_query*, std::pair<InputObjects, InputObjects>>
    transactionObjects;
/// Forwards requests to hardware.
HostToPuppetmasterRequestProxy* fpga;
/// Sends feedback to puppets interface.
HostToPuppetRequestProxy* puppets;
/// Mutex for transactionObjects.
std::mutex g_tr_map_lock;
/// Mutex shared by fpga and puppets.
std::mutex g_hw_request_lock;
/// Condition variable for transactionObjects;
std::condition_variable g_tr_map_cv;

// ----------------------------------------------------------------------------
// Helper function definitions.

/// Pretty-print object arrays.
std::ostream& operator<<(std::ostream& os, const InputObjects& objs) {
    os << "[" << std::hex;
    std::copy(objs.begin(), objs.end(), std::ostream_iterator<std::size_t>(os, ", "));
    os << "]" << std::dec;
    return os;
}

/// Function called from database for scheduling transactions.
void register_txn(txn_man* m_txn, base_query* m_query, row_t* reads[], row_t* writes[],
                  std::size_t num_reads, std::size_t num_writes) {
    InputObjects readObjects{};
    InputObjects writtenObjects{};

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

    TransactionId tid = reinterpret_cast<TransactionId>(m_query);
    TransactionData trData = reinterpret_cast<TransactionData>(m_txn);

    PM_LOG("enqueuing", tid,
           ", " << num_reads << " reads: " << readObjects << ", " << num_writes
                << " writes: " << writtenObjects);

    {
        std::scoped_lock hwGuard(g_hw_request_lock);
        fpga->enqueueTransaction(
            tid, trData, num_reads, readObjects[0], readObjects[1], readObjects[2],
            readObjects[3], readObjects[4], readObjects[5], readObjects[6],
            readObjects[7], num_writes, writtenObjects[0], writtenObjects[1],
            writtenObjects[2], writtenObjects[3], writtenObjects[4], writtenObjects[5],
            writtenObjects[6], writtenObjects[7]);
    }
}

// ----------------------------------------------------------------------------
// Helper class definitions.

/// Handler for messages received from the FPGA.
class PuppetmasterToHostIndication : public PuppetmasterToHostIndicationWrapper {
public:
    void transactionRenamed(TransactionId tid) { PM_LOG("renamed", tid, ""); }
    void transactionFreed(TransactionId tid) { PM_LOG("freed", tid, ""); }
    void transactionFailed(TransactionId tid) {
        PM_LOG("failed", tid, "");

        // Remove failed transaction from global set.
        base_query* m_query = reinterpret_cast<base_query*>(tid);
        {
            std::scoped_lock trMapGuard(g_tr_map_lock);
            transactionObjects.erase(m_query);
        }
    }
    PuppetmasterToHostIndication(int id) : PuppetmasterToHostIndicationWrapper(id) {}
};

/// Handler for messages between FPGA and puppets.
class PuppetToHostIndication : public PuppetToHostIndicationWrapper {
public:
    void startTransaction(const PuppetId pid, const TransactionId tid,
                          const TransactionData trData) {
        base_query* m_query = reinterpret_cast<base_query*>(tid);
        txn_man* m_txn = reinterpret_cast<txn_man*>(trData);

        std::pair<InputObjects, InputObjects> objects;
        {
            std::scoped_lock trMapGuard(g_tr_map_lock);
            objects = transactionObjects.at(m_query);
            transactionObjects.erase(m_query);
        }
        g_tr_map_cv.notify_all();
        row_t** reads = reinterpret_cast<row_t**>(objects.first.data());
        row_t** writes = reinterpret_cast<row_t**>(objects.second.data());

        PM_LOG("started", tid,
               ", reads: " << objects.first << ", writes: " << objects.second);

        if (WORKLOAD == TEST) {
            if (g_test_case == READ_WRITE) {
                ((TestTxnMan*)m_txn)->commit_txn(g_test_case, 0, reads, writes);
                ((TestTxnMan*)m_txn)->commit_txn(g_test_case, 1, reads, writes);
                printf("READ_WRITE TEST PASSED\n");
            } else if (g_test_case == CONFLICT) {
                ((TestTxnMan*)m_txn)->commit_txn(g_test_case, 0, reads, writes);
            }
        } else {
            m_txn->commit_txn(m_query, reads, writes);
        }

        PM_LOG("finishing", tid, " on puppet " << +pid);

        {
            std::scoped_lock hwGuard(g_hw_request_lock);
            puppets->transactionFinished(pid);
        }
    }
    PuppetToHostIndication(int id) : PuppetToHostIndicationWrapper(id) {}
};

// ----------------------------------------------------------------------------
// Global helper class instances.

/// Indication interface for general progress messages.
PuppetmasterToHostIndication* pmToHost;

/// Indication interface for simulating puppets in software.
PuppetToHostIndication* puppetsToHost;

// ----------------------------------------------------------------------------

/// Program entry point. Initializes globals and portals. Runs database.
int main(int argc, char** argv) {
    CXX_MSG("Connectal setting up...");

    fpga = new HostToPuppetmasterRequestProxy(IfcNames_HostToPuppetmasterRequestS2H);
    puppets = new HostToPuppetRequestProxy(IfcNames_HostToPuppetRequestS2H);
    CXX_MSG("Initialized the request interface to the FPGA");

    pmToHost =
        new PuppetmasterToHostIndication(IfcNames_PuppetmasterToHostIndicationH2S);
    puppetsToHost = new PuppetToHostIndication(IfcNames_PuppetToHostIndicationH2S);
    CXX_MSG("Initialized the indication interfaces");

    // Set up db.
    g_params["abort_buffer_enable"] = ABORT_BUFFER_ENABLE ? "true" : "false";
    g_params["write_copy_form"] = WRITE_COPY_FORM;
    g_params["validation_lock"] = VALIDATION_LOCK;
    g_params["pre_abort"] = PRE_ABORT;
    g_params["atomic_timestamp"] = ATOMIC_TIMESTAMP;
    if (g_thread_cnt < g_init_parallelism) {
        g_init_parallelism = g_thread_cnt;
    }

    int64_t startTime = get_server_clock();

    // Call db runner.
    run();

    int64_t runEndTime = get_server_clock();

    // Wait until fewer transactions are left than what the scheduler expects.
    {
        std::unique_lock trMapGuard(g_tr_map_lock);
        g_tr_map_cv.wait(trMapGuard, [] {
            return transactionObjects.size() == schedulingPoolSize - 2;
        });
    }
    // Run remaining transactions.
    {
        std::scoped_lock hwGuard(g_hw_request_lock);
        fpga->clearState();
    }
    // Wait for all of them to finish.
    {
        std::unique_lock trMapGuard(g_tr_map_lock);
        g_tr_map_cv.wait(trMapGuard, [] { return transactionObjects.empty(); });
    }

    int64_t endTime = get_server_clock();
    std::cout << "Commit time: " << endTime - runEndTime << "\n";
    std::cout << "Total time: " << endTime - startTime << "\n";

    // Wait a few more seconds for messages to be sent.
    std::this_thread::sleep_for(chrono::seconds(1));
}

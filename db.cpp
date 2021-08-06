#include <algorithm>
#include <array>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <mutex>
#include <sstream>
#include <string_view>
#include <thread>
#include <unordered_map>
#include <unordered_set>

#include "GeneratedTypes.h"
#include "HostToPuppetRequest.h"
#include "HostToPuppetmasterRequest.h"
#include "PuppetToHostIndication.h"
#include "PuppetmasterToHostIndication.h"
#include "config.h"
#include "extern_cc.h"
#include "global.h"
#include "row.h"
#include "test.h"
#include "txn.h"

#ifdef QUIET
#define PM_LOG(verb, tid, rest)
#else
#define PM_LOG(verb, tid, rest)                                                        \
    CXX_MSG(verb << " T#" << std::setw(2 * sizeof(TransactionId)) << std::setfill('0') \
                 << std::hex << tid << std::setfill(' ') << std::dec << rest)
#endif

// ----------------------------------------------------------------------------
// Forward declarations.

/// Runs configured workload. Defined in DBx1000/system/run.cpp.
int run();

// ----------------------------------------------------------------------------
// Constants and types.

/// Size of transaction read and write sets.
constexpr std::size_t objSetSize = 8;
/// Total size of queues in hardware
constexpr std::size_t hwQueueSize = 16;

/// Wrapper for transaction read and write sets.
typedef std::array<ObjectAddress, objSetSize> InputObjects;

typedef struct {
    txn_man* m_txn;
    std::size_t num_reads;
    std::size_t num_writes;
    InputObjects readObjects;
    InputObjects writtenObjects;
} TransactionProps;

typedef struct {
    TransactionId tid;
    Timestamp cycle;
} ThreadMsg;

// ----------------------------------------------------------------------------
// Global variables.

/// Maps query pointer to array of associated read and written objects.
std::unordered_map<TransactionId, TransactionProps> activeTransactions;
std::mutex g_tr_map_lock;
std::condition_variable g_tr_map_cv;
/// Number of transactions that have been queued but not renamed or failed.
std::atomic<std::size_t> numPendingTransactions;
std::mutex numPendingTransactionsMutex;
std::condition_variable numPendingTransactionsCv;
/// Number of transactions that have been renamed but not freed.
std::atomic<std::size_t> numActiveTransactions;
std::mutex numActiveTransactionsMutex;
std::condition_variable numActiveTransactionsCv;
/// Runner threads (puppets).
std::vector<std::thread> threads;
/// For communicating with runner threads.
std::array<std::deque<ThreadMsg>, THREAD_CNT> threadQueues{};
std::array<std::mutex, THREAD_CNT> threadQueueMutexes{};
std::array<std::condition_variable, THREAD_CNT> threadQueueCvs{};
bool programDone = false;
/// Portals to hardware.
HostToPuppetmasterRequestProxy* fpga;
HostToPuppetRequestProxy* puppets;
std::mutex g_hw_request_lock;

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

    TransactionId tid = reinterpret_cast<TransactionId>(m_query);
    TransactionData trData = reinterpret_cast<TransactionData>(m_txn);

    {
        std::unique_lock numPendingTransactionsLock(numPendingTransactionsMutex);
        numPendingTransactionsCv.wait(numPendingTransactionsLock, [] {
            return numPendingTransactions < hwQueueSize;
        });
        numPendingTransactions++;
    }
    numPendingTransactionsCv.notify_all();

    {
        std::unique_lock trMapGuard(g_tr_map_lock);
        activeTransactions[tid] =
            TransactionProps{m_txn, num_reads, num_writes, readObjects, writtenObjects};
    }

    PM_LOG("enqueuing", tid,
#ifdef DEBUG
           ", " << num_reads << " reads: " << readObjects << ", " << num_writes
                << " writes: " << writtenObjects
#else
           ""
#endif
    );

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

void puppetThread(PuppetId pid) {
    while (!programDone) {
        ThreadMsg msg;
        {
            std::unique_lock threadQueueLock(threadQueueMutexes[pid]);
            threadQueueCvs[pid].wait(threadQueueLock, [&pid] {
                return !threadQueues[pid].empty() || programDone;
            });
            if (threadQueues[pid].empty()) {
                continue;
            }
            msg = threadQueues[pid].front();
            threadQueues[pid].pop_front();
        }

        TransactionProps tp;
        {
            std::scoped_lock trMapGuard(g_tr_map_lock);
            tp = activeTransactions.at(msg.tid);
            activeTransactions.erase(msg.tid);
        }
        g_tr_map_cv.notify_all();

        base_query* m_query = reinterpret_cast<base_query*>(msg.tid);
        row_t** reads = reinterpret_cast<row_t**>(tp.readObjects.data());
        row_t** writes = reinterpret_cast<row_t**>(tp.writtenObjects.data());

        PM_LOG("started", msg.tid,
               " at " << msg.cycle
#ifdef DEBUG
                      << ", " << num_reads << " reads: " << readObjects << ", "
                      << num_writes << " writes: " << writtenObjects
#endif
        );

        if (WORKLOAD == TEST) {
            if (g_test_case == READ_WRITE) {
                ((TestTxnMan*)tp.m_txn)->commit_txn(g_test_case, 0, reads, writes);
                ((TestTxnMan*)tp.m_txn)->commit_txn(g_test_case, 1, reads, writes);
                printf("READ_WRITE TEST PASSED\n");
            } else if (g_test_case == CONFLICT) {
                ((TestTxnMan*)tp.m_txn)->commit_txn(g_test_case, 0, reads, writes);
            }
        } else {
            tp.m_txn->commit_txn(m_query, reads, writes);
        }

        PM_LOG("finishing", msg.tid, " on puppet " << +pid);

        {
            std::scoped_lock hwGuard(g_hw_request_lock);
            puppets->transactionFinished(pid);
        }
    }
}

// ----------------------------------------------------------------------------
// Helper class definitions.

/// Handler for messages received from the FPGA.
class PuppetmasterToHostIndication : public PuppetmasterToHostIndicationWrapper {
public:
    void transactionRenamed(Message m) {
        numPendingTransactions--;
        numPendingTransactionsCv.notify_all();
        numActiveTransactions++;
        numActiveTransactionsCv.notify_all();
        PM_LOG("renamed", m.tid,
               " at " << m.endTime << " in " << m.endTime - m.startTime << " cycles");
    }

    void transactionFailed(Message m) {
        numPendingTransactions--;
        numPendingTransactionsCv.notify_all();
        PM_LOG("failed", m.tid,
               " at " << m.endTime << " in " << m.endTime - m.startTime << " cycles");

        // Remove failed transaction from global set.
        {
            std::scoped_lock trMapGuard(g_tr_map_lock);
            activeTransactions.erase(m.tid);
        }
        g_tr_map_cv.notify_all();
    }

    void transactionFreed(Message m) {
        numActiveTransactions--;
        numActiveTransactionsCv.notify_all();
        PM_LOG("freed", m.tid,
               " at " << m.endTime << " in " << m.endTime - m.startTime << " cycles");
    }

    PuppetmasterToHostIndication(int id) : PuppetmasterToHostIndicationWrapper(id) {}
};

/// Handler for messages between FPGA and puppets.
class PuppetToHostIndication : public PuppetToHostIndicationWrapper {
public:
    void startTransaction(const PuppetId pid, const TransactionId tid,
                          const TransactionData trData, Timestamp cycle) {
        {
            std::scoped_lock threadQueueLock(threadQueueMutexes[pid]);
            threadQueues[pid].push_back({tid, cycle});
        }
        threadQueueCvs[pid].notify_all();
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

    // Set up runner threads.
    for (std::size_t i = 0; i < THREAD_CNT; i++) {
        threads.push_back(std::thread{puppetThread, i});
    }

    int64_t startTime = get_server_clock();

    // Call db runner.
    run();

    int64_t runEndTime = get_server_clock();

    // Wait until all transactions have been renamed or have failed.
    {
        std::unique_lock numPendingTransactionsLock(numPendingTransactionsMutex);
        numPendingTransactionsCv.wait(numPendingTransactionsLock,
                                      [] { return numPendingTransactions == 0; });
    }
    // Run remaining transactions.
    {
        std::scoped_lock hwGuard(g_hw_request_lock);
        fpga->clearState();
    }
    // Wait for all transactions to be freed.
    {
        std::unique_lock numActiveTransactionsLock(numActiveTransactionsMutex);
        numActiveTransactionsCv.wait(numActiveTransactionsLock,
                                     [] { return numActiveTransactions == 0; });
    }

    int64_t endTime = get_server_clock();
    CXX_MSG("Commit time: " << endTime - runEndTime);
    CXX_MSG("Total time: " << endTime - startTime);

    // Terminate runner threads.
    programDone = true;
    for (std::size_t i = 0; i < THREAD_CNT; i++) {
        threadQueueCvs[i].notify_all();
        threads[i].join();
    }
    threads.clear();
}

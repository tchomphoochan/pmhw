#include <semaphore.h>

#include <array>
#include <cerrno>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

#include "GeneratedTypes.h"
#include "HostSetupRequest.h"
#include "HostTxnRequest.h"
#include "DebugIndication.h"
#include "WorkIndication.h"
#include "HostWorkDone.h"

constexpr std::size_t objSetSize = 8;
constexpr std::size_t poolSize = 8;

typedef std::array<ObjectAddress, objSetSize> InputObjects;

typedef struct {
    TransactionId tid;
    TransactionType trType;
    InputObjects readObjects;
    InputObjects writtenObjects;
    TransactionObjectCounter readObjectCount;
    TransactionObjectCounter writtenObjectCount;
} InputTransaction;

static sem_t sem_all_renamed;
static sem_t sem_all_freed;

// Helper function equivalent to C++20 std::unordered_set::contains.
template <typename type>
bool set_contains(std::unordered_set<type>& set, type& key) {
    return set.find(key) != set.end();
}

// Helper function to print log messages.
void print_log(std::string_view msg) {
    std::cout << "[        ] main.cpp: " << msg << std::endl;
}

// Handler for messages received from the FPGA
class DebugIndication : public DebugIndicationWrapper {
private:
    const int numTansactions;
    int numRenamed = 0;
    int numFreed = 0;

    void log_message(TransactionId tid, std::string_view verb) {
        std::ostringstream msg;
        msg << verb << " T#" << std::setw(2 * sizeof(tid)) << std::setfill('0')
            << std::hex << tid;
        print_log(msg.str());
    }

public:
    void getPmConfig(PmConfigValues m) {
    }

    void transactionRenamed(DebugMessage m) {
        log_message(m.tid, "renamed");
        if (++numRenamed == numTansactions) {
            numRenamed = 0;
            sem_post(&sem_all_renamed);
        }
    }

    void transactionFreed(DebugMessage m) {
        log_message(m.tid, "freed");
        if (++numFreed == numTansactions) {
            numFreed = 0;
            sem_post(&sem_all_freed);
        }
    }

    void transactionFailed(DebugMessage m) {
        log_message(m.tid, "failed");
        // Failed transactions skip both the renaming and the freeing step.
        if (++numRenamed == numTansactions) {
            numRenamed = 0;
            sem_post(&sem_all_renamed);
        }
        if (++numFreed == numTansactions) {
            numFreed = 0;
            sem_post(&sem_all_freed);
        }
    }

    DebugIndication(int id, int totalTransactions)
        : DebugIndicationWrapper(id), numTansactions(totalTransactions) {}
};

void load_default_test(std::vector<InputTransaction>& testInputs) {
    int numE2ETestRounds = 5;
    int numE2ETests = numE2ETestRounds * poolSize;

    for (int i = 0; i < numE2ETests; i = i + 1) {
        auto iDiv = std::div(i, numE2ETestRounds);
        auto round = iDiv.quot;
        auto offset = iDiv.rem;
        testInputs.emplace_back();
        testInputs[i].tid = i;
        testInputs[i].trType = offset <= 1 ? TransactionType::DatabaseRead
                               : offset <= 4 ? TransactionType::DatabaseWrite
                               : TransactionType::DatabaseTransfer;
        testInputs[i].readObjectCount = objSetSize;
        testInputs[i].writtenObjectCount = objSetSize;
        for (std::size_t j = 0; j < objSetSize; j = j + 1) {
            testInputs[i].readObjects[j] = objSetSize * i * 2 + j * 2;
            testInputs[i].writtenObjects[j] =
                round == 0   ? (objSetSize * i * 2 + j * 2 + 1)
                : round == 1 ? (objSetSize * (i - i % 2) * 2 + j * 2 + 1)
                : round == 2 ? (objSetSize * (i % 2) * 2 + j * 2 + 1)
                             : (objSetSize * 2 + j * 2 + 1);
        }
        for (std::size_t j = 0; j < objSetSize; j = j + 1) {
            testInputs[i].readObjects[j] <<= 6;
            testInputs[i].writtenObjects[j] <<= 6;
	}
    }
}

std::size_t load_test_from_file(std::vector<InputTransaction>& testInputs,
                                std::size_t startIndex, const char* fname) {
    // Open input file.
    std::ifstream source;
    source.open(fname);
    if (!source.is_open()) {
        throw std::runtime_error("file doesn't exist: " + std::string(fname));
    }

    std::intmax_t typeIndex = -1;
    std::unordered_set<std::size_t> objIndices;
    std::unordered_set<std::size_t> readIndices;
    std::unordered_set<std::size_t> writeIndices;
    std::size_t indexCount;

    // Parse header for location of read and write object fields.
    std::string header;
    if (!std::getline(source, header)) {
        throw std::runtime_error("no header in file: " + std::string(fname));
    }
    std::stringstream headerBuffer(header);
    std::string label;
    for (indexCount = 0; std::getline(headerBuffer, label, ','); indexCount++) {
        if (label.find("Read object") == 0) {
            objIndices.insert(indexCount);
            readIndices.insert(indexCount);
        } else if (label.find("Written object") == 0) {
            objIndices.insert(indexCount);
            writeIndices.insert(indexCount);
        } else if (label.find("Type") == 0) {
            typeIndex = indexCount;
        }
    }
    if (typeIndex < 0) {
        throw std::runtime_error("no type column in file: " + std::string(fname));
    }

    // Parse content lines.
    std::string line;
    while (std::getline(source, line)) {
        InputTransaction tr;
        tr.tid = startIndex++;
        tr.readObjectCount = 0;
        tr.writtenObjectCount = 0;

        // Parse each comma-separated value in line.
        std::stringstream lineBuffer(line);
        std::string value;
        for (std::size_t i = 0; i < indexCount; i++) {
            std::getline(lineBuffer, value, ',');
            if (i == static_cast<std::size_t>(typeIndex)) {
                tr.trType = value == "get"         ? TransactionType::DatabaseRead
                            : value == "set"       ? TransactionType::DatabaseWrite
                            : value == "transfer" ? TransactionType::DatabaseTransfer
                            : value == "fetch"     ? TransactionType::MessageFetch
                            : value == "post"
                                ? TransactionType::MessagePost
                                : throw std::runtime_error("unknown type: " + value);
            } else if (value.length() != 0 && set_contains(objIndices, i)) {
                ObjectAddress address;
                address = std::stoul(value);
                if (set_contains(readIndices, i) && tr.readObjectCount < objSetSize) {
                    tr.readObjects[tr.readObjectCount++] = address;
                } else if (set_contains(writeIndices, i) &&
                           tr.writtenObjectCount < objSetSize) {
                    tr.writtenObjects[tr.writtenObjectCount++] = address;
                }
            }
        }
        testInputs.push_back(tr);
    }
    source.close();
    return startIndex;
}

int main(int argc, char** argv) {
    std::vector<ClockPeriod> periods;
    std::vector<InputTransaction> testInputs;
    std::size_t testIndex = 0;

    // Process command line arguments.
    typedef enum { FLAG_NONE, FLAG_FILE, FLAG_MULTIPLIER } Flag;
    Flag flag = FLAG_NONE;
    for (int i = 1; i < argc; i++) {
        std::string arg(argv[i]);

        // Switch modes if needed.
        if (arg == "-f") {
            flag = FLAG_FILE;
            continue;
        } else if (arg == "-x") {
            flag = FLAG_MULTIPLIER;
            continue;
        }

        // Digest next argument.
        switch (flag) {
        case FLAG_NONE:
            std::cerr << "usage: PROG -f [file1 [file2 ...]] -x [multiplier1 "
                         "[multiplier2 ...]]\n";
            throw std::runtime_error("unknown argument: " + arg);
        case FLAG_FILE:
            print_log("Loading tests from: " + std::string(argv[i]));
            testIndex = load_test_from_file(testInputs, testIndex, argv[i]);
            break;
        case FLAG_MULTIPLIER:
            periods.push_back(std::stoul(arg));
            break;
        }
    }

    // Construct default test if no tests given.
    if (testInputs.size() == 0) {
        print_log("Loading default tests...");
        load_default_test(testInputs);
    }

    // Add a default clock period if none given.
    if (periods.size() == 0) {
        periods.push_back(20);
    }

    // Run tests.
    print_log("Connectal setting up...");

    HostSetupRequestProxy* setup =
        new HostSetupRequestProxy(IfcNames_HostSetupRequestS2H);
    HostTxnRequestProxy* txn =
        new HostTxnRequestProxy(IfcNames_HostTxnRequestS2H);
    print_log("Initialized the request interface to the FPGA");

    DebugIndication puppetmasterToHost(
        IfcNames_DebugIndicationH2S, testInputs.size());
    print_log("Initialized the indication interface");

    for (auto&& period : periods) {
        std::ostringstream msg;
        msg << "Enqueuing " << testInputs.size() << " transactions with clock period "
            << period;
        print_log(msg.str());
        setup->setSimulatedPuppets(true, period);
        setup->setTxnDriver(true);
        for (auto&& tr : testInputs) {
            txn->enqueueTransaction(
                tr.tid, tr.trType, tr.readObjectCount, tr.readObjects[0],
                tr.readObjects[1], tr.readObjects[2], tr.readObjects[3],
                tr.readObjects[4], tr.readObjects[5], tr.readObjects[6],
                tr.readObjects[7], tr.writtenObjectCount, tr.writtenObjects[0],
                tr.writtenObjects[1], tr.writtenObjects[2], tr.writtenObjects[3],
                tr.writtenObjects[4], tr.writtenObjects[5], tr.writtenObjects[6],
                tr.writtenObjects[7]);
        }
        sleep(2);
        setup->startFakeTxnDriver();
        sem_wait(&sem_all_renamed);
        print_log("Waiting for termination...");
        txn->clearState();
        sem_wait(&sem_all_freed);
        setup->stopFakeTxnDriver();
    }
}

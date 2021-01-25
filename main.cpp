#include <semaphore.h>

#include <array>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

#include "GeneratedTypes.h"
#include "HostToPuppetmaster.h"
#include "PuppetmasterToHostIndication.h"

constexpr std::size_t objSetSize = 8;
constexpr std::size_t tsWidth = 6;

typedef std::array<ObjectAddress, objSetSize> InputObjects;

typedef struct {
    TransactionId tid;
    TransactionType trType;
    InputObjects readObjects;
    InputObjects writtenObjects;
    TransactionObjectCounter readObjectCount;
    TransactionObjectCounter writtenObjectCount;
} InputTransaction;

static sem_t sem_cleared;

// Helper function equivalent to C++20 std::unordered_set::contains.
template <typename type>
bool set_contains(std::unordered_set<type>& set, type& key) {
    return set.find(key) != set.end();
}

// Helper function to print log messages.
void print_log(std::string_view msg) {
    std::cout << "[" << std::setw(tsWidth + 1) << std::setfill('-') << "]"
              << " main.cpp: " << msg << std::endl;
}

// Handler for messages received from the FPGA
class PuppetmasterToHostIndication : public PuppetmasterToHostIndicationWrapper {
private:
    void log_message(std::optional<TransactionId> tid, Timestamp timestamp,
                     std::string_view msg) {
        std::cout << "[" << std::setw(tsWidth) << std::setfill(' ') << std::dec
                  << timestamp << "] PmTop: " << msg;
        if (tid.has_value()) {
            std::cout << " " << std::setw(4) << std::setfill('0') << std::hex
                      << tid.value();
        }
        std::cout << std::endl;
    }

public:
    void transactionReceived(TransactionId tid, Timestamp timestamp) {
        log_message(tid, timestamp, "received");
    }

    void transactionRenamed(TransactionId tid, Timestamp timestamp) {
        log_message(tid, timestamp, "renamed");
    }

    void transactionStarted(TransactionId tid, Timestamp timestamp) {
        log_message(tid, timestamp, "started");
    }

    void transactionFinished(TransactionId tid, Timestamp timestamp) {
        log_message(tid, timestamp, "finished");
    }

    void stateCleared(Timestamp timestamp) {
        log_message(std::nullopt, timestamp, "state cleared");
        sem_post(&sem_cleared);
    }

    PuppetmasterToHostIndication(int id) : PuppetmasterToHostIndicationWrapper(id) {}
};

void load_default_test(std::vector<InputTransaction>& testInputs) {
    int numE2ETestRounds = 4;
    int poolSize = 8;
    int numE2ETests = numE2ETestRounds * poolSize;

    for (int i = 0; i < numE2ETests; i = i + 1) {
        auto iDiv = std::div(i, numE2ETestRounds);
        auto round = iDiv.quot;
        auto offset = iDiv.rem;
        testInputs.emplace_back();
        testInputs[i].tid = i;
        testInputs[i].trType = offset == 0   ? TransactionType::DatabaseRead
                               : offset == 1 ? TransactionType::DatabaseWrite
                               : offset == 2 ? TransactionType::DatabaseIncrement
                                             : TransactionType::DatabaseSwap;
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
    }
}

void load_test_from_file(std::vector<InputTransaction>& testInputs,
                         std::size_t& testIndex, const char* fname) {
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

    // Parse header for location of read and write object fields.
    std::string header;
    if (!std::getline(source, header)) {
        throw std::runtime_error("no header in file: " + std::string(fname));
    }
    std::stringstream headerBuffer(header);
    std::string label;
    for (std::size_t i = 0; std::getline(headerBuffer, label, ','); i++) {
        if (label.find("Read object") == 0) {
            objIndices.insert(i);
            readIndices.insert(i);
        } else if (label.find("Written object") == 0) {
            objIndices.insert(i);
            writeIndices.insert(i);
        } else if (label.find("Type") == 0) {
            typeIndex = i;
        }
    }
    if (typeIndex < 0) {
        throw std::runtime_error("no type column in file: " + std::string(fname));
    }

    // Parse content lines.
    std::string line;
    while (std::getline(source, line)) {
        InputTransaction tr;
        tr.tid = testIndex++;

        // Parse each comma-separated value in line.
        std::stringstream lineBuffer(line);
        std::string value;
        for (std::size_t i = 0; std::getline(lineBuffer, value, ','); i++) {
            if (i == static_cast<std::size_t>(typeIndex)) {
                tr.trType = value == "get"         ? TransactionType::DatabaseRead
                            : value == "set"       ? TransactionType::DatabaseWrite
                            : value == "increment" ? TransactionType::DatabaseIncrement
                            : value == "swap"      ? TransactionType::DatabaseSwap
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
}

int main(int argc, char** argv) {
    print_log("Connectal setting up...");

    HostToPuppetmasterProxy* fpga =
        new HostToPuppetmasterProxy(IfcNames_HostToPuppetmasterS2H);
    print_log("Initialized the request interface to the FPGA");

    PuppetmasterToHostIndication puppetmasterToHost(
        IfcNames_PuppetmasterToHostIndicationH2S);
    print_log("Initialized the indication interface");

    std::vector<ClockMultiplier> multipliers;
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
        } else if (arg == "-m") {
            flag = FLAG_MULTIPLIER;
            continue;
        }

        // Digest next argument.
        switch (flag) {
        case FLAG_NONE:
            std::cerr << "usage: PROG -f [file1 [file2 ...]] -m [mult1 [mult2 ...]]\n";
            throw std::runtime_error("unknown argument: " + arg);
        case FLAG_FILE:
            print_log("Loading tests from: " + std::string(argv[i]));
            load_test_from_file(testInputs, testIndex, argv[i]);
            break;
        case FLAG_MULTIPLIER:
            multipliers.push_back(std::stoul(arg));
            break;
        }
    }

    // Construct default test if no tests given.
    if (testInputs.size() == 0) {
        print_log("Loading default tests...");
        load_default_test(testInputs);
    }

    // Add a default multiplier if no multipliers given.
    if (multipliers.size() == 0) {
        multipliers.push_back(2000);
    }

    // Run tests.
    for (auto&& multiplier : multipliers) {
        std::ostringstream msg;
        msg << "Enqueuing transactions with multiplier " << multiplier;
        print_log(msg.str());
        fpga->setPuppetClockMultiplier(multiplier);
        for (auto&& tr : testInputs) {
            fpga->enqueueTransaction(
                tr.tid, tr.trType, tr.readObjectCount, tr.readObjects[0],
                tr.readObjects[1], tr.readObjects[2], tr.readObjects[3],
                tr.readObjects[4], tr.readObjects[5], tr.readObjects[6],
                tr.readObjects[7], tr.writtenObjectCount, tr.writtenObjects[0],
                tr.writtenObjects[1], tr.writtenObjects[2], tr.writtenObjects[3],
                tr.writtenObjects[4], tr.writtenObjects[5], tr.writtenObjects[6],
                tr.writtenObjects[7]);
        }
        fpga->clearState();
        sem_wait(&sem_cleared);
    }
}

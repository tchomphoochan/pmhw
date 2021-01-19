#include <array>
#include <cstddef>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_set>
#include <vector>

#include "GeneratedTypes.h"
#include "HostToPuppetmaster.h"
#include "PuppetmasterToHostIndication.h"

constexpr std::size_t objSetSize = 8;

typedef std::array<ObjectAddress, objSetSize> InputObjects;

typedef struct {
    TransactionId tid;
    InputObjects readObjects;
    InputObjects writtenObjects;
    TransactionObjectCounter readObjectCount;
    TransactionObjectCounter writtenObjectCount;
} InputTransaction;

// Helper function equivalent to C++20 std::unordered_set::contains.
template <typename type>
bool set_contains(std::unordered_set<type> &set, type &key) {
    return set.find(key) != set.end();
}

// Handler for messages received from the FPGA
class PuppetmasterToHostIndication : public PuppetmasterToHostIndicationWrapper {
public:
    void transactionStarted(std::uint64_t tid, std::uint64_t timestamp) {
        std::cout << "Started transaction " << tid << " at " << timestamp << std::endl;
    }

    void transactionFinished(std::uint64_t tid, std::uint64_t timestamp) {
        std::cout << "Finished transaction " << tid << " at " << timestamp << std::endl;
    }

    PuppetmasterToHostIndication(unsigned int id)
        : PuppetmasterToHostIndicationWrapper(id) {}
};

int main(int argc, char **argv) {
    std::cout << "Connectal setting up..." << std::endl;

    HostToPuppetmasterProxy *fpga =
        new HostToPuppetmasterProxy(IfcNames_HostToPuppetmasterS2H);
    std::cout << "Initialized the request interface to the FPGA" << std::endl;

    PuppetmasterToHostIndication puppetmasterToHost(
        IfcNames_PuppetmasterToHostIndicationH2S);
    std::cout << "Initialized the indication interface" << std::endl;

    std::vector<InputTransaction> testInputs;

    if (argc <= 1) {
        // No test files given, construct default test.
        std::cout << "Loading default tests..." << std::endl;

        unsigned numTests = 4;
        unsigned transactionsPerRound = 8;

        for (unsigned i = 0; i < numTests * transactionsPerRound; i++) {
            InputTransaction tr;
            tr.tid = i;
            tr.readObjectCount = objSetSize;
            tr.writtenObjectCount = objSetSize;
            for (unsigned j = 0; j < objSetSize; j++) {
                tr.readObjects[j] = objSetSize * i * 2 + j * 2;
                tr.writtenObjects[j] =
                    i % 4 == 0   ? objSetSize * i * 2 + j * 2 + 1
                    : i % 4 == 1 ? objSetSize * (i - i % 2) * 2 + j * 2 + 1
                    : i % 4 == 2 ? objSetSize * (i % 2) * 2 + j * 2 + 1
                                 : objSetSize * 2 + j * 2 + 1;
            }
            testInputs.push_back(tr);
        }
    } else {
        // Load each input file given into tests.
        std::size_t testIndex = 0;
        for (int i = 1; i < argc; i++) {
            std::cout << "Loading tests from: " << argv[i] << std::endl;

            // Open input file.
            std::ifstream source;
            source.open(argv[i]);
            if (!source.is_open()) {
                std::cerr << "File doesn't exist." << std::endl;
                return 1;
            }

            // Parse header for location of read and write object fields.
            std::string header;
            if (!std::getline(source, header)) {
                std::cerr << "No header found in file." << std::endl;
                return 2;
            }
            std::unordered_set<std::size_t> readIndices;
            std::unordered_set<std::size_t> writeIndices;
            std::stringstream headerBuffer(header);
            std::string label;
            for (std::size_t i = 0; std::getline(headerBuffer, label, ','); i++) {
                if (label.find("Read object") == 0) {
                    readIndices.insert(i);
                } else if (label.find("Written object") == 0) {
                    writeIndices.insert(i);
                }
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
                    if (value.length() != 0 && (set_contains(readIndices, i) ||
                                                set_contains(writeIndices, i))) {
                        ObjectAddress address;
                        try {
                            address = std::stoul(value);
                        } catch (const std::invalid_argument &) {
                            std::cerr << "Not an address: \"" << value << "\""
                                      << std::endl;
                            return 3;
                        } catch (const std::out_of_range &) {
                            std::cerr << "Out of range: " << value << std::endl;
                            return 4;
                        }
                        if (set_contains(readIndices, i) &&
                            tr.readObjectCount < objSetSize) {
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
    }

    // Run tests.
    std::cout << "Enqueuing transactions..." << std::endl;
    for (auto &&tr : testInputs) {
        fpga->enqueueTransaction(
            tr.tid, tr.readObjectCount, tr.readObjects[0], tr.readObjects[1],
            tr.readObjects[2], tr.readObjects[3], tr.readObjects[4], tr.readObjects[5],
            tr.readObjects[6], tr.readObjects[7], tr.writtenObjectCount,
            tr.writtenObjects[0], tr.writtenObjects[1], tr.writtenObjects[2],
            tr.writtenObjects[3], tr.writtenObjects[4], tr.writtenObjects[5],
            tr.writtenObjects[6], tr.writtenObjects[7]);
    }

    std::cout << "Waiting for results..." << std::endl;
    while (true) {
        // Wait for simulation.
    }
}

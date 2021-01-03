#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <map>
#include <numeric>
#include <vector>

#include "HostToPuppetmaster.h"
#include "PuppetmasterToHostIndication.h"

extern "C" {
void connectal_setup();
void enqueue_requests(unsigned int points);
void startTime(unsigned int points);
void endTime(unsigned int points);
}

// Handler for messages received from the FPGA
class PuppetmasterToHostIndication
    : public PuppetmasterToHostIndicationWrapper {
private:
    std::map<std::int64_t, std::uint64_t> startTime;
    std::map<std::uint64_t, std::uint64_t> endTime;

public:
    void transactionStarted(std::uint64_t tid, std::uint64_t timestamp) {
        printf("Started %lx at %lu\n", tid, timestamp);
        fflush(stdout);
        startTime[tid] = timestamp;
    }

    void transactionFinished(std::uint64_t tid, std::uint64_t timestamp) {
        printf("Finished %lx at %lu\n", tid, timestamp);
        fflush(stdout);
        endTime[tid] = timestamp;
    }

    PuppetmasterToHostIndication(unsigned int id)
        : PuppetmasterToHostIndicationWrapper(id), startTime(), endTime() {}
};

// Global handle for both communication directions
HostToPuppetmasterProxy *fpga;
PuppetmasterToHostIndication *puppetmasterToHost;

extern "C" void connectal_setup() {
    printf("Connectal setting up ...\n");
    fflush(stdout);

    fpga = new HostToPuppetmasterProxy(IfcNames_HostToPuppetmasterS2H);
    printf("Initialize the request interface to the FPGA\n");
    fflush(stdout);

    puppetmasterToHost = new PuppetmasterToHostIndication(
        IfcNames_PuppetmasterToHostIndicationH2S);
    printf("Initialize the indication interface\n");
    fflush(stdout);
}

int main() {
    connectal_setup();

    unsigned numTests = 4;
    unsigned maxScheduledObjects = 8;
    unsigned objSetSize = 8;

    for (unsigned i = 0; i < numTests * maxScheduledObjects; i++) {
        std::vector<Object> objs(2 * objSetSize);
        for (unsigned j = 0; j < objSetSize; j++) {
            objs[2 * j] = (Object){
                .valid = 1,
                .write = 0,
                .object = objSetSize * i * 2 + j * 2,
            };
            objs[2 * j + 1] = (Object){
                .valid = 1,
                .write = 1,
                .object = i % 4 == 0 ? objSetSize * i * 2 + j * 2 + 1
                          : i % 4 == 1
                              ? objSetSize * (i - i % 2) * 2 + j * 2 + 1
                          : i % 4 == 2 ? objSetSize * (i % 2) * 2 + j * 2 + 1
                                       : objSetSize * 2 + j * 2 + 1,
            };
        }

        fpga->enqueueTransaction(i, objs[0], objs[1], objs[2], objs[3],
                                 objs[4], objs[5], objs[6], objs[7], objs[8],
                                 objs[9], objs[10], objs[11], objs[12],
                                 objs[13], objs[14], objs[15]);
    }
    while(1);
    return 0;
}

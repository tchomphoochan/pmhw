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

    std::vector<unsigned> ids(64);
    std::iota(ids.begin(), ids.end(), 0);

    for (auto id : ids) {
        fpga->enqueueTransaction(
            id, (Object){1, 0, 123}, (Object){1, 0, 45}, (Object){1, 0, 12},
            (Object){1, 0, 8}, (Object){1, 0, 9}, (Object){1, 0, 345},
            (Object){1, 0, 643}, (Object){1, 0, 6445}, (Object){1, 1, 3},
            (Object){1, 1, 34}, (Object){1, 1, 974}, (Object){1, 1, 76},
            (Object){1, 1, 653}, (Object){1, 1, 445}, (Object){1, 1, 43},
            (Object){1, 1, 4});
    }
    return 0;
}

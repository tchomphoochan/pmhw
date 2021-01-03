#include <cstdint>
#include <cstdio>
#include <vector>

#include "HostToPuppetmaster.h"
#include "PuppetmasterToHostIndication.h"

// Handler for messages received from the FPGA
class PuppetmasterToHostIndication
    : public PuppetmasterToHostIndicationWrapper {
public:
    void transactionStarted(std::uint64_t tid, std::uint64_t timestamp) {
        printf("Started %02lx at %lu\n", tid, timestamp);
        fflush(stdout);
    }

    void transactionFinished(std::uint64_t tid, std::uint64_t timestamp) {
        printf("Finished %02lx at %lu\n", tid, timestamp);
        fflush(stdout);
    }

    PuppetmasterToHostIndication(unsigned int id)
        : PuppetmasterToHostIndicationWrapper(id) {}
};

int main(int argc, char **argv) {
    printf("Connectal setting up ...\n");
    fflush(stdout);

    HostToPuppetmasterProxy *fpga =
        new HostToPuppetmasterProxy(IfcNames_HostToPuppetmasterS2H);
    printf("Initialized the request interface to the FPGA\n");
    fflush(stdout);

    PuppetmasterToHostIndication puppetmasterToHost(
        IfcNames_PuppetmasterToHostIndicationH2S);
    printf("Initialized the indication interface\n");
    fflush(stdout);

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
    while (true) {
        // Wait for simulation.
    }
    return 0;
}

#include <map>
#include <algorithm>
#include "HostToPuppetmaster.h"
#include "PuppetmasterToHost.h"

using namedspace std;

extern "C" {
        void connectal_setup();
        void enqueue_requests(unsigned int points);
        void startTime(unsigned int points);
        void endTime(unsigned int points);
}

HostToPuppetmasterProxy *fpga;
PuppetmasterToHost *puppetmasterToHost;

extern "C" void connectal_setup() {
        printf("Connectal setting up ...\n");
        fflush(stdout);
        
        fpga = new HostToPuppetmasterProxy(IfcNames_HostToPuppetmasterProxyS2H);
        printf("Init the request to FPGA\n");
        fflush(stdout);
        
        puppetmasterToHost = new PuppetmasterToHost(IfcNames_PuppetmasterToHostH2S);
        printf("Init the indication\n");
        fflush(stdout);
}

class PuppetmasterToHost : public PuppetmasterToHostWrapper
{
        private:
        map<int64_t,uint64_t> startTime; 
        map<uint64_t,uint64_t> endTime; 

        public:
        void transactionStarted(uint64_t tid, uint64_t timestamp) {
                startTime[tid] = timestamp;
        }
        
        void transactionFinished(uint64_t tid, uint64_t timestamp) {
                endTime[tid] = timestamp;
        }
        
        PuppetmasterToHost(unsigned int id) : PuppetmasterToHostWrapper(id), startTime(), endTime() {
        }
};


#include <map>
#include <algorithm>
#include "HostToPuppetmaster.h"
#include "PuppetmasterToHostIndication.h"

using namespace std;

extern "C" {
        void connectal_setup();
        void enqueue_requests(unsigned int points);
        void startTime(unsigned int points);
        void endTime(unsigned int points);
}

// Handler for messages received from the FPGA
class PuppetmasterToHostIndication : public PuppetmasterToHostIndicationWrapper
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
        
        PuppetmasterToHostIndication(unsigned int id) : PuppetmasterToHostIndicationWrapper(id), startTime(), endTime() {
        }
};

// Global handle for both communication directions 
HostToPuppetmasterProxy *fpga;
PuppetmasterToHostIndication *puppetmasterToHost;


extern "C" void connectal_setup() {
        printf("Connectal setting up ...\n");
        fflush(stdout);
        
        fpga = new HostToPuppetmasterProxy(IfcNames_HostToPuppetmasterS2H);
        printf("Init the request to FPGA\n");
        fflush(stdout);
        
        puppetmasterToHost = new PuppetmasterToHostIndication(IfcNames_PuppetmasterToHostIndicationH2S);
        printf("Init the indication\n");
        fflush(stdout);
}


int main () {
	return 0;
}

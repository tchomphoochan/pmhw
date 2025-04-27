.PHONY: clean
clean:
	rm -f bsv/*.cxx bsv/*.h bsv/*.o bsv/*.so bsv/*.bo bsv/*.ba bsv/*.sched bsv/a.out bsv/*.exe

MY_BSC_FLAGS=+RTS -Ksize -RTS \
  -aggressive-conditions -show-schedule \
  -suppress-warnings G0020:G0023:G0024:S0073:S0080 \
  -p +:%/Libraries/FPGA/Xilinx:bsv \
  -vdir ./bsv \
  -bdir ./bsv \
  -simdir ./bsv \
  -info-dir ./bsv \

.PHONY: test
test:
	make FILE=Scheduler.bsv MODULE=mkSchedulerTestbench _test
	make FILE=Renamer.bsv MODULE=mkRenamerTestbench _test
	make FILE=Shard.bsv MODULE=mkShardTestbench _test
	make FILE=Puppetmaster.bsv MODULE=mkPuppetmasterTestbench _test
	rm -f bsv/*.cxx bsv/*.h bsv/*.o bsv/*.bo bsv/*.ba bsv/*.sched bsv/a.out
	chmod +x bsv/*.exe

.PHONY: _test
_test:
	bsc $(MY_BSC_FLAGS) -sim -g $(MODULE) -u ./bsv/$(FILE)
	bsc $(MY_BSC_FLAGS) -sim -e $(MODULE) -o ./bsv/$(MODULE).exe

ifndef CONNECTALDIR
$(error CONNECTALDIR variable is not defined, aborting build)
endif

# Declare all Connectal interfaces
S2H_INTERFACES = HostSetupRequest:HwTop.hostSetupRequest \
  HostTxnRequest:HwTop.hostTxnRequest \
	HostWorkDone:HwTop.hostWorkDone
H2S_INTERFACES = HwTop:DebugIndication \
  HwTop:WorkIndication

# Connectal requires these Make variables
BSVFILES += bsv/PmConfig.bsv bsv/HwTypes.bsv
BSVPATH += $(CONNECTALDIR)/bsv
ifeq ($(strip $(DB)),)
CPPFILES += main.cpp
else
CPPFILES += db.cpp
DB_SRC_DIRS = ./ ./benchmarks/ ./concurrency_control/ ./storage/ ./system/
DB_CPPS = $(foreach dir, $(DB_SRC_DIRS), $(wildcard ./DBx1000/$(dir)*.cpp))
CPPFILES += $(filter-out ./DBx1000/./main.cpp, $(DB_CPPS))
endif

CONNECTALFLAGS += --mainclockperiod=20
CONNECTALFLAGS += --cxxflags="-g -std=c++17 -DNOGRAPHITE=1"
CONNECTALFLAGS += --bscflags="+RTS -K1G -H6G -RTS"
ifneq ($(strip $(DB)),)
CONNECTALFLAGS += $(foreach dir, $(DB_SRC_DIRS), -I./DBx1000/$(dir))
endif
ifneq ($(strip $(QUIET)),)
CONNECTALFLAGS += -D QUIET
endif
ifneq ($(strip $(DEBUG)),)
CONNECTALFLAGS += -D DEBUG
endif
CONNECTALFLAGS += --nonstrict

CONNECTALFLAGS += --verilatorflags="--no-timing"


CONNECTALFLAGS += -D DEBUG
include $(CONNECTALDIR)/Makefile.connectal

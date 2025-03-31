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
CONNECTALFLAGS += --cxxflags="-std=c++17 -DNOGRAPHITE=1"
CONNECTALFLAGS += --bscflags="+RTS -K1G -H6G -RTS"
ifneq ($(strip $(DB)),)
CONNECTALFLAGS += $(foreach dir, $(DB_SRC_DIRS), -I./DBx1000/$(dir))
CONNECTALFLAGS += -D EXTERNAL_PUPPETS
endif
ifneq ($(strip $(QUIET)),)
CONNECTALFLAGS += -D QUIET
endif
ifneq ($(strip $(DEBUG)),)
CONNECTALFLAGS += -D DEBUG
CONNECTALFLAGS += -D DEBUG_S
CONNECTALFLAGS += -D DEBUG_SH
CONNECTALFLAGS += -D DEBUG_R
endif
CONNECTALFLAGS += --nonstrict

CONNECTALFLAGS += --verilatorflags="--no-timing"


include $(CONNECTALDIR)/Makefile.connectal

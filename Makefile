ifndef CONNECTALDIR
$(error CONNECTALDIR variable is not defined, aborting build)
endif

S2H_INTERFACES = HostToPuppetmasterRequest:PmTop.request
H2S_INTERFACES = PmTop:PuppetmasterToHostIndication
BSVFILES += bsv/PmConfig.bsv bsv/PmIfc.bsv
BSVPATH += $(CONNECTALDIR)/bsv
CPPFILES += db.cpp
DB_SRC_DIRS = ./ ./benchmarks/ ./concurrency_control/ ./storage/ ./system/
DB_CPPS = $(foreach dir, $(DB_SRC_DIRS), $(wildcard ./DBx1000/$(dir)*.cpp))
CPPFILES += $(filter-out ./DBx1000/./main.cpp, $(DB_CPPS))

CONNECTALFLAGS += --mainclockperiod=20
CONNECTALFLAGS += --cxxflags="-std=c++17 -D NOGRAPHITE=1"
CONNECTALFLAGS += --nonstrict
CONNECTALFLAGS += --bscflags="+RTS -K1G -H6G -RTS"
CONNECTALFLAGS += $(foreach dir, $(DB_SRC_DIRS), -I./DBx1000/$(dir))
CONNECTALFLAGS += -D DISPLAY_LOG
ifneq ($(strip $(DEBUG)),)
CONNECTALFLAGS += -D DEBUG
endif


include $(CONNECTALDIR)/Makefile.connectal

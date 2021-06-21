ifndef CONNECTALDIR
$(error CONNECTALDIR variable is not defined, aborting build)
endif

ifeq ($(strip $(DB)),)
S2H_INTERFACES = HostToPuppetmasterRequest:PmTop.request
H2S_INTERFACES = PmTop:PuppetmasterToHostIndication
else
S2H_INTERFACES = HostToPuppetmasterRequest:PmTop.request HostToPuppetRequest:PmTop.puppetRequest
H2S_INTERFACES = PmTop:PuppetmasterToHostIndication PmTop:PuppetToHostIndication
endif
BSVFILES += bsv/PmConfig.bsv bsv/PmIfc.bsv
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
CONNECTALFLAGS += --cxxflags="-std=c++17 -D NOGRAPHITE=1"
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
endif


include $(CONNECTALDIR)/Makefile.connectal

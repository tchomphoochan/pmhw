ifndef CONNECTALDIR
$(error CONNECTALDIR variable is not defined, aborting build)
endif

S2H_INTERFACES = HostToPuppetmaster:PmTop.request
H2S_INTERFACES = PmTop:PuppetmasterToHostIndication
BSVFILES += PmIfc.bsv
BSVPATH += $(CONNECTALDIR)/bsv
CPPFILES += main.cpp

CONNECTALFLAGS += --mainclockperiod=20
CONNECTALFLAGS += --cxxflags="-std=c++17"
CONNECTALFLAGS += --nonstrict
CONNECTALFLAGS += --bscflags="+RTS -K1G -H6G -RTS"


include $(CONNECTALDIR)/Makefile.connectal

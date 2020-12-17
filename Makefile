CONNECTALDIR ?= connectal
S2H_INTERFACES = \
	HostToPuppetmaster:PmTop.request
H2S_INTERFACES= \
	PmTop:PuppetmasterToHostIndication
BSVFILES += \
	PmIfc.bsv \
	PmTop.bsv 
BSVPATH += / \
	$(CONNECTALDIR)/bsv
CPPFILES += \
	main.cpp \


CONNECTALFLAGS += --cxxflags="-std=c++17"
CONNECTALFLAGS += --nonstrict

include $(CONNECTALDIR)/Makefile.connectal

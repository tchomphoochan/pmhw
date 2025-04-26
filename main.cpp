#include <stdio.h>

#include "pmhw.h"

int main() {
  pmhw_reset();

  pmhw_config_t config;
  pmhw_get_config(&config);

  printf("%d\n", config.logNumberRenamerThreads);
  printf("%d\n", config.logNumberShards);
  printf("%d\n", config.logSizeShard);
  printf("%d\n", config.logNumberHashes);
  printf("%d\n", config.logNumberComparators);
  printf("%d\n", config.logNumberSchedulingRounds);
  printf("%d\n", config.logNumberPuppets);
  printf("%d\n", config.numberAddressOffsetBits);
  printf("%d\n", config.logSizeRenamerBuffer);
  printf("%d\n", config.useSimulatedTxnDriver);
  printf("%d\n", config.useSimulatedPuppets);
  printf("%d\n", config.simulatedPuppetsClockPeriod);

}

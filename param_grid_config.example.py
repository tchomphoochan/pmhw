"""Parameter grid configuration example."""
# 1. LogNumberRenamerThreads
# 2. LogSizeShard
# 3. LogNumberShards
# 4. LogNumberHashes
# 5. LogNumberComparators
# 6. LogNumberSchedulingRounds
# 7. LogNumberPuppets
# 8. NumberAddressOffsetBits
# 9. LogSizeRenamerBuffer
# 10. transaction time multiplier
param_combs = [
    (3, 7, 3, 3, 1, 1, 3, 6, 8, 512),
]

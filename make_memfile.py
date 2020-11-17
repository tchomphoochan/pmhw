"""Generate memory file for rename table."""
import math
import sys

params = {}

with open(sys.argv[1], "rt") as infile:
    while (line := infile.readline()) :
        words = line.strip(" ;\n").split()
        if len(words) != 3 or words[0] != "typedef":
            continue
        try:
            value = int(words[1])
        except ValueError:
            continue
        else:
            params[words[2]] = value

log_num_table_entries = params["LogNumberLiveObjects"] - params["LogNumberShards"]
num_table_entries = 2 ** log_num_table_entries
counter_width = params["LogNumberLiveObjects"] + 1
address_width = params["LogSizeMemory"]
entry_width = counter_width + address_width
entry_width_hex = math.ceil(entry_width / 4)

with open(sys.argv[2], "wt") as outfile:
    for _ in range(num_table_entries):
        entry = "".join("0" for _ in range(entry_width_hex))
        print(entry, file=outfile)

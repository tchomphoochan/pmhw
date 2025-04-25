#!/usr/bin/env python3
"""Script to analyze Puppetmaster hardware logs."""
from __future__ import annotations

import bisect
from argparse import ArgumentParser, FileType
from collections import defaultdict
from collections.abc import Sequence
from io import TextIOBase
from typing import Optional

try:
    import matplotlib.pyplot as plt
    import numpy as np
except ImportError:
    plt = np = None


def main() -> None:
    """Entry point for analyze.py."""
    # Parse command-line arguments.
    parser = ArgumentParser(description=__doc__)
    parser.add_argument("file", help="hardware log file", type=FileType("r"))
    parser.add_argument(
        "-q",
        "--frequency",
        help="frequency used for throughput calculation (in MHz)",
        type=int,
        default=200,
    )
    parser.add_argument(
        "-b",
        "--bins",
        help="number of bins in the latency historgams",
        type=int,
        default=10,
    )
    parser.add_argument(
        "-c",
        "--cutoff",
        help="cutoff (value between 0 and 1) for top bin in the latency historgams",
        type=float,
        default=0.95,
    )
    parser.add_argument(
        "-t",
        "--text-only",
        help="do not display plot even if Matplotlib is available",
        action="store_true",
        default=np is None or plt is None,
    )
    args = parser.parse_args()

    # Analyze log file.
    throughput, latency_data, core_usage = load_file(args.file)

    # Output throughput.
    print("Throughput:")
    print(f"\t{throughput:.3e} /cycle")
    print(f"\t{throughput * args.frequency * 1e6:,.0f} /s at {args.frequency} MHz")

    # Output latency.
    latencies = sorted(latency_data.values())
    min_latency = latencies[0]
    max_latency = latencies[int(len(latencies) * args.cutoff) - 1]
    print("Latency:")
    print_cutoff_sums(latencies, 2)
    if not args.text_only:
        plot_histogram(latencies, args.bins, min_latency, max_latency)

    # Output core utilization.
    total_cores = max(n for n, val in core_usage.items() if val > 0)
    total_cycles = sum(core_usage.values())
    print("Core usage:")
    for n_cores in range(total_cores + 1):
        print(f"\t{n_cores} cores: {core_usage[n_cores] / total_cycles:.2%}")


def load_file(file: TextIOBase) -> tuple[float, dict[int, int], dict[int, int]]:
    """Read and parse log file into dictionary mapping ids to data."""
    receive_times: dict[int, int] = dict()
    start_times: dict[int, int] = dict()
    core_usage: defaultdict[int, int] = defaultdict(int)

    first_start: Optional[int] = None
    last_start: int
    last_finish: int = -1
    n_transactions = 0
    n_cores_used = 0

    while True:
        # Line format: "[  XXXX] FILE: MESSAGE"
        line = file.readline()
        if not line:  # EOF
            break
        try:
            timestamp_str, entry = line.split("]")
        except ValueError:  # no or multiple ]s
            continue

        try:
            src, message = entry.strip().split(":")
            if src != 'Puppetmaster':
                continue
        except ValueError:  # no or multiple :s
            continue

        verb, *phrase = message.strip().split(" ")

        if not phrase or not phrase[0].startswith("T#"):
            continue

        transaction_id_str = phrase[0][2:]
        try:
            transaction_id = int(transaction_id_str, base=16)
        except ValueError:
            raise ValueError(f"invalid transaction id: {transaction_id_str}")

        timestamp_str = timestamp_str.strip("[ ")
        try:
            timestamp = int(timestamp_str)
        except ValueError:
            continue

        if verb == "enqueued":
            if transaction_id in receive_times:
                raise ValueError(
                    f"duplicate 'enqueued' entry for transaction {transaction_id}"
                )
            receive_times[transaction_id] = timestamp
        elif verb == "starting":
            if transaction_id in start_times:
                raise ValueError(
                    f"duplicate 'starting' entry for transaction {transaction_id}"
                )
            start_times[transaction_id] = timestamp
            if first_start is None:
                first_start = timestamp
            else:
                core_usage[n_cores_used] += timestamp - max(last_start, last_finish)
            n_cores_used += 1
            last_start = timestamp
            n_transactions += 1
        elif verb == "finished":
            core_usage[n_cores_used] += timestamp - max(last_start, last_finish)
            n_cores_used -= 1
            last_finish = timestamp

    if first_start is None:
        raise ValueError("no transactions started")
    throughput = n_transactions / (last_start - first_start)

    # if len(receive_times) != len(start_times):
    #     raise ValueError(
    #         f"{len(receive_times) - len(start_times)} transactions not started"
    #     )
    for k in list(receive_times.keys()):
        if k not in start_times:
            del receive_times[k]

    latency_data = dict()
    for transaction_id, receive_time in receive_times.items():
        if transaction_id not in start_times:
            raise ValueError(f"transaction {transaction_id} not started")
        start_time = start_times[transaction_id]
        latency_data[transaction_id] = start_time - receive_time

    return throughput, latency_data, core_usage


def plot_histogram(values: Sequence[int], n_bins: int, min_val: int, max_val: int):
    """Plot histogram using Matplotlib."""
    hist, bin_edges = np.histogram(values, bins=n_bins, range=(min_val, max_val))
    _, axis = plt.subplots()
    axis.plot(bin_edges[1:], hist)
    plt.show()


def print_histogram(values: Sequence[int], n_bins: int, min_val: int, max_val: int):
    """Print histogram-like table."""
    val_range = max_val - min_val
    bin_limits = [min_val + val_range * (n + 1) / n_bins for n in range(n_bins)]
    bin_counts = [0] * (len(bin_limits) + 1)
    for value in values:
        bin_index = bisect.bisect_left(bin_limits, value)
        bin_counts[bin_index] += 1
    val_width = len(str(max_val))
    count_width = len(str(len(values)))
    for i, count in enumerate(bin_counts):
        low = min_val if i == 0 else round(bin_limits[i - 1])
        high = float("inf") if i == len(bin_limits) else round(bin_limits[i])
        print(f"\t{low:{val_width}} < x <= {high:{val_width}}: {count:{count_width}}")


def print_cutoff_sums(values: Sequence[int], n_orders: int):
    """Print cumulative sum of counts up to certain cutoffs."""
    cutoff_prec = abs(n_orders) - 2
    val_width = len(str(values[-1]))
    for n in range(abs(n_orders)):
        half_cutoff = 1 - 1 / (2 * 10 ** n)
        half_index = int(len(values) * half_cutoff) - 1
        half_value = values[half_index]
        print(f"\t{half_cutoff:.{cutoff_prec}%} below: {half_value:{val_width}}")
        tenth_cutoff = 1 - 1 / 10 ** (n + 1)
        tenth_index = int(len(values) * tenth_cutoff) - 1
        tenth_value = values[tenth_index]
        print(f"\t{tenth_cutoff:.{cutoff_prec}%} below: {tenth_value:{val_width}}")


if __name__ == "__main__":
    main()

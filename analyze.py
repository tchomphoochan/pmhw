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
        default=250,
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
    print_histogram(latencies, args.bins, min_latency, max_latency)
    if not args.text_only:
        plot_histogram(latencies, args.bins, min_latency, max_latency)

    # Output core utilization.
    total_cores = max(core_usage)
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
            filename, message = entry.strip().split(":")
        except ValueError:  # no or multiple :s
            continue

        if filename.strip() != "PmTop":
            continue

        try:
            verb, transaction_id_str = message.strip().split(" ")
        except ValueError:
            raise ValueError(f"unknown log entry format: {message}")

        # Strip id prefix from newer logs
        if transaction_id_str[:2] == "T#":
            transaction_id_str = transaction_id_str[2:]

        try:
            transaction_id = int(transaction_id_str, base=16)
        except ValueError:
            raise ValueError(f"invalid transaction id: {transaction_id_str}")

        timestamp_str = timestamp_str.strip("[ ")
        if set(timestamp_str) == {"-"}:
            continue
        try:
            timestamp = int(timestamp_str)
        except ValueError as e:
            raise ValueError(f"invalid timestamp: {e}")

        if verb == "received":
            if transaction_id in receive_times:
                raise ValueError(
                    f"duplicate 'received' entry for transaction {transaction_id}"
                )
            receive_times[transaction_id] = timestamp
        elif verb == "started":
            if transaction_id in start_times:
                raise ValueError(
                    f"duplicate 'started' entry for transaction {transaction_id}"
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

    if len(receive_times) != len(start_times):
        raise ValueError(
            f"{len(receive_times) - len(start_times)} transactions not started"
        )

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


if __name__ == "__main__":
    main()

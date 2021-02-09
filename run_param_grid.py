#!/usr/bin/env python3
"""Explore a grid of design parameters for Puppetmaster."""
import itertools
import subprocess
import traceback
from pathlib import Path

# Fixed parameters.
tr_count = 1024
mem_size = 1024

# Variable parameters (logarithmic).
buffer_sizes = [1, 2, 3, 4]
shard_sizes = [7]
shard_counts = [3]
hash_counts = [3]
comparator_counts = [1]
scheduling_counts = [1]
puppet_counts = [3]
transaction_times = [128]

# Fixed paths.
src_dir = Path.cwd()
data_dir = src_dir / "csv"
log_dir = src_dir / "log"
build_dir = src_dir / "verilator"
run_dir = build_dir / "bin"


def run_sim(
    buffer_size: int,
    shard_size: int,
    shard_count: int,
    hash_count: int,
    comparator_count: int,
    scheduling_count: int,
    puppet_count: int,
    tr_time: int,
) -> None:
    """Build and run a single simulation with the given parameters."""
    # Write configuration file.
    with open(src_dir / "PmConfig.bsv", "w") as conf:
        print(
            f"typedef {buffer_size} LogSizeRenamerBuffer;\n"
            f"typedef {shard_size} LogSizeShard;\n"
            f"typedef {shard_count} LogNumberShards;\n"
            f"typedef {hash_count} LogNumberHashes;\n"
            f"typedef {comparator_count} LogNumberComparators;\n"
            f"typedef {scheduling_count} LogNumberSchedulingRounds;\n"
            f"typedef {puppet_count} LogNumberPuppets;\n",
            file=conf,
        )

    # Build simulation executable.
    subprocess.run(
        [
            "/usr/bin/env",
            "make",
            'VERILATOR_PROJECT_ARGS="--autoflush"',
            "-j",
            "-C",
            build_dir,
        ],
        check=True,
    )

    # Compute paths.
    sim_name = f"database_{tr_count}_m{mem_size}"
    cur_log_dir = (
        log_dir
        / f"{shard_size + shard_count}bit"
        / f"{2 ** buffer_size}rename"
        / f"{2 ** (scheduling_count + comparator_count + 1)}schedule"
        / f"{2 ** comparator_count}comp"
        / f"{2 ** shard_count}shard"
        / f"{2 ** hash_count}hash"
        / f"{2 ** puppet_count}puppet"
    )
    cur_log_file = cur_log_dir / f"{sim_name}_x{tr_time}.log"

    # Run simulation.
    cur_log_dir.mkdir(parents=True, exist_ok=True)
    with cur_log_file.open("x") as logfile:
        subprocess.run(
            [
                run_dir / "ubuntu.exe",
                "-f",
                data_dir / f"{sim_name}.csv",
                "-x",
                str(tr_time),
            ],
            check=True,
            stdout=logfile,
        )


def main() -> None:
    """Execute parameter grid script."""
    # Create build directory.
    subprocess.run(
        [
            "/usr/bin/env",
            "make",
            "gen.verilator",
            "-C",
            src_dir,
        ],
        check=True,
    )

    # Run every possible parameter combination.
    param_combs = itertools.product(
        buffer_sizes,
        shard_sizes,
        shard_counts,
        hash_counts,
        comparator_counts,
        scheduling_counts,
        puppet_counts,
        transaction_times,
    )
    for bufsize, shardsize, shards, hashes, comps, scheds, puppets, tr_t in param_combs:
        try:
            run_sim(bufsize, shardsize, shards, hashes, comps, scheds, puppets, tr_t)
        except Exception:
            # Print traceback on error in a given simulation and continue.
            traceback.print_exc()


if __name__ == "__main__":
    main()

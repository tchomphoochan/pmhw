#!/usr/bin/env python3
"""Explore a grid of design parameters for Puppetmaster."""
import subprocess
import sys
import traceback
from pathlib import Path

# Fixed parameters.
tr_count = 1024
mem_size = 65536

# Variable parameters.
# 1. LogSizeRenamerBuffer
# 2. LogSizeShard
# 3. LogNumberShards
# 4. LogNumberHashes
# 5. LogNumberComparators
# 6. LogNumberSchedulingRounds
# 7. LogNumberPuppets
# 8. transaction time multiplier
param_combs = [
    # base
    # (3, 7, 3, 3, 1, 1, 5, 512),
    # rename buffer
    (1, 7, 3, 3, 1, 1, 5, 512),
    (2, 7, 3, 3, 1, 1, 5, 512),
    (4, 7, 3, 3, 1, 1, 5, 512),
    (6, 7, 3, 3, 1, 1, 5, 512),
    (8, 7, 3, 3, 1, 1, 5, 512),
    (10, 7, 3, 3, 1, 1, 5, 512),
    # sharding
    (3, 10, 0, 3, 1, 1, 8, 512),
    (3, 8, 2, 3, 1, 1, 5, 512),
    (3, 6, 4, 3, 1, 1, 5, 512),
    (3, 4, 6, 3, 1, 1, 5, 512),
    (3, 2, 8, 3, 1, 1, 5, 512),
    # hashing
    (3, 7, 3, 1, 1, 1, 5, 512),
    (3, 7, 3, 2, 1, 1, 5, 512),
    (3, 7, 3, 4, 1, 1, 5, 512),
    (3, 7, 3, 6, 1, 1, 5, 512),
    # comparators with pool size 8
    (3, 7, 3, 3, 2, 0, 5, 512),
    (3, 7, 3, 3, 0, 2, 5, 512),
    # comparators with pool size 32
    (3, 7, 3, 3, 4, 0, 5, 512),
    (3, 7, 3, 3, 2, 2, 5, 512),
    (3, 7, 3, 3, 3, 1, 5, 512),
    (3, 7, 3, 3, 0, 4, 5, 512),
    # comparators with pool size 128
    (3, 7, 3, 3, 6, 0, 5, 512),
    (3, 7, 3, 3, 4, 2, 5, 512),
    (3, 7, 3, 3, 2, 4, 5, 512),
    (3, 7, 3, 3, 1, 5, 5, 512),
    (3, 7, 3, 3, 0, 6, 5, 512),
    # transaction times on 32 puppets
    (3, 7, 3, 3, 1, 1, 5, 256),
    (3, 7, 3, 3, 1, 1, 5, 512),
    (3, 7, 3, 3, 1, 1, 5, 1024),
    # transaction times on 64 puppets
    (3, 7, 3, 3, 1, 1, 6, 512),
    (3, 7, 3, 3, 1, 1, 6, 1024),
    (3, 7, 3, 3, 1, 1, 6, 2048),
    # transaction times on 128 puppets
    (3, 7, 3, 3, 1, 1, 7, 1024),
    (3, 7, 3, 3, 1, 1, 7, 2048),
    (3, 7, 3, 3, 1, 1, 7, 4096),
    # transaction times on 256 puppets
    (3, 7, 3, 3, 1, 1, 8, 2048),
    (3, 7, 3, 3, 1, 1, 8, 4096),
    (3, 7, 3, 3, 1, 1, 8, 8192),
    # transaction times on 512 puppets
    (3, 7, 3, 3, 1, 1, 9, 4096),
    (3, 7, 3, 3, 1, 1, 9, 8192),
    (3, 7, 3, 3, 1, 1, 9, 16192),
    # transaction times on 1024 puppets
    (3, 7, 3, 3, 1, 1, 10, 8192),
    (3, 7, 3, 3, 1, 1, 10, 16192),
    (3, 7, 3, 3, 1, 1, 10, 32384),
]

# Fixed paths.
src_dir = Path(sys.argv[1]).resolve() if sys.argv[1:] else Path.cwd()
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
    for bufsize, shardsize, shards, hashes, comps, scheds, puppets, tr_t in param_combs:
        try:
            run_sim(bufsize, shardsize, shards, hashes, comps, scheds, puppets, tr_t)
        except Exception:
            # Print traceback on error in a given simulation and continue.
            traceback.print_exc()


if __name__ == "__main__":
    main()

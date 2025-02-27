#!/usr/bin/env python3
"""Explore a grid of design parameters for Puppetmaster."""
import subprocess
import sys
import traceback
from pathlib import Path

from param_grid_config import param_combs

# Fixed parameters.
tr_count = 1024
mem_size = 65536

# Fixed paths.
src_dir = Path(sys.argv[1]).resolve() if sys.argv[1:] else Path.cwd()
data_dir = src_dir / "csv"
log_dir = src_dir / "log"
build_dir = src_dir / "verilator"
run_dir = build_dir / "bin"


def run_sim(
    renamer_threads: int,
    shard_size: int,
    shard_count: int,
    hash_count: int,
    comparator_count: int,
    scheduling_count: int,
    puppet_count: int,
    offset_size: int,
    buffer_size: int,
    tr_time: int,
) -> None:
    """Build and run a single simulation with the given parameters."""
    # Write configuration file.
    with open(src_dir / "PmConfig.bsv", "w") as conf:
        print(
            f"typedef {renamer_threads} LogNumberRenamerThreads;\n"
            f"typedef {shard_size} LogSizeShard;\n"
            f"typedef {shard_count} LogNumberShards;\n"
            f"typedef {hash_count} LogNumberHashes;\n"
            f"typedef {comparator_count} LogNumberComparators;\n"
            f"typedef {scheduling_count} LogNumberSchedulingRounds;\n"
            f"typedef {puppet_count} LogNumberPuppets;\n",
            f"typedef {offset_size} NumberAddressOffsetBits;\n",
            f"typedef {buffer_size} LogSizeRenamerBuffer;\n",
            file=conf,
        )

    # Build simulation executable.
    subprocess.run(
        [
            "/usr/bin/env",
            "make",
            'VERILATOR_PROJECT_ARGS=--autoflush --no-timing',
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
        / f"{2 ** renamer_threads}rename"
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
    for params in param_combs:
        try:
            run_sim(*params)
        except Exception:
            # Print traceback on error in a given simulation and continue.
            traceback.print_exc()


if __name__ == "__main__":
    main()

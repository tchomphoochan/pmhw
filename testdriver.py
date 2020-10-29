#! /bin/env python3
"""Test script for BSV modules."""
import io
import itertools
import subprocess
import sys
from argparse import ArgumentParser
from pathlib import Path

cur_dir = Path(__file__).parent.resolve()

# Assemble available tests.
tests = {}
test_dir = cur_dir / "test_outputs"
for test_file in test_dir.iterdir():
    test_name = test_file.stem
    with open(test_file, "rt") as f:
        tests[test_name] = f.readlines()

# Read command line arguments.
parser = ArgumentParser(description=__doc__)
parser.add_argument(
    "module",
    metavar="InterfaceName",
    choices=tests,
    help="name of the interface that the module under testing implements",
)
args = parser.parse_args()

# Run test.
proc_dir = cur_dir / "build"
module_name = f"mk{args.module}Testbench"
proc_path = proc_dir / module_name
if not proc_path.is_file():
    sys.exit(f"Module executable {module_name} not found.")
proc = subprocess.Popen([proc_path], cwd=proc_dir, stdout=subprocess.PIPE)
if proc.stdout is not None:
    proc_output = io.TextIOWrapper(proc.stdout)
else:
    raise RuntimeError("this should never happen.")

# Check test output.
for test_index in itertools.count():
    expected = tests[args.module]
    if test_index == len(expected):
        print(f"Passed {test_index}/{len(expected)} tests.")
        sys.exit()
    result = proc_output.readline()
    assert (
        result == expected[test_index]
    ), f"got {result}, expected {expected[test_index]}"

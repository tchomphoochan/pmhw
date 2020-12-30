#! /bin/env python3
"""Test script for BSV modules."""
import os
import signal
import subprocess
import sys
from argparse import ArgumentParser
from pathlib import Path
from threading import Thread
from typing import List

TEST_TIMEOUT_SECONDS = 5

cur_dir = Path(__file__).parent.resolve()

# Assemble available tests.
tests = {}
test_dir = cur_dir / "test_outputs"
for test_file in test_dir.iterdir():
    test_name = test_file.stem
    with open(test_file, "rt") as f:
        tests[test_name] = f.read().splitlines()

# Read command line arguments.
parser = ArgumentParser(description=__doc__)
parser.add_argument(
    "module",
    metavar="InterfaceName",
    choices=tests,
    help="name of the interface that the module under testing implements",
)
parser.add_argument(
    "-d",
    "--directory",
    metavar="dir",
    help="look for tested binaries in dir",
    default=os.getcwd(),
)
args = parser.parse_args()

# Run test.
proc_dir = Path(args.directory)
module_name = f"mk{args.module}Testbench"
proc_path = proc_dir / module_name
if not proc_path.is_file():
    sys.exit(f"Module executable {proc_path} not found.")
proc = subprocess.Popen(
    [proc_path],
    cwd=proc_dir,
    stdout=subprocess.PIPE,
    stderr=subprocess.STDOUT,
    text=True,
    bufsize=1,
    preexec_fn=os.setsid,
)
assert proc.stdout is not None
proc_output = proc.stdout

# Check test output.
expected = tests[args.module]
results: List[str] = []
passed = 0
for i in range(len(expected)):
    thread = Thread(target=lambda: results.append(proc_output.readline()))
    thread.daemon = True
    thread.start()
    thread.join(TEST_TIMEOUT_SECONDS)
    try:
        assert not thread.is_alive(), "test timed out"
        result = results[-1].rstrip()
        assert result == expected[i], f"got\n\t{result}\nexpected\n\t{expected[i]}"
    except AssertionError as e:
        print(f"Test {i + 1} failed: {e}")
    else:
        passed += 1
print("-" * 80)
print(f"Passed {passed}/{len(expected)} tests.")

os.killpg(os.getpgid(proc.pid), signal.SIGTERM)

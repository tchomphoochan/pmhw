"""Test script for BSV modules."""
import io
import itertools
import subprocess
import sys
from pathlib import Path

cur_dir = Path(__file__).parent.resolve()

# Assemble available tests.
tests = {}
test_dir = cur_dir / "test_outputs"
for test_file in test_dir.iterdir():
    test_name = test_file.stem.lower()
    with open(test_file, "rt") as f:
        tests[test_name] = f.readlines()

# Find test for this module.
module = sys.argv[1]
if module.lower() not in tests:
    sys.exit(f"unknown module {module}")
else:
    expected = tests[module.lower()]

# Run test.
proc_dir = cur_dir / "build"
proc_path = proc_dir / f"mk{module.capitalize()}Testbench"
proc = subprocess.Popen([proc_path], cwd=proc_dir, stdout=subprocess.PIPE)
if proc.stdout is not None:
    proc_output = io.TextIOWrapper(proc.stdout)
else:
    raise RuntimeError("this should never happen.")

# Check test output.
for test_index in itertools.count():
    if test_index == len(expected):
        print(f"Passed {test_index}/{len(expected)} tests.")
        sys.exit()
    result = proc_output.readline()
    assert (
        result == expected[test_index]
    ), f"got {result}, expected {expected[test_index]}"

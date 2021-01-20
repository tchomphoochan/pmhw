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
group = parser.add_mutually_exclusive_group(required=True)
group.add_argument(
    "-m",
    "--modules",
    choices=tests,
    nargs="+",
    help="modules to test",
)
group.add_argument(
    "-a",
    "--all",
    action="store_true",
    help="test all modules",
)
parser.add_argument(
    "-d",
    "--directory",
    metavar="dir",
    help="look for tested binaries in dir",
    default=os.getcwd(),
)
args = parser.parse_args()

# Run tests.
for module in args.modules or tests:
    print(f"Running tests for {module}... ", end="")

    # Start process.
    proc_dir = Path(args.directory)
    module_name = f"mk{module}Testbench"
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
        preexec_fn=os.setsid,  # necessary for the os.killpg call to work
    )
    assert proc.stdout is not None
    proc_output = proc.stdout

    # Check output.
    expected = tests[module]
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
            assert (
                result == expected[i]
            ), f"\n\tgot:     {result}\n\texpected: {expected[i]}"
        except AssertionError as e:
            print(f"\nTest {i + 1} failed: {e}", end="")
        else:
            passed += 1
    if passed == len(expected):
        print("all passed.")
    else:
        print(f"\n{passed}/{len(expected)} passed.")

    os.killpg(os.getpgid(proc.pid), signal.SIGTERM)  # kill subprocesses too

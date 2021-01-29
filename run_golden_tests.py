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

# Read command line arguments.
parser = ArgumentParser(description=__doc__)
parser.add_argument(
    "-k",
    metavar="pattern".upper(),
    dest="pattern",
    help="only run tests which match the given substring",
    default="",
)
parser.add_argument(
    "-d",
    metavar="directory".upper(),
    dest="directory",
    help="directory for golden tests",
    default="../golden_tests",
)
parser.add_argument(
    "-t",
    metavar="timeout".upper(),
    dest="timeout",
    type=int,
    help="test timeout in seconds",
    default=5,
)
args = parser.parse_args()

# Assemble tests.
tests = {}
test_dir = Path(args.directory).resolve()
for test_file in test_dir.iterdir():
    test_name = test_file.stem
    if args.pattern not in test_name:
        continue
    with open(test_file, "rt") as f:
        tests[test_name] = f.read().splitlines()

# Run tests.
for module, expected in tests.items():
    print(f"Running golden test for {module}... ", end="")

    # Start process.
    proc_dir = Path(os.getcwd())
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
    if proc.stdout is None:
        raise RuntimeError("process has no stdout")
    proc_output = proc.stdout

    # Check output.
    results: List[str] = []
    passed = 0
    for i in range(len(expected)):
        thread = Thread(target=lambda: results.append(proc_output.readline()))
        thread.daemon = True
        thread.start()
        thread.join(args.timeout)
        if thread.is_alive():
            print("test timed out.", end="")
            break
        result = results[-1].rstrip()
        if result != expected[i]:
            print(f"\nLine {i + 1} doesn't match:")
            print(f"\tgot:      {result}")
            print(f"\texpected: {expected[i]}", end="")
        else:
            passed += 1
    if passed == len(expected):
        print("passed.")
    else:
        print(f"\n{passed}/{len(expected)} lines match.")

    os.killpg(os.getpgid(proc.pid), signal.SIGTERM)  # kill subprocesses too

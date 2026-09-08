#!/usr/bin/env python3
"""Run every PANELCOMP test. Exits non-zero if any check fails."""
import os, subprocess, sys

HERE = os.path.dirname(os.path.abspath(__file__))
TESTS = ['test_sheet.py', 'test_matching.py', 'test_signatures.py']

failed = []
for t in TESTS:
    print("\n" + "=" * 60 + "\n" + t + "\n" + "=" * 60)
    if subprocess.call([sys.executable, os.path.join(HERE, t)]) != 0:
        failed.append(t)

print("\n" + "=" * 60)
print("FAILED: %s" % ", ".join(failed) if failed else "all suites pass")
sys.exit(1 if failed else 0)

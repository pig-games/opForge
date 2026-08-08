#!/usr/bin/env python3
"""Run deterministic native-porting checks without launching FS-UAE or network work."""

from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path

CHECKS = (
    "check_native_porting_slice.py",
    "check_native_instrumentation_safety.py",
    "check_native_contract_asserts.py",
    "check_fsuae_invocation_policy.py",
    "check_native_runtime_no_growth.py",
    "check_native_debug_evidence_classification.py",
    "check_native_test_module_ownership.py",
    "check_native_fs_uae_proof_contract.py",
)


def commands(root: Path, staged: bool, metadata: str | None) -> list[list[str]]:
    script_dir = root / "scripts/workflow"
    result: list[list[str]] = []
    for name in CHECKS:
        command = [sys.executable, str(script_dir / name), "--root", str(root)]
        if staged:
            command.append("--staged")
        if name == "check_native_porting_slice.py" and metadata:
            command.extend(["--metadata", metadata])
        result.append(command)
    result.append(["make", "native-68000-format-check"])
    return result


def run_gate(root: Path, staged: bool, metadata: str | None) -> int:
    for command in commands(root, staged, metadata):
        print(f"==> {' '.join(command)}", flush=True)
        result = subprocess.run(command, cwd=root, text=True)
        if result.returncode:
            print(
                f"FAIL: native porting quality gate stopped at `{Path(command[1]).name if command[0] == sys.executable else command[1]}`",
                file=sys.stderr,
            )
            return result.returncode
    print("PASS: native porting quality gate complete")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", default=".")
    parser.add_argument("--staged", action="store_true")
    parser.add_argument("--metadata")
    parser.add_argument("--fsuae-test")
    args = parser.parse_args()
    if args.fsuae_test:
        print(
            "FAIL: default wrapper never launches FS-UAE; run the named test explicitly after deterministic checks",
            file=sys.stderr,
        )
        return 2
    return run_gate(Path(args.root).resolve(), args.staged, args.metadata)


if __name__ == "__main__":
    raise SystemExit(main())

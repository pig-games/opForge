#!/usr/bin/env python3
"""Validate documented or scripted FS-UAE invocation policy."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from native_porting_common import read_text, selected_paths


def validate_invocation(text: str, source: str = "<input>") -> list[str]:
    if "FS-UAE" not in text and "fs_uae" not in text.lower():
        return []
    errors: list[str] = []
    lower = text.lower()
    if "cargo test" in lower and "--test-threads=1" not in lower:
        errors.append(f"{source}: FS-UAE cargo test invocation requires `--test-threads=1`")
    if "cargo test" in lower and not any(
        marker in lower for marker in ("single-instance", "terminate_preexisting", "test-threads=1")
    ):
        errors.append(f"{source}: FS-UAE invocation lacks single-instance configuration")
    if any(token in text for token in ("OPFORGE_FS_UAE_SMOKE=1", "OPFORGE_FS_UAE_TESTS=1")):
        if "opt-in-allowed" not in lower and "known-good invocation" not in lower:
            errors.append(f"{source}: forbidden FS-UAE opt-in gate without policy allowance")
    if "cargo test" in lower and any(
        word in lower for word in ("reduced fixture", "prefix fixture", "truncated fixture")
    ):
        if "localization" not in lower or "proof level e" not in lower:
            errors.append(f"{source}: reduced FS-UAE fixture requires localization and proof Level E")
    return errors


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("paths", nargs="*")
    parser.add_argument("--root", default=".")
    parser.add_argument("--staged", action="store_true")
    args = parser.parse_args()
    root = Path(args.root).resolve()
    errors: list[str] = []
    for path in selected_paths(root, args.paths, args.staged):
        if path == "scripts/workflow/check_fsuae_invocation_policy.py" or path.startswith(
            "scripts/workflow/tests/"
        ):
            continue
        if Path(path).suffix.lower() not in {".md", ".py", ".sh", ".rs"}:
            continue
        errors.extend(validate_invocation(read_text(root, path), path))
    for error in errors:
        print(f"FAIL: {error}", file=sys.stderr)
    if errors:
        return 1
    print("PASS: FS-UAE invocation policy checks passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

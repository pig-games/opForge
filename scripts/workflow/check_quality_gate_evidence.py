#!/usr/bin/env python3
"""Validate workflow quality-gate sidecar evidence files."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path


GATED_ARTIFACT_PATTERNS = (
    "documentation/plans/",
    "documentation/reviews/",
    "dev-docs/reviews/",
)


def is_quality_gate(path: Path) -> bool:
    return path.name.endswith(".quality-gate.txt")


def requires_gate(path: Path) -> bool:
    normalized = path.as_posix()
    if not normalized.endswith(".md"):
        return False
    if "spec" in path.name.lower() and normalized.startswith("documentation/"):
        return True
    return any(normalized.startswith(prefix) for prefix in GATED_ARTIFACT_PATTERNS)


def validate_gate_file(path: Path) -> list[str]:
    errors: list[str] = []
    if not path.exists():
        return [f"{path}: missing quality gate file"]
    if not path.is_file():
        return [f"{path}: quality gate path is not a file"]

    text = path.read_text(encoding="utf-8")
    first_line = text.splitlines()[0].strip() if text.splitlines() else ""
    if first_line != "PASS" and not first_line.startswith("PASS:"):
        errors.append(f"{path}: quality gate file must begin with `PASS` or `PASS:`")

    source_name = path.name.removesuffix(".quality-gate.txt")
    source_path = path.with_name(source_name)
    if not source_path.exists():
        errors.append(f"{path}: source artifact `{source_path}` does not exist")

    return errors


def validate_artifact(path: Path) -> list[str]:
    if is_quality_gate(path):
        return validate_gate_file(path)
    if not requires_gate(path):
        return []
    return validate_gate_file(path.with_name(f"{path.name}.quality-gate.txt"))


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Check workflow artifacts for PASS quality-gate sidecar evidence."
    )
    parser.add_argument("paths", nargs="+", help="Artifact or .quality-gate.txt paths")
    args = parser.parse_args()

    all_errors: list[str] = []
    checked = 0
    for raw_path in args.paths:
        path = Path(raw_path)
        errors = validate_artifact(path)
        if errors:
            all_errors.extend(errors)
        if errors or is_quality_gate(path) or requires_gate(path):
            checked += 1

    if all_errors:
        for error in all_errors:
            print(f"FAIL: {error}", file=sys.stderr)
        return 1

    print(f"PASS: quality gate evidence valid for {checked} path(s)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
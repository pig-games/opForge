#!/usr/bin/env python3
"""Validate machine-readable metadata for a native parity slice."""

from __future__ import annotations

import argparse
import sys
import tomllib
from pathlib import Path

from native_porting_common import native_asm_paths, selected_paths

LEVELS = {"A", "B", "C", "D", "E"}


def parse_metadata(text: str) -> tuple[dict | None, list[str]]:
    try:
        return tomllib.loads(text), []
    except tomllib.TOMLDecodeError as err:
        return None, [f"malformed native slice metadata: {err}"]


def discover_metadata(paths: list[str]) -> str | None:
    candidates = [
        path
        for path in paths
        if path.startswith("documentation/plans/slices/") and path.endswith(".toml")
    ]
    return candidates[0] if len(candidates) == 1 else None


def validate_metadata(data: dict) -> list[str]:
    errors: list[str] = []
    section = data.get("slice")
    if not isinstance(section, dict):
        return ["metadata requires a [slice] table"]
    for field in ("name", "kind", "rust_reference", "native_boundary", "invariant"):
        if not section.get(field):
            errors.append(f"slice metadata requires non-empty `{field}`")
    tests = data.get("tests")
    if not isinstance(tests, list) or not tests:
        errors.append("slice metadata requires at least one [[tests]] entry")
        return errors
    levels: set[str] = set()
    for index, test in enumerate(tests, 1):
        if not isinstance(test, dict):
            errors.append(f"tests entry {index} must be a table")
            continue
        level = str(test.get("proof_level", "")).upper()
        if level not in LEVELS:
            errors.append(f"tests entry {index} has invalid proof_level `{level}`")
        else:
            levels.add(level)
        for field in ("name", "proves", "does_not_prove"):
            if not test.get(field):
                errors.append(f"tests entry {index} requires `{field}`")
        name = str(test.get("name", "")).lower()
        reduced = any(word in name for word in ("reduced", "prefix", "truncated"))
        if reduced and level != "E" and not test.get("semantic_completeness_justification"):
            errors.append(f"tests entry {index} reduced fixture must be Level E or justified")
    if "D" in levels and not levels.intersection({"A", "B", "C"}):
        if not section.get("host_proof_absence_justification"):
            errors.append("Level D evidence requires host-side proof or an explicit justification")
    return errors


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("paths", nargs="*")
    parser.add_argument("--root", default=".")
    parser.add_argument("--staged", action="store_true")
    parser.add_argument("--metadata")
    args = parser.parse_args()
    root = Path(args.root).resolve()
    paths = selected_paths(root, args.paths, args.staged)
    native = native_asm_paths(paths)
    if not native:
        print("PASS: native porting slice metadata not required")
        return 0
    metadata_path = args.metadata
    if not metadata_path:
        metadata_path = discover_metadata(paths)
    if not metadata_path:
        print("FAIL: native assembly changes require --metadata", file=sys.stderr)
        return 1
    try:
        text = (root / metadata_path).read_text(encoding="utf-8")
    except OSError as err:
        print(f"FAIL: missing native slice metadata: {err}", file=sys.stderr)
        return 1
    data, errors = parse_metadata(text)
    if errors:
        for error in errors:
            print(f"FAIL: {error}", file=sys.stderr)
        return 1
    assert data is not None
    errors = validate_metadata(data)
    for error in errors:
        print(f"FAIL: {error}", file=sys.stderr)
    if errors:
        return 1
    print("PASS: native porting slice metadata is complete")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

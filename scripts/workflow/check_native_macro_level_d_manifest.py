#!/usr/bin/env python3
"""Validate retained Level D receipts for the native macro preprocessor slice."""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from datetime import datetime
from pathlib import Path


EXPECTED_TESTS = (
    "native_macro_invocation_fixture_fs_uae",
    "native_macro_preprocessor_harness_fs_uae_proves_capture_lookup_and_nested_frame_rejection",
)
COMMAND_TEMPLATE = "cargo test -p asm {name} -- --nocapture --test-threads=1"
SHA_RE = re.compile(r"^[0-9a-f]{40}$")


def git_value(root: Path, expression: str) -> str:
    return subprocess.run(
        ["git", "rev-parse", expression],
        cwd=root,
        check=True,
        text=True,
        capture_output=True,
    ).stdout.strip()


def validate_manifest(data: object, *, expected_commit: str | None, expected_tree: str | None) -> list[str]:
    if not isinstance(data, dict):
        return ["manifest root must be an object"]
    errors: list[str] = []
    if data.get("manifest_version") != 1:
        errors.append("manifest_version must be 1")
    if data.get("kind") != "native-macro-preprocessor-level-d":
        errors.append("kind must be native-macro-preprocessor-level-d")
    if data.get("scope") != "macro-substitution-reentry":
        errors.append("scope must be macro-substitution-reentry")
    if data.get("status") != "PASS":
        errors.append("status must be PASS")
    timestamp = data.get("completed_at_utc")
    if not isinstance(timestamp, str):
        errors.append("completed_at_utc must be an ISO-8601 UTC string")
    else:
        try:
            datetime.strptime(timestamp, "%Y-%m-%dT%H:%M:%SZ")
        except ValueError:
            errors.append("completed_at_utc must use YYYY-MM-DDTHH:MM:SSZ")
    source = data.get("source")
    if not isinstance(source, dict):
        errors.append("source must be an object")
    else:
        for field, expected in (("commit", expected_commit), ("tree", expected_tree)):
            value = source.get(field)
            if not isinstance(value, str) or not SHA_RE.fullmatch(value):
                errors.append(f"source.{field} must be a full git SHA")
            elif expected is not None and value != expected:
                errors.append(f"source.{field} does not match the expected source identity")
    tests = data.get("tests")
    if not isinstance(tests, list):
        return [*errors, "tests must be an array"]
    names: list[object] = []
    for index, test in enumerate(tests, 1):
        if not isinstance(test, dict):
            errors.append(f"tests entry {index} must be an object")
            continue
        name = test.get("name")
        names.append(name)
        if test.get("result") != "PASS":
            errors.append(f"tests entry {index} must record result PASS")
        if test.get("command") != COMMAND_TEMPLATE.format(name=name):
            errors.append(f"tests entry {index} must retain the exact cargo command")
    if tuple(names) != EXPECTED_TESTS:
        errors.append("tests must list every required Level D test in canonical order")
    return errors


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("manifest", type=Path)
    parser.add_argument("--root", type=Path, default=Path("."))
    parser.add_argument("--expect-head", action="store_true")
    args = parser.parse_args()
    try:
        data = json.loads(args.manifest.read_text(encoding="utf-8"))
    except OSError as err:
        print(f"FAIL: could not read manifest: {err}", file=sys.stderr)
        return 1
    except json.JSONDecodeError as err:
        print(f"FAIL: malformed manifest JSON: {err}", file=sys.stderr)
        return 1
    expected_commit = expected_tree = None
    if args.expect_head:
        try:
            root = args.root.resolve()
            expected_commit = git_value(root, "HEAD")
            expected_tree = git_value(root, "HEAD^{tree}")
        except subprocess.CalledProcessError as err:
            print(f"FAIL: could not resolve expected source identity: {err}", file=sys.stderr)
            return 1
    errors = validate_manifest(data, expected_commit=expected_commit, expected_tree=expected_tree)
    if errors:
        for error in errors:
            print(f"FAIL: {error}", file=sys.stderr)
        return 1
    print(f"PASS: native macro Level D manifest valid for {args.manifest}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

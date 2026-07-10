#!/usr/bin/env python3
"""Validate retained Level D completion manifests for native CLI expansion."""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from datetime import datetime
from pathlib import Path


EXPECTED_TESTS = (
    "native_column_one_directive_routing_fs_uae",
    "native_opcore_counted_for_fs_uae",
    "native_opcore_sequence_assignment_fs_uae",
    "native_opcore_iterable_for_fs_uae",
    "native_opcore_while_fs_uae",
    "native_opcore_conditionals_fs_uae",
    "native_opcore_scopes_fs_uae",
)
COMMAND_TEMPLATE = "cargo test -p asm {name} -- --nocapture --test-threads=1"
COMMIT_RE = re.compile(r"^[0-9a-f]{40}$")
TREE_RE = re.compile(r"^[0-9a-f]{40}$")


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
    if data.get("kind") != "native-cli-expansion-level-d":
        errors.append("kind must be native-cli-expansion-level-d")
    if data.get("scope") != "aggregate-baseline-items-5.1-to-5.6":
        errors.append("scope must identify the aggregate Items 5.1–5.6 baseline")
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
        commit = source.get("commit")
        tree = source.get("tree")
        if not isinstance(commit, str) or not COMMIT_RE.fullmatch(commit):
            errors.append("source.commit must be a full git SHA")
        elif expected_commit is not None and commit != expected_commit:
            errors.append("source.commit does not match the expected source identity")
        if not isinstance(tree, str) or not TREE_RE.fullmatch(tree):
            errors.append("source.tree must be a full git tree SHA")
        elif expected_tree is not None and tree != expected_tree:
            errors.append("source.tree does not match the expected source identity")
    tests = data.get("tests")
    if not isinstance(tests, list):
        return [*errors, "tests must be an array"]
    names = []
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
    print(f"PASS: native Level D manifest valid for {args.manifest}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

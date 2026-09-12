#!/usr/bin/env python3
"""Protect release notes for tagged versions."""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path


RELEASE_NOTES_RE = re.compile(r"^RELEASE_NOTES_v(?P<version>\d+\.\d+\.\d+)\.md$")

def run_git(root: Path, args: list[str]) -> list[str]:
    result = subprocess.run(
        ["git", *args],
        cwd=root,
        check=True,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    return [line for line in result.stdout.splitlines() if line]


def git_changed_files(root: Path, base: str | None) -> list[str]:
    if base:
        return run_git(root, ["diff", "--name-only", "--diff-filter=ACMRD", base, "HEAD"])

    files: set[str] = set()
    for args in (
        ["diff", "--name-only", "--diff-filter=ACMRD"],
        ["diff", "--cached", "--name-only", "--diff-filter=ACMRD"],
        ["ls-files", "--others", "--exclude-standard"],
    ):
        files.update(run_git(root, args))
    return sorted(files)


def release_note_version(path: str) -> str | None:
    match = RELEASE_NOTES_RE.match(Path(path).name)
    if not match:
        return None
    if Path(path).parent != Path("."):
        return None
    return match.group("version")


def git_tags(root: Path) -> set[str]:
    return set(run_git(root, ["tag", "--list", "v*"]))


def validate_policy(root: Path, changed_files: list[str]) -> list[str]:
    release_note_files = [path for path in changed_files if release_note_version(path)]
    if not release_note_files:
        return []

    errors: list[str] = []
    tags = git_tags(root)
    for path in release_note_files:
        version = release_note_version(path)
        assert version is not None
        tag = f"v{version}"
        if tag in tags:
            errors.append(
                f"{path}: release notes for already-tagged release `{tag}` must not be changed"
            )

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Reject changes to release notes for already-tagged versions."
    )
    parser.add_argument("paths", nargs="*", help="Changed paths to inspect")
    parser.add_argument(
        "--root",
        default=".",
        help="Repository root. Defaults to the current directory.",
    )
    parser.add_argument(
        "--base",
        help="Git revision to diff against when paths are not provided.",
    )
    args = parser.parse_args()

    root = Path(args.root).resolve()
    changed_files = args.paths or git_changed_files(root, args.base)
    normalized = [Path(path).as_posix() for path in changed_files]
    errors = validate_policy(root, normalized)

    if errors:
        for error in errors:
            print(f"FAIL: {error}", file=sys.stderr)
        return 1

    release_note_count = sum(1 for path in normalized if release_note_version(path))
    print(f"PASS: release-note policy valid for {release_note_count} release-note path(s)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
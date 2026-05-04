#!/usr/bin/env python3
"""Validate release-note updates against release workflow evidence."""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path


RELEASE_NOTES_RE = re.compile(r"^RELEASE_NOTES_v(?P<version>\d+\.\d+\.\d+)\.md$")
VERSION_IMPACT_RE = re.compile(r"^## Version Impact\s*$", re.MULTILINE)
IMPACT_CLASS_RE = re.compile(r"^[ \t]*-[ \t]*Impact class:[ \t]*([^\n]*)$", re.MULTILINE)
RELEASE_MARKERS = (
    "release notes",
    "release-bearing",
    "release workflow",
    "release prep",
    "release tag",
    "tag the release",
)
VALID_RELEASE_IMPACTS = {"patch", "minor", "major"}


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
        return run_git(root, ["diff", "--name-only", "--diff-filter=ACMR", base, "HEAD"])

    files: set[str] = set()
    for args in (
        ["diff", "--name-only", "--diff-filter=ACMR"],
        ["diff", "--cached", "--name-only", "--diff-filter=ACMR"],
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
    try:
        return set(run_git(root, ["tag", "--list", "v*"]))
    except subprocess.CalledProcessError:
        return set()


def is_evidence_path(path: str) -> bool:
    if not path.endswith(".md"):
        return False
    if release_note_version(path) is not None:
        return False
    return path.startswith(("documentation/", "dev-docs/"))


def read_text(root: Path, path: str) -> str:
    try:
        return (root / path).read_text(encoding="utf-8")
    except (FileNotFoundError, UnicodeDecodeError):
        return ""


def has_release_version_impact(text: str) -> bool:
    if not VERSION_IMPACT_RE.search(text):
        return False
    match = IMPACT_CLASS_RE.search(text)
    if not match:
        return False
    impact_class = match.group(1).strip().lower()
    if impact_class not in VALID_RELEASE_IMPACTS:
        return False
    lower_text = text.lower()
    return any(marker in lower_text for marker in RELEASE_MARKERS)


def validate_policy(root: Path, changed_files: list[str]) -> list[str]:
    release_note_files = [path for path in changed_files if release_note_version(path)]
    if not release_note_files:
        return []

    errors: list[str] = []
    tags = git_tags(root)
    evidence_files = [path for path in changed_files if is_evidence_path(path)]
    evidence_texts = {path: read_text(root, path) for path in evidence_files}
    valid_evidence = [
        path for path, text in evidence_texts.items() if has_release_version_impact(text)
    ]

    for path in release_note_files:
        version = release_note_version(path)
        assert version is not None
        tag = f"v{version}"
        if tag in tags:
            errors.append(
                f"{path}: release notes for already-tagged release `{tag}` must not be changed"
            )

    if not valid_evidence:
        errors.append(
            "release-note changes require changed Markdown evidence with `## Version Impact`, "
            "impact class patch/minor/major, and release workflow rationale"
        )

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Check that release-note updates are tied to release workflow evidence."
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
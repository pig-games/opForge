#!/usr/bin/env python3
"""Validate that generated reference updates are explicitly scoped and evidenced."""

from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path


REFERENCE_PREFIXES = (
    "examples/reference/",
    "crates/opforge-asm/tests/goldens/",
)
EVIDENCE_SUFFIXES = {".md", ".txt"}
EVIDENCE_MARKERS = (
    "opforge_update_reference=1",
    "update reference",
    "updated reference",
    "reference refresh",
    "reference drift",
    "refreshed",
    "regenerated",
    "golden",
    "fixture",
)


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


def is_reference_path(path: str) -> bool:
    return any(path.startswith(prefix) for prefix in REFERENCE_PREFIXES)


def is_evidence_path(path: str) -> bool:
    suffix = Path(path).suffix.lower()
    if suffix not in EVIDENCE_SUFFIXES:
        return False
    return path.startswith(("documentation/", "dev-docs/", "templates/"))


def artifact_tokens(reference_path: str) -> list[str]:
    path = Path(reference_path)
    tokens = [reference_path, path.name, path.stem]
    if len(path.parts) >= 2:
        tokens.append("/".join(path.parts[-2:]))
    return sorted(set(token.lower() for token in tokens if token))


def read_text(root: Path, path: str) -> str:
    try:
        return (root / path).read_text(encoding="utf-8").lower()
    except (FileNotFoundError, UnicodeDecodeError):
        return ""


def evidence_mentions_reference(evidence_text: str, reference_path: str) -> bool:
    tokens = artifact_tokens(reference_path)
    return any(token in evidence_text for token in tokens)


def evidence_has_refresh_marker(evidence_text: str) -> bool:
    return any(marker in evidence_text for marker in EVIDENCE_MARKERS)


def validate_scope(root: Path, changed_files: list[str]) -> list[str]:
    reference_files = [path for path in changed_files if is_reference_path(path)]
    if not reference_files:
        return []

    evidence_files = [path for path in changed_files if is_evidence_path(path)]
    evidence_texts = {path: read_text(root, path) for path in evidence_files}
    errors: list[str] = []

    if not evidence_files:
        errors.append(
            "reference/golden outputs changed but no Markdown evidence artifact changed"
        )

    for reference_path in reference_files:
        matching_evidence = [
            path
            for path, text in evidence_texts.items()
            if evidence_mentions_reference(text, reference_path)
        ]

        if not matching_evidence:
            errors.append(
                f"{reference_path}: missing changed Markdown evidence naming this reference/golden artifact"
            )
            continue

        if not any(evidence_has_refresh_marker(evidence_texts[path]) for path in matching_evidence):
            errors.append(
                f"{reference_path}: evidence must include an intentional reference/golden refresh marker"
            )

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Check that generated examples/reference and checked-in golden updates "
            "are explicitly named in changed workflow evidence."
        )
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
    errors = validate_scope(root, normalized)

    if errors:
        for error in errors:
            print(f"FAIL: {error}", file=sys.stderr)
        return 1

    reference_count = sum(1 for path in normalized if is_reference_path(path))
    print(f"PASS: reference update scope valid for {reference_count} reference/golden path(s)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
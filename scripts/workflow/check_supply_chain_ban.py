#!/usr/bin/env python3
"""Enforce repository-level supply-chain bans with a deterministic scan."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path


DEFAULT_FORBIDDEN = ["litellm"]
SKIP_DIRS = {
    ".git",
    ".venv",
    "target",
    "worktrees",
    "__pycache__",
    "node_modules",
}
SKIP_FILES = {
    Path("scripts/workflow/check_supply_chain_ban.py"),
}
POLICY_CONTEXT_WORDS = {
    "ban",
    "banned",
    "forbid",
    "forbids",
    "forbidden",
    "hard stop",
    "never",
    "do not",
    "must not",
    "security issue",
    "appears to require",
    "discovered",
}
SENSITIVE_SUFFIXES = {
    ".toml",
    ".lock",
    ".sh",
    ".py",
    ".rs",
    ".yml",
    ".yaml",
    ".json",
}


def is_probably_text(path: Path) -> bool:
    try:
        sample = path.read_bytes()[:4096]
    except OSError:
        return False
    return b"\0" not in sample


def iter_files(root: Path):
    for path in root.rglob("*"):
        if any(part in SKIP_DIRS for part in path.relative_to(root).parts):
            continue
        if not path.is_file():
            continue
        relative = path.relative_to(root)
        if relative in SKIP_FILES:
            continue
        if not is_probably_text(path):
            continue
        yield path


def is_policy_context(path: Path, line: str, nearby_lines: list[str]) -> bool:
    lower_line = line.lower()
    if path.suffix.lower() in SENSITIVE_SUFFIXES:
        return False
    context = "\n".join(nearby_lines).lower()
    return any(word in lower_line or word in context for word in POLICY_CONTEXT_WORDS)


def scan(root: Path, forbidden_tokens: list[str]) -> list[str]:
    errors: list[str] = []
    lowered_tokens = [token.lower() for token in forbidden_tokens]

    for path in iter_files(root):
        relative = path.relative_to(root)
        try:
            lines = path.read_text(encoding="utf-8").splitlines()
        except UnicodeDecodeError:
            continue

        for line_number, line in enumerate(lines, start=1):
            lower_line = line.lower()
            for token in lowered_tokens:
                if token not in lower_line:
                    continue
                start = max(0, line_number - 3)
                end = min(len(lines), line_number + 2)
                if is_policy_context(path, line, lines[start:end]):
                    continue
                errors.append(
                    f"{relative}:{line_number}: forbidden supply-chain token `{token}`"
                )

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Scan repository files for forbidden supply-chain dependencies or guidance."
    )
    parser.add_argument(
        "--root",
        default=".",
        help="Repository root to inspect. Defaults to the current directory.",
    )
    parser.add_argument(
        "--forbid",
        action="append",
        default=[],
        help="Additional forbidden token to scan for. Can be repeated.",
    )
    args = parser.parse_args()

    root = Path(args.root).resolve()
    forbidden_tokens = DEFAULT_FORBIDDEN + args.forbid
    errors = scan(root, forbidden_tokens)
    if errors:
        for error in errors:
            print(f"FAIL: {error}", file=sys.stderr)
        return 1

    print("PASS: no forbidden supply-chain tokens found outside policy context")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
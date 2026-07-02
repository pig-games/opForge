#!/usr/bin/env python3
"""Shared deterministic inputs for native-porting workflow checks."""

from __future__ import annotations

import subprocess
from pathlib import Path


NATIVE_PREFIX = "native/motorola68000/"


def git_paths(root: Path, staged: bool) -> list[str]:
    args = ["git", "diff", "--name-only", "--diff-filter=ACMR"]
    if staged:
        args.insert(2, "--cached")
    result = subprocess.run(args, cwd=root, check=True, text=True, capture_output=True)
    return sorted(filter(None, result.stdout.splitlines()))


def selected_paths(root: Path, paths: list[str], staged: bool) -> list[str]:
    return sorted({Path(path).as_posix() for path in (paths or git_paths(root, staged))})


def read_text(root: Path, relative: str) -> str:
    try:
        return (root / relative).read_text(encoding="utf-8")
    except (FileNotFoundError, UnicodeDecodeError):
        return ""


def native_asm_paths(paths: list[str]) -> list[str]:
    return [
        path
        for path in paths
        if path.startswith(NATIVE_PREFIX) and Path(path).suffix.lower() in {".asm", ".s", ".i"}
    ]

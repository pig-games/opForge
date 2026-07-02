#!/usr/bin/env python3
"""Validate native debug-contract definitions, documentation, and macro use."""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

from native_porting_common import read_text

ID_RE = re.compile(r"^\s*(CONTRACT_[A-Z0-9_]+)\s*=\s*([^\s;]+)", re.M)
USE_RE = re.compile(r"\b(CONTRACT_[A-Z0-9_]+)\b")
APPROVED = {"DEBUG_ASSERT_SPAN_IN_TEXT", "DEBUG_ASSERT_NO_BUFFER_OVERLAP"}
REQUIRED = ("Rust reference:", "Native boundary:", "Condition:", "Failure meaning:", "Stability:")


def validate_contracts(ids_text: str, docs_text: str, asm_texts: dict[str, str]) -> list[str]:
    errors: list[str] = []
    definitions: dict[str, str] = {}
    values: dict[str, str] = {}
    for name, value in ID_RE.findall(ids_text):
        if name in definitions:
            errors.append(f"duplicate contract name `{name}`")
        if value.lower() in values:
            errors.append(f"duplicate contract value `{value}` for `{name}` and `{values[value.lower()]}`")
        definitions[name] = value
        values[value.lower()] = name
    used = set()
    for path, text in asm_texts.items():
        used.update(USE_RE.findall(text))
        for macro in re.findall(r"(?m)^\s*\.(DEBUG_ASSERT_[A-Z0-9_]+)\b", text):
            if macro not in APPROVED:
                errors.append(f"{path}: unapproved assert macro `{macro}`")
    for name in sorted(used - definitions.keys()):
        errors.append(f"used contract `{name}` is not defined")
    for name in sorted(definitions):
        match = re.search(
            rf"(?ms)^###\s+`?{re.escape(name)}`?\s*$([\s\S]*?)(?=^###\s+|\Z)", docs_text
        )
        if not match:
            errors.append(f"defined contract `{name}` is not documented")
            continue
        block = match.group(1)
        for field in REQUIRED:
            if field not in block:
                errors.append(f"{name}: documentation requires `{field}`")
        stability = re.search(r"Stability:\s*([a-z-]+)", block, re.I)
        if stability and stability.group(1).lower() not in {"stable", "transitional", "diagnostic-only"}:
            errors.append(f"{name}: invalid stability tag `{stability.group(1)}`")
    return errors


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", default=".")
    parser.add_argument("--staged", action="store_true")
    args = parser.parse_args()
    root = Path(args.root).resolve()
    ids = read_text(root, "native/motorola68000/amigaos/debug/debug_contract_ids.asm")
    docs = read_text(root, "documentation/architecture/native-debug-contracts.md")
    asm = {
        path.relative_to(root).as_posix(): path.read_text(encoding="utf-8")
        for path in sorted((root / "native/motorola68000").rglob("*"))
        if path.suffix.lower() in {".asm", ".s", ".i"}
    }
    errors = validate_contracts(ids, docs, asm)
    for error in errors:
        print(f"FAIL: {error}", file=sys.stderr)
    if errors:
        return 1
    print(f"PASS: {len(ID_RE.findall(ids))} native contracts are canonical")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

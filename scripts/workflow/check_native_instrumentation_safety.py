#!/usr/bin/env python3
"""Reject unsafe native assembly instrumentation."""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

from native_porting_common import native_asm_paths, read_text, selected_paths

APPROVED_MACROS = {
    "DEBUG_ASSERT_SPAN_IN_TEXT",
    "DEBUG_ASSERT_NO_BUFFER_OVERLAP",
    "DEBUG_EVENT_U32X4",
}
FLAG_SETTER = re.compile(r"^\s*(?:cmp|tst|add|sub|and|or)\w*(?:\.\w+)?\b", re.I)
BRANCH = re.compile(r"^\s*b(?:eq|ne|cc|cs|lt|le|gt|ge|hi|ls|mi|pl|vc|vs)\b", re.I)
RAW_CALL = re.compile(r"\b(?:jsr|bsr(?:\.\w+)?)\s+.*(?:debug|diag|print)", re.I)
PROHIBITED = re.compile(r"\b(?:LastErrorBuffer|RequestBuffer|ServiceBuffer)\b", re.I)


def validate_text(path: str, text: str) -> list[str]:
    if "/debug/" in path:
        return []
    errors: list[str] = []
    lines = text.splitlines()
    for index, line in enumerate(lines):
        if RAW_CALL.search(line):
            errors.append(f"{path}:{index + 1}: raw debug/diagnostic call is forbidden")
        if PROHIBITED.search(line) and ("debug" in line.lower() or "diag" in line.lower()):
            errors.append(f"{path}:{index + 1}: instrumentation touches a prohibited buffer")
        label = re.match(r"^\s*([A-Za-z_.$?][\w.$?]*(?:Debug|Diag)[\w.$?]*)\s*(?:$|\.block)", line, re.I)
        if label:
            errors.append(f"{path}:{index + 1}: unknown instrumentation label `{label.group(1)}`")
        macro = re.search(r"\.?(DEBUG_(?:ASSERT|EVENT)_[A-Z0-9_]+)", line)
        if macro and macro.group(1) not in APPROVED_MACROS:
            errors.append(f"{path}:{index + 1}: unapproved instrumentation macro `{macro.group(1)}`")
        if FLAG_SETTER.search(line):
            cursor = index + 1
            seen_macro = None
            while cursor < len(lines) and cursor <= index + 4:
                candidate = lines[cursor]
                found = re.search(r"\.?(DEBUG_(?:ASSERT|EVENT)_[A-Z0-9_]+)", candidate)
                if found:
                    seen_macro = found.group(1)
                if BRANCH.search(candidate):
                    if seen_macro and seen_macro not in APPROVED_MACROS:
                        errors.append(
                            f"{path}:{cursor + 1}: unsafe instrumentation between flag setter and branch"
                        )
                    break
                cursor += 1
    return errors


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("paths", nargs="*")
    parser.add_argument("--root", default=".")
    parser.add_argument("--staged", action="store_true")
    args = parser.parse_args()
    root = Path(args.root).resolve()
    errors: list[str] = []
    for path in native_asm_paths(selected_paths(root, args.paths, args.staged)):
        errors.extend(validate_text(path, read_text(root, path)))
    for error in errors:
        print(f"FAIL: {error}", file=sys.stderr)
    if errors:
        return 1
    print("PASS: native instrumentation safety checks passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

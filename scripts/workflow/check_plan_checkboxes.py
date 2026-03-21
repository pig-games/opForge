#!/usr/bin/env python3
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path


CHECKBOX_RE = re.compile(r"^\s*-\s\[( |x|X)\]\s+")
WORK_ITEMS_HEADER = "## Work Items"


def analyze_plan(path: Path) -> list[str]:
    text = path.read_text(encoding="utf-8")
    lines = text.splitlines()
    errors: list[str] = []

    if "- Source:" not in text:
        errors.append("missing `- Source:` in metadata")
    if "- Mode:" not in text:
        errors.append("missing `- Mode:` in metadata")

    in_work_items = False
    checkbox_indices = []
    for i, line in enumerate(lines):
        if line.strip() == WORK_ITEMS_HEADER:
            in_work_items = True
            continue
        if in_work_items and line.startswith("## "):
            in_work_items = False
        if in_work_items and CHECKBOX_RE.match(line):
            checkbox_indices.append(i)
    if not checkbox_indices:
        errors.append("no checkbox work items found")
        return errors

    for index in checkbox_indices:
        window = lines[index + 1 : index + 8]
        if not any("Validation:" in line for line in window):
            errors.append(f"checkbox at line {index + 1} missing nearby `Validation:`")
        if not any("Definition of done:" in line for line in window):
            errors.append(
                f"checkbox at line {index + 1} missing nearby `Definition of done:`"
            )

    checked = [i for i in checkbox_indices if "[x]" in lines[i].lower()]
    if checked and "## Milestones" not in text and "## Work Items" not in text:
        errors.append("checked work exists but expected plan sections are missing")

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Check branch-local plan documents for basic checkbox discipline."
    )
    parser.add_argument("paths", nargs="+", help="Plan markdown files to inspect")
    args = parser.parse_args()

    had_errors = False
    for raw_path in args.paths:
        path = Path(raw_path)
        if not path.exists():
            print(f"{path}: missing file", file=sys.stderr)
            had_errors = True
            continue
        errors = analyze_plan(path)
        if errors:
            had_errors = True
            print(f"{path}: FAIL")
            for error in errors:
                print(f"  - {error}")
        else:
            print(f"{path}: PASS")

    return 1 if had_errors else 0


if __name__ == "__main__":
    raise SystemExit(main())

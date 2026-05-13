#!/usr/bin/env python3
"""Validate branch-local specification artifact structure."""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path


REQUIRED_HEADINGS = [
    "## Summary",
    "## Problem",
    "## Goals",
    "## Non-Goals",
    "## Invariants / Constraints",
    "## Behavioral Contract",
    "## Boundary Cases",
    "## Acceptance Criteria",
    "## Validation Expectations",
]

CHECKBOX_RE = re.compile(r"^\s*-\s\[[ xX]\]\s+", re.MULTILINE)


def find_section(text: str, heading: str) -> tuple[int, int] | None:
    start = text.find(heading)
    if start == -1:
        return None
    next_heading = text.find("\n## ", start + len(heading))
    if next_heading == -1:
        return (start, len(text))
    return (start, next_heading)


def validate_spec(path: Path) -> list[str]:
    errors: list[str] = []
    text = path.read_text(encoding="utf-8")

    if path.suffix.lower() != ".md":
        errors.append(f"{path}: specification must be a Markdown file")

    for heading in REQUIRED_HEADINGS:
        if heading not in text:
            errors.append(f"{path}: missing required heading `{heading}`")

    goals_span = find_section(text, "## Goals")
    if goals_span is not None:
        goals_text = text[goals_span[0] : goals_span[1]]
        if not CHECKBOX_RE.search(goals_text):
            errors.append(f"{path}: goals section must contain at least one checkbox item")

    acceptance_span = find_section(text, "## Acceptance Criteria")
    if acceptance_span is not None:
        acceptance_text = text[acceptance_span[0] : acceptance_span[1]]
        if not CHECKBOX_RE.search(acceptance_text):
            errors.append(
                f"{path}: acceptance criteria section must contain at least one checkbox item"
            )

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description="Validate a specification artifact.")
    parser.add_argument("paths", nargs="+", help="Markdown specification paths")
    args = parser.parse_args()

    all_errors: list[str] = []
    for raw_path in args.paths:
        path = Path(raw_path)
        if not path.exists():
            all_errors.append(f"{path}: file not found")
            continue
        all_errors.extend(validate_spec(path))

    if all_errors:
        for error in all_errors:
            print(f"FAIL: {error}", file=sys.stderr)
        return 1

    for raw_path in args.paths:
        print(f"PASS: {raw_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

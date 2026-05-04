#!/usr/bin/env python3
"""Validate required Version Impact sections for workflow artifacts."""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path


VALID_IMPACT_CLASSES = {"none", "patch", "minor", "major"}
REQUIRED_FIELDS = [
    "Affected component(s):",
    "Impact class:",
    "Owned contract:",
    "Rationale:",
]
PLACEHOLDER_VALUES = {"", "tbd", "todo", "n/a?", "?"}


def find_section(text: str, heading: str) -> str | None:
    start = text.find(heading)
    if start == -1:
        return None
    next_heading = text.find("\n## ", start + len(heading))
    if next_heading == -1:
        return text[start:]
    return text[start:next_heading]


def field_value(section: str, field_name: str) -> str | None:
    pattern = re.compile(rf"^[ \t]*-[ \t]*{re.escape(field_name)}[ \t]*([^\n]*)$", re.MULTILINE)
    match = pattern.search(section)
    if not match:
        return None
    return match.group(1).strip()


def is_placeholder(value: str | None) -> bool:
    if value is None:
        return True
    return value.strip().lower() in PLACEHOLDER_VALUES


def validate_version_impact(path: Path) -> list[str]:
    errors: list[str] = []
    text = path.read_text(encoding="utf-8")
    section = find_section(text, "## Version Impact")

    if section is None:
        return [f"{path}: missing required heading `## Version Impact`"]

    for field_name in REQUIRED_FIELDS:
        value = field_value(section, field_name)
        if is_placeholder(value):
            errors.append(f"{path}: missing non-empty `{field_name}` field")

    impact_class = field_value(section, "Impact class:")
    if impact_class and impact_class.lower() not in VALID_IMPACT_CLASSES:
        errors.append(
            f"{path}: invalid impact class `{impact_class}`; expected one of {sorted(VALID_IMPACT_CLASSES)}"
        )

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Validate Version Impact sections in implementation/remediation plans and release reviews."
    )
    parser.add_argument("paths", nargs="+", help="Markdown files to inspect")
    args = parser.parse_args()

    all_errors: list[str] = []
    for raw_path in args.paths:
        path = Path(raw_path)
        if not path.exists():
            all_errors.append(f"{path}: file not found")
            continue
        all_errors.extend(validate_version_impact(path))

    if all_errors:
        for error in all_errors:
            print(f"FAIL: {error}", file=sys.stderr)
        return 1

    for raw_path in args.paths:
        print(f"PASS: {raw_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
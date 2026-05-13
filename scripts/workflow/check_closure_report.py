#!/usr/bin/env python3
"""Validate branch-local finding closure report structure."""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path


REQUIRED_HEADINGS = [
    "## Finding",
    "## Claimed Fix",
    "## Validation Evidence",
    "## Closure Status",
]
VALID_STATUSES = {
    "fixed",
    "partially fixed",
    "not fixed",
    "superseded",
    "deferred",
}


def field_value(text: str, field_name: str) -> str | None:
    pattern = re.compile(rf"^[ \t]*-[ \t]*{re.escape(field_name)}[ \t]*([^\n]*)$", re.MULTILINE)
    match = pattern.search(text)
    if not match:
        return None
    value = match.group(1).strip()
    return value or None


def validate_closure(path: Path) -> list[str]:
    errors: list[str] = []
    text = path.read_text(encoding="utf-8")

    if path.suffix.lower() != ".md":
        errors.append(f"{path}: closure report must be a Markdown file")

    for heading in REQUIRED_HEADINGS:
        if heading not in text:
            errors.append(f"{path}: missing required heading `{heading}`")

    finding_id = field_value(text, "ID:")
    if not finding_id:
        errors.append(f"{path}: missing non-empty finding `ID` field")

    original_summary = field_value(text, "Original summary:")
    if not original_summary:
        errors.append(f"{path}: missing non-empty `Original summary` field")

    plan_item = field_value(text, "Plan item:")
    if not plan_item:
        errors.append(f"{path}: missing non-empty `Plan item` field")

    implementation = field_value(text, "Implementation slice or commit:")
    changed_files = field_value(text, "Changed files:")
    if not implementation and not changed_files:
        errors.append(
            f"{path}: closure report must include changed files or implementation commit evidence"
        )

    command = field_value(text, "Command or check:")
    result = field_value(text, "Result:")
    if not command or not result:
        errors.append(f"{path}: missing validation command/check and result evidence")

    status = field_value(text, "Status:")
    if not status:
        errors.append(f"{path}: missing non-empty closure `Status` field")
    elif status.lower() not in VALID_STATUSES:
        errors.append(
            f"{path}: invalid closure status `{status}`; expected one of {sorted(VALID_STATUSES)}"
        )

    rationale = field_value(text, "Closure rationale:")
    if not rationale:
        errors.append(f"{path}: missing non-empty `Closure rationale` field")

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description="Validate a finding closure report artifact.")
    parser.add_argument("paths", nargs="+", help="Markdown closure report paths")
    args = parser.parse_args()

    all_errors: list[str] = []
    for raw_path in args.paths:
        path = Path(raw_path)
        if not path.exists():
            all_errors.append(f"{path}: file not found")
            continue
        all_errors.extend(validate_closure(path))

    if all_errors:
        for error in all_errors:
            print(f"FAIL: {error}", file=sys.stderr)
        return 1

    for raw_path in args.paths:
        print(f"PASS: {raw_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

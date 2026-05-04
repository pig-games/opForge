#!/usr/bin/env python3
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path


CHECKBOX_RE = re.compile(r"^\s*-\s\[( |x|X)\]\s+")
WORK_ITEMS_HEADER = "## Work Items"
REQUIRED_HEADINGS = [
    "## Metadata",
    "## Objective",
    "## Constraints",
    "## Work Items",
    "## Blocking Rules",
]
REQUIRED_WORK_ITEM_FIELDS = [
    "Source requirement or finding IDs:",
    "Expected files:",
    "Full quality gates:",
    "Plan-compliance review evidence:",
    "Commit outcome:",
    "Definition of done:",
]
BLOCKING_RULE_PATTERNS = [
    ("AGENTS.md rules remain binding", re.compile(r"AGENTS\.md.+rules", re.IGNORECASE | re.DOTALL)),
    ("all quality gates pass before commit", re.compile(r"quality gates.+before.+commit", re.IGNORECASE | re.DOTALL)),
    ("plan-compliance-reviewer passes before commit", re.compile(r"plan-compliance-reviewer.+before.+commit", re.IGNORECASE | re.DOTALL)),
    ("each work item or phase ends in a new commit", re.compile(r"each.+(?:work item|phase).+commit", re.IGNORECASE | re.DOTALL)),
]


def has_nonempty_field(text: str, field_name: str) -> bool:
    pattern = re.compile(rf"^\s*-\s*{re.escape(field_name)}\s*(.+?)\s*$", re.MULTILINE)
    match = pattern.search(text)
    return bool(match and match.group(1).strip())


def block_has_field(block: list[str], field_name: str) -> bool:
    needle = field_name.lower()
    return any(needle in line.lower() for line in block)


def analyze_plan(path: Path) -> list[str]:
    text = path.read_text(encoding="utf-8")
    lines = text.splitlines()
    errors: list[str] = []

    for heading in REQUIRED_HEADINGS:
        if heading not in text:
            errors.append(f"missing required heading `{heading}`")

    if not has_nonempty_field(text, "Source:"):
        errors.append("missing non-empty `- Source:` in metadata")
    if not has_nonempty_field(text, "Mode:"):
        errors.append("missing non-empty `- Mode:` in metadata")

    for description, pattern in BLOCKING_RULE_PATTERNS:
        if not pattern.search(text):
            errors.append(f"missing blocking rule: {description}")

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

    for checkbox_number, index in enumerate(checkbox_indices):
        next_checkbox = (
            checkbox_indices[checkbox_number + 1]
            if checkbox_number + 1 < len(checkbox_indices)
            else len(lines)
        )
        block_end = next_checkbox
        for line_index in range(index + 1, next_checkbox):
            if lines[line_index].startswith("## "):
                block_end = line_index
                break
        block = lines[index + 1 : block_end]

        for field_name in REQUIRED_WORK_ITEM_FIELDS:
            if not block_has_field(block, field_name):
                errors.append(
                    f"checkbox at line {index + 1} missing `{field_name}` in work item block"
                )

    checked = [i for i in checkbox_indices if "[x]" in lines[i].lower()]
    if checked and "## Milestones" not in text and "## Work Items" not in text:
        errors.append("checked work exists but expected plan sections are missing")

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Check branch-local plan documents for required workflow structure."
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

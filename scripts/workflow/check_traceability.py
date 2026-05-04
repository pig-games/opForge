#!/usr/bin/env python3
"""Run lightweight deterministic traceability checks on workflow artifacts."""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path


FINDING_ID_RE = re.compile(r"RVW-\d{4}-\d{2}-\d{2}-\d{3,}")
CHECKBOX_RE = re.compile(r"^\s*-\s\[(?: |x|X)\]\s+", re.MULTILINE)
EMPTY_VALUES = {"", "tbd", "todo", "n/a", "none", "?"}


def field_value(text: str, field_name: str) -> str | None:
    pattern = re.compile(rf"^[ \t]*-[ \t]*{re.escape(field_name)}[ \t]*([^\n]*)$", re.MULTILINE)
    match = pattern.search(text)
    if not match:
        return None
    return match.group(1).strip().strip("`")


def find_section(text: str, heading: str) -> str | None:
    start = text.find(heading)
    if start == -1:
        return None
    next_heading = text.find("\n## ", start + len(heading))
    if next_heading == -1:
        return text[start:]
    return text[start:next_heading]


def is_empty(value: str | None) -> bool:
    return value is None or value.strip().lower() in EMPTY_VALUES


def review_finding_ids(root: Path) -> set[str]:
    ids: set[str] = set()
    for pattern in ("documentation/reviews/*.md", "dev-docs/reviews/*.md", "dev-docs/**/reviews/*.md"):
        for path in root.glob(pattern):
            try:
                ids.update(FINDING_ID_RE.findall(path.read_text(encoding="utf-8")))
            except UnicodeDecodeError:
                continue
    return ids


def validate_plan(path: Path) -> list[str]:
    errors: list[str] = []
    text = path.read_text(encoding="utf-8")
    work_items_text = find_section(text, "## Work Items")
    if work_items_text is None:
        return errors

    lines = work_items_text.splitlines()
    checkbox_indices = [i for i, line in enumerate(lines) if CHECKBOX_RE.match(line)]

    for checkbox_number, index in enumerate(checkbox_indices):
        next_index = checkbox_indices[checkbox_number + 1] if checkbox_number + 1 < len(checkbox_indices) else len(lines)
        block = "\n".join(lines[index + 1 : next_index])
        source_ids = field_value(block, "Source requirement or finding IDs:")
        if is_empty(source_ids):
            errors.append(f"{path}: checkbox at line {index + 1} has empty source requirement/finding IDs")

    return errors


def validate_closure(path: Path, known_finding_ids: set[str]) -> list[str]:
    errors: list[str] = []
    text = path.read_text(encoding="utf-8")
    finding_id = field_value(text, "ID:")
    if is_empty(finding_id):
        errors.append(f"{path}: missing finding ID")
    elif finding_id not in known_finding_ids:
        errors.append(f"{path}: finding ID `{finding_id}` was not found in review artifacts")

    plan_item = field_value(text, "Plan item:")
    if is_empty(plan_item):
        errors.append(f"{path}: missing plan item trace")

    command = field_value(text, "Command or check:")
    result = field_value(text, "Result:")
    if is_empty(command) or is_empty(result):
        errors.append(f"{path}: missing validation command/result trace")

    implementation = field_value(text, "Implementation slice or commit:")
    changed_files = field_value(text, "Changed files:")
    if is_empty(implementation) and is_empty(changed_files):
        errors.append(f"{path}: missing implementation slice, commit, or changed-file trace")

    return errors


def validate_path(root: Path, path: Path, known_finding_ids: set[str]) -> list[str]:
    try:
        normalized = path.resolve().relative_to(root).as_posix()
    except ValueError:
        normalized = path.as_posix()
    if normalized.startswith("documentation/plans/") and normalized.endswith(".md"):
        return validate_plan(path)
    if "closure" in path.name.lower() and normalized.endswith(".md"):
        return validate_closure(path, known_finding_ids)
    return []


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Check deterministic traceability links in plans and finding closure reports."
    )
    parser.add_argument("paths", nargs="+", help="Plan or closure report paths to inspect")
    parser.add_argument(
        "--root",
        default=".",
        help="Repository root used to locate review artifacts. Defaults to current directory.",
    )
    args = parser.parse_args()

    root = Path(args.root).resolve()
    known_finding_ids = review_finding_ids(root)
    all_errors: list[str] = []

    for raw_path in args.paths:
        path = Path(raw_path)
        if not path.exists():
            all_errors.append(f"{path}: file not found")
            continue
        all_errors.extend(validate_path(root, path, known_finding_ids))

    if all_errors:
        for error in all_errors:
            print(f"FAIL: {error}", file=sys.stderr)
        return 1

    for raw_path in args.paths:
        print(f"PASS: {raw_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
#!/usr/bin/env python3
"""Validate branch-local review report structure."""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path


REQUIRED_HEADINGS = [
    "## Scope",
    "## Findings",
    "## Testing Gaps",
    "## Residual Risks",
    "## Brief Summary",
]

FINDING_HEADER_RE = re.compile(r"^###\s+(RVW-\d{4}-\d{2}-\d{2}-\d{3,})\s*$", re.MULTILINE)
FIELD_PATTERNS = {
    "Severity": re.compile(r"^- Severity:\s*(.+?)\s*$", re.MULTILINE),
    "File": re.compile(r"^- File:\s*(.+?)\s*$", re.MULTILINE),
    "Why it matters": re.compile(r"^- Why it matters:\s*(.+?)\s*$", re.MULTILINE),
    "Fix direction": re.compile(r"^- Fix direction(?:\s*\([^)]*\))?:\s*(.+?)\s*$", re.MULTILINE),
}
VALID_SEVERITIES = {"critical", "high", "medium", "low"}
FORBIDDEN_FIX_PATTERNS = [
    re.compile(r"\boption\s+\d+\b", re.IGNORECASE),
    re.compile(r"\beither\b.+\bor\b", re.IGNORECASE),
    re.compile(r"\balternatively\b", re.IGNORECASE),
    re.compile(r"\bone approach would be\b", re.IGNORECASE),
    re.compile(r"\banother approach would be\b", re.IGNORECASE),
]


def find_section(text: str, heading: str) -> tuple[int, int] | None:
    start = text.find(heading)
    if start == -1:
        return None
    next_heading = text.find("\n## ", start + len(heading))
    if next_heading == -1:
        return (start, len(text))
    return (start, next_heading)


def validate_review(path: Path) -> list[str]:
    errors: list[str] = []
    text = path.read_text(encoding="utf-8")

    if path.suffix.lower() != ".md":
        errors.append(f"{path}: review report must be a Markdown file")

    if "## Open Questions" in text:
        errors.append(f"{path}: forbidden section `## Open Questions` present")

    for heading in REQUIRED_HEADINGS:
        if heading not in text:
            errors.append(f"{path}: missing required heading `{heading}`")

    findings_span = find_section(text, "## Findings")
    if findings_span is None:
        return errors

    findings_text = text[findings_span[0] : findings_span[1]]
    finding_headers = list(FINDING_HEADER_RE.finditer(findings_text))

    if not finding_headers:
        if "No material findings." not in findings_text:
            errors.append(
                f"{path}: findings section must contain at least one stable finding ID or `No material findings.`"
            )
        return errors

    for index, header_match in enumerate(finding_headers):
        finding_id = header_match.group(1)
        block_start = header_match.start()
        block_end = (
            finding_headers[index + 1].start()
            if index + 1 < len(finding_headers)
            else len(findings_text)
        )
        block = findings_text[block_start:block_end]

        for field_name, pattern in FIELD_PATTERNS.items():
            match = pattern.search(block)
            if not match:
                errors.append(f"{path}: {finding_id} missing `{field_name}` field")
                continue
            value = match.group(1).strip()
            if not value:
                errors.append(f"{path}: {finding_id} has empty `{field_name}` field")
                continue
            if field_name == "Severity" and value.lower() not in VALID_SEVERITIES:
                errors.append(
                    f"{path}: {finding_id} has invalid severity `{value}`; expected one of {sorted(VALID_SEVERITIES)}"
                )
            if field_name == "Fix direction":
                for forbidden in FORBIDDEN_FIX_PATTERNS:
                    if forbidden.search(value):
                        errors.append(
                            f"{path}: {finding_id} fix direction contains unresolved competing options: `{value}`"
                        )
                        break

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description="Validate a review report artifact.")
    parser.add_argument("paths", nargs="+", help="Markdown review report paths")
    args = parser.parse_args()

    all_errors: list[str] = []
    for raw_path in args.paths:
        path = Path(raw_path)
        if not path.exists():
            all_errors.append(f"{path}: file not found")
            continue
        all_errors.extend(validate_review(path))

    if all_errors:
        for error in all_errors:
            print(f"FAIL: {error}", file=sys.stderr)
        return 1

    for raw_path in args.paths:
        print(f"PASS: {raw_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

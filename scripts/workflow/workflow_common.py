#!/usr/bin/env python3
"""Shared helpers for opForge workflow scripts."""

from __future__ import annotations

import re
from pathlib import Path


FINDING_HEADER_RE = re.compile(
    r"^###\s+(?P<id>RVW-\d{4}-\d{2}-\d{2}-(?P<ordinal>\d{3,}))\s*$",
    re.MULTILINE,
)
FIELD_RE_TEMPLATE = r"^[ \t]*-[ \t]*{field}[ \t]*([^\n]*)$"
CHECKBOX_ITEM_RE = re.compile(r"^\s*-\s\[(?: |x|X)\]\s+(?P<text>.+?)\s*$", re.MULTILINE)
PROVENANCE_RE = re.compile(
    r"^<!--\s*workflow-provenance:\s*skill=(?P<skill>[^;]+);\s*entrypoint=(?P<entrypoint>[^>]+)\s*-->\n?",
    re.MULTILINE,
)


def repo_root() -> Path:
    return Path(__file__).resolve().parents[2]


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def write_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def workflow_provenance_comment(skill: str, entrypoint: str) -> str:
    return f"<!-- workflow-provenance: skill={skill}; entrypoint={entrypoint} -->\n"


def read_workflow_provenance(text: str) -> dict[str, str] | None:
    match = PROVENANCE_RE.search(text)
    if not match:
        return None
    return {
        "skill": match.group("skill").strip(),
        "entrypoint": match.group("entrypoint").strip(),
    }


def apply_workflow_provenance(text: str, skill: str, entrypoint: str) -> str:
    comment = workflow_provenance_comment(skill, entrypoint)
    if PROVENANCE_RE.search(text):
        return PROVENANCE_RE.sub(comment, text, count=1)
    if text.startswith("# "):
        return comment + text
    return comment + "\n" + text


def replace_title(text: str, title: str) -> str:
    return re.sub(r"^# .*$", f"# {title}", text, count=1, flags=re.MULTILINE)


def replace_field(text: str, field_name: str, value: str) -> str:
    pattern = re.compile(
        rf"^([ \t]*-[ \t]*{re.escape(field_name)}[ \t]*).*$", re.MULTILINE
    )
    return pattern.sub(rf"\1{value}", text, count=1)


def find_section(text: str, heading: str) -> str | None:
    start = text.find(heading)
    if start == -1:
        return None
    next_heading = text.find("\n## ", start + len(heading))
    if next_heading == -1:
        return text[start:]
    return text[start:next_heading]


def set_section_body(text: str, heading: str, body: str) -> str:
    start = text.find(heading)
    if start == -1:
        return text
    next_heading = text.find("\n## ", start + len(heading))
    if next_heading == -1:
        next_heading = len(text)
    replacement = f"{heading}\n\n{body.strip()}\n"
    if next_heading < len(text):
        replacement += "\n"
    return text[:start] + replacement + text[next_heading:]


def checkbox_items(section_text: str | None) -> list[str]:
    if section_text is None:
        return []
    return [match.group("text").strip() for match in CHECKBOX_ITEM_RE.finditer(section_text)]


def field_value(text: str, field_name: str) -> str | None:
    pattern = re.compile(
        FIELD_RE_TEMPLATE.format(field=re.escape(field_name)),
        re.MULTILINE,
    )
    match = pattern.search(text)
    if not match:
        return None
    value = match.group(1).strip().strip("`")
    return value or None


def review_finding_blocks(text: str) -> list[tuple[str, str]]:
    findings_section = find_section(text, "## Findings")
    if findings_section is None:
        return []

    headers = list(FINDING_HEADER_RE.finditer(findings_section))
    blocks: list[tuple[str, str]] = []
    for index, match in enumerate(headers):
        block_start = match.start()
        block_end = headers[index + 1].start() if index + 1 < len(headers) else len(findings_section)
        blocks.append((match.group("id"), findings_section[block_start:block_end]))
    return blocks


def next_review_finding_id(existing_text: str, date_text: str) -> str:
    ordinals = [
        int(match.group("ordinal"))
        for match in FINDING_HEADER_RE.finditer(existing_text)
        if match.group("id").startswith(f"RVW-{date_text}-")
    ]
    next_ordinal = max(ordinals, default=0) + 1
    return f"RVW-{date_text}-{next_ordinal:03d}"


def parse_gate_text(text: str) -> dict[str, str]:
    lines = [line.rstrip() for line in text.splitlines() if line.strip()]
    if not lines:
        return {}

    first_line = lines[0].strip()
    if first_line == "PASS":
        return {"status": "PASS"}
    if first_line.startswith("PASS:"):
        return {"status": "PASS", "summary": first_line.partition(":")[2].strip()}
    if first_line == "FAIL":
        return {"status": "FAIL"}
    if first_line.startswith("FAIL:"):
        return {"status": "FAIL", "summary": first_line.partition(":")[2].strip()}

    metadata: dict[str, str] = {}
    for line in lines:
        key, sep, value = line.partition(":")
        if not sep:
            continue
        metadata[key.strip().lower()] = value.strip()
    return metadata


def gate_status(text: str) -> str | None:
    metadata = parse_gate_text(text)
    status = metadata.get("status")
    if status is None:
        return None
    return status.upper()


def render_gate_text(
    *,
    status: str,
    gate: str,
    artifact: str,
    summary: str,
    reviewer: str | None = None,
) -> str:
    lines = [
        f"status: {status.upper()}",
        f"gate: {gate}",
        f"artifact: {artifact}",
        f"summary: {summary}",
    ]
    if reviewer:
        lines.append(f"reviewer: {reviewer}")
    return "\n".join(lines) + "\n"

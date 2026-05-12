#!/usr/bin/env python3
"""Derive a draft plan artifact from a spec or review artifact."""

from __future__ import annotations

import argparse
from pathlib import Path

from workflow_common import (
    apply_workflow_provenance,
    checkbox_items,
    field_value,
    find_section,
    read_text,
    render_gate_text,
    replace_field,
    replace_title,
    repo_root,
    review_finding_blocks,
    write_text,
)


DEFAULT_GATE = (
    "scripts/workflow/run_rust_quality_gate.sh "
    "(includes native/motorola68000 formatter enforcement) plus focused validation for the slice"
)
DEFAULT_PLAN_COMPLIANCE = "`plan-compliance-reviewer` returns `PASS` before commit"
DEFAULT_COMMIT_OUTCOME = "one commit that completes only this work item"


def relative_to_repo(path: Path) -> str:
    root = repo_root()
    try:
        return path.resolve().relative_to(root).as_posix()
    except ValueError:
        return path.as_posix()


def spec_work_items(source_path: Path, text: str) -> list[str]:
    acceptance = checkbox_items(find_section(text, "## Acceptance Criteria"))
    goals = checkbox_items(find_section(text, "## Goals"))
    items = acceptance or goals
    blocks: list[str] = []
    for index, item in enumerate(items, start=1):
        source_id = f"spec:{relative_to_repo(source_path)}#item-{index}"
        blocks.append(
            "\n".join(
                [
                    f"- [ ] {item}",
                    f"  - Source requirement or finding IDs: `{source_id}`",
                    "  - Expected files: TBD",
                    f"  - Full quality gates: {DEFAULT_GATE}",
                    f"  - Plan-compliance review evidence: {DEFAULT_PLAN_COMPLIANCE}",
                    f"  - Commit outcome: {DEFAULT_COMMIT_OUTCOME}",
                    f"  - Definition of done: {item}",
                ]
            )
        )
    return blocks or [
        "\n".join(
            [
                "- [ ] Review source artifact and define first implementation slice",
                f"  - Source requirement or finding IDs: `spec:{relative_to_repo(source_path)}`",
                "  - Expected files: TBD",
                f"  - Full quality gates: {DEFAULT_GATE}",
                f"  - Plan-compliance review evidence: {DEFAULT_PLAN_COMPLIANCE}",
                f"  - Commit outcome: {DEFAULT_COMMIT_OUTCOME}",
                "  - Definition of done: first implementation slice is concrete and bounded",
            ]
        )
    ]


def review_work_items(text: str) -> list[str]:
    blocks: list[str] = []
    for finding_id, block in review_finding_blocks(text):
        issue = field_value(block, "Issue:") or finding_id
        blocks.append(
            "\n".join(
                [
                    f"- [ ] Address {finding_id}: {issue}",
                    f"  - Source requirement or finding IDs: `{finding_id}`",
                    "  - Expected files: TBD",
                    f"  - Full quality gates: {DEFAULT_GATE}",
                    f"  - Plan-compliance review evidence: {DEFAULT_PLAN_COMPLIANCE}",
                    f"  - Commit outcome: {DEFAULT_COMMIT_OUTCOME}",
                    f"  - Definition of done: the issue described by `{finding_id}` no longer reproduces",
                ]
            )
        )
    return blocks or [
        "\n".join(
            [
                "- [ ] Review artifact contains no material findings; confirm whether a plan is still needed",
                "  - Source requirement or finding IDs: `No material findings.`",
                "  - Expected files: TBD",
                f"  - Full quality gates: {DEFAULT_GATE}",
                f"  - Plan-compliance review evidence: {DEFAULT_PLAN_COMPLIANCE}",
                f"  - Commit outcome: {DEFAULT_COMMIT_OUTCOME}",
                "  - Definition of done: scope for any follow-up work is explicit",
            ]
        )
    ]


def infer_kind(text: str) -> str:
    if "## Findings" in text:
        return "review"
    return "spec"


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Create a draft plan artifact from a spec or review artifact."
    )
    parser.add_argument("source_path", help="Spec or review artifact path")
    parser.add_argument("output_path", help="Plan path to create")
    parser.add_argument(
        "--mode",
        default="implementation",
        help="Plan mode to record. Defaults to implementation.",
    )
    parser.add_argument("--owner", help="Plan owner")
    parser.add_argument("--title", help="Plan title override")
    args = parser.parse_args()

    root = repo_root()
    source_path = (root / args.source_path).resolve()
    output_path = (root / args.output_path).resolve()
    if output_path.exists():
        raise SystemExit(f"FAIL: refusing to overwrite existing file: {output_path}")

    source_text = read_text(source_path)
    kind = infer_kind(source_text)
    template_path = root / "templates/plan-template.md"
    plan_text = read_text(template_path)

    default_title = (
        args.title
        or f"Plan Derived From {source_path.stem.replace('-', ' ').replace('_', ' ').title()}"
    )
    plan_text = replace_title(plan_text, default_title)
    plan_text = apply_workflow_provenance(
        plan_text,
        "opforge-plan-authoring",
        "derive_plan_from_artifact.py",
    )
    plan_text = replace_field(plan_text, "Source:", relative_to_repo(source_path))
    plan_text = replace_field(plan_text, "Mode:", args.mode)
    plan_text = replace_field(plan_text, "Affected component(s):", "workflow-derived implementation slice")
    plan_text = replace_field(plan_text, "Impact class:", "none")
    plan_text = replace_field(plan_text, "Owned contract:", "derived plan contract")
    plan_text = replace_field(
        plan_text,
        "Rationale:",
        "Derived draft plan scaffold; update impact details before approval if release scope changes.",
    )
    if args.owner:
        plan_text = replace_field(plan_text, "Owner:", args.owner)

    work_item_blocks = (
        review_work_items(source_text) if kind == "review" else spec_work_items(source_path, source_text)
    )
    plan_text = plan_text.replace(
        "- [ ] Item 1\n  - Source requirement or finding IDs:\n  - Expected files:\n  - Full quality gates:\n  - Plan-compliance review evidence:\n  - Commit outcome:\n  - Definition of done:\n",
        "\n\n".join(work_item_blocks) + "\n",
        1,
    )
    plan_text = plan_text.replace("- [ ] Milestone 1", "- [ ] Complete all drafted work items", 1)

    write_text(output_path, plan_text)
    gate_path = output_path.with_name(f"{output_path.name}.quality-gate.txt")
    write_text(
        gate_path,
        render_gate_text(
            status="PENDING",
            gate="plan-quality",
            artifact=relative_to_repo(output_path),
            summary="pending plan quality review",
        ),
    )

    print(f"Created draft plan {relative_to_repo(output_path)} from {relative_to_repo(source_path)}")
    print(f"Created {relative_to_repo(output_path)}.quality-gate.txt gate stub")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

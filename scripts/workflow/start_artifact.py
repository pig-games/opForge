#!/usr/bin/env python3
"""Bootstrap workflow artifacts with deterministic prefilled fields."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from workflow_common import (
    apply_workflow_provenance,
    field_value,
    read_text,
    render_gate_text,
    replace_field,
    replace_title,
    repo_root,
    review_finding_blocks,
    set_section_body,
    write_text,
)


TEMPLATES = {
    "spec": "templates/spec-template.md",
    "plan": "templates/plan-template.md",
    "review": "templates/review-report-template.md",
    "closure": "templates/finding-closure-report-template.md",
}
GATED_KINDS = {"spec", "plan", "review"}
DEFAULT_SKILLS = {
    "spec": "opforge-spec-authoring",
    "plan": "opforge-plan-authoring",
    "review": "opforge-review-reporting",
    "closure": "opforge-review-closure",
}


def relative_to_repo(path: Path) -> str:
    root = repo_root()
    try:
        return path.resolve().relative_to(root).as_posix()
    except ValueError:
        return path.as_posix()


def review_summary(review_path: Path, finding_id: str) -> str | None:
    text = read_text(review_path)
    for candidate_id, block in review_finding_blocks(text):
        if candidate_id != finding_id:
            continue
        issue = field_value(block, "Issue:")
        return issue or f"Finding {finding_id}"
    return None


def apply_prefill(text: str, args: argparse.Namespace) -> str:
    if args.title:
        text = replace_title(text, args.title)

    if args.kind == "plan":
        text = replace_field(text, "Affected component(s):", "workflow artifact draft")
        text = replace_field(text, "Impact class:", "none")
        text = replace_field(text, "Owned contract:", "workflow artifact contract")
        text = replace_field(
            text,
            "Rationale:",
            "Initial draft scaffold for implementation planning; update before approval if behavior scope changes.",
        )
        if args.source:
            text = replace_field(text, "Source:", args.source)
        if args.mode:
            text = replace_field(text, "Mode:", args.mode)
        if args.owner:
            text = replace_field(text, "Owner:", args.owner)

    if args.kind == "review":
        text = replace_field(text, "Affected component(s):", "review scope under analysis")
        text = replace_field(text, "Impact class:", "none")
        text = replace_field(text, "Owned contract:", "review reporting contract")
        text = replace_field(
            text,
            "Rationale:",
            "Initial review scaffold; update if findings reveal release-bearing impact.",
        )
        text = set_section_body(text, "## Findings", "No material findings.")
        if args.scope:
            text = set_section_body(text, "## Scope", args.scope)

    if args.kind == "closure":
        if args.finding_id:
            text = replace_field(text, "ID:", args.finding_id)
        if args.plan_item:
            text = replace_field(text, "Plan item:", args.plan_item)
        text = replace_field(
            text,
            "Implementation slice or commit:",
            "pending implementation slice trace",
        )
        text = replace_field(
            text,
            "Changed files:",
            args.changed_files or "pending changed-file trace",
        )
        text = replace_field(text, "Command or check:", "pending validation command")
        text = replace_field(text, "Result:", "pending validation result")
        text = replace_field(text, "Status:", "deferred")
        text = replace_field(text, "Residual risk:", "closure evidence still pending")
        text = replace_field(
            text,
            "Closure rationale:",
            "Closure scaffold created; reviewer evidence still pending.",
        )
        if args.review and args.finding_id:
            summary = review_summary(Path(args.review), args.finding_id)
            if summary:
                text = replace_field(text, "Original summary:", summary)

    return text


def create_gate_stub(kind: str, output_path: Path) -> None:
    gate_path = output_path.with_name(f"{output_path.name}.quality-gate.txt")
    if gate_path.exists():
        return
    gate_name = f"{kind}-quality"
    summary = f"pending {kind} quality review"
    write_text(
        gate_path,
        render_gate_text(
            status="PENDING",
            gate=gate_name,
            artifact=relative_to_repo(output_path),
            summary=summary,
        ),
    )


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Create a workflow artifact with deterministic prefilled metadata."
    )
    parser.add_argument("kind", choices=sorted(TEMPLATES), help="Artifact kind")
    parser.add_argument("output_path", help="Artifact path to create")
    parser.add_argument("--title", help="Markdown H1 title to write")
    parser.add_argument("--source", help="Plan source summary")
    parser.add_argument("--mode", help="Plan mode")
    parser.add_argument("--owner", help="Plan owner")
    parser.add_argument("--scope", help="Review scope text")
    parser.add_argument("--review", help="Review artifact path used to prefill closure data")
    parser.add_argument("--finding-id", help="Finding ID used to prefill a closure report")
    parser.add_argument("--plan-item", help="Plan item trace used to prefill a closure report")
    parser.add_argument("--changed-files", help="Changed-file trace used to prefill a closure report")
    parser.add_argument(
        "--entrypoint",
        default="start_artifact.py",
        help="Workflow entrypoint to record in artifact provenance.",
    )
    parser.add_argument(
        "--no-gate-stub",
        action="store_true",
        help="Do not create a companion .quality-gate.txt stub for gated artifact kinds",
    )
    args = parser.parse_args()

    root = repo_root()
    output_path = (root / args.output_path).resolve()
    if output_path.exists():
        print(f"FAIL: refusing to overwrite existing file: {output_path}", file=sys.stderr)
        return 1

    template_path = root / TEMPLATES[args.kind]
    text = read_text(template_path)
    text = apply_prefill(text, args)
    text = apply_workflow_provenance(text, DEFAULT_SKILLS[args.kind], args.entrypoint)
    write_text(output_path, text)

    if args.kind in GATED_KINDS and not args.no_gate_stub:
        create_gate_stub(args.kind, output_path)

    print(f"Created {relative_to_repo(output_path)} from {TEMPLATES[args.kind]}")
    if args.kind in GATED_KINDS and not args.no_gate_stub:
        print(f"Created {relative_to_repo(output_path)}.quality-gate.txt gate stub")
    print("Next step:")
    if args.kind == "plan":
        print(
            f"  python3 scripts/workflow/check_workflow_artifact_bundle.py plan {relative_to_repo(output_path)} --allow-pending-gate"
        )
    elif args.kind == "spec":
        print(
            f"  python3 scripts/workflow/check_workflow_artifact_bundle.py spec {relative_to_repo(output_path)} --allow-pending-gate"
        )
    elif args.kind == "review":
        print(
            f"  python3 scripts/workflow/check_workflow_artifact_bundle.py review {relative_to_repo(output_path)} --allow-pending-gate"
        )
    else:
        print(
            f"  python3 scripts/workflow/check_workflow_artifact_bundle.py closure {relative_to_repo(output_path)}"
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

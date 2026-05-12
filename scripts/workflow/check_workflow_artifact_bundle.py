#!/usr/bin/env python3
"""Validate all deterministic checks relevant to one workflow artifact."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

import check_closure_report
import check_plan_checkboxes
import check_quality_gate_evidence
import check_review_report
import check_spec_artifact
import check_traceability
import check_version_impact
from workflow_common import read_text, read_workflow_provenance


EXPECTED_PROVENANCE = {
    "spec": {"skill": "opforge-spec-authoring"},
    "plan": {"skill": "opforge-plan-authoring"},
    "review": {"skill": "opforge-review-reporting"},
    "closure": {"skill": "opforge-review-closure"},
}


def validate_workflow_provenance(kind: str, path: Path) -> list[str]:
    text = read_text(path)
    provenance = read_workflow_provenance(text)
    if provenance is None:
        return [f"{path}: missing workflow provenance comment; use the matching local skill and workflow wrapper"]

    expected_skill = EXPECTED_PROVENANCE[kind]["skill"]
    if provenance.get("skill") != expected_skill:
        return [
            f"{path}: workflow provenance skill `{provenance.get('skill')}` does not match expected `{expected_skill}`"
        ]

    entrypoint = provenance.get("entrypoint", "").strip()
    if not entrypoint:
        return [f"{path}: workflow provenance is missing a non-empty entrypoint"]

    return []


def validate_gate_presence(path: Path) -> list[str]:
    gate_path = path.with_name(f"{path.name}.quality-gate.txt")
    if not gate_path.exists():
        return [f"{gate_path}: missing quality gate file"]
    if not gate_path.is_file():
        return [f"{gate_path}: quality gate path is not a file"]
    return []


def validate_bundle(
    kind: str,
    path: Path,
    *,
    root: Path,
    allow_pending_gate: bool,
) -> list[str]:
    errors: list[str] = []
    errors.extend(validate_workflow_provenance(kind, path))

    if kind == "spec":
        errors.extend(check_spec_artifact.validate_spec(path))
        if allow_pending_gate:
            errors.extend(validate_gate_presence(path))
        else:
            errors.extend(check_quality_gate_evidence.validate_artifact(path))
        return errors

    if kind == "plan":
        errors.extend(check_plan_checkboxes.analyze_plan(path))
        errors.extend(check_version_impact.validate_version_impact(path))
        if allow_pending_gate:
            errors.extend(validate_gate_presence(path))
        else:
            errors.extend(check_quality_gate_evidence.validate_artifact(path))
        errors.extend(check_traceability.validate_path(root, path, check_traceability.review_finding_ids(root)))
        return errors

    if kind == "review":
        errors.extend(check_review_report.validate_review(path))
        errors.extend(check_version_impact.validate_version_impact(path))
        if allow_pending_gate:
            errors.extend(validate_gate_presence(path))
        else:
            errors.extend(check_quality_gate_evidence.validate_artifact(path))
        return errors

    if kind == "closure":
        errors.extend(check_closure_report.validate_closure(path))
        errors.extend(check_traceability.validate_path(root, path, check_traceability.review_finding_ids(root)))
        return errors

    return [f"{path}: unsupported bundle kind `{kind}`"]


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Run the deterministic validator bundle for a workflow artifact kind."
    )
    parser.add_argument("kind", choices=["spec", "plan", "review", "closure"])
    parser.add_argument("paths", nargs="+", help="Artifact paths to validate")
    parser.add_argument("--root", default=".", help="Repository root. Defaults to current directory.")
    parser.add_argument(
        "--allow-pending-gate",
        action="store_true",
        help="Require the quality gate sidecar to exist but do not require PASS status yet.",
    )
    args = parser.parse_args()

    root = Path(args.root).resolve()
    all_errors: list[str] = []

    for raw_path in args.paths:
        path = Path(raw_path)
        if not path.exists():
            all_errors.append(f"{path}: file not found")
            continue
        all_errors.extend(
            validate_bundle(
                args.kind,
                path,
                root=root,
                allow_pending_gate=args.allow_pending_gate,
            )
        )

    if all_errors:
        for error in all_errors:
            print(f"FAIL: {error}", file=sys.stderr)
        return 1

    for raw_path in args.paths:
        print(f"PASS: {args.kind} bundle valid for {raw_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""Validate the permanent native debug-evidence classification ledger."""

from __future__ import annotations

import argparse
import sys
import tomllib
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
MANIFEST = ROOT / "scripts/workflow/native_debug_evidence_classification.toml"
REQUIRED_IDS = (
    "debug-contract-harness",
    "cli-debug-event-harness",
    "macro-preprocessor-harness",
    "macro-preprocessor-helper",
    "pipeline-select-harness",
    "pipeline-select-helper",
    "macro-cli-debug-event-harness",
    "macro-cli-debug-event-helper",
    "console-debugger-probe",
    "console-debugger-runner",
    "console-debugger-contract",
    "macro-hang-console-report",
)
LEVELS = {"B", "C", "D", "E"}
ROLES = {
    "permanent-contract",
    "focused-negative",
    "focused-contract",
    "diagnostic",
    "diagnostic-tool",
    "diagnostic-contract",
    "diagnostic-report",
}
AUTHORITIES = {"none", "focused-contract"}
LIFECYCLES = {"permanent", "temporary", "remove"}


def load_manifest(path: Path) -> object:
    with path.open("rb") as handle:
        return tomllib.load(handle)


def validate(
    root: Path = ROOT,
    manifest_path: Path | None = None,
    required_ids: tuple[str, ...] = REQUIRED_IDS,
) -> list[str]:
    root = root.resolve()
    manifest_path = manifest_path or root / MANIFEST.relative_to(ROOT)
    try:
        data = load_manifest(manifest_path)
    except (OSError, tomllib.TOMLDecodeError) as error:
        return [f"cannot load evidence classification: {error}"]
    if not isinstance(data, dict):
        return ["classification root must be a table"]

    errors: list[str] = []
    policy = data.get("policy")
    if not isinstance(policy, dict):
        errors.append("policy must be a table")
    else:
        if policy.get("schema_version") != 1:
            errors.append("policy.schema_version must be 1")
        if policy.get("macro_artifact_parity_authority") != "examples/opcore/macro_invocation_native.asm":
            errors.append("the untouched macro CLI fixture must remain sole macro artifact parity authority")
        if policy.get("level_e_can_close_completion") is not False:
            errors.append("Level E evidence must not close completion")

    artifacts = data.get("artifacts")
    if not isinstance(artifacts, list):
        return [*errors, "artifacts must be an array of tables"]
    ids: list[str] = []
    for index, artifact in enumerate(artifacts, 1):
        label = f"artifacts entry {index}"
        if not isinstance(artifact, dict):
            errors.append(f"{label} must be a table")
            continue
        artifact_id = artifact.get("id")
        if not isinstance(artifact_id, str) or not artifact_id:
            errors.append(f"{label} requires a non-empty id")
            continue
        ids.append(artifact_id)
        label = artifact_id
        proof_level = artifact.get("proof_level")
        role = artifact.get("role")
        authority = artifact.get("authority")
        lifecycle = artifact.get("lifecycle")
        deletion_condition = artifact.get("deletion_condition")
        if proof_level not in LEVELS:
            errors.append(f"{label}: invalid proof_level")
        if role not in ROLES:
            errors.append(f"{label}: invalid role")
        if authority not in AUTHORITIES:
            errors.append(f"{label}: invalid authority")
        if lifecycle not in LIFECYCLES:
            errors.append(f"{label}: invalid lifecycle")
        if proof_level == "E" and authority != "none":
            errors.append(f"{label}: Level E evidence cannot be completion authority")
        if isinstance(role, str) and role.startswith("diagnostic") and proof_level != "E":
            errors.append(f"{label}: diagnostic evidence must be Level E")
        if lifecycle == "temporary" and not deletion_condition:
            errors.append(f"{label}: temporary evidence requires a deletion condition")

        relative = artifact.get("path")
        marker = artifact.get("marker")
        if not isinstance(relative, str) or not relative or Path(relative).is_absolute():
            errors.append(f"{label}: path must be repository-relative")
            continue
        source = (root / relative).resolve()
        if root not in source.parents:
            errors.append(f"{label}: path escapes repository root")
            continue
        if not source.is_file():
            errors.append(f"{label}: classified artifact is missing: {relative}")
            continue
        if not isinstance(marker, str) or not marker:
            errors.append(f"{label}: marker must be non-empty")
        elif marker not in source.read_text(encoding="utf-8"):
            errors.append(f"{label}: declared classification marker is missing from {relative}")

    if len(ids) != len(set(ids)):
        errors.append("artifact ids must be unique")
    if tuple(ids) != required_ids:
        errors.append("artifacts must list every required debug evidence item in canonical order")

    tests_source = root / "crates/opforge-asm/src/tests.rs"
    if tests_source.is_file() and "Proof level D diagnostic" in tests_source.read_text(encoding="utf-8"):
        errors.append("tests.rs retains the contradictory `Proof level D diagnostic` classification")
    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--staged", action="store_true", help="Accepted for native-gate composition; the complete ledger is always checked.")
    args = parser.parse_args()
    errors = validate(args.root)
    if errors:
        print("native debug evidence classification: FAIL", file=sys.stderr)
        for error in errors:
            print(f"  - {error}", file=sys.stderr)
        return 1
    print("native debug evidence classification: PASS")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

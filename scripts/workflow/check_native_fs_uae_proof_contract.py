#!/usr/bin/env python3
"""Enforce the singular fail-closed FS-UAE native parity proof contract."""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RUNNER = Path("crates/opforge-asm/src/fs_uae_smoke.rs")
TEST_ROOT = Path("crates/opforge-asm/src/tests")
RULES = (
    Path("AGENTS.md"),
    Path("agents/rules/native-rust-parity-porting.md"),
    Path("agents/rules/fs-uae.md"),
)


def struct_initializers(source: str, type_name: str) -> list[str]:
    marker = f"{type_name} {{"
    blocks: list[str] = []
    start = 0
    while True:
        index = source.find(marker, start)
        if index < 0:
            return blocks
        brace = source.find("{", index)
        depth = 0
        for cursor in range(brace, len(source)):
            if source[cursor] == "{":
                depth += 1
            elif source[cursor] == "}":
                depth -= 1
                if depth == 0:
                    blocks.append(source[index : cursor + 1])
                    start = cursor + 1
                    break
        else:
            blocks.append(source[index:])
            return blocks


def validate(root: Path = ROOT) -> list[str]:
    root = root.resolve()
    errors: list[str] = []
    runner_path = root / RUNNER
    try:
        runner = runner_path.read_text(encoding="utf-8")
    except OSError as error:
        return [f"cannot read {RUNNER}: {error}"]

    required_runner_tokens = (
        "OPFORGE-FS-UAE-PROOF-V1 START",
        "OPFORGE-FS-UAE-PROOF-V1 DONE",
        "opforge_native_cli_case_identity",
        "resolved_package_bytes",
        "opforge_native_cli_run_challenge",
        "clear_native_cli_output_artifacts",
        "EphemeralArtifactDir",
        "if let Err(error) = verify_native_cli_case_proof(case, &mut run) {",
        "OpforgeNativeCliProof::ExactArtifact",
        "OpforgeNativeCliProof::ExactArtifacts",
        "actual != artifact.rust_oracle",
        "proof_errors.push(error)",
        "drop(ephemeral_artifact_dir)",
        "require_completed_guest_protocol",
        "after every case was attempted",
    )
    for token in required_runner_tokens:
        if token not in runner:
            errors.append(f"{RUNNER}: missing proof-contract token {token!r}")
    if runner.count("let ephemeral_artifact_dir = EphemeralArtifactDir") < 3:
        errors.append(f"{RUNNER}: every FS-UAE execution path must own ephemeral artifacts")
    if runner.count("success: protocol_completed && guest_exit_code == Some(0)") < 2:
        errors.append(
            f"{RUNNER}: non-parity FS-UAE success must require guest completion and zero exit"
        )
    if runner.count("require_completed_guest_protocol(") < 3:
        errors.append(
            f"{RUNNER}: every generic FS-UAE result must fail closed before it reaches a test"
        )

    forbidden_tokens = (
        "record_last_green_fs_uae_test_run",
        "FS_UAE_LAST_GREEN",
        "last_green.txt",
    )
    searchable_paths = [runner_path, *(root.glob("crates/opforge-asm/src/tests/**/*.rs"))]
    searchable_source = ""
    for path in searchable_paths:
        try:
            source = path.read_text(encoding="utf-8")
        except OSError as error:
            errors.append(f"cannot read {path.relative_to(root)}: {error}")
            continue
        searchable_source += source
        for token in forbidden_tokens:
            if token in source:
                errors.append(f"{path.relative_to(root)}: persistent stale-evidence token {token!r} is forbidden")
        for case_type in (
            "OpforgeNativeCliParityCase",
            "OpforgeNativeCliMosFixtureCase",
        ):
            for block in struct_initializers(source, case_type):
                if re.search(r"\bname\s*(?=:|,)", block) is None or "proof:" not in block:
                    errors.append(
                        f"{path.relative_to(root)}: every {case_type} FS-UAE case must declare its name and mandatory proof mode"
                    )

    if "poisoned.into_inner()" not in searchable_source:
        errors.append(
            "native FS-UAE tests must recover the serial coordinator after a failed case"
        )

    required_rule_phrases = (
        "fresh per-run challenge",
        "byte-for-byte",
        "removed before the runner returns",
        "actual test case",
        "must not prevent later cases",
        "launcher success never substitutes",
        "no test result is valid",
    )
    for relative in RULES:
        path = root / relative
        try:
            source = path.read_text(encoding="utf-8")
        except OSError as error:
            errors.append(f"cannot read {relative}: {error}")
            continue
        normalized = " ".join(source.split()).lower()
        for phrase in required_rule_phrases:
            if phrase not in normalized:
                errors.append(f"{relative}: missing canonical FS-UAE proof phrase {phrase!r}")
    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--staged", action="store_true", help="Accepted for gate composition; the complete contract is always checked.")
    args = parser.parse_args()
    errors = validate(args.root)
    if errors:
        print("native FS-UAE proof contract: FAIL", file=sys.stderr)
        for error in errors:
            print(f"  - {error}", file=sys.stderr)
        return 1
    print("native FS-UAE proof contract: PASS")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

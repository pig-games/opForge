#!/usr/bin/env python3
"""Render canonical workflow quality-gate command presets."""

from __future__ import annotations

import argparse
import sys


PRESETS = {
    "rust-full": "scripts/workflow/run_rust_quality_gate.sh",
    "native-68000-format": "scripts/workflow/run_native_68000_format_gate.sh",
    "workflow-docs": "make workflow-gate",
    "fsuae-smoke": (
        "OPFORGE_FS_UAE_SMOKE=1 "
        "OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' "
        "OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' "
        "OPFORGE_FS_UAE_ARGS='{fsuae_config}' "
        "cargo test -p asm external_fs_uae_ -- --nocapture --test-threads=1"
    ),
    "closure-structure": "python3 scripts/workflow/check_closure_report.py <closure-path>",
    "plan-bundle": "python3 scripts/workflow/check_workflow_artifact_bundle.py plan <plan-path>",
    "review-bundle": "python3 scripts/workflow/check_workflow_artifact_bundle.py review <review-path>",
    "spec-bundle": "python3 scripts/workflow/check_workflow_artifact_bundle.py spec <spec-path>",
}


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Print a canonical quality-gate preset command or list available presets."
    )
    parser.add_argument("preset", nargs="?", help="Preset name to render")
    parser.add_argument("--list", action="store_true", help="List available preset names")
    args = parser.parse_args()

    if args.list:
        for preset in sorted(PRESETS):
            print(preset)
        return 0

    if not args.preset:
        parser.error("preset is required unless --list is used")

    command = PRESETS.get(args.preset)
    if command is None:
        print(f"FAIL: unknown preset `{args.preset}`", file=sys.stderr)
        return 1

    print(command)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

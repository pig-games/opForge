#!/usr/bin/env python3
"""Validate VS Code agent selector symlinks against canonical agent files."""

from __future__ import annotations

import argparse
import os
import sys
from pathlib import Path


EXPECTED_TARGET_PREFIX = "../../agents"


def validate_agent_symlinks(root: Path) -> list[str]:
    errors: list[str] = []
    canonical_dir = root / "agents"
    selector_dir = root / ".github" / "agents"

    if not canonical_dir.is_dir():
        return [f"missing canonical agent directory: {canonical_dir}"]
    if not selector_dir.is_dir():
        return [f"missing VS Code selector agent directory: {selector_dir}"]

    canonical_names = sorted(path.name for path in canonical_dir.glob("*.agent.md"))
    selector_names = sorted(path.name for path in selector_dir.glob("*.agent.md"))

    missing = sorted(set(canonical_names) - set(selector_names))
    extra = sorted(set(selector_names) - set(canonical_names))
    for name in missing:
        errors.append(f"missing selector symlink for canonical agent `{name}`")
    for name in extra:
        errors.append(f"selector agent `{name}` has no canonical agent counterpart")

    for name in selector_names:
        selector_path = selector_dir / name
        canonical_path = canonical_dir / name
        expected_target = f"{EXPECTED_TARGET_PREFIX}/{name}"

        if not selector_path.is_symlink():
            errors.append(f"{selector_path}: must be a symlink, not a copied file")
            continue

        actual_target = os.readlink(selector_path)
        if actual_target != expected_target:
            errors.append(
                f"{selector_path}: expected symlink target `{expected_target}`, got `{actual_target}`"
            )

        if not canonical_path.is_file():
            errors.append(f"{selector_path}: symlink target does not resolve to a file")

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Check that .github/agents contains only symlinks to canonical agents/."
    )
    parser.add_argument(
        "--root",
        default=".",
        help="Repository root to inspect. Defaults to the current directory.",
    )
    args = parser.parse_args()

    root = Path(args.root).resolve()
    errors = validate_agent_symlinks(root)
    if errors:
        for error in errors:
            print(f"FAIL: {error}", file=sys.stderr)
        return 1

    print("PASS: VS Code agent selector symlinks match canonical agents")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
#!/usr/bin/env python3
"""Enforce certified native runtime no-growth and ownership boundaries."""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
import tomllib
from dataclasses import dataclass
from pathlib import Path

from check_native_runtime_boundary_inventory import source_surface

ROOT = Path(__file__).resolve().parents[2]
BASELINE = ROOT / "scripts/workflow/native_runtime_ownership_baseline.toml"
NATIVE_ROOT = Path("native/motorola68000/amigaos")
BLOCK_RE = re.compile(r"^([A-Za-z_][A-Za-z0-9_]*)\s+\.block\b", re.MULTILINE | re.IGNORECASE)
MODULE_RE = re.compile(r"^\s*\.module\s+([^\s;]+)", re.MULTILINE | re.IGNORECASE)
OWNER_RE = re.compile(r"^\s*;\s*@opforge-owner:\s*(\S+)\s*$", re.MULTILINE)
ROLE_RE = re.compile(r"^\s*;\s*@opforge-role:\s*(\S+)\s*$", re.MULTILINE)
ALLOWED_HOTSPOT_ROLES = {"facade", "delegation"}


@dataclass(frozen=True)
class Baseline:
    certified_modules: frozenset[str]
    hotspot_blocks: dict[str, frozenset[str]]


def load_baseline(path: Path = BASELINE) -> Baseline:
    data = tomllib.loads(path.read_text(encoding="utf-8"))
    if data.get("schema_version") != 1:
        raise ValueError("native runtime ownership baseline schema_version must be 1")
    certified = frozenset(data.get("certified_modules", ()))
    hotspots = {
        relative: frozenset(record.get("blocks", ()))
        for relative, record in data.get("hotspots", {}).items()
    }
    return Baseline(certified_modules=certified, hotspot_blocks=hotspots)


def annotation_values(text: str) -> tuple[str | None, str | None]:
    owner = OWNER_RE.search(text)
    role = ROLE_RE.search(text)
    return (
        owner.group(1) if owner else None,
        role.group(1).lower() if role else None,
    )


def annotation_window(lines: list[str], line_index: int) -> str:
    start = line_index
    while start > 0 and line_index - start < 12:
        candidate = lines[start - 1].strip()
        if not candidate or candidate.startswith(";"):
            start -= 1
            continue
        break
    return "\n".join(lines[start:line_index])


def validate_annotation(
    relative: str,
    annotation: str,
    *,
    require_role: bool,
) -> list[str]:
    errors: list[str] = []
    owner, role = annotation_values(annotation)
    if not owner:
        errors.append(f"{relative}: missing @opforge-owner annotation")
    if require_role and role not in ALLOWED_HOTSPOT_ROLES:
        errors.append(
            f"{relative}: new hotspot routine requires @opforge-role: facade or delegation"
        )
    return errors


def validate_hotspots(root: Path, baseline: Baseline) -> list[str]:
    errors: list[str] = []
    for relative, allowed in baseline.hotspot_blocks.items():
        path = root / relative
        if not path.is_file():
            errors.append(f"missing certified hotspot: {relative}")
            continue
        lines = path.read_text(encoding="utf-8").splitlines()
        for line_index, line in enumerate(lines):
            match = BLOCK_RE.match(line)
            if not match or match.group(1) in allowed:
                continue
            name = match.group(1)
            annotation = annotation_window(lines, line_index)
            entry_errors = validate_annotation(
                f"{relative}:{line_index + 1}:{name}", annotation, require_role=True
            )
            if entry_errors:
                errors.extend(entry_errors)
    return errors


def is_production_module(relative: str, text: str) -> bool:
    if not relative.startswith(f"{NATIVE_ROOT.as_posix()}/"):
        return False
    if "/tests/" in relative or "/debug/" in relative:
        return False
    return bool(MODULE_RE.search(text))


def validate_new_modules(root: Path, baseline: Baseline) -> list[str]:
    errors: list[str] = []
    native_root = root / NATIVE_ROOT
    for relative in sorted(baseline.certified_modules):
        if not (root / relative).is_file():
            errors.append(f"missing annotation-exempt source: {relative}")
    for path in sorted(native_root.rglob("*.asm")):
        relative = path.relative_to(root).as_posix()
        if relative in baseline.certified_modules:
            continue
        text = path.read_text(encoding="utf-8")
        if not is_production_module(relative, text):
            continue
        try:
            surface = source_surface(relative, root)
        except (OSError, ValueError) as error:
            errors.append(f"{relative}: {error}")
            continue
        # Provenance is conservative: a macro may generate executable blocks.
        # The inventory excludes those from routine counts, but their module
        # still needs an owner, as it did before include-aware scanning.
        if not any(BLOCK_RE.search(text) for text in surface.sources.values()):
            continue
        header = "\n".join(text.splitlines()[:40])
        errors.extend(validate_annotation(relative, header, require_role=False))
    return errors


def validate(root: Path = ROOT, baseline_path: Path | None = None) -> list[str]:
    baseline = load_baseline(baseline_path or root / BASELINE.relative_to(ROOT))
    return [
        *validate_hotspots(root, baseline),
        *validate_new_modules(root, baseline),
    ]


def run_cpu_boundary(root: Path, staged: bool) -> int:
    command = [
        sys.executable,
        str(root / "scripts/workflow/check_cpu_specific_arch_boundary.py"),
        "--no-report",
    ]
    if staged:
        command.append("--staged")
    return subprocess.run(command, cwd=root, check=False).returncode


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", default=".")
    parser.add_argument("--staged", action="store_true")
    args = parser.parse_args()
    root = Path(args.root).resolve()
    errors = validate(root)
    if errors:
        print("native runtime no-growth guard: FAIL")
        for error in errors:
            print(f"  - {error}")
        return 1
    if run_cpu_boundary(root, args.staged):
        print("native runtime no-growth guard: FAIL (CPU ownership boundary)")
        return 1
    print("native runtime no-growth guard: PASS")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

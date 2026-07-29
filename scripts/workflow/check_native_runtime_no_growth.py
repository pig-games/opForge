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


ROOT = Path(__file__).resolve().parents[2]
BASELINE = ROOT / "scripts/workflow/native_runtime_ownership_baseline.toml"
NATIVE_ROOT = Path("native/motorola68000/amigaos")
BLOCK_RE = re.compile(r"^([A-Za-z_][A-Za-z0-9_]*)\s+\.block\b", re.MULTILINE)
MODULE_RE = re.compile(r"^\s*\.module\s+([^\s;]+)", re.MULTILINE)
OWNER_RE = re.compile(r"^\s*;\s*@opforge-owner:\s*(\S+)\s*$", re.MULTILINE)
SLICE_RE = re.compile(r"^\s*;\s*@opforge-slice:\s*(\S+)\s*$", re.MULTILINE)
ROLE_RE = re.compile(r"^\s*;\s*@opforge-role:\s*(\S+)\s*$", re.MULTILINE)
MUTABLE_ENGINE_RE = re.compile(
    r"\bOpasmEngine(?:Context|AssemblySession[A-Za-z0-9_]*|Session[A-Za-z0-9_]*|"
    r"Source[A-Za-z0-9_]*|Stmt[A-Za-z0-9_]*|Label[A-Za-z0-9_]*|Image[A-Za-z0-9_]*)\b",
    re.IGNORECASE,
)
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


def annotation_values(text: str) -> tuple[str | None, str | None, str | None]:
    owner = OWNER_RE.search(text)
    slice_match = SLICE_RE.search(text)
    role = ROLE_RE.search(text)
    return (
        owner.group(1) if owner else None,
        slice_match.group(1) if slice_match else None,
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
    root: Path,
    relative: str,
    annotation: str,
    *,
    require_role: bool,
) -> list[str]:
    errors: list[str] = []
    owner, slice_path, role = annotation_values(annotation)
    if not owner:
        errors.append(f"{relative}: missing @opforge-owner annotation")
    if not slice_path:
        errors.append(f"{relative}: missing @opforge-slice annotation")
    elif not slice_path.startswith("documentation/plans/slices/") or not slice_path.endswith(".toml"):
        errors.append(f"{relative}: invalid @opforge-slice path: {slice_path}")
    elif not (root / slice_path).is_file():
        errors.append(f"{relative}: @opforge-slice does not exist: {slice_path}")
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
            match = re.match(r"^([A-Za-z_][A-Za-z0-9_]*)\s+\.block\b", line)
            if not match or match.group(1) in allowed:
                continue
            name = match.group(1)
            annotation = annotation_window(lines, line_index)
            entry_errors = validate_annotation(
                root, f"{relative}:{line_index + 1}:{name}", annotation, require_role=True
            )
            if entry_errors:
                errors.extend(entry_errors)
    return errors


def strip_asm_comment_and_strings(line: str) -> str:
    code = line.split(";", 1)[0]
    return re.sub(r'"(?:[^"\\]|\\.)*"', '""', code)


def validate_tkpkg_engine_state(root: Path) -> list[str]:
    errors: list[str] = []
    tkpkg_root = root / NATIVE_ROOT / "tkpkg"
    for path in sorted(tkpkg_root.glob("*.asm")):
        relative = path.relative_to(root).as_posix()
        for line_number, line in enumerate(path.read_text(encoding="utf-8").splitlines(), start=1):
            code = strip_asm_comment_and_strings(line)
            match = MUTABLE_ENGINE_RE.search(code)
            if match:
                errors.append(
                    f"{relative}:{line_number}: direct tkpkg access to opasm mutable state: {match.group(0)}"
                )
    return errors


def is_production_semantic_module(relative: str, text: str) -> bool:
    if not relative.startswith(f"{NATIVE_ROOT.as_posix()}/"):
        return False
    if "/test-harnesses/" in relative or "/debug/" in relative:
        return False
    return bool(MODULE_RE.search(text) and BLOCK_RE.search(text))


def validate_new_modules(root: Path, baseline: Baseline) -> list[str]:
    errors: list[str] = []
    native_root = root / NATIVE_ROOT
    for path in sorted(native_root.rglob("*.asm")):
        relative = path.relative_to(root).as_posix()
        if relative in baseline.certified_modules:
            continue
        text = path.read_text(encoding="utf-8")
        if not is_production_semantic_module(relative, text):
            continue
        header = "\n".join(text.splitlines()[:40])
        errors.extend(validate_annotation(root, relative, header, require_role=False))
    return errors


def validate(root: Path = ROOT, baseline_path: Path | None = None) -> list[str]:
    baseline = load_baseline(baseline_path or root / BASELINE.relative_to(ROOT))
    return [
        *validate_hotspots(root, baseline),
        *validate_tkpkg_engine_state(root),
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

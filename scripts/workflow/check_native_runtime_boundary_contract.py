#!/usr/bin/env python3
"""Validate the promoted native ownership and import boundaries."""

from __future__ import annotations

import argparse
import re
import sys
from collections import Counter
from pathlib import Path

from check_native_runtime_boundary_inventory import source_surface


ROOT = Path(__file__).resolve().parents[2]
NATIVE_ROOT = Path("native/motorola68000/amigaos")
BOUNDARY_ROOTS = (NATIVE_ROOT / "vm", NATIVE_ROOT / "packages")

# Exact source/edge exceptions, reviewed against calls and storage use. These
# are not blanket permission for a VM or package helper to acquire assembler
# responsibilities. The semantic edges remain boundary debt, not wire imports.
ASSEMBLER_IMPORTS = {
    f"{NATIVE_ROOT}/vm/exvm/runtime.asm": {
        "opforge.assembler.source.records": "numeric token/wire constants only",
    },
    f"{NATIVE_ROOT}/vm/exprvm/values_runtime.asm": {
        "opforge.assembler.values.values": "typed owner arena and scalar/compound services",
        "opforge.assembler.values.records": "typed record validation and projection",
        "opforge.assembler.values.structs": "nominal struct construction and assembler literal policy",
        "opforge.assembler.symbols.bindings": "typed symbol cells and binding lookup",
    },
    f"{NATIVE_ROOT}/packages/validation.asm": {
        "opforge.assembler.layout.state": "state.validate checks package STVM tables",
    },
}

# Keep these acquisition/request owners narrow. Order is not an ownership
# property. The loader's possible debug dependency comes from its gated include.
OWNER_IMPORTS = {
    f"{NATIVE_ROOT}/cli/entry.asm": frozenset({
        "opforge.assembler.driver", "opforge.cli.arguments", "opforge.cli.input",
        "opforge.cli.target",
    }),
    f"{NATIVE_ROOT}/packages/loader.asm": frozenset({
        "opforge.packages.catalog", "opforge.packages.format",
        "opforge.packages.validation", "opforge.platform.memory",
        "opforge.debug.memory_profile",
    }),
}


def imports(text: str) -> list[str]:
    """Possible imports, including gated/template definitions; no execution."""
    return [name.lower() for name in re.findall(
        r"^\s*\.use\s+([^\s;]+)", text, re.MULTILINE | re.IGNORECASE)]


def within(module: str, namespace: str) -> bool:
    return module == namespace or module.startswith(namespace + ".")


def validate(root: Path = ROOT) -> list[str]:
    root = root.resolve()
    errors: list[str] = []
    paths = set(OWNER_IMPORTS)
    for relative in BOUNDARY_ROOTS:
        directory = root / relative
        if not directory.is_dir():
            errors.append(f"missing native boundary source directory: {relative}")
            continue
        paths.update(path.relative_to(root).as_posix()
                     for path in directory.rglob("*")
                     if path.is_file() and path.suffix.lower() in {".asm", ".i"})
    # A removed exception source must require review instead of disappearing
    # silently from a directory scan.
    paths.update(ASSEMBLER_IMPORTS)
    seen: dict[str, list[str]] = {}
    boundary_sources: set[str] = set()
    owner_imports: dict[str, list[str]] = {}
    for relative in sorted(paths):
        try:
            surface = source_surface(relative, root)
        except (OSError, ValueError) as error:
            errors.append(f"{relative}: {error}")
            continue
        for name, text in surface.sources.items():
            seen[name] = imports(text)
            if name != relative and re.search(r"^\s*\.module\b", text, re.MULTILINE | re.IGNORECASE):
                errors.append(f"{relative}: included module owner {name}; use a module import")
        if relative != f"{NATIVE_ROOT}/cli/entry.asm":
            boundary_sources.update(surface.sources)
        if relative in OWNER_IMPORTS:
            owner_imports[relative] = [edge for text in surface.sources.values()
                                       for edge in imports(text)]

    # Apply exceptions to the defining source, never to every helper that happens
    # to be included by an exempt consumer. CLI entry is checked separately.
    for relative, found in sorted(seen.items()):
        if relative not in boundary_sources:
            continue
        allowed = ASSEMBLER_IMPORTS.get(relative, {})
        for edge in found:
            if within(edge, "opforge.cli") or (
                within(edge, "opforge.assembler") and edge not in allowed
            ):
                errors.append(f"prohibited reverse import: {relative} -> {edge}")
    for relative, allowed in ASSEMBLER_IMPORTS.items():
        if relative not in seen:
            continue  # Missing source is already reported above.
        for edge in allowed:
            if edge not in seen[relative]:
                errors.append(f"unused reviewed import exception: {relative} -> {edge}")
    for relative, expected in OWNER_IMPORTS.items():
        if relative not in owner_imports:
            continue
        found = owner_imports[relative]
        if set(found) != expected:
            errors.append(f"owner imports changed: {relative}: "
                          f"missing {sorted(expected - set(found))}, "
                          f"unexpected {sorted(set(found) - expected)}")
        duplicates = sorted(edge for edge, count in Counter(found).items() if count > 1)
        if duplicates:
            errors.append(f"duplicate owner imports: {relative}: {duplicates}")
    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--staged", action="store_true", help="Accepted for gate composition; all boundary sources are checked.")
    args = parser.parse_args()
    errors = validate(args.root)
    if errors:
        print("native runtime boundary contract: FAIL")
        for error in errors:
            print(f"  - {error}")
        return 1
    print("native runtime boundary contract: PASS")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

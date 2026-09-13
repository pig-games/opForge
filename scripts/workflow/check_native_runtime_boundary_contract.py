#!/usr/bin/env python3
"""Validate native runtime ownership and import boundaries."""

from __future__ import annotations

import re
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
FORBIDDEN_IMPORTS = {
    "native/motorola68000/amigaos/opasm/opasm_engine.asm": ("tkpkg.", "opcore.", "opforge_cli."),
    "native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm": ("tkpkg.amigaos.service",),
    "native/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm": ("opasm.amigaos.engine",),
    "native/motorola68000/amigaos/tkpkg/tkpkg_tokenizer_vm.asm": ("opasm.amigaos.engine",),
    "native/motorola68000/amigaos/prvm/prvm_runtime.asm": ("opasm.amigaos.engine",),
    "native/motorola68000/amigaos/opcore/opcore_expr_bridge.asm": ("opasm.amigaos.engine",),
}
RETAINED_OWNER_IMPORTS = {
    "native/motorola68000/amigaos/opasm/opasm_engine.asm": (
        "opasm.amigaos.events",
        "opasm.amigaos.progress",
        "debug.amigaos.symbol_expr_profile",
        "debug.amigaos.platform_profile",
    ),
    "native/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm": (
        "tkpkg.amigaos.abi",
        "tkpkg.amigaos.buffers",
        "tkpkg.amigaos.compact_table",
        "tkpkg.amigaos.state_service",
        "tkpkg.amigaos.token_policy",
    ),
}


def imports(path: Path) -> list[str]:
    return re.findall(r"^\s*\.use\s+([^\s;]+)", path.read_text(encoding="utf-8"), re.MULTILINE)


def validate(root: Path = ROOT) -> list[str]:
    errors: list[str] = []
    for relative, forbidden in FORBIDDEN_IMPORTS.items():
        found = imports(root / relative)
        for edge in forbidden:
            if any(imported.startswith(edge) for imported in found):
                errors.append(f"prohibited current reverse import: {relative} -> {edge}")
    for relative, expected in RETAINED_OWNER_IMPORTS.items():
        found = tuple(imports(root / relative))
        if found != expected:
            errors.append(
                f"retained-owner imports changed: {relative}: "
                f"expected {expected}, found {found}"
            )
    service_imports = imports(root / "native/motorola68000/amigaos/tkpkg/tkpkg_service.asm")
    if "opasm.amigaos.engine" in service_imports:
        errors.append("obsolete service-to-engine import remains")
    return errors


def main() -> int:
    errors = validate()
    if errors:
        print("native runtime boundary contract: FAIL")
        for error in errors:
            print(f"  - {error}")
        return 1
    print("native runtime boundary contract: PASS")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

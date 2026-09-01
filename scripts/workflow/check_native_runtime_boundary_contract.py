#!/usr/bin/env python3
"""Validate the Item 5.3 native runtime ownership/dependency contract."""

from __future__ import annotations

import re
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
CONTRACT = ROOT / "documentation/architecture/native-runtime-boundary-contract-v0_1.md"
SLICE = ROOT / "documentation/plans/slices/native-porting-slice-runtime-boundary-contract.toml"

REQUIRED_TEXT = (
    "CLI frontend",
    "Preprocessor staging",
    "Assembly driver",
    "Assembly engine",
    "tkpkg facade",
    "Expression service",
    "Package runtimes",
    "Diagnostic/event projection",
    "current pass",
    "current address",
    "symbol lookup",
    "symbol stability/finalization",
    "diagnostic sink",
    "The following edges are prohibited",
    "Item 5.7.2 removes the obsolete `tkpkg.amigaos.service ->",
    "## Item 5.11 retained-owner audit",
    "documented no-change decision",
)
LEDGER_ITEMS = ("5.4", "5.4.1", "5.5", "5.5.1", "5.6", "5.6.1", "5.6.2", "5.7", "5.7.1", "5.7.2", "5.8", "5.8.1", "5.9", "5.9.1", "5.9.2", "5.9.3", "5.9.4", "5.10", "5.11")
FORBIDDEN_IMPORTS = {
    "native/motorola68000/amigaos/opasm/opasm_engine.asm": ("tkpkg.", "opcore.", "opforge_cli."),
    "native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm": ("tkpkg.amigaos.service",),
    "native/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm": ("opasm.amigaos.engine",),
    "native/motorola68000/amigaos/tkpkg/tkpkg_tokenizer_vm.asm": ("opasm.amigaos.engine",),
    "native/motorola68000/amigaos/prvm/prvm_runtime.asm": ("opasm.amigaos.engine",),
    "native/motorola68000/amigaos/opcore/opcore_expr_bridge.asm": ("opasm.amigaos.engine",),
}
RETAINED_ITEM_511_IMPORTS = {
    "native/motorola68000/amigaos/opasm/opasm_engine.asm": (
        "opasm.amigaos.events",
        "opasm.amigaos.progress",
    ),
    "native/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm": (
        "tkpkg.amigaos.abi",
        "tkpkg.amigaos.buffers",
        "tkpkg.amigaos.state_service",
        "tkpkg.amigaos.token_policy",
    ),
}


def imports(path: Path) -> list[str]:
    return re.findall(r"^\s*\.use\s+([^\s;]+)", path.read_text(encoding="utf-8"), re.MULTILINE)


def validate(root: Path = ROOT) -> list[str]:
    errors: list[str] = []
    contract = root / CONTRACT.relative_to(ROOT)
    slice_path = root / SLICE.relative_to(ROOT)
    if not contract.exists():
        return [f"missing contract: {contract.relative_to(root)}"]
    text = contract.read_text(encoding="utf-8")
    for required in REQUIRED_TEXT:
        if required not in text:
            errors.append(f"contract missing required declaration: {required}")
    for item in LEDGER_ITEMS:
        if f"| {item} |" not in text:
            errors.append(f"contract ledger missing Item {item}")
    if not slice_path.exists():
        errors.append(f"missing affected slice metadata: {slice_path.relative_to(root)}")
    elif "kind = \"native-rust-parity\"" not in slice_path.read_text(encoding="utf-8"):
        errors.append("affected slice metadata has unexpected kind")
    for relative, forbidden in FORBIDDEN_IMPORTS.items():
        found = imports(root / relative)
        for edge in forbidden:
            if any(imported.startswith(edge) for imported in found):
                errors.append(f"prohibited current reverse import: {relative} -> {edge}")
    for relative, expected in RETAINED_ITEM_511_IMPORTS.items():
        found = tuple(imports(root / relative))
        if found != expected:
            errors.append(
                f"Item 5.11 retained-owner imports changed: {relative}: "
                f"expected {expected}, found {found}"
            )
    service_imports = imports(root / "native/motorola68000/amigaos/tkpkg/tkpkg_service.asm")
    if "opasm.amigaos.engine" in service_imports:
        errors.append("obsolete service-to-engine import remains after Item 5.7.2")
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

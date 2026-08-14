#!/usr/bin/env python3
"""Fail closed when the Item 5.2 inventory drifts from its audited sources."""

from __future__ import annotations

import argparse
import hashlib
import re
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
INVENTORY = ROOT / "documentation/architecture/native-runtime-boundary-inventory-v0_1.md"
TARGETS = {
    "opasm.amigaos.assembly_driver": (
        "native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm",
        "assembleSessionV1",
        ("opasm.amigaos.directive_router", "opasm.amigaos.engine", "opasm.amigaos.tkpkg_bridge"),
    ),
    "opasm.amigaos.directive_router": (
        "native/motorola68000/amigaos/opasm/opasm_directive_router.asm",
        "classifyV1",
        (),
    ),
    "opasm.amigaos.operand_eval": (
        "native/motorola68000/amigaos/opasm/opasm_operand_eval.asm",
        "prepareSelectedRequestV1",
        ("opasm.amigaos.callback_abi", "opasm.amigaos.engine", "opasm.amigaos.flow_scopes"),
    ),
    "opasm.amigaos.directive_data": (
        "native/motorola68000/amigaos/opasm/opasm_directive_data.asm",
        "emitNumericDirectiveV1",
        ("opasm.amigaos.engine",),
    ),
    "opasm.amigaos.directive_text": (
        "native/motorola68000/amigaos/opasm/opasm_directive_text.asm",
        "emitTextDirectiveV1",
        ("opasm.amigaos.engine",),
    ),
    "opasm.amigaos.layout": (
        "native/motorola68000/amigaos/opasm/opasm_layout.asm",
        "alignCursorV1",
        ("opasm.amigaos.engine",),
    ),
    "tkpkg.amigaos.service": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_service.asm",
        "dispatchV1",
        ("opcore.amigaos.expr_bridge", "tkpkg.amigaos.pipeline"),
    ),
    "tkpkg.amigaos.selection_service": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_selection_service.asm",
        "selectInstructionV1",
        ("tkpkg.amigaos.runtime_context", "opcore.amigaos.expr_bridge"),
    ),
    "tkpkg.amigaos.operand_runtime": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_operand_runtime.asm",
        "tkpkgMselTryBuildCandidateV1",
        (
            "tkpkg.amigaos.buffers",
            "tkpkg.amigaos.selection_state",
            "tkpkg.amigaos.runtime_context",
            "opcore.amigaos.expr_bridge",
        ),
    ),
    "tkpkg.amigaos.encode_service": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_encode_service.asm",
        "encodeSelectedInstructionV1",
        (
            "tkpkg.amigaos.abi",
            "tkpkg.amigaos.buffers",
            "tkpkg.amigaos.selection_service",
        ),
    ),
    "tkpkg.amigaos.runtime_context": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_runtime_context.asm",
        "getAbiVersionV1",
        ("tkpkg.amigaos.engine_context_adapter",),
    ),
    "tkpkg.amigaos.engine_context_adapter": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_engine_context_adapter.asm",
        "lookupSymbolV1",
        ("opasm.amigaos.engine",),
    ),
    "opasm.amigaos.engine": (
        "native/motorola68000/amigaos/opasm/opasm_engine.asm",
        "initSessionV1",
        ("opasm.amigaos.events",),
    ),
    "tkpkg.amigaos.tokenizer_vm": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_tokenizer_vm.asm",
        "tkpkgTokenizerVmTokenizeLineV1",
        ("tkvm.amigaos.runtime",),
    ),
    "opcore.amigaos.expr_bridge": (
        "native/motorola68000/amigaos/opcore/opcore_expr_bridge.asm",
        "opcoreExprEvalOperandV1",
        ("exprvm.amigaos.runtime",),
    ),
    "prvm.amigaos.runtime": (
        "native/motorola68000/amigaos/prvm/prvm_runtime.asm",
        "prvmRun68000",
        (),
    ),
    "tkpkg.amigaos.pipeline": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm",
        "tkpkgPipelineSetActiveV1",
        ("tkpkg.amigaos.token_policy",),
    ),
    "opasm.amigaos.flow_text_encoding": (
        "native/motorola68000/amigaos/opasm/opasm_flow_text_encoding.asm",
        "resetStateV1",
        (),
    ),
}
SNAPSHOTS = {
    "opasm.amigaos.assembly_driver": ("cc7a15a046f94bf5641a0d37fe2a8aa5837b427144855f032953e3d07eec7a4c", 76, 18, ("code", "data", "bss"), 149),
    "opasm.amigaos.directive_router": ("b1011828f566a3896329cbda6d491379ee67f17888a5fb432e28aca15cce2339", 3, 0, ("code", "data"), 1),
    "opasm.amigaos.operand_eval": ("f4c8c3471ff2f4a543440b8825e527b390ef70956d480c3fe3b5d083d2f8c8ae", 15, 3, ("code", "bss"), 7),
    "opasm.amigaos.directive_data": ("ae827acedfad611d1d57ff93783edda61b6eb4de332c2907521343d6f9fdf3bc", 2, 3, ("code", "bss"), 19),
    "opasm.amigaos.directive_text": ("98d283b9678f051c68787dc915b841559ee1a3647e597b44d966492a931ff0c8", 2, 1, ("code", "bss"), 2),
    "opasm.amigaos.layout": ("086db5101020a5de7e4c930fa63c76812588c4fad50e8ea8332a18790872104a", 50, 1, ("code", "bss"), 2),
    "tkpkg.amigaos.service": ("747a6baf9f9a99ae135799ccd8147deb9ce1f23c5218634cf292e78c2318dfda", 42, 12, ("data", "bss", "code"), 112),
    "tkpkg.amigaos.selection_service": ("829f518387de387ad399c3c1375efd8aa750a06671f1b09767fc0401b963e05d", 21, 6, ("data", "code"), 62),
    "tkpkg.amigaos.operand_runtime": ("6a12e1f60cd69f40a8a6eb26620de05eeb59ee63559b5cbc667053c01d7d382f", 19, 4, ("data", "code"), 56),
    "tkpkg.amigaos.encode_service": ("c6da4d60d638389ed0088dac042dc29fcddc48313eabaf5d4004c59d8f32cd34", 12, 3, ("data", "code"), 11),
    "tkpkg.amigaos.runtime_context": ("9b1e03e3b8689dfe72157e04da3441cf595a8e21b4dac19e55700effb0a6bace", 8, 1, ("code", "bss"), 19),
    "tkpkg.amigaos.engine_context_adapter": ("a06328ca23472b6f624f579ffd24e09d51d3373acd0c2e825e2596086461ea54", 9, 1, ("code",), 0),
    "opasm.amigaos.engine": ("c9ef239f8330c4e97ad919441a513175e2f2978faca5f518e5a03694207f9aa8", 82, 1, ("code", "bss"), 22),
    "tkpkg.amigaos.tokenizer_vm": ("7bbafa635dcded0236c9a65368db47e0e10aded6b328d4389e580654125e5b65", 31, 5, ("data", "code"), 124),
    "opcore.amigaos.expr_bridge": ("1ee19dbd7cc1249921e0bb006f2bfb67b04d21c0a0c6b126e042b08b3b9f705a", 30, 1, ("code", "bss"), 5),
    "prvm.amigaos.runtime": ("d49fad6cedf4807ffed62932c3e55b8f1adbfba0ff93bd8085d66b4e42efb74b", 20, 0, ("data", "code"), 37),
    "tkpkg.amigaos.pipeline": ("d161f2cad5c379e2610c7c44294ef170538beceae92790019f559e6b0550b9ab", 35, 3, ("data", "code"), 19),
    "opasm.amigaos.flow_text_encoding": ("954eb2582ad5d90550efe5d620c07ffc7411b149e318aaf8626a3f09cf611666", 16, 0, ("code", "bss", "data"), 0),
}


def extract_inventory(source_text: str) -> tuple[list[str], list[str], list[str], list[str]]:
    """Extract the complete static Item 5.2 surface from one assembly module."""
    blocks = re.findall(r"^([A-Za-z_][A-Za-z0-9_]*)\s+\.block\b", source_text, re.MULTILINE)
    imports = re.findall(r"^\s*\.use\s+([^\s;]+)", source_text, re.MULTILINE)
    sections = re.findall(r"^\s*\.section\s+([^,\s]+)", source_text, re.MULTILINE)
    diagnostics = [
        f"{line_number}: {line.strip()}"
        for line_number, line in enumerate(source_text.splitlines(), start=1)
        if re.search(r"(?:diag|debug|error|event|status)", line, re.IGNORECASE)
    ]
    return blocks, imports, sections, diagnostics


def validate(root: Path = ROOT) -> list[str]:
    inventory = root / INVENTORY.relative_to(ROOT)
    errors: list[str] = []
    if not inventory.exists():
        return [f"missing inventory: {inventory.relative_to(root)}"]
    inventory_text = inventory.read_text(encoding="utf-8")
    for module, (relative, entry, imports) in TARGETS.items():
        source = root / relative
        if not source.exists():
            errors.append(f"{module}: missing source {relative}")
            continue
        source_text = source.read_text(encoding="utf-8")
        blocks, actual_imports, sections, diagnostics = extract_inventory(source_text)
        if f".module {module}" not in source_text:
            errors.append(f"{module}: source module declaration missing")
        if entry not in source_text:
            errors.append(f"{module}: representative public entry `{entry}` missing")
        if f"`{module}`" not in inventory_text or f"`{relative}`" not in inventory_text:
            errors.append(f"{module}: inventory section/source citation missing")
        for imported in imports:
            if f".use {imported}" not in source_text:
                errors.append(f"{module}: expected import `{imported}` missing")
        expected_hash, expected_blocks, expected_imports, expected_sections, expected_diagnostics = SNAPSHOTS[module]
        actual_hash = hashlib.sha256(source_text.encode()).hexdigest()
        if actual_hash != expected_hash:
            errors.append(f"{module}: source changed; refresh the complete Item 5.2 manifest")
        if (len(blocks), len(actual_imports), tuple(sections), len(diagnostics)) != (
            expected_blocks,
            expected_imports,
            expected_sections,
            expected_diagnostics,
        ):
            errors.append(f"{module}: complete routine/import/state/diagnostic inventory drifted")
    for required in (
        "Orchestration versus semantics",
        "Direct cross-subsystem state",
        "Segment and statement landing points",
    ):
        if required not in inventory_text:
            errors.append(f"inventory: missing mandatory finding `{required}`")
    return errors


def report(root: Path = ROOT) -> None:
    """Print the complete checked inventory for human review and evidence capture."""
    for module, (relative, _entry, _imports) in TARGETS.items():
        source_text = (root / relative).read_text(encoding="utf-8")
        blocks, imports, sections, diagnostics = extract_inventory(source_text)
        print(f"## {module}")
        print(f"source: {relative}")
        print("routines: " + ", ".join(blocks))
        print("imports: " + ", ".join(imports) if imports else "imports: (none)")
        print("state sections: " + ", ".join(sections))
        print("diagnostic paths:")
        for diagnostic in diagnostics:
            print(f"- {diagnostic}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--report", action="store_true", help="print the complete source inventory after validation")
    args = parser.parse_args()
    errors = validate()
    if errors:
        print("native runtime boundary inventory: FAIL")
        for error in errors:
            print(f"  - {error}")
        return 1
    print("native runtime boundary inventory: PASS")
    if args.report:
        report()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

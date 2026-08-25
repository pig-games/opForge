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
            "tkpkg.amigaos.compact_table",
        ),
    ),
    "tkpkg.amigaos.compact_table": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_compact_table.asm",
        "findFixedProgramFromRequestV1",
        (
            "tkpkg.amigaos.abi",
            "tkpkg.amigaos.buffers",
            "tkpkg.amigaos.selection_service",
        ),
    ),
    "tkpkg.amigaos.operand_record_service": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_operand_record_service.asm",
        "executeRequestV1",
        (
            "tkpkg.amigaos.abi",
            "tkpkg.amigaos.buffers",
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
    "opasm.amigaos.assembly_driver": ("2d4f39466309574a764f2cd69564fd30a48a9f3180f96a7edacd94922f4705fd", 78, 18, ("code", "data", "bss"), 149),
    "opasm.amigaos.directive_router": ("b1011828f566a3896329cbda6d491379ee67f17888a5fb432e28aca15cce2339", 3, 0, ("code", "data"), 1),
    "opasm.amigaos.operand_eval": ("ae3c196bbfb545f49f703539e4d7aac40686a0158dea316a9b65572f71864f5e", 17, 3, ("code", "bss"), 9),
    "opasm.amigaos.directive_data": ("ae827acedfad611d1d57ff93783edda61b6eb4de332c2907521343d6f9fdf3bc", 2, 3, ("code", "bss"), 19),
    "opasm.amigaos.directive_text": ("98d283b9678f051c68787dc915b841559ee1a3647e597b44d966492a931ff0c8", 2, 1, ("code", "bss"), 2),
    "opasm.amigaos.layout": ("319155f2c0d107b187c27ee0abcd218a731dae8d6f5f99f3111c6010d9faed2d", 53, 1, ("code", "bss"), 2),
    "tkpkg.amigaos.service": ("ee73bb8b17e459daf2edec91e717b39e48f3e75d9ccb0e2b3f26d87bc6370522", 42, 13, ("data", "bss", "code"), 117),
    "tkpkg.amigaos.selection_service": ("4f5546e9dc2a8589470b652b969160a1f148c1e63025bf13a20303fc168cbaa8", 34, 6, ("data", "code"), 89),
    "tkpkg.amigaos.operand_runtime": ("3e46e941f132d16b90fc03bbb1f4a9070d80ee4c514abad9069d03148d9ec54f", 21, 4, ("data", "code"), 59),
    "tkpkg.amigaos.encode_service": ("e03d46d7052c7775e01e7918bdd1e8ca7a1eca36eaa8b5f16632b71a2542e1ee", 19, 5, ("data", "code"), 18),
    "tkpkg.amigaos.compact_table": ("b6b9f451b8f9449916d7079af3563288acec06756dc3838c8489ee2a7b3d7c89", 5, 3, ("data", "code"), 1),
    "tkpkg.amigaos.operand_record_service": ("87dcfbc85d498b14021c638ee62a0ddf3f845e0dba0d7296f12733bdd6113788", 27, 2, ("data", "bss", "code"), 13),
    "tkpkg.amigaos.runtime_context": ("9b1e03e3b8689dfe72157e04da3441cf595a8e21b4dac19e55700effb0a6bace", 8, 1, ("code", "bss"), 19),
    "tkpkg.amigaos.engine_context_adapter": ("a06328ca23472b6f624f579ffd24e09d51d3373acd0c2e825e2596086461ea54", 9, 1, ("code",), 0),
    "opasm.amigaos.engine": ("28a8d5f3b274ee8396b431680e01ccd3068261305fd008522e2e1dc71b5a9fe9", 84, 1, ("code", "bss"), 22),
    "tkpkg.amigaos.tokenizer_vm": ("7bbafa635dcded0236c9a65368db47e0e10aded6b328d4389e580654125e5b65", 31, 5, ("data", "code"), 124),
    "opcore.amigaos.expr_bridge": ("f8c22759e6ebb88cfe593f4573e962591df9b80b01e5b0919065ac48b1f51cb2", 32, 1, ("code", "bss"), 6),
    "prvm.amigaos.runtime": ("d49fad6cedf4807ffed62932c3e55b8f1adbfba0ff93bd8085d66b4e42efb74b", 20, 0, ("data", "code"), 37),
    "tkpkg.amigaos.pipeline": ("d60652c3a5a66dcaa37e0df985db5cd65fbbbccd4359140451f490ecfcd6f73f", 38, 3, ("data", "code"), 19),
    "opasm.amigaos.flow_text_encoding": ("17fd0bac93c8e91ce9355ac37b9b1bfcb7afc428320e164884e49a19dc892bc0", 16, 0, ("code", "bss", "data"), 0),
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

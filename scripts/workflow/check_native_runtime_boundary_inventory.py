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
        (
            "opasm.amigaos.directive_router",
            "opasm.amigaos.engine",
            "opasm.amigaos.tkpkg_bridge",
            "tkpkg.amigaos.state_service",
            "debug.amigaos.symbol_expr_profile",
        ),
    ),
    "opasm.amigaos.directive_router": (
        "native/motorola68000/amigaos/opasm/opasm_directive_router.asm",
        "classifyV1",
        (),
    ),
    "opasm.amigaos.operand_eval": (
        "native/motorola68000/amigaos/opasm/opasm_operand_eval.asm",
        "prepareSelectedRequestV1",
        (
            "opasm.amigaos.callback_abi",
            "opasm.amigaos.engine",
            "opasm.amigaos.flow_scopes",
            "debug.amigaos.symbol_expr_profile",
        ),
    ),
    "opasm.amigaos.directive_data": (
        "native/motorola68000/amigaos/opasm/opasm_directive_data.asm",
        "emitNumericDirectiveV1",
        ("opasm.amigaos.engine", "tkpkg.amigaos.runtime_context"),
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
        (
            "opcore.amigaos.expr_bridge",
            "tkpkg.amigaos.pipeline",
            "debug.amigaos.runtime_profile",
        ),
    ),
    "tkpkg.amigaos.selection_service": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_selection_service.asm",
        "selectInstructionV1",
        (
            "tkpkg.amigaos.runtime_context",
            "tkpkg.amigaos.state_service",
            "opcore.amigaos.expr_bridge",
            "debug.amigaos.runtime_profile",
        ),
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
            "debug.amigaos.runtime_profile",
        ),
    ),
    "tkpkg.amigaos.compact_table": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_compact_table.asm",
        "find",
        (
            "tkpkg.amigaos.abi",
            "tkpkg.amigaos.buffers",
            "tkpkg.amigaos.selection_service",
            "tkpkg.amigaos.compact_prepare",
            "debug.amigaos.runtime_profile",
        ),
    ),
    "tkpkg.amigaos.compact_prepare": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_compact_prepare.asm",
        "prepare",
        (
            "tkpkg.amigaos.buffers",
            "tkpkg.amigaos.selection_service",
            "debug.amigaos.runtime_profile",
        ),
    ),
    "tkpkg.amigaos.operand_record_service": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_operand_record_service.asm",
        "executeRequestV1",
        (
            "tkpkg.amigaos.abi",
            "tkpkg.amigaos.buffers",
            "debug.amigaos.runtime_profile",
        ),
    ),
    "tkpkg.amigaos.runtime_context": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_runtime_context.asm",
        "getAbiVersionV1",
        ("tkpkg.amigaos.engine_context_adapter", "tkpkg.amigaos.state_service"),
    ),
    "tkpkg.amigaos.engine_context_adapter": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_engine_context_adapter.asm",
        "lookupSymbolV1",
        ("opasm.amigaos.engine",),
    ),
    "opasm.amigaos.engine": (
        "native/motorola68000/amigaos/opasm/opasm_engine.asm",
        "initSessionV1",
        (
            "opasm.amigaos.events",
            "opasm.amigaos.progress",
            "debug.amigaos.symbol_expr_profile",
            "debug.amigaos.platform_profile",
        ),
    ),
    "tkpkg.amigaos.tokenizer_vm": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_tokenizer_vm.asm",
        "tkpkgTokenizerVmTokenizeLineV1",
        ("tkvm.amigaos.runtime",),
    ),
    "opcore.amigaos.expr_bridge": (
        "native/motorola68000/amigaos/opcore/opcore_expr_bridge.asm",
        "opcoreExprEvalOperandV1",
        (
            "exprvm.amigaos.runtime",
            "debug.amigaos.symbol_expr_profile",
            "debug.amigaos.runtime_profile",
        ),
    ),
    "exprvm.amigaos.i64_math": (
        "native/motorola68000/amigaos/exprvm/exprvm_i64_math.asm",
        "multiplyV1",
        (),
    ),
    "prvm.amigaos.runtime": (
        "native/motorola68000/amigaos/prvm/prvm_runtime.asm",
        "prvmRun68000",
        ("debug.amigaos.runtime_profile",),
    ),
    "tkpkg.amigaos.pipeline": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm",
        "tkpkgPipelineSetActiveV1",
        (
            "tkpkg.amigaos.token_policy",
            "tkpkg.amigaos.state_service",
            "tkpkg.amigaos.compact_table",
        ),
    ),
    "tkpkg.amigaos.state_service": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_state_service.asm",
        "initializeActiveV1",
        ("tkpkg.amigaos.buffers", "debug.amigaos.runtime_profile"),
    ),
    "opasm.amigaos.flow_text_encoding": (
        "native/motorola68000/amigaos/opasm/opasm_flow_text_encoding.asm",
        "resetStateV1",
        (),
    ),
}
SNAPSHOTS = {
    "opasm.amigaos.assembly_driver": ('c1094d0332b53c8be957a58d848ef296c42f807b181753c1db71a122347bc050', 97, 23, ('code', 'data', 'bss'), 177),
    "opasm.amigaos.directive_router": ("10c2b66e9ae47150f9394679b0dae8725b6b13ee5208970b01f11b5bcc8cf4c2", 3, 0, ("code", "data"), 1),
    "opasm.amigaos.operand_eval": ('64391b93de1bb36849a43d31c9b32ff295426f429d99cab0bb1788b8a7c33ef3', 27, 4, ('code', 'bss'), 14),
    "opasm.amigaos.directive_data": ('b63509a744dd18a99dd48656af6fb20dc333e47648832eb5eb7137398866ae60', 2, 4, ('code', 'bss'), 19),
    "opasm.amigaos.directive_text": ("98d283b9678f051c68787dc915b841559ee1a3647e597b44d966492a931ff0c8", 2, 1, ("code", "bss"), 2),
    "opasm.amigaos.layout": ('8c2e0e3353da78c09c0cc005cc8fc32fe0e7a11d33c705020a81d38c156d0646', 65, 2, ('code', 'bss'), 3),
    "tkpkg.amigaos.service": ('8144838404c499305b79fd2d28a16163d69a0b79ef404b9bb98f8b105e623631', 42, 14, ('data', 'bss', 'code'), 118),
    "tkpkg.amigaos.selection_service": ('6364ab10c21630c0e30c7063da58610bd4d85ba394bfdce105f8df7f7247d900', 86, 8, ('data', 'code'), 233),
    "tkpkg.amigaos.operand_runtime": ("8ccf698f85b2fa2e675109ab119f8918c478fb3bc94f661980a14d0fee1d65d8", 23, 5, ("data", "code"), 60),
    "tkpkg.amigaos.encode_service": ('84fa5d212e61a2510c63fc92276fd60127e1fa994373f633febf67320f607288', 35, 7, ('data', 'bss', 'code'), 39),
    "tkpkg.amigaos.compact_table": ('495b7a6dccdb5e5631f25769d671c460ac0af606513595246fcdb4ce03901d88', 5, 5, ('data', 'bss', 'code'), 1),
    "tkpkg.amigaos.compact_prepare": ('58d7c18e2613c4a693b950c7d21925e8b443251ffeacc64e5768a91c659f2ee7', 7, 3, ('data', 'bss', 'code'), 4),
    "tkpkg.amigaos.operand_record_service": ("ca1211dcfe387d26304b0bc29ebfebc1f70f5824efe0442863dbf188a6cd78b2", 27, 3, ("data", "bss", "code"), 14),
    "tkpkg.amigaos.runtime_context": ('dd070f06b891604963876064267209be0beeab21cbbed83cebc89d8a2037cfcb', 15, 3, ('code', 'bss'), 19),
    "tkpkg.amigaos.engine_context_adapter": ("e6dde6ed66b083488f8ff5911a6a1aca7d24d73f7488eefdb208d5aec4f01504", 11, 1, ("code",), 0),
    "opasm.amigaos.engine": ("ad02f0cdd333cedae7d8315f0fbb09421403d4730fbf9a6a4580be1429eee8ef", 106, 4, ("code", "bss"), 37),
    "tkpkg.amigaos.tokenizer_vm": ("7bbafa635dcded0236c9a65368db47e0e10aded6b328d4389e580654125e5b65", 31, 5, ("data", "code"), 124),
    "opcore.amigaos.expr_bridge": ('37c664c2db68e549fbcedde2b8d595cc42d1333b6282774064a7f0dd78b19917', 35, 3, ('code', 'bss'), 16),
    "exprvm.amigaos.i64_math": ("a2aab311913ced26dd94eedac949ab019d7eac91f7b6723014e2ce667179ef8c", 4, 0, ("code",), 0),
    "prvm.amigaos.runtime": ("ab6aee2ef4ba63d13ad6f98bb16102c57b2e7ef17779dd6fdd59343dbcd6eec6", 20, 1, ("data", "code"), 38),
    "tkpkg.amigaos.pipeline": ('7478d94f4b1ef54e623d2a2a70118174a84d258b77d82faf2adbc88119b0fa40', 40, 6, ('data', 'code'), 20),
    "tkpkg.amigaos.state_service": ("85ba591d2904a184a3ae8f1985321e1de969662d8c585ce3b6a2b4063e31ab40", 21, 2, ("data", "code", "bss"), 15),
    "opasm.amigaos.flow_text_encoding": ("17fd0bac93c8e91ce9355ac37b9b1bfcb7afc428320e164884e49a19dc892bc0", 16, 0, ("code", "bss", "data"), 0),
}


def extract_inventory(source_text: str, root: Path = ROOT) -> tuple[list[str], list[str], list[str], list[str]]:
    """Extract the complete static Item 5.2 surface from one assembly module."""
    blocks = re.findall(r"^([A-Za-z_][A-Za-z0-9_]*)\s+\.block\b", source_text, re.MULTILINE)
    imports = re.findall(r"^\s*\.use\s+([^\s;]+)", source_text, re.MULTILINE)
    # Telemetry owns its conditional imports so call sites stay declarative.
    # Include those dependencies without mistaking macro bodies for routines.
    if re.search(r'^\s*\.include "telemetry_macros\.i"', source_text, re.MULTILINE):
        telemetry = root / "native/motorola68000/amigaos/debug/telemetry_macros.i"
        if telemetry.exists():
            imports.extend(re.findall(r"^\s*\.use\s+([^\s;]+)", telemetry.read_text(), re.MULTILINE))
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
        blocks, actual_imports, sections, diagnostics = extract_inventory(source_text, root)
        if f".module {module}" not in source_text:
            errors.append(f"{module}: source module declaration missing")
        if entry not in source_text:
            errors.append(f"{module}: representative public entry `{entry}` missing")
        if f"`{module}`" not in inventory_text or f"`{relative}`" not in inventory_text:
            errors.append(f"{module}: inventory section/source citation missing")
        for imported in imports:
            if imported not in actual_imports:
                errors.append(f"{module}: expected import `{imported}` missing")
        expected_hash, expected_blocks, expected_imports, expected_sections, expected_diagnostics = SNAPSHOTS[module]
        actual_hash = hashlib.sha256(source_text.encode()).hexdigest()
        if actual_hash != expected_hash:
            errors.append(f"{module}: source changed; review and refresh the complete source inventory")
        if (len(blocks), len(actual_imports), tuple(sections), len(diagnostics)) != (
            expected_blocks,
            expected_imports,
            expected_sections,
            expected_diagnostics,
        ):
            errors.append(f"{module}: complete routine/import/state/diagnostic inventory drifted")
    return errors


def report(root: Path = ROOT) -> None:
    """Print the complete checked inventory for human review and evidence capture."""
    for module, (relative, _entry, _imports) in TARGETS.items():
        source_text = (root / relative).read_text(encoding="utf-8")
        blocks, imports, sections, diagnostics = extract_inventory(source_text, root)
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

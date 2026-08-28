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
        (
            "tkpkg.amigaos.runtime_context",
            "tkpkg.amigaos.state_service",
            "opcore.amigaos.expr_bridge",
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
        ("tkpkg.amigaos.token_policy", "tkpkg.amigaos.state_service"),
    ),
    "tkpkg.amigaos.state_service": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_state_service.asm",
        "initializeActiveV1",
        ("tkpkg.amigaos.buffers",),
    ),
    "opasm.amigaos.flow_text_encoding": (
        "native/motorola68000/amigaos/opasm/opasm_flow_text_encoding.asm",
        "resetStateV1",
        (),
    ),
}
SNAPSHOTS = {
    "opasm.amigaos.assembly_driver": ("f095bd3175eae050ce8a982c7f40f06d76abb8ca6892b77e69322c426c548545", 89, 21, ("code", "data", "bss"), 157),
    "opasm.amigaos.directive_router": ("10c2b66e9ae47150f9394679b0dae8725b6b13ee5208970b01f11b5bcc8cf4c2", 3, 0, ("code", "data"), 1),
    "opasm.amigaos.operand_eval": ("668152a1957a88d457983850738d6c77269e2fd9116e45470165f6fa145c4896", 23, 3, ("code", "bss"), 9),
    "opasm.amigaos.directive_data": ("ae827acedfad611d1d57ff93783edda61b6eb4de332c2907521343d6f9fdf3bc", 2, 3, ("code", "bss"), 19),
    "opasm.amigaos.directive_text": ("98d283b9678f051c68787dc915b841559ee1a3647e597b44d966492a931ff0c8", 2, 1, ("code", "bss"), 2),
    "opasm.amigaos.layout": ("ea615bcdd86ec311f93f81ac95cbb9cfe8ac323aa7a0c3eac004bd2e37075740", 60, 1, ("code", "bss"), 2),
    "tkpkg.amigaos.service": ("ee73bb8b17e459daf2edec91e717b39e48f3e75d9ccb0e2b3f26d87bc6370522", 42, 13, ("data", "bss", "code"), 117),
    "tkpkg.amigaos.selection_service": ("daf8134456f72e986a553185bbe29dcf327b37b8551a9fe0ec8e958e04458c9d", 85, 7, ("data", "code"), 221),
    "tkpkg.amigaos.operand_runtime": ("c13b6a0676d3640c89c44074e4e09dadeeaf145ab0687f3e37382b69fae0c1e2", 23, 4, ("data", "code"), 59),
    "tkpkg.amigaos.encode_service": ("35a0b9fe2fa9f35f3f92a5275a83d3bb7f25d3d67f70b06fef3069482af8bb32", 33, 5, ("data", "code"), 35),
    "tkpkg.amigaos.compact_table": ("ec94a6cc88fa889b8751c57806ad92227ce64a1492d8fc5c94c49c744175d507", 5, 3, ("data", "code"), 1),
    "tkpkg.amigaos.operand_record_service": ("7d703b307b57c2bbf44d7ab0465be9e6fae33853a8d45b7d90479c2f71dd68ff", 27, 2, ("data", "bss", "code"), 13),
    "tkpkg.amigaos.runtime_context": ("b7a4227d205c7af32c3fbb4fa23721047afa03acaef3475dab1f815de2b5c137", 11, 2, ("code", "bss"), 19),
    "tkpkg.amigaos.engine_context_adapter": ("e6dde6ed66b083488f8ff5911a6a1aca7d24d73f7488eefdb208d5aec4f01504", 11, 1, ("code",), 0),
    "opasm.amigaos.engine": ("3b1311963d8a6011001c0a019c7403280d78d9a796f58cf455315be8255b1555", 89, 1, ("code", "bss"), 22),
    "tkpkg.amigaos.tokenizer_vm": ("7bbafa635dcded0236c9a65368db47e0e10aded6b328d4389e580654125e5b65", 31, 5, ("data", "code"), 124),
    "opcore.amigaos.expr_bridge": ("f8c22759e6ebb88cfe593f4573e962591df9b80b01e5b0919065ac48b1f51cb2", 32, 1, ("code", "bss"), 6),
    "prvm.amigaos.runtime": ("d49fad6cedf4807ffed62932c3e55b8f1adbfba0ff93bd8085d66b4e42efb74b", 20, 0, ("data", "code"), 37),
    "tkpkg.amigaos.pipeline": ("c98f344cdc0ccf11636d50026e30053c80736aa11fda3d1d01d70f24c759eca3", 38, 4, ("data", "code"), 19),
    "tkpkg.amigaos.state_service": ("91af79921ac4ddbcbf476d1ab41172c6e6c952d704631fc78c4d833bb9046625", 21, 1, ("data", "code", "bss"), 14),
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

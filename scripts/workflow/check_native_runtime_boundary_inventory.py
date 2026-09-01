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
    "opasm.amigaos.assembly_driver": ("22e84545d8bf58c39b29cdc86489d3f0d682adfd9000dc436244436fb6e6393c", 91, 21, ("code", "data", "bss"), 171),
    "opasm.amigaos.directive_router": ("10c2b66e9ae47150f9394679b0dae8725b6b13ee5208970b01f11b5bcc8cf4c2", 3, 0, ("code", "data"), 1),
    "opasm.amigaos.operand_eval": ("1dc138178b8515cd834cd8adec94db87a050e273fbb9726211fe7ac4ef1d2273", 26, 3, ("code", "bss"), 13),
    "opasm.amigaos.directive_data": ("2156dbc893b0d1e380e17b6e90be6d0368bec6c36d01086783747a40e2834d0c", 2, 3, ("code", "bss"), 19),
    "opasm.amigaos.directive_text": ("98d283b9678f051c68787dc915b841559ee1a3647e597b44d966492a931ff0c8", 2, 1, ("code", "bss"), 2),
    "opasm.amigaos.layout": ("173fe7da420697d6ecdb63fa42b937c3c90735c3299e27b186583169aced441f", 61, 1, ("code", "bss"), 2),
    "tkpkg.amigaos.service": ("8203dc6fd183d88b25825a450a667547829d23b8c7534b2359cc5bfe5a0777ad", 42, 13, ("data", "bss", "code"), 117),
    "tkpkg.amigaos.selection_service": ("0490a3bffa26fcd5f3b4094007971a4651d3f0a41c9e77dc0482edbfa6b8d44a", 86, 7, ("data", "code"), 232),
    "tkpkg.amigaos.operand_runtime": ("c44ebcf3fc1861a6785bfbbc6c574dce3528e9e9f513af1036725f30cf6c952c", 23, 4, ("data", "code"), 59),
    "tkpkg.amigaos.encode_service": ("151880bedfb5b04a0de5d41468e129ef7ca01c5fb79786ca14b838bab2d54e54", 33, 5, ("data", "code"), 35),
    "tkpkg.amigaos.compact_table": ("ec94a6cc88fa889b8751c57806ad92227ce64a1492d8fc5c94c49c744175d507", 5, 3, ("data", "code"), 1),
    "tkpkg.amigaos.operand_record_service": ("7d703b307b57c2bbf44d7ab0465be9e6fae33853a8d45b7d90479c2f71dd68ff", 27, 2, ("data", "bss", "code"), 13),
    "tkpkg.amigaos.runtime_context": ("ac74b0113e59faf4aa303cc08c359ad41b1f174328cd7f8a8dae852660284aad", 12, 2, ("code", "bss"), 19),
    "tkpkg.amigaos.engine_context_adapter": ("e6dde6ed66b083488f8ff5911a6a1aca7d24d73f7488eefdb208d5aec4f01504", 11, 1, ("code",), 0),
    "opasm.amigaos.engine": ("0753ab1faf22a465aa7015105c0634560aaf37a61d344e0b9c88de299f60631a", 100, 1, ("code", "bss"), 22),
    "tkpkg.amigaos.tokenizer_vm": ("7bbafa635dcded0236c9a65368db47e0e10aded6b328d4389e580654125e5b65", 31, 5, ("data", "code"), 124),
    "opcore.amigaos.expr_bridge": ("a4121f1f41ac2653598e63b17e9e0340a681dda8fc693b8899f200adbcc9fc71", 32, 1, ("code", "bss"), 7),
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

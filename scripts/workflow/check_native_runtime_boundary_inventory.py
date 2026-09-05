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
    "prvm.amigaos.runtime": (
        "native/motorola68000/amigaos/prvm/prvm_runtime.asm",
        "prvmRun68000",
        ("debug.amigaos.runtime_profile",),
    ),
    "tkpkg.amigaos.pipeline": (
        "native/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm",
        "tkpkgPipelineSetActiveV1",
        ("tkpkg.amigaos.token_policy", "tkpkg.amigaos.state_service"),
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
    "opasm.amigaos.assembly_driver": ("22527fd0123472a9d3e58bfb0462abcf167bf0844497cbfa0843feaa0af4d785", 92, 22, ("code", "data", "bss"), 172),
    "opasm.amigaos.directive_router": ("10c2b66e9ae47150f9394679b0dae8725b6b13ee5208970b01f11b5bcc8cf4c2", 3, 0, ("code", "data"), 1),
    "opasm.amigaos.operand_eval": ("6046bb89d5fc063a736fea2bd6707b7d183a75c8db808734e8935908b124a62c", 27, 4, ("code", "bss"), 14),
    "opasm.amigaos.directive_data": ("2156dbc893b0d1e380e17b6e90be6d0368bec6c36d01086783747a40e2834d0c", 2, 3, ("code", "bss"), 19),
    "opasm.amigaos.directive_text": ("98d283b9678f051c68787dc915b841559ee1a3647e597b44d966492a931ff0c8", 2, 1, ("code", "bss"), 2),
    "opasm.amigaos.layout": ("34973d8dfba917cd8aa2a39b15211a3246f97589f484c8a5675a3a0720fbe686", 61, 2, ("code", "bss"), 3),
    "tkpkg.amigaos.service": ("a06f2b3bb2b11835754d6cbc1af7aafc18f49df44a7c8d6a8b9e6370491946f5", 42, 14, ("data", "bss", "code"), 118),
    "tkpkg.amigaos.selection_service": ("dda348947ac2505b7fbb3b26bb4a10248c5f9112074608c45556b5a86112fc00", 86, 8, ("data", "code"), 233),
    "tkpkg.amigaos.operand_runtime": ("8ccf698f85b2fa2e675109ab119f8918c478fb3bc94f661980a14d0fee1d65d8", 23, 5, ("data", "code"), 60),
    "tkpkg.amigaos.encode_service": ("3161d50e60d5ea92cc2853bdd7cea5ceefe7f3318b2e9a305822672379cacc7a", 33, 6, ("data", "code"), 36),
    "tkpkg.amigaos.compact_table": ("ec94a6cc88fa889b8751c57806ad92227ce64a1492d8fc5c94c49c744175d507", 5, 3, ("data", "code"), 1),
    "tkpkg.amigaos.operand_record_service": ("ca1211dcfe387d26304b0bc29ebfebc1f70f5824efe0442863dbf188a6cd78b2", 27, 3, ("data", "bss", "code"), 14),
    "tkpkg.amigaos.runtime_context": ("ac74b0113e59faf4aa303cc08c359ad41b1f174328cd7f8a8dae852660284aad", 12, 2, ("code", "bss"), 19),
    "tkpkg.amigaos.engine_context_adapter": ("e6dde6ed66b083488f8ff5911a6a1aca7d24d73f7488eefdb208d5aec4f01504", 11, 1, ("code",), 0),
    "opasm.amigaos.engine": ("d608a3efddc98d9395e7b78508cdbcf1fb91e7464c643718fb760bd87e769b0c", 102, 4, ("code", "bss"), 35),
    "tkpkg.amigaos.tokenizer_vm": ("7bbafa635dcded0236c9a65368db47e0e10aded6b328d4389e580654125e5b65", 31, 5, ("data", "code"), 124),
    "opcore.amigaos.expr_bridge": ("2c81137868fcac93ab32f0b8fc3d1b3d895bf7cd2c7c45bf8da318897ed4ecc1", 33, 3, ("code", "bss"), 17),
    "prvm.amigaos.runtime": ("ab6aee2ef4ba63d13ad6f98bb16102c57b2e7ef17779dd6fdd59343dbcd6eec6", 20, 1, ("data", "code"), 38),
    "tkpkg.amigaos.pipeline": ("c98f344cdc0ccf11636d50026e30053c80736aa11fda3d1d01d70f24c759eca3", 38, 4, ("data", "code"), 19),
    "tkpkg.amigaos.state_service": ("85ba591d2904a184a3ae8f1985321e1de969662d8c585ce3b6a2b4063e31ab40", 21, 2, ("data", "code", "bss"), 15),
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

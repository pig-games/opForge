#!/usr/bin/env python3
"""
Detect and optionally fix redundant native Motorola 68000 `tst.* Dn` instructions.

Default mode is report-only.

The safe auto-fix target is deliberately narrow:

    <CCR-setting write to Dn>
    tst.<same width> Dn
    <conditional branch> target

The rewrite deletes only the `tst.* Dn` line.

It also reports, but does not auto-fix, suspicious call-return tests:

    bsr.w helper
    tst.l d0
    bmi.w fail

Those require callee ABI/CCR contract knowledge unless the callee is in the
reviewed allowlist below.
"""

from __future__ import annotations

import argparse
import collections
import dataclasses
import re
import sys
from pathlib import Path
from typing import Sequence


DEFAULT_SCAN_ROOT = Path("native/motorola68000")

DATA_REG_RE = r"[dD][0-7]"
WIDTH_RE = r"[bBwWlL]"

COND_BRANCH_RE = re.compile(
    r"^\s*b(?:ra|hi|ls|cc|cs|ne|eq|vc|vs|pl|mi|ge|lt|gt|le|hs|lo)\s*(?:\.[sbwlSBWL])?\b"
)

CALL_RE = re.compile(r"^\s*(?:bsr|jsr)\s*(?:\.[sbwlSBWL])?\b")
CALL_TARGET_RE = re.compile(
    r"^\s*(?:bsr|jsr)\s*(?:\.[sbwlSBWL])?\s+(?P<target>[^\s;]+)"
)

TST_DATA_RE = re.compile(
    rf"^\s*tst\.(?P<width>{WIDTH_RE})\s+(?P<reg>{DATA_REG_RE})\s*(?:;.*)?$",
    re.IGNORECASE,
)
TST_ANY_RE = re.compile(
    rf"^\s*tst\.(?P<width>{WIDTH_RE})\s+(?P<operand>[^;]+?)\s*(?:;.*)?$",
    re.IGNORECASE,
)

LABEL_RE = re.compile(r"^\s*[A-Za-z_.$][A-Za-z0-9_.$]*\s*:")
DIRECTIVE_RE = re.compile(r"^\s*\.[A-Za-z]")
COMMENT_OR_BLANK_RE = re.compile(r"^\s*(?:;.*)?$")
BLOCK_LABEL_RE = re.compile(r"^([A-Za-z0-9_.$]+)\t\.block$")
SIGNIFICANT_LINE_RE = re.compile(r"^\s*(?!;)(?:.+\S)?$")
INSTRUCTION_RE = re.compile(r"^\s*[A-Za-z]")
SUSPICIOUS_CCR_CLEANUP_RE = re.compile(
    r"^\s*(?:move\.[bwl]\s+\(sp\)\+,|movea\.l\s+\(sp\)\+,|addq\.[wl]\s+#\d+,\s*sp\b|lea\s+[^,]+\(sp\),\s*sp\b)",
    re.IGNORECASE,
)
EXPLICIT_D0_SET_RE = re.compile(
    rf"^\s*(?:moveq\s+#?[^,]+,\s*d0|clr\.{WIDTH_RE}\s+d0|ext\.[wl]\s+d0|move\.{WIDTH_RE}\s+.+,\s*d0)\b",
    re.IGNORECASE,
)
LENGTH_COUNT_HINT_RE = re.compile(
    r"\b(?:len|length|count|bytes|size|capacity|remaining|remain|total|used|available|limit|index)\b",
    re.IGNORECASE,
)

INVENTORY_CATEGORY_POST_WRITE_REDUNDANT = "TST-POST-WRITE-REDUNDANT"
INVENTORY_CATEGORY_POST_CALL_REVIEWED = "TST-POST-CALL-REVIEWED"
INVENTORY_CATEGORY_POST_CALL_RETAINED = "TST-POST-CALL-RETAINED"
INVENTORY_CATEGORY_POST_CALL_UNKNOWN = "TST-POST-CALL-UNKNOWN"
INVENTORY_CATEGORY_INDIRECT_CALL_BOUNDARY = "TST-INDIRECT-CALL-BOUNDARY"
INVENTORY_CATEGORY_MEMORY_PROBE = "TST-MEMORY-PROBE"
INVENTORY_CATEGORY_LENGTH_OR_COUNT_PROBE = "TST-LENGTH-OR-COUNT-PROBE"
INVENTORY_CATEGORY_WIDTH_SENSITIVE = "TST-WIDTH-SENSITIVE"
INVENTORY_CATEGORY_SEMANTIC_REGISTER_PROBE = "TST-SEMANTIC-REGISTER-PROBE"
INVENTORY_CATEGORY_UNCLASSIFIED = "TST-UNCLASSIFIED"

INVENTORY_SUSPICIOUS_CATEGORIES = frozenset(
    {
        INVENTORY_CATEGORY_POST_CALL_UNKNOWN,
        INVENTORY_CATEGORY_WIDTH_SENSITIVE,
        INVENTORY_CATEGORY_SEMANTIC_REGISTER_PROBE,
        INVENTORY_CATEGORY_UNCLASSIFIED,
    }
)


# Reviewed helpers whose contracts are known to return with CCR reflecting D0.
# Call sites may branch directly on D0 after these helpers without an extra tst.
# Width sets stay explicit so we only autofix widths we have reviewed for each
# helper or alias.
REVIEWED_CCR_D0_CALL_TST_WIDTHS = {
    "module_use.opforgeNativeCliRecordImportSelect": frozenset({"l"}),
    "control.tkvmSetStepBudget68000": frozenset({"l"}),
    "currentTokenPtr": frozenset({"l"}),
    "emitBeginStatement": frozenset({"l"}),
    "emitDirectiveText": frozenset({"l"}),
    "emitFinishLine": frozenset({"l"}),
    "emitLabelText": frozenset({"l"}),
    "emitMnemonicText": frozenset({"l"}),
    "emitOperandExprSlot": frozenset({"l"}),
    "emitOperandTextSpan": frozenset({"l"}),
    "eng.opasmEngineAppendImageBytesV1": frozenset({"l"}),
    "eng.opasmEngineGetStatementTextMetadataV1": frozenset({"l"}),
    "eng.opasmEngineStatementLooksBareColumnOneV1": frozenset({"l"}),
    "eng.opasmEngineStatementMnemonicDuplicatesLabelV1": frozenset({"l"}),
    "eng.prepareEvaluateExpressionRequestV1": frozenset({"l"}),
    "eng.prepareSelectedEvaluateRequestV1": frozenset({"l"}),
    "engine.prepareEvaluateExpressionRequestV1": frozenset({"l"}),
    "engine.prepareSelectedEvaluateRequestV1": frozenset({"l"}),
    "expr_bridge.opcoreExvmEvalOperandV1": frozenset({"l"}),
    "copyToken": frozenset({"l"}),
    "copyLocatorToBufferV1": frozenset({"b"}),
    "findCpuEntryV1": frozenset({"b", "l"}),
    "findDialectEntryV1": frozenset({"b", "l"}),
    "findExprOpcodeVersionV1": frozenset({"b", "l"}),
    "findExvmOpcodeVersionV1": frozenset({"b", "l"}),
    "findFamilyEntryV1": frozenset({"b", "l"}),
    "findOwner": frozenset({"b", "l"}),
    "findParserVmOwnerV1": frozenset({"b", "l"}),
    "findRequestedDialectEntryV1": frozenset({"b", "l"}),
    "findTokenizerVmOwnerV1": frozenset({"b", "l"}),
    "lineStartsWith": frozenset({"l"}),
    "line_router.prvmRouteLine68000": frozenset({"l"}),
    "line_processor.opforgeNativeCliTokenizeCurrentLine": frozenset({"l"}),
    "line_text.opforgeNativeCliLineStartsWith": frozenset({"l"}),
    "line_text.opforgeNativeCliSkipLineWhitespace": frozenset({"l"}),
    "labelEquals": frozenset({"l"}),
    "include_use.opforgeNativeCliPreparePendingInclude": frozenset({"l"}),
    "opforgeNativeCliBuildPrvmRouteFrame": frozenset({"l"}),
    "opforgeNativeCliInitPackagePipeline": frozenset({"l"}),
    "opforgeNativeCliTokenizeFile": frozenset({"l"}),
    "opforgeNativeCliTokenizePendingInclude": frozenset({"l"}),
    "opforgeNativeCliTokenizePendingUseModule": frozenset({"l"}),
    "opforgeNativeCliTokenizeResolvedUseModule": frozenset({"l"}),
    "opforgeNativeCliPrepareParseLineServiceRequest": frozenset({"l"}),
    "opasmEngineGetStatementTextMetadataV1": frozenset({"l"}),
    "amigaosCliFileioWriteCstr": frozenset({"l"}),
    "opforgeNativeCliActivePrvmRequireBytes": frozenset({"l"}),
    "opforgeNativeCliCopyOptionalValue": frozenset({"l"}),
    "opforgeNativeCliCopyRequiredValue": frozenset({"l"}),
    "opforgeNativeCliCopyToken": frozenset({"l"}),
    "opforgeNativeCliIsUnsupportedFlag": frozenset({"l"}),
    "opforgeNativeCliLineStartsWith": frozenset({"l"}),
    "opforgeNativeCliParserMnemonicEquals": frozenset({"l"}),
    "path.opforgeNativeCliAppendPathBuffer": frozenset({"l"}),
    "path.opforgeNativeCliCopyPathBuffer": frozenset({"l"}),
    "path.opforgeNativeCliCopyPathRoot": frozenset({"l"}),
    "package_pipeline.opforgeNativeCliInitPackagePipeline": frozenset({"l"}),
    "popCheckpointAddress": frozenset({"l"}),
    "pushCheckpoint": frozenset({"l"}),
    "readProgramTarget": frozenset({"l"}),
    "resolveSelectedDialectV1": frozenset({"l"}),
    "resultRecordPtr": frozenset({"l"}),
    "runtime.tkvmRun68000": frozenset({"l"}),
    "tkpkg_control_block.opforgeNativeCliReadOutputLen": frozenset({"w"}),
    "tkpkg_control_block.opforgeNativeCliReadStatus": frozenset({"b"}),
    "tkpkgDebugCliReadOutputLenV1": frozenset({"w"}),
    "tkpkgDebugCliReadStatusV1": frozenset({"b"}),
    "tkpkgDebugCliRunLastErrorV1": frozenset({"b"}),
    "tkpkgMselPlanEqualsV1": frozenset({"b", "l"}),
    "tkpkgServiceStringEqAsciiCasefoldV1": frozenset({"b", "l"}),
    "tokenEquals": frozenset({"l"}),
    "tokenPtrByIndex": frozenset({"l"}),
    "token_util.opforgeNativeCliTokenEquals": frozenset({"l"}),
    "validateResult": frozenset({"b", "l"}),
    "validateExpressionResultSlot": frozenset({"l"}),
    "writeFailureReport": frozenset({"l"}),
    "writeExpressionRequest": frozenset({"l"}),
    "writeResumeState": frozenset({"l"}),
    "writeU32": frozenset({"l"}),
}


@dataclasses.dataclass(frozen=True)
class ReviewedRetainedCallSite:
    path_suffix: str
    call_target: str
    tst_width: str
    branch_text: str
    reason: str
    block_name: str | None = None
    prev_line_text: str | None = None


REVIEWED_RETAINED_CALL_TST_SITES = (
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opasm/opasm_engine.asm",
        block_name="prepareSelectedEvaluateRequestV1",
        call_target="getStatementSourceLineTextV1",
        tst_width="l",
        branch_text="beq.s syntheticRequest",
        reason="semantic stored-source-slice presence probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opasm/opasm_engine.asm",
        block_name="opasmEngineRunTwoPassV1",
        call_target="runPassOne",
        tst_width="l",
        branch_text="bne.s done",
        reason="whole-pass boundary outcome check",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opasm/opasm_engine.asm",
        block_name="runPassOne",
        call_target="(a0)",
        tst_width="l",
        branch_text="bne.s return",
        prev_line_text="movea.l opasm_engine_ctx_pass1_begin_cb(a5), a0",
        reason="indirect pass-one begin callback boundary",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opasm/opasm_engine.asm",
        block_name="runPassOne",
        call_target="(a0)",
        tst_width="l",
        branch_text="bne.s return",
        prev_line_text="movea.l opasm_engine_ctx_record_label_cb(a5), a0",
        reason="indirect pass-one record-label callback boundary",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opasm/opasm_engine.asm",
        block_name="runPassOne",
        call_target="(a0)",
        tst_width="l",
        branch_text="bne.s return",
        prev_line_text="movea.l opasm_engine_ctx_advance_pc_cb(a5), a0",
        reason="indirect pass-one advance-PC callback boundary",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opasm/opasm_engine.asm",
        block_name="runPassTwo",
        call_target="(a0)",
        tst_width="l",
        branch_text="bne.s return",
        prev_line_text="movea.l opasm_engine_ctx_pass2_begin_cb(a5), a0",
        reason="indirect pass-two begin callback boundary",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opasm/opasm_engine.asm",
        block_name="runPassTwo",
        call_target="(a0)",
        tst_width="l",
        branch_text="bne.s return",
        prev_line_text="movea.l opasm_engine_ctx_emit_image_cb(a5), a0",
        reason="indirect pass-two emit-image callback boundary",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opasm/opasm_engine.asm",
        block_name="runPassTwo",
        call_target="(a0)",
        tst_width="l",
        branch_text="bne.s return",
        prev_line_text="movea.l opasm_engine_ctx_advance_pc_cb(a5), a0",
        reason="indirect pass-two advance-PC callback boundary",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm",
        block_name="readOperandValueForStatement",
        call_target="eng.getStatementSourceLineTextV1",
        tst_width="l",
        branch_text="bne.s haveText",
        reason="cross-module source-line presence probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opforge-cli/output.asm",
        block_name="opforgeNativeCliWriteFlatOutput",
        call_target="dos.openOutput",
        tst_width="l",
        branch_text="beq.s fail",
        reason="DOS file-open handle probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opforge-cli/module_use.asm",
        call_target="dos.openInput",
        tst_width="l",
        branch_text="bne.s found",
        reason="DOS include-path file-open handle probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opforge-cli/source_reader.asm",
        block_name="opforgeNativeCliTokenizeFileAtPath",
        call_target="dos.openInput",
        tst_width="l",
        branch_text="bne.s openOk",
        reason="DOS source-reader file-open handle probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opforge-cli/package_pipeline.asm",
        block_name="opforgeNativeCliStagePackage",
        call_target="dos.openInput",
        tst_width="l",
        branch_text="bne.s externalOpenOk",
        reason="DOS external-package file-open handle probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/test-harnesses/prvm/prvm_line_iterator_smoke.asm",
        block_name="start",
        call_target="OPEN_LIBRARY(a6)",
        tst_width="l",
        branch_text="beq.w done",
        reason="fallback OS/library handle acquisition probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/test-harnesses/prvm/prvm_smoke.asm",
        block_name="start",
        call_target="OPEN_LIBRARY(a6)",
        tst_width="l",
        branch_text="beq.w done",
        reason="fallback OS/library handle acquisition probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/test-harnesses/prvm/prvm_debug_cli.asm",
        block_name="start",
        call_target="OPEN_LIBRARY(a6)",
        tst_width="l",
        branch_text="beq.w done",
        reason="fallback OS/library handle acquisition probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/test-harnesses/tkpkg/tkpkg_debug_cli.asm",
        call_target="tkpkgDebugCliOpenInputV1",
        tst_width="l",
        branch_text="bne.s tkpkgDebugCliManifestOpenOk",
        reason="DOS file-open handle probe through local wrapper",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/test-harnesses/tkpkg/tkpkg_debug_cli.asm",
        call_target="tkpkgDebugCliOpenInputV1",
        tst_width="l",
        branch_text="bne.s tkpkgDebugCliFileOpenOk",
        reason="DOS file-open handle probe through local wrapper",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/test-harnesses/tkvm/tokvm_cli_harness.asm",
        block_name="tokvmAmigaosCliHarnessRun",
        call_target="amigaosCliFileioInit",
        tst_width="l",
        branch_text="bne.w cleanup",
        reason="host bootstrap DOS/library availability probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/test-harnesses/tkvm/tokvm_cli_harness.asm",
        block_name="tokvmAmigaosCliHarnessRun",
        call_target="amigaosCliFileioOpenOutput",
        tst_width="l",
        branch_text="bne.s outputOpened",
        reason="output file-open handle probe through DOS wrapper",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/test-harnesses/tkvm/tokvm_cli_harness.asm",
        block_name="tokvmAmigaosCliHarnessRun",
        call_target="amigaosCliFileioOpenInput",
        tst_width="l",
        branch_text="bne.s inputOpened",
        reason="input file-open handle probe through DOS wrapper",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/test-harnesses/tkvm/tokvm_cli_harness.asm",
        block_name="tokvmAmigaosCliHarnessRun",
        call_target="amigaosCliFileioClose",
        tst_width="l",
        branch_text="beq.s probeClosed",
        reason="input close outcome probe before bounded overflow handling",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/test-harnesses/tkvm/tokvm_cli_harness.asm",
        block_name="tokvmAmigaosCliHarnessRun",
        call_target="writeReport",
        tst_width="l",
        branch_text="beq.s cleanup",
        reason="whole-report outcome check after VM execution",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/test-harnesses/tkvm/tokvm_cli_harness.asm",
        block_name="amigaosCliFileioInit",
        call_target="OPEN_LIBRARY(a6)",
        tst_width="l",
        branch_text="beq.s fail",
        reason="direct DOS library-open handle probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/test-harnesses/tkvm/tokvm_cli_harness.asm",
        block_name="amigaosCliFileioOpenInput",
        call_target="OPEN(a6)",
        tst_width="l",
        branch_text="bne.s done",
        reason="direct DOS input-open handle probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/test-harnesses/tkvm/tokvm_cli_harness.asm",
        block_name="amigaosCliFileioOpenOutput",
        call_target="OPEN(a6)",
        tst_width="l",
        branch_text="bne.s done",
        reason="direct DOS output-open handle probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/test-harnesses/tkvm/tokvm_cli_harness.asm",
        block_name="amigaosCliFileioClose",
        call_target="CLOSE(a6)",
        tst_width="l",
        branch_text="bne.w ok",
        reason="direct DOS close outcome probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opforge-cli/run.asm",
        block_name="opforgeNativeCliRun",
        call_target="constants.OPEN_LIBRARY(a6)",
        tst_width="l",
        branch_text="bne.s haveDos",
        reason="OS/library handle acquisition probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opforge-cli/run.asm",
        block_name="opforgeNativeCliRun",
        call_target="constants.OPEN_LIBRARY(a6)",
        tst_width="l",
        branch_text="beq.w done",
        reason="fallback OS/library handle acquisition probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opforge-cli/run.asm",
        block_name="opforgeNativeCliRun",
        call_target="dos.openInput",
        tst_width="l",
        branch_text="bne.s inputOpened",
        reason="DOS input file-open handle probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opforge-cli/run.asm",
        block_name="opforgeNativeCliRun",
        call_target="engine_callbacks.opforgeNativeCliRunTwoPassEngine",
        tst_width="l",
        branch_text="beq.s passesOk",
        reason="engine callback bridge whole-pass outcome check",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opforge-cli/run.asm",
        block_name="opforgeNativeCliRun",
        call_target="engine.opasmEngineGetImageByteCountV1",
        tst_width="l",
        branch_text="beq.s emitStub",
        reason="semantic image-byte-count probe",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opforge-cli/line_processor.asm",
        call_target="opforgeNativeCliParseCurrentLine",
        tst_width="l",
        branch_text="bne.s fail",
        reason="semantic tokenizer/service-to-parser handoff check",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opforge-cli/line_processor.asm",
        call_target="assembly_session.opforgeNativeCliRecordPrvmStatementLine",
        tst_width="l",
        branch_text="bne.w fail",
        reason="cross-module PRVM statement-record status check",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/opforge-cli/assembly_session.asm",
        call_target="engine.opasmEngineGetStatementCountV1",
        tst_width="w",
        branch_text="bpl.s skipEmit",
        reason="semantic statement-count sign probe for debug-record emission",
    ),
    ReviewedRetainedCallSite(
        path_suffix="native/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm",
        call_target="policy.resolveLocatorV1",
        tst_width="b",
        branch_text="bne.w done",
        reason="cross-module token-policy locator-resolution status check",
    ),
)


@dataclasses.dataclass(frozen=True)
class Finding:
    code: str
    path: Path
    line_no: int
    message: str
    autofixable: bool
    original_lines: tuple[str, ...]
    suggested_action: str


@dataclasses.dataclass(frozen=True)
class InventorySite:
    category: str
    path: Path
    line_no: int
    block_name: str | None
    prev_line_no: int | None
    prev_line: str | None
    tst_line: str
    next_line_no: int | None
    next_line: str | None
    reason: str


@dataclasses.dataclass(frozen=True)
class WriteInfo:
    mnemonic: str
    width: str
    dest_reg: str
    compatible_tst_widths: frozenset[str]


def strip_comment(line: str) -> str:
    return line.split(";", 1)[0].rstrip()


def normalize_code(line: str) -> str:
    return re.sub(r"\s+", " ", strip_comment(line).strip().lower())


def normalize_reg(reg: str) -> str:
    return reg.lower()


def normalize_width(width: str) -> str:
    return width.lower()


def has_label_or_directive(line: str) -> bool:
    return bool(LABEL_RE.match(line) or DIRECTIVE_RE.match(line))


def is_instruction_line(line: str) -> bool:
    code = strip_comment(line).strip()
    if not code:
        return False
    if BLOCK_LABEL_RE.match(code):
        return False
    if has_label_or_directive(code):
        return False
    return bool(INSTRUCTION_RE.match(code))


def is_conditional_branch(line: str) -> bool:
    return bool(COND_BRANCH_RE.match(strip_comment(line)))


def parse_call_target(line: str) -> str | None:
    match = CALL_TARGET_RE.match(strip_comment(line))
    if match is None:
        return None
    return match.group("target")


def normalize_operand(operand: str) -> str:
    return re.sub(r"\s+", "", operand).lower()


def parse_ccr_setting_write_to_data_reg(line: str) -> WriteInfo | None:
    """
    Return write info when `line` is a narrow, known-safe CCR-setting write to Dn.

    Only recognizes forms that are safe for mechanical adjacent-tst removal.
    It intentionally does not parse arbitrary 68000 syntax.
    """
    code = strip_comment(line).strip()
    if not code:
        return None

    # Exclude known address-register and bulk operations explicitly.
    if re.match(r"^(movea|lea|adda|suba|movem|exg)\b", code, re.IGNORECASE):
        return None

    # moveq #imm, Dn sets CCR as a long result.
    m = re.match(
        rf"^(?P<mnemonic>moveq)\s+#?[^,]+,\s*(?P<dest>{DATA_REG_RE})$",
        code,
        re.IGNORECASE,
    )
    if m:
        return WriteInfo(
            mnemonic=m.group("mnemonic").lower(),
            width="l",
            dest_reg=normalize_reg(m.group("dest")),
            compatible_tst_widths=frozenset({"l"}),
        )

    # clr.<w> Dn sets CCR for that width.
    m = re.match(
        rf"^(?P<mnemonic>clr)\.(?P<width>{WIDTH_RE})\s+(?P<dest>{DATA_REG_RE})$",
        code,
        re.IGNORECASE,
    )
    if m:
        width = normalize_width(m.group("width"))
        return WriteInfo(
            mnemonic=m.group("mnemonic").lower(),
            width=width,
            dest_reg=normalize_reg(m.group("dest")),
            compatible_tst_widths=frozenset({width}),
        )

    # ext.w Dn / ext.l Dn sets CCR for the extended result width.
    m = re.match(
        rf"^(?P<mnemonic>ext)\.(?P<width>[wWlL])\s+(?P<dest>{DATA_REG_RE})$",
        code,
        re.IGNORECASE,
    )
    if m:
        width = normalize_width(m.group("width"))
        return WriteInfo(
            mnemonic=m.group("mnemonic").lower(),
            width=width,
            dest_reg=normalize_reg(m.group("dest")),
            compatible_tst_widths=frozenset({width}),
        )

    # General two-operand data-register destination operations.
    # Keep this conservative: require explicit size and a Dn destination.
    m = re.match(
        rf"^(?P<mnemonic>move|and|andi|or|ori|eor|eori|add|addi|sub|subi)\."
        rf"(?P<width>{WIDTH_RE})\s+.+,\s*(?P<dest>{DATA_REG_RE})$",
        code,
        re.IGNORECASE,
    )
    if m:
        width = normalize_width(m.group("width"))
        return WriteInfo(
            mnemonic=m.group("mnemonic").lower(),
            width=width,
            dest_reg=normalize_reg(m.group("dest")),
            compatible_tst_widths=frozenset({width}),
        )

    return None


def split_local_blocks(lines: Sequence[str]) -> dict[str, list[str]]:
    blocks: dict[str, list[str]] = {}
    current_name: str | None = None
    current_lines: list[str] = []

    for raw_line in lines:
        line = raw_line.rstrip("\n")
        block_match = BLOCK_LABEL_RE.match(line)
        if block_match is not None:
            if current_name is not None:
                blocks[current_name] = current_lines
            current_name = block_match.group(1)
            current_lines = []
            continue
        if current_name is not None:
            current_lines.append(line)

    if current_name is not None:
        blocks[current_name] = current_lines

    return blocks


def map_line_to_block(lines: Sequence[str]) -> dict[int, str]:
    line_to_block: dict[int, str] = {}
    current_name: str | None = None

    for idx, raw_line in enumerate(lines):
        line = raw_line.rstrip("\n")
        block_match = BLOCK_LABEL_RE.match(line)
        if block_match is not None:
            current_name = block_match.group(1)
            continue
        if current_name is not None:
            line_to_block[idx] = current_name

    return line_to_block


def find_prev_significant_instruction_index(lines: Sequence[str], start: int) -> int | None:
    for idx in range(start - 1, -1, -1):
        if BLOCK_LABEL_RE.match(lines[idx].rstrip("\n")):
            break
        if is_instruction_line(lines[idx]):
            return idx
    return None


def find_next_significant_instruction_index(lines: Sequence[str], start: int) -> int | None:
    for idx in range(start + 1, len(lines)):
        if BLOCK_LABEL_RE.match(lines[idx].rstrip("\n")):
            break
        if is_instruction_line(lines[idx]):
            return idx
    return None


def is_indirect_call_target(target: str | None) -> bool:
    if target is None:
        return False
    normalized = normalize_operand(target)
    return "(" in normalized or normalized.endswith(")")


def line_has_length_or_count_hint(line: str | None) -> bool:
    if line is None:
        return False
    normalized = strip_comment(line).replace(".", " ").replace("_", " ")
    return bool(LENGTH_COUNT_HINT_RE.search(normalized))


def operand_has_length_or_count_hint(operand: str) -> bool:
    normalized = operand.replace(".", " ").replace("_", " ")
    return bool(LENGTH_COUNT_HINT_RE.search(normalized))


def site_has_length_or_count_hint(
    *,
    operand: str,
    context_lines: Sequence[str],
) -> bool:
    if operand_has_length_or_count_hint(operand):
        return True
    return any(line_has_length_or_count_hint(line) for line in context_lines)


def helper_tail_appears_ccr_safe(block_lines: Sequence[str]) -> bool:
    significant = [line for line in block_lines if SIGNIFICANT_LINE_RE.match(line) and not COMMENT_OR_BLANK_RE.match(line)]
    if not significant:
        return False

    rts_index = None
    for idx in range(len(significant) - 1, -1, -1):
        if strip_comment(significant[idx]).strip().lower() == "rts":
            rts_index = idx
            break
    if rts_index is None:
        return False

    tail = significant[max(0, rts_index - 11) : rts_index + 1]
    last_suspicious_index = None
    for idx, line in enumerate(tail):
        if SUSPICIOUS_CCR_CLEANUP_RE.match(strip_comment(line).strip()):
            last_suspicious_index = idx

    if last_suspicious_index is None:
        return True

    for line in tail[last_suspicious_index + 1 :]:
        if EXPLICIT_D0_SET_RE.match(strip_comment(line).strip()):
            return True

    return False


def is_reviewed_retained_call_site(
    *,
    path: Path,
    block_name: str | None,
    prev_line_text: str | None,
    call_target: str | None,
    tst_width: str,
    branch: str,
) -> bool:
    if call_target is None:
        return False

    normalized_branch = normalize_code(branch)
    normalized_prev_line = normalize_code(prev_line_text) if prev_line_text is not None else None
    path_text = path.as_posix()

    for site in REVIEWED_RETAINED_CALL_TST_SITES:
        if not path_text.endswith(site.path_suffix):
            continue
        if site.block_name is not None and site.block_name != block_name:
            continue
        if site.call_target != call_target:
            continue
        if site.tst_width != tst_width:
            continue
        if site.branch_text.lower() != normalized_branch:
            continue
        if site.prev_line_text is not None and site.prev_line_text.lower() != normalized_prev_line:
            continue
        return True

    return False


def classify_inventory_site(
    *,
    path: Path,
    lines: Sequence[str],
    index: int,
    line_to_block: dict[int, str],
    local_blocks: dict[str, list[str]],
) -> InventorySite | None:
    tst_line = lines[index].rstrip("\n")
    tst_match = TST_ANY_RE.match(strip_comment(tst_line).strip())
    if tst_match is None:
        return None

    tst_width = normalize_width(tst_match.group("width"))
    operand = tst_match.group("operand").strip()
    normalized_operand = normalize_operand(operand)
    block_name = line_to_block.get(index)

    prev_idx = find_prev_significant_instruction_index(lines, index)
    next_idx = find_next_significant_instruction_index(lines, index)
    prev_line = lines[prev_idx].rstrip("\n") if prev_idx is not None else None
    next_line = lines[next_idx].rstrip("\n") if next_idx is not None else None
    prev_line_text = lines[prev_idx - 1].rstrip("\n") if prev_idx is not None and prev_idx > 0 else None
    context_window = [
        lines[window_idx].rstrip("\n")
        for window_idx in range(max(0, index - 2), min(len(lines), index + 3))
    ]

    category = INVENTORY_CATEGORY_UNCLASSIFIED
    reason = "classifier fallback"
    data_reg_match = re.fullmatch(DATA_REG_RE, operand, re.IGNORECASE)

    if data_reg_match is None:
        if site_has_length_or_count_hint(
            operand=operand,
            context_lines=context_window,
        ):
            category = INVENTORY_CATEGORY_LENGTH_OR_COUNT_PROBE
            reason = "memory/non-register probe with nearby length/count naming hints"
        else:
            category = INVENTORY_CATEGORY_MEMORY_PROBE
            reason = "tst operand is memory or another non-data-register location"
    else:
        tst_reg = normalize_reg(data_reg_match.group(0))
        write = parse_ccr_setting_write_to_data_reg(prev_line) if prev_line is not None else None

        if write is not None and write.dest_reg == tst_reg:
            if tst_width in write.compatible_tst_widths:
                category = INVENTORY_CATEGORY_POST_WRITE_REDUNDANT
                reason = (
                    f"previous instruction {write.mnemonic} writes {tst_reg} and already sets CCR for tst.{tst_width}"
                )
            else:
                category = INVENTORY_CATEGORY_WIDTH_SENSITIVE
                reason = (
                    f"previous instruction writes {tst_reg} but CCR width {write.width} differs from tst.{tst_width}"
                )
        elif prev_line is not None and CALL_RE.match(strip_comment(prev_line)) and tst_reg == "d0":
            call_target = parse_call_target(prev_line)
            branch_line = next_line if next_line is not None and is_conditional_branch(next_line) else ""
            if is_reviewed_retained_call_site(
                path=path,
                block_name=block_name,
                prev_line_text=prev_line_text,
                call_target=call_target,
                tst_width=tst_width,
                branch=branch_line,
            ):
                category = INVENTORY_CATEGORY_POST_CALL_RETAINED
                reason = "explicit reviewed retained call-site exception"
            elif is_indirect_call_target(call_target):
                category = INVENTORY_CATEGORY_INDIRECT_CALL_BOUNDARY
                reason = "previous significant instruction is an indirect call boundary"
            else:
                reviewed_widths = REVIEWED_CCR_D0_CALL_TST_WIDTHS.get(call_target)
                if reviewed_widths is not None and tst_width in reviewed_widths:
                    local_callee = call_target.split(".")[-1] if call_target is not None else None
                    local_block = local_blocks.get(local_callee) if local_callee is not None else None
                    if local_block is not None and not helper_tail_appears_ccr_safe(local_block):
                        reason = (
                            f"call target {call_target} is reviewed for tst.{tst_width}, but the local helper tail does not look CCR-safe"
                        )
                    else:
                        reason = (
                            f"call target {call_target} is reviewed to return with CCR reflecting D0 for tst.{tst_width}"
                        )
                    category = INVENTORY_CATEGORY_POST_CALL_REVIEWED
                else:
                    category = INVENTORY_CATEGORY_POST_CALL_UNKNOWN
                    reason = "call target is not reviewed to return with CCR reflecting D0"
        elif site_has_length_or_count_hint(
            operand=operand,
            context_lines=context_window,
        ):
            category = INVENTORY_CATEGORY_LENGTH_OR_COUNT_PROBE
            reason = "register probe appears tied to a length/count value"
        else:
            category = INVENTORY_CATEGORY_SEMANTIC_REGISTER_PROBE
            reason = "register probe is not an adjacent post-write or reviewed call-return cleanup case"

    return InventorySite(
        category=category,
        path=path,
        line_no=index + 1,
        block_name=block_name,
        prev_line_no=prev_idx + 1 if prev_idx is not None else None,
        prev_line=prev_line,
        tst_line=tst_line,
        next_line_no=next_idx + 1 if next_idx is not None else None,
        next_line=next_line,
        reason=reason,
    )


def find_inventory_sites(path: Path, lines: Sequence[str]) -> list[InventorySite]:
    sites: list[InventorySite] = []
    local_blocks = split_local_blocks(lines)
    line_to_block = map_line_to_block(lines)

    for idx in range(len(lines)):
        site = classify_inventory_site(
            path=path,
            lines=lines,
            index=idx,
            line_to_block=line_to_block,
            local_blocks=local_blocks,
        )
        if site is not None:
            sites.append(site)

    return sites


def find_redundant_tests(path: Path, lines: Sequence[str]) -> list[Finding]:
    findings: list[Finding] = []
    local_blocks = split_local_blocks(lines)
    line_to_block = map_line_to_block(lines)

    for i in range(len(lines) - 2):
        prev = lines[i].rstrip("\n")
        tst = lines[i + 1].rstrip("\n")
        branch = lines[i + 2].rstrip("\n")

        if COMMENT_OR_BLANK_RE.match(prev) or COMMENT_OR_BLANK_RE.match(tst):
            continue

        tst_match = TST_DATA_RE.match(strip_comment(tst).strip())
        if not tst_match:
            continue

        if not is_conditional_branch(branch):
            continue

        if has_label_or_directive(tst) or has_label_or_directive(branch):
            continue

        tst_width = normalize_width(tst_match.group("width"))
        tst_reg = normalize_reg(tst_match.group("reg"))

        write = parse_ccr_setting_write_to_data_reg(prev)
        if write is not None:
            if write.dest_reg == tst_reg and tst_width in write.compatible_tst_widths:
                findings.append(
                    Finding(
                        code="R68000-TST-001",
                        path=path,
                        line_no=i + 2,
                        message=(
                            f"redundant tst.{tst_width} after CCR-setting "
                            f"{write.mnemonic} to {tst_reg}"
                        ),
                        autofixable=True,
                        original_lines=(prev, tst, branch),
                        suggested_action="delete the tst line; the following branch can use existing CCR",
                    )
                )
            continue

        # Report-only call-return pattern. Only for D0 and only adjacent.
        if CALL_RE.match(strip_comment(prev)) and tst_reg == "d0":
            call_target = parse_call_target(prev)
            block_name = line_to_block.get(i)
            prev_line_text = lines[i - 1].rstrip("\n") if i > 0 else None
            if is_reviewed_retained_call_site(
                path=path,
                block_name=block_name,
                prev_line_text=prev_line_text,
                call_target=call_target,
                tst_width=tst_width,
                branch=branch,
            ):
                continue
            reviewed_widths = REVIEWED_CCR_D0_CALL_TST_WIDTHS.get(call_target)
            if reviewed_widths is not None and tst_width in reviewed_widths:
                local_callee = call_target.split(".")[-1] if call_target is not None else None
                local_block = local_blocks.get(local_callee) if local_callee is not None else None
                if local_block is not None and not helper_tail_appears_ccr_safe(local_block):
                    findings.append(
                        Finding(
                            code="R68000-TST-102",
                            path=path,
                            line_no=i + 2,
                            message=(
                                f"reviewed helper {call_target} has a local return tail that "
                                "does not look CCR-safe; keep the tst until the helper contract is fixed"
                            ),
                            autofixable=False,
                            original_lines=(prev, tst, branch),
                            suggested_action=(
                                "inspect the callee epilogue for manual stack cleanup or other "
                                "CCR-clobbering instructions after the returned D0 status is computed"
                            ),
                        )
                    )
                    continue
                findings.append(
                    Finding(
                        code="R68000-TST-002",
                        path=path,
                        line_no=i + 2,
                        message=(
                            f"redundant tst.l d0 after reviewed CCR-preserving call "
                            f"to {call_target}"
                        ),
                        autofixable=True,
                        original_lines=(prev, tst, branch),
                        suggested_action=(
                            "delete the tst line; the reviewed callee contract already "
                            "returns CCR reflecting D0"
                        ),
                    )
                )
                continue
            findings.append(
                Finding(
                    code="R68000-TST-101",
                    path=path,
                    line_no=i + 2,
                    message=(
                        "suspicious tst.l d0 after call; removable only if callee "
                        "documents CCR reflects D0 on return"
                    ),
                    autofixable=False,
                    original_lines=(prev, tst, branch),
                    suggested_action="inspect callee contract; do not auto-fix",
                )
            )

    return findings


def apply_safe_fixes(lines: list[str], findings: Sequence[Finding]) -> list[str]:
    delete_indices = {finding.line_no - 1 for finding in findings if finding.autofixable}
    return [line for idx, line in enumerate(lines) if idx not in delete_indices]


def iter_asm_files(paths: Sequence[Path]) -> list[Path]:
    result: list[Path] = []

    for path in paths:
        if path.is_file():
            if path.suffix.lower() == ".asm":
                result.append(path)
            continue

        if path.is_dir():
            result.extend(sorted(path.rglob("*.asm")))

    seen: set[Path] = set()
    unique: list[Path] = []
    for path in result:
        if path not in seen:
            seen.add(path)
            unique.append(path)
    return unique


def format_finding(finding: Finding, explain: bool = False) -> str:
    status = "autofixable" if finding.autofixable else "report-only"
    parts = [
        f"{finding.path}:{finding.line_no}: {finding.code} [{status}] {finding.message}",
    ]

    if explain:
        parts.append("  original:")
        for line in finding.original_lines:
            parts.append(f"    {line}")
        parts.append(f"  action: {finding.suggested_action}")

    return "\n".join(parts)


def format_inventory_site(site: InventorySite, explain: bool = False) -> str:
    parts = [
        f"{site.category}: {site.path}:{site.line_no}",
        f"  block: {site.block_name or '-'}",
        f"  prev: {site.prev_line_no or '-'}: {site.prev_line or '-'}",
        f"  tst:  {site.line_no}: {site.tst_line}",
        f"  next: {site.next_line_no or '-'}: {site.next_line or '-'}",
        f"  note: {site.reason}",
    ]

    if explain and site.prev_line is not None:
        prev_code = strip_comment(site.prev_line).strip()
        if CALL_RE.match(prev_code):
            target = parse_call_target(site.prev_line)
            if target is not None:
                parts.append(f"  call-target: {target}")
        write = parse_ccr_setting_write_to_data_reg(site.prev_line)
        if write is not None:
            parts.append(
                "  prev-write: "
                f"{write.mnemonic}.{write.width} -> {write.dest_reg} "
                f"(compatible tst widths: {', '.join(sorted(write.compatible_tst_widths))})"
            )

    return "\n".join(parts)


def print_inventory_summary(sites: Sequence[InventorySite]) -> None:
    category_counts = collections.Counter(site.category for site in sites)
    file_counts = collections.Counter(site.path.as_posix() for site in sites)

    print("Inventory category counts:")
    for category in sorted(category_counts):
        print(f"  {category}: {category_counts[category]}")

    print("Inventory file counts:")
    for path_text in sorted(file_counts):
        print(f"  {path_text}: {file_counts[path_text]}")


def run(argv: Sequence[str]) -> int:
    parser = argparse.ArgumentParser(
        description="Detect redundant native Motorola 68000 tst.* Dn instructions."
    )
    parser.add_argument(
        "paths",
        nargs="*",
        type=Path,
        help="Files or directories to scan. Defaults to native/motorola68000.",
    )
    parser.add_argument(
        "--write",
        action="store_true",
        help="Apply mechanically safe fixes by deleting redundant tst lines.",
    )
    parser.add_argument(
        "--fail",
        action="store_true",
        help="Exit nonzero when findings remain/report.",
    )
    parser.add_argument(
        "--explain",
        action="store_true",
        help="Print original lines and suggested action for each finding.",
    )
    parser.add_argument(
        "--include-report-only-in-fail",
        action="store_true",
        help="With --fail, also fail on report-only findings such as call-return tst d0.",
    )
    parser.add_argument(
        "--inventory",
        action="store_true",
        help="Print classified inventory entries for every native 68000 tst.* site.",
    )
    parser.add_argument(
        "--inventory-summary",
        action="store_true",
        help="Print only classified inventory counts by category and file.",
    )
    parser.add_argument(
        "--inventory-suspicious-only",
        action="store_true",
        help="With inventory output, keep only suspicious categories such as unknown post-call and width-sensitive probes.",
    )
    args = parser.parse_args(argv)

    scan_paths = args.paths or [DEFAULT_SCAN_ROOT]
    asm_files = iter_asm_files(scan_paths)

    if not asm_files:
        print("No .asm files found.", file=sys.stderr)
        return 2

    all_findings: list[Finding] = []
    changed_files: list[Path] = []
    all_inventory_sites: list[InventorySite] = []

    for path in asm_files:
        original = path.read_text(encoding="utf-8").splitlines(keepends=True)
        findings = find_redundant_tests(path, original)
        all_findings.extend(findings)
        if args.inventory or args.inventory_summary or args.inventory_suspicious_only:
            all_inventory_sites.extend(find_inventory_sites(path, original))

        if args.write:
            fixed = apply_safe_fixes(original, findings)
            if fixed != original:
                path.write_text("".join(fixed), encoding="utf-8")
                changed_files.append(path)

    filtered_inventory_sites = all_inventory_sites
    if args.inventory_suspicious_only:
        filtered_inventory_sites = [
            site for site in filtered_inventory_sites if site.category in INVENTORY_SUSPICIOUS_CATEGORIES
        ]

    show_inventory_sites = (args.inventory or args.inventory_suspicious_only) and not args.inventory_summary

    if show_inventory_sites:
        for site in filtered_inventory_sites:
            print(format_inventory_site(site, explain=args.explain))

    if args.inventory or args.inventory_summary or args.inventory_suspicious_only:
        print(
            f"Inventory scanned {len(asm_files)} .asm file(s); "
            f"{len(all_inventory_sites)} total tst.* site(s); "
            f"{len(filtered_inventory_sites)} reported."
        )
        print_inventory_summary(filtered_inventory_sites)
    else:
        for finding in all_findings:
            print(format_finding(finding, explain=args.explain))

    autofixable_count = sum(1 for f in all_findings if f.autofixable)
    report_only_count = len(all_findings) - autofixable_count

    print(
        f"Scanned {len(asm_files)} .asm file(s); "
        f"{len(all_findings)} finding(s): "
        f"{autofixable_count} autofixable, {report_only_count} report-only."
    )

    if args.write:
        print(f"Changed {len(changed_files)} file(s).")
        for path in changed_files:
            print(f"  {path}")

    if args.fail:
        fail_count = len(all_findings) if args.include_report_only_in_fail else autofixable_count
        if fail_count:
            return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(run(sys.argv[1:]))

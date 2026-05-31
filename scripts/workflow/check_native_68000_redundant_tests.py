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

LABEL_RE = re.compile(r"^\s*[A-Za-z_.$][A-Za-z0-9_.$]*\s*:")
DIRECTIVE_RE = re.compile(r"^\s*\.[A-Za-z]")
COMMENT_OR_BLANK_RE = re.compile(r"^\s*(?:;.*)?$")


# Reviewed helpers whose contracts are known to return with CCR reflecting D0.
# Call sites may branch directly on D0 after these helpers without an extra tst.
# Width sets stay explicit so we only autofix widths we have reviewed for each
# helper or alias.
REVIEWED_CCR_D0_CALL_TST_WIDTHS = {
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
    "opforgeNativeCliBuildPrvmRouteFrame": frozenset({"l"}),
    "opforgeNativeCliInitPackagePipeline": frozenset({"l"}),
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
class Finding:
    code: str
    path: Path
    line_no: int
    message: str
    autofixable: bool
    original_lines: tuple[str, ...]
    suggested_action: str


@dataclasses.dataclass(frozen=True)
class WriteInfo:
    mnemonic: str
    width: str
    dest_reg: str
    compatible_tst_widths: frozenset[str]


def strip_comment(line: str) -> str:
    return line.split(";", 1)[0].rstrip()


def normalize_reg(reg: str) -> str:
    return reg.lower()


def normalize_width(width: str) -> str:
    return width.lower()


def has_label_or_directive(line: str) -> bool:
    return bool(LABEL_RE.match(line) or DIRECTIVE_RE.match(line))


def is_conditional_branch(line: str) -> bool:
    return bool(COND_BRANCH_RE.match(strip_comment(line)))


def parse_call_target(line: str) -> str | None:
    match = CALL_TARGET_RE.match(strip_comment(line))
    if match is None:
        return None
    return match.group("target")


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


def find_redundant_tests(path: Path, lines: Sequence[str]) -> list[Finding]:
    findings: list[Finding] = []

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
            reviewed_widths = REVIEWED_CCR_D0_CALL_TST_WIDTHS.get(call_target)
            if reviewed_widths is not None and tst_width in reviewed_widths:
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
    args = parser.parse_args(argv)

    scan_paths = args.paths or [DEFAULT_SCAN_ROOT]
    asm_files = iter_asm_files(scan_paths)

    if not asm_files:
        print("No .asm files found.", file=sys.stderr)
        return 2

    all_findings: list[Finding] = []
    changed_files: list[Path] = []

    for path in asm_files:
        original = path.read_text(encoding="utf-8").splitlines(keepends=True)
        findings = find_redundant_tests(path, original)
        all_findings.extend(findings)

        if args.write:
            fixed = apply_safe_fixes(original, findings)
            if fixed != original:
                path.write_text("".join(fixed), encoding="utf-8")
                changed_files.append(path)

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

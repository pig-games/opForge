#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Deterministic architecture-boundary guard for opForge.
#
# Generic Rust VM/native VM/CLI implementation code must not grow
# CPU/family/addressing-mode/register/instruction-specific knowledge.
#
# CPU-specific vocabulary belongs in package VM definitions, family/dialect
# packages, examples, fixtures, tests, docs, or reviewed allowlist entries.
# Selected broader Rust implementation crates are also scanned in warning-only
# mode so the workflow can show advisory findings before promotion to enforced
# scope.

from __future__ import annotations

import argparse
import fnmatch
import os
import re
import subprocess
from dataclasses import dataclass
from pathlib import Path


SCRIPT_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parent.parent

TERMS_FILE = SCRIPT_DIR / "cpu_specific_terms.txt"
ALLOWLIST_FILE = SCRIPT_DIR / "cpu_specific_arch_boundary_allowlist.txt"
ENFORCED_REPORT = REPO_ROOT / "build" / "reports" / "cpu_specific_arch_boundary_enforced_findings.txt"
WARNING_SCAN_REPORT = REPO_ROOT / "build" / "reports" / "cpu_specific_arch_boundary_warning_scan.txt"


LOW_SIGNAL_TERMS = {
    "a0",
    "a1",
    "a2",
    "a3",
    "a4",
    "a5",
    "a6",
    "a7",
    "accumulator",
    "adc",
    "add",
    "addr",
    "absolute",
    "af",
    "af'",
    "af_prime",
    "bc",
    "beq",
    "bit",
    "bra",
    "cacr",
    "call",
    "cli",
    "cmp",
    "d0",
    "d1",
    "d2",
    "d3",
    "d4",
    "d5",
    "d6",
    "d7",
    "de",
    "dfc",
    "direct",
    "displacement",
    "ex",
    "ext",
    "hl",
    "illegal",
    "im",
    "immediate",
    "implied",
    "in",
    "inc",
    "indexed",
    "indirect",
    "ix",
    "iy",
    "jmp",
    "jsr",
    "ld",
    "link",
    "map",
    "mnemonic",
    "mnemonics",
    "mov",
    "move",
    "mul",
    "nop",
    "not",
    "opcode",
    "opcodes",
    "ord",
    "out",
    "pc",
    "per",
    "pop",
    "push",
    "rc",
    "relative",
    "res",
    "reset",
    "ret",
    "row",
    "rts",
    "sbc",
    "see",
    "set",
    "sfc",
    "sp",
    "sr",
    "ssp",
    "std",
    "sub",
    "swap",
    "sync",
    "tab",
    "trap",
    "usp",
    "vbr",
}


NATIVE_REENABLED_LOW_SIGNAL_TERMS = {
    "absolute",
    "accumulator",
    "dec",
    "direct",
    "displacement",
    "immediate",
    "inc",
    "implied",
    "indexed",
    "indirect",
    "relative",
}


WARNING_ONLY_TERMS = {
    "absolute",
    "dec",
    "inc",
    "relative",
}


NATIVE_ASM_EXTENSIONS = {".asm", ".s"}
NATIVE_DEFINITION_DIRECTIVES = {".block", ".macro"}
NATIVE_CONTEXT_DIRECTIVES = {".module", ".namespace", ".section", ".segment"}
NATIVE_DATA_DIRECTIVE_RE = re.compile(r"^\s*(?:\.(?:ascii|asciz|byte|string|text)|dc\.[bwl])\b", re.IGNORECASE)
NATIVE_CONTEXT_DIRECTIVE_RE = re.compile(
    r"^\s*(?P<directive>\.(?:module|namespace|section|segment))\s+(?P<name>[^\s,]+)",
    re.IGNORECASE,
)
NATIVE_LABEL_WITH_DIRECTIVE_RE = re.compile(
    r"^\s*(?P<name>[A-Za-z_.$?][\w.$?]*)\s+(?P<directive>\.[A-Za-z][\w.]*)\b",
    re.IGNORECASE,
)
NATIVE_ASSIGN_RE = re.compile(r"^\s*(?P<name>[A-Za-z_.$?][\w.$?]*)\s*=\s*.+$")
NATIVE_STANDALONE_LABEL_RE = re.compile(r"^\s*(?P<name>[A-Za-z_.$?][\w.$?]*)\s*$")
NATIVE_STRING_RE = re.compile(r'"([^"\\]|\\.)*"')


SKIP_DIR_NAMES = {
    ".git",
    ".idea",
    ".vscode",
    ".venv",
    "__pycache__",
    "target",
    "build",
    "dist",
    "node_modules",
}


TEXT_EXTENSIONS = {
    ".rs",
    ".asm",
    ".s",
    ".inc",
    ".h",
    ".c",
    ".cpp",
    ".hpp",
    ".toml",
    ".json",
    ".md",
    ".txt",
    ".opasm",
}


PROTECTED_PATHS = [
    "crates/opforge-core/src/**",
    "crates/opforge-types/src/**",
    "native/**",
    "src/**",
    "scripts/workflow/**",
]


WARNING_PROTECTED_PATHS = [
    "crates/opforge-engine/src/**",
    "crates/opforge-formatter/src/**",
    "crates/opforge-package/src/**",
    "crates/opforge-vm/src/**",
]


DEFAULT_ALLOWED_PATHS = [
    "README.md",
    "**/README.md",
    "**/README*.md",
    "**/test/**",
    "**/tests/**",
    "**/test.rs",
    "**/tests.rs",
    "**/test_*.rs",
    "**/*_test.rs",
    "**/*_tests.rs",
    "documentation/**",
    "dev-docs/**",
    "examples/**",
    "tests/**",
    "fixtures/**",
    "reference/**",
    "packages/**",
    "cpu/**",
    "families/**",
    "dialects/**",
]


@dataclass(frozen=True)
class AllowRule:
    pattern: str
    terms: frozenset[str]
    reason: str


@dataclass(frozen=True)
class Violation:
    path: str
    line: int
    column: int
    term: str
    text: str
    severity: str = "error"
    scan_scope: str = "enforced"


def repo_relative(path: Path) -> str:
    return path.relative_to(REPO_ROOT).as_posix()


def is_under_skipped_dir(path: Path) -> bool:
    return any(part in SKIP_DIR_NAMES for part in path.parts)


def load_terms(path: Path, reenabled_terms: set[str] | None = None) -> list[str]:
    if not path.is_file():
        raise SystemExit(f"error: missing CPU-specific terms file: {path}")

    reenabled_terms = reenabled_terms or set()
    terms: set[str] = set()
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        lower = line.lower()
        if lower in LOW_SIGNAL_TERMS and lower not in reenabled_terms:
            continue
        terms.add(lower)

    if not terms:
        raise SystemExit(f"error: no terms loaded from {path}")

    return sorted(terms, key=len, reverse=True)


def load_allowlist(path: Path) -> list[AllowRule]:
    if not path.exists():
        return []

    rules: list[AllowRule] = []
    for line_no, raw in enumerate(path.read_text(encoding="utf-8").splitlines(), start=1):
        line = raw.strip()
        if not line or line.startswith("#"):
            continue

        parts = [part.strip() for part in line.split("|", 2)]
        if len(parts) != 3:
            raise SystemExit(
                f"error: invalid allowlist line {path}:{line_no}: "
                "expected 'path-glob | terms | reason'"
            )

        pattern, terms_raw, reason = parts
        if not pattern:
            raise SystemExit(f"error: empty allowlist path at {path}:{line_no}")
        if not terms_raw:
            raise SystemExit(f"error: empty allowlist term set at {path}:{line_no}")
        if not reason:
            raise SystemExit(f"error: empty allowlist reason at {path}:{line_no}")

        terms = frozenset(term.strip().lower() for term in terms_raw.split(",") if term.strip())
        rules.append(AllowRule(pattern=pattern, terms=terms, reason=reason))

    return rules


def matches_any(path: str, patterns: list[str]) -> bool:
    return any(fnmatch.fnmatch(path, pattern) for pattern in patterns)


def is_allowed_by_rule(path: str, term: str, allowlist: list[AllowRule]) -> bool:
    for rule in allowlist:
        if not fnmatch.fnmatch(path, rule.pattern):
            continue
        if "*" in rule.terms or term in rule.terms:
            return True
    return False


def compile_terms_regex(terms: list[str]) -> re.Pattern[str]:
    escaped = [re.escape(term) for term in terms]
    pattern = r"(?<![A-Za-z0-9_])(" + "|".join(escaped) + r")(?![A-Za-z0-9_])"
    return re.compile(pattern, re.IGNORECASE)


def scan_scope_for_path(path: str) -> str | None:
    if matches_any(path, PROTECTED_PATHS):
        return "enforced"
    if matches_any(path, WARNING_PROTECTED_PATHS):
        return "warning"
    return None


def finding_severity(term: str, scan_scope: str) -> str:
    if scan_scope == "warning":
        return "warning"
    return "warning" if term in WARNING_ONLY_TERMS else "error"


def is_native_asm_file(rel: str, path: Path) -> bool:
    return rel.startswith("native/") and path.suffix.lower() in NATIVE_ASM_EXTENSIONS


def strip_asm_comment(line: str) -> str:
    in_string = False
    escape = False

    for index, char in enumerate(line):
        if char == '"' and not escape:
            in_string = not in_string
        elif char == ";" and not in_string:
            return line[:index]

        escape = char == "\\" and not escape
        if char != "\\":
            escape = False

    return line


def canonicalize_native_text(text: str) -> tuple[str, list[int]]:
    canonical_chars: list[str] = []
    index_map: list[int] = []

    for index, char in enumerate(text):
        if char == "_":
            continue
        canonical_chars.append(char.lower())
        index_map.append(index)

    return "".join(canonical_chars), index_map


def has_native_boundary(text: str, start: int, length: int) -> bool:
    end = start + length

    if start == 0:
        before_ok = True
    else:
        before = text[start - 1]
        current = text[start]
        before_ok = (
            not before.isalnum()
            or before in "_-.:/"
            or (before.islower() and current.isupper())
            or (before.isalpha() and current.isdigit())
        )

    if end >= len(text):
        after_ok = True
    else:
        after = text[end]
        current = text[end - 1]
        after_ok = (
            not after.isalnum()
            or after in "_-.:/"
            or (current.islower() and after.isupper())
            or (current.isalpha() and after.isdigit())
        )

    return before_ok and after_ok


def find_native_candidate_matches(text: str, terms: list[str]) -> list[tuple[str, int]]:
    canonical_text, index_map = canonicalize_native_text(text)
    matches: list[tuple[str, int]] = []
    seen: set[tuple[str, int]] = set()

    for term in terms:
        canonical_term = term.replace("_", "").lower()
        if not canonical_term:
            continue

        start = 0
        while True:
            index = canonical_text.find(canonical_term, start)
            if index == -1:
                break
            original_index = index_map[index]
            original_end = index_map[index + len(canonical_term) - 1]
            original_length = original_end - original_index + 1

            if has_native_boundary(text, original_index, original_length) and (term, original_index) not in seen:
                matches.append((term, original_index))
                seen.add((term, original_index))
            start = index + 1

    matches.sort(key=lambda item: (item[1], item[0]))
    return matches


def scan_native_candidate(
    *,
    rel: str,
    line_no: int,
    line: str,
    column_offset: int,
    candidate: str,
    scan_scope: str,
    terms: list[str],
    allowlist: list[AllowRule],
) -> list[Violation]:
    violations: list[Violation] = []

    for term, index in find_native_candidate_matches(candidate, terms):
        if is_allowed_by_rule(rel, term, allowlist):
            continue

        violations.append(
            Violation(
                path=rel,
                line=line_no,
                column=column_offset + index,
                term=term,
                text=line.rstrip(),
                severity=finding_severity(term, scan_scope),
                scan_scope=scan_scope,
            )
        )

    return violations


def scan_native_asm_file(
    path: Path,
    rel: str,
    scan_scope: str,
    native_terms: list[str],
    allowlist: list[AllowRule],
) -> list[Violation]:
    try:
        text = path.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        return []

    violations: list[Violation] = []
    pending_data_label: tuple[str, bool] | None = None

    for line_no, raw_line in enumerate(text.splitlines(), start=1):
        line = strip_asm_comment(raw_line)
        stripped = line.strip()

        if not stripped:
            continue

        context_match = NATIVE_CONTEXT_DIRECTIVE_RE.match(line)
        if context_match:
            pending_data_label = None
            violations.extend(
                scan_native_candidate(
                    rel=rel,
                    line_no=line_no,
                    line=raw_line,
                    column_offset=context_match.start("name") + 1,
                    candidate=context_match.group("name"),
                    scan_scope=scan_scope,
                    terms=native_terms,
                    allowlist=allowlist,
                )
            )
            continue

        assign_match = NATIVE_ASSIGN_RE.match(line)
        if assign_match:
            pending_data_label = None
            violations.extend(
                scan_native_candidate(
                    rel=rel,
                    line_no=line_no,
                    line=raw_line,
                    column_offset=assign_match.start("name") + 1,
                    candidate=assign_match.group("name"),
                    scan_scope=scan_scope,
                    terms=native_terms,
                    allowlist=allowlist,
                )
            )
            continue

        definition_match = NATIVE_LABEL_WITH_DIRECTIVE_RE.match(line)
        if definition_match:
            pending_data_label = None
            name = definition_match.group("name")
            directive = definition_match.group("directive").lower()
            candidate_violations = scan_native_candidate(
                rel=rel,
                line_no=line_no,
                line=raw_line,
                column_offset=definition_match.start("name") + 1,
                candidate=name,
                scan_scope=scan_scope,
                terms=native_terms,
                allowlist=allowlist,
            )
            violations.extend(candidate_violations)
            if directive not in NATIVE_DEFINITION_DIRECTIVES and NATIVE_DATA_DIRECTIVE_RE.match(line):
                pending_data_label = (name, bool(candidate_violations))
            continue

        standalone_label_match = NATIVE_STANDALONE_LABEL_RE.match(line)
        if standalone_label_match:
            name = standalone_label_match.group("name")
            candidate_violations = scan_native_candidate(
                rel=rel,
                line_no=line_no,
                line=raw_line,
                column_offset=standalone_label_match.start("name") + 1,
                candidate=name,
                scan_scope=scan_scope,
                terms=native_terms,
                allowlist=allowlist,
            )
            violations.extend(candidate_violations)
            pending_data_label = (name, bool(candidate_violations))
            continue

        if pending_data_label and NATIVE_DATA_DIRECTIVE_RE.match(line):
            _, should_scan_strings = pending_data_label
            if should_scan_strings:
                for string_match in NATIVE_STRING_RE.finditer(line):
                    literal = string_match.group(0)[1:-1]
                    violations.extend(
                        scan_native_candidate(
                            rel=rel,
                            line_no=line_no,
                            line=raw_line,
                            column_offset=string_match.start(0) + 2,
                            candidate=literal,
                            scan_scope=scan_scope,
                            terms=native_terms,
                            allowlist=allowlist,
                        )
                    )
            continue

        pending_data_label = None

    return violations


def all_repo_files() -> list[Path]:
    files: list[Path] = []
    for root, dirnames, filenames in os.walk(REPO_ROOT):
        dirnames[:] = sorted(
            name for name in dirnames if name not in SKIP_DIR_NAMES
        )
        for filename in sorted(filenames):
            path = Path(root, filename)
            if path.suffix.lower() in TEXT_EXTENSIONS:
                files.append(path)
    return files


def changed_files_against_git() -> list[Path]:
    command = ["git", "diff", "--name-only", "--cached"]
    result = subprocess.run(
        command,
        cwd=REPO_ROOT,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )

    if result.returncode != 0:
        raise SystemExit(result.stderr.strip() or "error: git diff --cached failed")

    files: list[Path] = []
    for raw in result.stdout.splitlines():
        rel = raw.strip()
        if not rel:
            continue
        path = REPO_ROOT / rel
        if path.is_file() and path.suffix.lower() in TEXT_EXTENSIONS:
            files.append(path)

    return files


def scan_file(
    path: Path,
    regex: re.Pattern[str],
    native_terms: list[str],
    allowlist: list[AllowRule],
) -> list[Violation]:
    rel = repo_relative(path)
    scan_scope = scan_scope_for_path(rel)

    if scan_scope is None:
        return []

    if matches_any(rel, DEFAULT_ALLOWED_PATHS):
        return []

    if is_native_asm_file(rel, path):
        return scan_native_asm_file(path, rel, scan_scope, native_terms, allowlist)

    try:
        text = path.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        return []

    violations: list[Violation] = []

    for line_no, line in enumerate(text.splitlines(), start=1):
        for match in regex.finditer(line):
            term = match.group(1).lower()

            if is_allowed_by_rule(rel, term, allowlist):
                continue

            violations.append(
                Violation(
                    path=rel,
                    line=line_no,
                    column=match.start(1) + 1,
                    term=term,
                    text=line.rstrip(),
                    severity=finding_severity(term, scan_scope),
                    scan_scope=scan_scope,
                )
            )

    return violations


def format_violation(violation: Violation) -> list[str]:
    return [
        f"{violation.path}:{violation.line}:{violation.column}",
        f"  scan: {violation.scan_scope}",
        f"  severity: {violation.severity}",
        f"  term: {violation.term}",
        f"  line: {violation.text}",
        "",
    ]


def write_report(path: Path, title: str, intro_lines: list[str], findings: list[Violation]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)

    lines = [
        title,
        "",
        *intro_lines,
        "",
    ]

    for violation in findings:
        lines.extend(format_violation(violation))

    path.write_text("\n".join(lines), encoding="utf-8")


def write_enforced_report(enforced_findings: list[Violation]) -> None:
    write_report(
        ENFORCED_REPORT,
        "CPU-specific architecture boundary enforced-scope report",
        [
            "This report contains enforced-scope findings.",
            "Errors fail the gate. Warning-only terms in enforced scope do not fail the gate by themselves.",
        ],
        enforced_findings,
    )


def write_warning_scan_report(warning_scope_findings: list[Violation]) -> None:
    write_report(
        WARNING_SCAN_REPORT,
        "CPU-specific architecture boundary warning-scan report",
        [
            "This report contains advisory findings outside the enforced scope.",
            "These findings do not fail the gate, but they remain visible here for future tightening.",
        ],
        warning_scope_findings,
    )


def clear_report(path: Path) -> None:
    if path.exists():
        path.unlink()


def print_violations(violations: list[Violation], *, reports_enabled: bool = True) -> None:
    enforced_errors = [
        violation
        for violation in violations
        if violation.scan_scope == "enforced" and violation.severity == "error"
    ]
    enforced_warnings = [
        violation
        for violation in violations
        if violation.scan_scope == "enforced" and violation.severity == "warning"
    ]
    warning_scope_findings = [
        violation for violation in violations if violation.scan_scope == "warning"
    ]
    enforced_findings = enforced_errors + enforced_warnings

    if reports_enabled:
        if enforced_findings:
            write_enforced_report(enforced_findings)
        else:
            clear_report(ENFORCED_REPORT)

        if warning_scope_findings:
            write_warning_scan_report(warning_scope_findings)
        else:
            clear_report(WARNING_SCAN_REPORT)

    print("CPU-specific architecture boundary summary.\n")

    print(
        "Generic opForge VM/native/CLI implementation code must not contain "
        "CPU/family/register/addressing-mode/instruction-specific vocabulary."
    )
    print()
    print("Fix by doing one of these:")
    print("  1. Move CPU-specific behavior into package VM definitions or family/dialect packages.")
    print("  2. Rename accidental generic identifiers that collide with CPU vocabulary.")
    print("  3. Add a narrow reviewed allowlist entry with a concrete reason.")
    print()
    if enforced_errors:
        print("FAIL: enforced scope has blocking errors.")
    else:
        print("PASS: no enforced-scope errors.")
    print("Warning scope is advisory and reports future-tightening candidates.")
    print()
    if enforced_errors:
        print(f"FAIL: {len(enforced_errors)} enforced-scope leak(s)")
    if enforced_warnings:
        print(f"WARN: {len(enforced_warnings)} warning-only term finding(s) in enforced scope")
    if enforced_findings and reports_enabled:
        print(f"REPORT: {ENFORCED_REPORT.relative_to(REPO_ROOT).as_posix()}")
    if warning_scope_findings:
        print(f"WARN: {len(warning_scope_findings)} warning-scan finding(s) outside enforced scope")
        if reports_enabled:
            print(f"REPORT: {WARNING_SCAN_REPORT.relative_to(REPO_ROOT).as_posix()}")


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Check that CPU-specific vocabulary does not leak into generic opForge implementation code."
    )
    parser.add_argument(
        "--staged",
        action="store_true",
        help="Scan only staged files. Full scan is the default.",
    )
    parser.add_argument(
        "--no-report",
        action="store_true",
        help="Enforce findings without writing or clearing report files.",
    )
    args = parser.parse_args()

    terms = load_terms(TERMS_FILE)
    native_terms = load_terms(TERMS_FILE, reenabled_terms=NATIVE_REENABLED_LOW_SIGNAL_TERMS)
    allowlist = load_allowlist(ALLOWLIST_FILE)
    regex = compile_terms_regex(terms)

    files = changed_files_against_git() if args.staged else all_repo_files()

    violations: list[Violation] = []
    for path in files:
        violations.extend(scan_file(path, regex, native_terms, allowlist))

    if violations:
        print_violations(violations, reports_enabled=not args.no_report)
        return 1 if any(violation.scan_scope == "enforced" and violation.severity == "error" for violation in violations) else 0

    if not args.no_report:
        clear_report(ENFORCED_REPORT)
        clear_report(WARNING_SCAN_REPORT)
    print("PASS: CPU-specific architecture boundary clean.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

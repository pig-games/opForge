#!/usr/bin/env python3
"""Reject known benchmark identities in production Rust/native source.

A narrow tripwire, not proof against arbitrary test detection. Test files,
harnesses and performance tooling are outside the production scan.
"""
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[2]
# Preserve quoted text when removing comments: paths and identity strings are
# exactly the inputs this check must inspect. Assembly uses semicolon comments.
RUST_TOKENS = re.compile(r'"(?:\\.|[^"\\])*"|//[^\n]*|/\*.*?\*/', re.S)
ASM_TOKENS = re.compile(r'"(?:\\.|[^"\\])*"|\x27[^\x27]*\x27|;[^\n]*')
SELECTORS = re.compile(
    r'(?<![\w$])B(?:0[1-9]|10)(?!\w)'
    r'|(?:documentation/performance/results|scripts/performance/fixtures|'
    r'examples/performance|fixtures/performance)/'
)
EXCLUDED_DIRS = {"test", "tests", "test-harnesses", "fixtures", "benches", "target", "build"}


def production_path(path: Path) -> bool:
    if EXCLUDED_DIRS.intersection(path.parts):
        return False
    name = path.name
    if name in {"test.rs", "tests.rs"} or name.startswith("test_") or name.endswith(("_test.rs", "_tests.rs")):
        return False
    return path.suffix in {".rs", ".asm", ".s", ".inc"}


def violations(text: str, suffix: str) -> list[tuple[int, str]]:
    tokens = RUST_TOKENS if suffix == ".rs" else ASM_TOKENS
    def without_comments(match):
        token = match.group()
        if token.startswith(("//", "/*", ";")):
            return re.sub(r"[^\n]", " ", token)
        return token
    code = tokens.sub(without_comments, text)
    return [(code.count("\n", 0, m.start()) + 1, m.group()) for m in SELECTORS.finditer(code)]


def scan(root: Path) -> tuple[int, list[str]]:
    paths = set((root / "src").rglob("*.rs"))
    for crate in (root / "crates").glob("*/src"):
        paths.update(crate.rglob("*.rs"))
    paths.update(p for p in (root / "native").rglob("*") if p.is_file())
    checked = 0
    errors = []
    for path in sorted(paths):
        relative = path.relative_to(root)
        if not production_path(relative):
            continue
        checked += 1
        for line, selector in violations(path.read_text(encoding="utf-8"), path.suffix):
            errors.append(f"{relative}:{line}: known benchmark selector in production source: {selector}")
    return checked, errors


def main() -> int:
    checked, errors = scan(ROOT)
    if errors:
        print("\n".join(errors))
        return 1
    print(f"PASS: no known benchmark selectors in {checked} production Rust/native files")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""Verify the Item 6.1 native parity test split preserves exact test filters."""

from __future__ import annotations

import argparse
import re
import sys
from collections import defaultdict
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
LEDGER = ROOT / "scripts/workflow/native_parity_test_names.txt"
MODULES = (
    "native_harness_evidence",
    "native_reference_shards",
    "native_fs_uae_parity",
)
ADDITIONAL_FILTER_MODULES = ("native_mos_forward_ref_stability",)
TEST_RE = re.compile(r"(?m)^#\[test\]\nfn ([A-Za-z_][A-Za-z0-9_]*)\(")


def read_ledger(path: Path) -> tuple[dict[str, list[str]], list[str]]:
    expected: dict[str, list[str]] = defaultdict(list)
    errors: list[str] = []
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except OSError as error:
        return {}, [f"cannot read test-name ledger: {error}"]
    for line_number, line in enumerate(lines, 1):
        if not line or line.startswith("#"):
            continue
        parts = line.split()
        if len(parts) != 2:
            errors.append(f"ledger line {line_number} must contain module and test name")
            continue
        module, name = parts
        expected[module].append(name)
    return dict(expected), errors


def validate(root: Path = ROOT, ledger_path: Path | None = None) -> list[str]:
    root = root.resolve()
    ledger_path = ledger_path or root / LEDGER.relative_to(ROOT)
    expected, errors = read_ledger(ledger_path)
    if tuple(expected) != MODULES:
        errors.append("ledger must list the three owning modules in canonical order")
    ledger_names = [name for module in MODULES for name in expected.get(module, [])]
    if len(ledger_names) != len(set(ledger_names)):
        errors.append("ledger test names must be unique")

    main_path = root / "crates/opforge-asm/src/tests.rs"
    try:
        main = main_path.read_text(encoding="utf-8")
    except OSError as error:
        return [*errors, f"cannot read tests.rs: {error}"]
    for module in MODULES:
        declaration = f'#[path = "tests/{module}.rs"]\nmod {module};'
        if main.count(declaration) != 1:
            errors.append(f"tests.rs must declare owning module {module} exactly once")

        module_path = root / f"crates/opforge-asm/src/tests/{module}.rs"
        try:
            module_source = module_path.read_text(encoding="utf-8")
        except OSError as error:
            errors.append(f"cannot read {module_path.relative_to(root)}: {error}")
            continue
        if "use super::*;" not in module_source:
            errors.append(f"{module}: child module must reuse the parent test helpers")
        actual = TEST_RE.findall(module_source)
        if actual != expected.get(module, []):
            errors.append(f"{module}: test function names/order differ from the pre-move ledger")

    main_names = set(TEST_RE.findall(main))
    lingering = sorted(main_names.intersection(ledger_names))
    if lingering:
        errors.append(f"tests.rs still owns moved test functions: {', '.join(lingering)}")
    if "examples_match_reference_outputs" not in main_names:
        errors.append("examples_match_reference_outputs must retain its exact parent-module filter")

    all_names = list(main_names)
    for module in MODULES:
        module_path = root / f"crates/opforge-asm/src/tests/{module}.rs"
        if module_path.is_file():
            all_names.extend(TEST_RE.findall(module_path.read_text(encoding="utf-8")))
    for module in ADDITIONAL_FILTER_MODULES:
        module_path = root / f"crates/opforge-asm/src/tests/{module}.rs"
        if module_path.is_file():
            all_names.extend(TEST_RE.findall(module_path.read_text(encoding="utf-8")))
    duplicates = sorted(name for name in set(all_names) if all_names.count(name) > 1)
    if duplicates:
        errors.append(f"test functions must remain unique after the split: {', '.join(duplicates)}")

    for wrapper in (
        "scripts/workflow/run_native_macro_completion.sh",
        "scripts/workflow/run_native_existing_parity_completion.sh",
        "scripts/workflow/run_native_reference_parity_completion.sh",
    ):
        wrapper_path = root / wrapper
        if not wrapper_path.is_file():
            errors.append(f"missing completion wrapper: {wrapper}")
            continue
        wrapper_text = wrapper_path.read_text(encoding="utf-8")
        test_array = re.search(r"(?ms)^tests=\(\n(.*?)^\)$", wrapper_text)
        if test_array is None:
            errors.append(f"{wrapper}: canonical tests array is missing")
            continue
        for name in re.findall(r"(?m)^  ([A-Za-z_][A-Za-z0-9_]*)$", test_array.group(1)):
            if name not in all_names:
                errors.append(f"{wrapper}: exact test filter is missing after split: {name}")
    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--staged", action="store_true", help="Accepted for workflow composition; the complete ownership contract is always checked.")
    args = parser.parse_args()
    errors = validate(args.root)
    if errors:
        print("native test module ownership: FAIL", file=sys.stderr)
        for error in errors:
            print(f"  - {error}", file=sys.stderr)
        return 1
    print("native test module ownership: PASS")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

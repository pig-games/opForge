import tempfile
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_native_test_module_ownership import MODULES, ROOT, validate


class NativeTestModuleOwnershipTests(unittest.TestCase):
    def make_root(self) -> Path:
        root = Path(tempfile.mkdtemp())
        source_dir = root / "crates/opforge-asm/src"
        tests_dir = source_dir / "tests"
        workflow_dir = root / "scripts/workflow"
        tests_dir.mkdir(parents=True)
        workflow_dir.mkdir(parents=True)
        declarations = "\n".join(
            f'#[path = "tests/{module}.rs"]\nmod {module};' for module in MODULES
        )
        (source_dir / "tests.rs").write_text(
            declarations + "\n#[test]\nfn examples_match_reference_outputs() {}\n",
            encoding="utf-8",
        )
        ledger = []
        for module in MODULES:
            name = f"{module}_test"
            ledger.append(f"{module} {name}")
            (tests_dir / f"{module}.rs").write_text(
                f"use super::*;\n#[test]\nfn {name}() {{}}\n",
                encoding="utf-8",
            )
        (workflow_dir / "native_parity_test_names.txt").write_text(
            "\n".join(ledger) + "\n", encoding="utf-8"
        )
        for wrapper in (
            "run_native_macro_completion.sh",
            "run_native_existing_parity_completion.sh",
        ):
            (workflow_dir / wrapper).write_text("tests=(\n)\n", encoding="utf-8")
        return root

    def test_repository_split_matches_pre_move_ledger(self):
        self.assertEqual(validate(ROOT), [])

    def test_renamed_filter_is_rejected(self):
        root = self.make_root()
        module = MODULES[0]
        path = root / f"crates/opforge-asm/src/tests/{module}.rs"
        path.write_text(path.read_text().replace(f"{module}_test", "renamed"), encoding="utf-8")
        self.assertIn("differ from the pre-move ledger", "\n".join(validate(root)))

    def test_lingering_parent_definition_is_rejected(self):
        root = self.make_root()
        main = root / "crates/opforge-asm/src/tests.rs"
        main.write_text(
            main.read_text() + "#[test]\nfn native_harness_evidence_test() {}\n",
            encoding="utf-8",
        )
        self.assertIn("still owns moved test functions", "\n".join(validate(root)))


if __name__ == "__main__":
    unittest.main()

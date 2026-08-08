import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_native_fs_uae_proof_contract import validate


class NativeFsUaeProofContractTests(unittest.TestCase):
    def test_repository_contract_passes(self):
        self.assertEqual(validate(), [])

    def test_missing_case_proof_and_persistent_evidence_fail(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            runner = root / "crates/opforge-asm/src/fs_uae_smoke.rs"
            runner.parent.mkdir(parents=True)
            runner.write_text("record_last_green_fs_uae_test_run\n", encoding="utf-8")
            tests = root / "crates/opforge-asm/src/tests/case.rs"
            tests.parent.mkdir(parents=True)
            tests.write_text(
                "OpforgeNativeCliParityCase { name: \"raw\" }\n"
                "OpforgeNativeCliMosFixtureCase { name: \"mos\" }\n",
                encoding="utf-8",
            )
            for relative in (
                "AGENTS.md",
                "agents/rules/native-rust-parity-porting.md",
                "agents/rules/fs-uae.md",
            ):
                path = root / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("", encoding="utf-8")

            errors = validate(root)
            self.assertTrue(any("persistent stale-evidence" in error for error in errors))
            self.assertGreaterEqual(
                sum("mandatory proof mode" in error for error in errors), 2
            )


if __name__ == "__main__":
    unittest.main()

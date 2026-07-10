import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_native_porting_slice import discover_metadata, parse_metadata, validate_metadata


class NativePortingSliceTests(unittest.TestCase):
    def valid(self):
        return {
            "slice": {
                "name": "x",
                "kind": "native-rust-parity",
                "rust_reference": ["r"],
                "native_boundary": ["n"],
                "invariant": "i",
            },
            "tests": [
                {"name": f"level-{level}", "proof_level": level, "proves": "p", "does_not_prove": "n"}
                for level in "ABCDE"
            ],
        }

    def test_all_proof_levels_pass(self):
        self.assertEqual(validate_metadata(self.valid()), [])

    def test_single_staged_slice_metadata_is_discovered(self):
        path = "documentation/plans/slices/native-porting-slice-test.toml"
        self.assertEqual(discover_metadata(["native/motorola68000/x.asm", path]), path)
        self.assertIsNone(discover_metadata([path, path.replace("test", "other")]))

    def test_missing_and_malformed_fields_fail(self):
        for data in ({}, {"slice": {}, "tests": []}):
            with self.subTest(data=data):
                self.assertTrue(validate_metadata(data))
        parsed, errors = parse_metadata("[slice")
        self.assertIsNone(parsed)
        self.assertIn("malformed", errors[0])

    def test_proof_declarations_are_required(self):
        data = self.valid()
        del data["tests"][0]["does_not_prove"]
        self.assertIn("requires `does_not_prove`", "\n".join(validate_metadata(data)))

    def test_reduced_fixture_requires_level_e_or_justification(self):
        data = self.valid()
        data["tests"][0]["name"] = "reduced fixture"
        self.assertIn("must be Level E", "\n".join(validate_metadata(data)))
        data["tests"][0]["semantic_completeness_justification"] = "complete"
        self.assertEqual(validate_metadata(data), [])

    def test_level_d_only_requires_host_justification(self):
        data = self.valid()
        data["tests"] = [data["tests"][3]]
        self.assertIn("host-side proof", "\n".join(validate_metadata(data)))
        data["slice"]["host_proof_absence_justification"] = "hardware-only boundary"
        self.assertEqual(validate_metadata(data), [])

    def test_version_two_requires_complete_execution_contract(self):
        data = self.valid()
        data["slice"]["schema_version"] = 2
        errors = "\n".join(validate_metadata(data))
        for field in (
            "expected_inputs",
            "expected_outputs",
            "known_non_equivalences",
            "fast_proof_command",
            "level_d_command",
            "level_d_fail_closed",
        ):
            self.assertIn(field, errors)
        data["slice"].update(
            {
                "expected_inputs": "canonical source and configured FS-UAE environment",
                "expected_outputs": "native output exactly matches live Rust output",
                "known_non_equivalences": "none for the assigned fixture",
                "fast_proof_command": "cargo test -p asm level-a -- --nocapture",
                "level_d_command": "cargo test -p asm level-d -- --nocapture --test-threads=1",
                "level_d_fail_closed": True,
            }
        )
        self.assertEqual(validate_metadata(data), [])

    def test_version_one_requires_explicit_migration_note(self):
        data = self.valid()
        data["slice"]["schema_version"] = 1
        self.assertIn("legacy_contract_migration", "\n".join(validate_metadata(data)))
        data["slice"]["legacy_contract_migration"] = "Awaiting Item 4 v2 migration."
        self.assertEqual(validate_metadata(data), [])

    def test_unsupported_schema_version_fails(self):
        data = self.valid()
        data["slice"]["schema_version"] = 99
        self.assertIn("unsupported schema_version", "\n".join(validate_metadata(data)))


if __name__ == "__main__":
    unittest.main()

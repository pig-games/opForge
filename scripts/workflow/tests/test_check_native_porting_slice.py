import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_native_porting_slice import parse_metadata, validate_metadata


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


if __name__ == "__main__":
    unittest.main()

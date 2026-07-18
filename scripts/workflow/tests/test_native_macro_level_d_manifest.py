import importlib.util
import json
import subprocess
import tempfile
import unittest
from pathlib import Path


class NativeMacroLevelDManifestTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.root = Path(__file__).resolve().parents[3]
        cls.validator = cls.root / "scripts/workflow/check_native_macro_level_d_manifest.py"
        spec = importlib.util.spec_from_file_location("native_macro_level_d_manifest", cls.validator)
        cls.module = importlib.util.module_from_spec(spec)
        assert spec.loader is not None
        spec.loader.exec_module(cls.module)

    def valid_manifest(self):
        return {
            "manifest_version": 1,
            "kind": "native-macro-preprocessor-level-d",
            "scope": "macro-substitution-reentry",
            "status": "PASS",
            "completed_at_utc": "2026-07-18T12:00:00Z",
            "source": {"commit": "a" * 40, "tree": "b" * 40},
            "tests": [
                {"name": name, "command": self.module.COMMAND_TEMPLATE.format(name=name), "result": "PASS"}
                for name in self.module.EXPECTED_TESTS
            ],
        }

    def run_validator(self, payload, *args):
        with tempfile.TemporaryDirectory() as temp_dir:
            manifest = Path(temp_dir) / "manifest.json"
            manifest.write_text(json.dumps(payload), encoding="utf-8")
            return subprocess.run(["python3", str(self.validator), str(manifest), *args], cwd=self.root, text=True, capture_output=True, check=False)

    def test_valid_manifest_passes(self):
        result = self.run_validator(self.valid_manifest())
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_missing_extra_or_reordered_test_fails(self):
        for mutate in (lambda tests: tests.pop(), lambda tests: tests.append(tests[0]), lambda tests: tests.reverse()):
            with self.subTest(mutate=mutate):
                payload = self.valid_manifest()
                mutate(payload["tests"])
                result = self.run_validator(payload)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("canonical order", result.stderr)

    def test_nonpassing_and_source_mismatch_fail(self):
        payload = self.valid_manifest()
        payload["tests"][0]["result"] = "SKIP"
        result = self.run_validator(payload)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("result PASS", result.stderr)
        result = self.run_validator(self.valid_manifest(), "--expect-head")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("source.commit does not match", result.stderr)

    def test_malformed_manifest_fails(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            manifest = Path(temp_dir) / "manifest.json"
            manifest.write_text("not-json", encoding="utf-8")
            result = subprocess.run(["python3", str(self.validator), str(manifest)], cwd=self.root, text=True, capture_output=True, check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("malformed manifest JSON", result.stderr)


if __name__ == "__main__":
    unittest.main()

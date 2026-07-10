import importlib.util
import json
import subprocess
import tempfile
import unittest
from pathlib import Path


class NativeLevelDManifestTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.root = Path(__file__).resolve().parents[3]
        cls.validator = cls.root / "scripts/workflow/check_native_level_d_manifest.py"
        spec = importlib.util.spec_from_file_location("native_level_d_manifest", cls.validator)
        cls.module = importlib.util.module_from_spec(spec)
        assert spec.loader is not None
        spec.loader.exec_module(cls.module)

    def valid_manifest(self):
        tests = []
        for name in self.module.EXPECTED_TESTS:
            tests.append(
                {
                    "name": name,
                    "command": self.module.COMMAND_TEMPLATE.format(name=name),
                    "result": "PASS",
                }
            )
        return {
            "manifest_version": 1,
            "kind": "native-cli-expansion-level-d",
            "scope": "aggregate-baseline-items-5.1-to-5.6",
            "status": "PASS",
            "completed_at_utc": "2026-07-10T12:00:00Z",
            "source": {"commit": "a" * 40, "tree": "b" * 40},
            "tests": tests,
        }

    def run_validator(self, payload, *extra_args):
        with tempfile.TemporaryDirectory() as temp_dir:
            manifest = Path(temp_dir) / "manifest.json"
            manifest.write_text(json.dumps(payload), encoding="utf-8")
            return subprocess.run(
                ["python3", str(self.validator), str(manifest), *extra_args],
                cwd=self.root,
                text=True,
                capture_output=True,
                check=False,
            )

    def test_valid_manifest_passes(self):
        result = self.run_validator(self.valid_manifest())
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_missing_required_test_fails(self):
        payload = self.valid_manifest()
        payload["tests"].pop()
        result = self.run_validator(payload)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("every required Level D test", result.stderr)

    def test_skipped_or_nonpassing_result_fails(self):
        payload = self.valid_manifest()
        payload["tests"][0]["result"] = "SKIPPED"
        result = self.run_validator(payload)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("result PASS", result.stderr)

    def test_malformed_manifest_fails(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            manifest = Path(temp_dir) / "manifest.json"
            manifest.write_text("not-json", encoding="utf-8")
            result = subprocess.run(
                ["python3", str(self.validator), str(manifest)],
                cwd=self.root,
                text=True,
                capture_output=True,
                check=False,
            )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("malformed manifest JSON", result.stderr)

    def test_expected_source_identity_mismatch_fails(self):
        payload = self.valid_manifest()
        result = self.run_validator(payload, "--expect-head")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("source.commit does not match", result.stderr)

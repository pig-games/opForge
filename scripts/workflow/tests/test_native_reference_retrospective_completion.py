import os
import subprocess
import unittest
from pathlib import Path


class NativeReferenceRetrospectiveCompletionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.root = Path(__file__).resolve().parents[3]
        cls.wrapper = cls.root / "scripts/workflow/run_native_reference_retrospective_completion.sh"

    def test_missing_configuration_fails_closed(self):
        env = os.environ.copy()
        for name in (
            "OPFORGE_FS_UAE_SMOKE",
            "OPFORGE_FS_UAE_BIN",
            "OPFORGE_FS_UAE_CONFIG_TEMPLATE",
            "OPFORGE_FS_UAE_ARGS",
        ):
            env.pop(name, None)
        result = subprocess.run(
            ["bash", str(self.wrapper), "--check-config"],
            cwd=self.root,
            env=env,
            text=True,
            capture_output=True,
            check=False,
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("required for fail-closed native completion", result.stderr)

    def test_wrapper_names_every_retrospective_level_d_proof(self):
        source = self.wrapper.read_text()
        for name in (
            "schema_binary_parity_matches_live_rust_cli",
            "schema_listing_parity_matches_live_rust_cli",
            "schema_diagnostic_parity_matches_live_rust_cli",
            "expression_metadata_fallback_matches_live_rust_cli",
            "source_cpu_normalization_matches_live_rust_cli",
            "debug_output_isolation_preserves_normal_output",
        ):
            self.assertIn(name, source)
        self.assertIn("--test-threads=1", source)


if __name__ == "__main__":
    unittest.main()

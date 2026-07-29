import os
import subprocess
import unittest
from pathlib import Path


class NativeExistingParityCompletionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.root = Path(__file__).resolve().parents[3]
        cls.wrapper = cls.root / "scripts/workflow/run_native_existing_parity_completion.sh"

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
        self.assertIn("required for fail-closed native parity completion", result.stderr)

    def test_wrapper_names_every_established_parity_group(self):
        source = self.wrapper.read_text(encoding="utf-8")
        required_representatives = (
            "schema_binary_parity_matches_live_rust_cli",
            "native_opcore_counted_for_fs_uae",
            "native_opcore_conditionals_fs_uae",
            "native_opcore_scopes_fs_uae",
            "native_macro_invocation_fixture_fs_uae",
            "native_opcore_structs_fs_uae",
            "native_expression_multiplicative_fs_uae",
            "native_opcore_text_encoding_fs_uae",
            "native_module_local_symbol_fs_uae",
            "native_pipeline_select_harness_fs_uae",
            "tkpkg_native_mos6502_family_corpus",
            "item7_layout_directives_match_rust_guided_bytes",
            "native_cli_65c02_expr_syntax_matches_rust_bin",
        )
        for name in required_representatives:
            self.assertIn(name, source)
        self.assertIn("^SKIP:", source)
        self.assertIn("--test-threads=1", source)
        self.assertIn("running 1 test", source)
        self.assertIn("parent-plan Items 7.4-7.7 remain open", source)


if __name__ == "__main__":
    unittest.main()

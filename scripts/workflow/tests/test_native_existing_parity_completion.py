import os
import subprocess
import tempfile
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
            "native_m68000_move_control_parity",
        )
        for name in required_representatives:
            self.assertIn(name, source)
        self.assertIn("^SKIP:", source)
        self.assertIn("--test-threads=1", source)
        self.assertIn("running 1 test", source)
        self.assertIn("parent-plan Items 7.4-7.7 remain open", source)

    def test_cleanup_output_may_split_libtest_name_from_ok(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            fake_cargo = root / "cargo"
            fake_cargo.write_text(
                "#!/usr/bin/env bash\n"
                "printf 'running 1 test\\n'\n"
                "printf 'test tests::native_fs_uae_parity::%s ... cleanup\\n' \"$4\"\n"
                "printf 'ok\\n\\ntest result: ok. 1 passed; 0 failed;\\n'\n",
                encoding="utf-8",
            )
            fake_cargo.chmod(0o755)
            emulator = root / "fs-uae"
            emulator.write_text("#!/usr/bin/env bash\n", encoding="utf-8")
            emulator.chmod(0o755)
            config = root / "config.fs-uae"
            config.write_text("", encoding="utf-8")
            env = os.environ.copy()
            env.update(
                {
                    "CARGO": str(fake_cargo),
                    "OPFORGE_FS_UAE_SMOKE": "1",
                    "OPFORGE_FS_UAE_BIN": str(emulator),
                    "OPFORGE_FS_UAE_CONFIG_TEMPLATE": str(config),
                    "OPFORGE_FS_UAE_ARGS": "{fsuae_config}",
                }
            )
            result = subprocess.run(
                ["bash", str(self.wrapper), "--verify"],
                cwd=self.root,
                env=env,
                text=True,
                capture_output=True,
                check=False,
            )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("PASS: complete established native Level D parity corpus", result.stdout)


if __name__ == "__main__":
    unittest.main()

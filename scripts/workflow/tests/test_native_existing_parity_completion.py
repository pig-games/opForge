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
        cls.generation_two_bonus_wrapper = (
            cls.root / "scripts/workflow/run_native_generation_two_bonus_completion.sh"
        )

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
            "native_m68080_integer_parity",
            "native_m68080_ammx_parity",
        )
        for name in required_representatives:
            self.assertIn(name, source)
        self.assertIn("^SKIP:", source)
        self.assertIn("--test-threads=1", source)
        self.assertIn("running 1 test", source)
        self.assertIn("parent-plan Items 7.4-7.7 remain open", source)

    def run_phase_zero(self, behavior="pass"):
        # Level A/B wrapper contract only: fake cargo supplies no native proof.
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            cargo = root / "cargo"
            cargo.write_text(
                '#!/usr/bin/env bash\n'
                'if [[ "$4" == external_fs_uae_opforge_native_cli_schema_binary_parity_matches_live_rust_cli ]]; then\n'
                '  case "$FAKE_BEHAVIOR" in\n'
                '    fail) echo "failed first group"; exit 1 ;;\n'
                '    skip) echo "SKIP: unavailable" ;;\n'
                '    empty) echo "running 0 tests"; exit 0 ;;\n'
                '  esac\n'
                'fi\n'
                'printf "running 1 test\\ntest tests::%s ... ok\\ntest result: ok. 1 passed; 0 failed;\\n" "$4"\n',
                encoding="utf-8",
            )
            cargo.chmod(0o755)
            config = root / "config.fs-uae"
            config.write_text("", encoding="utf-8")
            env = {**os.environ, "CARGO": str(cargo), "FAKE_BEHAVIOR": behavior,
                   "OPFORGE_FS_UAE_SMOKE": "1", "OPFORGE_FS_UAE_BIN": str(cargo),
                   "OPFORGE_FS_UAE_CONFIG_TEMPLATE": str(config), "OPFORGE_FS_UAE_ARGS": "{fsuae_config}"}
            return subprocess.run(["bash", str(self.wrapper), "--verify-phase-zero"], cwd=self.root,
                                  env=env, capture_output=True, text=True, check=False)

    def test_phase_zero_defers_exactly_two_terminal_groups(self):
        source = self.wrapper.read_text(encoding="utf-8")
        test_array = source.split("tests=(\n", 1)[1].split("\n)", 1)[0]
        complete = [line.strip() for line in test_array.splitlines()
                    if line.startswith("  ") and not line.lstrip().startswith("#")]
        deferred = {"external_fs_uae_native_opforge_full_product_artifact_parity",
                    "external_fs_uae_native_opforge_two_generation_self_host_parity"}
        result = self.run_phase_zero()
        self.assertEqual(result.returncode, 0, result.stderr)
        selected = [line.split(": ", 1)[1] for line in result.stdout.splitlines()
                    if line.startswith("==> Established native Level D parity:")]
        self.assertEqual(selected, [name for name in complete if name not in deferred])
        self.assertEqual(len(selected), 51)
        self.assertIn("PASS: Phase 0 nonterminal native Level D gate verified (51 tests; 2 terminal groups deferred)", result.stdout)
        self.assertNotIn("PASS: complete established", result.stdout)

    def test_phase_zero_attempts_later_groups_but_rejects_fail_skip_and_empty(self):
        for behavior in ("fail", "skip", "empty"):
            with self.subTest(behavior=behavior):
                result = self.run_phase_zero(behavior)
                self.assertNotEqual(result.returncode, 0)
                self.assertEqual(result.stdout.count("==> Established native Level D parity:"), 51)
                self.assertIn("1 of 51 groups failed; all selected groups attempted", result.stderr)
                self.assertNotIn("PASS: Phase 0", result.stdout)

    def test_generation_two_bonus_delegates_the_same_complete_inventory(self):
        source = self.wrapper.read_text(encoding="utf-8")
        bonus = self.generation_two_bonus_wrapper.read_text(encoding="utf-8")
        test_array = source.split("tests=(\n", 1)[1].split("\n)", 1)[0]
        named_tests = [
            line.strip()
            for line in test_array.splitlines()
            if line.startswith("  ") and not line.lstrip().startswith("#")
        ]

        self.assertEqual(len(named_tests), 53)
        self.assertEqual(len(set(named_tests)), 53)
        self.assertEqual(
            named_tests[-1],
            "external_fs_uae_native_opforge_two_generation_self_host_parity",
        )
        self.assertIn("--verify-generation-two-first", bonus)
        self.assertIn("run_native_existing_parity_completion.sh", bonus)
        self.assertIn('generation_two_first_tests=("${generation_two_test}")', source)
        self.assertIn('tests=("${generation_two_first_tests[@]}")', source)

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

    def test_generation_two_bonus_runs_terminal_proof_before_remaining_corpus(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            fake_cargo = root / "cargo"
            fake_cargo.write_text(
                "#!/usr/bin/env bash\n"
                "printf 'running 1 test\\n'\n"
                "printf 'test tests::native_fs_uae_parity::%s ... ok\\n' \"$4\"\n"
                "printf 'test result: ok. 1 passed; 0 failed;\\n'\n",
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
                ["bash", str(self.generation_two_bonus_wrapper)],
                cwd=self.root,
                env=env,
                text=True,
                capture_output=True,
                check=False,
            )

        self.assertEqual(result.returncode, 0, result.stderr)
        first_banner = next(
            line
            for line in result.stdout.splitlines()
            if line.startswith("==> Established native Level D parity:")
        )
        self.assertTrue(
            first_banner.endswith(
                "external_fs_uae_native_opforge_two_generation_self_host_parity"
            )
        )
        self.assertIn(
            "PASS: complete established native Level D parity corpus verified (53 tests)",
            result.stdout,
        )


if __name__ == "__main__":
    unittest.main()

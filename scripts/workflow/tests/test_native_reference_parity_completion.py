import os
import re
import subprocess
import tempfile
import unittest
from pathlib import Path


class NativeReferenceParityCompletionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.root = Path(__file__).resolve().parents[3]
        cls.wrapper = cls.root / "scripts/workflow/run_native_reference_parity_completion.sh"
        source = cls.wrapper.read_text(encoding="utf-8")
        match = re.search(r"(?ms)^tests=\(\n(.*?)^\)$", source)
        if match is None:
            raise AssertionError("completion wrapper has no canonical tests array")
        cls.tests = tuple(
            line.strip() for line in match.group(1).splitlines() if line.strip()
        )

    def configured_environment(self, temp_dir: str, cargo_body: str):
        root = Path(temp_dir)
        cargo = root / "cargo"
        cargo.write_text(cargo_body, encoding="utf-8")
        cargo.chmod(0o755)
        emulator = root / "fs-uae"
        emulator.write_text("#!/usr/bin/env bash\n", encoding="utf-8")
        emulator.chmod(0o755)
        config = root / "config.fs-uae"
        config.write_text("", encoding="utf-8")
        log = root / "cargo-invocations.txt"
        env = os.environ.copy()
        env.update(
            {
                "CARGO": str(cargo),
                "TEST_INVOCATION_LOG": str(log),
                "OPFORGE_FS_UAE_SMOKE": "1",
                "OPFORGE_FS_UAE_BIN": str(emulator),
                "OPFORGE_FS_UAE_CONFIG_TEMPLATE": str(config),
                "OPFORGE_FS_UAE_ARGS": "{fsuae_config}",
            }
        )
        return env, log

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
        self.assertIn("required for fail-closed native reference parity completion", result.stderr)

    def test_exact_active_scope_is_6502_65c02_only(self):
        self.assertEqual(
            self.tests,
            (
                "external_fs_uae_opforge_native_cli_schema_binary_parity_matches_live_rust_cli",
                "external_fs_uae_opforge_native_cli_schema_listing_parity_matches_live_rust_cli",
                "external_fs_uae_opforge_native_cli_schema_diagnostic_parity_matches_live_rust_cli",
                "external_fs_uae_opforge_native_cli_expression_metadata_fallback_matches_live_rust_cli",
                "external_fs_uae_opforge_native_cli_source_cpu_normalization_matches_live_rust_cli",
                "external_fs_uae_opforge_native_cli_debug_output_isolation_preserves_normal_output",
                "native_mos_forward_ref_stability_fs_uae",
                "native_reference_opcore_syntax_expression_fs_uae",
                "native_reference_opcore_module_macro_statement_fs_uae",
                "native_reference_opcore_layout_output_fs_uae",
                "native_reference_opcore_diagnostic_fs_uae",
            ),
        )
        source = self.wrapper.read_text(encoding="utf-8")
        for foreign in ("intel8080", "motorola6800", "motorola68000", "z80", "riscv"):
            self.assertNotIn(foreign, source.lower())
        self.assertIn("--test-threads=1", source)
        self.assertIn("rg -q 'SKIP:'", source)
        self.assertIn("running 1 test", source)

    def test_zero_test_result_fails_closed_after_attempting_every_test(self):
        cargo_body = (
            "#!/usr/bin/env bash\n"
            "printf '%s\\n' \"$4\" >> \"$TEST_INVOCATION_LOG\"\n"
            "printf 'running 0 tests\\n\\n'\n"
            "printf 'test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured\\n'\n"
        )
        with tempfile.TemporaryDirectory() as temp_dir:
            env, log = self.configured_environment(temp_dir, cargo_body)
            result = subprocess.run(
                ["bash", str(self.wrapper), "--verify"],
                cwd=self.root,
                env=env,
                text=True,
                capture_output=True,
                check=False,
            )
            invocations = log.read_text(encoding="utf-8").splitlines()
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(invocations, list(self.tests))
        self.assertIn(f"failed for {len(self.tests)}/{len(self.tests)} tests", result.stderr)

    def test_one_failure_does_not_prevent_later_tests(self):
        cargo_body = (
            "#!/usr/bin/env bash\n"
            "test_name=$4\n"
            "printf '%s\\n' \"$test_name\" >> \"$TEST_INVOCATION_LOG\"\n"
            "if [[ $test_name == external_fs_uae_opforge_native_cli_schema_binary_parity_matches_live_rust_cli ]]; then\n"
            "  printf 'emulator crashed\\n'\n"
            "  exit 1\n"
            "fi\n"
            "printf 'running 1 test\\n'\n"
            "printf 'test tests::%s ... ok\\n' \"$test_name\"\n"
            "printf 'test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured\\n'\n"
        )
        with tempfile.TemporaryDirectory() as temp_dir:
            env, log = self.configured_environment(temp_dir, cargo_body)
            result = subprocess.run(
                ["bash", str(self.wrapper), "--verify"],
                cwd=self.root,
                env=env,
                text=True,
                capture_output=True,
                check=False,
            )
            invocations = log.read_text(encoding="utf-8").splitlines()
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(invocations, list(self.tests))
        self.assertIn("failed for 1/11 tests", result.stderr)

    def test_prefixed_libtest_skip_fails_and_later_tests_are_attempted(self):
        cargo_body = (
            "#!/usr/bin/env bash\n"
            "test_name=$4\n"
            "printf '%s\\n' \"$test_name\" >> \"$TEST_INVOCATION_LOG\"\n"
            "printf 'running 1 test\\n'\n"
            "if [[ $test_name == external_fs_uae_opforge_native_cli_schema_binary_parity_matches_live_rust_cli ]]; then\n"
            "  printf 'test tests::%s ... SKIP: emulator configuration became unavailable\\n' \"$test_name\"\n"
            "  printf 'ok\\n'\n"
            "else\n"
            "  printf 'test tests::%s ... ok\\n' \"$test_name\"\n"
            "fi\n"
            "printf 'test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured\\n'\n"
        )
        with tempfile.TemporaryDirectory() as temp_dir:
            env, log = self.configured_environment(temp_dir, cargo_body)
            result = subprocess.run(
                ["bash", str(self.wrapper), "--verify"],
                cwd=self.root,
                env=env,
                text=True,
                capture_output=True,
                check=False,
            )
            invocations = log.read_text(encoding="utf-8").splitlines()
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(invocations, list(self.tests))
        self.assertIn("failed for 1/11 tests", result.stderr)


if __name__ == "__main__":
    unittest.main()

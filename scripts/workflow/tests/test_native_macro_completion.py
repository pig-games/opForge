import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path


class NativeMacroCompletionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.root = Path(__file__).resolve().parents[3]
        cls.wrapper = cls.root / "scripts/workflow/run_native_macro_completion.sh"
        cls.tests = (
            "native_macro_invocation_fixture_fs_uae",
            "native_macro_preprocessor_harness_fs_uae_proves_capture_lookup_and_nested_frame_rejection",
        )

    def test_missing_configuration_fails_closed(self):
        env = os.environ.copy()
        for name in ("OPFORGE_FS_UAE_SMOKE", "OPFORGE_FS_UAE_BIN", "OPFORGE_FS_UAE_CONFIG_TEMPLATE", "OPFORGE_FS_UAE_ARGS"):
            env.pop(name, None)
        result = subprocess.run(["bash", str(self.wrapper), "--check-config"], cwd=self.root, env=env, text=True, capture_output=True, check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("required for fail-closed native macro completion", result.stderr)

    def test_wrapper_declares_canonical_tests_and_modes(self):
        source = self.wrapper.read_text(encoding="utf-8")
        for name in self.tests:
            self.assertIn(name, source)
        for mode in ("--verify", "--manifest", "--check-config"):
            self.assertIn(mode, source)
        self.assertIn("clean worktree", source)

    def test_skip_and_zero_test_output_fail_closed(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            wrapper = root / "scripts/workflow/run_native_macro_completion.sh"
            wrapper.parent.mkdir(parents=True)
            shutil.copy2(self.wrapper, wrapper)
            test_source = root / "crates/opforge-asm/src/tests/native_macro_completion.rs"
            test_source.parent.mkdir(parents=True)
            test_source.write_text("\n".join(f"fn {name}() {{}}" for name in self.tests), encoding="utf-8")
            fake_bin = root / "bin"; fake_bin.mkdir()
            (fake_bin / "git").write_text("#!/usr/bin/env bash\nif [[ $1 == rev-parse ]]; then printf '%040d\\n' 1; fi\n", encoding="utf-8")
            (fake_bin / "cargo").write_text("#!/usr/bin/env bash\nprintf 'running 1 test\\nSKIP: fake\\ntest result: ok. 1 passed; 0 failed;\\n'\n", encoding="utf-8")
            emulator = fake_bin / "fs-uae"; emulator.write_text("#!/usr/bin/env bash\n", encoding="utf-8")
            for executable in fake_bin.iterdir(): executable.chmod(0o755)
            config = root / "config.fs-uae"; config.write_text("", encoding="utf-8")
            env = os.environ.copy(); env.update({"PATH": f"{fake_bin}:{env['PATH']}", "CARGO": str(fake_bin / "cargo"), "OPFORGE_FS_UAE_SMOKE": "1", "OPFORGE_FS_UAE_BIN": str(emulator), "OPFORGE_FS_UAE_CONFIG_TEMPLATE": str(config), "OPFORGE_FS_UAE_ARGS": "{fsuae_config}"})
            result = subprocess.run(["bash", str(wrapper), "--verify"], cwd=root, env=env, text=True, capture_output=True, check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("did not run and pass exactly once", result.stderr)

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
        self.assertIn("PASS: native macro Level D completion verified", result.stdout)

    def test_manifest_rejects_a_dirty_worktree_before_running_tests(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            wrapper = root / "scripts/workflow/run_native_macro_completion.sh"
            wrapper.parent.mkdir(parents=True)
            shutil.copy2(self.wrapper, wrapper)
            fake_bin = root / "bin"
            fake_bin.mkdir()
            (fake_bin / "git").write_text(
                "#!/usr/bin/env bash\nif [[ $1 == status ]]; then printf ' M file\\n'; fi\n",
                encoding="utf-8",
            )
            emulator = fake_bin / "fs-uae"
            emulator.write_text("#!/usr/bin/env bash\n", encoding="utf-8")
            for executable in fake_bin.iterdir():
                executable.chmod(0o755)
            config = root / "config.fs-uae"
            config.write_text("", encoding="utf-8")
            env = os.environ.copy()
            env.update(
                {
                    "PATH": f"{fake_bin}:{env['PATH']}",
                    "OPFORGE_FS_UAE_SMOKE": "1",
                    "OPFORGE_FS_UAE_BIN": str(emulator),
                    "OPFORGE_FS_UAE_CONFIG_TEMPLATE": str(config),
                    "OPFORGE_FS_UAE_ARGS": "{fsuae_config}",
                }
            )
            result = subprocess.run(
                ["bash", str(wrapper), "--manifest", str(root / "receipt.json")],
                cwd=root,
                env=env,
                text=True,
                capture_output=True,
                check=False,
            )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("requires a clean worktree", result.stderr)


if __name__ == "__main__":
    unittest.main()

import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path


class NativeCliExpansionCompletionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.root = Path(__file__).resolve().parents[3]
        cls.wrapper = cls.root / "scripts/workflow/run_native_cli_expansion_completion.sh"

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

    def test_wrapper_requires_a_manifest_for_execution(self):
        result = subprocess.run(
            ["bash", str(self.wrapper)],
            cwd=self.root,
            text=True,
            capture_output=True,
            check=False,
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("--manifest is required", result.stderr)

    def test_wrapper_names_every_required_level_d_test_and_skip_guard(self):
        source = self.wrapper.read_text(encoding="utf-8")
        for name in (
            "native_column_one_directive_routing_fs_uae",
            "native_opcore_counted_for_fs_uae",
            "native_opcore_sequence_assignment_fs_uae",
            "native_opcore_iterable_for_fs_uae",
            "native_opcore_while_fs_uae",
            "native_opcore_conditionals_fs_uae",
            "native_opcore_scopes_fs_uae",
        ):
            self.assertIn(name, source)
        self.assertIn("required Level D test skipped", source)
        self.assertIn("clean worktree", source)
        self.assertIn("--test-threads=1", source)

    def test_check_config_rejects_non_executable_emulator(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            config = Path(temp_dir) / "config.fs-uae"
            config.write_text("", encoding="utf-8")
            env = os.environ.copy()
            env.update(
                {
                    "OPFORGE_FS_UAE_SMOKE": "1",
                    "OPFORGE_FS_UAE_BIN": str(Path(temp_dir) / "missing-fs-uae"),
                    "OPFORGE_FS_UAE_CONFIG_TEMPLATE": str(config),
                    "OPFORGE_FS_UAE_ARGS": "{fsuae_config}",
                }
            )
            result = subprocess.run(
                ["bash", str(self.wrapper), "--check-config"],
                cwd=self.root,
                env=env,
                text=True,
                capture_output=True,
                check=False,
            )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("binary is not executable", result.stderr)

    def test_zero_test_filtered_run_fails_closed(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            wrapper = root / "scripts/workflow/run_native_cli_expansion_completion.sh"
            wrapper.parent.mkdir(parents=True)
            shutil.copy2(self.wrapper, wrapper)
            test_source = root / "crates/opforge-asm/src/tests.rs"
            test_source.parent.mkdir(parents=True)
            test_names = (
                "native_column_one_directive_routing_fs_uae",
                "native_opcore_counted_for_fs_uae",
                "native_opcore_sequence_assignment_fs_uae",
                "native_opcore_iterable_for_fs_uae",
                "native_opcore_while_fs_uae",
                "native_opcore_conditionals_fs_uae",
                "native_opcore_scopes_fs_uae",
            )
            test_source.write_text(
                "\n".join(f"#[test]\nfn {name}() {{}}" for name in test_names),
                encoding="utf-8",
            )
            fake_bin = root / "bin"
            fake_bin.mkdir()
            fake_git = fake_bin / "git"
            fake_git.write_text(
                "#!/usr/bin/env bash\n"
                "if [[ $1 == status ]]; then exit 0; fi\n"
                "if [[ $1 == rev-parse && $2 == HEAD ]]; then printf '%040d\\n' 1; exit 0; fi\n"
                "if [[ $1 == rev-parse ]]; then printf '%040d\\n' 2; exit 0; fi\n"
                "exit 2\n",
                encoding="utf-8",
            )
            fake_cargo = fake_bin / "cargo"
            fake_cargo.write_text(
                "#!/usr/bin/env bash\n"
                "printf 'running 0 tests\\n\\n'\n"
                "printf 'test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out\\n'\n",
                encoding="utf-8",
            )
            fake_emulator = fake_bin / "fs-uae"
            fake_emulator.write_text("#!/usr/bin/env bash\n", encoding="utf-8")
            for executable in (fake_git, fake_cargo, fake_emulator):
                executable.chmod(0o755)
            config = root / "config.fs-uae"
            config.write_text("", encoding="utf-8")
            env = os.environ.copy()
            env.update(
                {
                    "PATH": f"{fake_bin}:{env['PATH']}",
                    "CARGO": str(fake_cargo),
                    "OPFORGE_FS_UAE_SMOKE": "1",
                    "OPFORGE_FS_UAE_BIN": str(fake_emulator),
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
        self.assertIn("did not run and pass exactly once", result.stderr)

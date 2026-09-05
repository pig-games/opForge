"""Command-contract tests for the explicit, fail-closed LSP gate deferral."""
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


SCRIPT = Path(__file__).resolve().parents[1] / "run_rust_quality_gate.sh"


class RustQualityGateScopeTests(unittest.TestCase):
    def run_gate(self, *args, fail_test=False):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            workflow = root / "scripts/workflow"
            workflow.mkdir(parents=True)
            shutil.copyfile(SCRIPT, workflow / SCRIPT.name)
            native = workflow / "run_native_68000_format_gate.sh"
            native.write_text("#!/bin/sh\nexit 0\n")
            native.chmod(0o755)
            commands = root / "bin"
            commands.mkdir()
            log = root / "commands.log"
            for name in ("cargo", "python3", "cc"):
                command = commands / name
                command.write_text(
                    '#!/bin/sh\nprintf "%s\\n" "' + name + ' $*" >> "$GATE_TEST_LOG"\n'
                    'if [ "$1" = test ] && [ "$GATE_TEST_FAIL" = 1 ]; then exit 7; fi\n'
                    'exit 0\n'
                )
                command.chmod(0o755)
            environment = dict(os.environ, PATH=f"{commands}:{os.environ['PATH']}",
                               GATE_TEST_LOG=str(log), GATE_TEST_FAIL=str(int(fail_test)))
            result = subprocess.run(["bash", str(workflow / SCRIPT.name), *args],
                                    text=True, capture_output=True, env=environment)
            return result, log.read_text().splitlines() if log.exists() else []

    def test_default_keeps_full_gate(self):
        result, commands = self.run_gate()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("cargo clippy -- -D warnings", commands)
        self.assertIn("cargo test --locked", commands)
        self.assertIn("PASS: Rust quality gate complete.", result.stdout)
        self.assertNotIn("DEFERRED", result.stdout)

    def test_explicit_mode_excludes_only_lsp(self):
        result, commands = self.run_gate("--defer-lsp")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("cargo clippy --workspace --exclude lsp -- -D warnings", commands)
        self.assertIn("cargo test --workspace --exclude lsp --locked", commands)
        self.assertIn("cargo fmt --all", commands)
        self.assertIn("cargo audit", commands)
        self.assertIn("cc --version", commands)
        self.assertEqual(sum(line.startswith("python3 ") for line in commands), 2)
        self.assertIn("lsp clippy/tests DEFERRED", result.stdout)
        self.assertNotIn("PASS: Rust quality gate complete.", result.stdout)

    def test_other_failures_still_fail(self):
        result, _ = self.run_gate("--defer-lsp", fail_test=True)
        self.assertEqual(result.returncode, 7)
        self.assertNotIn("PASS:", result.stdout)

    def test_unknown_or_extra_option_rejected(self):
        for args in (("--exclude",), ("--defer-lsp", "extra")):
            with self.subTest(args=args):
                result, commands = self.run_gate(*args)
                self.assertNotEqual(result.returncode, 0)
                self.assertEqual(commands, [])


if __name__ == "__main__":
    unittest.main()

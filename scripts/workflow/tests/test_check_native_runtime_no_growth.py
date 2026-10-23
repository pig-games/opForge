import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import Mock, patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_native_runtime_no_growth import (  # noqa: E402
    BASELINE,
    load_baseline,
    run_cpu_boundary,
    validate,
)


class NativeRuntimeNoGrowthTests(unittest.TestCase):
    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.root = Path(self.tempdir.name)
        (self.root / "scripts/workflow").mkdir(parents=True)

    def tearDown(self):
        self.tempdir.cleanup()

    def write(self, relative: str, text: str) -> Path:
        path = self.root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")
        return path

    def baseline(self, hotspot: str | None = None, blocks: tuple[str, ...] = ()) -> Path:
        lines = ["schema_version = 1", "", "certified_modules = []"]
        if hotspot:
            rendered = ", ".join(f'"{name}"' for name in blocks)
            lines.extend(["", f'[hotspots."{hotspot}"]', f"blocks = [{rendered}]"])
        return self.write(
            "scripts/workflow/native_runtime_ownership_baseline.toml",
            "\n".join(lines) + "\n",
        )

    def test_repository_baseline_and_current_sources_pass(self):
        baseline = load_baseline(BASELINE)
        self.assertEqual(len(baseline.hotspot_blocks), 4)
        self.assertIn(
            "native/motorola68000/amigaos/assembler/driver.asm",
            baseline.hotspot_blocks,
        )
        self.assertIn(
            "native/motorola68000/amigaos/vm/exprvm/i64_math.asm",
            baseline.hotspot_blocks,
        )
        self.assertEqual(validate(), [])

    def test_new_private_hotspot_routine_fails(self):
        hotspot = "native/motorola68000/amigaos/assembler/prepare/hotspot.asm"
        baseline = self.baseline(hotspot, ("existingV1",))
        self.write(
            hotspot,
            ".module opforge.assembler.prepare.hotspot\nexistingV1 .block\nnewSemanticRoutine .block\n",
        )
        errors = validate(self.root, baseline)
        self.assertTrue(any("newSemanticRoutine" in error for error in errors))
        self.assertTrue(any("@opforge-role" in error for error in errors))

    def test_declared_hotspot_delegation_is_allowed(self):
        hotspot = "native/motorola68000/amigaos/assembler/prepare/hotspot.asm"
        baseline = self.baseline(hotspot, ("existingV1",))
        self.write(
            hotspot,
            ".module opforge.assembler.prepare.hotspot\nexistingV1 .block\n"
            "; @opforge-owner: opforge.assembler.prepare.owner\n"
            "; @opforge-role: delegation\n"
            "delegateV1 .block\n",
        )
        self.assertEqual(validate(self.root, baseline), [])

    def test_declared_hotspot_facade_is_allowed(self):
        hotspot = "native/motorola68000/amigaos/assembler/prepare/hotspot.asm"
        baseline = self.baseline(hotspot, ("existingV1",))
        self.write(
            hotspot,
            ".module opforge.assembler.prepare.hotspot\nexistingV1 .block\n"
            "; @opforge-owner: opforge.assembler.prepare.owner\n"
            "; @opforge-role: facade\n"
            "facadeV1 .block\n",
        )
        self.assertEqual(validate(self.root, baseline), [])

    def test_native_test_modules_are_not_production_hotspots(self):
        baseline = self.baseline()
        self.write(
            "native/motorola68000/amigaos/tests/prepare/probe.asm",
            ".module opforge.tests.prepare.probe\nrun .block\n",
        )
        self.assertEqual(validate(self.root, baseline), [])

    def test_new_semantic_module_requires_owner(self):
        baseline = self.baseline()
        self.write(
            "native/motorola68000/amigaos/assembler/prepare/new_owner.asm",
            ".module opforge.assembler.prepare.new_owner\nrunV1 .block\n",
        )
        errors = validate(self.root, baseline)
        self.assertTrue(any("missing @opforge-owner" in error for error in errors))

    def test_new_semantic_module_with_owner_is_allowed_without_slice(self):
        baseline = self.baseline()
        self.write(
            "native/motorola68000/amigaos/assembler/prepare/new_owner.asm",
            "; @opforge-owner: opforge.assembler.prepare.new_owner\n"
            ".module opforge.assembler.prepare.new_owner\nrunV1 .block\n",
        )
        self.assertEqual(validate(self.root, baseline), [])

    def test_hotspot_invalid_role_fails(self):
        hotspot = "native/motorola68000/amigaos/assembler/prepare/hotspot.asm"
        baseline = self.baseline(hotspot)
        self.write(
            hotspot,
            "; @opforge-owner: opforge.assembler.prepare.owner\n"
            "; @opforge-role: implementation\n"
            ".module opforge.assembler.prepare.hotspot\n"
            "; @opforge-owner: opforge.assembler.prepare.owner\n"
            "; @opforge-role: implementation\n"
            "newRoutine .block\n",
        )
        errors = validate(self.root, baseline)
        self.assertTrue(any("@opforge-role" in error for error in errors))

    def test_cpu_boundary_guard_receives_staged_mode(self):
        with patch(
            "check_native_runtime_no_growth.subprocess.run",
            return_value=Mock(returncode=0),
        ) as runner:
            self.assertEqual(run_cpu_boundary(Path("/repo"), True), 0)
        self.assertEqual(
            runner.call_args.args[0],
            [
                sys.executable,
                "/repo/scripts/workflow/check_cpu_specific_arch_boundary.py",
                "--no-report",
                "--staged",
            ],
        )


if __name__ == "__main__":
    unittest.main()

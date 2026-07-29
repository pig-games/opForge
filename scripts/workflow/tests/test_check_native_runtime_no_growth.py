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
        (self.root / "native/motorola68000/amigaos/tkpkg").mkdir(parents=True)

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
        self.assertEqual(len(baseline.hotspot_blocks), 3)
        self.assertIn(
            "native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm",
            baseline.hotspot_blocks,
        )
        self.assertEqual(validate(), [])

    def test_new_private_hotspot_routine_fails(self):
        hotspot = "native/motorola68000/amigaos/opasm/hotspot.asm"
        baseline = self.baseline(hotspot, ("existingV1",))
        self.write(
            hotspot,
            ".module opasm.hotspot\nexistingV1 .block\nnewSemanticRoutine .block\n",
        )
        errors = validate(self.root, baseline)
        self.assertTrue(any("newSemanticRoutine" in error for error in errors))
        self.assertTrue(any("@opforge-role" in error for error in errors))

    def test_declared_hotspot_delegation_is_allowed(self):
        hotspot = "native/motorola68000/amigaos/opasm/hotspot.asm"
        baseline = self.baseline(hotspot, ("existingV1",))
        slice_path = "documentation/plans/slices/delegation.toml"
        self.write(slice_path, "[slice]\nschema_version = 2\n")
        self.write(
            hotspot,
            ".module opasm.hotspot\nexistingV1 .block\n"
            "; @opforge-owner: opasm.owner\n"
            f"; @opforge-slice: {slice_path}\n"
            "; @opforge-role: delegation\n"
            "delegateV1 .block\n",
        )
        self.assertEqual(validate(self.root, baseline), [])

    def test_declared_hotspot_facade_is_allowed(self):
        hotspot = "native/motorola68000/amigaos/opasm/hotspot.asm"
        baseline = self.baseline(hotspot, ("existingV1",))
        slice_path = "documentation/plans/slices/facade.toml"
        self.write(slice_path, "[slice]\nschema_version = 2\n")
        self.write(
            hotspot,
            ".module opasm.hotspot\nexistingV1 .block\n"
            "; @opforge-owner: opasm.owner\n"
            f"; @opforge-slice: {slice_path}\n"
            "; @opforge-role: facade\n"
            "facadeV1 .block\n",
        )
        self.assertEqual(validate(self.root, baseline), [])

    def test_tkpkg_direct_engine_mutable_state_access_fails(self):
        baseline = self.baseline()
        self.write(
            "native/motorola68000/amigaos/tkpkg/bad.asm",
            "\tmove.l OpasmEngineLabelValueTable, d0\n",
        )
        errors = validate(self.root, baseline)
        self.assertTrue(any("direct tkpkg access" in error for error in errors))

    def test_tkpkg_comments_and_strings_do_not_count_as_state_access(self):
        baseline = self.baseline()
        self.write(
            "native/motorola68000/amigaos/tkpkg/comment.asm",
            '; OpasmEngineLabelValueTable\n\tdc.b "OpasmEngineLabelValueTable",0\n',
        )
        self.assertEqual(validate(self.root, baseline), [])

    def test_new_semantic_module_requires_owner_and_slice(self):
        baseline = self.baseline()
        self.write(
            "native/motorola68000/amigaos/opasm/new_owner.asm",
            ".module opasm.new_owner\nrunV1 .block\n",
        )
        errors = validate(self.root, baseline)
        self.assertTrue(any("missing @opforge-owner" in error for error in errors))
        self.assertTrue(any("missing @opforge-slice" in error for error in errors))

    def test_new_semantic_module_with_existing_slice_is_allowed(self):
        baseline = self.baseline()
        slice_path = "documentation/plans/slices/new-owner.toml"
        self.write(slice_path, "[slice]\nschema_version = 2\n")
        self.write(
            "native/motorola68000/amigaos/opasm/new_owner.asm",
            "; @opforge-owner: opasm.new_owner\n"
            f"; @opforge-slice: {slice_path}\n"
            ".module opasm.new_owner\nrunV1 .block\n",
        )
        self.assertEqual(validate(self.root, baseline), [])

    def test_declared_module_with_missing_slice_file_fails(self):
        baseline = self.baseline()
        self.write(
            "native/motorola68000/amigaos/opasm/new_owner.asm",
            "; @opforge-owner: opasm.new_owner\n"
            "; @opforge-slice: documentation/plans/slices/missing.toml\n"
            ".module opasm.new_owner\nrunV1 .block\n",
        )
        errors = validate(self.root, baseline)
        self.assertTrue(any("does not exist" in error for error in errors))

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

import sys
import subprocess
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_native_runtime_boundary_contract import (
    ASSEMBLER_IMPORTS,
    BOUNDARY_ROOTS,
    NATIVE_ROOT,
    OWNER_IMPORTS,
    validate,
)


class NativeRuntimeBoundaryContractTests(unittest.TestCase):
    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.addCleanup(self.tempdir.cleanup)
        self.root = Path(self.tempdir.name)
        for directory in BOUNDARY_ROOTS:
            (self.root / directory).mkdir(parents=True)
        for relative in set(ASSEMBLER_IMPORTS) | set(OWNER_IMPORTS):
            edges = OWNER_IMPORTS.get(relative, ASSEMBLER_IMPORTS.get(relative, ()))
            self.write(relative, "".join(f".use {edge}\n" for edge in sorted(edges)))

    def write(self, relative, text):
        path = self.root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")
        return path

    def errors(self):
        return "\n".join(validate(self.root))

    def test_repository_imports_pass(self):
        self.assertEqual(validate(), [])

    def test_staged_cli_still_checks_full_supplied_root(self):
        script = Path(__file__).resolve().parents[1] / "check_native_runtime_boundary_contract.py"
        command = [sys.executable, str(script), "--root", str(self.root), "--staged"]
        result = subprocess.run(command, capture_output=True, text=True, check=False)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.write(f"{NATIVE_ROOT}/vm/untracked.i", ".use opforge.cli.entry\n")
        result = subprocess.run(command, capture_output=True, text=True, check=False)
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertIn("untracked.i -> opforge.cli.entry", result.stdout)

    def test_valid_imports_pass_without_plans_or_prose_ledgers(self):
        self.assertFalse((self.root / "documentation").exists())
        self.assertEqual(validate(self.root), [])

    def test_new_source_is_discovered_and_reverse_edges_fail(self):
        for edge in ("opforge.assembler.driver", "opforge.assembler.prepare.expression",
                     "opforge.cli.progress", "opforge.cli", "opforge.assembler"):
            with self.subTest(edge=edge):
                self.write(f"{NATIVE_ROOT}/vm/new/runtime.asm", f".use {edge}\n")
                self.assertIn(f"prohibited reverse import: {NATIVE_ROOT}/vm/new/runtime.asm -> {edge}",
                              self.errors())

    def test_nested_helpers_outside_vm_directory_are_checked(self):
        self.write(f"{NATIVE_ROOT}/vm/new.asm", '.include "../debug/helper.i"\n')
        self.write(f"{NATIVE_ROOT}/debug/helper.i", '.include "nested.i"\n')
        self.write(f"{NATIVE_ROOT}/debug/nested.i", '.use opforge.assembler.driver\n')
        self.assertIn("debug/nested.i -> opforge.assembler.driver", self.errors())

    def test_unreferenced_package_helper_is_also_checked(self):
        self.write(f"{NATIVE_ROOT}/packages/new.i", '.use opforge.cli.input\n')
        self.assertIn("packages/new.i -> opforge.cli.input", self.errors())

    def test_uppercase_directives_cannot_hide_include_import(self):
        self.write(f"{NATIVE_ROOT}/vm/new.asm", '.INCLUDE "../debug/helper.i"\n')
        self.write(f"{NATIVE_ROOT}/debug/helper.i", '.USE OPFORGE.ASSEMBLER.DRIVER\n')
        self.assertIn("debug/helper.i -> opforge.assembler.driver", self.errors())

    def test_conditional_and_template_imports_are_possible_edges(self):
        self.write(f"{NATIVE_ROOT}/vm/new.i", '''.if 0
M .macro
.use opforge.cli.entry
.endmacro
.endif
''')
        self.assertIn("new.i -> opforge.cli.entry", self.errors())

    def test_reviewed_exception_does_not_extend_to_helpers(self):
        path = self.root / f"{NATIVE_ROOT}/vm/exvm/runtime.asm"
        path.write_text(path.read_text() + '.include "new.i"\n')
        self.write(f"{NATIVE_ROOT}/vm/exvm/new.i", '.use opforge.assembler.source.records\n')
        self.assertIn("exvm/new.i -> opforge.assembler.source.records", self.errors())

    def test_reviewed_exception_does_not_allow_child_namespace(self):
        path = self.root / f"{NATIVE_ROOT}/vm/exvm/runtime.asm"
        path.write_text(path.read_text() + '.use opforge.assembler.source.records.service\n')
        self.assertIn("records.service", self.errors())

    def test_unused_exception_requires_review(self):
        self.write(f"{NATIVE_ROOT}/vm/exvm/runtime.asm", "")
        self.assertIn("unused reviewed import exception", self.errors())

    def test_missing_exception_source_fails(self):
        (self.root / f"{NATIVE_ROOT}/vm/exvm/runtime.asm").unlink()
        self.assertIn("missing source/include", self.errors())

    def test_missing_directory_fails(self):
        for path in (self.root / NATIVE_ROOT / "packages").glob("*"):
            path.unlink()
        (self.root / NATIVE_ROOT / "packages").rmdir()
        self.assertIn("missing native boundary source directory", self.errors())

    def test_namespace_matching_does_not_reject_unrelated_prefixes(self):
        self.write(f"{NATIVE_ROOT}/vm/new.i", ".use opforge.cli_tools\n.use opforge.assembler_tools\n")
        self.assertEqual(validate(self.root), [])

    def test_owner_import_order_is_not_a_boundary(self):
        path = f"{NATIVE_ROOT}/cli/entry.asm"
        self.write(path, "".join(f".use {edge}\n" for edge in reversed(sorted(OWNER_IMPORTS[path]))))
        self.assertEqual(validate(self.root), [])

    def test_owner_import_changes_and_duplicates_fail(self):
        relative = f"{NATIVE_ROOT}/cli/entry.asm"
        path = self.root / relative
        original = path.read_text()
        for text, message in (("", "owner imports changed"),
                              (original + ".use opforge.vm.prvm.runtime\n", "owner imports changed"),
                              (original + ".use opforge.cli.input\n", "duplicate owner imports")):
            with self.subTest(message=message, text=text):
                path.write_text(text)
                self.assertIn(message, self.errors())

    def test_owner_helpers_share_import_set_without_becoming_vm_owners(self):
        relative = f"{NATIVE_ROOT}/cli/entry.asm"
        self.write(relative, '.include "entry_helpers.i"\n')
        self.write(f"{NATIVE_ROOT}/cli/entry_helpers.i", "".join(
            f".use {edge}\n" for edge in sorted(OWNER_IMPORTS[relative])))
        self.assertEqual(validate(self.root), [])

    def test_including_another_module_owner_is_rejected(self):
        self.write(f"{NATIVE_ROOT}/vm/new.asm", '.include "other.asm"\n')
        self.write(f"{NATIVE_ROOT}/vm/other.asm", '.module opforge.vm.other\n')
        self.assertIn("included module owner", self.errors())

    def test_missing_and_nonliteral_includes_fail_closed(self):
        for text, message in (('.include "missing.i"\n', "missing source/include"),
                              ('.include filename\n', "unsupported nonliteral include")):
            with self.subTest(text=text):
                self.write(f"{NATIVE_ROOT}/vm/new.asm", text)
                self.assertIn(message, self.errors())


if __name__ == "__main__":
    unittest.main()

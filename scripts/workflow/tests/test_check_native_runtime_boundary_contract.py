import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_native_runtime_boundary_contract import (
    FORBIDDEN_IMPORTS,
    RETAINED_OWNER_IMPORTS,
    validate,
)


class NativeRuntimeBoundaryContractTests(unittest.TestCase):
    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.addCleanup(self.tempdir.cleanup)
        self.root = Path(self.tempdir.name)
        paths = set(FORBIDDEN_IMPORTS) | set(RETAINED_OWNER_IMPORTS)
        for relative in paths:
            path = self.root / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("".join(
                f".use {module}\n"
                for module in RETAINED_OWNER_IMPORTS.get(relative, ())
            ))

    def test_repository_imports_pass(self):
        self.assertEqual(validate(), [])

    def test_valid_imports_pass_without_plans_or_prose_ledgers(self):
        self.assertFalse((self.root / "documentation").exists())
        self.assertEqual(validate(self.root), [])

    def test_prohibited_reverse_import_fails(self):
        path = self.root / "native/motorola68000/amigaos/vm/prvm/runtime.asm"
        path.write_text(".use opforge.assembler.driver\n")
        self.assertTrue(any("prohibited current reverse import" in error
                            for error in validate(self.root)))

    def test_retained_owner_import_change_fails(self):
        path = self.root / "native/motorola68000/amigaos/cli/entry.asm"
        path.write_text("")
        self.assertTrue(any("retained-owner imports changed" in error
                            for error in validate(self.root)))

    def test_vm_wire_contract_import_remains_allowed(self):
        path = self.root / "native/motorola68000/amigaos/vm/exvm/runtime.asm"
        path.write_text(".use opforge.assembler.source.records\n")
        self.assertEqual(validate(self.root), [])


if __name__ == "__main__":
    unittest.main()

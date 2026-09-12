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
        paths.add("native/motorola68000/amigaos/tkpkg/tkpkg_service.asm")
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
        path = self.root / "native/motorola68000/amigaos/prvm/prvm_runtime.asm"
        path.write_text(".use opasm.amigaos.engine\n")
        self.assertTrue(any("prohibited current reverse import" in error
                            for error in validate(self.root)))

    def test_retained_owner_import_change_fails(self):
        path = self.root / "native/motorola68000/amigaos/opasm/opasm_engine.asm"
        path.write_text("")
        self.assertTrue(any("retained-owner imports changed" in error
                            for error in validate(self.root)))

    def test_obsolete_service_to_engine_import_fails(self):
        path = self.root / "native/motorola68000/amigaos/tkpkg/tkpkg_service.asm"
        path.write_text(".use opasm.amigaos.engine\n")
        self.assertTrue(any("obsolete service-to-engine import" in error
                            for error in validate(self.root)))


if __name__ == "__main__":
    unittest.main()

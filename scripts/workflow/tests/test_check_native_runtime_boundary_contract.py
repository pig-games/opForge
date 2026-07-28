import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_native_runtime_boundary_contract import (
    FORBIDDEN_IMPORTS,
    LEDGER_ITEMS,
    RETAINED_ITEM_511_IMPORTS,
    validate,
)


class NativeRuntimeBoundaryContractTests(unittest.TestCase):
    def test_checked_contract_and_dependency_model_pass(self):
        self.assertEqual(validate(), [])

    def test_contract_has_a_lifecycle_entry_for_each_future_extraction(self):
        self.assertEqual(len(LEDGER_ITEMS), 19)
        self.assertIn("5.7", LEDGER_ITEMS)
        self.assertIn("5.9.4", LEDGER_ITEMS)

    def test_reverse_edge_scope_covers_engine_driver_and_runtime_consumers(self):
        self.assertEqual(len(FORBIDDEN_IMPORTS), 6)
        self.assertIn("native/motorola68000/amigaos/opasm/opasm_engine.asm", FORBIDDEN_IMPORTS)

    def test_item_511_pins_exact_retained_owner_imports(self):
        self.assertEqual(len(RETAINED_ITEM_511_IMPORTS), 2)
        self.assertEqual(
            RETAINED_ITEM_511_IMPORTS[
                "native/motorola68000/amigaos/opasm/opasm_engine.asm"
            ],
            ("opasm.amigaos.events",),
        )
        self.assertEqual(
            RETAINED_ITEM_511_IMPORTS[
                "native/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm"
            ],
            (
                "tkpkg.amigaos.abi",
                "tkpkg.amigaos.buffers",
                "tkpkg.amigaos.token_policy",
            ),
        )


if __name__ == "__main__":
    unittest.main()

import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_native_runtime_boundary_contract import FORBIDDEN_IMPORTS, LEDGER_ITEMS, validate


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


if __name__ == "__main__":
    unittest.main()

import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_native_runtime_boundary_inventory import TARGETS, validate


class NativeRuntimeBoundaryInventoryTests(unittest.TestCase):
    def test_inventory_matches_all_audited_sources(self):
        self.assertEqual(validate(), [])

    def test_audit_set_covers_exactly_eight_modules(self):
        self.assertEqual(len(TARGETS), 8)
        self.assertIn("opasm.amigaos.assembly_driver", TARGETS)
        self.assertIn("tkpkg.amigaos.service", TARGETS)


if __name__ == "__main__":
    unittest.main()

import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_native_runtime_boundary_inventory import TARGETS, validate


class NativeRuntimeBoundaryInventoryTests(unittest.TestCase):
    def test_inventory_matches_all_audited_sources(self):
        self.assertEqual(validate(), [])

    def test_audit_set_covers_promoted_orchestration_packages_and_vms(self):
        self.assertEqual(len(TARGETS), 25)
        for module in (
            "opforge.assembler.driver", "opforge.assembler.prepare.frontend",
            "opforge.assembler.layout.assembly", "opforge.packages.loader",
            "opforge.packages.validation", "opforge.vm.exvm.runtime",
            "opforge.vm.encoding.runtime", "opforge.vm.value.runtime",
            "opforge.vm.exprvm.i64_math", "opforge.cli.entry",
        ):
            self.assertIn(module, TARGETS)


if __name__ == "__main__":
    unittest.main()

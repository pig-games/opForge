import importlib.util
from pathlib import Path
import re
import sys
import unittest


SPEC = importlib.util.spec_from_file_location(
    "prepared_source_native", Path(__file__).parents[1] / "prepared_source_native.py"
)
runner = importlib.util.module_from_spec(SPEC)
sys.path.insert(0, str(Path(__file__).parents[1]))
try:
    SPEC.loader.exec_module(runner)
finally:
    sys.path.pop(0)


class PreparedSourceNativeTests(unittest.TestCase):
    def test_expression_replay_has_stable_independent_bytes_for_both_families(self):
        known = {
            ("m6502", 8): "a902a2068d0020d0040c010d20ea",
            ("m68000", 8): "7002323c000611c0200066040c0120104e71",
        }
        for (cpu, blocks), prefix in known.items():
            source, expected = runner.workload(cpu, blocks, "expression-replay")
            self.assertTrue(expected.hex().startswith(prefix))
            self.assertEqual(len(expected), (14 if cpu == "m6502" else 18) * blocks)
            self.assertEqual(len(re.findall(r"^expr_start\d+:$", source, re.MULTILINE)), blocks)
            self.assertIn("($ - $)", source)
            self.assertIn("expr_end0 - expr_start0", source)

    def test_layout_adds_multiplication_and_is_a_distinct_contract(self):
        for cpu in ("m6502", "m68000"):
            replay_source, replay_bytes = runner.workload(cpu, 32, "expression-replay")
            layout_source, layout_bytes = runner.workload(cpu, 32, "expression-layout")
            self.assertNotEqual(replay_source, layout_source)
            self.assertNotEqual(replay_bytes, layout_bytes)
            self.assertGreaterEqual(layout_source.count(" * "), 2 * 32)
            self.assertEqual(len(layout_bytes), (14 if cpu == "m6502" else 18) * 32)
            self.assertEqual(layout_bytes[:2].hex(), "a90d" if cpu == "m6502" else "7005")

    def test_expression_workloads_keep_the_bounded_input_sizes(self):
        for cpu, blocks in (("m6502", 1024), ("m68000", 0), ("z80", 8)):
            with self.assertRaises(ValueError):
                runner.expression_workload(cpu, blocks)


if __name__ == "__main__":
    unittest.main()

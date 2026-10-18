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
    def test_8m_config_pins_two_fast_banks_and_removes_extra_memory(self):
        template = "[fs-uae]\nfast_memory = 8192\nuae_fastmem2_size = 8\nuae_z3mem_size = 64\nhard_drive_0 = /sys\nuae_cpu_type = 68040\nuae_addmem1 = 0x10000000,0x800000\nuae_addmem2 = 0x20000000,0x800000\nuae_quickstart = A4000,1\ncpu_speed = real\naccuracy = 1\nuae_cpu_speed = real\nuae_cpu_cycle_exact = true\nuae_cpu_memory_cycle_exact = true\nuae_blitter_cycle_exact = true\nuae_cycle_exact = true\n"
        text = runner.effective_emulator_config(template, "68020-8m")
        settings = dict((key.strip(), value.strip()) for line in text.splitlines()
                        if "=" in line for key, value in [line.split("=", 1)])
        self.assertEqual(settings["chip_memory"], "2048")
        self.assertEqual(settings["fast_memory"], "4096")
        self.assertEqual(settings["uae_fastmem2_size"], "2")
        self.assertEqual(settings["uae_z3mem_size"], "0")
        self.assertEqual(settings["graphics_memory"], "0")
        self.assertEqual(settings["hard_drive_0"], "/sys")
        self.assertEqual(settings["cpu_speed"], "max")
        self.assertEqual(settings["accuracy"], "0")
        self.assertEqual(settings["uae_cpu_speed"], "max")
        self.assertEqual(settings["uae_cpu_cycle_exact"], "false")
        self.assertEqual(settings["uae_cpu_memory_cycle_exact"], "false")
        self.assertEqual(settings["uae_blitter_cycle_exact"], "false")
        self.assertEqual(settings["uae_cycle_exact"], "false")
        for removed in ("uae_quickstart", "uae_cpu_type", "uae_addmem1", "uae_addmem2"):
            self.assertNotIn(removed, settings)
        self.assertEqual(len(settings), sum("=" in line for line in text.splitlines()))


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

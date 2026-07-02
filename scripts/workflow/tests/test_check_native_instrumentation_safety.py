import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_native_instrumentation_safety import validate_text


class InstrumentationSafetyTests(unittest.TestCase):
    def test_unsafe_patterns_fail(self):
        cases = {
            "raw": "jsr debugPrint",
            "buffer": "move.l d0, LastErrorBuffer ; debug",
            "label": "temporaryDebug .block",
            "macro": ".DEBUG_ASSERT_UNKNOWN CONTRACT_X",
        }
        for name, text in cases.items():
            with self.subTest(name=name):
                self.assertTrue(validate_text("native/motorola68000/amigaos/x.asm", text))

    def test_approved_macro_at_branch_boundary_passes(self):
        text = "cmp.l d0,d1\n.DEBUG_ASSERT_SPAN_IN_TEXT CONTRACT_X\nbeq.s done"
        self.assertEqual(validate_text("native/motorola68000/amigaos/x.asm", text), [])

    def test_debug_framework_module_is_approved(self):
        self.assertEqual(
            validate_text("native/motorola68000/amigaos/debug/debug_events.asm", "jsr debugWrite"),
            [],
        )


if __name__ == "__main__":
    unittest.main()

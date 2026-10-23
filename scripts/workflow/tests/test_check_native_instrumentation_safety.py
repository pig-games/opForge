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
            validate_text("native/motorola68000/amigaos/debug/events.asm", "jsr debugWrite"),
            [],
        )

    def test_retired_harness_has_no_instrumentation_exemption(self):
        for path in (
            "native/motorola68000/amigaos/tests/tkpkg/debug_cli.asm",
            "native/motorola68000/amigaos/tests/tkpkg/expression_i64_harness.asm",
        ):
            self.assertTrue(validate_text(path, "bsr.w writeFailureDiagnostic"))
            self.assertTrue(validate_text(path, "bsr.w debugPrint"))

    def test_functional_byte_warning_output_exception_is_owner_and_routine_scoped(self):
        owner = "native/motorola68000/amigaos/assembler/driver_diagnostics.i"
        routine = "reportByteWarning .block\n" + "\n".join(
            ["bsr.w writeDiagnostic"] * 4
        ) + "\n.bend ; reportByteWarning"
        self.assertEqual(validate_text(owner, routine), [])
        self.assertTrue(validate_text(owner, "bsr.w writeDiagnostic"))
        self.assertTrue(validate_text("native/motorola68000/amigaos/other.i", routine))

        injected = routine.replace(
            "bsr.w writeDiagnostic\nbsr.w writeDiagnostic",
            "bsr.w debugPrint\nbsr.w writeDiagnostic",
            1,
        )
        self.assertTrue(validate_text(owner, injected))

    def test_standalone_harness_still_rejects_prohibited_debug_buffers(self):
        path = "native/motorola68000/amigaos/tests/tkpkg/debug_cli.asm"
        self.assertTrue(validate_text(path, "move.l d0, RequestBuffer ; debug"))


if __name__ == "__main__":
    unittest.main()

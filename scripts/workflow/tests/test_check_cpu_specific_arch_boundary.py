import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import check_cpu_specific_arch_boundary as boundary


class CpuSpecificArchitectureBoundaryTests(unittest.TestCase):
    def test_all_repo_files_prunes_skipped_directories_before_descent(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            (root / "native").mkdir()
            (root / "native" / "runtime.asm").write_text(".module runtime\n")
            (root / ".git" / "objects").mkdir(parents=True)
            (root / ".git" / "objects" / "ignored.asm").write_text("ignored\n")
            (root / "target" / "generated").mkdir(parents=True)
            (root / "target" / "generated" / "ignored.rs").write_text("ignored\n")

            with patch.object(boundary, "REPO_ROOT", root):
                files = boundary.all_repo_files()

        self.assertEqual(files, [root / "native" / "runtime.asm"])

    def scan_native(self, source):
        with tempfile.TemporaryDirectory() as temp_dir:
            path = Path(temp_dir) / "runtime.asm"
            path.write_text(source)
            return boundary.scan_native_asm_file(
                path, "native/runtime.asm", "enforced", ["bsr"], []
            )

    def test_declared_macro_parameter_operand_is_not_a_directive(self):
        findings = self.scan_native(
            "LEVEL .macro tighter, token\n"
            "\tbsr.w .tighter\n"
            "\tbsr .TIGHTER ; case-insensitive parameter\n"
            "\t.endmacro\n"
        )
        self.assertEqual(findings, [])

    def test_macro_parameters_do_not_hide_definitions_or_data(self):
        findings = self.scan_native(
            "LEVEL .macro tighter, block, byte, word\n"
            "\tbsr .block\n"
            "\tbsr .byte 1\n"
            "\tbsr .word 1\n"
            "bsr .macro target\n"
            "\t.endmacro\n"
            "\t.endmacro\n"
        )
        self.assertEqual([finding.line for finding in findings], [2, 3, 4, 5])

    def test_parameter_exception_does_not_escape_its_macro(self):
        findings = self.scan_native(
            "LEVEL .macro tighter\n"
            "\tbsr.w .tighter\n"
            "\t.endmacro\n"
            "\tbsr.w .tighter\n"
            "OTHER .macro other\n"
            "\tbsr.w .tighter\n"
            "\t.endmacro\n"
        )
        self.assertEqual([finding.line for finding in findings], [4, 6])

    def test_print_violations_can_skip_report_mutation(self):
        violation = boundary.Violation(
            path="native/runtime.asm",
            line=1,
            column=1,
            term="d0",
            text="move.l d0,d1",
            severity="error",
            scan_scope="enforced",
        )
        with (
            patch.object(boundary, "write_enforced_report") as write_enforced,
            patch.object(boundary, "write_warning_scan_report") as write_warning,
            patch.object(boundary, "clear_report") as clear_report,
        ):
            boundary.print_violations([violation], reports_enabled=False)

        write_enforced.assert_not_called()
        write_warning.assert_not_called()
        clear_report.assert_not_called()


if __name__ == "__main__":
    unittest.main()

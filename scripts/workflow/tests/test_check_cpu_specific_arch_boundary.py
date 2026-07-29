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

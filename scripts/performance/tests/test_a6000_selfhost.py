import importlib.util
from pathlib import Path
import tempfile
import unittest


SPEC = importlib.util.spec_from_file_location(
    "a6000_selfhost", Path(__file__).parents[1] / "run_a6000_selfhost.py"
)
runner = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(runner)


class HardwareCompletionTests(unittest.TestCase):
    def result(self, *, rc=0, marker="fresh case", output=b"oracle", missing=False):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for name, data in {
                "input": b"source",
                "start.marker": f"START {marker}".encode(),
                "done.marker": f"DONE {marker}".encode(),
                "exitcode": str(rc).encode(),
                "start.time": b"Thursday 01-Oct-26 23:59:58",
                "end.time": b"Friday 02-Oct-26 00:00:03",
                "output.hunk": output,
            }.items():
                (root / name).write_bytes(data)
            if missing:
                (root / "input").unlink()
            return runner.inspect_result(root, {"input": b"source"}, b"oracle", "fresh case", 5.5)

    def test_exact_completed_case_has_guest_time_across_midnight(self):
        result = self.result()
        self.assertTrue(result["success"])
        self.assertEqual(result["guest_overall_seconds"], 5)

    def test_launcher_success_cannot_hide_assembler_failure_or_mismatch(self):
        self.assertFalse(self.result(rc=20)["success"])
        self.assertFalse(self.result(output=b"different")["success"])

    def test_stale_markers_or_incomplete_copy_cannot_prove_completion(self):
        for options in ({"marker": "old case"}, {"missing": True}):
            with self.subTest(options=options), self.assertRaises(ValueError):
                self.result(**options)

    def test_guest_clock_failure_is_not_reported_as_zero(self):
        self.assertIsNone(runner.guest_seconds("no clock", "no clock", 5))
        self.assertIsNone(runner.guest_seconds("10:00:00", "09:00:00", 5))

    def test_guest_script_preserves_exit_before_date_and_binds_markers(self):
        script = runner.guest_script("Development:opforge-fresh", "fresh case", "opforge p.bin src/entry.asm output.hunk")
        self.assertIn("Echo $RC >exitcode\nC:Date >end.time", script)
        self.assertIn('Echo "START fresh case"', script)
        self.assertIn('Echo "DONE fresh case"', script)
        self.assertLessEqual(max(map(len, script.splitlines())), 255)


if __name__ == "__main__":
    unittest.main()

from pathlib import Path
import sys
import tempfile
import unittest
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import run_a6000_incremental as pair


class IncrementalPairTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        for phase, counts in (("cold", "00000000 rebuilt=0x00000003"),
                              ("warm", "00000003 rebuilt=0x00000000")):
            root = self.root / phase
            root.mkdir()
            challenge = pair.phase_marker("fresh digest", phase)
            values = {"start.marker": "START " + challenge,
                      "done.marker": "DONE " + challenge, "exitcode": "0",
                      "copy-exitcode": "0", "start.time": "12:00:00",
                      "end.time": "12:00:02", "output.hunk": "oracle",
                      "assembly.stdout": "incremental: reused=0x" + counts + "\n"}
            for name, data in values.items():
                (root / name).write_text(data)

    def inspect(self):
        return pair.inspect_result(self.root, {}, b"oracle", "fresh digest", 8)

    def test_success_has_independent_guest_times_and_combined_host_time(self):
        result = self.inspect()
        self.assertTrue(result["success"])
        for phase in pair.PHASES:
            self.assertTrue(result["phases"][phase]["exact_rust_match"])
            self.assertEqual(result["phases"][phase]["guest_overall_seconds"], 2)
            self.assertNotIn("host_command_seconds_including_connection", result["phases"][phase])
        self.assertEqual(result["host_command_seconds_including_connection"], 8)

    def test_wrong_output_zero_exit_fails(self):
        (self.root / "warm/output.hunk").write_bytes(b"wrong")
        self.assertFalse(self.inspect()["success"])

    def test_warm_success_does_not_hide_cold_failure(self):
        (self.root / "cold/exitcode").write_text("20")
        result = self.inspect()
        self.assertFalse(result["success"])
        self.assertFalse(result["phases"]["cold"]["success"])
        self.assertTrue(result["phases"]["warm"]["success"])

    def test_wrong_or_stale_marker_fails(self):
        for value in ("DONE stale digest", "DONE " + pair.phase_marker("fresh digest", "cold")):
            (self.root / "warm/done.marker").write_text(value)
            self.assertFalse(self.inspect()["success"])

    def test_warm_rebuild_or_missing_reuse_fails(self):
        for summary in ("incremental: reused=0x00000003 rebuilt=0x00000001",
                        "incremental: reused=0x00000002 rebuilt=0x00000000",
                        "incremental: cache unavailable; assembling fresh"):
            (self.root / "warm/assembly.stdout").write_text(summary)
            self.assertFalse(self.inspect()["success"])

    def test_copy_failure_fails(self):
        (self.root / "cold/copy-exitcode").write_text("20")
        self.assertFalse(self.inspect()["success"])

    def test_counters_are_hex_and_unique(self):
        text = "incremental: reused=0x0000000a rebuilt=0x000000FF\n"
        self.assertEqual(pair.incremental_counters(text), {"reused": 10, "rebuilt": 255})
        for invalid in ("", text + text, "incremental: reused=10 rebuilt=20"):
            with self.assertRaises(ValueError):
                pair.incremental_counters(invalid)

    def test_script_keeps_cache_and_bootstrap_in_one_directory(self):
        script = pair.guest_script("Development:fresh", "uuid digest", "opforge -i src/main.asm --capture-dir scratch", capture_mode="disk")
        self.assertEqual(script.count("CD Development:fresh"), 1)
        self.assertEqual(script.count(" --rebuild"), 1)
        self.assertIn("MakeDir scratch", script)
        self.assertNotIn("Delete build/ofm", script)
        self.assertNotIn("Delete opforge", script)
        self.assertEqual(script.count("Delete build/opforge_compact"), 2)
        for phase in pair.PHASES:
            self.assertIn(f"FailAt 10\nIf EXISTS {phase}", script)
            self.assertIn(f"Delete {phase} ALL", script)
            self.assertIn(f"Copy build/opforge_compact {phase}/output.hunk\nEcho $RC >{phase}/copy-exitcode", script)
            self.assertIn(f"Echo $RC >{phase}/exitcode\nC:Date >{phase}/end.time", script)
            self.assertIn(pair.phase_marker("uuid digest", phase), script)

    def test_dry_run_uses_transport_without_network(self):
        manifest = {"bootstrap_defines": [], "bootstrap_package_storage": "embedded",
                    "output_package_storage": "embedded", "native_output_file": "build/opforge_compact",
                    "command": "opforge -i src/main.asm", "over_classic_limit_components": []}
        with mock.patch.object(pair.base, "load_bundle", return_value=(manifest, {}, b"oracle", "digest")), \
             mock.patch.object(pair.base.subprocess, "run", side_effect=AssertionError("network")), \
             mock.patch.object(pair.base.tempfile, "mkdtemp", return_value=str(self.root / "prepared")):
            (self.root / "prepared").mkdir()
            self.assertEqual(pair.main(["--bundle", str(self.root), "--dry-run"]), 0)
        script = next((self.root / "prepared").glob("*/run-selfhost")).read_text()
        self.assertIn(pair.PAIR_PROTOCOL, script)

    def test_rejects_instrumented_bundle(self):
        with mock.patch.object(pair.base, "load_bundle", return_value=({"bootstrap_defines": ["debug"]}, {}, b"", "digest")):
            with self.assertRaises(ValueError):
                pair.load_bundle(self.root)


if __name__ == "__main__":
    unittest.main()

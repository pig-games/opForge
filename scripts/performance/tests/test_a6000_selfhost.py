import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
import struct
import sys

sys.path.insert(0, str(Path(__file__).parents[1]))

SPEC = importlib.util.spec_from_file_location(
    "a6000_selfhost", Path(__file__).parents[1] / "run_a6000_selfhost.py"
)
runner = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(runner)


class HardwareCompletionTests(unittest.TestCase):
    def bundle(self, root, instrumented):
        source = root / "original"
        source.mkdir()
        (source / "entry.asm").write_bytes(b"source")
        (root / "src").mkdir()
        (root / "src/entry.asm").write_bytes(b"source")
        oracle = b"release"
        bootstrap = b"profile" if instrumented else oracle
        package = b"BS12package"
        command = "opforge p.bin src/entry.asm output.hunk"
        manifest = {
            "release_defines": [],
            "filename_mapping": "identity; source include literals are unchanged",
            "classic_filename_compatible": True,
            "source_root": str(source),
            "source_manifest_digest": runner.fnv(b"entry.asm\0source\0"),
            "source_mapping": [{"staged_path": "src/entry.asm", "logical_path": "entry.asm",
                                "bytes": 6, "digest": runner.fnv(b"source")}],
            "release_hunk_digest": runner.fnv(oracle),
            "bootstrap_hunk_digest": runner.fnv(bootstrap),
            "bootstrap_defines": sorted(runner.INSTRUMENTATION_DEFINES) if instrumented else [],
            "telemetry_file": "memory.bin" if instrumented else None,
            "runtime_package_digest": runner.fnv(package), "command": command,
        }
        for name, data in {"opforge": bootstrap, "oracle.hunk": oracle, "p.bin": package,
                           "command.txt": command.encode(),
                           "manifest.json": json.dumps(manifest).encode()}.items():
            (root / name).write_bytes(data)

    def test_instrumented_bootstrap_is_distinct_from_release_oracle_and_case(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.bundle(root, False)
            release_case = runner.load_bundle(root)[3]
            manifest = json.loads((root / "manifest.json").read_text())
            manifest.update({"bootstrap_defines": sorted(runner.INSTRUMENTATION_DEFINES),
                             "bootstrap_hunk_digest": runner.fnv(b"profile"),
                             "telemetry_file": "memory.bin"})
            (root / "manifest.json").write_text(json.dumps(manifest))
            (root / "opforge").write_bytes(b"profile")
            _, files, oracle, case = runner.load_bundle(root)
            self.assertEqual(oracle, b"release")
            self.assertEqual(files["opforge"], b"profile")
            self.assertNotIn("oracle.hunk", files)
            self.assertNotEqual(case, release_case)
            (root / "opforge").write_bytes(b"corrupt")
            with self.assertRaisesRegex(ValueError, "Bootstrap digest"):
                runner.load_bundle(root)

    def test_release_cannot_use_different_bootstrap_even_with_valid_digest(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.bundle(root, True)
            manifest = json.loads((root / "manifest.json").read_text())
            manifest.update({"bootstrap_defines": [], "telemetry_file": None})
            (root / "manifest.json").write_text(json.dumps(manifest))
            with self.assertRaisesRegex(ValueError, "Release bootstrap/oracle"):
                runner.load_bundle(root)

    def test_preexisting_output_or_telemetry_stops_before_execution(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            runner.verify_fresh_directory(root)
            for name in ("output.hunk", "memory.bin", "done.marker"):
                (root / name).write_bytes(b"old")
                with self.assertRaisesRegex(ValueError, "old result"):
                    runner.verify_fresh_directory(root)
                (root / name).unlink()

    def test_telemetry_required_but_cannot_replace_successful_assembly(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            manifest = {"bootstrap_defines": sorted(runner.INSTRUMENTATION_DEFINES)}
            result = {"success": True}
            runner.add_telemetry(result, root, manifest)
            self.assertFalse(result["success"])
            self.assertTrue(result["assembly_success"])
            words = [0] * 570
            words[0], words[28] = 0x4D454D44, 1000
            words[19:28] = [0, 1, 0, 0, 1, 50, 0, 1, 100]
            words[31] = 1000
            for native_success, errors, accepted in [(True, 0, True), (False, 0, False), (True, 32, False)]:
                words[29] = errors
                (root / "memory.bin").write_bytes(struct.pack(">570I", *words))
                result = {"success": native_success}
                runner.add_telemetry(result, root, manifest)
                self.assertEqual(result["success"], accepted)
                self.assertEqual(result["assembly_success"], native_success)
            words[29], words[31] = 0, 1100
            (root / "memory.bin").write_bytes(struct.pack(">570I", *words))
            result = {"success": True}
            runner.add_telemetry(result, root, manifest)
            self.assertFalse(result["telemetry_completed"])

    def test_incompatible_filename_bundle_stops_before_transfer(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "manifest.json").write_text(json.dumps({
                "release_defines": [],
                "filename_mapping": "identity; source include literals are unchanged",
                "classic_filename_compatible": False,
            }))
            with self.assertRaisesRegex(ValueError, "filenames over 30 bytes"):
                runner.load_bundle(root)

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

    def test_invocation_uses_classic_shell_redirection_without_extra_arguments(self):
        command = "opforge p.bin src/entry.asm output.hunk -M src -I src/debug"
        script = runner.guest_script("Development:opforge-fresh", "fresh case", command)
        self.assertIn("opforge >assembly.stdout p.bin src/entry.asm output.hunk -M src -I src/debug\n", script)
        self.assertNotIn("*>", script)
        self.assertIn("If EXISTS C:CPU\nC:CPU >>environment.txt\nEndIf\n", script)


if __name__ == "__main__":
    unittest.main()

import argparse
import contextlib
import io
import json
from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from measure_compact_control import completed_test, image_argument, parse_args


def memory_record():
    return {
        "eclock_hz": 709379,
        "live_owned_bytes": 0,
        "packed_source_bytes": 27022,
        "peak_owned_bytes": 1282376,
        "preparation_stages": [{"calls": 3617, "name": "source_io_and_other", "seconds": 5.700363275484614}],
        "runtime_prefix_bytes": 425506,
        "source_bytes_read": 11592,
        "total_allocated_bytes": 3182152,
        "total_freed_bytes": 3182152,
    }


def passing_output(memory=None):
    record = f"ITERABLE_MEMORY {memory}\n" if memory is not None else ""
    return ("running 1 test\n"
            "test fs_uae_smoke::native_active_includes::native_active_include_control_fs_uae ... "
            "kill: 26445: No such process\n"
            f"{record}"
            "ITERABLE_CASE name=active-include-control cpu=68020 failure=false source_bytes=11592 "
            "seconds=Some(33.496751) image_bytes=678952\n"
            "ok\n\n"
            "test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 2941 filtered out\n")


class MeasureCompactControlTests(unittest.TestCase):
    def test_label_path_parser_rejects_unsafe_or_incomplete_values(self):
        self.assertEqual(image_argument("trial-1=/tmp/compact.hunk"),
                         ("trial-1", Path("/tmp/compact.hunk")))
        for value in ("../escape=/tmp/x", "bad/name=/tmp/x", "label=", "=path", "white space=/tmp/x"):
            with self.subTest(value=value), self.assertRaises(argparse.ArgumentTypeError):
                image_argument(value)

    def test_arguments_require_images_and_distinct_safe_labels(self):
        with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
            parse_args(["--test-binary", "test", "--report", "report.json"])
        args = parse_args(["--test-binary", "test", "--report", "report.json",
                           "--release-image", "base=/tmp/base", "--memory-image", "profile=/tmp/profile"])
        self.assertEqual([(label, instrumented) for label, _, instrumented in args.images],
                         [("base", False), ("profile", True)])
        with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
            parse_args(["--test-binary", "test", "--report", "report.json",
                        "--release-image", "same=/tmp/base", "--memory-image", "same=/tmp/profile"])

    def test_completed_test_requires_single_success_and_timing(self):
        output = passing_output()
        self.assertEqual(completed_test(output, 0), (33.496751, None, "68020", 11592))
        inline = output.replace("kill: 26445: No such process\nITERABLE_CASE", "ITERABLE_CASE")
        self.assertEqual(completed_test(inline, 0), (33.496751, None, "68020", 11592))
        with self.assertRaises(ValueError):
            completed_test(output.replace("failure=false", "failure=true"), 0)
        for changed in (output.replace("running 1 test", "running 2 tests"),
                        output.replace("1 passed; 0 failed", "0 passed; 1 failed"),
                        output.replace("ITERABLE_CASE", "ITERABLE_CASE_MISSING"),
                        output.replace("Some(33.496751)", "Some(nan)"),
                        output.replace("native_active_include_control_fs_uae", "another_test")):
            with self.subTest(output=changed), self.assertRaises(ValueError):
                completed_test(changed, 0)

    def test_instrumented_case_requires_one_valid_decoded_memory_record(self):
        record = memory_record()
        output = passing_output(json.dumps(record))
        self.assertEqual(completed_test(output, 0, require_memory=True)[1], record)
        duplicate = output.replace("ITERABLE_CASE", f"ITERABLE_MEMORY {json.dumps(record)}\nITERABLE_CASE")
        malformed = passing_output("{not-json}")
        missing = passing_output()
        for changed in (duplicate, malformed, missing):
            with self.subTest(output=changed), self.assertRaises(ValueError):
                completed_test(changed, 0, require_memory=True)


if __name__ == "__main__":
    unittest.main()

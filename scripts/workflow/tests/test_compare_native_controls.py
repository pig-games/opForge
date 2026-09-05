"""Level A/B synthetic control validation; no emulator or parity evidence."""
import copy
import hashlib
from pathlib import Path
import struct
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "performance"))
import compare_native_controls as controls
from test_decode_native_progress import (decoder, record, work_record,
                                         symbol_expression_record, runtime_record, platform_record)


class NativeControlComparisonTests(unittest.TestCase):
    def setUp(self):
        self.case = {"id": "B03", "sha256": "a" * 64, "files": {"main.asm": {"bytes": 1, "sha256": "b" * 64}}}
        self.frozen = {"cases": [self.case], "sha256": "c" * 64, "package": {"sha256": "d" * 64}}
        self.rows = [self.make_row(mode, n) for n, mode in enumerate(["all", "all-no-io", "all-no-io", "all"])]

    def make_row(self, mode, n):
        progress = bytearray(record(decoder.FLAG_INCOMPLETE | decoder.FLAG_ABORT_REQUESTED, visits=1, exit_status=1))
        struct.pack_into(">I", progress, 116, 1)
        platform = bytearray(platform_record(decoder.PLATFORM_FLAG_INCOMPLETE, exit_status=1))
        if mode == "all-no-io":
            flags = struct.unpack_from(">H", platform, 6)[0]
            struct.pack_into(">H", platform, 6, flags & ~decoder.PLATFORM_FLAG_IO_ENABLED)
            platform[20:140] = bytes(120)
            platform[168:184] = bytes(16)
        raw = {key: list(value) for key, value in {
            "ofpr": progress, "ofwk": work_record(decoder.WORK_FLAG_INCOMPLETE, exit_status=1),
            "ofse": symbol_expression_record(decoder.SYMBOL_EXPR_FLAG_INCOMPLETE | decoder.SYMBOL_EXPR_FLAG_DETAIL, exit_status=1),
            "ofvm": runtime_record(decoder.RUNTIME_FLAG_INCOMPLETE, exit_status=1), "ofio": platform}.items()}
        start = f"OPFORGE-FS-UAE-PROOF-V1 START fresh-{n} fingerprint"
        done = start.replace(" START ", " DONE ")
        config = f"cpu = 68020\nhard_drive_1 = /ephemeral/run-{n}\n"
        capture = {"proof_level": "E", "complete": False, "parity_passed": False, "protocol_completed": True,
                   "exit_status": 1, "id": self.case["id"], "case_sha256": self.case["sha256"],
                   "corpus_sha256": self.frozen["sha256"], "package_sha256": self.frozen["package"]["sha256"],
                   "profile": controls.decode_records(raw), "raw_records": raw, "command_template": "main.asm",
                   "instrumentation_defines": ["OPFORGE_DEBUG_CONTRACTS", "OPFORGE_PROGRESS_ABORT_VISITS=1"] +
                   (["OPFORGE_PROGRESS_PLATFORM_NO_IO"] if mode == "all-no-io" else []),
                   "guest_protocol": {"started": start + "\n", "done": done + "\n", "exitcode": "1\n"}}
        sample = {"source_inventory_matches_frozen": True, "source_inventory": self.case["files"],
                  "control_mode": "console", "profile_mode": mode, "cleanup": "complete", "commands": [],
                  "sample_observed": False, "guest_start_observed": True, "expected_start": start,
                  "config": config, "config_sha256": hashlib.sha256(config.encode()).hexdigest(),
                  "sampler_sha256": "e" * 64, "native_binary_sha256": mode, "native_binary_bytes": 10,
                  "labels_sha256": mode, "fs_uae_version": "synthetic", "control_completion": {"observed_before_cleanup": True,
                  "guest_exit_observed": 1, "expected_done_observed": done, "start_to_done_host_seconds": [10, 8, 9, 11][n]}}
        return {"capture": capture, "live_sample": sample, "case_id": self.case["id"], "case_sha256": self.case["sha256"],
                "corpus_sha256": self.frozen["sha256"], "abort_visits": 1, "capture_ok": True, "test_exit": 0,
                "comparison_eligible": False, "complete": False, "parity_passed": False, "control_mode": "console",
                "profile_mode": mode, "generator_sha256": "f" * 64, "command": ["diagnose"]}

    def compare(self):
        return controls.compare_controls(self.rows, self.frozen)

    def test_same_work_abba_summary_is_diagnostic_only(self):
        result = self.compare()
        self.assertEqual(result["all_minus_noio_mean_seconds"], 2)
        self.assertEqual(result["modes"]["all"]["seconds"], [10, 11])
        self.assertFalse(result["complete"])
        self.assertFalse(result["parity_passed"])

    def test_outer_identity_must_match_frozen_not_just_other_runs(self):
        for key in ("case_sha256", "corpus_sha256"):
            with self.subTest(key=key):
                saved = copy.deepcopy(self.rows)
                for row in self.rows:
                    row[key] = "0" * 64
                with self.assertRaises(ValueError):
                    self.compare()
                self.rows = saved

    def test_start_requires_protocol_grammar_even_when_all_fields_agree(self):
        row = self.rows[0]
        row["capture"]["guest_protocol"].update(started="garbage", done="garbage")
        row["live_sample"]["expected_start"] = "garbage"
        row["live_sample"]["control_completion"]["expected_done_observed"] = "garbage"
        with self.assertRaisesRegex(ValueError, "protocol"):
            self.compare()

    def test_duplicate_challenge_rejected(self):
        self.rows[3]["capture"]["guest_protocol"] = copy.deepcopy(self.rows[0]["capture"]["guest_protocol"])
        self.rows[3]["live_sample"]["expected_start"] = self.rows[0]["live_sample"]["expected_start"]
        self.rows[3]["live_sample"]["control_completion"]["expected_done_observed"] = self.rows[0]["live_sample"]["control_completion"]["expected_done_observed"]
        with self.assertRaisesRegex(ValueError, "duplicate run"):
            self.compare()

    def test_host_code_and_defines_are_pinned(self):
        for section, key, value in ((None, "generator_sha256", "changed"), ("live_sample", "sampler_sha256", "changed"),
                                    ("capture", "command_template", "changed"), ("live_sample", "fs_uae_version", "changed"),
                                    ("capture", "instrumentation_defines", ["different"] )):
            with self.subTest(key=key):
                saved = copy.deepcopy(self.rows)
                target = self.rows[2][section] if section else self.rows[2]
                target[key] = value
                with self.assertRaises(ValueError):
                    self.compare()
                self.rows = saved

    def test_rejects_changed_shared_work_even_with_matching_stored_profile(self):
        raw = self.rows[2]["capture"]["raw_records"]
        data = bytearray(raw["ofvm"])
        struct.pack_into(">I", data, 24, 999)
        raw["ofvm"] = list(data)
        self.rows[2]["capture"]["profile"] = controls.decode_records(raw)
        with self.assertRaisesRegex(ValueError, "shared work"):
            self.compare()

    def test_rejects_stored_profile_tampering(self):
        self.rows[0]["capture"]["profile"]["current_statement"] += 1
        with self.assertRaisesRegex(ValueError, "independently decoded"):
            self.compare()

    def test_rejects_invalid_timing_protocol_inventory_and_mode(self):
        sample = self.rows[1]["live_sample"]
        mutations = [(sample["control_completion"], "start_to_done_host_seconds", value) for value in (0, 115, float("nan"), True)]
        mutations += [(sample["control_completion"], "guest_exit_observed", value) for value in (0, True, 2 ** 32)]
        mutations += [(sample, "source_inventory_matches_frozen", False), (sample, "commands", ["g"]),
                      (sample, "config_sha256", "changed"), (sample, "profile_mode", "all")]
        for target, key, value in mutations:
            with self.subTest(key=key, value=value):
                old = target[key]
                target[key] = value
                with self.assertRaises(ValueError):
                    self.compare()
                target[key] = old

    def test_native_identity_and_normalized_configuration_must_match(self):
        self.rows[3]["live_sample"]["native_binary_bytes"] += 1
        with self.assertRaisesRegex(ValueError, "native binary"):
            self.compare()
        self.rows[3]["live_sample"]["native_binary_bytes"] -= 1
        sample = self.rows[3]["live_sample"]
        sample["config"] += "cpu_speed = different\n"
        sample["config_sha256"] = hashlib.sha256(sample["config"].encode()).hexdigest()
        with self.assertRaisesRegex(ValueError, "configuration"):
            self.compare()


if __name__ == "__main__":
    unittest.main()

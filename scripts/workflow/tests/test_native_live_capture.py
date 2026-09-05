"""Level A launcher safety checks only; no guest or native parity proof."""
import importlib.util
from pathlib import Path
import tempfile
import unittest
from unittest import mock
from scripts.workflow.tests import test_decode_native_progress as fixtures

SCRIPT = Path(__file__).resolve().parents[2] / "performance/capture_native_live.py"
SPEC = importlib.util.spec_from_file_location("capture_native_live", SCRIPT)
live = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(live)


class NativeLiveCaptureTests(unittest.TestCase):
    def test_debugger_key_requires_verified_target_foreground(self):
        with mock.patch.object(live.subprocess, "run", return_value=mock.Mock(stdout="123:123\n")) as run:
            receipt = live.enter_debugger(123)
            self.assertEqual(receipt["foreground_before_pid"], 123)
            self.assertTrue(receipt["key_request_returned"])
            self.assertFalse(receipt["prompt_acknowledged"])
            argv = run.call_args.args[0]
            self.assertLess(argv.index('if beforePid is not 123 then error "debugger target is not foreground; key not sent"'),
                            argv.index("key code 2 using command down"))
            self.assertEqual(argv.count("key code 2 using command down"), 1)
            self.assertTrue(run.call_args.kwargs["check"])
            self.assertEqual(run.call_args.kwargs["timeout"], 5)
        with mock.patch.object(live.subprocess, "run", return_value=mock.Mock(stdout="123:456")):
            self.assertEqual(live.enter_debugger(123)["foreground_after_pid"], 456)
        for invalid in ("", "456:123", "123:123 extra", "123:0"):
            with self.subTest(invalid=invalid), mock.patch.object(live.subprocess, "run", return_value=mock.Mock(stdout=invalid)):
                with self.assertRaises(ValueError):
                    live.enter_debugger(123)
        with mock.patch.object(live.subprocess, "run") as run:
            for invalid in (0, -1, True, "123"):
                with self.assertRaises(ValueError):
                    live.enter_debugger(invalid)
            run.assert_not_called()
        with mock.patch.object(live.subprocess, "run", side_effect=live.subprocess.CalledProcessError(1, "osascript")):
            with self.assertRaises(live.subprocess.CalledProcessError):
                live.enter_debugger(123)

    def test_debugger_ack_requires_complete_pause_local_frame_and_prompt(self):
        frame = "WARNING: Activated debugger\n"
        for kind in ("D", "A"):
            for start in (0, 4):
                frame += " ".join(f"{kind}{i} 00000000" for i in range(start, start + 4)) + "\n"
        frame += "T=0 S=0 M=0 X=0 N=0 Z=0 V=0 C=0 IMASK=0\nNext PC: 00123456\n>"
        entry = {"prompt_acknowledged": False}
        for partial in ("", ">g\n", "WARNING: Activated debugger\nNext PC: 00123456\n>", frame[:-1]):
            self.assertFalse(live.acknowledge_debugger_entry(entry, partial, 61))
            self.assertFalse(entry["prompt_acknowledged"])
        self.assertTrue(live.acknowledge_debugger_entry(entry, frame, 61))
        self.assertEqual(entry["prompt_after_start_seconds"], 61)
        # A complete frame received before the first key must not combine
        # with a new bare prompt to acknowledge that request.
        raw = bytearray(frame.encode())
        with mock.patch.object(live, "enter_debugger", return_value={"key_request_returned": True}) as enter:
            offset, receipt = live.begin_debugger_pause(123, raw)
        enter.assert_called_once_with(123)
        raw.extend(b"\n>")
        first = {**receipt, "prompt_acknowledged": False}
        self.assertFalse(live.acknowledge_debugger_entry(
            first, live.normalized_transcript(bytes(raw[offset:])), 61))
        raw.extend(frame.encode())
        self.assertTrue(live.acknowledge_debugger_entry(
            first, live.normalized_transcript(bytes(raw[offset:])), 62))
        later = {"prompt_acknowledged": False}
        self.assertFalse(live.acknowledge_debugger_entry(later, ">g\n", 101))
        self.assertFalse(later["prompt_acknowledged"])

    def test_missing_ack_has_specific_failure_without_changing_other_outcomes(self):
        pending = {"key_request_returned": True, "prompt_acknowledged": False}
        report = {"stop_reason": "deadline", "debugger_entries": [pending]}
        live.finalize_debugger_entries(report)
        self.assertEqual(report["stop_reason"], "debugger-prompt-timeout")
        for reason in ("capture-error", "guest-completed-before-sample", "bounded-sample-complete"):
            report = {"stop_reason": reason, "debugger_entries": [pending]}
            live.finalize_debugger_entries(report)
            self.assertEqual(report["stop_reason"], reason)
        for entries in ([], [{"key_request_returned": False}], [{"key_request_returned": True, "prompt_acknowledged": True}]):
            report = {"stop_reason": "deadline", "debugger_entries": entries}
            live.finalize_debugger_entries(report)
            self.assertEqual(report["stop_reason"], "deadline")

    def test_guest_inventory_rejects_extra_discoverable_alias_or_changed_bytes(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            main = work / "main.asm"
            main.write_bytes(b".byte 1\n")
            expected = {"main.asm": {"bytes": 8, "sha256": live.hashlib.sha256(main.read_bytes()).hexdigest()}}
            self.assertEqual(live.verified_source_inventory(work, expected), expected)
            alias = work / "generic.asm"
            alias.write_bytes(main.read_bytes())
            with self.assertRaisesRegex(ValueError, "differs"):
                live.verified_source_inventory(work, expected)
            alias.unlink()
            main.write_bytes(b".byte 2\n")
            with self.assertRaisesRegex(ValueError, "differs"):
                live.verified_source_inventory(work, expected)

    def test_later_failed_stop_cannot_alias_early_counters_or_pc_binding(self):
        initial = {"pc": "0x1000", "frame_after_start_seconds": 61,
                   "live_records": {"ofpr": [1]}, "record_locations": {"ofpr": (4096, 128)}}
        report = {"resample_after_seconds": 100, "snapshots": [dict(initial)], "cleanup": "complete",
                  "pc": "0x00F80000", "frame_after_start_seconds": 101,
                  "code_binding": {"runtime_base": 4096},
                  "binding_anchor": {"sampled_pc_in_bound_code": True}, "record_error": "rejected"}
        live.finalize_snapshots(report)
        for key in ("live_records", "record_locations", "binding_anchor", "code_binding"):
            self.assertNotIn(key, report)
        self.assertFalse(report["resample_observed"])
        self.assertEqual(report["pc"], "0x00F80000")
        self.assertEqual(report["snapshots"][0]["pc"], "0x1000")
        self.assertTrue(report["snapshots"][0]["binding_anchor"]["sampled_pc_in_bound_code"])
        report["snapshots"].append({"pc": "0x00F80000", "mapping_snapshot_index": 0})
        live.finalize_snapshots(report)
        self.assertTrue(report["resample_observed"])
        self.assertNotIn("live_records", report)

    def test_resample_window_is_bounded_and_requires_a_sample(self):
        live.validate_resample(60, 100)
        live.validate_resample(100, None)
        for first, second, mode in ((60, 60, "sample"), (60, 64, "sample"), (60, 101, "sample"),
                                    (0, 100, "sample"), (60, True, "sample"), (60, 100, "app")):
            with self.assertRaises(ValueError):
                live.validate_resample(first, second, mode)

    def test_each_pause_requires_all_fresh_correlated_active_records(self):
        records = {"ofpr": fixtures.record(1), "ofwk": fixtures.work_record(1),
                   "ofse": fixtures.symbol_expression_record(9), "ofvm": fixtures.runtime_record(1),
                   "ofio": fixtures.platform_record(1)}
        locations = {name: (0x1000 + i * 0x1000, len(data)) for i, (name, data) in enumerate(records.items())}

        def dump(payloads):
            lines = []
            for name, data in payloads.items():
                base, _ = locations[name]
                for offset in range(0, len(data), 16):
                    words = " ".join(data[i:i + 2].hex() for i in range(offset, offset + 16, 2))
                    lines.append(f"{base + offset:08x} {words}  text")
            return "\n".join(lines)

        first = live.snapshot_records(dump(records), locations)
        self.assertEqual(first["profile"]["run_id"], 0x1234)
        self.assertEqual(live.snapshot_records(dump(records), locations, expected_io=True), first)
        with self.assertRaisesRegex(ValueError, "requested diagnostic mode"):
            live.snapshot_records(dump(records), locations, expected_io=False)
        disabled = bytearray(records["ofio"])
        disabled[6:8] = (1 | fixtures.decoder.PLATFORM_FLAG_BULK_ENABLED).to_bytes(2, "big")
        for start, end in ((20, 140), (168, 184)):
            disabled[start:end] = bytes(end - start)
        disabled_records = {**records, "ofio": bytes(disabled)}
        result = live.snapshot_records(dump(disabled_records), locations, expected_io=False)
        self.assertEqual(result["profile"]["platform_io"]["enabled_groups"], {"io": False, "bulk": True})
        with self.assertRaises(ValueError):
            live.snapshot_records(dump(disabled_records), locations, expected_io=True)
        self.assertEqual(live.snapshot_records(dump(records), locations, 0x1234), first)
        with self.assertRaisesRegex(ValueError, "identity changed"):
            live.snapshot_records(dump(records), locations, 0x5678)
        with self.assertRaisesRegex(ValueError, "memory unavailable"):
            live.snapshot_records(dump({"ofpr": records["ofpr"]}), locations, 0x1234)
        for name, data in (("ofpr", fixtures.record(2)),
                           ("ofpr", fixtures.record(1, overflow_bits=1)),
                           ("ofwk", fixtures.work_record(1, run_id=0x5678)),
                           ("ofvm", fixtures.runtime_record(1, phase=2)),
                           ("ofse", fixtures.symbol_expression_record(9, overflow_bits=1))):
            with self.subTest(name=name), self.assertRaises(ValueError):
                live.snapshot_records(dump({**records, name: data}), locations, 0x1234)

    def test_control_requires_fresh_start_done_and_explicit_exit_before_timing(self):
        with tempfile.TemporaryDirectory() as directory:
            done, status = Path(directory) / "done", Path(directory) / "exit"
            expected = "OPFORGE-FS-UAE-PROOF-V1 DONE fresh case"
            self.assertIsNone(live.control_completion(10.0, done, expected, status, 20.0))
            done.write_text(expected)
            self.assertIsNone(live.control_completion(10.0, done, expected, status, 20.0))
            for bad in ("", "success", "1\n2", "1" * 100, "1" + " " * 63 + "junk",
                        "2147483648", "-2147483649", "\N{LATIN SMALL LETTER E WITH ACUTE}"):
                status.write_text(bad)
                self.assertIsNone(live.control_completion(10.0, done, expected, status, 20.0))
            status.write_text("1\n")
            self.assertIsNone(live.control_completion(None, done, expected, status, 20.0))
            result = live.control_completion(10.0, done, expected, status, 20.0)
            self.assertEqual(result["guest_exit_observed"], 1)
            self.assertEqual(result["start_to_done_host_seconds"], 10.0)
            self.assertTrue(result["observed_before_cleanup"])
            with mock.patch.object(Path, "open", side_effect=PermissionError("unreadable")):
                self.assertIsNone(live.control_completion(10.0, done, expected, status, 20.0))
            done.write_text(expected.replace("fresh", "stale"))
            self.assertIsNone(live.control_completion(10.0, done, expected, status, 20.0))

    def test_hunk_and_unique_code_binding_mask_only_declared_relocations(self):
        words = [0x3f3, 0, 1, 0, 0, 16, 0x3e9, 16]
        payload = bytes(range(64))
        binary = b"".join(value.to_bytes(4, "big") for value in words) + payload
        binary += b"".join(value.to_bytes(4, "big") for value in (0x3ec, 1, 0, 8, 0, 0x3f2))
        segments = live.hunk_segments(binary)
        odd_relocation = binary[:-12] + (9).to_bytes(4, "big") + binary[-8:]
        self.assertEqual(live.hunk_segments(odd_relocation)[0]["relocations"], {9: 0})
        runtime = payload[:8] + b"\xaa\xbb\xcc\xdd" + payload[12:]
        transcript = "\n".join(f"{0x1000 + i:08x} {runtime[i:i+2].hex()}                     opcode" for i in range(0, 64, 2))
        binding = live.bind_code(transcript, segments, 0x1000)
        self.assertEqual(binding["runtime_base"], 0x1000)
        self.assertEqual(binding["unrelocated_bytes_checked"], 60)
        with self.assertRaises(ValueError):
            live.bind_code(transcript, [*segments, *segments], 0x1000)
        with self.assertRaises(ValueError):
            live.bind_code(transcript.replace("0001", "ffff", 1), segments, 0x1000)
        for malformed in (binary[:-1], binary + b"extra", b"", binary[:30]):
            with self.assertRaises(ValueError):
                live.hunk_segments(malformed)

    def test_memory_parser_does_not_confuse_disassembly_with_dump(self):
        transcript = ">00001000 0001 0203 0405 0607 0809 0A0B 0C0D 0E0F  text\n00002000 4e75                     RTS\n"
        self.assertEqual(live.memory_bytes(transcript), {0x1000 + i: i for i in range(16)})

    def test_missing_stale_partial_and_wrong_challenge_never_start_capture(self):
        with tempfile.TemporaryDirectory() as directory:
            marker = Path(directory) / "started"
            expected = "OPFORGE-FS-UAE-PROOF-V1 START fresh case"
            self.assertFalse(live.fresh_start(marker, expected))
            for text in ("", "OPFORGE-FS-UAE-PROOF-V1 START stale case", expected[:-1],
                         expected + " extra", expected + "\n" + "x" * 1024, expected + " " * 1024 + "extra"):
                marker.write_text(text)
                self.assertFalse(live.fresh_start(marker, expected))
            marker.write_text(expected + "\n")
            self.assertTrue(live.fresh_start(marker, expected))

    def test_console_config_preserves_mounts_and_pinned_processor(self):
        source = "[fs-uae]\ncpu=68020\nuae_cpu_speed=max\njit_compiler=0\nhard_drive_1=/tmp/Work\n"
        for override in ("", "console_debugger=0\n", "Console-Debugger=0\nconsole_debugger=1\n"):
            self.assertEqual(live.console_config(source + override), source + "console_debugger = 1\n")
            self.assertEqual(live.console_config(source + override, False), source + "console_debugger = 0\n")
        for source in ("", "cpu=68020", "[fs-uae]\n[fs-uae]\n"):
            with self.assertRaises(ValueError):
                live.console_config(source)


if __name__ == "__main__":
    unittest.main()

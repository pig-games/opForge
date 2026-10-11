import importlib.util
from pathlib import Path
import unittest


SPEC = importlib.util.spec_from_file_location(
    "decode_binding_diagnostic", Path(__file__).parents[1] / "decode_binding_diagnostic.py"
)
decoder = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(decoder)


def progress(phase, first=0, second=0, third=0, fourth=10):
    return f"progress p={phase:08x} f={first:08x} l={second:08x} r={third:08x} m={fourth:08x}"


def preparation_failure(stage=4, *, used=7, name_count=2, live_owned=99, raw=None):
    lines = [progress(301, stage, used, name_count).replace("m=0000000a", f"m={live_owned:08x}")]
    if raw is not None:
        raw = raw.ljust(24, b"\0")[:24]
        words = [int.from_bytes(raw[i:i + 4], "big") for i in range(0, 24, 4)]
        lines.append(progress(302, *words[:3]))
        lines.append(progress(303, *words[3:]))
    return "\n".join(lines)


def pre_call_input(input_pointer=0x1000, prepared_pointer=0x1000, raw=None, live_owned=99):
    raw = (raw or bytes([7]) + bytes(range(1, 32))).ljust(32, b"\0")[:32]
    words = [int.from_bytes(raw[i:i + 4], "big") for i in range(0, 32, 4)]
    return "\n".join([
        progress(304, input_pointer, prepared_pointer, words[0], live_owned),
        progress(305, *words[1:4], fourth=live_owned),
        progress(306, *words[4:7], fourth=live_owned),
        progress(307, words[7], 0, 0, live_owned),
    ])


def writer_status(status, reserved_first=0, reserved_second=0, live_owned=99):
    return progress(308, status, reserved_first, reserved_second, live_owned)


def snapshot(name=b"Widget", *, index=1, related=0xFFFFFFFF, stage=5, count=3):
    entry = bytearray(28)
    entry[0:4] = bytes.fromhex("00001000")
    entry[4:6] = len(name).to_bytes(2, "big")
    entry[6:8] = (2).to_bytes(2, "big")
    entry[8:10] = (1).to_bytes(2, "big")
    entry[10:12] = (0x20).to_bytes(2, "big")
    entry[12:14] = (0xFFFF).to_bytes(2, "big")
    entry[14:16] = (0xFFFF).to_bytes(2, "big")
    entry[16:18] = (2).to_bytes(2, "big")
    entry[20:22] = (4).to_bytes(2, "big")
    entry[22:24] = (3).to_bytes(2, "big")
    entry[24:26] = (1).to_bytes(2, "big")
    lines = [
        progress(32, stage, index, related),
        progress(33, (0x1234 << 16) | count, 2, len(name)),
    ]
    for phase in (34, 35):
        chunk = entry[(phase - 34) * 12 : (phase - 33) * 12]
        lines.append(progress(phase, *[int.from_bytes(chunk[i:i + 4], "big") for i in (0, 4, 8)]))
    lines.append(progress(36, int.from_bytes(entry[24:28], "big")))
    padded_name = name.ljust(256, b"\0")
    for i, phase in enumerate(range(64, 85)):
        chunk = padded_name[i * 12 : (i + 1) * 12]
        lines.append(progress(phase, *[int.from_bytes(chunk[j:j + 4], "big") for j in (0, 4, 8)]))
    lines.append(progress(85, int.from_bytes(padded_name[252:256], "big")))
    return "\n".join(lines)


def correlation(kind, **kwargs):
    metadata, spelling = decoder.SNAPSHOT_PHASES[kind]
    lines = []
    for line in snapshot(**kwargs).splitlines():
        match = decoder.PROGRESS.fullmatch(line)
        phase, first, second, third, _ = (int(part, 16) for part in match.groups())
        phase += metadata - 32 if phase < 64 else spelling - 64
        lines.append(progress(phase, first, second, third))
    return "\n".join(lines)


class BindingDiagnosticTests(unittest.TestCase):
    def test_optional_correlated_views_keep_original_snapshot(self):
        text = snapshot(stage=16)
        for kind in ("owner", "previous", "next"):
            text += "\n" + correlation(kind, stage=16, index=0, related=1)
        result = decoder.decode_binding_diagnostic(text)
        self.assertEqual(result["snapshot"]["entry_index"], 1)
        self.assertEqual(set(result["correlations"]), {"owner", "previous", "next"})
        self.assertEqual(result["correlations"]["owner"]["entry_index"], 0)

    def test_correlations_must_be_complete_and_consistent(self):
        primary = snapshot(stage=16)
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "incomplete"):
            decoder.decode_binding_diagnostic(primary + "\n" + progress(96, 16, 0, 1))
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "inconsistent owner"):
            decoder.decode_binding_diagnostic(
                primary + "\n" + correlation("owner", stage=16, related=2)
            )
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "without primary"):
            decoder.decode_binding_diagnostic(correlation("owner", stage=16, related=1))

    def test_complete_snapshot_decodes_entry_and_stage(self):
        result = decoder.decode_binding_diagnostic("unrelated output\n" + snapshot() + "\n")
        self.assertTrue(result["localization_only"])
        self.assertEqual(result["status"], "present")
        self.assertEqual(result["snapshot"]["stage_meaning"], "undeclared explicit name")
        self.assertEqual(result["snapshot"]["name"], "Widget")
        self.assertEqual(len(bytes.fromhex(result["snapshot"]["entry_bytes"])), 28)
        self.assertEqual(result["snapshot"]["name_buffer_bytes"], 256)
        self.assertEqual(result["snapshot"]["base"], 0x1234)
        self.assertEqual(result["snapshot"]["current_scope"], 2)
        self.assertEqual(result["snapshot"]["entry"]["name_offset"], "0x00001000")
        self.assertEqual(result["snapshot"]["entry"]["owner"], 2)
        self.assertEqual(result["snapshot"]["entry"]["flags"], 0x20)
        self.assertEqual(result["snapshot"]["entry"]["target"], 0xFFFF)

    def test_unrelated_progress_phases_are_ignored(self):
        result = decoder.decode_binding_diagnostic(progress(1) + "\n" + snapshot())
        self.assertEqual(result["status"], "present")

    def test_preparation_failure_301_without_buffer_is_localization_only(self):
        result = decoder.decode_binding_diagnostic(preparation_failure(stage=8))
        self.assertEqual(result["preparation_failure"], {
            "stage": 8,
            "stage_meaning": "expression",
            "stage_interpretation": "last attempted boundary",
            "last_published_used": 7,
            "last_published_name_count": 2,
            "live_owned_bytes": 99,
        })
        self.assertNotIn("preparation_failure", decoder.decode_binding_diagnostic("hello"))

    def test_preparation_stage_zero_means_frontend_boundary_not_entered(self):
        failure = decoder.decode_binding_diagnostic(
            preparation_failure(stage=0)
        )["preparation_failure"]
        self.assertEqual(failure["stage_meaning"], "not entered")
        self.assertEqual(failure["stage_interpretation"], "last attempted boundary")
        self.assertNotIn("actual_record_extent", failure)

    def test_writer_status_is_exact_at_writer_and_documented_stale_for_template(self):
        writer = decoder.decode_binding_diagnostic(
            preparation_failure(stage=9) + "\n" + writer_status(0xFFFFFFFE)
        )["preparation_failure"]
        self.assertEqual(writer["last_writer_or_expression_status"], 0xFFFFFFFE)
        self.assertNotIn("last_writer_or_expression_status_note", writer)
        template = decoder.decode_binding_diagnostic(
            preparation_failure(stage=10) + "\n" + writer_status(0xFFFFFFFE)
        )["preparation_failure"]
        self.assertEqual(template["last_writer_or_expression_status"], 0xFFFFFFFE)
        self.assertIn("may be stale", template["last_writer_or_expression_status_note"])

    def test_writer_status_rejects_malformed_duplicate_and_reserved_words(self):
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "malformed preparation"):
            decoder.decode_binding_diagnostic(
                preparation_failure(stage=9) + "\nprogress p=00000134 f=1 l=2 r=3"
            )
        status = writer_status(1)
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "duplicate preparation"):
            decoder.decode_binding_diagnostic(
                preparation_failure(stage=9) + "\n" + status + "\n" + status
            )
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "phase 308 trailing"):
            decoder.decode_binding_diagnostic(
                preparation_failure(stage=9) + "\n" + writer_status(1, reserved_first=1)
            )

    def test_preparation_buffer_uses_actual_extent_and_caps_prefix(self):
        raw = bytes([48]) + bytes(range(1, 24))
        failure = decoder.decode_binding_diagnostic(
            preparation_failure(raw=raw)
        )["preparation_failure"]
        self.assertEqual(failure["actual_record_extent"], 49)
        self.assertEqual(bytes.fromhex(failure["record_prefix_hex"]), raw)
        self.assertTrue(failure["truncated"])

        raw = bytes([7]) + b"abc" + b"hidden tokens beyond extent"
        failure = decoder.decode_binding_diagnostic(
            preparation_failure(raw=raw)
        )["preparation_failure"]
        self.assertEqual(failure["actual_record_extent"], 8)
        self.assertEqual(bytes.fromhex(failure["record_prefix_hex"]), raw[:8])
        self.assertFalse(failure["truncated"])

    def test_pre_call_input_snapshot_decodes_raw_record_and_pointer_aliasing(self):
        raw = bytes([40]) + bytes(range(1, 32))
        result = decoder.decode_binding_diagnostic(
            preparation_failure(raw=b"\x00invalid") + "\n" + pre_call_input(raw=raw)
        )["preparation_failure"]
        self.assertFalse(result["record_framing_valid"])
        self.assertEqual(result["captured_prefix_hex"], (b"\x00invalid".ljust(24, b"\0")).hex())
        snapshot = result["pre_call_input"]
        self.assertEqual(snapshot["input_pointer"], "0x00001000")
        self.assertTrue(snapshot["pointers_alias"])
        self.assertEqual(snapshot["actual_record_extent"], 41)
        self.assertEqual(snapshot["record_prefix_hex"], raw.hex())
        self.assertTrue(snapshot["truncated"])
        distinct = decoder.decode_binding_diagnostic(
            preparation_failure() + "\n" + pre_call_input(0x1000, 0x2000)
        )["preparation_failure"]["pre_call_input"]
        self.assertFalse(distinct["pointers_alias"])

    def test_pre_call_input_snapshot_requires_all_phases_and_valid_extent(self):
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "requires phases 304-307"):
            decoder.decode_binding_diagnostic(
                preparation_failure() + "\n" + progress(304, 1, 2, 3, 4)
            )
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "below 4"):
            decoder.decode_binding_diagnostic(
                preparation_failure() + "\n" + pre_call_input(raw=b"\0" * 32)
            )
        lines = pre_call_input().splitlines()
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "duplicate preparation"):
            decoder.decode_binding_diagnostic(preparation_failure() + "\n" + "\n".join(lines + [lines[0]]))

    def test_postfailure_zero_framing_preserves_opaque_capture(self):
        failure = decoder.decode_binding_diagnostic(
            preparation_failure(raw=b"\0" + bytes(range(1, 24)))
        )["preparation_failure"]
        self.assertFalse(failure["record_framing_valid"])
        self.assertNotIn("record_prefix_hex", failure)
        self.assertEqual(len(bytes.fromhex(failure["captured_prefix_hex"])), 24)

    def test_preparation_records_reject_malformed_combinations(self):
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "both phases"):
            line = preparation_failure(raw=b"\x04abc").splitlines()[0]
            decoder.decode_binding_diagnostic(line + "\n" + progress(302))
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "duplicate preparation"):
            line = preparation_failure().splitlines()[0]
            decoder.decode_binding_diagnostic(line + "\n" + line)
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "unknown preparation"):
            decoder.decode_binding_diagnostic(preparation_failure(stage=11))
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "malformed preparation"):
            decoder.decode_binding_diagnostic("progress p=0000012d f=1 l=2 r=3 m=0000000a")
        self.assertFalse(decoder.decode_binding_diagnostic(
            preparation_failure(raw=b"\x00abc")
        )["preparation_failure"]["record_framing_valid"])

    def test_stage_14_decodes_canonical_import_target_declaration(self):
        result = decoder.decode_binding_diagnostic(snapshot(stage=14))
        self.assertEqual(
            result["snapshot"]["stage_meaning"], "canonical import target declaration"
        )

    def test_stages_15_and_16_decode_target_scan_checks(self):
        meanings = {
            15: "declared canonical target found by linear scan",
            16: "first declared spelling with same leaf",
        }
        for stage, meaning in meanings.items():
            with self.subTest(stage=stage):
                result = decoder.decode_binding_diagnostic(snapshot(stage=stage, related=0))
                self.assertEqual(result["snapshot"]["stage_meaning"], meaning)
                self.assertEqual(result["snapshot"]["related_entry_index"], 0)

    def test_stage_17_decodes_forward_member_acquisition(self):
        result = decoder.decode_binding_diagnostic(snapshot(stage=17))
        self.assertEqual(
            result["snapshot"]["stage_meaning"], "forward numeric member acquisition"
        )

    def test_absent_is_distinct_from_incomplete(self):
        result = decoder.decode_binding_diagnostic("hello\n" + progress(1))
        self.assertEqual(result, {"localization_only": True, "snapshot": None, "status": "absent"})
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "incomplete"):
            decoder.decode_binding_diagnostic(progress(32, 5, 0, 0xFFFFFFFF))

    def test_rejects_overlength_and_inconsistent_ranges(self):
        text = snapshot()
        text = text.replace(progress(33, (0x1234 << 16) | 3, 2, 6),
                            progress(33, (0x1234 << 16) | 3, 2, 256))
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "exceeds 255"):
            decoder.decode_binding_diagnostic(text)
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "outside count"):
            decoder.decode_binding_diagnostic(snapshot(index=3))

    def test_padded_name_chunks_are_truncated_to_declared_length(self):
        result = decoder.decode_binding_diagnostic(snapshot(b"A\0B"))
        self.assertEqual(result["snapshot"]["name"], "A\0B")
        self.assertEqual(result["snapshot"]["name_hex"], "410042")

    def test_current_scope_count_is_valid_and_malformed_trailing_words_reject(self):
        text = snapshot().replace(
            progress(33, (0x1234 << 16) | 3, 2, 6),
            progress(33, (0x1234 << 16) | 3, 3, 6),
        )
        self.assertEqual(decoder.decode_binding_diagnostic(text)["snapshot"]["current_scope"], 3)
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "phase 36"):
            decoder.decode_binding_diagnostic(
                text.replace(progress(36, 0x00010000), progress(36, 0x00010000, 1))
            )
        with self.assertRaisesRegex(decoder.BindingDiagnosticError, "phase 85"):
            decoder.decode_binding_diagnostic(text.replace(progress(85), progress(85, 0, 1, 0)))


if __name__ == "__main__":
    unittest.main()

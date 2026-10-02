import importlib.util
from pathlib import Path
import unittest


SPEC = importlib.util.spec_from_file_location(
    "decode_binding_diagnostic", Path(__file__).parents[1] / "decode_binding_diagnostic.py"
)
decoder = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(decoder)


def progress(phase, first=0, second=0, third=0):
    return f"progress p={phase:08x} f={first:08x} l={second:08x} r={third:08x} m=0000000a"


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

import importlib.util
from pathlib import Path
import unittest
from contextlib import redirect_stdout, redirect_stderr
import io
import json
import tempfile


SPEC = importlib.util.spec_from_file_location(
    "capture_telemetry", Path(__file__).parents[1] / "capture_telemetry.py")
decoder = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(decoder)


def line(phase, values):
    return "progress " + " ".join(
        f"{key}={value:08x}" for key, value in zip(("p", "f", "l", "r", "m"), (phase, *values, 0)))


class CaptureTelemetryTests(unittest.TestCase):
    def capture(self, mask=7, header_bytes=56):
        rows = [line(400, [1, 0, mask])]
        # Four records, their declared headers, and ten 20-byte tokens.
        rows.append(line(401, [4, 4 * header_bytes, 200]))
        for phase in range(402, 407):
            rows.append(line(phase, [0, 0, 0]))
        rows[2] = line(402, [1, 2, 3])
        rows[3] = line(403, [4, 5, 6])
        rows[4] = line(404, [7, 4 * header_bytes + 206, 9])
        rows[5] = line(405, [10, 11, 12])
        rows[6] = line(406, [13, 14, 15])
        for base in (416, 424, 432):
            # Every owner has used=10, capacity=12, slack=2.
            values = ([1, 10, 12], [2, 10, 12], [10, 12, 10], [12, 2, 2], [2, 0, 0])
            rows.extend(line(base + i, row) for i, row in enumerate(values))
        return "\n".join(rows)

    def test_decodes_all_stages_and_counts(self):
        result = decoder.decode_capture_telemetry(self.capture())["capture_summary"]
        self.assertEqual(result["boundary_mask"], 7)
        self.assertEqual(set(result["stages"]), {"acquisition", "replay", "release"})
        self.assertEqual(result["stages"]["acquisition"]["retained_slack"], 2)
        self.assertEqual(result["counters"]["construction_copy"]["retained_append_copy_bytes"], 430)

    def test_declared_source_layout_controls_frozen_observations(self):
        source = (Path(__file__).parents[3] /
                  "native/motorola68000/amigaos/experimental/binary_capture.asm").read_text()
        self.assertEqual(decoder.capture_record_header_bytes(source), 56)
        self.assertEqual(decoder.capture_record_header_bytes(source, defines=["OPFORGE_LAZY_WORK"]), 56)
        work = ["OPFORGE_LAZY_AUDIT", "OPFORGE_LAZY_WORK"]
        self.assertEqual(decoder.capture_record_header_bytes(source, defines=work), 60)
        self.assertEqual(decoder.capture_record_header_bytes(source, defines=work + ["OPFORGE_LAZY_CANDIDATES"]), 72)
        self.assertEqual(decoder.capture_record_header_bytes(source, defines=[name.lower() for name in work]), 60)
        frozen = source.replace("Line\t.word ?", "OldReserved0\t.long ?\nOldReserved1\t.long ?\nLine\t.word ?")
        self.assertEqual(decoder.capture_record_header_bytes(frozen), 64)
        with self.assertRaisesRegex(decoder.CaptureTelemetryError, "56 bytes per record"):
            decoder.decode_capture_telemetry(self.capture(header_bytes=64))
        summary = decoder.decode_capture_telemetry(
            self.capture(header_bytes=64),
            record_header_bytes=decoder.capture_record_header_bytes(frozen))["capture_summary"]
        self.assertEqual(summary["counters"]["retained_composition"]["header_bytes"], 256)
        with self.assertRaisesRegex(decoder.CaptureTelemetryError, "64 bytes per record"):
            decoder.decode_capture_telemetry(self.capture(), record_header_bytes=64)

    def test_nested_symbol_guards_and_alternate_declarations(self):
        source = """.ifndef EMPTY
Record .struct
Magic .long ?
.ifdef OUTER
.ifndef INNER
Extra .long ?
.else
Inner .word ?
.endif
.else
Small .byte ?
.endif
Tokens .byte ?
.endstruct
HEADER_BYTES = Record.Tokens
.else
Record .struct
Only .word ?
Tokens .byte ?
.endstruct
HEADER_BYTES = Record.Tokens
.endif
"""
        for defines, expected in (([], 5), (["OUTER"], 8), (["OUTER", "INNER"], 6), (["EMPTY"], 2)):
            with self.subTest(defines=defines):
                self.assertEqual(decoder.capture_record_header_bytes(source, defines=defines), expected)

    def test_conditional_syntax_and_define_inputs_fail_closed(self):
        source = "Record .struct\nMagic .long ?\nTokens .byte ?\n.endstruct\nHEADER_BYTES = Record.Tokens"
        guards = (".if 1\n.endif", ".ifdef FLAG + 1\n.endif", ".ifndef\n.endif",
                  ".else", ".endif", ".ifdef FLAG", ".ifdef FLAG\n.else\n.else\n.endif",
                  ".ifdef FLAG\n.elseif OTHER\n.endif", ".ifdef FLAG\n.if 0\n.endif\n.endif",
                  ".ifdef FLAG\n.ifndef OTHER\n.endif", ".ifdef FLAG\n.endif extra")
        for guard in guards:
            with self.subTest(guard=guard), self.assertRaises(decoder.CaptureTelemetryError):
                decoder.capture_record_header_bytes(source + "\n" + guard)
        for defines in ("FLAG", None, [1], ["FLAG=1"], ["FLAG extra"], [""], ["1FLAG"]):
            with self.subTest(defines=defines), self.assertRaises(decoder.CaptureTelemetryError):
                decoder.capture_record_header_bytes(source, defines=defines)

    def test_cli_capture_defines_select_layout_and_require_source(self):
        source = (Path(__file__).parents[3] /
                  "native/motorola68000/amigaos/experimental/binary_capture.asm")
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "stdout.txt"
            path.write_text(self.capture(header_bytes=60))
            output = io.StringIO()
            with redirect_stdout(output):
                self.assertEqual(decoder.main([str(path), "--capture-source", str(source),
                    "--capture-define", "OPFORGE_LAZY_AUDIT", "--capture-define", "OPFORGE_LAZY_WORK"]), 0)
            result = json.loads(output.getvalue())
            self.assertEqual(result["capture_summary"]["counters"]["retained_composition"]["header_bytes"], 240)
            with redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as error:
                decoder.main([str(path), "--capture-define", "OPFORGE_LAZY_WORK"])
            self.assertEqual(error.exception.code, 2)

    def test_rejects_ambiguous_or_unsupported_source_layout(self):
        source = "Record .struct\nMagic .long ?\nTokens .byte ?\n.endstruct\nHEADER_BYTES = Record.Tokens"
        self.assertEqual(decoder.capture_record_header_bytes(source), 4)
        for bad in (source.replace(".long ?", ".long 0"),
                    source.replace("Tokens .byte", "Tokens .word"),
                    source.replace(".endstruct", "Trailing .byte ?\n.endstruct"),
                    source.replace("Record.Tokens", "4"),
                    source + "\n" + source,
                    source.replace(".endstruct", "")):
            with self.subTest(source=bad), self.assertRaises(decoder.CaptureTelemetryError):
                decoder.capture_record_header_bytes(bad)
        for bad in (0, -1, True, 56.0):
            with self.subTest(size=bad), self.assertRaises(decoder.CaptureTelemetryError):
                decoder.decode_capture_telemetry(self.capture(), record_header_bytes=bad)

    def test_absent_and_masked_partial_stages(self):
        self.assertIsNone(decoder.decode_capture_telemetry("ordinary output\n")["capture_summary"])
        result = decoder.decode_capture_telemetry(self.capture(mask=2))["capture_summary"]
        self.assertEqual(set(result["stages"]), {"replay"})
        with self.assertRaisesRegex(decoder.CaptureTelemetryError, "incomplete"):
            decoder.decode_capture_telemetry(
                "progress p=00000190 f=00000001 l=00000000 r=00000007 m=00000000")

    def test_rejects_partial_malformed_duplicate_and_bad_version_errors_mask(self):
        valid = self.capture()
        cases = (
            (valid.splitlines()[0], "incomplete"),
            (valid.replace("progress p=00000190", "progress p=bad"), "malformed"),
            (valid + "\n" + valid.splitlines()[0], "duplicate"),
            (valid.replace("f=00000001", "f=00000002", 1), "version"),
            (valid.replace("l=00000000", "l=00000001", 1), "error bits"),
            (valid.replace("r=00000007", "r=00000008", 1), "unknown bits"),
        )
        for text, message in cases:
            with self.subTest(message=message), self.assertRaisesRegex(decoder.CaptureTelemetryError, message):
                decoder.decode_capture_telemetry(text)

    def test_rejects_owner_arithmetic_and_header_bound(self):
        bad_owner = self.capture().replace("f=0000000c", "f=0000000d", 1)
        with self.assertRaisesRegex(decoder.CaptureTelemetryError, "capacity/slack"):
            decoder.decode_capture_telemetry(bad_owner)
        bad_header = self.capture().replace("f=00000004 l=000000e0", "f=00000004 l=000000df")
        with self.assertRaisesRegex(decoder.CaptureTelemetryError, "56 bytes per record"):
            decoder.decode_capture_telemetry(bad_header)
        bad_tokens = self.capture().replace("r=000000c8", "r=000000c9")
        with self.assertRaisesRegex(decoder.CaptureTelemetryError, "multiple of 20"):
            decoder.decode_capture_telemetry(bad_tokens)
        bad_sum = self.capture().replace("l=000001ae", "l=000001af")
        with self.assertRaisesRegex(decoder.CaptureTelemetryError, "composition total"):
            decoder.decode_capture_telemetry(bad_sum)


if __name__ == "__main__":
    unittest.main()

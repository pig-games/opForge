from __future__ import annotations

import importlib.util
import struct
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
MODULE_PATH = ROOT / "scripts/performance/decode_native_progress.py"
SPEC = importlib.util.spec_from_file_location("decode_native_progress", MODULE_PATH)
assert SPEC and SPEC.loader
decoder = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(decoder)


def record(
    flags: int,
    *,
    visits: int = 9,
    phase: int = 6,
    overflow_bits: int = 0,
    exit_status: int = 0,
) -> bytes:
    data = bytearray(decoder.RECORD_BYTES)
    struct.pack_into(">IHHIHHH", data, 0, decoder.MAGIC, 1, flags, 0x1234, phase, 2, 3)
    struct.pack_into(">IIII", data, 20, 7, 6, 100, visits)
    struct.pack_into(">III", data, 60, 400, 100, 300)
    struct.pack_into(">I", data, 76 + (phase - 1) * 4, 55)
    struct.pack_into(">IIII", data, 108, 4096, 8192, 0, overflow_bits)
    struct.pack_into(">I", data, 124, exit_status)
    return bytes(data)


def work_record(
    flags: int,
    *,
    run_id: int = 0x1234,
    mode: int = 2,
    overflow_bits: int = 0,
    exit_status: int = 0,
) -> bytes:
    data = bytearray(decoder.WORK_RECORD_BYTES)
    struct.pack_into(
        ">IHHIHH",
        data,
        0,
        decoder.WORK_MAGIC,
        decoder.WORK_SCHEMA_VERSION,
        flags,
        run_id,
        mode,
        0,
    )
    struct.pack_into(">" + "I" * 19, data, 16, *range(1, 20))
    struct.pack_into(">I", data, 92, overflow_bits)
    struct.pack_into(">I", data, 96, exit_status)
    return bytes(data)


class NativeProgressDecoderTests(unittest.TestCase):
    def test_decodes_complete_big_endian_record(self) -> None:
        report = decoder.decode_progress(record(decoder.FLAG_COMPLETE), require_complete=True)
        self.assertEqual(report["state"], "complete")
        self.assertEqual(report["phase"], "layout")
        self.assertEqual(report["layout_round"], 3)
        self.assertEqual(report["statement_visits"], 9)
        self.assertEqual(report["phase_elapsed_ticks"]["layout"], 55)
        self.assertNotIn("proof_eligible", report)

    def test_incomplete_abort_decodes_but_is_rejected_as_proof(self) -> None:
        payload = record(decoder.FLAG_INCOMPLETE | decoder.FLAG_ABORT_REQUESTED)
        report = decoder.decode_progress(payload)
        self.assertEqual(report["state"], "incomplete")
        self.assertTrue(report["abort_requested"])
        self.assertNotIn("proof_eligible", report)
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "localization evidence"):
            decoder.decode_progress(payload, require_complete=True)

    def test_rejects_truncated_contradictory_and_unknown_records(self) -> None:
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "expected 128"):
            decoder.decode_progress(b"OFPR")
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "both complete"):
            decoder.decode_progress(record(decoder.FLAG_COMPLETE | decoder.FLAG_INCOMPLETE))
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "must be active"):
            decoder.decode_progress(record(0))
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "zero exit status"):
            decoder.decode_progress(record(decoder.FLAG_COMPLETE, exit_status=10))
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "unknown phase"):
            decoder.decode_progress(record(decoder.FLAG_ACTIVE, phase=9))
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "unknown flag bits"):
            decoder.decode_progress(record(decoder.FLAG_ACTIVE | 0x20))
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "unknown overflow bits"):
            decoder.decode_progress(
                record(decoder.FLAG_ACTIVE, overflow_bits=0x80000000)
            )
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "abort requested"):
            decoder.decode_progress(
                record(decoder.FLAG_COMPLETE | decoder.FLAG_ABORT_REQUESTED)
            )

    def test_decodes_correlated_complete_work_record(self) -> None:
        report = decoder.decode_work_multiplication(
            work_record(decoder.WORK_FLAG_COMPLETE),
            expected_run_id=0x1234,
            require_complete=True,
        )
        self.assertEqual(report["state"], "complete")
        self.assertEqual(report["mode"], "layout")
        self.assertEqual(report["pass_one_visits"], 1)
        self.assertEqual(report["layout_rounds"], 4)
        self.assertEqual(report["forward_redirects"], 9)
        self.assertEqual(report["statement_classifications"]["generic"], 14)
        self.assertEqual(report["final_image_bytes"], 19)

    def test_rejects_malformed_or_uncorrelated_work_record(self) -> None:
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "does not match"):
            decoder.decode_work_multiplication(
                work_record(decoder.WORK_FLAG_COMPLETE), expected_run_id=7
            )
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "state active"):
            decoder.decode_work_multiplication(
                work_record(decoder.WORK_FLAG_ACTIVE), expected_state="complete"
            )
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "exit status 9"):
            decoder.decode_work_multiplication(
                work_record(decoder.WORK_FLAG_INCOMPLETE, exit_status=9),
                expected_exit_status=8,
            )
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "unknown work flag"):
            decoder.decode_work_multiplication(work_record(0x20))
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "unknown work overflow"):
            decoder.decode_work_multiplication(
                work_record(decoder.WORK_FLAG_ACTIVE, overflow_bits=0x20)
            )
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "unknown work mode"):
            decoder.decode_work_multiplication(
                work_record(decoder.WORK_FLAG_ACTIVE, mode=4)
            )
        malformed = bytearray(work_record(decoder.WORK_FLAG_ACTIVE))
        malformed[127] = 1
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "reserved"):
            decoder.decode_work_multiplication(bytes(malformed))
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "not proof"):
            decoder.decode_work_multiplication(
                work_record(decoder.WORK_FLAG_INCOMPLETE, exit_status=20),
                require_complete=True,
            )
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "nonzero exit"):
            decoder.decode_work_multiplication(
                work_record(decoder.WORK_FLAG_INCOMPLETE)
            )
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "zero exit"):
            decoder.decode_work_multiplication(
                work_record(decoder.WORK_FLAG_ACTIVE, exit_status=1)
            )


if __name__ == "__main__":
    unittest.main()

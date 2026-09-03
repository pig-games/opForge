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


def symbol_expression_record(
    flags: int,
    *,
    run_id: int = 0x1234,
    phase: int = 6,
    pass_number: int = 2,
    overflow_bits: int = 0,
    exit_status: int = 0,
) -> bytes:
    data = bytearray(decoder.SYMBOL_EXPR_RECORD_BYTES)
    struct.pack_into(
        ">IHHIHH",
        data,
        0,
        decoder.SYMBOL_EXPR_MAGIC,
        decoder.SYMBOL_EXPR_SCHEMA_VERSION,
        flags,
        run_id,
        phase,
        pass_number,
    )
    for value, offset in enumerate(range(20, 200, 4), start=1):
        struct.pack_into(">I", data, offset, value)
    struct.pack_into(">II", data, 200, overflow_bits, exit_status)
    return bytes(data)


def runtime_record(
    flags: int,
    *,
    run_id: int = 0x1234,
    phase: int = 6,
    pass_number: int = 2,
    current_ids: tuple[int, int, int] = (0, 0, 0),
    overflow_bits: int = 0,
    exit_status: int = 0,
) -> bytes:
    data = bytearray(decoder.RUNTIME_RECORD_BYTES)
    struct.pack_into(
        ">IHHIHHHHH",
        data,
        0,
        decoder.RUNTIME_MAGIC,
        decoder.RUNTIME_SCHEMA_VERSION,
        flags,
        run_id,
        phase,
        pass_number,
        *current_ids,
    )
    for value, offset in enumerate(range(24, 128, 4), start=1):
        struct.pack_into(">I", data, offset, value)
    struct.pack_into(">II", data, 128, overflow_bits, exit_status)
    for value, offset in enumerate(range(136, 168, 4), start=27):
        struct.pack_into(">I", data, offset, value)
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

    def test_decodes_correlated_symbol_expression_work(self) -> None:
        report = decoder.decode_symbol_expression_work(
            symbol_expression_record(
                decoder.SYMBOL_EXPR_FLAG_COMPLETE | decoder.SYMBOL_EXPR_FLAG_DETAIL
            ),
            expected_run_id=0x1234,
            expected_state="complete",
            expected_exit_status=0,
            expected_phase=6,
            expected_pass=2,
            require_complete=True,
        )
        self.assertEqual(report["state"], "complete")
        self.assertTrue(report["detail_enabled"])
        self.assertEqual(report["phase"], "layout")
        self.assertEqual(report["lookups"]["exact"]["calls"], 1)
        self.assertEqual(report["lookups"]["final_component"]["candidates"], 8)
        self.assertEqual(report["final_component_ambiguous"], 21)
        self.assertEqual(report["expression"]["requests"], 24)
        self.assertEqual(report["max_hash_chain"], 37)

    def test_rejects_overflowing_symbol_expression_proof(self) -> None:
        complete = decoder.SYMBOL_EXPR_FLAG_COMPLETE | decoder.SYMBOL_EXPR_FLAG_DETAIL
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "not complete proof"):
            decoder.decode_symbol_expression_work(
                symbol_expression_record(complete, overflow_bits=0x01),
                require_complete=True,
            )

        report = decoder.decode_symbol_expression_work(
            symbol_expression_record(complete, overflow_bits=0x01)
        )
        self.assertEqual(report["overflow_bits"], 0x01)

    def test_rejects_malformed_or_uncorrelated_symbol_expression_work(self) -> None:
        complete = decoder.SYMBOL_EXPR_FLAG_COMPLETE | decoder.SYMBOL_EXPR_FLAG_DETAIL
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "run id"):
            decoder.decode_symbol_expression_work(
                symbol_expression_record(complete), expected_run_id=7
            )
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "state complete"):
            decoder.decode_symbol_expression_work(
                symbol_expression_record(complete), expected_state="active"
            )
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "phase 6"):
            decoder.decode_symbol_expression_work(
                symbol_expression_record(complete), expected_phase=5
            )
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "pass 2"):
            decoder.decode_symbol_expression_work(
                symbol_expression_record(complete), expected_pass=1
            )
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "unknown symbol/expression flag"):
            decoder.decode_symbol_expression_work(symbol_expression_record(0x10))
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "unknown symbol/expression overflow"):
            decoder.decode_symbol_expression_work(
                symbol_expression_record(
                    decoder.SYMBOL_EXPR_FLAG_ACTIVE | decoder.SYMBOL_EXPR_FLAG_DETAIL,
                    overflow_bits=0x80,
                )
            )
        malformed = bytearray(symbol_expression_record(complete))
        malformed[255] = 1
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "reserved"):
            decoder.decode_symbol_expression_work(bytes(malformed))
        detail_disabled = bytearray(
            symbol_expression_record(decoder.SYMBOL_EXPR_FLAG_ACTIVE)
        )
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "detail-disabled"):
            decoder.decode_symbol_expression_work(bytes(detail_disabled))
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "not proof"):
            decoder.decode_symbol_expression_work(
                symbol_expression_record(
                    decoder.SYMBOL_EXPR_FLAG_INCOMPLETE
                    | decoder.SYMBOL_EXPR_FLAG_DETAIL,
                    exit_status=20,
                ),
                require_complete=True,
            )
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "nonzero exit"):
            decoder.decode_symbol_expression_work(
                symbol_expression_record(
                    decoder.SYMBOL_EXPR_FLAG_INCOMPLETE
                    | decoder.SYMBOL_EXPR_FLAG_DETAIL
                )
            )

    def test_decodes_correlated_runtime_execution(self) -> None:
        report = decoder.decode_runtime_execution(
            runtime_record(decoder.RUNTIME_FLAG_COMPLETE),
            expected_run_id=0x1234,
            expected_state="complete",
            expected_exit_status=0,
            expected_phase=6,
            expected_pass=2,
            require_complete=True,
        )
        self.assertEqual(report["state"], "complete")
        self.assertEqual(report["vm_invocations"]["tkvm"], 1)
        self.assertEqual(report["vm_opcodes"]["exprvm"], 8)
        self.assertEqual(report["service_invocations"]["value"], 24)
        self.assertEqual(report["candidates"]["selection"], 25)
        self.assertEqual(report["services_by_phase"]["other"], 34)

    def test_rejects_malformed_uncorrelated_or_overflowing_runtime(self) -> None:
        complete = decoder.RUNTIME_FLAG_COMPLETE
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "run id"):
            decoder.decode_runtime_execution(runtime_record(complete), expected_run_id=7)
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "current runtime VM"):
            decoder.decode_runtime_execution(
                runtime_record(complete, current_ids=(5, 0, 0))
            )
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "unknown runtime overflow"):
            decoder.decode_runtime_execution(runtime_record(complete, overflow_bits=0x40))
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "not complete proof"):
            decoder.decode_runtime_execution(
                runtime_record(complete, overflow_bits=1), require_complete=True
            )
        malformed = bytearray(runtime_record(complete))
        malformed[191] = 1
        with self.assertRaisesRegex(decoder.ProgressDecodeError, "reserved runtime"):
            decoder.decode_runtime_execution(bytes(malformed))


if __name__ == "__main__":
    unittest.main()

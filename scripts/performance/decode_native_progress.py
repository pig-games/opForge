#!/usr/bin/env python3
"""Decode the provisional 128-byte native OFPR progress bridge record."""

from __future__ import annotations

import argparse
import json
import struct
import sys
from pathlib import Path

MAGIC = 0x4F465052
SCHEMA_VERSION = 1
RECORD_BYTES = 128

FLAG_ACTIVE = 1
FLAG_COMPLETE = 2
FLAG_INCOMPLETE = 4
FLAG_ABORT_REQUESTED = 8
FLAG_HEARTBEAT = 16
KNOWN_FLAGS = (
    FLAG_ACTIVE
    | FLAG_COMPLETE
    | FLAG_INCOMPLETE
    | FLAG_ABORT_REQUESTED
    | FLAG_HEARTBEAT
)

OVERFLOW_VISITS = 1
OVERFLOW_PHASE_TICKS = 2
KNOWN_OVERFLOW_BITS = OVERFLOW_VISITS | OVERFLOW_PHASE_TICKS

WORK_MAGIC = 0x4F46574D
WORK_SCHEMA_VERSION = 1
WORK_RECORD_BYTES = 128
WORK_FLAG_ACTIVE = 1
WORK_FLAG_COMPLETE = 2
WORK_FLAG_INCOMPLETE = 4
WORK_KNOWN_FLAGS = WORK_FLAG_ACTIVE | WORK_FLAG_COMPLETE | WORK_FLAG_INCOMPLETE
WORK_KNOWN_OVERFLOW_BITS = 0x1F

WORK_MODES = ("none", "pass_one", "layout", "final_emission")

PHASES = (
    "idle",
    "startup",
    "package",
    "frontend",
    "statement_build",
    "pass_one",
    "layout",
    "final_emission",
    "artifacts",
)


class ProgressDecodeError(ValueError):
    """The record is malformed or cannot satisfy the requested proof policy."""


def _u16(data: bytes, offset: int) -> int:
    return struct.unpack_from(">H", data, offset)[0]


def _u32(data: bytes, offset: int) -> int:
    return struct.unpack_from(">I", data, offset)[0]


def decode_progress(data: bytes, *, require_complete: bool = False) -> dict[str, object]:
    if len(data) != RECORD_BYTES:
        raise ProgressDecodeError(f"expected {RECORD_BYTES} bytes, got {len(data)}")
    if _u32(data, 0) != MAGIC:
        raise ProgressDecodeError("record magic is not OFPR")
    version = _u16(data, 4)
    if version != SCHEMA_VERSION:
        raise ProgressDecodeError(f"unsupported schema version {version}")

    flags = _u16(data, 6)
    unknown_flags = flags & ~KNOWN_FLAGS
    if unknown_flags:
        raise ProgressDecodeError(f"unknown flag bits 0x{unknown_flags:04x}")
    complete = bool(flags & FLAG_COMPLETE)
    incomplete = bool(flags & FLAG_INCOMPLETE)
    active = bool(flags & FLAG_ACTIVE)
    if complete and incomplete:
        raise ProgressDecodeError("record cannot be both complete and incomplete")
    if active and (complete or incomplete):
        raise ProgressDecodeError("terminal record cannot remain active")
    if complete and (flags & FLAG_ABORT_REQUESTED):
        raise ProgressDecodeError("complete record cannot have abort requested")
    if not active and not (complete or incomplete):
        raise ProgressDecodeError("record must be active, complete, or incomplete")
    if require_complete and not complete:
        raise ProgressDecodeError("incomplete or active progress is localization evidence, not proof")

    exit_status = _u32(data, 124)
    if complete and exit_status != 0:
        raise ProgressDecodeError("complete record must have zero exit status")

    overflow_bits = _u32(data, 120)
    unknown_overflow_bits = overflow_bits & ~KNOWN_OVERFLOW_BITS
    if unknown_overflow_bits:
        raise ProgressDecodeError(
            f"unknown overflow bits 0x{unknown_overflow_bits:08x}"
        )

    phase = _u16(data, 12)
    if phase >= len(PHASES):
        raise ProgressDecodeError(f"unknown phase {phase}")
    current = _u32(data, 20)
    last = _u32(data, 24)
    phase_ticks = {
        PHASES[index + 1]: _u32(data, 76 + index * 4)
        for index in range(8)
    }
    return {
        "schema_version": version,
        "run_id": _u32(data, 8),
        "state": "complete" if complete else "incomplete" if incomplete else "active",
        "active": active,
        "abort_requested": bool(flags & FLAG_ABORT_REQUESTED),
        "heartbeat_enabled": bool(flags & FLAG_HEARTBEAT),
        "phase": PHASES[phase],
        "phase_id": phase,
        "pass": _u16(data, 14),
        "layout_round": _u16(data, 16),
        "current_statement": None if current == 0xFFFFFFFF else current,
        "last_completed_statement": None if last == 0xFFFFFFFF else last,
        "total_statements": _u32(data, 28),
        "statement_visits": _u32(data, 32),
        "current_source_id": _u32(data, 36),
        "current_module_id": _u32(data, 40),
        "current_vm_service_id": _u32(data, 44),
        "current_program_id": _u32(data, 48),
        "flow_redirects": _u32(data, 52),
        "backward_redirects": _u32(data, 56),
        "last_progress_tick": _u32(data, 60),
        "run_start_tick": _u32(data, 64),
        "total_elapsed_ticks": _u32(data, 68),
        "phase_elapsed_ticks": phase_ticks,
        "heartbeat_quantum": _u32(data, 108),
        "next_heartbeat": _u32(data, 112),
        "abort_after_visits": _u32(data, 116),
        "overflow_bits": overflow_bits,
        "exit_status": exit_status,
    }


def decode_work_multiplication(
    data: bytes,
    *,
    expected_run_id: int | None = None,
    expected_state: str | None = None,
    expected_exit_status: int | None = None,
    require_complete: bool = False,
) -> dict[str, object]:
    if len(data) != WORK_RECORD_BYTES:
        raise ProgressDecodeError(
            f"expected {WORK_RECORD_BYTES} work bytes, got {len(data)}"
        )
    if _u32(data, 0) != WORK_MAGIC:
        raise ProgressDecodeError("work record magic is not OFWM")
    version = _u16(data, 4)
    if version != WORK_SCHEMA_VERSION:
        raise ProgressDecodeError(f"unsupported work schema version {version}")
    flags = _u16(data, 6)
    unknown_flags = flags & ~WORK_KNOWN_FLAGS
    if unknown_flags:
        raise ProgressDecodeError(f"unknown work flag bits 0x{unknown_flags:04x}")
    active = bool(flags & WORK_FLAG_ACTIVE)
    complete = bool(flags & WORK_FLAG_COMPLETE)
    incomplete = bool(flags & WORK_FLAG_INCOMPLETE)
    if complete and incomplete:
        raise ProgressDecodeError("work record cannot be both complete and incomplete")
    if active and (complete or incomplete):
        raise ProgressDecodeError("terminal work record cannot remain active")
    if not active and not (complete or incomplete):
        raise ProgressDecodeError("work record must be active, complete, or incomplete")
    if require_complete and not complete:
        raise ProgressDecodeError("incomplete or active work record is not proof")

    state = "complete" if complete else "incomplete" if incomplete else "active"
    if expected_state is not None and state != expected_state:
        raise ProgressDecodeError(
            f"work record state {state} does not match progress state {expected_state}"
        )

    run_id = _u32(data, 8)
    if expected_run_id is not None and run_id != expected_run_id:
        raise ProgressDecodeError(
            f"work record run id {run_id} does not match progress run id {expected_run_id}"
        )
    mode = _u16(data, 12)
    if mode >= len(WORK_MODES):
        raise ProgressDecodeError(f"unknown work mode {mode}")
    if _u16(data, 14) != 0 or any(data[100:]):
        raise ProgressDecodeError("reserved work record bytes must be zero")
    overflow_bits = _u32(data, 92)
    unknown_overflow_bits = overflow_bits & ~WORK_KNOWN_OVERFLOW_BITS
    if unknown_overflow_bits:
        raise ProgressDecodeError(
            f"unknown work overflow bits 0x{unknown_overflow_bits:08x}"
        )
    exit_status = _u32(data, 96)
    if complete and exit_status != 0:
        raise ProgressDecodeError("complete work record must have zero exit status")
    if incomplete and exit_status == 0:
        raise ProgressDecodeError("incomplete work record must have nonzero exit status")
    if active and exit_status != 0:
        raise ProgressDecodeError("active work record must have zero exit status")
    if expected_exit_status is not None and exit_status != expected_exit_status:
        raise ProgressDecodeError(
            "work record exit status "
            f"{exit_status} does not match progress exit status {expected_exit_status}"
        )

    return {
        "schema_version": version,
        "run_id": run_id,
        "state": state,
        "mode": WORK_MODES[mode],
        "mode_id": mode,
        "pass_one_visits": _u32(data, 16),
        "layout_visits": _u32(data, 20),
        "final_emission_visits": _u32(data, 24),
        "layout_rounds": _u32(data, 28),
        "final_emissions": _u32(data, 32),
        "layout_label_changes": _u32(data, 36),
        "layout_placement_changes": _u32(data, 40),
        "flow_rows": _u32(data, 44),
        "forward_redirects": _u32(data, 48),
        "backward_redirects": _u32(data, 52),
        "statement_classifications": {
            "module": _u32(data, 56),
            "endmodule": _u32(data, 60),
            "use": _u32(data, 64),
            "generic": _u32(data, 68),
        },
        "max_statement": _u32(data, 72),
        "max_forward_span": _u32(data, 76),
        "max_backward_span": _u32(data, 80),
        "convergence_image_bytes": _u32(data, 84),
        "final_image_bytes": _u32(data, 88),
        "overflow_bits": overflow_bits,
        "exit_status": exit_status,
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("record", type=Path)
    parser.add_argument("--work-record", type=Path)
    parser.add_argument("--require-complete", action="store_true")
    args = parser.parse_args()
    try:
        report = decode_progress(
            args.record.read_bytes(), require_complete=args.require_complete
        )
        if args.work_record is not None:
            report["work_multiplication"] = decode_work_multiplication(
                args.work_record.read_bytes(),
                expected_run_id=int(report["run_id"]),
                expected_state=str(report["state"]),
                expected_exit_status=int(report["exit_status"]),
                require_complete=args.require_complete,
            )
    except (OSError, ProgressDecodeError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    print(json.dumps(report, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

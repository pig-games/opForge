"""Decode guest progress records for bounded capture ownership telemetry.

The summary is an observation of the guest's capture accounting. It does not
establish that assembly completed or that native output matched a reference.
"""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path


class CaptureTelemetryError(ValueError):
    """The capture telemetry records are malformed or internally inconsistent."""


_LINE = re.compile(
    r"^progress p=([0-9a-fA-F]{8}) f=([0-9a-fA-F]{8}) "
    r"l=([0-9a-fA-F]{8}) r=([0-9a-fA-F]{8}) m=([0-9a-fA-F]{8})$"
)
_SCHEMA = {
    400: ("metadata", ("version", "error_bits", "boundary_mask")),
    401: ("retained_composition", ("records", "header_bytes", "token_bytes")),
    402: ("preparation", ("lexeme_bytes", "plan_bytes", "padding_bytes")),
    403: ("construction", ("retained_source_bytes", "constructed_records", "construction_cleared_bytes")),
    404: ("construction_copy", ("construction_copy_bytes", "retained_append_copy_bytes", "configuration_materialization_copy_bytes")),
    405: ("replay_copy", ("semantic_replay_materialization_copy_bytes", "global_grow_calls", "global_grow_copy_used_bytes")),
    406: ("retained_growth", ("retained_grow_calls", "retained_grow_copy_used_bytes", "retained_peak_capacity_including_overlap")),
}
_SNAPSHOTS = {
    416: ("acquisition", (
        ("retained_region_count", "retained_used", "retained_capacity"),
        ("retained_slack", "temporary_used", "temporary_capacity"),
        ("table_used", "table_capacity", "order_used"),
        ("order_capacity", "temporary_slack", "table_slack"),
        ("order_slack", "reserved_0", "reserved_1"),
    )),
    424: ("replay", (
        ("retained_region_count", "retained_used", "retained_capacity"),
        ("retained_slack", "temporary_used", "temporary_capacity"),
        ("table_used", "table_capacity", "order_used"),
        ("order_capacity", "temporary_slack", "table_slack"),
        ("order_slack", "reserved_0", "reserved_1"),
    )),
    432: ("release", (
        ("retained_region_count", "retained_used", "retained_capacity"),
        ("retained_slack", "temporary_used", "temporary_capacity"),
        ("table_used", "table_capacity", "order_used"),
        ("order_capacity", "temporary_slack", "table_slack"),
        ("order_slack", "reserved_0", "reserved_1"),
    )),
}
_STAGE_BITS = {"acquisition": 1, "replay": 2, "release": 4}
_EXPECTED_PHASES = set(_SCHEMA) | {
    base + i for base in _SNAPSHOTS for i in range(5)
}


def _fail(message: str):
    raise CaptureTelemetryError(message)


def decode_capture_telemetry(text: str) -> dict:
    """Decode exact progress rows, rejecting partial or invalid summaries."""
    rows = {}
    for line_number, line in enumerate(text.splitlines(), 1):
        if not line.strip().lower().startswith("progress p="):
            continue
        match = _LINE.fullmatch(line.strip())
        if not match:
            _fail(f"malformed progress row on line {line_number}")
        phase, *values = (int(value, 16) for value in match.groups())
        if phase not in _EXPECTED_PHASES:
            continue
        if phase in rows:
            _fail(f"duplicate capture telemetry phase 0x{phase:x}")
        rows[phase] = values

    if not rows:
        return {"capture_summary": None}
    missing = sorted(_EXPECTED_PHASES - rows.keys())
    if missing:
        _fail("incomplete capture telemetry; missing phases: " + ", ".join(f"0x{x:x}" for x in missing))

    metadata = dict(zip(_SCHEMA[400][1], rows[400]))
    if metadata["version"] != 1:
        _fail(f"unsupported capture telemetry version {metadata['version']}")
    if metadata["error_bits"]:
        _fail(f"capture telemetry reports error bits 0x{metadata['error_bits']:x}")
    if metadata["boundary_mask"] & ~7:
        _fail("capture telemetry boundary mask has unknown bits")

    summary = {"version": 1, "boundary_mask": metadata["boundary_mask"], "stages": {}, "counters": {}}
    for phase, (name, fields) in _SCHEMA.items():
        values = dict(zip(fields, rows[phase]))
        if phase == 400:
            continue
        summary["counters"][name] = values
    composition = summary["counters"]["retained_composition"]
    append = summary["counters"]["construction_copy"]["retained_append_copy_bytes"]
    if composition["header_bytes"] != composition["records"] * 64:
        _fail("retained header bytes do not equal 64 bytes per record")
    if composition["token_bytes"] % 20:
        _fail("retained token bytes are not a multiple of 20")
    if composition["token_bytes"] > composition["records"] * 64 * 20:
        _fail("retained token bytes exceed the 64-token-per-record bound")
    preparation = summary["counters"]["preparation"]
    composition_total = (composition["header_bytes"] + composition["token_bytes"]
                         + preparation["lexeme_bytes"] + preparation["plan_bytes"]
                         + preparation["padding_bytes"])
    if composition_total != append:
        _fail("retained composition total does not equal append-copy bytes")
    if summary["counters"]["construction"]["constructed_records"] < composition["records"]:
        _fail("constructed record count is below retained record count")

    for base, (stage, row_fields) in _SNAPSHOTS.items():
        bit = _STAGE_BITS[stage]
        if not metadata["boundary_mask"] & bit:
            continue
        snapshot = {}
        for offset, fields in enumerate(row_fields):
            snapshot.update(zip(fields, rows[base + offset]))
        if snapshot["reserved_0"] or snapshot["reserved_1"]:
            _fail(f"{stage} reserved snapshot fields must be zero")
        for owner in ("retained", "temporary", "table", "order"):
            used, capacity, slack = (snapshot[f"{owner}_{key}"] for key in ("used", "capacity", "slack"))
            if capacity < used or slack != capacity - used:
                _fail(f"{stage} {owner} capacity/slack is inconsistent")
        summary["stages"][stage] = snapshot
    return {"capture_summary": summary}


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("file", type=Path, help="guest stdout text file")
    args = parser.parse_args(argv)
    try:
        result = decode_capture_telemetry(args.file.read_text())
    except (OSError, CaptureTelemetryError) as error:
        parser.error(str(error))
    print(json.dumps(result, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

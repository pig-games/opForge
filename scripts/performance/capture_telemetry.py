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


def _active_capture_statements(source, defines):
    """Evaluate only symbol-presence guards; this is not an assembler evaluator."""
    if isinstance(defines, (str, bytes)):
        _fail("capture defines must be a collection of symbol names")
    try:
        symbols = set()
        for name in defines:
            if not isinstance(name, str) or not re.fullmatch(r"[A-Za-z_][A-Za-z_0-9]*", name):
                _fail("capture defines must contain symbol names without assignments")
            symbols.add(name.casefold())
    except TypeError:
        _fail("capture defines must be a collection of symbol names")
    statements, stack, active = [], [], True
    for line in source.splitlines():
        statement = line.partition(";")[0].strip()
        guard = re.fullmatch(r"\.(ifdef|ifndef)\s+([A-Za-z_][A-Za-z_0-9]*)", statement, re.IGNORECASE)
        if guard:
            condition = guard[2].casefold() in symbols
            if guard[1].casefold() == "ifndef":
                condition = not condition
            stack.append((active, condition, False))
            active = active and condition
        elif statement.casefold() == ".else":
            if not stack or stack[-1][2]:
                _fail("unbalanced or duplicate capture conditional else")
            parent, condition, _ = stack[-1]
            stack[-1] = (parent, condition, True)
            active = parent and not condition
        elif statement.casefold() == ".endif":
            if not stack:
                _fail("unbalanced capture conditional endif")
            active = stack.pop()[0]
        elif re.match(r"\.(?:if|else|elif|endif)", statement, re.IGNORECASE):
            _fail("unsupported capture conditional directive")
        elif active:
            statements.append(statement)
    if stack:
        _fail("unbalanced capture conditional nesting")
    return statements


def capture_record_header_bytes(source: str, *, defines=()) -> int:
    """Read the declared Record.Tokens offset; reject unsupported declarations.

    This inspects the capture source paired with an observed executable, allowing
    a frozen comparison bundle to declare its own layout without inferring it
    from telemetry totals. Defines are symbol names; only ifdef/ifndef/else/endif
    are supported, including guards around the declaration and its fields.
    """
    statements = _active_capture_statements(source, defines)
    starts = [i for i, line in enumerate(statements)
              if re.fullmatch(r"Record\s+\.struct", line, re.IGNORECASE)]
    if len(starts) != 1:
        _fail("capture source must declare exactly one Record struct")
    offset = 0
    fields = set()
    tokens_offset = None
    ended = False
    for statement in statements[starts[0] + 1:]:
        if not statement:
            continue
        if re.fullmatch(r"\.endstruct", statement, re.IGNORECASE):
            ended = True
            break
        match = re.fullmatch(r"([A-Za-z_][A-Za-z_0-9]*)\s+\.(long|word|byte)\s+\?",
                             statement, re.IGNORECASE)
        if not match:
            _fail("unsupported capture Record field declaration")
        name, width = match.groups()
        name, width = name.lower(), width.lower()
        if name in fields or tokens_offset is not None:
            _fail("duplicate or trailing capture Record field")
        fields.add(name)
        if name == "tokens":
            if width != "byte":
                _fail("capture Record.Tokens must be a byte field")
            tokens_offset = offset
        offset += {"long": 4, "word": 2, "byte": 1}[width]
    if not ended or tokens_offset is None or tokens_offset <= 0:
        _fail("incomplete capture Record declaration")
    assignments = [line for line in statements
                   if re.match(r"HEADER_BYTES\s*=", line, re.IGNORECASE)]
    if len(assignments) != 1 or not re.fullmatch(
            r"HEADER_BYTES\s*=\s*Record\.Tokens", assignments[0], re.IGNORECASE):
        _fail("capture HEADER_BYTES must equal Record.Tokens")
    return tokens_offset


def decode_capture_telemetry(text: str, *, record_header_bytes: int = 56) -> dict:
    """Decode exact progress rows, rejecting partial or invalid summaries."""
    if type(record_header_bytes) is not int or record_header_bytes <= 0:
        _fail("record header bytes must be a positive integer")
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
    if composition["header_bytes"] != composition["records"] * record_header_bytes:
        _fail(f"retained header bytes do not equal {record_header_bytes} bytes per record")
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


def decode_capture_store_telemetry(text: str) -> dict | None:
    """Decode optional bounded-store counters through the existing progress ABI.

    Absence permits reference images predating disk backing. Partial/duplicate
    summaries fail closed. These counters do not establish native completion.
    Payload capacity excludes the directory, executable and OS file buffers.
    """
    rows = {}
    for line in text.splitlines():
        match = _LINE.fullmatch(line.strip())
        if not match:
            if line.strip().lower().startswith("progress p="):
                _fail("malformed capture store progress row")
            continue
        phase, *values = (int(value, 16) for value in match.groups())
        if phase not in (360, 361, 362):
            continue
        if phase in rows:
            _fail("duplicate capture store telemetry")
        rows[phase] = values
    if not rows:
        return None
    if set(rows) != {360, 361, 362}:
        _fail("incomplete capture store telemetry")
    if rows[360][2] or rows[361][2]:
        _fail("capture store reserved fields must be zero")
    return dict(zip(("read_calls", "read_bytes", "write_calls", "write_bytes",
                     "fault_attempts", "evictions", "peak_payload_capacity"),
                    rows[360][:2] + rows[361][:2] + rows[362][:3]))


def decode_ordered_record_telemetry(text: str) -> dict | None:
    """Decode optional copy/reuse counters; absence permits reference images.

    Attempt counts include failures. Saving counts describe successful identity
    layouts, and do not establish native completion or the actual heap peak.
    """
    rows = {}
    for line in text.splitlines():
        match = _LINE.fullmatch(line.strip())
        if not match:
            if line.strip().lower().startswith("progress p="):
                _fail("malformed ordered record progress row")
            continue
        phase, *values = (int(value, 16) for value in match.groups())
        if phase not in (363, 364):
            continue
        if phase in rows:
            _fail("duplicate ordered record telemetry")
        rows[phase] = values
    if not rows:
        return None
    if set(rows) != {363, 364}:
        _fail("incomplete ordered record telemetry")
    calls, reuses, copied = rows[363][:3]
    avoided, capacity, reserved = rows[364][:3]
    if reserved:
        _fail("ordered record reserved field must be zero")
    if reuses > calls or (not calls and copied) or (not reuses and (avoided or capacity)):
        _fail("inconsistent ordered record counters")
    return dict(materializations=calls, identity_reuses=reuses, copied_bytes=copied,
                avoided_bytes=avoided, avoided_capacity=capacity)


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("file", type=Path, help="guest stdout text file")
    parser.add_argument("--capture-source", type=Path,
                        help="capture source paired with executable (default: current 56-byte layout)")
    parser.add_argument("--capture-define", action="append", default=[], metavar="NAME",
                        help="defined symbol for --capture-source layout; repeat for multiple symbols")
    args = parser.parse_args(argv)
    if args.capture_define and args.capture_source is None:
        parser.error("--capture-define requires --capture-source")
    try:
        header_bytes = (capture_record_header_bytes(args.capture_source.read_text(), defines=args.capture_define)
                        if args.capture_source else 56)
        text = args.file.read_text()
        result = decode_capture_telemetry(text, record_header_bytes=header_bytes)
        result["capture_store"] = decode_capture_store_telemetry(text)
        result["ordered_records"] = decode_ordered_record_telemetry(text)
    except (OSError, CaptureTelemetryError) as error:
        parser.error(str(error))
    print(json.dumps(result, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""Decode bounded binding failure snapshots from captured native progress text.

Usage: python3 scripts/performance/decode_binding_diagnostic.py capture.txt
The result is localization evidence only; it does not establish assembly success
or parity. An absent snapshot is returned explicitly as ``snapshot: null``.
"""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path


PROGRESS = re.compile(
    r"^progress p=([0-9a-fA-F]{8}) f=([0-9a-fA-F]{8}) "
    r"l=([0-9a-fA-F]{8}) r=([0-9a-fA-F]{8}) m=([0-9a-fA-F]{8})$"
)
PREPARATION_PROGRESS_PREFIX = re.compile(r"^progress p=([0-9a-fA-F]{8})\b")
PREPARATION_STAGES = {
    0: "not entered",
    1: "declaration",
    2: "conditional",
    3: "configuration",
    4: "operand roles",
    5: "scope",
    6: "file",
    7: "data",
    8: "expression",
    9: "writer",
    10: "template",
    11: "fragment binding",
    12: "fragment suffix",
    13: "fragment append",
    14: "statement definition storage",
    15: "statement invocation",
}
STAGES = {
    1: "lexical scope still open",
    2: "aggregate import completion",
    3: "section completion",
    4: "output-section resolution",
    5: "undeclared explicit name",
    6: "exhausted lexical parent search",
    7: "module visibility",
    8: "prepared-record identity remap",
    9: "missing module in all-import validation",
    10: "missing module in selected-import validation",
    11: "invalid selected names in all-import validation",
    12: "invalid selected names in selected-import validation",
    13: "unresolved import proxy",
    14: "canonical import target declaration",
    15: "declared canonical target found by linear scan",
    16: "first declared spelling with same leaf",
    17: "forward numeric member acquisition",
    20: "canonical binding identity",
}
ENTRY_FIELDS = (
    (0, 4, "name_offset", "u32"),
    (4, 2, "length", "u16"),
    (6, 2, "owner", "u16"),
    (8, 2, "leaf", "u16"),
    (10, 2, "flags", "u16"),
    (12, 2, "target", "u16"),
    (14, 2, "next", "u16"),
    (16, 2, "scope_kind", "u16"),
    (18, 2, "padding", "u16"),
    (20, 2, "member_base", "u16"),
    (22, 2, "template_module", "u16"),
    (24, 2, "template_flags", "u16"),
)
SNAPSHOT_PHASES = {
    "snapshot": (32, 64),
    "owner": (96, 128),
    "previous": (160, 192),
    "next": (224, 256),
}
MODULE_METADATA_PHASES = {459: "source", 460: "target"}
PACKED_RECORD_PHASES = range(336, 344)
PACKED_RECORD_CONSUMERS = {
    8: "prepared-record identity remap",
    18: "block index",
    19: "block references",
}


class BindingDiagnosticError(ValueError):
    """A captured diagnostic is malformed, incomplete, or inconsistent."""


def _field(raw: bytes, start: int, size: int) -> int:
    return int.from_bytes(raw[start : start + size], "big")


def _decode_entry(raw: bytes) -> dict[str, object]:
    fields: dict[str, object] = {}
    for offset, size, name, kind in ENTRY_FIELDS:
        value = _field(raw, offset, size)
        fields[name] = f"0x{value:08x}" if kind == "u32" else value
    return fields


def decode_binding_diagnostic(text: str) -> dict[str, object]:
    """Decode all phase lines for the first binding snapshot in captured text."""
    groups = {name: {} for name in SNAPSHOT_PHASES}
    preparation_phases: dict[int, tuple[int, int, int, int]] = {}
    packed_record_phases: dict[int, tuple[int, int, int, int]] = {}
    module_phases: dict[int, tuple[int, int, int, int]] = {}
    for line in text.splitlines():
        match = PROGRESS.fullmatch(line.strip())
        if not match:
            candidate = PREPARATION_PROGRESS_PREFIX.match(line.strip())
            if candidate:
                malformed_phase = int(candidate.group(1), 16)
                if malformed_phase in (301, 302, 303, 304, 305, 306, 307, 308):
                    raise BindingDiagnosticError("malformed preparation failure progress record")
                if malformed_phase in MODULE_METADATA_PHASES:
                    raise BindingDiagnosticError("malformed module metadata progress record")
                if malformed_phase in PACKED_RECORD_PHASES:
                    raise BindingDiagnosticError("malformed packed-record failure progress record")
            continue
        phase, *values = (int(part, 16) for part in match.groups())
        if phase in (301, 302, 303, 304, 305, 306, 307, 308):
            if phase in preparation_phases:
                raise BindingDiagnosticError(f"duplicate preparation failure phase {phase}")
            preparation_phases[phase] = tuple(values)
            continue
        if phase in MODULE_METADATA_PHASES:
            if phase in module_phases:
                raise BindingDiagnosticError(f"duplicate module metadata phase {phase}")
            module_phases[phase] = tuple(values)
            continue
        if phase in PACKED_RECORD_PHASES:
            if phase in packed_record_phases:
                raise BindingDiagnosticError(f"duplicate packed-record failure phase {phase}")
            packed_record_phases[phase] = tuple(values)
            continue
        for name, (metadata, spelling) in SNAPSHOT_PHASES.items():
            normalized = None
            if metadata <= phase < metadata + 5:
                normalized = phase - metadata + 32
            elif spelling <= phase < spelling + 22:
                normalized = phase - spelling + 64
            if normalized is not None:
                if normalized in groups[name]:
                    raise BindingDiagnosticError(f"duplicate binding phase {phase}")
                groups[name][normalized] = tuple(values)
                break
    primary = _decode_snapshot(groups["snapshot"])
    correlations = {
        name: _decode_snapshot(phases)
        for name, phases in groups.items()
        if name != "snapshot" and phases
    }
    if correlations:
        if primary is None:
            raise BindingDiagnosticError("correlation without primary snapshot")
        for name, correlated in correlations.items():
            if (correlated["related_entry_index"] != primary["entry_index"]
                or any(correlated[field] != primary[field]
                       for field in ("stage", "base", "count", "current_scope"))):
                raise BindingDiagnosticError(f"inconsistent {name} correlation")
    result = {"localization_only": True, "snapshot": primary,
              "status": "present" if primary is not None else "absent"}
    if correlations:
        result["correlations"] = correlations
    if module_phases:
        result["module_metadata"] = _decode_module_metadata(module_phases, primary)
    if preparation_phases:
        result["preparation_failure"] = _decode_preparation_failure(preparation_phases)
    if packed_record_phases:
        packed_record_failure = _decode_packed_record_failure(packed_record_phases)
        consumer_stage = packed_record_failure["consumer_stage"]
        if consumer_stage == 8 and (primary is None or primary["stage"] != 8):
            raise BindingDiagnosticError("stage 8 packed-record capture requires a primary stage 8 snapshot")
        if primary is not None and primary["stage"] != consumer_stage:
            raise BindingDiagnosticError("packed-record capture has inconsistent primary snapshot context")
        result["packed_record_failure"] = packed_record_failure
    return result


def _decode_module_metadata(phases, primary):
    if primary is None or primary["entry"] is None:
        raise BindingDiagnosticError("module metadata requires a primary entry snapshot")
    result = {}
    for phase, (flags, owner, origin, _) in phases.items():
        if any(value > 0xFFFF for value in (flags, owner, origin)):
            raise BindingDiagnosticError("module metadata exceeds its u16 cells")
        kind = MODULE_METADATA_PHASES[phase]
        index = primary["entry_index"] if kind == "source" else primary["entry"]["target"] - primary["base"]
        if not 0 <= index < primary["count"]:
            raise BindingDiagnosticError("module metadata index exceeds the owning scope bounds")
        result[kind] = {
            "entry_index": index,
            "flags": flags,
            "public": bool(flags & 1),
            "used": bool(flags & 2),
            "mixed_origins": bool(flags & 4),
            "declaration_owner": owner,
            "reference_origin": origin,
        }
    return result


def _decode_packed_record_failure(
    phases: dict[int, tuple[int, int, int, int]],
) -> dict[str, object]:
    required = set(PACKED_RECORD_PHASES)
    missing = sorted(required - phases.keys())
    if missing:
        raise BindingDiagnosticError(
            "incomplete packed-record failure capture; missing phases " + ", ".join(map(str, missing))
        )
    if phases[342][1] != 0 or phases[342][2] != 0:
        raise BindingDiagnosticError("phase 342 trailing words must be zero")
    consumer_stage, reserved_first, reserved_second, _ = phases[343]
    if reserved_first != 0 or reserved_second != 0:
        raise BindingDiagnosticError("phase 343 trailing words must be zero")
    if consumer_stage not in PACKED_RECORD_CONSUMERS:
        raise BindingDiagnosticError(f"unknown packed-record consumer stage {consumer_stage}")
    raw = b"".join(
        word.to_bytes(4, "big")
        for phase in range(336, 342)
        for word in phases[phase][:3]
    ) + phases[342][0].to_bytes(4, "big")
    if len(raw) != 76:
        raise BindingDiagnosticError("remap failure capture must contain 76 bytes")
    cursor, record, extent = (int.from_bytes(raw[i:i + 4], "big") for i in (0, 4, 8))
    head, window = raw[12:44], raw[44:76]
    if record == 0xFFFFFFFF:
        if extent != 0:
            raise BindingDiagnosticError("absent remap record must have zero extent")
        record_offset = None
        record_prefix = b""
    else:
        if extent == 0 or record > cursor or cursor - record > extent:
            raise BindingDiagnosticError("remap record extent does not contain cursor")
        record_offset = record
        record_prefix = head[:min(extent, 32)]
    return {
        "consumer_stage": consumer_stage,
        "consumer_stage_meaning": PACKED_RECORD_CONSUMERS[consumer_stage],
        "cursor_offset": cursor,
        "record_offset": record_offset,
        "record_bytes": extent,
        "record_prefix_hex": record_prefix.hex(),
        "cursor_window_hex": window.hex(),
    }


def _decode_preparation_failure(
    phases: dict[int, tuple[int, int, int, int]],
) -> dict[str, object]:
    """Decode preparation localization evidence, not a precise callee PC.

    The stage identifies the last attempted boundary; stage zero means no
    frontend processRecord boundary was entered.
    """
    if 301 not in phases:
        raise BindingDiagnosticError("preparation buffer phases require phase 301")
    has_first, has_second = 302 in phases, 303 in phases
    if has_first != has_second:
        raise BindingDiagnosticError("preparation buffer requires both phases 302 and 303")
    stage, used, name_count, live_owned = phases[301]
    if stage not in PREPARATION_STAGES:
        raise BindingDiagnosticError(f"unknown preparation failure stage {stage}")
    result: dict[str, object] = {
        "stage": stage,
        "stage_meaning": PREPARATION_STAGES[stage],
        "stage_interpretation": "last attempted boundary",
        "last_published_used": used,
        "last_published_name_count": name_count,
        "live_owned_bytes": live_owned,
    }
    if 308 in phases:
        status, reserved_first, reserved_second, _ = phases[308]
        if reserved_first != 0 or reserved_second != 0:
            raise BindingDiagnosticError("phase 308 trailing words must be zero")
        result["last_writer_or_expression_status"] = status
        if stage not in (8, 9):
            result["last_writer_or_expression_status_note"] = (
                "may be stale for template and other stages"
            )
    if has_first:
        raw = b"".join(word.to_bytes(4, "big") for phase in (302, 303) for word in phases[phase][:3])
        extent = raw[0] + 1
        if extent < 4:
            result.update({
                "record_framing_valid": False,
                "captured_prefix_hex": raw.hex(),
            })
        else:
            prefix_length = min(extent, len(raw))
            result.update({
                "record_framing_valid": True,
                "actual_record_extent": extent,
                "record_prefix_hex": raw[:prefix_length].hex(),
                "truncated": extent > len(raw),
            })
    input_phases = {phase: phases[phase] for phase in range(304, 308) if phase in phases}
    if input_phases:
        if set(input_phases) != set(range(304, 308)):
            missing = sorted(set(range(304, 308)) - input_phases.keys())
            raise BindingDiagnosticError(
                "pre-call input snapshot requires phases 304-307; missing "
                + ", ".join(map(str, missing))
            )
        first = input_phases[304]
        if input_phases[307][1] != 0 or input_phases[307][2] != 0:
            raise BindingDiagnosticError("phase 307 trailing words must be zero")
        raw = b"".join(
            word.to_bytes(4, "big")
            for word in (
                first[2],
                *input_phases[305][:3],
                *input_phases[306][:3],
                input_phases[307][0],
            )
        )
        extent = raw[0] + 1
        if extent < 4:
            raise BindingDiagnosticError(f"pre-call record extent {extent} is below 4 bytes")
        result["pre_call_input"] = {
            "input_pointer": f"0x{first[0]:08x}",
            "prepared_pointer": f"0x{first[1]:08x}",
            "pointers_alias": first[0] == first[1],
            "live_owned_bytes": {str(phase): input_phases[phase][3] for phase in range(304, 308)},
            "actual_record_extent": extent,
            "record_prefix_hex": raw[:min(extent, 32)].hex(),
            "truncated": extent > 32,
        }
    return result


def _decode_snapshot(phases: dict[int, tuple[int, int, int, int]]) -> dict[str, object] | None:
    """Decode one normalized, complete optional snapshot block."""
    if not phases:
        return None
    required = set(range(32, 37)) | set(range(64, 86))
    missing = sorted(required - phases.keys())
    if missing:
        raise BindingDiagnosticError(
            "incomplete binding snapshot; missing phases " + ", ".join(map(str, missing))
        )

    stage, index, related, _ = phases[32]
    base_count, current, name_length, _ = phases[33]
    base, count = base_count >> 16, base_count & 0xFFFF
    if stage not in STAGES:
        raise BindingDiagnosticError(f"unknown failure stage {stage}")
    if name_length > 255:
        raise BindingDiagnosticError(f"name length {name_length} exceeds 255-byte bound")
    if index != 0xFFFFFFFF and index >= count:
        raise BindingDiagnosticError(f"entry index {index} is outside count {count}")
    if related != 0xFFFFFFFF and related >= count:
        raise BindingDiagnosticError(f"related entry index {related} is outside count {count}")
    if current != 0xFFFFFFFF and current > count:
        raise BindingDiagnosticError(f"current scope {current} is outside count {count}")

    if phases[36][1] != 0 or phases[36][2] != 0:
        raise BindingDiagnosticError("phase 36 trailing words must be zero")
    if phases[85][1] != 0 or phases[85][2] != 0:
        raise BindingDiagnosticError("phase 85 trailing words must be zero")

    raw_entry = b"".join(
        word.to_bytes(4, "big")
        for phase in (34, 35)
        for word in phases[phase][:3]
    ) + phases[36][0].to_bytes(4, "big")
    if raw_entry[26:28] != b"\0\0":
        raise BindingDiagnosticError("entry padding bytes 26-27 must be zero")
    if index == 0xFFFFFFFF and any(raw_entry):
        raise BindingDiagnosticError("entry bytes must be zero when entry index is absent")

    name_buffer = b"".join(
        word.to_bytes(4, "big")
        for phase in range(64, 85)
        for word in phases[phase][:3]
    ) + phases[85][0].to_bytes(4, "big")
    name = name_buffer[:name_length]
    if any(name_buffer[name_length:]):
        raise BindingDiagnosticError("name buffer has nonzero bytes after declared length")
    # JSON escapes control bytes on output; preserve the canonical byte sequence
    # as a reversible Latin-1 string, with name_hex available for consumers.
    decoded_name = name.decode("latin-1")

    snapshot: dict[str, object] = {
        "stage": stage,
        "stage_meaning": STAGES[stage],
        "entry_index": None if index == 0xFFFFFFFF else index,
        "related_entry_index": None if related == 0xFFFFFFFF else related,
        "base": base,
        "count": count,
        "current_scope": None if current == 0xFFFFFFFF else current,
        "name_length": name_length,
        "name": decoded_name,
        "name_hex": name.hex(),
        "entry_bytes": raw_entry.hex(),
        "name_buffer_bytes": len(name_buffer),
        "entry": _decode_entry(raw_entry) if index != 0xFFFFFFFF else None,
    }
    return snapshot


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("text_path", type=Path, help="captured guest stdout text")
    args = parser.parse_args()
    try:
        result = decode_binding_diagnostic(args.text_path.read_text(encoding="utf-8"))
    except (OSError, BindingDiagnosticError) as exc:
        parser.error(str(exc))
    print(json.dumps(result, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()

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
    for line in text.splitlines():
        match = PROGRESS.fullmatch(line.strip())
        if not match:
            continue
        phase, *values = (int(part, 16) for part in match.groups())
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

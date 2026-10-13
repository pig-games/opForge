"""Decode bounded native hotspot observations, independently of artifact proof.

Own samples measure inclusive and exclusive time only for selected invocations.
Timing windows also include child-only windows needed for parent subtraction.
Estimates scale own samples by calls/samples; fixed-period sampling can be biased.
Nested inclusive times and estimated exclusive times must not be added to measured
preparation stages or interpreted as measured full-runtime exclusive time.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import re


class HotspotTelemetryError(ValueError):
    """Malformed, partial or inconsistent native hotspot records."""


LABELS = (
    "CAPTURE_LINE", "CANDIDATE_ACQUISITION", "CAPTURE_CONSTRUCTION",
    "CAPTURE_APPEND", "BLOCK_GROW", "REPLAY_LINE", "CAPTURE_PREVIEW",
    "PACKED_WRITE", "NAME_BIND", "SCOPE_BIND", "IMPORT_RESOLVE",
    "EXPRESSION_COMPILE", "RECORD_PROCESS", "PLAN_BIND", "TOKENIZE",
    "SCOPE_LOOKUP",
    "SOURCE_CONTROL", "DECLARATION", "CONDITIONAL", "METADATA",
    "CONFIGURATION", "OPERAND_BIND", "SCOPE_LINE", "VALUE_CAPTURE",
    "FILE_LINE", "DATA_PREPARE", "EXPRESSION_PREPARE", "PREPARED_COPY",
    "GRAPH_LINE", "SCOPE_COUNT", "LOOKUP_HASH", "LOOKUP_CHAIN",
)
WORK_COUNTERS = (
    "CAPTURE_EMPTY_RECORDS", "CAPTURE_EMPTY_BYTES", "REPLAY_EMPTY_RECORDS",
    "PROCESS_EMPTY_INPUTS", "PROCESS_EMPTY_OUTPUTS", "SOURCE_CONSUMED",
    "CONDITION_CONSUMED", "LOOKUP_HASH_BYTES", "LOOKUP_CHAIN_PROBES",
    "LOOKUP_NAME_COMPARES", "LOOKUP_COMPARE_BYTES", "LOOKUP_HITS",
    "LOOKUP_MISSES", "LOOKUP_NAMESPACE_REJECTS", "LOOKUP_LENGTH_REJECTS",
    "PROCESS_RECORDS",
)
_LINE = re.compile(
    r"^progress p=([0-9a-fA-F]{8}) f=([0-9a-fA-F]{8}) "
    r"l=([0-9a-fA-F]{8}) r=([0-9a-fA-F]{8}) m=([0-9a-fA-F]{8})$"
)
_PHASES = {500, 501, 502} | set(range(512, 624))


def expected_samples(calls: int, hotspot_id: int, period: int, seed: int) -> int:
    """Count selected calls without iterating or overflowing native counters."""
    first = (-(17 * hotspot_id + seed)) & (period - 1)
    return 0 if calls <= first else 1 + (calls - first - 1) // period


def decode_hotspot_telemetry(text: str, *, terminal_complete: bool = True) -> dict:
    rows = {}
    for number, line in enumerate(text.splitlines(), 1):
        if not line.strip().lower().startswith("progress p="):
            continue
        match = _LINE.fullmatch(line.strip())
        if match is None:
            raise HotspotTelemetryError(f"malformed progress row on line {number}")
        phase, f, l, r, _memory = (int(word, 16) for word in match.groups())
        if phase not in _PHASES:
            continue
        if phase in rows:
            raise HotspotTelemetryError(f"duplicate hotspot telemetry phase 0x{phase:x}")
        rows[phase] = (f, l, r)
    if not rows:
        return {"hotspot_summary": None}
    missing = sorted(_PHASES - rows.keys())
    if missing:
        raise HotspotTelemetryError("incomplete hotspot telemetry; missing phases: "
                                    + ", ".join(f"0x{phase:x}" for phase in missing))
    version, errors, period = rows[500]
    frequency, max_depth, active_depth = rows[501]
    capacity, seed, stopped = rows[502]
    if version != 1:
        raise HotspotTelemetryError(f"unsupported hotspot telemetry version {version}")
    if errors:
        raise HotspotTelemetryError(f"hotspot telemetry reports error bits 0x{errors:x}")
    if period not in (64, 128, 256):
        raise HotspotTelemetryError("hotspot sampling period must be 64, 128 or 256")
    if not frequency:
        raise HotspotTelemetryError("hotspot E-clock frequency must be positive")
    if capacity != 32 or max_depth > capacity or active_depth or stopped:
        raise HotspotTelemetryError("invalid hotspot stack capacity, depth or stopped state")
    hotspots = []
    for hotspot_id, label in enumerate(LABELS):
        base = 512 + 3 * hotspot_id
        reported_id, calls, samples = rows[base]
        windows, inclusive_hi, inclusive_lo = rows[base + 1]
        exclusive_hi, exclusive_lo, reserved = rows[base + 2]
        inclusive = (inclusive_hi << 32) | inclusive_lo
        exclusive = (exclusive_hi << 32) | exclusive_lo
        if reported_id != hotspot_id:
            raise HotspotTelemetryError(f"hotspot ID differs at phase 0x{base:x}")
        if reserved:
            raise HotspotTelemetryError(f"hotspot {label} reserved field must be zero")
        if not samples <= windows <= calls:
            raise HotspotTelemetryError(f"hotspot {label} samples/windows/calls are inconsistent")
        if terminal_complete and samples != expected_samples(calls, hotspot_id, period, seed):
            raise HotspotTelemetryError(f"hotspot {label} own samples disagree with sampling phase")
        if exclusive > inclusive or (not samples and inclusive):
            raise HotspotTelemetryError(f"hotspot {label} inclusive/exclusive own-sample ticks are inconsistent")
        sample_inclusive = inclusive / frequency
        sample_exclusive = exclusive / frequency
        scale = calls / samples if samples else None
        hotspots.append({
            "id": hotspot_id, "label": label, "calls": calls,
            "own_samples": samples, "timing_windows": windows,
            "sample_inclusive_ticks": inclusive, "sample_exclusive_ticks": exclusive,
            "sample_inclusive_seconds": sample_inclusive,
            "sample_exclusive_seconds": sample_exclusive,
            "estimated_total_inclusive_seconds": sample_inclusive * scale if scale is not None else None,
            "estimated_total_exclusive_seconds": sample_exclusive * scale if scale is not None else None,
        })
    work_counters = {}
    for counter_id, label in enumerate(WORK_COUNTERS):
        reported_id, count, reserved = rows[608 + counter_id]
        if reported_id != counter_id:
            raise HotspotTelemetryError(f"work counter ID differs at phase 0x{608 + counter_id:x}")
        if reserved:
            raise HotspotTelemetryError(f"work counter {label} reserved field must be zero")
        work_counters[label] = count
    def hottest(field):
        return [row["label"] for row in sorted(
            hotspots, key=lambda row: (-(row[field] or 0), row["id"]))]
    return {"hotspot_summary": {
        "version": version, "sampling_period": period, "phase_seed": seed,
        "eclock_hz": frequency, "stack_capacity": capacity,
        "maximum_depth": max_depth, "active_depth": active_depth, "stopped": stopped,
        "terminal_sample_counts_validated": terminal_complete, "hotspots": hotspots,
        "work_counters": work_counters,
        "hottest_exclusive": hottest("estimated_total_exclusive_seconds"),
        "hottest_inclusive": hottest("estimated_total_inclusive_seconds"),
        "interpretation": (
            "Sample seconds measure only own selected invocations; estimated totals scale by calls/own_samples. "
            "Fixed-period sampling can be biased. Inclusive times overlap across nested scopes. "
            "Do not add inclusive times or estimated exclusive totals to measured preparation stages; "
            "estimated exclusive totals are not measured full-runtime exclusive time."
        ),
    }}


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("file", type=Path, help="guest stdout text file")
    args = parser.parse_args(argv)
    try:
        result = decode_hotspot_telemetry(args.file.read_text())
    except (OSError, HotspotTelemetryError) as error:
        parser.error(str(error))
    print(json.dumps(result, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

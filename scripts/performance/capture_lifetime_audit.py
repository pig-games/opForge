"""Validate passive capture lifetime and hypothetical token-budget observations."""
import re

PREFIX = re.compile(r"^progress p=([0-9a-fA-F]+)\b")
ROW = re.compile(r"^progress p=([0-9a-fA-F]{8}) f=([0-9a-fA-F]{8}) l=([0-9a-fA-F]{8}) r=([0-9a-fA-F]{8}) m=([0-9a-fA-F]{8})$")
FIXED = set(range(600000, 600006)) | set(range(600010, 600014)) | set(range(600020, 600028)) | set(range(600100, 600120))
PEAK_FIELDS = ("event", "phase", "heap", "capture", "eligible", "span")
CURVE_FIELDS = ("span", "event", "phase", "heap", "capture", "eligible", "model_capacity", "pins", "regions")
REGION_FIELDS = ("index", "file", "records", "tokens", "original_used", "compact_used", "actual_capacity", "fallback_records", "fallback_tokens")
PHASES = ("startup", "acquisition", "configuration_order", "replay_initialization", "semantic_replay",
          "capture_release", "postreplay_finalization", "assembly", "output", "cleanup")


def require(condition, detail):
    if not condition:
        raise ValueError(f"Invalid capture lifetime {detail}")


def compact_capacity(used):
    return max(256, 1 << (used - 1).bit_length()) if used else 0


def decode(stdout, memory, audit, capture):
    """Require a complete positive-run report and reconcile independent observers.

    ``capture`` is the existing decoded capture_summary, not a second inventory.
    Native completion and Rust parity remain the caller's independent obligations.
    """
    rows = {}
    for line in stdout.splitlines():
        prefix = PREFIX.match(line)
        if prefix is None or not 600000 <= int(prefix[1], 16) < 620000:
            continue
        match = ROW.fullmatch(line)
        require(match is not None, "malformed progress row")
        phase, *words = (int(value, 16) for value in match.groups())
        require(phase not in rows, f"duplicate phase {phase}")
        require(phase in FIXED or 601000 <= phase < 601768 or 602000 <= phase < 602768,
                f"unexpected phase {phase}")
        rows[phase] = words[:3]
    require(FIXED <= rows.keys(), f"missing fixed phases {sorted(FIXED - rows.keys())}")
    schema, errors, mode = rows[600000]
    state_bytes, phase_count, curve_limit = rows[600001]
    heap_live, heap_peak, capture_live = rows[600002]
    capture_peak, allocated, freed = rows[600003]
    alloc_calls, free_calls, curve_count = rows[600004]
    event, phase, region_count = rows[600005]
    records, tokens, original = rows[600020]
    compact, fallback_records, fallback_tokens = rows[600021]
    header_bytes, lexeme_bytes, plan_bytes = rows[600022]
    maxima = dict(zip(("count", "source_bytes", "lexeme_bytes", "kind", "reserved", "start", "end", "offset", "length"),
                      rows[600023] + rows[600024] + rows[600025]))
    repeated_regions, actual_capacity, compact_cap = rows[600026]
    header_width, wide_stride, compact_stride = rows[600027]
    require(schema == 1 and not (errors or mode or heap_live or capture_live), "schema/errors/mode/terminal live ownership")
    require(phase_count == 10 and curve_limit == 256 and region_count <= 256 and curve_count <= 256
            and repeated_regions == region_count and phase < phase_count, "phase/region/curve bounds")
    require(state_bytes >= 18868 and state_bytes % 4 == 0, "state size/alignment")
    require(allocated - freed == heap_live and event == alloc_calls + free_calls
            and heap_live <= heap_peak <= allocated and capture_live <= capture_peak <= heap_peak,
            "heap allocation/event conservation")
    require(wide_stride == 20 and compact_stride == 12, "token strides")
    work = audit.get("work_attribution", {})
    require(audit.get("observer_complete") and work.get("accounting_complete")
            and work.get("template_inventory", {}).get("complete"), "incomplete existing L1/work/template report")
    require(not memory.get("diagnostics"), "existing memory diagnostics")
    require(work["capture_header_bytes"] == 60 and (header_width == 60 or (records == 0 and header_width == 0)),
            "record header width")
    require(header_bytes == records * header_width and compact == original - 8 * (tokens - fallback_tokens)
            and compact <= original and fallback_records <= records and fallback_records <= fallback_tokens <= tokens
            and fallback_tokens <= fallback_records * 64 and bool(fallback_records) == bool(fallback_tokens),
            "record/token/compact budget conservation")
    require(maxima["count"] <= 64 and maxima["reserved"] <= 65535 and maxima["kind"] <= 65535
            and tokens <= records * maxima["count"] and maxima["lexeme_bytes"] <= lexeme_bytes,
            "token maxima bounds")
    narrow = maxima["kind"] <= 255 and all(maxima[field] <= 65535 for field in ("start", "end", "offset", "length"))
    require(bool(fallback_records) == (not narrow), "fallback/maxima disagreement")
    require(records != 0 or not any(maxima.values()), "empty-record maxima")
    require(tokens != 0 or not any(maxima[field] for field in ("kind", "reserved", "start", "end", "offset", "length")),
            "empty-token maxima")

    expected = FIXED | set(range(601000, 601000 + curve_count * 3)) | set(range(602000, 602000 + region_count * 3))
    require(rows.keys() == expected, f"missing or unexpected detail phases {sorted(rows.keys() ^ expected)}")

    def vector(start, fields):
        return dict(zip(fields, [word for offset in range(len(fields) // 3) for word in rows[start + offset]]))

    def peak(start, expected_phase=None):
        result = vector(start, PEAK_FIELDS)
        require(result["event"] <= event and result["phase"] < phase_count and result["span"] <= curve_count
                and result["capture"] <= result["heap"] <= heap_peak and result["capture"] <= capture_peak
                and result["eligible"] <= actual_capacity, "peak bounds")
        if result["event"] == 0:
            require(not any(result.values()), "empty peak row")
        else:
            require(result["heap"] > 0 and (expected_phase is None or result["phase"] == expected_phase), "peak phase/heap")
        result["instantaneous_eligible_upper_bound"] = min(result["eligible"], result["capture"])
        return result

    global_peak, retained_peak = peak(600010), peak(600012)
    phase_peaks = {name: peak(600100 + index * 2, index) for index, name in enumerate(PHASES)}
    require(global_peak["heap"] == heap_peak and retained_peak["capture"] == capture_peak
            and max(row["heap"] for row in phase_peaks.values()) == heap_peak, "global/phase peak reconciliation")
    require(not heap_peak or all(global_peak[field] == phase_peaks[PHASES[global_peak["phase"]]][field]
                                 for field in PEAK_FIELDS), "global peak phase snapshot")
    curves, regions = [], []
    last_event = last_eligible = last_regions = 0
    for index in range(curve_count):
        row = vector(601000 + index * 3, CURVE_FIELDS)
        require(row["span"] == index + 1 and last_event <= row["event"] <= event
                and row["phase"] < phase_count and row["capture"] <= row["heap"] <= heap_peak
                and row["capture"] <= capture_peak and row["capture"] == row["model_capacity"] <= actual_capacity
                and last_eligible <= row["eligible"] <= row["model_capacity"]
                and last_regions <= row["regions"] <= region_count
                and row["pins"] <= audit["max_parent_pins"], "curve bounds/order")
        last_event, last_eligible, last_regions = row["event"], row["eligible"], row["regions"]
        curves.append(row)
    for index in range(region_count):
        row = vector(602000 + index * 3, REGION_FIELDS)
        require(row["index"] == index and row["original_used"] <= row["actual_capacity"]
                and row["compact_used"] == row["original_used"] - 8 * (row["tokens"] - row["fallback_tokens"])
                and row["fallback_records"] <= row["records"] and row["fallback_records"] <= row["fallback_tokens"] <= row["tokens"]
                and row["tokens"] <= row["records"] * 64 and row["fallback_tokens"] <= row["fallback_records"] * 64
                and bool(row["fallback_records"]) == bool(row["fallback_tokens"]), "region composition/bounds")
        row["compact_capacity"] = compact_capacity(row["compact_used"])
        require(0 <= row["compact_used"] <= row["original_used"]
                and row["compact_capacity"] <= row["actual_capacity"], "region hypothetical capacity")
        regions.append(row)
    for row in [global_peak, retained_peak, *phase_peaks.values()]:
        prior = curves[row["span"] - 1] if row["span"] else None
        require(row["eligible"] == (prior["eligible"] if prior else 0)
                and (prior is None or row["event"] >= prior["event"])
                and (row["span"] == curve_count or row["event"] <= curves[row["span"]]["event"]),
                "peak span/eligibility chronology")
    for field, total in (("records", records), ("tokens", tokens), ("original_used", original), ("compact_used", compact),
                         ("fallback_records", fallback_records), ("fallback_tokens", fallback_tokens),
                         ("actual_capacity", actual_capacity), ("compact_capacity", compact_cap)):
        require(sum(row[field] for row in regions) == total, f"region sum {field}")
    require(actual_capacity <= capture_peak, "retained capacity/peak")
    for field, value in (("live_owned_bytes", heap_live), ("peak_owned_bytes", heap_peak),
                         ("total_allocated_bytes", allocated), ("total_freed_bytes", freed)):
        require(memory[field] == value, f"MEMD {field} conservation")
    require(capture is not None and capture["version"] == 1 and capture["boundary_mask"] == 7, "existing capture boundaries")
    counters, stages = capture["counters"], capture["stages"]
    require(maxima["source_bytes"] <= counters["construction"]["retained_source_bytes"], "source byte maximum")
    for field, value in (("records", records), ("header_bytes", header_bytes), ("token_bytes", tokens * 20)):
        require(counters["retained_composition"][field] == value, f"capture composition {field}")
    for field, value in (("lexeme_bytes", lexeme_bytes), ("plan_bytes", plan_bytes)):
        require(counters["preparation"][field] == value, f"capture preparation {field}")
    require(original == header_bytes + tokens * 20 + lexeme_bytes + plan_bytes + counters["preparation"]["padding_bytes"]
            == counters["construction_copy"]["retained_append_copy_bytes"], "capture payload composition")
    require(counters["retained_growth"]["retained_peak_capacity_including_overlap"] == capture_peak, "capture growth peak")
    require(stages["replay"]["retained_used"] == original and stages["replay"]["retained_capacity"] == actual_capacity
            and stages["replay"]["retained_region_count"] == region_count
            and stages["release"]["retained_used"] == stages["release"]["retained_capacity"] == 0, "capture lifecycle snapshots")
    require(audit["capture_used_bytes"] == original and audit["capture_capacity_bytes"] == actual_capacity
            and audit["regions"] == region_count and audit["completed_spans"] == curve_count
            and audit["scheduled_spans"] == curve_count and audit["potentially_retireable_capacity_bytes"] == last_eligible,
            "L1 aggregate conservation")
    for detail in audit.get("details", {}).get("residency", []):
        span, _used, capacity, eligible, predicted, reserved = detail
        require(0 < span <= curve_count and not reserved and curves[span - 1]["model_capacity"] == capacity
                and curves[span - 1]["eligible"] == eligible and predicted == capacity - eligible, "L1 residency curve")
    for index, detail in enumerate(audit.get("details", {}).get("regions", [])):
        _base, _end, file, used, capacity, *_rest = detail
        require(index < region_count and regions[index]["file"] == file
                and regions[index]["original_used"] == used and regions[index]["actual_capacity"] == capacity,
                "L1 region detail")
    total = work["metrics"]["total"]
    for field, value in (("capture_bytes", original), ("capture_header_bytes", header_bytes),
                         ("capture_token_bytes", tokens * 20), ("capture_lexeme_bytes", lexeme_bytes),
                         ("capture_plan_bytes", plan_bytes), ("capture_padding_bytes", counters["preparation"]["padding_bytes"])):
        require(total[field] == value, f"work {field} conservation")
    return {"schema": schema, "complete": True, "observer_state_bytes": state_bytes,
            "phase": phase, "phase_name": PHASES[phase], "events": event,
            "heap_live": heap_live, "heap_peak": heap_peak, "capture_live": capture_live, "capture_peak": capture_peak,
            "allocated": allocated, "freed": freed, "allocation_calls": alloc_calls, "free_calls": free_calls,
            "records": records, "tokens": tokens, "original_bytes": original, "compact_bytes": compact,
            "fallback_records": fallback_records, "fallback_tokens": fallback_tokens,
            "header_bytes": header_bytes, "lexeme_bytes": lexeme_bytes, "plan_bytes": plan_bytes,
            "record_header_width": header_width, "wide_stride": wide_stride, "compact_stride": compact_stride,
            "token_maxima": maxima, "actual_capacity": actual_capacity, "compact_capacity": compact_cap,
            "global_peak": global_peak, "retained_peak": retained_peak, "phase_peaks": phase_peaks,
            "curve_count": curve_count, "region_count": region_count, "curves": curves, "regions": regions,
            "evidence_limit": "Passive observation of the existing heap domain including growth overlap. Eligibility remains a historical L1 shadow classification; deferred recipes can retain capture offsets, so it does not establish retirement safety. Eligible bytes at a peak are capped by live capture only as an instantaneous upper bound; they do not predict a later peak. Compact token bytes preserve Reserved.u16 and use whole-record fallback. Compact capacity preserves current region partitions and assumes power-of-two growth from 256; conversion/selector costs, changed growth sequences and repartition are unresolved. Observation implements no compaction or retirement and establishes no heap or runtime saving."}

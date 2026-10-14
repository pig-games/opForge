"""Decode the current L1 shadow report; it never establishes guest completion.

Coverage errors, deferred generated bindings and predicted capture residency are
separate from native parity and measured process memory. Detail printing is capped;
aggregate counts cover the complete bounded observer tables.
"""


def decode(stdout):
    rows = {}
    for line in stdout.splitlines():
        if not line.startswith("progress p="):
            continue
        fields = {}
        for item in line.split()[1:]:
            key, value = item.split("=", 1)
            if key in fields:
                raise ValueError(f"Duplicate L1 progress field {key}")
            fields[key] = int(value, 16)
            if not 0 <= fields[key] <= 0xffffffff:
                raise ValueError("L1 progress word outside u32 bounds")
        if not {"p", "f", "l", "r"} <= fields.keys():
            raise ValueError("Incomplete L1 progress fields")
        phase = fields["p"]
        if 700 <= phase <= 2466:
            if phase in rows:
                raise ValueError(f"Duplicate L1 report phase {phase}")
            rows[phase] = [fields["f"], fields["l"], fields["r"]]
    required = set(range(700, 709)) | set(range(710, 715))
    missing = required - rows.keys()
    if missing:
        raise ValueError(f"Missing L1 report phases: {sorted(missing)}")
    if rows[700][0] != 1:
        raise ValueError("Unsupported L1 report schema")
    main_bytes, region_limit, regions = rows[701]
    units, maps, used = rows[705]
    capacity, classified, unavailable = rows[706]
    detail_limit, unit_limit, map_limit = rows[708]
    matched, deferred, unmatched = rows[710]
    unit_errors, events, depth = rows[711]
    unit_bytes, event_limit, event_detail_limit = rows[712]
    eligible, peak_eligible, predicted = rows[713]
    eligible_regions, retained_regions, late = rows[714]
    consistent = (regions <= region_limit and units <= unit_limit and maps <= map_limit
                  and events <= event_limit and matched + deferred + unmatched == units
                  and used <= capacity and eligible + predicted == capacity
                  and eligible <= peak_eligible and eligible_regions + retained_regions == regions
                  and rows[707][2] == eligible)
    if not consistent:
        raise ValueError("Inconsistent L1 aggregate bounds/accounting")
    details = {}
    groups = (("regions", 720, 3, min(regions, detail_limit)),
              ("units", 1120, 2, min(units, detail_limit)),
              ("mappings", 1400, 2, min(maps, detail_limit)),
              ("publications", 1700, 5, min(events, event_detail_limit)),
              ("residency", 1800, 2, min(maps, detail_limit)))
    for name, first, stride, count in groups:
        details[name] = []
        for index in range(count):
            try:
                values = [value for part in range(stride) for value in rows[first + stride * index + part]]
            except KeyError as error:
                raise ValueError(f"Missing L1 {name} detail row") from error
            details[name].append(values)
    errors = rows[700][1]
    boundary_mask = rows[700][2]
    scheduled, completed, includes = rows[703]
    pins, max_pins, lookups = rows[704]
    observer_complete = boundary_mask == 15 and not (errors or unit_errors or depth or late or pins) and scheduled == completed
    result = {"schema": 1, "observer_complete": observer_complete,
            "ordinary_unit_reconciliation_complete": observer_complete and unmatched == 0,
            "main_errors": errors, "unit_errors": unit_errors, "late_accesses": late,
            "observed_boundaries_mask": boundary_mask, "required_boundaries_mask": 15,
            "observer_static_table_bytes": main_bytes + unit_bytes,
            "regions": regions, "units": units, "publication_events": events,
            "matched_ordinary_units": matched, "deferred_generated_units": deferred,
            "unmatched_units": unmatched, "scheduled_spans": scheduled, "completed_spans": completed,
            "include_acquisitions": includes, "active_parent_pins": pins, "max_parent_pins": max_pins,
            "capture_lookups": lookups, "physical_records": rows[702][0], "published_names": rows[702][2],
            "capture_used_bytes": used, "capture_capacity_bytes": capacity,
            "shared_head_classifications": classified, "unavailable_classifications": unavailable,
            "potentially_retireable_capacity_bytes": eligible,
            "peak_potentially_retireable_capacity_bytes": peak_eligible,
            "predicted_remaining_capacity_bytes": predicted,
            "eligible_regions": eligible_regions, "retained_regions": retained_regions,
            "detail_limits": {"ordinary": detail_limit, "publications": event_detail_limit},
            "details": details,
            "evidence_limit": "No capture was retired or preparation skipped; residency is a prediction. Per-member/proxy/schema links and acquisition-only unit inventory remain deferred."}

    if 2000 in rows:
        result["work_attribution"] = decode_work(rows, observer_complete)
    return result


WORK_METRICS = ("bind_calls", "compile_calls", "captured_plan_binds", "process_calls",
                "bound_plan_bytes", "template_bytes", "prepared_copy_bytes", "replay_copy_bytes",
                "capture_header_bytes", "capture_token_bytes", "capture_lexeme_bytes",
                "capture_plan_bytes", "capture_padding_bytes", "capture_bytes", "candidate_calls",
                "descriptor_calls", "fragment_calls", "plan_create_calls")


def decode_work(rows, observer_complete):
    def required(phase):
        if phase not in rows:
            raise ValueError(f"Missing work attribution phase {phase}")
        return rows[phase]

    version, errors, count = required(2000)
    if version != 1:
        raise ValueError("Unsupported work attribution schema")
    static_bytes, limit, reported = required(2001)
    header_bytes, audit_header_bytes, reasons = required(2002)
    capacity_bytes, capture_bytes, observer_header_bytes = required(2003)
    if limit != 16 or reported != min(count, limit) or header_bytes != 60 or audit_header_bytes != 4:
        raise ValueError("Inconsistent work attribution bounds/header contract")
    vectors = {}
    for name, start in (("total", 2010), ("selected", 2020), ("ultimately_unreferenced", 2030),
                        ("shared_required", 2040), ("unknown", 2050)):
        values = [value for phase in range(start, start + 6) for value in required(phase)]
        vectors[name] = dict(zip(WORK_METRICS, values))
    for metric in WORK_METRICS:
        if vectors["total"][metric] != sum(vectors[name][metric] for name in vectors if name != "total"):
            raise ValueError(f"Inconsistent work attribution conservation: {metric}")
    if capture_bytes != vectors["total"]["capture_bytes"] or capture_bytes > capacity_bytes:
        raise ValueError("Inconsistent capture used/capacity attribution")
    if vectors["total"]["capture_header_bytes"] % header_bytes or capture_bytes != sum(
            vectors["total"][metric] for metric in WORK_METRICS[8:13]):
        raise ValueError("Inconsistent capture component attribution")
    if observer_header_bytes != vectors["total"]["capture_header_bytes"] // header_bytes * audit_header_bytes:
        raise ValueError("Inconsistent audit-only capture header overhead")
    details = []
    seen = set()
    for index in range(reported):
        start = 2060 + index * 7
        event, unit, reason = required(start)
        if event == 0 or event in seen:
            raise ValueError("Invalid or duplicate work attribution event")
        seen.add(event)
        values = [value for phase in range(start + 1, start + 7) for value in required(phase)]
        details.append({"event": event, "unit": unit, "reasons": reason,
                        "exclusive": dict(zip(WORK_METRICS, values))})
    for metric in WORK_METRICS:
        if sum(row["exclusive"][metric] for row in details) > vectors["total"][metric]:
            raise ValueError(f"Inconsistent work detail conservation: {metric}")
    result = {"schema": version, "errors": errors, "row_count": count,
            "coverage_complete": observer_complete and errors == 0 and reasons == 0
                                 and not any(vectors["unknown"].values()),
            "accounting_complete": observer_complete and errors == 0,
            "unknown_reason_mask": reasons, "observer_static_bytes": static_bytes,
            "detail_limit": limit, "reported_rows": reported, "detail_truncated": count > reported,
            "capture_header_bytes": header_bytes, "audit_header_bytes": audit_header_bytes,
            "capture_capacity_bytes": capacity_bytes, "capture_used_bytes": capture_bytes,
            "observer_capture_header_bytes": observer_header_bytes,
            "metrics": vectors, "details": details,
            "evidence_limit": "Observed preparation calls and retained used bytes; no avoided work, memory savings or native completion proof."}

    if any(2200 <= phase <= 2281 for phase in rows):
        extension, top_count, provenance_bytes = required(2200)
        if (extension, top_count, provenance_bytes) != (1, 3, 36):
            raise ValueError("Unsupported unknown ownership extension contract")
        def vector(start):
            return dict(zip(WORK_METRICS, (value for phase in range(start, start + 6)
                                          for value in required(phase))))
        unit_unknown, residual = vector(2210), vector(2220)
        for metric in WORK_METRICS:
            if unit_unknown[metric] + residual[metric] != vectors["unknown"][metric]:
                raise ValueError(f"Inconsistent unknown ownership conservation: {metric}")
        if errors and any(unit_unknown.values()):
            raise ValueError("Invalid observer cannot claim unit unknown ownership")
        causes = required(2230) + required(2231)
        if causes[-1] != 0:
            raise ValueError("Nonzero reserved unknown ownership word")
        contributors = []
        for index, offset in enumerate((52, 8, 16)):
            start = 2240 + index * 9
            metric_offset, event, unit = required(start)
            reason, opened, closed = required(start + 1)
            origin, line, original_open = required(start + 2)
            values = vector(start + 3)
            if metric_offset != offset or any(values[m] > unit_unknown[m] for m in WORK_METRICS):
                raise ValueError("Inconsistent largest unknown contributor")
            if event == 0 and (unit or reason or opened or closed or origin or line or original_open
                               or any(values.values())):
                raise ValueError("Absent unknown contributor has nonzero provenance")
            if event and (not reason or event > count):
                raise ValueError("Invalid unknown contributor event/reasons")
            metric = WORK_METRICS[offset // 4]
            if not errors and count == reported:
                candidates = [row for row in details if row["reasons"] and row["exclusive"][metric]]
                winner = max(candidates, key=lambda row: (row["exclusive"][metric], -row["event"]), default=None)
                if (winner is None and event != 0) or (winner is not None and
                        (event != winner["event"] or unit != winner["unit"] or reason != winner["reasons"]
                         or values != dict(winner["exclusive"], template_bytes=0))):
                    raise ValueError("Largest unknown contributor does not match complete details")
            contributors.append({"metric": metric, "event": event, "unit": unit, "reasons": reason,
                                 "capture_open": opened, "capture_close": closed, "origin": origin,
                                 "line": line, "original_open": original_open, "exclusive": values})
        if not errors and count == reported:
            for metric in WORK_METRICS:
                expected = sum(row["exclusive"][metric] for row in details if row["reasons"] and metric != "template_bytes")
                if unit_unknown[metric] != expected:
                    raise ValueError(f"Unknown unit ownership does not match complete details: {metric}")
        # Region cuts describe capture segmentation; they need not invalidate ownership.
        examples = []
        for index in range(4):
            start = 2270 + index * 3
            event, unit, cause = required(start)
            opened, closed, origin = required(start + 1)
            line, base, end = required(start + 2)
            if (event and cause != index + 1) or event > count or (event == 0 and any((unit, cause, opened, closed, origin, line, base, end))):
                raise ValueError("Invalid unknown range example provenance")
            if bool(event) != bool(causes[index]):
                raise ValueError("Range example disagrees with observation count")
            if event and not (closed < opened, closed >= end, True, opened < base)[index]:
                raise ValueError("Range example does not demonstrate its observation")
            examples.append({"event": event, "unit": unit, "cause": cause, "open": opened,
                             "close": closed, "origin": origin, "line": line,
                             "region_base": base, "region_end": end})
        context_values = [value for phase in range(2282, 2286) for value in required(phase)]
        if sum(context_values[::2]) != vectors["total"]["captured_plan_binds"] or sum(context_values[1::2]) != vectors["total"]["bound_plan_bytes"]:
            raise ValueError("Inconsistent bind context conservation")
        contexts = {name: {"calls": context_values[index * 2], "bound_bytes": context_values[index * 2 + 1]}
                    for index, name in enumerate(("without_unit_definition", "without_unit_expansion", "without_unit_ordinary",
                                                  "with_unit_definition", "with_unit_expansion", "with_unit_ordinary"))}
        if any(2300 <= phase <= 2466 for phase in rows):
            for slot, provenance in enumerate(contributors + examples):
                start = 2300 + slot * 24
                origin, length, ordinal = required(start)
                if origin != provenance["origin"] or ordinal != slot + 1 or length >= 256 or (not origin and length):
                    raise ValueError("Inconsistent source path provenance")
                chunks = (length + 11) // 12
                if any(start + 1 + chunks <= phase < start + 24 for phase in rows):
                    raise ValueError("Unexpected source path chunk")
                data = b"".join(word.to_bytes(4, "big") for phase in range(start + 1, start + 1 + chunks)
                                for word in required(phase))
                if any(data[length:]):
                    raise ValueError("Nonzero source path padding")
                try:
                    provenance["source_path"] = data[:length].decode("utf-8", errors="strict")
                except UnicodeDecodeError as error:
                    raise ValueError("Invalid UTF-8 source path") from error
        result["unknown_ownership"] = {"schema": extension, "reason_marked_units": unit_unknown,
            "global_or_unassigned_residual": residual,
            "range_observations": dict(zip(("close_before_open", "close_at_or_beyond_region_end",
                "missing_original_end", "open_before_region_base", "bad_parent"), causes[:-1])),
            "largest_contributors": contributors, "range_examples": examples, "bind_contexts": contexts,
            "bind_context_evidence_limit": "Contexts observe whether a unit owner exists at call time; preceding-entry transfer can change final ownership. Context totals are independent of the final unknown ownership split and do not describe conditional activity or reachability."}
    return result

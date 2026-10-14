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
        if 700 <= phase <= 1831:
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
    return {"schema": 1, "observer_complete": observer_complete,
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

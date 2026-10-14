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
        if (700 <= phase <= 2466 or 3000 <= phase <= 3014
                or 3100 <= phase < 19484 or 20000 <= phase < 32288
                or 34999 <= phase < 182456):
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
    elif any(phase >= 3000 for phase in rows):
        raise ValueError("Template inventory requires work attribution")
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
    if any(phase >= 3000 for phase in rows):
        result["template_inventory"] = decode_template_inventory(rows, result)
    return result


TEMPLATE_FIELDS = ("epoch", "handle", "pool", "kind", "name", "origin", "line",
                   "header_plan", "first", "last", "definition_bytes", "default_bytes",
                   "text_bytes", "closed", "calls", "nested_calls", "default_calls",
                   "first_caller", "first_origin", "first_line", "plan_binds",
                   "committed_bound_bytes", "default_slots", "reserved")
TEMPLATE_ERRORS = ((1, "overflow"), (2, "capacity"), (4, "invalid"),
                   (8, "unclosed"), (16, "unfinished"), (32, "reconciliation"))


def decode_template_inventory(rows, work):
    """Decode scalar inventory evidence; accepted queue calls establish usage.

    Binding deltas describe eager work while a published body is open. They are
    not unique retained plan payload, and all plan arena bytes remain residual.
    """
    def required(phase):
        if phase not in rows:
            raise ValueError(f"Missing template inventory phase {phase}")
        return rows[phase]

    schema, errors, epoch = required(3000)
    static_bytes, definition_limit, edge_limit = required(3001)
    count, edge_count, finished = required(3002)
    for phase in range(3003, 3015):
        required(phase)
    if schema != 1 or definition_limit != 2048 or edge_limit != 4096:
        raise ValueError("Unsupported template inventory schema/bounds")
    if (errors & ~63 or count > definition_limit or edge_count > edge_limit or required(3014)[2]
            or static_bytes == 0 or static_bytes % 4 or finished > epoch or required(3014)[1] > 4):
        raise ValueError("Invalid template inventory bounds/reserved/errors")
    expected_phases = set(range(3000, 3015)) | set(range(3100, 3100 + count * 8)) | set(range(20000, 20000 + edge_count * 3))
    if any(3000 <= phase < 34999 and phase not in expected_phases for phase in rows):
        raise ValueError("Unexpected template inventory detail phase")
    definitions, identities = [], set()
    for index in range(count):
        values = [value for phase in range(3100 + index * 8, 3108 + index * 8) for value in required(phase)]
        row = dict(zip(TEMPLATE_FIELDS, values), row=index + 1)
        identity = (row["epoch"], row["pool"], row["handle"])
        if (not 0 < row["epoch"] <= epoch or not row["handle"] or identity in identities
                or row["kind"] not in (0, 1, 2) or row["pool"] != (1 if row["kind"] == 2 else 0)
                or row["closed"] not in (0, 1) or row["reserved"] or row["first_caller"] > count):
            raise ValueError("Invalid or duplicate template definition")
        if row["closed"] and row["last"] < row["first"]:
            raise ValueError("Invalid template body offsets")
        identities.add(identity)
        row["body_bytes"] = row["last"] - row["first"] if row["closed"] else 0
        row["payload_bytes"] = sum(row[field] for field in ("definition_bytes", "default_bytes", "text_bytes", "body_bytes"))
        definitions.append(row)
    edges, relationships = [], set()
    for index in range(edge_count):
        caller, callee, origin = required(20000 + index * 3)
        line, calls, defaults = required(20001 + index * 3)
        slots, reserved1, reserved2 = required(20002 + index * 3)
        key = caller, callee, origin
        if (caller > count or not 0 < callee <= count or key in relationships or not calls
                or defaults > calls or bool(defaults) != bool(slots) or slots < defaults or reserved1 or reserved2
                or caller and definitions[caller - 1]["epoch"] != definitions[callee - 1]["epoch"]):
            raise ValueError("Invalid or duplicate template consumer relationship")
        relationships.add(key)
        edges.append({"caller_row": caller, "callee_row": callee, "origin": origin,
                      "first_line": line, "calls": calls, "default_calls": defaults, "default_slots": slots})
    decode_template_paths(rows, definitions, edges)
    open_rows, unfinished, unattributed_calls = required(3011)
    unattributed_bytes, row_calls, edge_calls = required(3012)
    row_binds, row_bytes, edge_defaults = required(3013)
    edge_slots, active, _ = required(3014)
    complete = work["accounting_complete"] and not (errors or open_rows or unfinished or active)
    pool = dict(zip(("internal_definition_bytes", "external_definition_bytes", "default_bytes", "text_bytes", "body_bytes", "plan_bytes"), required(3005) + required(3006)))
    residual = dict(zip(pool, required(3009) + required(3010)))
    attributed = dict(zip(tuple(pool)[:5], required(3007) + required(3008)[:2]))
    observed_binds, observed_bytes, calls = required(3003)
    nested_calls, default_calls, default_slots = required(3004)
    if complete:
        def reconcile(condition, detail):
            if not condition:
                raise ValueError(f"Inconsistent template inventory conservation: {detail}")
        reconcile(all(row["closed"] for row in definitions), "closed definitions")
        reconcile(required(3008)[2] == count, "closed count")
        reconcile(sum(pool.values()) - pool["plan_bytes"] == work["metrics"]["total"]["template_bytes"], "global template payload")
        actual_payload = {"internal_definition_bytes": sum(row["definition_bytes"] for row in definitions if row["pool"] == 0),
                          "external_definition_bytes": sum(row["definition_bytes"] for row in definitions if row["pool"] == 1)}
        for field in ("default_bytes", "text_bytes", "body_bytes"):
            actual_payload[field] = sum(row[field] for row in definitions)
        reconcile(actual_payload == attributed, "definition payload components")
        reconcile(all(attributed[field] + residual[field] == pool[field] for field in attributed)
                  and residual["plan_bytes"] == pool["plan_bytes"], "pool residuals")
        reconcile(row_calls == sum(row["calls"] for row in definitions) == calls == edge_calls == sum(edge["calls"] for edge in edges), "accepted calls")
        reconcile(nested_calls == sum(row["nested_calls"] for row in definitions) == sum(edge["calls"] for edge in edges if edge["caller_row"]), "nested calls")
        reconcile(default_calls == sum(row["default_calls"] for row in definitions) == edge_defaults == sum(edge["default_calls"] for edge in edges), "default calls")
        reconcile(default_slots == sum(row["default_slots"] for row in definitions) == edge_slots == sum(edge["default_slots"] for edge in edges), "default slots")
        reconcile(row_binds == sum(row["plan_binds"] for row in definitions) and row_bytes == sum(row["committed_bound_bytes"] for row in definitions), "body binding sums")
        reconcile(row_binds + unattributed_calls == observed_binds and row_bytes + unattributed_bytes == observed_bytes, "body/unattributed bindings")
        contexts = work.get("unknown_ownership", {}).get("bind_contexts")
        if contexts is None:
            raise ValueError("Missing template inventory definition bind contexts")
        reconcile(observed_binds == sum(contexts[name]["calls"] for name in ("without_unit_definition", "with_unit_definition"))
                  and observed_bytes == sum(contexts[name]["bound_bytes"] for name in ("without_unit_definition", "with_unit_definition")), "definition bind contexts")
        for row in definitions:
            consumers = [edge for edge in edges if edge["callee_row"] == row["row"]]
            reconcile(row["calls"] == sum(edge["calls"] for edge in consumers)
                      and row["nested_calls"] == sum(edge["calls"] for edge in consumers if edge["caller_row"])
                      and row["default_calls"] == sum(edge["default_calls"] for edge in consumers)
                      and row["default_slots"] == sum(edge["default_slots"] for edge in consumers), "per-definition consumers")
            exemplar = (row["first_caller"], row["first_origin"], row["first_line"])
            reconcile(any((edge["caller_row"], edge["origin"], edge["first_line"]) == exemplar for edge in consumers)
                      if row["calls"] else exemplar == (0, 0, 0), "first consumer exemplar")
    classification = None
    if complete:
        classification = {name: {"rows": [row["row"] for row in definitions if predicate(row)],
                                 "definition_count": sum(predicate(row) for row in definitions),
                                 "payload_bytes": sum(row["payload_bytes"] for row in definitions if predicate(row)),
                                 "body_plan_binds": sum(row["plan_binds"] for row in definitions if predicate(row)),
                                 "body_committed_bound_bytes": sum(row["committed_bound_bytes"] for row in definitions if predicate(row))}
                          for name, predicate in (("never_invoked", lambda row: row["calls"] == 0),
                                                  ("invoked", lambda row: row["calls"] > 0))}
    return {"schema": schema, "errors": errors, "error_names": [name for bit, name in TEMPLATE_ERRORS if errors & bit],
            "complete": complete, "classification_complete": complete, "current_epoch": epoch,
            "observer_static_bytes": static_bytes, "definition_limit": definition_limit, "edge_limit": edge_limit,
            "definition_count": count, "edge_count": edge_count, "finished_sessions": finished,
            "open_rows_at_finish": open_rows, "unfinished_sessions": unfinished, "active_session": active,
            "calls": calls, "direct_calls": calls - nested_calls if complete else None, "nested_calls": nested_calls,
            "default_calls": default_calls, "default_slots": default_slots,
            "observed_definition_plan_binds": observed_binds, "observed_definition_committed_bound_bytes": observed_bytes,
            "unattributed_definition_plan_binds": unattributed_calls, "unattributed_definition_committed_bound_bytes": unattributed_bytes,
            "pool_used_bytes": pool, "attributed_payload_bytes": attributed, "unattributed_pool_bytes": residual,
            "definitions": definitions, "consumer_relationships": edges, "classification": classification,
            "evidence_limit": "Calls count accepted queueCall invocations, including empty bodies, never lookups or previews. Relationships group caller row, callee row and origin with one first-line exemplar; they are not full source callsites. Lines use incoming invocation provenance; nested/generated records can inherit outer lines, so path/line pairs are not guaranteed original physical source coordinates. Body binding bytes are committed work deltas, not unique retained plan payload. All plan arena bytes are unattributed. No preparation was skipped and no savings or native completion are established."}


def decode_template_paths(rows, definitions, edges):
    """Validate optional copied path provenance without retaining source objects."""
    if not any(phase >= 34999 for phase in rows):
        return
    def required(phase):
        if phase not in rows:
            raise ValueError(f"Missing template source path phase {phase}")
        return rows[phase]
    if required(34999) != [1, len(definitions), len(edges)]:
        raise ValueError("Inconsistent template source path dictionary schema/counts")
    expected = {34999}
    for slot, provenance in enumerate(definitions + edges):
        start = 35000 + slot * 24
        origin, length, ordinal = required(start)
        if origin != provenance["origin"] or ordinal != slot + 1 or length >= 256 or (not origin and length):
            raise ValueError("Inconsistent template source path provenance")
        chunks = (length + 11) // 12
        expected.update(range(start, start + 1 + chunks))
        data = b"".join(word.to_bytes(4, "big") for phase in range(start + 1, start + 1 + chunks)
                        for word in required(phase))
        if any(data[length:]):
            raise ValueError("Nonzero template source path padding")
        try:
            provenance["source_path"] = data[:length].decode("utf-8", errors="strict")
        except UnicodeDecodeError as error:
            raise ValueError("Invalid UTF-8 template source path") from error
    if any(phase >= 34999 and phase not in expected for phase in rows):
        raise ValueError("Unexpected template source path chunk")

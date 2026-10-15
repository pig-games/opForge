"""Decode passive eager macro packing observations; never completion or savings proof."""
import re

FIELDS = ("writes", "tokens", "lexeme_bytes", "map_bytes", "packed_bytes", "names",
          "members", "package_hits", "source_binds", "capture_binds", "stored_records",
          "stored_bytes", "samples", "ticks_high", "ticks_low", "failures", "prefix_writes", "reserved")
PREFIX = re.compile(r"^progress p=([0-9a-fA-F]+)\b")
ROW = re.compile(r"^progress p=([0-9a-fA-F]{8}) f=([0-9a-fA-F]{8}) l=([0-9a-fA-F]{8}) r=([0-9a-fA-F]{8}) m=([0-9a-fA-F]{8})$")
HEADER = set(range(500000, 500005))
FIXED = HEADER | set(range(500010, 500016)) | set(range(500020, 500026))
BINDER_FIELDS = set(FIELDS[5:10])
MIN_STATE_BYTES = 2049 * 72 + 32 * 16 + 40 + 2 * 72


def definition_group(definition):
    if definition["kind"] == 1:
        return "invoked_macro" if definition["calls"] else "never_invoked_macro"
    return "other_templates"


def decode(stdout, inventory):
    """Join sparse packing owner ordinals to the existing complete template inventory."""
    rows = {}
    for line in stdout.splitlines():
        prefix = PREFIX.match(line)
        if prefix is None or not 500000 <= int(prefix[1], 16) < 520000:
            continue
        match = ROW.fullmatch(line)
        if match is None:
            raise ValueError("Malformed macro packing row")
        phase, *words = (int(value, 16) for value in match.groups())
        if phase in rows:
            raise ValueError(f"Duplicate macro packing phase {phase}")
        if phase not in FIXED and not 500100 <= phase < 500100 + 2049 * 6:
            raise ValueError(f"Unexpected macro packing phase {phase}")
        rows[phase] = words[:3]
    if not FIXED <= rows.keys():
        raise ValueError(f"Missing macro packing phases: {sorted(FIXED - rows.keys())}")
    schema, errors, depth = rows[500000]
    limit, row_bytes, state_bytes = rows[500001]
    period, seed, frequency = rows[500002]
    completed, maximum_depth, timing = rows[500003]
    used, reserved1, reserved2 = rows[500004]
    if (schema != 1 or errors or depth or limit != 2049 or row_bytes != 72
            or state_bytes % 4 or state_bytes < MIN_STATE_BYTES
            or period not in (64, 128, 256) or seed > 255 or timing not in (0, 1)
            or maximum_depth > 32 or used > limit or reserved1 or reserved2):
        raise ValueError("Invalid macro packing schema/bounds/errors/reserved contract")
    if not inventory.get("complete"):
        raise ValueError("Macro packing requires complete template inventory")
    definitions = {row["row"]: row for row in inventory["definitions"]}
    if len(definitions) != len(inventory["definitions"]):
        raise ValueError("Duplicate template inventory owner ordinal")

    def vector(start):
        if not set(range(start, start + 6)) <= rows.keys():
            raise ValueError(f"Missing macro packing vector at {start}")
        values = [word for phase in range(start, start + 6) for word in rows[phase]]
        result = dict(zip(FIELDS, values))
        result["ticks"] = (result.pop("ticks_high") << 32) | result.pop("ticks_low")
        if (result.pop("reserved") or result["failures"] or result["samples"] > result["writes"]
                or result["prefix_writes"] > result["writes"]
                or (not result["samples"] and result["ticks"])):
            raise ValueError("Invalid macro packing vector reserved/failure/sample bounds")
        if ((not timing and (result["samples"] or result["ticks"]))
                or (not frequency and (result["samples"] or result["ticks"]))):
            raise ValueError("Invalid macro packing timing configuration")
        return result

    gross, outside = vector(500010), vector(500020)
    if any(value for key, value in outside.items() if key not in BINDER_FIELDS):
        raise ValueError("Outside macro packing vector contains non-binder fields")
    if not timing and frequency:
        raise ValueError("Disabled macro packing timing has clock frequency")
    ordinals = sorted({(phase - 500100) // 6 for phase in rows if phase >= 500100})
    if len(ordinals) != used:
        raise ValueError("Macro packing used row count differs from emitted vectors")
    owners = []
    for ordinal in ordinals:
        if ordinal and ordinal not in definitions:
            raise ValueError("Unexpected macro packing owner ordinal")
        metrics = vector(500100 + ordinal * 6)
        definition = definitions.get(ordinal)
        if definition is None:
            group = "ordinary"
        else:
            group = definition_group(definition)
        if definition is not None and metrics["stored_bytes"] != definition["last"] - definition["first"]:
            raise ValueError("Macro packing stored bytes disagree with template body payload")
        owners.append({"owner_row": ordinal, "group": group, "metrics": metrics})
    for ordinal, definition in definitions.items():
        if ordinal not in ordinals and definition["last"] != definition["first"]:
            raise ValueError("Missing macro packing owner with stored template body payload")
    for field, value in gross.items():
        if value != sum(owner["metrics"][field] for owner in owners):
            raise ValueError(f"Inconsistent macro packing gross/owner conservation: {field}")
    if completed != gross["writes"]:
        raise ValueError("Macro packing completed writes disagree with gross writes")

    def timed(metrics):
        result = dict(metrics)
        result["observed_sample_seconds"] = metrics["ticks"] / frequency if frequency else None
        result["estimated_writer_seconds"] = (metrics["ticks"] / frequency * metrics["writes"] / metrics["samples"]
                                              if frequency and metrics["samples"] else None)
        return result

    groups = {}
    for group in ("ordinary", "never_invoked_macro", "invoked_macro", "other_templates"):
        matching = [owner for owner in owners if owner["group"] == group]
        template_rows = [row for row in definitions.values() if definition_group(row) == group]
        metrics = {field: sum(owner["metrics"][field] for owner in matching) for field in gross}
        groups[group] = {"owner_rows": [owner["owner_row"] for owner in matching],
                         "definition_count": len(template_rows), "metrics": timed(metrics),
                         "observed_template_payload_bytes": sum(row.get("payload_bytes", 0) for row in template_rows),
                         "observed_template_body_bytes": sum(row["last"] - row["first"] for row in template_rows),
                         "closed_definition_count": sum(row["closed"] for row in template_rows)}
    for owner in owners:
        owner["metrics"] = timed(owner["metrics"])
    return {"schema": schema, "complete": True, "errors": errors, "active_depth": depth,
            "maximum_depth": maximum_depth, "observer_state_bytes": state_bytes,
            "row_limit": limit, "row_bytes": row_bytes, "used_rows": used,
            "sample_period": period, "phase_seed": seed, "frequency": frequency,
            "timing_enabled": bool(timing), "completed_writes": completed,
            "gross": timed(gross), "outside_writer_binds": outside, "owners": owners, "groups": groups,
            "evidence_limit": "Passive eager packing. Lexeme counters measure input extents, not retained payload. Map bytes cover the primary writer map and exclude later prefix frontier fill. Source binds count source-name requests, including iterator probes, rather than allocations. Package hits describe resolution context and may overlap fallback or source-facet activity. Sampled writer time includes binding and nested writers; it is neither full preparation time nor avoided work. Each group estimate uses its own writes and samples; estimates must not be added to gross. Closing lines can be writes without stored records. Outside writer bind counts are separate from gross. Template usage counts accepted calls, not source reachability. No work was deferred and native completion requires separate proof."}

"""Decode the current bounded MEMD record without judging guest completion.

Times include instrumentation overhead. Token and binding measurements are
nested observations and must not be added to the exclusive preparation stages.
Older record layouts deliberately have no compatibility decoder.
"""

import struct


RECORD_BYTES = 2280
RECORD_MAGIC = 0x4D454D44
STAGES = (
    "source_io_and_other", "package_setup", "tokenization",
    "binding_and_raw_records", "expression_preparation",
    "runtime_finalization", "module_discovery",
)
BINDING_SCOPES = (
    "source_line_packed_write_and_bind", "initial_line_plan",
    "conditionals_scopes_imports", "record_finalization_and_append",
    "string_line_plan", "template_dispatch", "template_next",
)
TEMPLATE_COUNTERS = (
    "role_calls", "role_candidate_searches", "role_failed_candidates",
    "role_matched_candidates", "line_candidate_searches",
    "line_candidates_examined", "line_invocations", "line_regular_returns",
    "line_body_captures", "line_definition_headers", "initial_plan_vm_runs",
    "string_plan_captures",
)


def decode_memory_telemetry(data: bytes) -> dict:
    """Return measurements and terminal diagnostics, even for failed guest runs.

    A missing E-clock frequency yields None seconds while preserving ticks and
    counts. Missing or backwards DateStamps likewise yield None phase seconds.
    Optional probe modes cannot be inferred from zero counters in this schema.
    """
    if len(data) != RECORD_BYTES:
        raise ValueError(f"MEMD record must be {RECORD_BYTES} bytes; got {len(data)}")
    words = struct.unpack(">570I", data)
    if words[0] != RECORD_MAGIC:
        raise ValueError(f"invalid MEMD magic: 0x{words[0]:08x}")
    frequency = words[28]

    def ticks(offset):
        return (words[offset] << 32) | words[offset + 1]

    def seconds(value):
        return value / frequency if frequency else None

    def phase_seconds(start, end):
        def stamp(offset):
            return words[offset] * 86400 * 50 + words[offset + 1] * 60 * 50 + words[offset + 2]
        first, last = stamp(start), stamp(end)
        return (last - first) / 50 if first and last and last >= first else None

    def scopes(names, elapsed, entries):
        return {name: {"ticks": ticks(elapsed + 2 * i),
                       "seconds": seconds(ticks(elapsed + 2 * i)),
                       "calls": words[entries + i]}
                for i, name in enumerate(names)}

    stages = scopes(STAGES, 30, 44)
    sample_ticks = ticks(549)
    sample_seconds = seconds(sample_ticks)
    errors = words[29]
    diagnostics = []
    if errors:
        diagnostics.append(f"profiling error flags: 0x{errors:x}")
    if words[1] or words[11]:
        diagnostics.append("tracked allocations remain live at terminal cleanup")
    if words[3] != words[4]:
        diagnostics.append("allocated and freed capacities are unbalanced")
    if not frequency:
        diagnostics.append("E-clock unavailable")
    tokenizer = dict(zip(
        ("line_bytes", "tokens", "lexeme_bytes", "source_reads",
         "taken_eol", "taken_byte", "taken_class"), words[513:520]))
    tokenizer.update({
        "opcodes": list(words[51:72]),
        "opcode_pairs": [{"previous_opcode": i // 21, "opcode": i % 21, "count": count}
                         for i, count in enumerate(words[72:513]) if count],
        "opcode_total": sum(words[51:72]), "opcode_pair_total": sum(words[72:513]),
        "helpers_ticks": ticks(520), "helpers_seconds": seconds(ticks(520)),
        "commit_ticks": ticks(522), "commit_seconds": seconds(ticks(522)),
    })
    result = dict(zip((
        "live_owned_bytes", "peak_owned_bytes", "total_allocated_bytes",
        "total_freed_bytes", "prepared_live_bytes", "freed_before_assembly_bytes",
        "free_at_entry_bytes", "largest_at_entry_bytes", "exec_version",
        "assembly_live_bytes", "cleanup_live_bytes", "dos_version",
        "runtime_prefix_bytes", "packed_source_bytes", "source_bytes_read",
        "compiled_expressions", "evaluation_calls", "compiled_program_bytes",
    ), words[1:19]))
    result.update({
        "instrumented_preparation_seconds": phase_seconds(19, 22),
        "instrumented_assembly_seconds": phase_seconds(22, 25),
        "date_stamps": [list(words[i:i + 3]) for i in (19, 22, 25)],
        "eclock_hz": frequency, "profiling_errors": errors,
        "allocation_failure_flags": errors & (64 | 128),
        "allocation_failure_count": words[524],
        "last_failed_request_bytes": words[525],
        "last_failed_block_capacity_bytes": words[526],
        "last_failed_block_used_bytes": words[527],
        "diagnostics": diagnostics, "preparation_stages": stages,
        "preparation_stage_seconds": seconds(sum(value["ticks"] for value in stages.values())),
        "tokenizer": tokenizer,
        "binding_detail": {
            "scopes": scopes(BINDING_SCOPES, 528, 542),
            "binding_calls": words[551], "binding_samples": words[552],
            "binding_sample_ticks": sample_ticks, "binding_sample_seconds": sample_seconds,
            "binding_estimated_seconds": (sample_seconds * words[551] / words[552]
                                          if sample_seconds is not None and words[552] else None),
        },
        "template_work": dict(zip(TEMPLATE_COUNTERS, words[553:565])),
        "input_collection": {"ticks": ticks(565), "seconds": seconds(ticks(565)),
                             "calls": words[567], "bytes": words[568], "reads": words[569]},
    })
    return result

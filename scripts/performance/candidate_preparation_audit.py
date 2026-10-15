"""Decode passive candidate preparation attribution, never native completion proof."""
import re

FIELDS = ("records", "capture_bytes", "plan_bytes", "source_bytes", "token_bytes",
          "lexeme_bytes", "candidate_calls", "descriptor_calls", "fragment_calls",
          "plan_create_calls", "samples", "ticks_high", "ticks_low", "reserved0", "reserved1")
GROUPS = ("total", "non_macro", "never_invoked", "invoked", "unknown", "not_retained")
PREFIX = re.compile(r"^progress p=([0-9a-fA-F]+)\b")
ROW = re.compile(r"^progress p=([0-9a-fA-F]{8}) f=([0-9a-fA-F]{8}) l=([0-9a-fA-F]{8}) r=([0-9a-fA-F]{8}) m=([0-9a-fA-F]{8})$")
REQUIRED = set(range(400000, 400004)) | set(range(400010, 400040))


def decode(stdout):
    rows = {}
    for line in stdout.splitlines():
        prefix = PREFIX.match(line)
        if prefix is None or not 400000 <= int(prefix[1], 16) < 401000:
            continue
        match = ROW.fullmatch(line)
        if match is None:
            raise ValueError("Malformed candidate attribution row")
        phase, *words = (int(value, 16) for value in match.groups())
        if phase not in REQUIRED:
            raise ValueError(f"Unexpected candidate attribution phase {phase}")
        if phase in rows:
            raise ValueError(f"Duplicate candidate attribution phase {phase}")
        rows[phase] = words[:3]
    if rows.keys() != REQUIRED:
        raise ValueError(f"Missing candidate attribution phases: {sorted(REQUIRED - rows.keys())}")
    schema, errors, static = rows[400000]
    created, plan_bytes, duplicates = rows[400001]
    frequency, period, seed = rows[400002]
    timed_calls, samples, unassigned = rows[400003]
    if schema != 1 or not static or static % 4 or period not in (64, 128, 256) or seed >= period:
        raise ValueError("Invalid candidate attribution schema/static/timing contract")
    vectors = {}
    for index, name in enumerate(GROUPS):
        values = [word for phase in range(400010 + index * 5, 400015 + index * 5) for word in rows[phase]]
        vector = dict(zip(FIELDS, values))
        if vector['reserved0'] or vector['reserved1']:
            raise ValueError("Nonzero candidate attribution reserved word")
        vector['ticks'] = (vector.pop('ticks_high') << 32) | vector.pop('ticks_low')
        if vector['samples'] > vector['records'] or (not vector['samples'] and vector['ticks']):
            raise ValueError("Invalid candidate sample/tick bounds")
        vectors[name] = vector
    total = vectors['total']
    for field in total:
        if total[field] != sum(vectors[group][field] for group in GROUPS[1:]):
            raise ValueError(f"Inconsistent candidate attribution conservation: {field}")
    if total['records'] != created or total['plan_bytes'] != plan_bytes:
        raise ValueError("Inconsistent candidate created record/plan totals")
    if (samples != total['samples'] or samples > timed_calls or timed_calls > created
            or unassigned > vectors['unknown']['records'] or (not frequency and (samples or total['ticks']))):
        raise ValueError("Inconsistent candidate timing/owner totals")
    for vector in vectors.values():
        vector['observed_sample_seconds'] = vector['ticks'] / frequency if frequency else None
        vector['estimated_preparation_seconds'] = (vector['observed_sample_seconds'] * vector['records'] / vector['samples']
                                                    if vector['samples'] and frequency else None)
    return {'schema': schema, 'errors': errors, 'complete': errors == 0 and duplicates == 0,
            'observer_static_bytes': static, 'records_created': created, 'plan_bytes_created': plan_bytes,
            'duplicate_replays': duplicates, 'frequency': frequency, 'sample_period': period,
            'phase_seed': seed, 'timed_calls': timed_calls, 'timed_samples': samples,
            'unassigned_owner_records': unassigned, 'metrics': vectors,
            'evidence_limit': 'Passive eager preparation; gross acquisitions include records discarded before retained-region publication. The not_retained group does not establish avoidable work. Source classes include closing records. Sample clocks use per-record delta32 accumulated in 64 bits. Sample extrapolation is noisy diagnostic attribution, not release speedup or avoided work; no estimate exists for zero samples. Native completion requires separate proof.'}

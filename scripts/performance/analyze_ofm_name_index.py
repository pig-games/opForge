#!/usr/bin/env python3
"""Model OFMS name-index size and lookup work from frozen OFMI objects.

This is a deterministic model, not an observer or a measurement of query
frequency. See the JSON assumptions for the exact workload and byte model.
"""
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import struct
import sys

OFMI_MAGIC = b"OFMI"
OFMA_MAGIC = b"OFMA"
OFMS_MAGIC = b"OFMS"
OFMI_HEADER = 36
OFMA_HEADER = 48
OFMS_BASE_HEADER = 40
OFMS_INDEXED_HEADER = 48
EXPORT_BYTES = 28
WINDOW_BYTES = 65536
MEMORY_LIMIT = 1048576


def u32(data: bytes, offset: int) -> int:
    if offset < 0 or offset + 4 > len(data):
        raise ValueError(f"truncated u32 at offset {offset}")
    return struct.unpack_from(">I", data, offset)[0]


def region(data: bytes, offset: int, size: int, label: str) -> bytes:
    if offset < 0 or size < 0 or offset > len(data) or size > len(data) - offset:
        raise ValueError(f"{label} range {offset}+{size} exceeds {len(data)} bytes")
    return data[offset:offset + size]


def parse_ofmi(data: bytes, path: Path) -> tuple[bytes, int]:
    if len(data) < OFMI_HEADER:
        raise ValueError(f"{path}: truncated OFMI header")
    if data[:4] != OFMI_MAGIC:
        raise ValueError(f"{path}: expected OFMI at byte zero")
    total, blocks, block_off, relocs, reloc_off, payload, semantics, sem_bytes = struct.unpack_from(">8I", data, 4)
    if total != len(data) or total > MEMORY_LIMIT:
        raise ValueError(f"{path}: OFMI byte count mismatch or limit exceeded")
    if block_off != OFMI_HEADER or reloc_off != block_off + blocks * 32:
        raise ValueError(f"{path}: noncanonical OFMI block/relocation offsets")
    if payload != reloc_off + relocs * 28:
        raise ValueError(f"{path}: noncanonical OFMI payload offset")
    region(data, block_off, blocks * 32, "OFMI blocks")
    region(data, reloc_off, relocs * 28, "OFMI relocations")
    region(data, payload, total - payload, "OFMI payload")
    sem = region(data, semantics, sem_bytes, "OFMI semantics")
    if not sem or len(sem) < OFMA_HEADER or sem[:4] != OFMA_MAGIC:
        raise ValueError(f"{path}: OFMI semantics does not begin with OFMA")
    if u32(sem, 4) != len(sem):
        raise ValueError(f"{path}: OFMA byte count mismatch")
    values_off, values_bytes = u32(sem, 8), u32(sem, 12)
    ofms = region(sem, values_off, values_bytes, "OFMA values/OFMS")
    if len(ofms) < OFMS_BASE_HEADER or ofms[:4] != OFMS_MAGIC:
        raise ValueError(f"{path}: OFMA values does not begin with OFMS")
    if u32(ofms, 4) != len(ofms):
        raise ValueError(f"{path}: OFMS byte count mismatch")
    return ofms, len(sem)


def parse_ofms(data: bytes, path: Path) -> tuple[list[dict], int, dict | None]:
    if len(data) < OFMS_BASE_HEADER or data[:4] != OFMS_MAGIC:
        raise ValueError(f"{path}: malformed OFMS header")
    total, export_count, exports_off = struct.unpack_from(">3I", data, 4)
    header_bytes = exports_off
    if header_bytes not in (OFMS_BASE_HEADER, OFMS_INDEXED_HEADER) or len(data) < header_bytes:
        raise ValueError(f"{path}: unsupported OFMS header size {header_bytes}")
    member_count, members_off, names_off, names_bytes, arena_off, arena_bytes = struct.unpack_from(">6I", data, 16)
    index_count = index_off = 0
    if header_bytes == OFMS_INDEXED_HEADER:
        index_count, index_off = struct.unpack_from(">2I", data, 40)
    if total != len(data) or total > MEMORY_LIMIT:
        raise ValueError(f"{path}: OFMS byte count mismatch or limit exceeded")
    if members_off != exports_off + export_count * EXPORT_BYTES:
        raise ValueError(f"{path}: noncanonical OFMS export/member offsets")
    expected_names = members_off + member_count * 12
    if header_bytes == OFMS_INDEXED_HEADER:
        if index_count > export_count or index_off != expected_names:
            raise ValueError(f"{path}: noncanonical OFMS index offset/count")
        expected_names += index_count * 8
    if names_off != expected_names:
        raise ValueError(f"{path}: noncanonical OFMS names offset")
    region(data, exports_off, export_count * EXPORT_BYTES, "OFMS exports")
    region(data, members_off, member_count * 12, "OFMS members")
    names = region(data, names_off, names_bytes, "OFMS names")
    arena_start = (names_off + names_bytes + 7) & ~7
    if arena_off != arena_start or arena_off + arena_bytes != total:
        raise ValueError(f"{path}: invalid OFMS arena extent")
    exports = []
    for i in range(export_count):
        row = exports_off + i * EXPORT_BYTES
        entity, name_off, name_len = struct.unpack_from(">3I", data, row)
        if name_len:
            if name_off < names_off:
                raise ValueError(f"{path}: export {i} name precedes the names table")
            raw = region(names, name_off - names_off, name_len, f"OFMS export {i} name")
            try:
                name = raw.decode("ascii")
            except UnicodeDecodeError as exc:
                raise ValueError(f"{path}: export name is not ASCII") from exc
            exports.append({"id": entity, "name": name})
        elif name_off:
            raise ValueError(f"{path}: private export {i} has a name offset")
        else:
            exports.append({"id": entity, "name": None})
    indexes = None
    if header_bytes == 48:
        public_ordinals = [i for i, row in enumerate(exports) if row["name"] is not None]
        if index_count != len(public_ordinals):
            raise ValueError(f"{path}: index count does not cover all public exports")
        words = struct.unpack_from(f">{index_count * 2}I", data, index_off) if index_count else ()
        full_order, leaf_order = list(words[:index_count]), list(words[index_count:])
        for label, order, leaf_only in (("full", full_order, False), ("leaf", leaf_order, True)):
            if sorted(order) != public_ordinals:
                raise ValueError(f"{path}: {label} index is not a public-export permutation")
            keyed = [(fold(leaf(exports[i]["name"]) if leaf_only else exports[i]["name"]), i)
                     for i in order]
            if keyed != sorted(keyed):
                raise ValueError(f"{path}: {label} index ordering is invalid")
        indexes = {"full": full_order, "leaf": leaf_order}
    return exports, total, indexes


def fold(text: str) -> bytes:
    # Native folded() changes only ASCII A-Z.
    return bytes(c + 32 if 65 <= c <= 90 else c for c in text.encode("ascii"))


def leaf(name: str) -> str:
    return name.rsplit(".", 1)[-1]


def compare(a: bytes, b: bytes) -> tuple[int, int]:
    n = min(len(a), len(b))
    for i in range(n):
        if a[i] != b[i]:
            return (-1 if a[i] < b[i] else 1), i + 1
    if len(a) == len(b):
        return 0, n
    return (-1 if len(a) < len(b) else 1), n


def simulate(exports: list[dict], query: str, mode: str, index_orders: dict | None = None) -> dict:
    query_key = fold(query)
    full = "." in query
    if not query_key:
        return {"query": query, "kind": mode, "result": "INVALID",
                "scan_export_visits": 0, "scan_key_comparisons": 0,
                "scan_byte_comparisons": 0, "index_key_comparisons": 0,
                "index_byte_comparisons": 0}
    def key(row):
        n = row["name"] if full else leaf(row["name"])
        return fold(n)

    scan_rows = scan_key_comparisons = scan_bytes = 0
    found_ids = set()
    status = "NOT_FOUND"
    for row in exports:
        scan_rows += 1
        if row["name"] is None:
            continue
        scan_key_comparisons += 1
        candidate_key = key(row)
        order, cost = compare(candidate_key, query_key)
        # The legacy path checks lengths first and only byte-compares equal
        # lengths. Leaf extraction itself is not included in this counter.
        if len(candidate_key) == len(query_key):
            scan_bytes += cost
        if order == 0:
            found_ids.add(row["id"])
            if len(found_ids) > 1:
                status = "INVALID"
                break
            status = "OK"

    index_kind = "full" if full else "leaf"
    ordinals = index_orders[index_kind] if index_orders else sorted(
        (i for i, row in enumerate(exports) if row["name"] is not None), key=lambda i: (key(exports[i]), i))
    index = [(key(exports[i]), exports[i]["id"]) for i in ordinals]
    # Native lookup performs lower-bound, then scans the equal-key range.
    lo, hi = 0, len(index)
    comparisons = byte_comparisons = 0
    while lo < hi:
        mid = (lo + hi) // 2
        order, cost = compare(index[mid][0], query_key)
        comparisons += 1
        byte_comparisons += cost
        if order < 0:
            lo = mid + 1
        else:
            hi = mid
    new_ids = set()
    pos = lo
    while pos < len(index):
        order, cost = compare(index[pos][0], query_key)
        comparisons += 1
        byte_comparisons += cost
        if order:
            break
        new_ids.add(index[pos][1])
        if len(new_ids) > 1:
            break
        pos += 1
    new_status = "INVALID" if len(new_ids) > 1 else "OK" if new_ids else "NOT_FOUND"
    if status != new_status:
        raise ValueError(f"lookup model disagreement for {query!r}: scan={status}, index={new_status}")
    return {"query": query, "kind": mode, "result": status,
            "scan_export_visits": scan_rows, "scan_key_comparisons": scan_key_comparisons,
            "scan_byte_comparisons": scan_bytes,
            "index_key_comparisons": comparisons, "index_byte_comparisons": byte_comparisons}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("objects", type=Path, help="frozen directory containing raw .ofm objects")
    parser.add_argument("--missing-key", action="append", default=[], help="also model this missing query (repeatable)")
    parser.add_argument("--output", type=Path, help="write JSON here (default stdout)")
    parser.add_argument("--include-per-query", action="store_true", help="include every modeled query row in JSON")
    args = parser.parse_args()
    files = sorted(args.objects.glob("*.ofm"))
    if not files:
        parser.error(f"no .ofm files in {args.objects}")
    digest = hashlib.sha256()
    totals = {"files": len(files), "object_bytes": 0, "exports": 0, "public_exports": 0, "private_exports": 0}
    modeled = []
    max_exports = {"count": -1}
    max_index_bytes = max_result_ofma = 0
    actual_indexed_files = 0
    queries = []
    for path in files:
        raw = path.read_bytes()
        digest.update(path.name.encode("utf-8") + b"\0" + len(raw).to_bytes(8, "big") + raw)
        ofms, ofma_size = parse_ofmi(raw, path)
        exports, ofms_size, indexes = parse_ofms(ofms, path)
        public = [e for e in exports if e["name"] is not None]
        private = len(exports) - len(public)
        totals["object_bytes"] += len(raw)
        totals["exports"] += len(exports)
        totals["public_exports"] += len(public)
        totals["private_exports"] += private
        if len(exports) > max_exports["count"]:
            max_exports = {"file": path.name, "count": len(exports), "public": len(public), "ofms_bytes": ofms_size, "ofma_bytes": ofma_size}
        max_index_bytes = max(max_index_bytes, 8 * len(public) + 8)
        max_result_ofma = max(max_result_ofma, ofma_size + 8 * len(public) + 8)
        modeled.append({"file": path.name, "exports": len(exports), "public": len(public), "private": private,
                        "ofms_bytes": ofms_size, "ofma_bytes": ofma_size,
                        "stored_index": indexes is not None})
        if indexes is not None:
            actual_indexed_files += 1
        queries.extend(simulate(exports, e["name"], "public_full_key", indexes) for e in public)
        seen = set()
        for e in public:
            k = fold(leaf(e["name"]))
            if k not in seen:
                seen.add(k)
                queries.append(simulate(exports, leaf(e["name"]), "unique_leaf_key", indexes))
        queries.extend(simulate(exports, q, "missing_key", indexes) for q in args.missing_key)
    model_extra = 8 * totals["public_exports"] + 8 * totals["files"]
    sums = {k: sum(q[k] for q in queries) for k in ("scan_export_visits", "scan_key_comparisons", "scan_byte_comparisons", "index_key_comparisons", "index_byte_comparisons")}
    result = {
        "schema": 1,
        "input_directory": str(args.objects),
        "input_sha256": digest.hexdigest(),
        "model_label": "MODEL; deterministic full-coverage query set, not observed query frequency",
        "assumptions": {
            "object_selection": "sorted immediate-child *.ofm files only; .uaem sidecars excluded",
            "routing": "OFMI header semantics offset 28/bytes 32 -> OFMA values offset 8/bytes 12 -> OFMS",
            "name_comparison": "ASCII case fold A-Z; full key when query contains dot, otherwise final leaf",
            "current_scan": "all exports in serialized order; nameless/private rows skipped; stop after second distinct matching ID",
            "proposed_index": "two sorted u32 ordinal arrays (full and leaf), binary lower-bound plus equal-range scan; ties use export ordinal",
            "byte_comparison_count": "index compares count bytes inspected until mismatch; legacy scan counts bytes only for equal-length keys because it rejects unequal lengths first; leaf extraction and I/O are excluded",
            "workload": "one attempt per public full key and per unique folded leaf key, plus each requested missing key per object; empty leaf attempts are INVALID",
            "index_size": "8 bytes per public export in two u32 ordinal arrays; indexed OFMS headers are 48 bytes (8 bytes larger than the 40-byte baseline); alignment unchanged because additions are multiples of 8",
            "window_bytes": WINDOW_BYTES,
        },
        "totals": totals,
        "proposed_index": {"aggregate_extra_bytes": model_extra, "max_index_bytes": max_index_bytes,
                           "max_ofma_bytes_after_index": max_result_ofma,
                           "max_windows_needed": (max_result_ofma + WINDOW_BYTES - 1) // WINDOW_BYTES,
                           "objects_with_stored_index": actual_indexed_files},
        "maximum_export_object": max_exports,
        "simulation": {"queries": len(queries), "successful_queries": sum(q["result"] == "OK" for q in queries), **sums,
                       "key_comparison_reduction_percent": (100 * (1 - sums["index_key_comparisons"] / sums["scan_key_comparisons"])) if sums["scan_key_comparisons"] else 0,
                       "empty_leaf_queries": sum(q["kind"] == "unique_leaf_key" and not q["query"] for q in queries)},
        "files": modeled,
    }
    if args.include_per_query:
        result["simulation"]["per_query"] = queries
    encoded = json.dumps(result, indent=2, sort_keys=True) + "\n"
    if args.output:
        args.output.write_text(encoded, encoding="utf-8")
    else:
        sys.stdout.write(encoded)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, ValueError) as exc:
        print(f"error: {exc}", file=sys.stderr)
        raise SystemExit(2)

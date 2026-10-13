#!/usr/bin/env python3
"""Run a light-capture A6000 baseline and fixed-workload hotspot profiles."""

import argparse
import json
from pathlib import Path
import re
import subprocess
import tempfile

import run_a6000_selfhost as runner


def load_hotspot_cases(baseline, profiles):
    """Validate all exports and freeze the baseline workload before any run."""
    loaded = [runner.load_bundle(path) for path in [baseline, *profiles]]
    manifests, file_sets, oracles, _ = zip(*loaded)
    base = manifests[0]
    base_defines, _ = runner.hotspot_configuration(base.get("bootstrap_defines", []))
    required = {"OPFORGE_DEBUG_CONTRACTS", "OPFORGE_MEMORY_TELEMETRY",
                "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT", "OPFORGE_PREPARATION_PROGRESS",
                "OPFORGE_CAPTURE_TELEMETRY"}
    if (base.get("kind") != "compact-self-host-local-export"
            or base.get("manifest_version") != 1 or base_defines != required
            or base_defines & runner.DENSE_TELEMETRY_DEFINES):
        raise ValueError("Baseline must be a light MEMD+CAPTURE bootstrap without HOTSPOT or dense telemetry")
    if base.get("bootstrap_package_storage", "external") != "embedded":
        raise ValueError("A6000 hotspot workload requires the embedded 68020 package")
    if any(m.get("kind") != "compact-self-host-local-export" or m.get("manifest_version") != 1
           for m in manifests):
        raise ValueError("Require genuine compact self-host exports")
    if any(m["command"] != base["command"] for m in manifests[1:]):
        raise ValueError("Hotspot exports must use the same command")
    if any(oracle != oracles[0] for oracle in oracles[1:]):
        raise ValueError("Hotspot exports must use the exact same Rust oracle")
    for key, default in (("bootstrap_package_storage", "external"),
                         ("output_package_storage", "external"),
                         ("embedded_packages", []), ("output_embedded_packages", [])):
        if any(m.get(key, default) != base.get(key, default) for m in manifests[1:]):
            raise ValueError(f"Hotspot exports differ in {key}")
    for files in file_sets[1:]:
        if {k: v for k, v in files.items() if k != "opforge"} != {
                k: v for k, v in file_sets[0].items() if k != "opforge"}:
            raise ValueError("All non-bootstrap workload file bytes must match exactly")
    configurations = []
    for manifest in manifests[1:]:
        defines, config = runner.hotspot_configuration(manifest.get("bootstrap_defines", []))
        if ("OPFORGE_HOTSPOT_TELEMETRY" not in defines
                or defines != base_defines | {"OPFORGE_HOTSPOT_TELEMETRY"}
                or defines & runner.DENSE_TELEMETRY_DEFINES):
            raise ValueError("Profiles must retain baseline MEMD+CAPTURE flags and add only HOTSPOT telemetry")
        configurations.append(config)
    if len({(c["sampling_period"], c["phase_seed"]) for c in configurations}) != len(configurations):
        raise ValueError("Hotspot profiles must have distinct period/seed configurations")
    cases = []
    for path, manifest, own_files, own_digest, config in zip(
            profiles, manifests[1:], file_sets[1:], [row[3] for row in loaded[1:]], configurations):
        files = dict(file_sets[0], opforge=own_files["opforge"])
        case_digest = own_digest
        role = f"hotspot-period-{config['sampling_period']}-seed-{config['phase_seed']}"
        cases.append((path, manifest, files, oracles[0], case_digest, role, config))
    baseline_case = (baseline, base, file_sets[0], oracles[0], loaded[0][3], "light-baseline", None)
    return base, [baseline_case, *cases]


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline-bundle", type=Path, required=True)
    parser.add_argument("--profile-bundle", type=Path, action="append", required=True)
    parser.add_argument("--host", default="192.168.0.220")
    parser.add_argument("--volume", default="Development")
    parser.add_argument("--timeout", type=int, default=3600)
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args(argv)
    if not re.fullmatch(r"[A-Za-z0-9.-]+", args.host) or not re.fullmatch(r"[A-Za-z0-9_-]+", args.volume):
        parser.error("Host and volume must be plain names; omit the volume colon")
    if not 1 <= args.timeout < 86400:
        parser.error("Timeout must be between 1 and 86399 seconds")
    baseline = args.baseline_bundle.resolve(strict=True)
    profiles = [path.resolve(strict=True) for path in args.profile_bundle]
    _, cases = load_hotspot_cases(baseline, profiles)
    summary_root = Path(tempfile.mkdtemp(prefix="opforge-a6000-hotspots-"))
    summary_path = summary_root / "hotspots.json"
    summary = {"result_kind": "a6000_hotspot_profile_summary", "dry_run": args.dry_run,
               "instrumentation_note": "Hotspot timings include observer overhead; this is not a production optimization claim.",
               "runs": []}
    summary_path.write_text(json.dumps(summary, indent=2) + "\n")
    for index, (bundle, manifest, files, oracle, case_digest, role, config) in enumerate(cases):
        metadata = {"profile_role": role}
        if config:
            metadata["hotspot_configuration"] = config
        result, local = runner.run_case(args, bundle, manifest, files, oracle, case_digest,
                                        run_kind=role, metadata=metadata)
        record = {"role": role, "bundle": str(bundle), "case_sha256": case_digest,
                  "local_results": str(local), "result": result, **metadata}
        summary["runs"].append(record)
        base_result = summary["runs"][0]["result"]
        if index and base_result and result and base_result.get("success") and result.get("success"):
            base_seconds = base_result.get("guest_overall_seconds")
            prof_seconds = result.get("guest_overall_seconds")
            record["guest_duration_seconds"] = prof_seconds
            record["extra_observer_seconds_vs_light_baseline"] = (
                prof_seconds - base_seconds if base_seconds is not None and prof_seconds is not None else None)
        elif result and result.get("success"):
            record["guest_duration_seconds"] = result.get("guest_overall_seconds")
        summary_path.write_text(json.dumps(summary, indent=2) + "\n")
        print(f"Incremental summary: {summary_path}", flush=True)
    print(json.dumps(summary, indent=2), flush=True)
    print(f"Hotspot summary: {summary_path}", flush=True)
    return 0 if args.dry_run or all(row["result"] and row["result"]["success"] for row in summary["runs"]) else 1


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        raise SystemExit(f"Stopped: {error}. No successful hotspot profiling claim.")

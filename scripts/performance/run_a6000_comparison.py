#!/usr/bin/env python3
"""Compare two release bootstraps on one frozen A6000 workload, then self-host.

Both exports must pass the ordinary self-host bundle validation independently.
The fixed workload uses only the baseline's sources, command, package and Rust
oracle. Its candidate run is a benchmark, not candidate-source self-host proof.
Transfers are outside each timing window. No Terminal automation is required.
"""

import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess
import tempfile

import run_a6000_selfhost as runner

MEMORY_DEFINES = frozenset({
    "OPFORGE_DEBUG_CONTRACTS", "OPFORGE_MEMORY_TELEMETRY",
    "OPFORGE_PREPARATION_PROGRESS", "OPFORGE_CAPTURE_TELEMETRY",
    "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT",
})


def digest_parts(parts):
    digest = hashlib.sha256()
    for part in parts:
        digest.update(len(part).to_bytes(8, "big"))
        digest.update(part)
    return digest.hexdigest()


def load_comparison(baseline, candidate, frozen_source_root):
    baseline_manifest, baseline_files, oracle, _ = runner.load_bundle(baseline)
    candidate_manifest, candidate_files, candidate_oracle, _ = runner.load_bundle(candidate)
    manifests = (baseline_manifest, candidate_manifest)
    for manifest, files, own_oracle in zip(
        manifests, (baseline_files, candidate_files), (oracle, candidate_oracle)
    ):
        if (manifest.get("kind") != "compact-self-host-local-export"
                or manifest.get("manifest_version") != 1):
            raise ValueError("Require genuine compact self-host exports")
        if manifest.get("bootstrap_defines") or manifest.get("release_defines") != []:
            raise ValueError("Fixed-workload comparison requires release exports")
        # This comparison accepts only release images that are independently
        # their own bundle's oracle, including the embedded-output configuration.
        if files["opforge"] != own_oracle:
            raise ValueError("Comparison release bootstrap must equal its own oracle")
    if Path(baseline_manifest["source_root"]).resolve(strict=True) != frozen_source_root.resolve(strict=True):
        raise ValueError("Baseline export does not identify the requested frozen source root")
    for key, default in (("bootstrap_package_storage", "external"),
                         ("output_package_storage", "external"),
                         ("embedded_packages", []), ("output_embedded_packages", [])):
        if baseline_manifest.get(key, default) != candidate_manifest.get(key, default):
            raise ValueError(f"Comparison exports differ in {key}")
    packages = [
        (bundle / manifest.get("runtime_package_file", "p.bin")).read_bytes()
        for bundle, manifest in zip((baseline, candidate), manifests)
    ]
    if any(runner.fnv(package) != manifest["runtime_package_digest"]
           for package, manifest in zip(packages, manifests)):
        raise ValueError("Runtime package changed after bundle validation")
    if packages[0] != packages[1]:
        raise ValueError("Comparison requires the exact same runtime package bytes")

    workload_parts = [b"opforge-a6000-fixed-workload-v1", b"m68020",
                      baseline_manifest["command"].encode(), oracle, packages[0],
                      baseline_manifest.get("bootstrap_package_storage", "external").encode(),
                      baseline_manifest.get("output_package_storage", "external").encode()]
    for name, data in sorted(baseline_files.items()):
        if name != "opforge":
            workload_parts.extend((name.encode(), data))
    workload_digest = digest_parts(workload_parts)
    cases = []
    for label, bundle, manifest, bootstrap in (
        ("baseline", baseline, baseline_manifest, baseline_files["opforge"]),
        ("candidate", candidate, candidate_manifest, candidate_files["opforge"]),
    ):
        files = dict(baseline_files, opforge=bootstrap)
        case_digest = digest_parts([b"opforge-a6000-fixed-case-v1",
                                    workload_digest.encode(), bootstrap])
        metadata = {
            "result_kind": "fixed_workload_comparison",
            "comparison_role": label,
            "candidate_source_selfhost_proven": False,
            "workload_sha256": workload_digest,
            "workload_bundle": str(baseline),
            "frozen_source_root": str(frozen_source_root.resolve()),
            "workload_source_manifest_digest": baseline_manifest["source_manifest_digest"],
            "bootstrap_bundle": str(bundle),
            "bootstrap_sha256": hashlib.sha256(bootstrap).hexdigest(),
            "bootstrap_source_manifest_digest": manifest["source_manifest_digest"],
            "bootstrap_hunk_digest": manifest["bootstrap_hunk_digest"],
            "bundle_prior_native_validation": "Fixed-workload case has no prior native qualification.",
            "workload_export_prior_native_validation": runner.native_validation_status(baseline_manifest),
            "bootstrap_export_prior_native_validation": runner.native_validation_status(manifest),
        }
        cases.append((files, case_digest, metadata))
    return baseline_manifest, oracle, cases


def comparison_summary(records):
    """Keep assembly and clock evidence separate from transport overhead."""
    summary = {"result_kind": "fixed_workload_comparison_summary", "runs": records}
    if len(records) < 2:
        summary["fixed_workload_parity_passed"] = False
        return summary
    baseline, candidate = [row["result"] for row in records[:2]]
    summary["fixed_workload_parity_passed"] = all(
        result is not None and result["success"] for result in (baseline, candidate)
    )
    if summary["fixed_workload_parity_passed"]:
        first, second = [result["guest_overall_seconds"] for result in (baseline, candidate)]
        summary["guest_seconds_saved"] = first - second if first is not None and second is not None else None
        summary["guest_speedup"] = first / second if first is not None and second not in (None, 0) else None
    return summary


def load_memory_cases(baseline_release, candidate_release, baseline_memory,
                      candidate_memory, workload_digest):
    """Validate paired memory exports and bind them to the frozen baseline input."""
    release_paths = (baseline_release, candidate_release)
    memory_paths = (baseline_memory, candidate_memory)
    releases = [runner.load_bundle(path) for path in release_paths]
    memories = [runner.load_bundle(path) for path in memory_paths]
    baseline_manifest, workload_files, workload_oracle, _ = releases[0]
    workload_source_files = baseline_manifest.get("source_files", len(baseline_manifest["source_mapping"]))
    workload_source_bytes = baseline_manifest.get(
        "source_bytes", sum(row["bytes"] for row in baseline_manifest["source_mapping"])
    )
    cases = []
    for role, release_path, memory_path, release, memory in zip(
        ("baseline_memory", "candidate_memory"), release_paths, memory_paths, releases, memories
    ):
        release_manifest, release_files, release_oracle, _ = release
        memory_manifest, memory_files, memory_oracle, _ = memory
        if (release_manifest.get("kind") != "compact-self-host-local-export"
                or release_manifest.get("manifest_version") != 1
                or release_manifest.get("bootstrap_defines")
                or release_manifest.get("release_defines") != []
                or release_files["opforge"] != release_oracle):
            raise ValueError("Memory comparison requires matching release exports")
        if (memory_manifest.get("kind") != "compact-self-host-local-export"
                or memory_manifest.get("manifest_version") != 1
                or memory_manifest.get("release_defines") != []):
            raise ValueError("Memory comparison requires genuine compact self-host exports")
        for key, default in (("bootstrap_package_storage", "external"),
                             ("output_package_storage", "external"),
                             ("embedded_packages", []), ("output_embedded_packages", [])):
            if memory_manifest.get(key, default) != release_manifest.get(key, default):
                raise ValueError("Memory export storage settings must match its corresponding release export")
        flags = memory_manifest.get("bootstrap_defines", [])
        if len(flags) != len(set(flags)) or set(flags) != MEMORY_DEFINES:
            raise ValueError("Memory exports require exactly the light MEMD+CAPTURE defines")
        if (memory_oracle != release_oracle
                or {key: data for key, data in memory_files.items() if key != "opforge"}
                != {key: data for key, data in release_files.items() if key != "opforge"}
                or memory_manifest["command"] != release_manifest["command"]
                or memory_manifest.get("source_manifest_digest")
                != release_manifest.get("source_manifest_digest")):
            raise ValueError("Memory export sources, package, command and oracle must match its release export")
        if memory_manifest["command"] != baseline_manifest["command"]:
            raise ValueError("Memory comparison must use the frozen baseline command")
        if memory_files["opforge"] == release_files["opforge"]:
            raise ValueError("Memory export must use its actual instrumented bootstrap")

        # Every memory run executes the baseline release's frozen workload;
        # only its validated instrumented bootstrap is substituted.
        files = dict(workload_files, opforge=memory_files["opforge"])
        bootstrap = memory_files["opforge"]
        case_digest = digest_parts([
            b"opforge-a6000-fixed-memory-case-v1", workload_digest.encode(),
            *(flag.encode() for flag in sorted(flags)), bootstrap,
        ])
        metadata = {
            "result_kind": "fixed_workload_memory_comparison",
            "comparison_role": role,
            "candidate_source_selfhost_proven": False,
            "workload_sha256": workload_digest,
            "workload_bundle": str(baseline_release),
            "workload_source_root": baseline_manifest["source_root"],
            "workload_source_manifest_digest": baseline_manifest["source_manifest_digest"],
            "workload_source_files": workload_source_files,
            "workload_source_bytes": workload_source_bytes,
            "bootstrap_bundle": str(memory_path),
            "bootstrap_sha256": hashlib.sha256(bootstrap).hexdigest(),
            "bootstrap_source_manifest_digest": memory_manifest["source_manifest_digest"],
            "corresponding_release_bundle": str(release_path),
            "bootstrap_defines": flags,
            "bundle_prior_native_validation": "Fixed-workload memory case has no prior native qualification.",
            "workload_export_prior_native_validation": runner.native_validation_status(baseline_manifest),
            "bootstrap_export_prior_native_validation": runner.native_validation_status(memory_manifest),
            # The workload remains baseline-owned even for candidate memory.
            "source_files": workload_source_files,
            "source_bytes": workload_source_bytes,
        }
        cases.append((memory_path, memory_manifest, files, workload_oracle,
                      case_digest, role, metadata))
    return cases


def memory_comparison_summary(records):
    """Compare MEMD and capture ownership; instrumented timings never imply speedup."""
    if len(records) != 2:
        return None
    results = [row.get("result") for row in records]
    if any(not result or not result.get("success") or not result.get("telemetry_completed")
           or not isinstance(result.get("instrumented_memory"), dict)
           or not isinstance(result.get("capture_ownership"), dict)
           for result in results):
        return None
    memory_keys = (
        "peak_owned_bytes", "live_owned_bytes", "total_allocated_bytes",
        "total_freed_bytes", "packed_source_bytes", "prepared_live_bytes",
        "assembly_live_bytes",
    )
    capture_keys = {
        "capture_retained_append_copy_bytes": ("construction_copy", "retained_append_copy_bytes"),
        "capture_retained_peak_capacity_including_overlap_bytes": (
            "retained_growth", "retained_peak_capacity_including_overlap"),
    }

    def metrics_for(result):
        values = {key: result["instrumented_memory"][key] for key in memory_keys}
        counters = result["capture_ownership"]["counters"]
        values.update({name: counters[section][key]
                       for name, (section, key) in capture_keys.items()})
        return values

    baseline, candidate = map(metrics_for, results)
    return {
        "baseline_role": records[0]["role"],
        "candidate_role": records[1]["role"],
        "instrumented_timings_used_for_speedup": False,
        "metrics": {
            key: {"baseline": baseline[key], "candidate": candidate[key],
                  "delta": candidate[key] - baseline[key]}
            for key in baseline
        },
    }


def write_comparison_summary(path, records, dry_run):
    summary = comparison_summary(records)
    summary["dry_run"] = dry_run
    selfhost = next((row for row in reversed(records)
                     if row.get("role") == "current_source_selfhost"), None)
    result = selfhost.get("result") if selfhost else None
    summary["current_source_selfhost_passed"] = bool(result and result.get("success"))
    memory_records = [row for row in records
                      if row.get("role") in ("baseline_memory", "candidate_memory")]
    memory_summary = memory_comparison_summary(memory_records)
    if memory_summary is not None:
        summary["memory_summary"] = memory_summary
    path.write_text(json.dumps(summary, indent=2) + "\n")
    return summary


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline-bundle", type=Path, required=True)
    parser.add_argument("--candidate-bundle", type=Path, required=True)
    parser.add_argument("--baseline-source-root", type=Path, required=True,
                        help="frozen native source root identified by the baseline export")
    parser.add_argument("--selfhost-bundle", type=Path,
                        help="third run against this bundle's own source/oracle; defaults to candidate")
    parser.add_argument("--baseline-memory-bundle", type=Path,
                        help="optional light MEMD+CAPTURE export corresponding to the baseline")
    parser.add_argument("--candidate-memory-bundle", type=Path,
                        help="optional light MEMD+CAPTURE export corresponding to the candidate")
    parser.add_argument("--host", default=None, help="A6000 address; defaults to unique `ash -l` discovery")
    parser.add_argument("--volume", default="Development")
    parser.add_argument("--timeout", type=int, default=3600)
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args(argv)
    if ((args.host is not None and not runner.valid_host(args.host))
            or not re.fullmatch(r"[A-Za-z0-9_-]+", args.volume)):
        parser.error("Host and volume must be plain names; omit the volume colon")
    if not 1 <= args.timeout < 86400:
        parser.error("Timeout must be between 1 and 86399 seconds")
    if (args.baseline_memory_bundle is None) != (args.candidate_memory_bundle is None):
        parser.error("--baseline-memory-bundle and --candidate-memory-bundle must be provided together")
    baseline = args.baseline_bundle.resolve(strict=True)
    candidate = args.candidate_bundle.resolve(strict=True)
    selfhost = (args.selfhost_bundle or candidate).resolve(strict=True)
    manifest, oracle, cases = load_comparison(baseline, candidate, args.baseline_source_root)
    # Validate every planned run before connecting, including the independent
    # current-source self-host. Its bootstrap must be the tested candidate image.
    selfhost_manifest, selfhost_files, selfhost_oracle, selfhost_digest = runner.load_bundle(selfhost)
    if selfhost_files["opforge"] != cases[1][0]["opforge"]:
        raise ValueError("Current-source selfhost must use the same candidate bootstrap")
    memory_cases = []
    if args.baseline_memory_bundle is not None:
        memory_cases = load_memory_cases(
            baseline, candidate, args.baseline_memory_bundle.resolve(strict=True),
            args.candidate_memory_bundle.resolve(strict=True),
            cases[0][2]["workload_sha256"],
        )
    summary_root = Path(tempfile.mkdtemp(prefix="opforge-a6000-comparison-"))
    summary_path = summary_root / "comparison.json"
    records = []
    write_comparison_summary(summary_path, records, args.dry_run)
    print("Fixed workload: baseline source/package/command/Rust oracle; only the bootstrap changes.", flush=True)
    for files, case_digest, metadata in cases:
        result, local = runner.run_case(args, baseline, manifest, files, oracle, case_digest,
                                        run_kind=f"{metadata['comparison_role']} fixed-workload benchmark",
                                        metadata=metadata)
        records.append({"role": metadata["comparison_role"], "case_sha256": case_digest,
                        "local_results": str(local), "result": result, **metadata})
        write_comparison_summary(summary_path, records, args.dry_run)
    memory_records = []
    for bundle, memory_manifest, files, memory_oracle, case_digest, role, metadata in memory_cases:
        result, local = runner.run_case(args, bundle, memory_manifest, files, memory_oracle,
                                        case_digest, run_kind=f"{role} fixed-workload memory",
                                        metadata=metadata)
        record = {"role": role, "bundle": str(bundle), "case_sha256": case_digest,
                  "local_results": str(local), "result": result, **metadata}
        records.append(record)
        memory_records.append(record)
        write_comparison_summary(summary_path, records, args.dry_run)
    result, local = runner.run_case(args, selfhost, selfhost_manifest, selfhost_files,
                                    selfhost_oracle, selfhost_digest,
                                    metadata={"result_kind": "current_source_selfhost"})
    records.append({"role": "current_source_selfhost", "case_sha256": selfhost_digest,
                    "bundle": str(selfhost), "local_results": str(local), "result": result})
    summary = write_comparison_summary(summary_path, records, args.dry_run)
    print(json.dumps(summary, indent=2), flush=True)
    print(f"Comparison summary: {summary_root / 'comparison.json'}", flush=True)
    return 0 if args.dry_run or all(row["result"]["success"] for row in records) else 1


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        raise SystemExit(f"Stopped: {error}. No successful hardware comparison claim.")

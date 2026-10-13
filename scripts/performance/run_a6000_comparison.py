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
        if manifest.get("bootstrap_defines") or manifest.get("release_defines"):
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
    baseline, candidate = [row["result"] for row in records[:2]]
    summary["fixed_workload_parity_passed"] = all(
        result is not None and result["success"] for result in (baseline, candidate)
    )
    if summary["fixed_workload_parity_passed"]:
        first, second = [result["guest_overall_seconds"] for result in (baseline, candidate)]
        summary["guest_seconds_saved"] = first - second if first is not None and second is not None else None
        summary["guest_speedup"] = first / second if first is not None and second not in (None, 0) else None
    return summary


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline-bundle", type=Path, required=True)
    parser.add_argument("--candidate-bundle", type=Path, required=True)
    parser.add_argument("--baseline-source-root", type=Path, required=True,
                        help="frozen native source root identified by the baseline export")
    parser.add_argument("--selfhost-bundle", type=Path,
                        help="third run against this bundle's own source/oracle; defaults to candidate")
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
    candidate = args.candidate_bundle.resolve(strict=True)
    selfhost = (args.selfhost_bundle or candidate).resolve(strict=True)
    manifest, oracle, cases = load_comparison(baseline, candidate, args.baseline_source_root)
    # Validate every planned run before connecting, including the independent
    # current-source self-host. Its bootstrap must be the tested candidate image.
    selfhost_manifest, selfhost_files, selfhost_oracle, selfhost_digest = runner.load_bundle(selfhost)
    if selfhost_files["opforge"] != cases[1][0]["opforge"]:
        raise ValueError("Current-source selfhost must use the same candidate bootstrap")
    records = []
    print("Fixed workload: baseline source/package/command/Rust oracle; only the bootstrap changes.", flush=True)
    for files, case_digest, metadata in cases:
        result, local = runner.run_case(args, baseline, manifest, files, oracle, case_digest,
                                        run_kind=f"{metadata['comparison_role']} fixed-workload benchmark",
                                        metadata=metadata)
        records.append({"role": metadata["comparison_role"], "case_sha256": case_digest,
                        "local_results": str(local), "result": result, **metadata})
    result, local = runner.run_case(args, selfhost, selfhost_manifest, selfhost_files,
                                    selfhost_oracle, selfhost_digest,
                                    metadata={"result_kind": "current_source_selfhost"})
    records.append({"role": "current_source_selfhost", "case_sha256": selfhost_digest,
                    "bundle": str(selfhost), "local_results": str(local), "result": result})
    summary = comparison_summary(records)
    summary["dry_run"] = args.dry_run
    summary["current_source_selfhost_passed"] = result is not None and result["success"]
    summary_root = Path(tempfile.mkdtemp(prefix="opforge-a6000-comparison-"))
    (summary_root / "comparison.json").write_text(json.dumps(summary, indent=2) + "\n")
    print(json.dumps(summary, indent=2), flush=True)
    print(f"Comparison summary: {summary_root / 'comparison.json'}", flush=True)
    return 0 if args.dry_run or all(row["result"]["success"] for row in records) else 1


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        raise SystemExit(f"Stopped: {error}. No successful hardware comparison claim.")

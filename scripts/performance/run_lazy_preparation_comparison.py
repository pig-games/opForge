#!/usr/bin/env python3
"""Compare preparation observers on one frozen workload.

The workload case is produced by the existing full self-host exporter. All runs
reassemble its exact sources against a live Rust oracle through the maintained
native test. Timing images have no memory telemetry; memory images use identical
telemetry settings, with only OPFORGE_LAZY_AUDIT added to the candidate.
With --work-attribution, compare L1-only against L1+OPFORGE_LAZY_WORK instead.
With --baseline-images too, compare two work observers with identical build flags.
With --experiment-define, compare a selected capture experiment or observer.
The packing observer compares two eager writers sharing template inventory; its
memory candidate alone enables sampled packing clocks.
This is fixed-input audit qualification, not candidate-source self-host proof.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess

import memory_telemetry
import lazy_preparation_audit
import candidate_preparation_audit
import macro_packing_audit
import capture_lifetime_audit
import capture_telemetry
import run_a6000_selfhost as runner
from run_fs_uae_selfhost_comparison import TEST, reproducibility_metadata

CAPTURE_LIFETIME_EXPERIMENT = "OPFORGE_CAPTURE_LIFETIME_AUDIT"
PACKING_EXPERIMENT = "OPFORGE_MACRO_PACKING_AUDIT"
PACKING_BASELINE = ["OPFORGE_LAZY_AUDIT", "OPFORGE_LAZY_WORK"]
PACKING_REFERENCE = "eager macro packing with shared inventory"
EXPERIMENT_REFERENCES = {
    PACKING_EXPERIMENT: PACKING_REFERENCE,
    CAPTURE_LIFETIME_EXPERIMENT: "existing capture storage with shared inventory",
}

MEMORY_DEFINES = [
    "OPFORGE_DEBUG_CONTRACTS", "OPFORGE_MEMORY_TELEMETRY",
    "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT", "OPFORGE_PREPARATION_PROGRESS",
]

def read_checked(root, row):
    path = Path(row["path"])
    if path.is_absolute() or any(part in ("..", ".") for part in path.parts):
        raise ValueError(f"Unsafe workload path: {path}")
    data = (root / path).read_bytes()
    if runner.fnv(data) != row["digest"]:
        raise ValueError(f"Changed workload file: {path}")
    return data


def prepare(args):
    candidate = getattr(args, "candidate_attribution", False)
    timing = getattr(args, "candidate_timing", False)
    period = getattr(args, "candidate_sample_period", 64)
    seed = getattr(args, "candidate_phase_seed", 0)
    if period not in (64, 128, 256) or not 0 <= seed < period:
        raise ValueError("Invalid candidate sample period/phase seed")
    if (period != 64 or seed != 0) and not timing:
        raise ValueError("Nondefault candidate sample period/phase seed requires --candidate-timing")
    work_attribution = getattr(args, "work_attribution", False)
    baseline_images = getattr(args, "baseline_images", None)
    experiment_define = getattr(args, "experiment_define", None)
    lifetime = experiment_define == CAPTURE_LIFETIME_EXPERIMENT
    packing = experiment_define == PACKING_EXPERIMENT
    packing_period = getattr(args, "packing_sample_period", 64)
    packing_seed = getattr(args, "packing_phase_seed", 0)
    if packing_period not in (64, 128, 256) or not 0 <= packing_seed <= 255:
        raise ValueError("Invalid packing sample period/phase seed")
    if not packing and (packing_period != 64 or packing_seed != 0):
        raise ValueError("Packing sampling options require OPFORGE_MACRO_PACKING_AUDIT")
    if timing and not candidate:
        raise ValueError("--candidate-timing requires --candidate-attribution")
    if candidate and (not work_attribution or baseline_images is None):
        raise ValueError("--candidate-attribution requires --work-attribution and --baseline-images")
    if experiment_define and (candidate or work_attribution or baseline_images is not None):
        raise ValueError("--experiment-define cannot be combined with work-attribution or baseline images")
    if baseline_images is not None and not work_attribution:
        raise ValueError("Baseline images require --work-attribution")
    original = json.loads(args.workload_case.read_text())
    if original.get("schema") != 1:
        raise ValueError("Require current full-comparison case schema")
    root = args.workload_case.parent
    oracle = read_checked(root, original["oracle"])
    files = [(row["guest_path"], read_checked(root, row)) for row in original["files"]]
    if len({name for name, _ in files}) != len(files):
        raise ValueError("Duplicate workload guest path")
    build_manifest = json.loads((args.images / "images.json").read_text())
    image_manifest = {row["file"]: row for row in build_manifest["images"]}
    baseline_manifest = json.loads((baseline_images / "images.json").read_text()) if baseline_images else build_manifest
    baseline_rows = {row["file"]: row for row in baseline_manifest["images"]}
    digest = hashlib.sha256()
    for name, data in files:
        if not name.startswith("src/") or ".." in Path(name).parts:
            raise ValueError(f"Unsafe guest source path: {name}")
        for item in (name.encode(), data):
            digest.update(len(item).to_bytes(8, "big"))
            digest.update(item)
    workload_digest = digest.hexdigest()
    args.output.mkdir(parents=True, exist_ok=False)
    variants = []
    for repeat in range(args.repeats):
        order = ("reference", "audit") if repeat % 2 == 0 else ("audit", "reference")
        variants.extend((f"release-{repeat + 1}-{role}", role, False) for role in order)
    variants.extend((f"memory-{role}", role, True) for role in ("reference", "audit"))
    cases = []
    for label, role, memory in variants:
        directory = args.output / label
        directory.mkdir()
        image_role = role
        if experiment_define:
            image_role = "deferred" if role == "audit" else "reference"
        elif work_attribution:
            image_role = "work" if baseline_images or role == "audit" else "audit"
        image_name = f"{image_role}-{'memory' if memory else 'release'}.hunk"
        image_root = baseline_images if baseline_images and role == "reference" else args.images
        image = (image_root / image_name).read_bytes()
        defines = list(MEMORY_DEFINES) if memory else []
        if lifetime and memory:
            defines.append("OPFORGE_CAPTURE_TELEMETRY")
        if packing or lifetime:
            defines.extend(PACKING_BASELINE)
        if (role == "audit" and not experiment_define) or work_attribution:
            defines.append("OPFORGE_LAZY_AUDIT")
        if work_attribution and (role == "audit" or baseline_images):
            defines.append("OPFORGE_LAZY_WORK")
        if experiment_define and role == "audit":
            defines.append(experiment_define)
            if packing and memory:
                defines.append("OPFORGE_PACKING_TIMING")
                if packing_period != 64:
                    defines.append(f"OPFORGE_PACKING_SAMPLE_PERIOD={packing_period}")
                if packing_seed != 0:
                    defines.append(f"OPFORGE_PACKING_PHASE_SEED={packing_seed}")
        if candidate and role == "audit":
            defines.append("OPFORGE_LAZY_CANDIDATES")
            if timing and memory:
                defines.append("OPFORGE_CANDIDATE_TIMING")
                if period != 64:
                    defines.append(f"OPFORGE_CANDIDATE_SAMPLE_PERIOD={period}")
                if seed != 0:
                    defines.append(f"OPFORGE_CANDIDATE_PHASE_SEED={seed}")
        manifest = baseline_manifest if baseline_images and role == "reference" else build_manifest
        built = (baseline_rows if baseline_images and role == "reference" else image_manifest)[image_name]
        if built["fnv1a64"] != runner.fnv(image) or built["bytes"] != len(image):
            raise ValueError(f"Changed image: {image_name}")
        if sorted(built["defines"]) != sorted(defines):
            raise ValueError(f"Wrong measurement build defines: {image_name}")
        spec = {"schema": 1, "bootstrap_defines": defines, "files": [],
                "case_sha256": hashlib.sha256(workload_digest.encode() + image).hexdigest(),
                "metadata": {"result_kind": "lazy_audit_fixed_input_comparison",
                             "comparison_role": role, "measurement": "memory" if memory else "runtime",
                             "workload_sha256": workload_digest, "source_files": len(files),
                             "source_bytes": sum(len(data) for _, data in files),
                             "candidate_source_selfhost_proven": False,
                             "work_attribution_comparison": work_attribution,
                             "candidate_attribution_comparison": candidate,
                             "candidate_timing": timing,
                             "candidate_sample_period": period,
                             "candidate_phase_seed": seed,
                             "work_observer_revision_comparison": baseline_images is not None,
                             "macro_packing_fixed_input_comparison": packing,
                             "capture_lifetime_fixed_input_comparison": lifetime,
                             "packing_sample_period": packing_period,
                             "packing_phase_seed": packing_seed,
                             "experiment_define": experiment_define,
                             "reference_config": EXPERIMENT_REFERENCES.get(
                                 experiment_define, "eager macro services" if experiment_define else "ordinary release"),
                             "frozen_workload_case": str(args.workload_case),
                             "bootstrap_source_fnv1a64": built.get("native_source_fnv1a64", manifest.get("native_source_fnv1a64")),
                             "memory_profile": os.environ.get("OPFORGE_FS_UAE_MEMORY_PROFILE", "existing")}}
        for key, name, data in (("image", "bootstrap.hunk", image), ("oracle", "exported-oracle.hunk", oracle)):
            (directory / name).write_bytes(data)
            spec[key] = {"path": name, "digest": runner.fnv(data)}
        for name, data in files:
            relative = Path("inputs") / name
            target = directory / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(data)
            spec["files"].append({"path": relative.as_posix(), "guest_path": name, "digest": runner.fnv(data)})
        path = directory / "case.json"
        path.write_text(json.dumps(spec, indent=2, sort_keys=True) + "\n")
        cases.append(path)
    (args.output / "plan.json").write_text(json.dumps({"test": TEST, "cases": [str(p) for p in cases],
        "workload_sha256": workload_digest, "reproducibility": reproducibility_metadata(args)}, indent=2) + "\n")
    return cases


def validate_packing_configuration(report, metadata):
    if (report["sample_period"] != metadata.get("packing_sample_period", 64)
            or report["phase_seed"] != metadata.get("packing_phase_seed", 0)
            or not report["timing_enabled"]):
        raise ValueError("Packing report sampling configuration differs from image manifest")


def validate_packing_work(report, work):
    binder_calls = sum(report[section][field]
                       for section in ("gross", "outside_writer_binds")
                       for field in ("names", "members"))
    if binder_calls != work["metrics"]["total"]["bind_calls"]:
        raise ValueError("Packing/work name and member bind totals disagree")
    if report["gross"]["stored_bytes"] != work["template_inventory"]["pool_used_bytes"]["body_bytes"]:
        raise ValueError("Packing/work stored body byte totals disagree")


def validate_shared_inventory_comparison(memory, label):
    previous, current = (memory[role]["audit"]["work_attribution"] for role in ("reference", "audit"))
    if previous["metrics"] != current["metrics"]:
        raise ValueError(f"{label} observer changed work metrics on identical input")
    for key in ("definitions", "consumer_relationships", "pool_used_bytes", "attributed_payload_bytes", "unattributed_pool_bytes"):
        if previous["template_inventory"][key] != current["template_inventory"][key]:
            raise ValueError(f"{label} observer changed template {key}")


def validate_candidate_configuration(report, metadata):
    if (report["sample_period"] != metadata.get("candidate_sample_period", 64)
            or report["phase_seed"] != metadata.get("candidate_phase_seed", 0)):
        raise ValueError("Candidate report sampling configuration differs from image manifest")
    if not metadata.get("candidate_timing") and (report["frequency"] or report["timed_calls"]):
        raise ValueError("Candidate report unexpectedly enabled timing")


def validate_candidate_comparison(memory):
    previous, current = (memory[role]["audit"]["work_attribution"] for role in ("reference", "audit"))
    for key in ("definitions", "consumer_relationships"):
        if previous["template_inventory"][key] != current["template_inventory"][key]:
            raise ValueError(f"Candidate observer changed template {key}")
    report = memory["audit"]["candidate_attribution"]
    total = current["metrics"]["total"]
    discarded = report["metrics"]["not_retained"]
    gross = report["metrics"]["total"]
    expected_records = total["capture_header_bytes"] // current["capture_header_bytes"]
    if report["records_created"] - discarded["records"] != expected_records:
        raise ValueError("Candidate/work created record totals disagree")
    if report["plan_bytes_created"] - discarded["plan_bytes"] != total["capture_plan_bytes"]:
        raise ValueError("Candidate/work created plan byte totals disagree")
    for candidate_field, work_field in (("capture_bytes", "capture_bytes"),
                                        ("token_bytes", "capture_token_bytes"),
                                        ("lexeme_bytes", "capture_lexeme_bytes")):
        if gross[candidate_field] - discarded[candidate_field] != total[work_field]:
            raise ValueError(f"Candidate/work totals disagree: {candidate_field}")
    for field in ("candidate_calls", "descriptor_calls", "fragment_calls", "plan_create_calls"):
        if report["metrics"]["total"][field] != total[field]:
            raise ValueError(f"Candidate/work totals disagree: {field}")


def execute(args, cases):
    records = []
    for path in cases:
        print(f"Running {path.parent.name}", flush=True)
        with (path.parent / "test.log").open("w") as log:
            result = subprocess.run([str(args.test_binary), TEST, "--exact", "--ignored", "--nocapture", "--test-threads=1"],
                env=dict(os.environ, OPFORGE_FULL_COMPARISON_CASE=str(path)), stdout=log, stderr=subprocess.STDOUT)
        receipt = path.parent / "result/native-result.json"
        if not receipt.exists():
            row = {"success": False, "error": f"No completed result; inspect {path.parent / 'test.log'}"}
        else:
            row = json.loads(receipt.read_text())
            if row["metadata"]["measurement"] == "memory":
                try:
                    row["memory"] = memory_telemetry.decode_memory_telemetry((receipt.parent / "memory.bin").read_bytes())
                    row["success"] = row["success"] and not row["memory"]["diagnostics"]
                    if (row["metadata"].get("capture_lifetime_fixed_input_comparison")
                            or row["metadata"].get("macro_packing_fixed_input_comparison")
                            or row["metadata"].get("work_attribution_comparison")
                            or row["metadata"]["comparison_role"] == "audit"):
                        row["audit"] = lazy_preparation_audit.decode((receipt.parent / "assembly.stdout").read_text())
                        row["success"] = row["success"] and row["audit"]["observer_complete"]
                        if (row["metadata"].get("macro_packing_fixed_input_comparison")
                                or row["metadata"].get("capture_lifetime_fixed_input_comparison")):
                            work = row["audit"].get("work_attribution", {})
                            if not work.get("accounting_complete") or not work.get("template_inventory", {}).get("complete"):
                                raise ValueError("Incomplete shared work/template inventory")
                        if (row["metadata"].get("candidate_attribution_comparison")
                                and row["metadata"]["comparison_role"] == "audit"):
                            row["candidate_attribution"] = candidate_preparation_audit.decode(
                                (receipt.parent / "assembly.stdout").read_text())
                            validate_candidate_configuration(row["candidate_attribution"], row["metadata"])
                            if not row["candidate_attribution"]["complete"]:
                                raise ValueError("Incomplete candidate attribution")
                        if (row["metadata"].get("macro_packing_fixed_input_comparison")
                                and row["metadata"]["comparison_role"] == "audit"):
                            row["macro_packing"] = macro_packing_audit.decode(
                                (receipt.parent / "assembly.stdout").read_text(),
                                row["audit"]["work_attribution"]["template_inventory"])
                            validate_packing_configuration(row["macro_packing"], row["metadata"])
                            validate_packing_work(row["macro_packing"], row["audit"]["work_attribution"])
                        if row["metadata"].get("capture_lifetime_fixed_input_comparison"):
                            row["capture_ownership"] = capture_telemetry.decode_capture_telemetry(
                                (receipt.parent / "assembly.stdout").read_text(),
                                record_header_bytes=work["capture_header_bytes"])["capture_summary"]
                            if row["capture_ownership"] is None or row["capture_ownership"]["boundary_mask"] != 7:
                                raise ValueError("Incomplete capture ownership report")
                            if row["metadata"]["comparison_role"] == "audit":
                                row["capture_lifetime"] = capture_lifetime_audit.decode(
                                    (receipt.parent / "assembly.stdout").read_text(), row["memory"],
                                    row["audit"], row["capture_ownership"])
                        inventory = row["audit"].get("work_attribution", {}).get("template_inventory")
                        if inventory is not None and not inventory["complete"]:
                            raise ValueError("Incomplete template inventory")
                        needs_work_report = (
                            row["metadata"].get("macro_packing_fixed_input_comparison")
                            or row["metadata"].get("capture_lifetime_fixed_input_comparison")
                            or row["metadata"].get("work_observer_revision_comparison")
                            or (row["metadata"].get("work_attribution_comparison")
                                and row["metadata"]["comparison_role"] == "audit")
                        )
                        if needs_work_report:
                            row["success"] = row["success"] and row["audit"]["work_attribution"]["accounting_complete"]
                except (OSError, ValueError) as error:
                    row.update(success=False, error=str(error))
        row["test_exit_code"] = result.returncode
        row["success"] = bool(row["success"] and result.returncode == 0)
        (path.parent / "result.json").write_text(json.dumps(row, indent=2, sort_keys=True) + "\n")
        records.append(row)
        (args.output / "results.json").write_text(json.dumps(records, indent=2, sort_keys=True) + "\n")
        print(f"{path.parent.name}: {'passed' if row['success'] else 'failed'}", flush=True)
        if not row["success"] and getattr(args, "stop_on_failure", False):
            print("Stopped comparison after failure; no performance summary is valid.", flush=True)
            return False
    if not all(row["success"] for row in records):
        return False
    runtime = {role: [row["start_to_done_host_seconds"] for row in records
               if row["metadata"]["measurement"] == "runtime" and row["metadata"]["comparison_role"] == role]
               for role in ("reference", "audit")}
    means = {role: sum(values) / len(values) for role, values in runtime.items()}
    memory = {row["metadata"]["comparison_role"]: row for row in records if row["metadata"]["measurement"] == "memory"}
    if getattr(args, "baseline_images", None):
        previous = memory["reference"]["audit"]["work_attribution"]["metrics"]["total"]
        current = memory["audit"]["audit"]["work_attribution"]["metrics"]["total"]
        if getattr(args, "candidate_attribution", False):
            validate_candidate_comparison(memory)
            excluded = {"capture_header_bytes", "capture_padding_bytes", "capture_bytes"}
            previous = {key: value for key, value in previous.items() if key not in excluded}
            current = {key: value for key, value in current.items() if key not in excluded}
        if previous != current:
            raise ValueError("Observer revision changed global work totals on identical input")
    if getattr(args, "experiment_define", None) == PACKING_EXPERIMENT:
        validate_shared_inventory_comparison(memory, "Packing")
    if getattr(args, "experiment_define", None) == CAPTURE_LIFETIME_EXPERIMENT:
        validate_shared_inventory_comparison(memory, "Capture lifetime")
        if memory["reference"]["capture_ownership"] != memory["audit"]["capture_ownership"]:
            raise ValueError("Capture lifetime observer changed existing capture ownership accounting")
    baseline_name = "ordinary release"
    if getattr(args, "experiment_define", None):
        baseline_name = EXPERIMENT_REFERENCES.get(args.experiment_define, "eager macro services")
    if getattr(args, "work_attribution", False):
        baseline_name = "L1 audit"
    if getattr(args, "baseline_images", None):
        baseline_name = "previous work observer"
    summary = {"runtime_samples_seconds": runtime, "runtime_means_seconds": means,
               "audit_runtime_delta_seconds": means["audit"] - means["reference"],
               "audit_runtime_delta_percent": 100 * (means["audit"] / means["reference"] - 1),
               "memory": {role: row["memory"] for role, row in memory.items()},
               "audit_peak_owned_delta_bytes": memory["audit"]["memory"]["peak_owned_bytes"] - memory["reference"]["memory"]["peak_owned_bytes"],
               "audit_allocated_delta_bytes": memory["audit"]["memory"]["total_allocated_bytes"] - memory["reference"]["memory"]["total_allocated_bytes"],
               "linked_static_bytes": {role: row["linked_static_reserved_bytes"] for role, row in memory.items()},
               "audit_linked_static_delta_bytes": memory["audit"]["linked_static_reserved_bytes"] - memory["reference"]["linked_static_reserved_bytes"],
               "audit": memory["audit"].get("audit"),
               "baseline_audit": memory["reference"].get("audit"),
               "candidate_attribution": memory["audit"].get("candidate_attribution"),
               "macro_packing": memory["audit"].get("macro_packing"),
               "capture_lifetime": memory["audit"].get("capture_lifetime"),
               "capture_ownership": {role: row.get("capture_ownership") for role, row in memory.items()},
               "baseline": baseline_name,
               "evidence_limit": ("fixed frozen input; instrumentation times are diagnostic; "
                                  "not current-source self-host proof; no early capture release")}
    (args.output / "summary.json").write_text(json.dumps(summary, indent=2, sort_keys=True) + "\n")
    # Keep full bounded tables in the artifact, not repeated in terminal/model output.
    console_keys = ("baseline", "runtime_samples_seconds", "runtime_means_seconds",
                    "audit_runtime_delta_seconds", "audit_runtime_delta_percent",
                    "audit_peak_owned_delta_bytes", "audit_allocated_delta_bytes",
                    "audit_linked_static_delta_bytes", "evidence_limit")
    print(json.dumps(dict(summary_file=str(args.output / "summary.json"),
                          **{key: summary[key] for key in console_keys}), indent=2))
    return True


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workload-case", type=Path, required=True)
    parser.add_argument("--images", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--test-binary", type=Path)
    parser.add_argument("--repeats", type=int, default=2)
    parser.add_argument("--prepare-only", action="store_true")
    parser.add_argument("--stop-on-failure", action="store_true",
                        help="retain completed evidence and stop before remaining comparison runs on failure")
    parser.add_argument("--work-attribution", action="store_true",
                        help="measure marginal attribution overhead: audit images versus work images")
    parser.add_argument("--baseline-images", type=Path,
                        help="with --work-attribution, compare saved work images against current work images")
    parser.add_argument("--experiment-define", choices=(PACKING_EXPERIMENT, CAPTURE_LIFETIME_EXPERIMENT),
                        help="compare reference images with the selected preparation experiment or observer")
    parser.add_argument("--candidate-attribution", action="store_true", help="compare passive candidate attribution against baseline work images")
    parser.add_argument("--candidate-timing", action="store_true", help="sample candidate preparation only in the candidate memory image")
    parser.add_argument("--candidate-sample-period", type=int, choices=(64, 128, 256), default=64)
    parser.add_argument("--candidate-phase-seed", type=int, default=0)
    parser.add_argument("--packing-sample-period", type=int, choices=(64, 128, 256), default=64)
    parser.add_argument("--packing-phase-seed", type=int, default=0)
    args = parser.parse_args()
    for key, value in vars(args).items():
        if isinstance(value, Path):
            setattr(args, key, value.resolve())
    if not 0 <= args.packing_phase_seed <= 255:
        parser.error("packing phase seed must be 0..255")
    if (args.packing_sample_period != 64 or args.packing_phase_seed != 0) and args.experiment_define != PACKING_EXPERIMENT:
        parser.error("packing sampling requires OPFORGE_MACRO_PACKING_AUDIT")
    if not 1 <= args.repeats <= 4:
        parser.error("repeats must be 1..4")
    if not 0 <= args.candidate_phase_seed < args.candidate_sample_period:
        parser.error("candidate phase seed must be 0..sample-period-1")
    if (args.candidate_sample_period != 64 or args.candidate_phase_seed != 0) and not args.candidate_timing:
        parser.error("nondefault candidate sampling requires --candidate-timing")
    if args.candidate_timing and not args.candidate_attribution:
        parser.error("--candidate-timing requires --candidate-attribution")
    if args.candidate_attribution and (not args.work_attribution or not args.baseline_images or args.experiment_define):
        parser.error("--candidate-attribution requires work attribution and baseline images and excludes experiment define")
    if args.baseline_images and not args.work_attribution:
        parser.error("--baseline-images requires --work-attribution")
    if args.experiment_define and (args.work_attribution or args.baseline_images):
        parser.error("--experiment-define cannot be combined with work-attribution or baseline images")
    if not args.prepare_only and args.test_binary is None:
        parser.error("execution requires --test-binary")
    target = Path(__file__).resolve().parents[2] / "target"
    if args.output == target or target in args.output.parents:
        parser.error("preserve evidence outside target")
    cases = prepare(args)
    if not args.prepare_only:
        return 0 if execute(args, cases) else 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

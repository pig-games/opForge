#!/usr/bin/env python3
"""Compare preparation observers or first-use binding on one frozen workload.

The workload case is produced by the existing full self-host exporter. All runs
reassemble its exact sources against a live Rust oracle through the maintained
native test. Timing images have no memory telemetry; memory images use identical
telemetry settings, with only OPFORGE_LAZY_AUDIT added to the candidate.
With --work-attribution, compare L1-only against L1+OPFORGE_LAZY_WORK instead.
With --baseline-images too, compare two work observers with identical build flags.
With --experiment-define, compare eager and first-use macro-plan binding.
This is fixed-input audit qualification, not candidate-source self-host proof.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess

import memory_telemetry
import lazy_preparation_audit
import run_a6000_selfhost as runner
from run_fs_uae_selfhost_comparison import TEST, reproducibility_metadata

MEMORY_DEFINES = [
    "OPFORGE_DEBUG_CONTRACTS", "OPFORGE_MEMORY_TELEMETRY",
    "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT", "OPFORGE_PREPARATION_PROGRESS",
]
DEFERRED_PROGRESS = re.compile(
    r"^progress p=([0-9a-fA-F]{8}) f=([0-9a-fA-F]{8}) "
    r"l=([0-9a-fA-F]{8}) r=([0-9a-fA-F]{8}) m=([0-9a-fA-F]{8})$"
)
DEFERRED_PHASE_PREFIX = re.compile(r"^progress p=([0-9a-fA-F]{8})\b")


def decode_deferred_binding(stdout, total_allocated_bytes):
    rows = {}
    for line in stdout.splitlines():
        prefix = DEFERRED_PHASE_PREFIX.match(line)
        if prefix is None or int(prefix.group(1), 16) not in (3300, 3301):
            continue
        match = DEFERRED_PROGRESS.fullmatch(line)
        if match is None:
            raise ValueError("Malformed deferred binding progress row")
        phase, first, second, third, _live_heap = (int(value, 16) for value in match.groups())
        if phase in rows:
            raise ValueError(f"Duplicate deferred binding phase {phase}")
        # m is the existing progress ABI live-heap measurement, not reserved.
        if phase == 3301 and third != 0:
            raise ValueError("Deferred binding phase 3301 reserved field is nonzero")
        rows[phase] = (first, second, third)
    if 3300 not in rows or 3301 not in rows:
        raise ValueError("Missing deferred binding progress phases")
    created, bound, hits = rows[3300]
    recipe_bytes, normal_plan_used, _ = rows[3301]
    if bound > created:
        raise ValueError("Deferred binding count exceeds created plans")
    if recipe_bytes > total_allocated_bytes:
        raise ValueError("Deferred recipe bytes exceed total allocated bytes")
    return {"created": created, "bound": bound, "hits": hits,
            "recipe_bytes": recipe_bytes, "normal_plan_used": normal_plan_used}


def read_checked(root, row):
    path = Path(row["path"])
    if path.is_absolute() or any(part in ("..", ".") for part in path.parts):
        raise ValueError(f"Unsafe workload path: {path}")
    data = (root / path).read_bytes()
    if runner.fnv(data) != row["digest"]:
        raise ValueError(f"Changed workload file: {path}")
    return data


def prepare(args):
    work_attribution = getattr(args, "work_attribution", False)
    baseline_images = getattr(args, "baseline_images", None)
    experiment_define = getattr(args, "experiment_define", None)
    if experiment_define and (work_attribution or baseline_images is not None):
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
        if (role == "audit" and not experiment_define) or work_attribution:
            defines.append("OPFORGE_LAZY_AUDIT")
        if work_attribution and (role == "audit" or baseline_images):
            defines.append("OPFORGE_LAZY_WORK")
        if experiment_define and role == "audit":
            defines.append(experiment_define)
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
                             "work_observer_revision_comparison": baseline_images is not None,
                             "macro_binding_fixed_input_comparison": experiment_define is not None,
                             "experiment_define": experiment_define,
                             "frozen_workload_case": str(args.workload_case),
                             "bootstrap_source_fnv1a64": manifest.get("native_source_fnv1a64"),
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
                    if (row["metadata"].get("macro_binding_fixed_input_comparison")
                            and row["metadata"]["comparison_role"] == "audit"):
                        row["deferred_binding"] = decode_deferred_binding(
                            (receipt.parent / "assembly.stdout").read_text(),
                            row["memory"]["total_allocated_bytes"],
                        )
                    if (row["metadata"].get("work_attribution_comparison")
                            or (row["metadata"]["comparison_role"] == "audit"
                                and not row["metadata"].get("macro_binding_fixed_input_comparison"))):
                        row["audit"] = lazy_preparation_audit.decode((receipt.parent / "assembly.stdout").read_text())
                        row["success"] = row["success"] and row["audit"]["observer_complete"]
                        inventory = row["audit"].get("work_attribution", {}).get("template_inventory")
                        if inventory is not None and not inventory["complete"]:
                            raise ValueError("Incomplete template inventory")
                        needs_work_report = (
                            row["metadata"].get("work_observer_revision_comparison")
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
        if previous != current:
            raise ValueError("Observer revision changed global work totals on identical input")
    baseline_name = "ordinary release"
    if getattr(args, "experiment_define", None):
        baseline_name = "eager macro binding"
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
               "deferred_binding": memory["audit"].get("deferred_binding"),
               "baseline": baseline_name,
               "evidence_limit": ("fixed frozen input; instrumentation times are diagnostic; "
                                  "not current-source self-host proof; no early capture release")}
    (args.output / "summary.json").write_text(json.dumps(summary, indent=2, sort_keys=True) + "\n")
    print(json.dumps(summary, indent=2))
    return True


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workload-case", type=Path, required=True)
    parser.add_argument("--images", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--test-binary", type=Path)
    parser.add_argument("--repeats", type=int, default=2)
    parser.add_argument("--prepare-only", action="store_true")
    parser.add_argument("--work-attribution", action="store_true",
                        help="measure marginal attribution overhead: audit images versus work images")
    parser.add_argument("--baseline-images", type=Path,
                        help="with --work-attribution, compare saved work images against current work images")
    parser.add_argument("--experiment-define", choices=("OPFORGE_DEFERRED_MACRO_PLANS",),
                        help="compare eager reference images with the deferred macro-plan experiment")
    args = parser.parse_args()
    for key, value in vars(args).items():
        if isinstance(value, Path):
            setattr(args, key, value.resolve())
    if not 1 <= args.repeats <= 4:
        parser.error("repeats must be 1..4")
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

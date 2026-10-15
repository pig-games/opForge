#!/usr/bin/env python3
"""Prepare and serially run five full FS-UAE cases from validated self-host exports.

Use --prepare-only to inspect the immutable cases. Execution uses an already
built asm test binary and the existing OPFORGE_FS_UAE_* environment unchanged.
Results and copied captures persist under --output, outside the build cache.
The live Rust oracle honors the staged source's .output sections and filename,
as export validation does; it does not inject an additional CLI Hunk request.
"""
import argparse
import hashlib
import json
import os
import shutil
from pathlib import Path
import subprocess

import run_a6000_comparison as comparison
import run_a6000_selfhost as runner

TEST = "fs_uae_smoke::native_full_selfhost_comparison::native_full_selfhost_comparison_fs_uae"


REPRODUCIBILITY_ENV = (
    "OPFORGE_FS_UAE_SMOKE", "OPFORGE_FS_UAE_BIN", "OPFORGE_FS_UAE_ARGS",
    "OPFORGE_FS_UAE_CONFIG_TEMPLATE", "OPFORGE_FS_UAE_MEMORY_PROFILE",
    "OPFORGE_FS_UAE_GUEST_STACK_BYTES",
    "OPFORGE_FS_UAE_TIMEOUT_MS", "OPFORGE_FS_UAE_POST_START_TIMEOUT_MS", "OPFORGE_FS_UAE_POLL_MS",
    "OPFORGE_FS_UAE_START_FILE", "OPFORGE_FS_UAE_READY_FILE", "OPFORGE_FS_UAE_EXIT_CODE_FILE",
    "OPFORGE_FS_UAE_STDOUT_FILE", "OPFORGE_FS_UAE_STDERR_FILE",
    "OPFORGE_FS_UAE_CONSOLE_DEBUGGER", "OPFORGE_FS_UAE_CONSOLE_DEBUGGER_AUTOMATE",
    "OPFORGE_FS_UAE_CONSOLE_DEBUGGER_HARNESS",
)


def reproducibility_metadata(args):
    environment = {name: os.environ.get(name) for name in REPRODUCIBILITY_ENV}
    record = {"environment": environment, "unset_values": "use existing runner defaults"}
    template = environment["OPFORGE_FS_UAE_CONFIG_TEMPLATE"]
    if template:
        path = Path(template).resolve()
        data = path.read_bytes()
        settings = {}
        # Preserve only hardware settings; the full template is identified by its digest.
        keys = {"amiga_model", "cpu", "fpu", "mmu", "jit_compiler", "cpu_speed",
                "chip_memory", "fast_memory", "zorro_iii_memory", "slow_memory",
                "accuracy", "cycle_exact", "video_sync", "graphics_card", "graphics_memory"}
        for line in data.decode("utf-8", errors="replace").splitlines():
            key, separator, value = line.partition("=")
            if separator and key.strip() in keys:
                settings[key.strip()] = value.strip()
        record["config_template"] = {"path": str(path), "sha256": hashlib.sha256(data).hexdigest(),
                                     "bytes": len(data), "hardware_settings": settings}
    for name, value in (("fs_uae_binary", environment["OPFORGE_FS_UAE_BIN"] or "fs-uae"),
                        ("test_binary", getattr(args, "test_binary", None))):
        resolved = shutil.which(str(value)) if value else None
        if resolved and Path(resolved).is_file():
            path = Path(resolved).resolve()
            record[name] = {"path": str(path), "sha256": hashlib.sha256(path.read_bytes()).hexdigest()}
        else:
            record[name] = {"requested": str(value) if value else None, "resolved": None}
    source = Path(__file__).resolve().parents[2] / "crates/opforge-asm/src/fs_uae_smoke.rs"
    record["runner_source_sha256"] = hashlib.sha256(source.read_bytes()).hexdigest()
    record["existing_profile_zorro_iii_memory_kib"] = 65536
    return record


def prepare_cases(args):
    manifest, oracle, release_cases = comparison.load_comparison(
        args.baseline_bundle, args.candidate_bundle, args.baseline_source_root)
    if manifest.get("bootstrap_package_storage") != "embedded":
        raise ValueError("Full FS-UAE comparison currently requires embedded bootstraps")
    memory_cases = comparison.load_memory_cases(
        args.baseline_bundle, args.candidate_bundle, args.baseline_memory_bundle,
        args.candidate_memory_bundle, release_cases[0][2]["workload_sha256"])
    candidate_manifest, candidate_files, candidate_oracle, candidate_digest = runner.load_bundle(args.candidate_bundle)
    if candidate_files["opforge"] != candidate_oracle:
        raise ValueError("Candidate self-host release image differs from its own oracle")
    cases = [(row[2]["comparison_role"] + "_release", manifest, row[0], oracle, row[1], row[2])
             for row in release_cases]
    cases.extend((role, own_manifest, files, own_oracle, digest, metadata)
                 for _, own_manifest, files, own_oracle, digest, role, metadata in memory_cases)
    cases.append(("candidate_selfhost", candidate_manifest, candidate_files, candidate_oracle,
                  candidate_digest, {"result_kind": "candidate_source_selfhost",
                                     "bootstrap_bundle": str(args.candidate_bundle)}))
    reproducibility = reproducibility_metadata(args)
    specifications = []
    args.output.mkdir(parents=True, exist_ok=False)
    for index, (role, own_manifest, files, own_oracle, digest, metadata) in enumerate(cases):
        directory = args.output / f"{index + 1}-{role}"
        directory.mkdir()
        source_files = {name: data for name, data in files.items() if name.startswith("src/")}
        if "src/experimental/opforge_compact_cli.asm" not in source_files:
            raise ValueError("Full workload requires the mapped compact CLI entry")
        spec = {"schema": 1, "case_sha256": digest, "metadata": metadata,
                "bootstrap_defines": own_manifest.get("bootstrap_defines", []),
                "record_header_bytes": own_manifest.get("_validated_capture_record_header_bytes"),
                "telemetry_manifest": {key: own_manifest[key] for key in (
                    "bootstrap_defines", "bootstrap_package_storage", "embedded_packages",
                    "output_package_storage", "output_embedded_packages", "native_validation",
                    "native_protocol_completed", "native_assembler_exit_code",
                    "native_start_to_done_host_seconds") if key in own_manifest},
                "files": []}
        for key, filename, data in (("image", "bootstrap.hunk", files["opforge"]),
                                    ("oracle", "exported-oracle.hunk", own_oracle)):
            (directory / filename).write_bytes(data)
            spec[key] = {"path": filename, "digest": runner.fnv(data)}
        for name, data in sorted(source_files.items()):
            relative = Path("inputs") / name
            destination = directory / relative
            destination.parent.mkdir(parents=True, exist_ok=True)
            destination.write_bytes(data)
            spec["files"].append({"guest_path": name, "path": relative.as_posix(), "digest": runner.fnv(data)})
        spec["staged_inputs_sha256"] = comparison.digest_parts([
            part for row in spec["files"] for part in
            (row["guest_path"].encode(), (directory / row["path"]).read_bytes())])
        path = directory / "case.json"
        path.write_text(json.dumps(spec, indent=2, sort_keys=True) + "\n")
        specifications.append(path)
    (args.output / "plan.json").write_text(json.dumps({"cases": [str(p) for p in specifications],
                                                       "test": TEST, "reproducibility": reproducibility}, indent=2) + "\n")
    return specifications


def execute_cases(args, specifications):
    records = []
    for path in specifications:
        spec = json.loads(path.read_text())
        environment = dict(os.environ, OPFORGE_FULL_COMPARISON_CASE=str(path))
        print(f"Running {path.parent.name}", flush=True)
        with (path.parent / "test.log").open("w") as log:
            process = subprocess.run([str(args.test_binary), TEST, "--exact", "--ignored",
                                      "--nocapture", "--test-threads=1"],
                                     env=environment, stdout=log, stderr=subprocess.STDOUT)
        result_path = path.parent / "result/native-result.json"
        if result_path.is_file():
            result = json.loads(result_path.read_text())
            if spec["bootstrap_defines"]:
                runner.add_telemetry(result, result_path.parent,
                                     spec["telemetry_manifest"],
                                     record_header_bytes=spec["record_header_bytes"])
                memory = result.get("instrumented_memory")
                if memory:
                    result["linked_static_plus_peak_owned_bytes"] = (
                        result["linked_static_reserved_bytes"] + memory["peak_owned_bytes"])
        else:
            result = {"success": False, "error": "Native result unavailable; inspect test.log and failure.txt"}
        result["test_exit_code"] = process.returncode
        result["success"] = result["success"] and process.returncode == 0
        result["case_json_sha256"] = hashlib.sha256(path.read_bytes()).hexdigest()
        (path.parent / "result.json").write_text(json.dumps(result, indent=2, sort_keys=True) + "\n")
        records.append({"case": path.parent.name, "result": result})
        (args.output / "results.json").write_text(json.dumps(records, indent=2, sort_keys=True) + "\n")
        print(f"{path.parent.name}: {'passed' if result['success'] else 'failed'}", flush=True)
    return all(row["result"]["success"] for row in records)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("baseline-bundle", "candidate-bundle", "baseline-source-root",
                 "baseline-memory-bundle", "candidate-memory-bundle", "output"):
        parser.add_argument("--" + name, type=Path, required=True)
    parser.add_argument("--test-binary", type=Path)
    parser.add_argument("--prepare-only", action="store_true")
    args = parser.parse_args()
    for name, value in vars(args).items():
        if isinstance(value, Path):
            setattr(args, name, value.resolve())
    if not args.prepare_only and args.test_binary is None:
        parser.error("execution requires --test-binary")
    if args.output == Path.cwd() / "target" or Path.cwd() / "target" in args.output.parents:
        parser.error("durable output must be outside target")
    specifications = prepare_cases(args)
    print(f"Prepared five full cases: {args.output / 'plan.json'}", flush=True)
    return 0 if args.prepare_only or execute_cases(args, specifications) else 1


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""Prepare eight serial A6000 runs; execute only from the user's macOS Terminal.

The A6000 RAM is unrestricted. These measurements make no 8 MiB fit claim.
An existing state journal is never resumed or retried automatically.
"""
import argparse
import copy
from datetime import datetime, timezone
import json
import os
from pathlib import Path
import platform
import re
import shlex
import statistics
import subprocess
import tempfile
import time

import run_a6000_comparison as comparison

runner = comparison.runner


def atomic_json(path, value):
    """Durably replace a journal, including the containing directory entry."""
    descriptor, temporary = tempfile.mkstemp(prefix=path.name + ".", dir=path.parent)
    try:
        with os.fdopen(descriptor, "w") as stream:
            json.dump(value, stream, indent=2)
            stream.write("\n")
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
        descriptor = os.open(path.parent, os.O_RDONLY)
        try:
            os.fsync(descriptor)
        finally:
            os.close(descriptor)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)


def checked_file(root, row):
    relative = Path(row["path"])
    if relative.is_absolute() or ".." in relative.parts:
        raise ValueError("Frozen case paths must be relative and contained")
    path = (root / relative).resolve(strict=True)
    if not path.is_relative_to(root.resolve()):
        raise ValueError("Frozen case path escapes its directory")
    data = path.read_bytes()
    if runner.fnv(data) != row["digest"]:
        raise ValueError(f"Frozen case checksum mismatch: {relative}")
    return data


def prepare_cases(args):
    release = runner.load_bundle(args.release_bundle)
    memory = runner.load_bundle(args.memory_bundle)
    manifest, current_files, current_oracle, _ = release
    memory_manifest, memory_files, memory_oracle, _ = memory
    for own in (manifest, memory_manifest):
        if own.get("kind") != "compact-self-host-local-export" or own.get("manifest_version") != 1:
            raise ValueError("Require genuine current compact self-host exports")
    if manifest.get("bootstrap_defines") or current_files["opforge"] != current_oracle:
        raise ValueError("Current release bootstrap must equal its own release oracle")
    flags = memory_manifest.get("bootstrap_defines", [])
    if len(flags) != len(set(flags)) or set(flags) != comparison.MEMORY_DEFINES:
        raise ValueError("Memory export requires exactly light MEMD+CAPTURE and preparation progress")
    if (memory_oracle != current_oracle or memory_files["opforge"] == current_files["opforge"]
            or {k: v for k, v in memory_files.items() if k != "opforge"}
            != {k: v for k, v in current_files.items() if k != "opforge"}):
        raise ValueError("Memory export must match current release source, package and oracle")
    for key in ("source_manifest_digest", "command", "runtime_package_digest",
                "bootstrap_package_storage", "output_package_storage", "embedded_packages",
                "output_embedded_packages"):
        if manifest.get(key) != memory_manifest.get(key):
            raise ValueError(f"Memory and release exports differ in {key}")
    spec = json.loads(args.frozen_case.read_text())
    if spec.get("schema") != 1:
        raise ValueError("Require schema 1 frozen FS-UAE comparison case")
    root = args.frozen_case.parent
    checked_file(root, spec["image"])
    oracle = checked_file(root, spec["oracle"])
    frozen = {}
    folded = set()
    for row in spec["files"]:
        name = row["guest_path"]
        path = Path(name)
        if (path.is_absolute() or ".." in path.parts or not name.startswith("src/")
                or path.as_posix() != name or name.lower() in folded):
            raise ValueError("Invalid or duplicate frozen guest path")
        folded.add(name.lower())
        frozen[name] = checked_file(root, row)
    entry = re.search(r"(?:^| )-i ([^ ]+)(?: |$)", manifest["command"])
    if entry is None or entry.group(1) not in frozen:
        raise ValueError("Frozen workload lacks the current command entry source")
    if len(frozen) != 214:
        raise ValueError("Frozen workload must contain exactly 214 input files")
    digest = comparison.digest_parts([part for row in spec["files"] for part in
                                     (row["guest_path"].encode(), frozen[row["guest_path"]])])
    if digest != spec["staged_inputs_sha256"]:
        raise ValueError("Frozen staged input digest mismatch")
    package_file = manifest.get("runtime_package_file", "p.bin")
    package = (args.release_bundle / package_file).read_bytes()
    asset = f"src/packages/{package_file}"
    if frozen.get(asset) != package:
        raise ValueError("Frozen package asset must equal the current release package")
    if manifest.get("bootstrap_package_storage") != "embedded" or manifest.get("output_package_storage") != "embedded":
        raise ValueError("Frozen repeatability requires embedded input and output packages")
    frozen["opforge"] = current_files["opforge"]
    cases = []

    def add(role, storage, repetition, own, files, expected, bundle):
        own = copy.deepcopy(own)
        own["capture_mode"] = storage
        own["command"] = own["command"].removesuffix(" --capture-dir scratch")
        if storage == "disk":
            own["command"] += " --capture-dir scratch"
        sources = {k: v for k, v in files.items() if k.startswith("src/")}
        own.update(source_files=len(sources), source_bytes=sum(map(len, sources.values())),
                   native_validation="not_run")
        workload_digest = comparison.digest_parts([
            b"opforge-a6000-repeatability-workload-v1",
            own["command"].removesuffix(" --capture-dir scratch").encode(), expected,
            *(part for k, v in sorted(sources.items()) for part in (k.encode(), v))])
        for key in ("native_protocol_completed", "native_assembler_exit_code", "native_start_to_done_host_seconds"):
            own.pop(key, None)
        if role != "current_source_selfhost":
            own["kind"] = "a6000-repeatability-derived-case"
            own["source_manifest_digest"] = runner.fnv(b"".join(
                k[4:].encode() + b"\0" + v + b"\0" for k, v in sorted(sources.items())))
            own.pop("source_root", None)
            own["frozen_case"] = str(args.frozen_case)
            own["source_mapping"] = [dict(staged_path=k, logical_path=k[4:], bytes=len(v),
                                         digest=runner.fnv(v), origin="frozen_case")
                                     for k, v in sorted(sources.items())]
        case_digest = comparison.digest_parts([
            b"opforge-a6000-repeatability-v1", own["command"].encode(), expected,
            *(part for k, v in sorted(files.items()) for part in (k.encode(), v))])
        metadata = {"result_kind": role, "repetition": repetition,
                    "frozen_case": str(args.frozen_case) if role != "current_source_selfhost" else None,
                    "workload_sha256": workload_digest,
                    "ram_policy": "unrestricted; no 8 MiB fit claim",
                    "bundle_prior_native_validation": "Derived case has no prior native qualification."}
        cases.append(dict(role=role, storage=storage, repetition=repetition, bundle=bundle,
                          manifest=own, files=files, oracle=expected, case_sha256=case_digest,
                          metadata=metadata))

    for repetition in range(1, 4):
        for storage in ("resident", "disk"):
            add("frozen_release", storage, repetition, manifest, frozen, oracle, args.release_bundle)
    add("current_source_selfhost", "disk", 1, manifest, current_files, current_oracle, args.release_bundle)
    add("frozen_memory", "disk", 1, memory_manifest,
        dict(frozen, opforge=memory_files["opforge"]), oracle, args.memory_bundle)
    for case in cases:
        runner.guest_script(f"{args.volume}:opforge-{'0' * 12}",
                            f"{'0' * 32} {case['case_sha256']}", case["manifest"]["command"],
                            capture_mode=case["storage"])
    return cases


def summary(records):
    groups = {}
    for storage in ("resident", "disk"):
        rows = [row for row in records if row["role"] == "frozen_release" and row["storage"] == storage]
        values = [row["result"].get("guest_overall_seconds") for row in rows
                  if row.get("result", {}).get("success") and row["result"].get("guest_protocol_completed")]
        if len(values) == 3 and all(isinstance(v, (int, float)) and not isinstance(v, bool) for v in values):
            groups[storage] = dict(min=min(values), max=max(values), median=statistics.median(values),
                                   variance=statistics.pvariance(values), successful_runs=3)
    return {"ram_policy": "unrestricted; no 8 MiB fit claim", "groups": groups, "runs": records}


def capture_evidence(local, active):
    root = local / "captured" / active["remote_directory"].split(":", 1)[1]
    evidence = {"captured_path": str(root) if root.is_dir() else None}
    if root.is_dir():
        environment = root / "environment.txt"
        evidence["environment_path"] = str(environment) if environment.is_file() else None
        evidence["guest_environment"] = environment.read_text(errors="replace") if environment.is_file() else None
        stdout = root / "assembly.stdout"
        lines = stdout.read_text(errors="replace").splitlines() if stdout.is_file() else []
        evidence["last_progress_markers"] = [line for line in lines if re.search("prepar|progress", line, re.I)][-10:]
        scratch = root / "scratch"
        evidence["scratch_files_remaining"] = sorted(str(p.relative_to(root)) for p in scratch.rglob("*") if p.is_file()) if scratch.exists() else []
    return evidence


def execute_cases(args, cases):
    args.state.parent.mkdir(parents=True, exist_ok=True)
    # O_EXCL is the batch claim. Even a crash before the first replacement leaves
    # a guard that requires manual inspection; never remove it in an exception.
    descriptor = os.open(args.state, os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o600)
    os.fsync(descriptor)
    os.close(descriptor)
    descriptor = os.open(args.state.parent, os.O_RDONLY)
    try:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)
    now = time.time()
    state = {"status": "claimed", "active": None, "runs": [], "host_state": {
        "utc": datetime.fromtimestamp(now, timezone.utc).isoformat(), "unix_seconds": now,
        "monotonic_seconds": time.monotonic(), "platform": platform.platform(),
        "python_version": platform.python_version()}}
    atomic_json(args.state, state)
    try:
        for case in cases:
            state["status"] = "preparing"
            state["active"] = {k: case[k] for k in ("role", "storage", "repetition", "case_sha256")}
            atomic_json(args.state, state)

            def before_execute(evidence):
                state["status"] = "unresolved"
                state["active"].update(evidence)
                atomic_json(args.state, state)

            result, local = runner.run_case(
                args, case["bundle"], case["manifest"], case["files"], case["oracle"],
                case["case_sha256"], run_kind=f'{case["role"]} {case["storage"]} {case["repetition"]}',
                metadata=case["metadata"], before_execute=before_execute)
            if not result or result.get("guest_protocol_completed") is not True:
                raise ValueError("Incomplete guest protocol; serial batch stopped without retry")
            record = dict(state["active"], result=result, local_results=str(local))
            if "remote_directory" in state["active"]:
                record.update(capture_evidence(Path(local), state["active"]))
            state["runs"].append(record)
            state.update(active=None, status="completed_case" if result.get("success") else "failed")
            state["summary"] = summary(state["runs"])
            atomic_json(args.state, state)
            if not result.get("success"):
                return False
        state["status"] = "completed"
        atomic_json(args.state, state)
        return True
    except BaseException as error:
        state.update(status="unresolved", error=f"{type(error).__name__}: {error}",
                     diagnostic_guidance="Guest may still be running. Inspect the recorded remote directory read-only from macOS Terminal; do not Execute or retry this journal.")
        if (state.get("active") or {}).get("remote_directory"):
            state["active"].update(capture_evidence(Path(state["active"]["local_results"]), state["active"]))
        atomic_json(args.state, state)
        raise


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("frozen-case", "release-bundle", "memory-bundle", "output"):
        parser.add_argument("--" + name, type=Path, required=True)
    parser.add_argument("--state", type=Path)
    parser.add_argument("--host")
    parser.add_argument("--volume", default="Development", choices=["Development"])
    parser.add_argument("--timeout", type=int, default=3600)
    parser.add_argument("--prepare-only", "--dry-run", dest="dry_run", action="store_true")
    args = parser.parse_args(argv)
    if (args.host is not None and not runner.valid_host(args.host)) or not 1 <= args.timeout < 86400:
        parser.error("Invalid host or timeout (require 1..86399 seconds)")
    for name in ("frozen_case", "release_bundle", "memory_bundle", "output"):
        setattr(args, name, getattr(args, name).resolve())
    args.state = (args.state or args.output / "state.json").resolve()
    if args.state.exists() and not args.dry_run:
        raise ValueError("State journal already exists; no resume or retry is allowed")
    cases = prepare_cases(args)
    args.output.mkdir(parents=True, exist_ok=True)
    plan = [{k: str(v) if isinstance(v, Path) else v for k, v in case.items()
             if k not in ("files", "oracle")} for case in cases]
    atomic_json(args.output / "plan.json", {"cases": plan, "ram_policy": "unrestricted; no 8 MiB fit claim"})
    print(f"Prepared eight serial cases: {args.output / 'plan.json'}", flush=True)
    command = ["python3", str(Path(__file__).resolve()), "--frozen-case", str(args.frozen_case),
               "--release-bundle", str(args.release_bundle), "--memory-bundle", str(args.memory_bundle),
               "--output", str(args.output), "--state", str(args.state), "--timeout", str(args.timeout)]
    if args.host:
        command += ["--host", args.host]
    print("Run once in your macOS Terminal: " + shlex.join(command), flush=True)
    return 0 if args.dry_run or execute_cases(args, cases) else 1


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        raise SystemExit(f"Stopped: {error}. No hardware repeatability claim.")

#!/usr/bin/env python3
"""Run the prepared compact self-host bundle from macOS Terminal via acp/ash.

The export selects a release or instrumented bootstrap and a release oracle.
Transfers are outside the timing window.
Each invocation keeps its own local results and Development: directory.
"""

import argparse
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import time
import uuid

from memory_telemetry import decode_memory_telemetry

INSTRUMENTATION_DEFINES = {
    "OPFORGE_DEBUG_CONTRACTS", "OPFORGE_MEMORY_TELEMETRY",
    "OPFORGE_TOKEN_DETAIL_TELEMETRY", "OPFORGE_PREPARATION_PROGRESS",
    "OPFORGE_BINDING_DETAIL_TELEMETRY", "OPFORGE_TEMPLATE_WORK_TELEMETRY",
    "OPFORGE_INPUT_TELEMETRY", "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT",
}
PACKAGE_HEADER_BYTES = 192


def fnv(data):
    value = 0xCBF29CE484222325
    for byte in data:
        value = ((value ^ byte) * 0x100000001B3) & 0xFFFFFFFFFFFFFFFF
    return f"fnv1a64:{value:016x}"


def has_m68020_entry_preamble(entry):
    statements = []
    for line in entry.splitlines():
        statement = line.partition(b";")[0].strip()
        if statement:
            statements.append(statement)
            if len(statements) == 2:
                break
    return (
        len(statements) == 2
        and re.fullmatch(rb"\.module\s+main", statements[0], re.IGNORECASE) is not None
        and re.fullmatch(rb"\.cpu\s+(?:68020|m68020)", statements[1], re.IGNORECASE) is not None
    )


def load_bundle(bundle):
    manifest = json.loads((bundle / "manifest.json").read_text())
    output_storage = manifest.get("output_package_storage", "external")
    filename_mappings = {"identity; source include literals are unchanged"}
    if output_storage == "embedded":
        filename_mappings.add("identity; generated inputs have explicit origins")
    if manifest["release_defines"] or manifest["filename_mapping"] not in filename_mappings:
        raise ValueError("Require unchanged source filenames and a release output oracle")
    defines = set(manifest.get("bootstrap_defines", []))
    storage = manifest.get("bootstrap_package_storage", "external")
    embedded_packages = manifest.get("embedded_packages", [])
    output_packages = manifest.get("output_embedded_packages", [])
    if storage not in ("external", "embedded") or (
        storage == "external" and embedded_packages
    ) or output_storage not in ("external", "embedded") or (
        output_storage == "external" and output_packages
    ) or (output_storage == "embedded" and storage != "embedded"):
        raise ValueError("Unsupported self-host package storage")
    if defines:
        required = {"OPFORGE_DEBUG_CONTRACTS", "OPFORGE_MEMORY_TELEMETRY",
                    "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT"}
        if not required <= defines or not defines <= INSTRUMENTATION_DEFINES:
            raise ValueError("Unsupported bootstrap instrumentation defines")
        if manifest.get("telemetry_file") != "memory.bin":
            raise ValueError("Instrumented export must write local memory.bin")
    elif manifest.get("telemetry_file"):
        raise ValueError("Release export must not require telemetry")
    if not manifest["classic_filename_compatible"]:
        raise ValueError("Bundle has filenames over 30 bytes; regenerate the corrected export")
    files = {}
    identity = bytearray()
    origins = {}
    mapped_paths = set()
    for row in manifest["source_mapping"]:
        path = Path(row["staged_path"])
        logical = Path(row["logical_path"])
        if path != Path("src") / logical or logical.is_absolute() or ".." in logical.parts:
            raise ValueError("Invalid source mapping")
        data = (bundle / path).read_bytes()
        if len(data) != row["bytes"] or fnv(data) != row["digest"]:
            raise ValueError(f"Export source changed: {path}")
        origin = row.get("origin", "native")
        folded_path = path.as_posix().lower()
        if folded_path in mapped_paths:
            raise ValueError("Duplicate source mapping")
        mapped_paths.add(folded_path)
        if origin == "native":
            expected = (Path(manifest["source_root"]) / logical).read_bytes()
        elif output_storage == "embedded" and origin in (
            "configured_entry", "generated_catalog", "package_asset"
        ):
            if origin in origins:
                raise ValueError("Duplicate embedded source origin")
            origins[origin] = (logical.as_posix(), data)
            if origin == "configured_entry":
                if logical.as_posix() != "experimental/opforge_compact_cli.asm":
                    raise ValueError("Invalid configured entry mapping")
                original = (Path(manifest["source_root"]) / logical).read_bytes()
                literal = b'.include "package_catalog.i"'
                if original.count(literal) != 1:
                    raise ValueError("Configured entry requires exactly one catalog include")
                expected = original.replace(literal, b'.include "catalog.i"')
            else:
                expected = data
        else:
            raise ValueError("Invalid source mapping origin")
        if data != expected:
            raise ValueError(f"Current source differs; regenerate the export: {logical}")
        files[path.as_posix()] = data
        identity.extend(logical.as_posix().encode() + b"\0" + data + b"\0")
    if fnv(identity) != manifest["source_manifest_digest"]:
        raise ValueError("Source manifest mismatch")
    oracle = (bundle / "oracle.hunk").read_bytes()
    bootstrap = (bundle / "opforge").read_bytes()
    package_file = manifest.get("runtime_package_file", "p.bin")
    if not isinstance(package_file, str) or not (
        re.fullmatch(r"[A-Za-z0-9_-]+\.bin", package_file) or package_file == "p.bin"
    ):
        raise ValueError("Unsafe runtime package filename")
    package = (bundle / package_file).read_bytes()
    if fnv(oracle) != manifest["release_hunk_digest"]:
        raise ValueError("Release oracle mismatch")
    if fnv(bootstrap) != manifest.get("bootstrap_hunk_digest", manifest["release_hunk_digest"]):
        raise ValueError("Bootstrap digest mismatch")
    if not defines and (storage == "external" or output_storage == "embedded") and bootstrap != oracle:
        raise ValueError("Release bootstrap/oracle mismatch")
    # Only the current package contract is supported. This transport verifies
    # assets and header regions; the native runtime interprets VM opcodes.
    if fnv(package) != manifest["runtime_package_digest"] or package[:4] != b"BS22":
        raise ValueError("Runtime package mismatch")
    if len(package) < PACKAGE_HEADER_BYTES or int.from_bytes(package[4:8], "big") != len(package):
        raise ValueError("Invalid runtime package header")
    runtime_bytes = int.from_bytes(package[72:76], "big")
    if not PACKAGE_HEADER_BYTES <= runtime_bytes <= len(package) or runtime_bytes % 2:
        raise ValueError("Invalid runtime package region")
    bindings_offset = int.from_bytes(package[160:164], "big")
    bindings_count = int.from_bytes(package[164:168], "big")
    bindings_end = bindings_offset + bindings_count * 8
    if (bindings_offset < PACKAGE_HEADER_BYTES or bindings_offset % 2
            or bindings_count > 65535 or bindings_end > runtime_bytes):
        raise ValueError("Invalid member-binding table region")
    if any(package[offset + 6:offset + 8] != b"\0\0"
           for offset in range(bindings_offset, bindings_end, 8)):
        raise ValueError("Invalid member-binding reserved field")
    for label, header_offset, expected_size in (("head-policy", 168, 4), ("declaration", 180, 13)):
        offset = int.from_bytes(package[header_offset:header_offset + 4], "big")
        size = int.from_bytes(package[header_offset + 4:header_offset + 8], "big")
        version = int.from_bytes(package[header_offset + 8:header_offset + 10], "big")
        reserved = package[header_offset + 10:header_offset + 12]
        if (offset < PACKAGE_HEADER_BYTES or offset % 2 or size != expected_size
                or offset + size > runtime_bytes or version != 2 or reserved != b"\0\0"):
            raise ValueError(f"Invalid {label} program header")
    files["opforge"] = bootstrap
    if storage == "embedded":
        offset = int.from_bytes(package[124:128], "big")
        size = int.from_bytes(package[128:130], "big")
        if offset < PACKAGE_HEADER_BYTES or not 1 <= size <= 26 or offset + size > runtime_bytes:
            raise ValueError("Invalid embedded package identity")
        target = package[offset:offset + size].decode("ascii")
        if target != "m68020--motorola68k" or embedded_packages != [target + ".bin"]:
            raise ValueError("Require exactly the self-host m68020 package embedded")
        if package_file != target + ".bin":
            raise ValueError("Embedded package filename must identify its target")
        if bootstrap.count(package) != 1:
            raise ValueError("Embedded bootstrap must contain the exact package once")
    else:
        files["p.bin"] = package
    if output_storage == "embedded":
        if output_packages != [package_file] or set(origins) != {
            "configured_entry", "generated_catalog", "package_asset"
        }:
            raise ValueError("Require exactly the embedded output package and source origins")
        catalog_path, catalog = origins["generated_catalog"]
        asset_path, asset = origins["package_asset"]
        if catalog_path != "experimental/catalog.i" or asset_path != f"experimental/packages/{package_file}":
            raise ValueError("Invalid embedded source mapping")
        if asset != package:
            raise ValueError("Embedded source package asset mismatch")
        incbins = re.findall(rb'(?im)^\s*(?:[A-Za-z_][A-Za-z0-9_]*:\s*)?\.incbin\b([^\r\n]*)', catalog)
        if [operand.strip() for operand in incbins] != [f'"packages/{package_file}"'.encode()]:
            raise ValueError("Embedded catalog must include exactly its package asset")
        if oracle.count(package) != 1:
            raise ValueError("Embedded release oracle must contain the exact package once")
    command = manifest["command"]
    if command != (bundle / "command.txt").read_text().strip():
        raise ValueError("Assembly command mismatch")
    if not re.fullmatch(r"[A-Za-z0-9_./ -]+", command):
        raise ValueError("Unsafe assembly command")
    if storage == "embedded":
        search_options = r"(?: -(?:M|I) src(?:/[A-Za-z0-9_./-]+)?)*"
        command_tail = r"-i (src/[A-Za-z0-9_./-]+) --hunk output\.hunk" + search_options
        explicit_cpu = re.fullmatch(r"opforge --cpu 68020 " + command_tail, command)
        source_cpu = re.fullmatch(r"opforge " + command_tail, command)
        if not explicit_cpu and not source_cpu:
            raise ValueError("Embedded test cannot override package selection or search")
        if source_cpu:
            entry_path = source_cpu.group(1)
            entry = files.get(entry_path)
            if (manifest.get("entry") != entry_path or entry is None
                    or not has_m68020_entry_preamble(entry)):
                raise ValueError("Source-selected embedded test requires the mapped m68020 entry preamble")
    elif not command.startswith("opforge --runtime-package p.bin -i "):
        raise ValueError("External-package test requires the named runtime package")
    identity.extend(b"m68020\0" + command.encode() + b"\0" + package + b"\0" + oracle)
    identity.extend(b"\0output-package-storage\0" + output_storage.encode() + b"\0")
    if defines:
        identity.extend(b"\0instrumented-bootstrap\0" + bootstrap)
    if storage == "embedded":
        identity.extend(b"\0embedded-bootstrap\0" + bootstrap)
    return manifest, files, oracle, hashlib.sha256(identity).hexdigest()


def guest_script(remote, marker, command):
    # Save RC immediately; ash's Execute status alone is not assembly completion.
    executable, arguments = command.split(" ", 1)
    script = (
        f"FailAt 10\nCD {remote}\nStack 65536\nProtect opforge +e\n"
        "C:Version >environment.txt\nIf EXISTS C:CPU\nC:CPU >>environment.txt\nEndIf\n"
        "C:Avail >>environment.txt\nStack >>environment.txt\n"
        f'Echo "START {marker}" >start.marker\nC:Date >start.time\n'
        "FailAt 999\n"
        # OS 3.1 lacks *>; put ordinary output redirection immediately after
        # the executable, which also works with the Shell's oldredirect mode.
        f"{executable} >assembly.stdout {arguments}\n"
        "Echo $RC >exitcode\nC:Date >end.time\n"
        f'Echo "DONE {marker}" >done.marker\n'
    )
    if max(map(len, script.splitlines())) > 255:
        raise ValueError("Amiga Shell command exceeds 255 bytes")
    return script


def verify_files(root, files):
    for name, data in files.items():
        path = root / name
        if not path.is_file() or path.read_bytes() != data:
            raise ValueError(f"Remote copy differs or is missing: {name}")
    if "opforge" in files and "p.bin" not in files:
        for name in ("p.bin", "packages"):
            if (root / name).exists():
                raise ValueError(f"Embedded test must not have an external package: {name}")


def verify_fresh_directory(root):
    for name in ("output.hunk", "memory.bin", "start.marker", "done.marker", "exitcode"):
        if (root / name).exists():
            raise ValueError(f"Pre-run directory contains an old result: {name}")


def guest_seconds(start, end, host_seconds):
    # OS 3.1 Date has no portable LFORMAT option. Keep its raw output too.
    def seconds(text):
        match = re.search(r"\b(\d{1,2}):(\d{2}):(\d{2})\b", text)
        if not match:
            return None
        h, m, s = map(int, match.groups())
        return h * 3600 + m * 60 + s if h < 24 and m < 60 and s < 60 else None

    first, last = seconds(start), seconds(end)
    if first is None or last is None or host_seconds >= 86400:
        return None
    elapsed = (last - first) % 86400
    # A changed guest clock must not silently become a plausible measurement.
    return elapsed if elapsed <= host_seconds + 2 else None


def inspect_result(root, files, oracle, marker, host_seconds):
    verify_files(root, files)
    for name, expected in (("start.marker", f"START {marker}"), ("done.marker", f"DONE {marker}")):
        if (root / name).read_text().strip() != expected:
            raise ValueError(f"Missing or wrong fresh completion evidence: {name}")
    rc = int((root / "exitcode").read_text().strip())
    start = (root / "start.time").read_text().strip()
    end = (root / "end.time").read_text().strip()
    output = root / "output.hunk"
    matched = output.is_file() and output.read_bytes() == oracle
    return {
        "guest_protocol_completed": True,
        "assembler_exit_code": rc,
        "exact_rust_match": matched,
        "output_bytes": output.stat().st_size if output.is_file() else None,
        "guest_start": start,
        "guest_end": end,
        "guest_overall_seconds": guest_seconds(start, end, host_seconds),
        "guest_clock_resolution_seconds": 1,
        "host_command_seconds_including_connection": host_seconds,
        "success": rc == 0 and matched,
    }


def add_telemetry(result, root, manifest):
    defines = manifest.get("bootstrap_defines", [])
    result.update({"measurement_mode": "instrumented" if defines else "release",
                   "bootstrap_defines": defines,
                   "bootstrap_package_storage": manifest.get("bootstrap_package_storage", "external"),
                   "embedded_packages": manifest.get("embedded_packages", []),
                   "output_package_storage": manifest.get("output_package_storage", "external"),
                   "output_embedded_packages": manifest.get("output_embedded_packages", [])})
    if not defines:
        return
    result["assembly_success"] = result["success"]
    try:
        memory = decode_memory_telemetry((root / "memory.bin").read_bytes())
    except (OSError, ValueError) as error:
        result.update({"instrumented_memory": None, "telemetry_completed": False,
                       "telemetry_error": str(error), "success": False})
        return
    diagnostics = memory["diagnostics"]
    if memory["allocation_failure_count"]:
        diagnostics.append("allocation failures recorded")
    preparation = memory["instrumented_preparation_seconds"]
    assembly = memory["instrumented_assembly_seconds"]
    if preparation is None or assembly is None:
        diagnostics.append("missing or invalid phase clocks")
    stages = memory["preparation_stage_seconds"]
    if preparation is not None and stages is not None and abs(stages - preparation) > 0.040001:
        diagnostics.append("exclusive stage clocks disagree with preparation by more than 40 ms")
    result.update({"instrumented_memory": memory, "telemetry_completed": not diagnostics,
                   "success": result["success"] and not diagnostics})


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    current = Path("/tmp/opforge-a6000-current")
    default_bundle = current if current.is_dir() else Path("/tmp/opforge-a6000-compact-export")
    parser.add_argument("--bundle", type=Path, default=default_bundle,
                        help="bundle directory; defaults to the current export when available")
    parser.add_argument("--host", default="192.168.0.220")
    parser.add_argument("--volume", default="Development")
    parser.add_argument("--timeout", type=int, default=3600, help="assembly timeout in seconds")
    parser.add_argument("--dry-run", action="store_true", help="prepare locally without connecting")
    args = parser.parse_args()
    if not re.fullmatch(r"[A-Za-z0-9.-]+", args.host) or not re.fullmatch(r"[A-Za-z0-9_-]+", args.volume):
        parser.error("Host and volume must be plain names; omit the volume colon")
    if not 1 <= args.timeout < 86400:
        parser.error("Timeout must be between 1 and 86399 seconds")
    manifest, files, oracle, case_digest = load_bundle(args.bundle.resolve())
    run_id = uuid.uuid4().hex
    name = f"opforge-{run_id[:12]}"
    remote = f"{args.volume}:{name}"
    marker = f"{run_id} {case_digest}"
    local = Path(tempfile.mkdtemp(prefix="opforge-a6000-result-"))
    stage = local / name
    stage.mkdir()
    for path, data in files.items():
        target = stage / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(data)
    (stage / "run-selfhost").write_text(guest_script(remote, marker, manifest["command"]))
    print(f"Bundle: {args.bundle.resolve()}\nLocal results: {local}\nRemote directory: {remote}", flush=True)
    if manifest["over_classic_limit_components"]:
        print("Exact filenames required: " + ", ".join(manifest["over_classic_limit_components"]), flush=True)
    if args.dry_run:
        print("Prepared only; no network access or native execution.")
        return 0
    acp, ash = shutil.which("acp"), shutil.which("ash")
    if not acp or not ash:
        raise ValueError("acp and ash must be installed and available in PATH")

    def run(command, timeout=300):
        print("Running: " + " ".join(command), flush=True)
        subprocess.run(command, check=True, timeout=timeout)

    run([ash, args.host, f"Info {args.volume}:"], 30)
    run([acp, "-r", str(stage), f"{args.host}:{args.volume}"])
    copied = local / "copy-check"
    copied.mkdir()
    run([acp, "-r", f"{args.host}:{args.volume}/{name}", str(copied)])
    try:
        verify_files(copied / name, files)
        verify_files(copied / name, {"run-selfhost": (stage / "run-selfhost").read_bytes()})
        verify_fresh_directory(copied / name)
    except (OSError, ValueError) as error:
        raise ValueError(f"Pre-run transfer verification failed; assembly was not started: {error}") from error
    print("Remote source, package, bootstrap and script verified. Starting native self-host.", flush=True)
    started = time.monotonic()
    try:
        run([ash, args.host, f"Execute {remote}/run-selfhost"], args.timeout)
    except (subprocess.TimeoutExpired, subprocess.CalledProcessError, KeyboardInterrupt):
        print(f"No completion claim. The guest may still be running in {remote}; do not start an overlapping retry.", flush=True)
        raise
    elapsed = time.monotonic() - started
    captured = local / "captured"
    captured.mkdir()
    run([acp, "-r", f"{args.host}:{args.volume}/{name}", str(captured)])
    try:
        result = inspect_result(captured / name, files, oracle, marker, elapsed)
    except (OSError, ValueError) as error:
        raise ValueError(f"Post-run capture validation failed; execution returned but the hardware result is unverified: {error}") from error
    add_telemetry(result, captured / name, manifest)
    if not result["success"]:
        diagnostic = captured / name / "assembly.stdout"
        if diagnostic.is_file():
            print("Native diagnostic:\n" + diagnostic.read_text(errors="replace"), flush=True)
    result.update({"case_sha256": case_digest, "remote_directory": remote, "source_files": manifest["source_files"], "source_bytes": manifest["source_bytes"]})
    (local / "result.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, indent=2), flush=True)
    print(f"Results retained at {local}; remote files retained at {remote}.")
    return 0 if result["success"] else 1


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        raise SystemExit(f"Stopped: {error}. No successful hardware self-host claim.")

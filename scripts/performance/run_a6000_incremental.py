#!/usr/bin/env python3
"""Upload a release bundle and measure a cold/warm A6000 self-host pair."""

import argparse
import hashlib
from pathlib import Path
import re
import subprocess

import run_a6000_selfhost as base

PAIR_PROTOCOL = "a6000-incremental-pair-v1"
PHASES = ("cold", "warm")


def phase_marker(marker, phase):
    return f"{marker} {PAIR_PROTOCOL} {phase}"


def guest_script(remote, marker, command, *, capture_mode="resident"):
    # Both invocations run from the same directory and retain build/ofm.
    setup = base.guest_script(remote, marker, command, capture_mode=capture_mode)
    script = setup[:setup.index('Echo "START ')]
    for phase in PHASES:
        script += f"FailAt 10\nIf EXISTS {phase}\nDelete {phase} ALL\nEndIf\nMakeDir {phase}\n"
        for name in ("output.hunk", "build/opforge"):
            script += f"If EXISTS {name}\nDelete {name}\nEndIf\n"
        executable, arguments = command.split(" ", 1)
        if phase == "cold":
            arguments += " --rebuild"
        challenge = phase_marker(marker, phase)
        script += (
            f'Echo "START {challenge}" >{phase}/start.marker\n'
            f"C:Date >{phase}/start.time\nFailAt 999\n"
            f"{executable} >{phase}/assembly.stdout {arguments}\n"
            f"Echo $RC >{phase}/exitcode\nC:Date >{phase}/end.time\n"
            f"Copy build/opforge {phase}/output.hunk\n"
            f"Echo $RC >{phase}/copy-exitcode\n"
            f'Echo "DONE {challenge}" >{phase}/done.marker\n'
        )
    if max(map(len, script.splitlines())) > 255:
        raise ValueError("Amiga Shell command exceeds 255 bytes")
    return script


def incremental_counters(text):
    matches = re.findall(r"^incremental: reused=0x([0-9a-fA-F]{8}) rebuilt=0x([0-9a-fA-F]{8})\s*$",
                         text, re.MULTILINE)
    if len(matches) != 1:
        raise ValueError("Require exactly one incremental cache summary")
    reused, rebuilt = (int(value, 16) for value in matches[0])
    return {"reused": reused, "rebuilt": rebuilt}


def inspect_result(root, files, oracle, marker, host_seconds, *,
                   native_output_file="build/opforge"):
    if native_output_file != "build/opforge":
        raise ValueError("Incremental pair requires source-directed build/opforge output")
    base.verify_files(root, files)
    phases = {}
    diagnostics = []
    for phase in PHASES:
        directory = root / phase
        result = {"success": False}
        try:
            result = base.inspect_result(directory, {}, oracle, phase_marker(marker, phase),
                                         host_seconds, native_output_file="output.hunk")
            # Only the pair's connection duration is known; never label it per phase.
            result.pop("host_command_seconds_including_connection")
            result["copy_exit_code"] = int((directory / "copy-exitcode").read_text().strip())
            result["success"] &= result["copy_exit_code"] == 0
            result["incremental"] = incremental_counters(
                (directory / "assembly.stdout").read_text(errors="replace"))
        except (OSError, ValueError) as error:
            result.update({"success": False, "error": str(error)})
            diagnostics.append(f"{phase}: {error}")
        phases[phase] = result
    cold = phases["cold"].get("incremental", {})
    warm = phases["warm"].get("incremental", {})
    cache_ok = (cold.get("reused") == 0 and cold.get("rebuilt", 0) > 0
                and warm.get("reused") == cold.get("rebuilt") and warm.get("rebuilt") == 0)
    if not cache_ok:
        diagnostics.append("Cold must rebuild all modules; warm must reuse every cold module and rebuild none")
    for phase, result in phases.items():
        if not result["success"]:
            path = root / phase / "assembly.stdout"
            if path.is_file():
                diagnostics.append(f"{phase} stdout:\n" + path.read_text(errors="replace"))
    return {"protocol": PAIR_PROTOCOL, "phases": phases,
            "cache_semantics_verified": cache_ok, "diagnostics": diagnostics,
            "host_command_seconds_including_connection": host_seconds,
            "success": cache_ok and all(result["success"] for result in phases.values())}


def load_bundle(path):
    manifest, files, oracle, digest = base.load_bundle(path)
    if (manifest.get("bootstrap_defines")
            or manifest.get("bootstrap_package_storage") not in ("external", "embedded")
            or manifest.get("output_package_storage") not in ("external", "embedded")
            or manifest.get("native_output_file") != "build/opforge"
            or manifest.get("capture_mode") != "disk"):
        raise ValueError("Incremental pair requires a release source-directed m68020 disk-capture bundle")
    digest = hashlib.sha256((PAIR_PROTOCOL + "\0" + digest).encode()).hexdigest()
    return manifest, files, oracle, digest


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    current = Path("/tmp/opforge-a6000-current")
    parser.add_argument("--bundle", type=Path, default=current if current.is_dir() else Path("/tmp/opforge-a6000-compact-export"))
    parser.add_argument("--host", default=None, help="defaults to unique ash -l discovery")
    parser.add_argument("--volume", default="Development")
    parser.add_argument("--timeout", type=int, default=3600)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--dry-run", action="store_true")
    mode.add_argument("--copy-only", action="store_true", help="upload and verify files for manual cold/warm assembly without executing")
    args = parser.parse_args(argv)
    if (args.host is not None and not base.valid_host(args.host)) or not re.fullmatch(r"[A-Za-z0-9_-]+", args.volume):
        parser.error("Host and volume must be plain names; omit the volume colon")
    if not 1 <= args.timeout < 86400:
        parser.error("Timeout must be between 1 and 86399 seconds")
    bundle = args.bundle.resolve()
    manifest, files, oracle, digest = load_bundle(bundle)
    result, _ = base.run_case(args, bundle, manifest, files, oracle, digest,
                              run_kind="cold/warm incremental self-host", guest_script_builder=guest_script,
                              result_inspector=inspect_result,
                              manual_commands=[manifest["command"] + " --rebuild", manifest["command"]])
    return 0 if result is None or result["success"] else 1


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        raise SystemExit(f"Stopped: {error}. No successful hardware incremental claim.")

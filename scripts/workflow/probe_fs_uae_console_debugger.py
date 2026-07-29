#!/usr/bin/env python3
"""Opt-in feasibility probe for FS-UAE's stock console debugger.

This is deliberately not the production smoke runner.  It establishes whether
the installed binary retains a terminal when launched through a host PTY.  It
does not synthesize Cmd+D or send debugger commands: the stock macOS build may
require manual console entry after its window is focused.
"""

# @opforge-evidence: level=E; role=diagnostic-tool; authority=none; lifecycle=permanent

from __future__ import annotations

import argparse
import hashlib
import json
import os
import pty
import re
import select
import signal
import subprocess
import sys
import time
from pathlib import Path


OPT_IN_ENV = "OPFORGE_FS_UAE_CONSOLE_DEBUGGER_PROBE"
DEFAULT_BINARY = "/Applications/FS-UAE.app/Contents/MacOS/fs-uae"
DEFAULT_TEMPLATE = "/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae"
MAX_TRANSCRIPT_BYTES = 1_048_576


def debug_config(template: str) -> str:
    """Return a config that enables only the stock console debugger."""
    if not template.endswith("\n"):
        template += "\n"
    return template + "\n# opForge opt-in console debugger feasibility probe\nconsole_debugger = 1\n"


def normalized_transcript(raw: bytes) -> str:
    return raw.decode("utf-8", errors="replace").replace("\r\n", "\n").replace("\r", "\n")


def entry_from_transcript(transcript: str) -> str:
    # A real debugger prompt would permit automation.  Otherwise the documented
    # macOS Cmd+D entry remains the only supported outcome for this probe.
    lowered = transcript.lower()
    if "uae debugger" in lowered or "activated debugger" in lowered or "\n> " in transcript:
        return "pty-command"
    return "manual-debugger-entry-required"


def fs_uae_version(transcript: str) -> str:
    match = re.search(r"^FS-UAE ([^\r\n]+)$", transcript, flags=re.MULTILINE)
    return match.group(1) if match else "unknown"


def self_test() -> None:
    assert debug_config("[fs-uae]").endswith("console_debugger = 1\n")
    assert normalized_transcript(b"one\r\ntwo\rthree") == "one\ntwo\nthree"
    assert entry_from_transcript("HELP for UAE Debugger") == "pty-command"
    assert entry_from_transcript("ordinary emulator banner") == "manual-debugger-entry-required"
    assert fs_uae_version("FS-UAE 3.1.66 (Built for macOS ?)\n") == "3.1.66 (Built for macOS ?)"
    assert fs_uae_version("no version") == "unknown"


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("--binary", type=Path, default=Path(DEFAULT_BINARY))
    result.add_argument("--template", type=Path, default=Path(DEFAULT_TEMPLATE))
    result.add_argument("--artifact-dir", type=Path, required=False)
    result.add_argument("--timeout-seconds", type=float, default=8.0)
    result.add_argument("--self-test", action="store_true")
    return result


def main() -> int:
    args = parser().parse_args()
    if args.self_test:
        self_test()
        print("PASS: console debugger probe helper tests")
        return 0
    if os.environ.get(OPT_IN_ENV) != "1":
        print(f"refusing to launch FS-UAE; set {OPT_IN_ENV}=1", file=sys.stderr)
        return 2
    if args.timeout_seconds <= 0:
        print("--timeout-seconds must be positive", file=sys.stderr)
        return 2
    if not args.binary.is_file() or not os.access(args.binary, os.X_OK):
        print(f"FS-UAE binary is not executable: {args.binary}", file=sys.stderr)
        return 2
    if not args.template.is_file():
        print(f"FS-UAE template is not readable: {args.template}", file=sys.stderr)
        return 2

    artifact_dir = args.artifact_dir or Path("target") / f"fs-uae-console-debugger-probe-{int(time.time())}"
    artifact_dir.mkdir(parents=True, exist_ok=False)
    config_path = artifact_dir / "console-debugger.config.fs-uae"
    raw_path = artifact_dir / "console-debugger.raw.log"
    transcript_path = artifact_dir / "console-debugger.transcript.txt"
    report_path = artifact_dir / "console-debugger.report.json"
    config_path.write_text(debug_config(args.template.read_text()), encoding="utf-8")

    master_fd, slave_fd = pty.openpty()
    raw = bytearray()
    child: subprocess.Popen[bytes] | None = None
    cleanup = "complete"
    stop_reason = "timeout"
    started = time.monotonic()
    try:
        child = subprocess.Popen(
            [str(args.binary), str(config_path)],
            stdin=slave_fd,
            stdout=slave_fd,
            stderr=slave_fd,
            start_new_session=True,
        )
        os.close(slave_fd)
        slave_fd = -1
        while time.monotonic() - started < args.timeout_seconds:
            readable, _, _ = select.select([master_fd], [], [], 0.2)
            if readable:
                try:
                    chunk = os.read(master_fd, 65536)
                except OSError:
                    chunk = b""
                if chunk:
                    remaining = MAX_TRANSCRIPT_BYTES - len(raw)
                    raw.extend(chunk[:remaining])
                    if len(chunk) > remaining:
                        stop_reason = "artifact-limit"
                        break
            if child.poll() is not None:
                stop_reason = "process-exit"
                break
    finally:
        if slave_fd >= 0:
            os.close(slave_fd)
        if child is not None and child.poll() is None:
            try:
                os.killpg(child.pid, signal.SIGTERM)
                child.wait(timeout=3)
            except (OSError, subprocess.TimeoutExpired):
                try:
                    os.killpg(child.pid, signal.SIGKILL)
                    child.wait(timeout=3)
                except (OSError, subprocess.TimeoutExpired):
                    cleanup = "incomplete"
        os.close(master_fd)

    raw_bytes = bytes(raw)
    transcript = normalized_transcript(raw_bytes)
    raw_path.write_bytes(raw_bytes)
    transcript_path.write_text(transcript, encoding="utf-8")
    report = {
        "schema_version": 1,
        "mode": "fs-uae-console-debugger-feasibility-probe",
        "proof_level": "E",
        "fs_uae_binary": str(args.binary),
        "fs_uae_version": fs_uae_version(transcript),
        "entry": entry_from_transcript(transcript),
        "stop_reason": stop_reason,
        "process_ids": [] if child is None else [child.pid],
        "cleanup": cleanup,
        "raw_transcript_sha256": hashlib.sha256(raw_bytes).hexdigest(),
    }
    report_path.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(report_path)
    return 0 if cleanup == "complete" else 1


if __name__ == "__main__":
    raise SystemExit(main())

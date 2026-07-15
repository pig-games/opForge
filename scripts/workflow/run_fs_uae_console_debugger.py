#!/usr/bin/env python3
"""Run a bounded stock-FS-UAE console capture after manual Cmd+D entry.

The tool never creates that GUI entry itself.  After the operator has focused
FS-UAE and pressed Cmd+D, it observes the PTY debugger banner and sends only a
reviewed, read-only command file through that same terminal.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import pty
import select
import signal
import subprocess
import sys
import time
from pathlib import Path

from probe_fs_uae_console_debugger import (
    DEFAULT_BINARY,
    DEFAULT_TEMPLATE,
    MAX_TRANSCRIPT_BYTES,
    debug_config,
    fs_uae_version,
    normalized_transcript,
)


OPT_IN_ENV = "OPFORGE_FS_UAE_CONSOLE_DEBUGGER"
ALLOWED_COMMANDS = {"r", "m", "d", "fl", "fd", "g", "t", "z", "h", "hh", "zl", "za", "q"}


def command_lines(text: str) -> list[str]:
    lines = []
    for raw_line in text.splitlines():
        if not raw_line.strip() or raw_line.lstrip().startswith("#"):
            continue
        if not raw_line.isascii() or any(ord(character) < 32 for character in raw_line):
            raise ValueError(f"unsupported or unsafe debugger command: {raw_line!r}")
        lines.append(raw_line.strip())
    if not lines:
        raise ValueError("command file has no debugger commands")
    for line in lines:
        fields = line.split()
        command = fields[0].lower()
        no_argument = {"r", "fl", "fd", "z", "zl", "q"}
        optional_argument = {"g", "t", "h", "hh"}
        required_argument = {"m", "d", "za"}
        valid_arity = (
            (command in no_argument and len(fields) == 1)
            or (command in optional_argument and len(fields) <= 2)
            or (command in required_argument and len(fields) >= 2)
        )
        if command not in ALLOWED_COMMANDS or not valid_arity:
            raise ValueError(f"unsupported or unsafe debugger command: {line!r}")
    return lines


def self_test() -> None:
    assert command_lines("# frame\nr\nd 1000 4\nfl\n") == ["r", "d 1000 4", "fl"]
    for unsafe in ("W 100 1", "r d0 1", "f 1000", "ré", "r\t", ""):
        try:
            command_lines(unsafe)
        except ValueError:
            pass
        else:
            raise AssertionError(f"accepted unsafe command file: {unsafe!r}")


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("--binary", type=Path, default=Path(DEFAULT_BINARY))
    result.add_argument("--template", type=Path, default=Path(DEFAULT_TEMPLATE))
    result.add_argument("--commands", type=Path, required=False)
    result.add_argument("--artifact-dir", type=Path, required=False)
    result.add_argument("--manual-entry-timeout-seconds", type=float, default=60.0)
    result.add_argument("--post-command-timeout-seconds", type=float, default=10.0)
    result.add_argument("--self-test", action="store_true")
    return result


def main() -> int:
    args = parser().parse_args()
    if args.self_test:
        self_test()
        print("PASS: console debugger runner helper tests")
        return 0
    if os.environ.get(OPT_IN_ENV) != "1":
        print(f"refusing to launch FS-UAE; set {OPT_IN_ENV}=1", file=sys.stderr)
        return 2
    if args.commands is None or not args.commands.is_file():
        print("--commands must name a readable reviewed command file", file=sys.stderr)
        return 2
    try:
        commands = command_lines(args.commands.read_text(encoding="utf-8"))
    except ValueError as error:
        print(str(error), file=sys.stderr)
        return 2
    if min(args.manual_entry_timeout_seconds, args.post_command_timeout_seconds) <= 0:
        print("timeouts must be positive", file=sys.stderr)
        return 2
    if not args.binary.is_file() or not os.access(args.binary, os.X_OK) or not args.template.is_file():
        print("FS-UAE binary or config template is unavailable", file=sys.stderr)
        return 2

    artifact_dir = args.artifact_dir or Path("target") / f"fs-uae-console-debugger-{int(time.time())}"
    artifact_dir.mkdir(parents=True, exist_ok=False)
    config_path = artifact_dir / "console-debugger.config.fs-uae"
    raw_path = artifact_dir / "console-debugger.raw.log"
    transcript_path = artifact_dir / "console-debugger.transcript.txt"
    report_path = artifact_dir / "console-debugger.report.json"
    config_path.write_text(debug_config(args.template.read_text(encoding="utf-8")), encoding="utf-8")

    master_fd, slave_fd = pty.openpty()
    raw = bytearray()
    child: subprocess.Popen[bytes] | None = None
    cleanup = "complete"
    entry = "manual-debugger-entry-required"
    stop_reason = "manual-entry-timeout"
    commands_sent = False
    deadline = time.monotonic() + args.manual_entry_timeout_seconds
    try:
        child = subprocess.Popen([str(args.binary), str(config_path)], stdin=slave_fd, stdout=slave_fd,
                                 stderr=slave_fd, start_new_session=True)
        os.close(slave_fd)
        slave_fd = -1
        while time.monotonic() < deadline:
            readable, _, _ = select.select([master_fd], [], [], 0.2)
            if readable:
                try:
                    chunk = os.read(master_fd, 65536)
                except OSError:
                    chunk = b""
                remaining = MAX_TRANSCRIPT_BYTES - len(raw)
                raw.extend(chunk[:remaining])
                if len(chunk) > remaining:
                    stop_reason = "artifact-limit"
                    break
            transcript = normalized_transcript(bytes(raw))
            if "uae debugger" in transcript.lower():
                entry = "pty-command"
                os.write(master_fd, ("\n".join(commands) + "\n").encode("ascii"))
                commands_sent = True
                stop_reason = "command-capture-timeout"
                deadline = time.monotonic() + args.post_command_timeout_seconds
                break
            if child.poll() is not None:
                stop_reason = "process-exit"
                break
        while commands_sent and time.monotonic() < deadline and child.poll() is None:
            readable, _, _ = select.select([master_fd], [], [], 0.2)
            if readable:
                try:
                    chunk = os.read(master_fd, 65536)
                except OSError:
                    chunk = b""
                remaining = MAX_TRANSCRIPT_BYTES - len(raw)
                raw.extend(chunk[:remaining])
                if len(chunk) > remaining:
                    stop_reason = "artifact-limit"
                    break
        if child.poll() is not None and stop_reason == "command-capture-timeout":
            stop_reason = "process-exit"
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
    raw_path.write_bytes(raw_bytes)
    transcript_path.write_text(normalized_transcript(raw_bytes), encoding="utf-8")
    report_path.write_text(json.dumps({
        "schema_version": 1,
        "mode": "fs-uae-console-debugger",
        "proof_level": "E",
        "fs_uae_binary": str(args.binary),
        "fs_uae_version": fs_uae_version(normalized_transcript(raw_bytes)),
        "entry": entry,
        "stop_reason": stop_reason,
        "commands_sent": commands_sent,
        "config": config_path.name,
        "raw_transcript": raw_path.name,
        "transcript": transcript_path.name,
        "process_ids": [] if child is None else [child.pid],
        "cleanup": cleanup,
        "raw_transcript_sha256": hashlib.sha256(raw_bytes).hexdigest(),
    }, indent=2) + "\n", encoding="utf-8")
    print(report_path)
    return 0 if cleanup == "complete" else 1


if __name__ == "__main__":
    raise SystemExit(main())

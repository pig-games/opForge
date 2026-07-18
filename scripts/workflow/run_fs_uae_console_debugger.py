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
import re
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
    entry_from_transcript,
    fs_uae_version,
    normalized_transcript,
)


OPT_IN_ENV = "OPFORGE_FS_UAE_CONSOLE_DEBUGGER"
AUTOMATION_OPT_IN_ENV = "OPFORGE_FS_UAE_CONSOLE_DEBUGGER_AUTOMATE"
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


def rendered_commands(lines: list[str], transcript: str) -> list[str]:
    values = {}
    pc_match = re.search(r"Next PC:\s*([0-9A-Fa-f]+)", transcript)
    a7_match = re.search(r"\bA7\s+([0-9A-Fa-f]+)", transcript)
    if pc_match:
        values["pc"] = pc_match.group(1)
    if a7_match:
        values["a7"] = a7_match.group(1)
    rendered = []
    for line in lines:
        for placeholder in re.findall(r"\{([^}]+)\}", line):
            if placeholder not in values:
                raise ValueError(f"debugger command requires unavailable {{{placeholder}}}")
        rendered.append(line.format(**values))
    return command_lines("\n".join(rendered))


def command_prompt_ready(transcript: str) -> bool:
    return entry_from_transcript(transcript) == "pty-command" and "Next PC:" in transcript and "\n>" in transcript


def frame_values(transcript: str) -> tuple[str | None, str | None, dict[str, str]]:
    pcs = re.findall(r"Next PC:\s*([0-9A-Fa-f]+)", transcript)
    frame_pattern = re.compile(
        r"^\s*D0\s+(?P<d0>[0-9A-Fa-f]{8})\s+D1\s+(?P<d1>[0-9A-Fa-f]{8})\s+"
        r"D2\s+(?P<d2>[0-9A-Fa-f]{8})\s+D3\s+(?P<d3>[0-9A-Fa-f]{8})\s*$\n"
        r"^\s*D4\s+(?P<d4>[0-9A-Fa-f]{8})\s+D5\s+(?P<d5>[0-9A-Fa-f]{8})\s+"
        r"D6\s+(?P<d6>[0-9A-Fa-f]{8})\s+D7\s+(?P<d7>[0-9A-Fa-f]{8})\s*$\n"
        r"^\s*A0\s+(?P<a0>[0-9A-Fa-f]{8})\s+A1\s+(?P<a1>[0-9A-Fa-f]{8})\s+"
        r"A2\s+(?P<a2>[0-9A-Fa-f]{8})\s+A3\s+(?P<a3>[0-9A-Fa-f]{8})\s*$\n"
        r"^\s*A4\s+(?P<a4>[0-9A-Fa-f]{8})\s+A5\s+(?P<a5>[0-9A-Fa-f]{8})\s+"
        r"A6\s+(?P<a6>[0-9A-Fa-f]{8})\s+A7\s+(?P<a7>[0-9A-Fa-f]{8})\s*$",
        re.MULTILINE,
    )
    frames = list(frame_pattern.finditer(transcript))
    registers = {}
    sr = None
    if frames:
        frame = frames[-1]
        registers = {name: f"0x{frame.group(name).upper()}" for name in frame.groupdict()}
        flags = re.search(
            r"T=(\d+)\s+S=(\d)\s+M=(\d)\s+X=(\d)\s+N=(\d)\s+Z=(\d)\s+V=(\d)\s+C=(\d)\s+IMASK=(\d+)",
            transcript[frame.end():],
        )
        if flags:
            trace, supervisor, master, extend, negative, zero, overflow, carry, imask = (
                int(value) for value in flags.groups()
            )
            sr_value = ((trace & 0x3) << 14) | (supervisor << 13) | (master << 12)
            sr_value |= (imask & 0x7) << 8
            sr_value |= (extend << 4) | (negative << 3) | (zero << 2) | (overflow << 1) | carry
            sr = f"0x{sr_value:04X}"
    return (f"0x{pcs[-1].upper()}" if pcs else None, sr, registers)


def self_test() -> None:
    assert command_lines("# frame\nr\nd 1000 4\nfl\n") == ["r", "d 1000 4", "fl"]
    for unsafe in ("W 100 1", "r d0 1", "f 1000", "ré", "r\t", ""):
        try:
            command_lines(unsafe)
        except ValueError:
            pass
        else:
            raise AssertionError(f"accepted unsafe command file: {unsafe!r}")
    assert rendered_commands(["d {pc} 4", "m {a7} 4"], "A7 00112233\nNext PC: 00F8134C") == [
        "d 00F8134C 4", "m 00112233 4"
    ]
    assert command_prompt_ready("WARNING: Activated debugger\nNext PC: 00F8134C\n> ")
    fixture = ("D0 00000001   D1 00000002   D2 00000003   D3 00000004\n"
               "D4 00000005   D5 00000006   D6 00000007   D7 00000008\n"
               "A0 00000009   A1 0000000A   A2 0000000B   A3 0000000C\n"
               "A4 0000000D   A5 0000000E   A6 0000000F   A7 00112233\n"
               "T=00 S=1 M=0 X=0 N=0 Z=1 V=0 C=0 IMASK=3\nNext PC: 00F8134C")
    assert frame_values(fixture) == (
        "0x00F8134C", "0x2304", {"d0": "0x00000001", "d1": "0x00000002", "d2": "0x00000003",
        "d3": "0x00000004", "d4": "0x00000005", "d5": "0x00000006", "d6": "0x00000007",
        "d7": "0x00000008", "a0": "0x00000009", "a1": "0x0000000A", "a2": "0x0000000B",
        "a3": "0x0000000C", "a4": "0x0000000D", "a5": "0x0000000E", "a6": "0x0000000F",
        "a7": "0x00112233"}
    )


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("--binary", type=Path, default=Path(DEFAULT_BINARY))
    result.add_argument("--template", type=Path, default=Path(DEFAULT_TEMPLATE))
    result.add_argument("--config", type=Path, help="prepared FS-UAE config with its Work mount")
    result.add_argument("--commands", type=Path, required=False)
    result.add_argument("--artifact-dir", type=Path, required=False)
    result.add_argument(
        "--start-file",
        type=Path,
        help="guest start marker; Cmd+D automation waits for a marker written after launch",
    )
    result.add_argument(
        "--clear-start-file",
        action="store_true",
        help="remove the explicit stale guest start marker before launch",
    )
    result.add_argument(
        "--after-start-delay-seconds",
        type=float,
        default=0.0,
        help="additional wait after observing a fresh guest start marker before Cmd+D",
    )
    result.add_argument("--manual-entry-timeout-seconds", type=float, default=60.0)
    result.add_argument("--post-command-timeout-seconds", type=float, default=10.0)
    result.add_argument("--send-mod-d-after-seconds", type=float)
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
    if args.after_start_delay_seconds < 0:
        print("--after-start-delay-seconds cannot be negative", file=sys.stderr)
        return 2
    if args.send_mod_d_after_seconds is not None:
        if args.send_mod_d_after_seconds < 0:
            print("--send-mod-d-after-seconds cannot be negative", file=sys.stderr)
            return 2
        if os.environ.get(AUTOMATION_OPT_IN_ENV) != "1":
            print(f"refusing GUI automation; set {AUTOMATION_OPT_IN_ENV}=1", file=sys.stderr)
            return 2
    source_config = args.config or args.template
    if not args.binary.is_file() or not os.access(args.binary, os.X_OK) or not source_config.is_file():
        print("FS-UAE binary or config template is unavailable", file=sys.stderr)
        return 2
    if args.clear_start_file:
        if args.start_file is None:
            print("--clear-start-file requires --start-file", file=sys.stderr)
            return 2
        try:
            args.start_file.unlink(missing_ok=True)
        except OSError as error:
            print(f"remove stale guest start marker {args.start_file}: {error}", file=sys.stderr)
            return 2

    artifact_dir = args.artifact_dir or Path("target") / f"fs-uae-console-debugger-{int(time.time())}"
    artifact_dir.mkdir(parents=True, exist_ok=False)
    config_path = artifact_dir / "console-debugger.config.fs-uae"
    raw_path = artifact_dir / "console-debugger.raw.log"
    transcript_path = artifact_dir / "console-debugger.transcript.txt"
    report_path = artifact_dir / "console-debugger.report.json"
    config_path.write_text(debug_config(source_config.read_text(encoding="utf-8")), encoding="utf-8")

    master_fd, slave_fd = pty.openpty()
    raw = bytearray()
    child: subprocess.Popen[bytes] | None = None
    cleanup = "complete"
    entry = "manual-debugger-entry-required"
    stop_reason = "manual-entry-timeout"
    commands_sent = False
    automation = "not-requested"
    guest_start_observed = False
    guest_start_observed_at: float | None = None
    received_signal: int | None = None

    def record_signal(signum: int, _frame: object) -> None:
        nonlocal received_signal
        received_signal = signum

    previous_sigterm = signal.signal(signal.SIGTERM, record_signal)
    previous_sighup = signal.signal(signal.SIGHUP, record_signal)
    automation_deadline = (time.monotonic() + args.send_mod_d_after_seconds
                           if args.send_mod_d_after_seconds is not None else None)
    deadline = time.monotonic() + args.manual_entry_timeout_seconds
    # Mounted Amiga volumes commonly have coarse (and sometimes rounded-down)
    # timestamps. A marker removed immediately before launch is therefore
    # authoritative once it reappears; comparing it to host nanoseconds is not.
    start_file_was_absent = args.start_file is not None and not args.start_file.exists()
    launch_wall_time_ns = time.time_ns()
    try:
        child = subprocess.Popen([str(args.binary), str(config_path)], stdin=slave_fd, stdout=slave_fd,
                                 stderr=slave_fd, start_new_session=True)
        os.close(slave_fd)
        slave_fd = -1
        while received_signal is None and time.monotonic() < deadline:
            if args.start_file is not None and not guest_start_observed:
                try:
                    guest_start_observed = start_file_was_absent or (
                        args.start_file.stat().st_mtime_ns >= launch_wall_time_ns
                    )
                    if guest_start_observed:
                        guest_start_observed_at = time.monotonic()
                except FileNotFoundError:
                    pass
            automation_ready = (
                args.start_file is None
                or (guest_start_observed_at is not None
                    and time.monotonic() >= guest_start_observed_at + args.after_start_delay_seconds)
            )
            if (automation_deadline is not None and automation_ready
                    and time.monotonic() >= automation_deadline):
                try:
                    subprocess.run(
                        [
                            "osascript",
                            "-e",
                            'tell application "FS-UAE" to activate',
                            "-e",
                            "delay 0.5",
                            "-e",
                            'tell application "System Events"',
                            "-e",
                            'tell process "FS-UAE"',
                            "-e",
                            "try",
                            "-e",
                            "click window 1",
                            "-e",
                            "end try",
                            "-e",
                            "end tell",
                            "-e",
                            "delay 0.2",
                            "-e",
                            "key code 2 using command down",
                            "-e",
                            "end tell",
                        ],
                        check=True,
                        capture_output=True,
                        text=True,
                        timeout=5,
                    )
                    automation = "sent"
                except subprocess.CalledProcessError as error:
                    detail = (error.stderr or error.stdout or str(error)).strip()
                    automation = f"failed: {detail}"
                except (OSError, subprocess.SubprocessError) as error:
                    automation = f"failed: {error}"
                automation_deadline = None
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
            if command_prompt_ready(transcript):
                entry = "pty-command"
                try:
                    command_text = "\n".join(rendered_commands(commands, transcript)) + "\n"
                except ValueError as error:
                    stop_reason = f"command-render-error: {error}"
                    break
                os.write(master_fd, command_text.encode("ascii"))
                commands_sent = True
                stop_reason = "command-capture-timeout"
                deadline = time.monotonic() + args.post_command_timeout_seconds
                break
            if child.poll() is not None:
                stop_reason = "process-exit"
                break
        while (received_signal is None and commands_sent and time.monotonic() < deadline
               and child.poll() is None):
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
        if received_signal is not None:
            stop_reason = "host-signal"
        elif child.poll() is not None and stop_reason == "command-capture-timeout":
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
        try:
            os.close(master_fd)
        except OSError:
            pass
        signal.signal(signal.SIGTERM, previous_sigterm)
        signal.signal(signal.SIGHUP, previous_sighup)

    raw_bytes = bytes(raw)
    transcript = normalized_transcript(raw_bytes)
    pc, sr, registers = frame_values(transcript)
    raw_path.write_bytes(raw_bytes)
    transcript_path.write_text(transcript, encoding="utf-8")
    report = {
        "schema_version": 1,
        "mode": "fs-uae-console-debugger",
        "proof_level": "E",
        "fs_uae_binary": str(args.binary),
        "fs_uae_version": fs_uae_version(transcript),
        "entry": entry,
        "stop_reason": stop_reason,
        "commands_sent": commands_sent,
        "automation": automation,
        "guest_start_file": None if args.start_file is None else str(args.start_file),
        "guest_start_observed": guest_start_observed,
        "after_start_delay_seconds": args.after_start_delay_seconds,
        "host_signal": received_signal,
        "config": config_path.name,
        "raw_transcript": raw_path.name,
        "transcript": transcript_path.name,
        "stack_dump": transcript_path.name,
        "disassembly": transcript_path.name,
        "registers": registers,
        "process_ids": [] if child is None else [child.pid],
        "cleanup": cleanup,
        "raw_transcript_sha256": hashlib.sha256(raw_bytes).hexdigest(),
    }
    if pc is not None:
        report["pc"] = pc
    if sr is not None:
        report["sr"] = sr
    report_path.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(report_path)
    return 0 if cleanup == "complete" else 1


if __name__ == "__main__":
    raise SystemExit(main())

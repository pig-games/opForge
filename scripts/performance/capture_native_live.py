#!/usr/bin/env python3
"""One read-only, incomplete console capture inside a coordinator-owned run tree.

This launcher never supplies parity authority. Sampling stops the guest;
non-interrupting controls observe timing only. The normal coordinator alone
validates the actual guest protocol, exit, diagnostics and output bytes.
"""

# @opforge-evidence: level=E; role=diagnostic-tool; authority=none; lifecycle=permanent

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import pty
import re
import select
import signal
import subprocess
import sys
import time

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "workflow"))
sys.path.insert(0, str(Path(__file__).resolve().parent))
from run_fs_uae_console_debugger import command_prompt_ready, frame_values, rendered_commands
from probe_fs_uae_console_debugger import MAX_TRANSCRIPT_BYTES, normalized_transcript, fs_uae_version
from decode_native_progress import (decode_progress, decode_work_multiplication,
                                    decode_symbol_expression_work, decode_runtime_execution, decode_platform_io)


def validate_resample(first: int, second: int | None, mode: str = "sample") -> None:
    if second is not None and (mode != "sample" or type(second) is not int
                               or not 1 <= first <= 95 or not first + 5 <= second <= 100):
        raise ValueError("resample requires a live sample, at least 5 seconds later and no later than 100 seconds")


def verified_source_inventory(work: Path, expected: dict) -> dict:
    """The discoverable guest inputs must match the frozen case, not just its argv."""
    inventory = {}
    total = 0
    for path in sorted(work.rglob("*")):
        if path.is_symlink():
            raise ValueError("guest inventory cannot contain symlinks")
        if path.is_file() and path.suffix.lower() in (".asm", ".inc"):
            with path.open("rb") as source:
                data = source.read(2_097_153)
            total += len(data)
            if len(data) > 2_097_152 or total > 8_388_608 or len(inventory) >= 64:
                raise ValueError("guest source inventory exceeds diagnostic bound")
            inventory[str(path.relative_to(work))] = {"bytes": len(data), "sha256": hashlib.sha256(data).hexdigest()}
    if inventory != expected:
        raise ValueError("discoverable guest source inventory differs from the frozen case")
    return inventory


def snapshot_records(transcript: str, locations: dict, expected_run_id: int | None = None,
                     expected_io: bool | None = None) -> dict:
    """Decode one pause only; never fill missing later bytes from an earlier stop."""
    memory = memory_bytes(transcript)
    try:
        records = {name: bytes(memory[address + i] for i in range(size))
                   for name, (address, size) in locations.items()}
    except KeyError as error:
        raise ValueError(f"snapshot memory unavailable: {error}") from error
    progress = decode_progress(records["ofpr"])
    if progress["state"] != "active" or progress["run_id"] == 0:
        raise ValueError("live snapshot requires an active nonzero run identity")
    if expected_run_id is not None and progress["run_id"] != expected_run_id:
        raise ValueError("resumed snapshot run identity changed")
    common = dict(expected_run_id=progress["run_id"], expected_state=progress["state"],
                  expected_exit_status=progress["exit_status"])
    progress["work_multiplication"] = decode_work_multiplication(records["ofwk"], **common)
    common.update(expected_phase=progress["phase_id"], expected_pass=progress["pass"])
    for name, key, decoder in (("ofse", "symbol_expression_work", decode_symbol_expression_work),
                               ("ofvm", "runtime_execution", decode_runtime_execution),
                               ("ofio", "platform_io", decode_platform_io)):
        progress[key] = decoder(records[name], **common)
    if expected_io is not None and progress["platform_io"]["enabled_groups"] != {"io": expected_io, "bulk": True}:
        raise ValueError("live counter groups do not match the requested diagnostic mode")
    if any(group["overflow_bits"] != 0 for group in [progress, *(progress[key] for key in
            ("work_multiplication", "symbol_expression_work", "runtime_execution", "platform_io"))]):
        raise ValueError("live snapshot counter overflow")
    return {"live_records": {name: list(data) for name, data in records.items()}, "profile": progress}


def finalize_snapshots(report: dict) -> None:
    """A final register frame must never borrow an earlier pause's counters."""
    if report["resample_after_seconds"] is not None:
        # Binding fields describe the initial PC only. Keep them within that
        # snapshot, including when a later stop fails or never happens.
        for key in ("code_binding", "binding_anchor", "unscoped_label_candidates", "label_scope"):
            if key in report:
                if report["snapshots"]:
                    report["snapshots"][0][key] = report[key]
                del report[key]
        report.pop("live_records", None)
        report.pop("record_locations", None)
    elif report["snapshots"]:
        report["live_records"] = report["snapshots"][-1]["live_records"]
        report["record_locations"] = report["snapshots"][-1]["record_locations"]
    report["resample_observed"] = bool(len(report["snapshots"]) == 2 and report["cleanup"] == "complete")


def hunk_segments(data: bytes) -> list[dict]:
    """Fail-closed reader for the producer's CODE/DATA/BSS + RELOC32 subset."""
    cursor = 0

    def word():
        nonlocal cursor
        if cursor + 4 > len(data):
            raise ValueError("truncated Hunk")
        value = int.from_bytes(data[cursor:cursor + 4], "big")
        cursor += 4
        return value

    if word() != 0x3f3 or word() != 0:
        raise ValueError("unsupported Hunk header")
    count, first, last = word(), word(), word()
    if not 1 <= count <= 16 or first != 0 or last != count - 1:
        raise ValueError("unsupported Hunk table")
    sizes = [(word() & 0x3fffffff) * 4 for _ in range(count)]
    result = []
    for index in range(count):
        kind, size = word(), word() * 4
        if kind not in (0x3e9, 0x3ea, 0x3eb) or size > sizes[index]:
            raise ValueError("unsupported Hunk segment")
        payload = b""
        if kind != 0x3eb:
            if cursor + size > len(data):
                raise ValueError("truncated Hunk payload")
            payload = data[cursor:cursor + size]
            cursor += size
        relocations = {}
        marker = word()
        if marker == 0x3ec:
            while (entries := word()) != 0:
                target = word()
                if entries > len(payload) // 2 or target >= count:
                    raise ValueError("invalid Hunk relocation group")
                for _ in range(entries):
                    offset = word()
                    # The producer accepts byte offsets, including odd DATA
                    # locations. Do not impose an instruction-alignment rule.
                    if offset + 4 > len(payload) or offset in relocations:
                        raise ValueError(f"invalid Hunk relocation offset: segment={index}, offset={offset}, "
                                         f"payload={len(payload)}, duplicate={offset in relocations}")
                    relocations[offset] = target
            marker = word()
        if marker != 0x3f2:
            raise ValueError("unsupported Hunk trailer")
        result.append({"index": index, "kind": kind, "bytes": payload,
                       "allocation_bytes": sizes[index], "relocations": relocations})
    if cursor != len(data):
        raise ValueError("unconsumed Hunk data")
    return result


def bind_code(transcript: str, segments: list[dict], pc: int) -> dict:
    instructions = {}
    for match in re.finditer(r"(?:^|>)([0-9A-Fa-f]{8}) ((?:[0-9A-Fa-f]{4} )+) {2,}", transcript, re.MULTILINE):
        instructions[int(match[1], 16)] = bytes.fromhex(match[2])
    sequence = bytearray()
    while pc + len(sequence) in instructions:
        sequence.extend(instructions[pc + len(sequence)])
    if len(sequence) < 32:
        raise ValueError("insufficient contiguous disassembly for binary binding")
    matches = []
    for segment in segments:
        if segment["kind"] != 0x3e9:
            continue
        payload = segment["bytes"]
        masked = {offset + byte for offset in segment["relocations"] for byte in range(4)}
        offset = payload.find(sequence[:2])
        while offset >= 0:
            checked = [i for i in range(len(sequence)) if offset + i not in masked]
            if (offset % 2 == 0 and len(checked) >= 24 and offset + len(sequence) <= len(payload)
                    and all(payload[offset + i] == sequence[i] for i in checked)):
                matches.append({"segment": segment["index"], "offset": offset, "runtime_base": pc - offset,
                                "matched_bytes": len(sequence), "unrelocated_bytes_checked": len(checked)})
            offset = payload.find(sequence[:2], offset + 1)
    if len(matches) != 1:
        raise ValueError(f"runtime code binding is ambiguous: {len(matches)} matches")
    return matches[0]


def memory_bytes(transcript: str) -> dict[int, int]:
    result = {}
    for match in re.finditer(r"(?:^|>)([0-9A-Fa-f]{8}) ((?:[0-9A-Fa-f]{4} ){7}[0-9A-Fa-f]{4})  ", transcript, re.MULTILINE):
        result.update((int(match[1], 16) + i, byte) for i, byte in enumerate(bytes.fromhex(match[2])))
    return result


def fresh_start(path: Path, expected: str) -> bool:
    """Caller checks absence before launch; require the exact fresh challenge."""
    try:
        with path.open("rb") as source:
            contents = source.read(1025)
            return len(contents) <= 1024 and contents.strip() == expected.encode("ascii")
    except FileNotFoundError:
        return False


def console_config(text: str, enabled: bool = True) -> str:
    lines = [line for line in text.splitlines()
             if line.split("=", 1)[0].strip().lower().replace("-", "_") != "console_debugger"]
    if [line.strip().lower() for line in lines if line.strip().startswith("[")] != ["[fs-uae]"]:
        raise ValueError("expected one FS-UAE config section")
    return "\n".join([*lines, f"console_debugger = {int(enabled)}", ""])


def control_completion(started_at: float | None, done_file: Path, expected_done: str,
                       exit_file: Path, now: float) -> dict | None:
    try:
        if started_at is None or not fresh_start(done_file, expected_done):
            return None
        with exit_file.open("rb") as source:
            raw = source.read(65)
        if len(raw) > 64:
            return None
        text = raw.decode("ascii").strip()
    except (OSError, UnicodeDecodeError):
        return None
    if not re.fullmatch(r"-?[0-9]{1,10}", text):
        return None
    status = int(text)
    if not -(2 ** 31) <= status < 2 ** 31:
        return None
    return {"expected_done_observed": expected_done, "guest_exit_observed": status,
            "start_to_done_host_seconds": now - started_at,
            "poll_interval_seconds": 0.1, "observed_before_cleanup": True}


def enter_debugger(pid: int) -> dict:
    # Target only the process we launched, never a different emulator window.
    if type(pid) is not int or pid <= 0:
        raise ValueError("debugger entry requires a positive launched PID")
    result = subprocess.run(["osascript", "-e", 'tell application "System Events"',
                    "-e", f"set targetProcess to first application process whose unix id is {pid}",
                    "-e", "tell targetProcess", "-e", "set frontmost to true",
                    "-e", "end tell", "-e", "delay 0.5",
                    "-e", "set beforePid to unix id of first application process whose frontmost is true",
                    "-e", f'if beforePid is not {pid} then error "debugger target is not foreground; key not sent"',
                    "-e", "tell targetProcess", "-e", "key code 2 using command down", "-e", "end tell",
                    "-e", "set afterPid to unix id of first application process whose frontmost is true",
                    "-e", 'return (beforePid as text) & ":" & (afterPid as text)', "-e", "end tell"],
                   check=True, capture_output=True, text=True, timeout=5)
    match = re.fullmatch(r"([1-9][0-9]*):([1-9][0-9]*)", result.stdout.strip())
    if match is None or int(match[1]) != pid:
        raise ValueError("debugger foreground receipt is invalid")
    # These are host focus observations, never delivery or guest-pause proof.
    return {"target_pid": pid, "foreground_before_pid": int(match[1]),
            "foreground_after_pid": int(match[2]), "key_request_returned": True,
            "prompt_acknowledged": False}


def begin_debugger_pause(pid: int, raw: bytearray):
    # Capture the boundary before requesting the key: earlier frames cannot
    # acknowledge this pause, including the first debugger entry.
    offset = len(raw)
    return offset, enter_debugger(pid)


def acknowledge_debugger_entry(entry: dict, pause_transcript: str, elapsed: float) -> bool:
    """Only a fresh pause-local command prompt with a complete frame is an ack."""
    if not command_prompt_ready(pause_transcript):
        return False
    pc, sr, registers = frame_values(pause_transcript)
    if pc is None or sr is None or len(registers) != 16:
        return False
    entry.update(prompt_acknowledged=True, prompt_after_start_seconds=elapsed)
    return True


def finalize_debugger_entries(report: dict) -> None:
    entries = report["debugger_entries"]
    if (report["stop_reason"] == "deadline" and entries
            and entries[-1].get("key_request_returned") is True
            and not entries[-1].get("prompt_acknowledged")):
        report["stop_reason"] = "debugger-prompt-timeout"


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", type=Path, required=True)
    parser.add_argument("--config", type=Path, required=True)
    parser.add_argument("--hunk", type=Path, required=True)
    parser.add_argument("--labels", type=Path, required=True)
    parser.add_argument("--start-file", type=Path, required=True)
    parser.add_argument("--expected-start", required=True)
    parser.add_argument("--after-start-seconds", type=int, required=True)
    parser.add_argument("--control-mode", choices=("sample", "app", "pty", "console"), default="sample")
    parser.add_argument("--done-file", type=Path, required=True)
    parser.add_argument("--expected-done", required=True)
    parser.add_argument("--exit-file", type=Path, required=True)
    args = parser.parse_args()
    profile_mode = os.environ.get("OPFORGE_NATIVE_CORPUS_PROFILE")
    if profile_mode not in ("all", "all-no-io"):
        parser.error("live diagnostic requires an explicit all/all-no-io profile mode")
    resample_text = os.environ.get("OPFORGE_NATIVE_CORPUS_RESAMPLE_AFTER_SECONDS")
    try:
        resample_after = int(resample_text) if resample_text is not None else None
        validate_resample(args.after_start_seconds, resample_after, args.control_mode)
    except ValueError as error:
        parser.error(str(error))
    binding_register = os.environ.get("OPFORGE_NATIVE_CORPUS_BINDING_REGISTER")
    if binding_register is not None and (not re.fullmatch(r"[da][0-7]", binding_register)
                                         or args.control_mode != "sample"):
        parser.error("binding register requires a sample and one D0-D7/A0-A7 register")
    required = ("OPFORGE_NATIVE_CORPUS_LIVE_CAPTURE", "OPFORGE_NATIVE_CORPUS_DIAGNOSTIC",
                "OPFORGE_PERFORMANCE_CORPUS")
    if args.control_mode == "sample":
        required += ("OPFORGE_FS_UAE_CONSOLE_DEBUGGER_AUTOMATE",)
    if any(os.environ.get(key) != "1" for key in required):
        parser.error("live capture requires explicit corpus, diagnostic and GUI automation opt-ins")
    if args.control_mode == "sample" and not 1 <= args.after_start_seconds <= 100:
        parser.error("sample delay must be in 1..100 seconds (inside the 120-second coordinator ceiling)")
    if args.control_mode != "sample" and args.after_start_seconds != 0:
        parser.error("controls cannot request debugger entry")
    if not re.fullmatch(r"OPFORGE-FS-UAE-PROOF-V1 START [A-Za-z0-9_-]+ [A-Za-z0-9_-]+", args.expected_start):
        parser.error("invalid fresh start challenge")
    if args.expected_done != args.expected_start.replace(" START ", " DONE ", 1):
        parser.error("completion challenge must match the fresh start")
    if any(path.exists() for path in (args.start_file, args.done_file, args.exit_file)):
        parser.error("protocol file already exists before launch")

    config = console_config(args.config.read_text(), args.control_mode in ("sample", "console"))
    # All files remain inside the existing RAII-owned ephemeral run tree.
    config_path = args.config.with_name("live-console.fs-uae")
    with config_path.open("x") as destination:
        destination.write(config)
    hunk = args.hunk.read_bytes()
    segments = hunk_segments(hunk) if args.control_mode == "sample" else []
    labels_text = args.labels.read_text()
    labels = {name: int(value, 16) for name, value in re.findall(r"^(.+) = \$([0-9A-Fa-f]+)$", labels_text, re.MULTILINE)}
    manifest_path = Path(__file__).resolve().parents[2] / "documentation/performance/results/opforge-corpus-v1-manifest.json"
    frozen = json.loads(manifest_path.read_text())
    selected = [case for case in frozen["cases"] if case["id"] == os.environ.get("OPFORGE_NATIVE_CORPUS_CASES")]
    if len(selected) != 1 or args.hunk.parent.name != "build":
        parser.error("live inventory requires one frozen case and the canonical guest build path")
    inventory = verified_source_inventory(args.hunk.parent.parent, selected[0]["files"])
    report = {"schema_version": 1, "proof_level": "E", "mode": "native-live-console",
              "complete": False, "parity_passed": False, "protocol_completed": False,
              "expected_start": args.expected_start, "guest_start_observed": False,
              "after_start_seconds": args.after_start_seconds,
              "native_binary_sha256": hashlib.sha256(hunk).hexdigest(), "native_binary_bytes": len(hunk),
              "config_sha256": hashlib.sha256(config.encode()).hexdigest(),
              "config": config, "sampler_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
              "sample_observed": False, "cleanup": "complete", "commands": []}
    report["labels_sha256"] = hashlib.sha256(labels_text.encode()).hexdigest()
    report["control_mode"] = args.control_mode
    report["profile_mode"] = profile_mode
    report["source_inventory"] = inventory
    report["source_inventory_matches_frozen"] = True
    report["control_completion"] = None
    report["binding_register"] = binding_register
    report["resample_after_seconds"] = resample_after
    report["snapshots"] = []
    report["debugger_entries"] = []
    report["segments"] = [{key: row[key] for key in ("index", "kind", "allocation_bytes")} for row in segments]
    master, slave = pty.openpty()
    child = None
    raw = bytearray()
    received_signal = None

    def record_signal(signum, _frame):
        nonlocal received_signal
        received_signal = signum

    previous = {sig: signal.signal(sig, record_signal) for sig in (signal.SIGTERM, signal.SIGHUP)}
    started_at = None
    entered = False
    sent = False
    memory_stage = 0
    binding_candidate = None
    binding_ready_prompts = 6
    memory_prompt_offset = 0
    getters = {}
    record_locations = {}
    retained_locations = None
    retained_run_id = None
    pause_offset = 0
    next_sample_after = args.after_start_seconds
    record_specs = {"opasmProgressGetRecordV1": ("ofpr", 128, b"OFPR"),
                    "opasmProgressGetWorkRecordV1": ("ofwk", 128, b"OFWM"),
                    "opforgeSymbolExprProfileGetRecordV1": ("ofse", 256, b"OFSE"),
                    "opforgeRuntimeProfileGetRecordV1": ("ofvm", 192, b"OFVE"),
                    "opforgePlatformProfileGetRecordV1": ("ofio", 528, b"OFIO")}
    deadline = time.monotonic() + 180
    report["stop_reason"] = "deadline"
    try:
        command = [str(args.binary), str(config_path)]
        if args.control_mode == "app":
            bundle = next((parent for parent in args.binary.parents if parent.suffix == ".app"), None)
            if sys.platform != "darwin" or bundle is None:
                raise ValueError("app control requires the macOS FS-UAE application bundle")
            command = ["/usr/bin/open", "-W", "-n", str(bundle), "--args", str(config_path)]
        child = subprocess.Popen(command, stdin=slave, stdout=slave,
                                 stderr=slave, start_new_session=True)
        report["process_id"] = child.pid
        os.close(slave)
        slave = -1
        while received_signal is None and time.monotonic() < deadline:
            now = time.monotonic()
            if started_at is None and fresh_start(args.start_file, args.expected_start):
                started_at = now
                report["guest_start_observed"] = True
                deadline = now + (args.after_start_seconds + 15 if args.control_mode == "sample" else 115)
            report["control_completion"] = control_completion(started_at, args.done_file, args.expected_done,
                                                              args.exit_file, now)
            if (args.control_mode == "sample" and report["control_completion"] is None
                    and started_at is not None and not entered
                    and now - started_at >= next_sample_after):
                entry = {"target_pid": child.pid, "snapshot_index": len(report["snapshots"]),
                         "request_after_start_seconds": now - started_at,
                         "key_request_returned": False, "prompt_acknowledged": False}
                report["debugger_entries"].append(entry)
                pause_offset, key_receipt = begin_debugger_pause(child.pid, raw)
                entry.update(key_receipt)
                entered = True
                report["debugger_requested_after_start_seconds"] = time.monotonic() - started_at
                report["control_completion"] = control_completion(started_at, args.done_file, args.expected_done,
                                                                  args.exit_file, time.monotonic())
            if report["control_completion"] is not None:
                report["stop_reason"] = ("guest-completed-before-sample" if args.control_mode == "sample"
                                         else "control-guest-completion")
                break
            readable, _, _ = select.select([master], [], [], 0.1)
            if readable:
                try:
                    chunk = os.read(master, 65536)
                except OSError:
                    chunk = b""
                remaining = MAX_TRANSCRIPT_BYTES - len(raw)
                raw.extend(chunk[:remaining])
                if len(chunk) > remaining:
                    report["stop_reason"] = "transcript-limit"
                    break
            # Prompt counts, frames and memory are local to this pause. Earlier
            # transcript bytes remain in the report but cannot satisfy a read.
            transcript = normalized_transcript(bytes(raw[pause_offset:]))
            if entered and not sent and acknowledge_debugger_entry(
                    report["debugger_entries"][-1], transcript, time.monotonic() - started_at):
                # Only the prompt establishes that the guest is now paused.
                # It may have completed between Cmd+D and this PTY read.
                report["control_completion"] = control_completion(started_at, args.done_file, args.expected_done,
                                                                  args.exit_file, time.monotonic())
                if report["control_completion"] is not None:
                    report["stop_reason"] = "guest-completed-before-sample"
                    break
                # Fixed read-only commands: no register/memory writes, stepping,
                # breakpoints or arbitrary user-supplied command strings.
                commands = rendered_commands(["r", "d {pc} 16", "m {a7} 16", "Zl", "H 32"], transcript)
                os.write(master, ("\n".join(commands) + "\n").encode("ascii"))
                report["commands"].extend(commands)
                report["frame_after_start_seconds"] = time.monotonic() - started_at
                sent = True
                deadline = min(deadline, time.monotonic() + 12)
            prompts = len(re.findall(r"(?:^|\n)>", transcript))
            if sent and memory_stage == 0 and prompts >= binding_ready_prompts:
                memory_stage = 1
                try:
                    pc_text, _, _ = frame_values(transcript)
                    anchor = binding_candidate if binding_candidate is not None else int(pc_text, 16)
                    binding = (report["code_binding"] if retained_locations is not None
                               else bind_code(transcript, segments, anchor))
                    report["code_binding"] = binding
                    report.pop("binding_error", None)
                    if retained_locations is None:
                        report["binding_anchor"] = {"address": anchor,
                                                "origin": binding_register if binding_candidate is not None else "next_pc",
                                                "sampled_pc_in_bound_code": binding_candidate is None}
                    near = [(name, value) for name, value in labels.items()
                            if value <= binding["offset"] and "." in name and not name.rsplit(".", 1)[-1].isupper()]
                    # Default label output has section-relative values but no
                    # section identity. These are hints, not code neighbors.
                    report["unscoped_label_candidates"] = sorted(near, key=lambda item: -item[1])[:12]
                    report["label_scope"] = "section unknown; candidates require independent CODE verification"
                    for suffix in record_specs:
                        found = [(name, value) for name, value in labels.items() if name.endswith("." + suffix)]
                        if len(found) != 1:
                            raise ValueError(f"expected one record getter: {suffix}")
                        name, offset = found[0]
                        segment = segments[binding["segment"]]
                        if segment["bytes"][offset:offset + 2] != b"\x41\xf9" or offset + 2 not in segment["relocations"]:
                            raise ValueError(f"getter is not a relocated absolute LEA: {name}")
                        getters[suffix] = binding["runtime_base"] + offset
                    commands = [f"m {address:08x} 1" for address in getters.values()]
                    report["commands"].extend(commands)
                    os.write(master, ("\n".join(commands) + "\n").encode("ascii"))
                except ValueError as error:
                    report["binding_error"] = str(error)
                    memory_stage = -1
                    if binding_register is not None and binding_candidate is None and "code_binding" not in report:
                        _, _, registers = frame_values(transcript)
                        candidate_text = registers.get(binding_register)
                        if candidate_text is not None:
                            binding_candidate = int(candidate_text, 16)
                            command = f"d {binding_candidate:08x} 16"
                            report["commands"].append(command)
                            report["primary_binding_error"] = str(error)
                            memory_prompt_offset = 1
                            binding_ready_prompts = prompts + 1
                            os.write(master, (command + "\n").encode("ascii"))
                            memory_stage = 0
            if memory_stage == 1 and prompts >= 11 + memory_prompt_offset:
                memory_stage = 2
                memory = memory_bytes(transcript)
                try:
                    for suffix, address in getters.items():
                        if bytes(memory[address + i] for i in range(2)) != b"\x41\xf9":
                            raise ValueError("live getter opcode changed")
                        pointer = int.from_bytes(bytes(memory[address + i] for i in range(2, 6)), "big")
                        name, size, _ = record_specs[suffix]
                        if pointer == 0 or pointer + size > 0x100000000:
                            raise ValueError("invalid live record pointer")
                        record_locations[name] = (pointer, size)
                    if retained_locations is not None and record_locations != retained_locations:
                        raise ValueError("resumed record locations changed")
                    commands = [f"m {address:08x} {(size + 15) // 16:x}" for address, size in record_locations.values()]
                    report["commands"].extend(commands)
                    os.write(master, ("\n".join(commands) + "\n").encode("ascii"))
                except (KeyError, ValueError) as error:
                    report["binding_error"] = f"record getter memory unavailable: {error}"
                    memory_stage = -1
            if memory_stage == 2 and prompts >= 16 + memory_prompt_offset:
                try:
                    snapshot = snapshot_records(transcript, record_locations, retained_run_id,
                                                expected_io=profile_mode == "all")
                    pc, sr, registers = frame_values(transcript)
                    if not pc or len(registers) != 16:
                        raise ValueError("snapshot register frame missing")
                    snapshot.update(pc=pc, sr=sr, registers=registers,
                                    frame_after_start_seconds=report["frame_after_start_seconds"],
                                    records_after_start_seconds=time.monotonic() - started_at,
                                    record_locations=dict(record_locations),
                                    mapping_snapshot_index=0,
                                    mapping_origin="same-process-retained" if retained_locations is not None else "unique-hunk-binding")
                    report["snapshots"].append(snapshot)
                except (KeyError, ValueError) as error:
                    report["record_error"] = str(error)
                    report["stop_reason"] = "snapshot-rejected"
                    break
                if resample_after is not None and retained_locations is None:
                    if time.monotonic() - started_at >= resample_after - 2:
                        report["stop_reason"] = "insufficient-resample-window"
                        break
                    retained_locations = dict(record_locations)
                    retained_run_id = snapshot["profile"]["run_id"]
                    # Bare g resumes at the existing PC; it supplies no success
                    # inference and never changes registers or memory itself.
                    os.write(master, b"g\n")
                    report["commands"].append("g")
                    report["resume_requested_after_start_seconds"] = time.monotonic() - started_at
                    pause_offset = len(raw)
                    next_sample_after = resample_after
                    deadline = started_at + resample_after + 15
                    entered = sent = False
                    memory_stage = memory_prompt_offset = 0
                    binding_ready_prompts = 6
                    binding_candidate = None
                    continue
                report["stop_reason"] = "bounded-sample-complete"
                break
            if child.poll() is not None:
                report["stop_reason"] = "process-exit"
                break
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        report["stop_reason"] = "capture-error"
        report["error"] = str(error)
        if isinstance(error, subprocess.CalledProcessError):
            report["error_detail"] = error.stderr
    finally:
        # The coordinator may notice DONE first and send SIGTERM. Observe the
        # exact files before teardown; never include cleanup time in timing.
        if args.control_mode != "sample" and report["control_completion"] is None:
            report["control_completion"] = control_completion(started_at, args.done_file, args.expected_done,
                                                              args.exit_file, time.monotonic())
        if slave >= 0:
            os.close(slave)
        if child is not None:
            try:
                if child.poll() is None:
                    os.killpg(child.pid, signal.SIGTERM)
                child.wait(timeout=3)
            except (OSError, subprocess.TimeoutExpired):
                try:
                    os.killpg(child.pid, signal.SIGKILL)
                    child.wait(timeout=3)
                except (OSError, subprocess.TimeoutExpired):
                    report["cleanup"] = "incomplete"
        os.close(master)
        for sig, handler in previous.items():
            signal.signal(sig, handler)
    transcript = normalized_transcript(bytes(raw))
    pc, sr, registers = frame_values(normalized_transcript(bytes(raw[pause_offset:])))
    report.update(pc=pc, sr=sr, registers=registers, transcript=transcript,
                  raw_transcript_sha256=hashlib.sha256(raw).hexdigest(),
                  fs_uae_version=fs_uae_version(transcript), host_signal=received_signal)
    report["sample_observed"] = bool(sent and pc and len(registers) == 16
                                     and report["guest_start_observed"] and report["cleanup"] == "complete")
    finalize_snapshots(report)
    finalize_debugger_entries(report)
    print("CORPUS_LIVE_CAPTURE " + json.dumps(report, separators=(",", ":")), flush=True)
    return 1  # Launcher status never substitutes for the coordinator's guest proof.


if __name__ == "__main__":
    raise SystemExit(main())

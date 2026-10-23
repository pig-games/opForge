#!/usr/bin/env python3
"""Measure serial compact-native active-include controls in caller's FS-UAE env."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import re
import subprocess
import sys

sys.path.insert(0, str(Path(__file__).resolve().parent))
from native_case_runner import hunk_segments


TEST_NAME = "native_active_include_control_fs_uae"
TIMING = re.compile(
    r"\bITERABLE_CASE name=active-include-control cpu=([A-Za-z0-9_-]+) "
    r"failure=false source_bytes=([0-9]+) seconds=Some\(([^)]+)\)",
    re.MULTILINE,
)
LABEL = re.compile(r"[A-Za-z0-9][A-Za-z0-9._-]{0,63}\Z", re.ASCII)
MEMORY_REQUIRED = {
    "eclock_hz", "live_owned_bytes", "packed_source_bytes", "peak_owned_bytes",
    "preparation_stages", "runtime_prefix_bytes", "source_bytes_read",
    "total_allocated_bytes", "total_freed_bytes",
}
FS_ENV_KEYS = (
    "OPFORGE_FS_UAE_SMOKE", "OPFORGE_FS_UAE_BIN", "OPFORGE_FS_UAE_CONFIG_TEMPLATE",
    "OPFORGE_FS_UAE_ARGS", "OPFORGE_FS_UAE_MEMORY_PROFILE", "OPFORGE_FS_UAE_TIMEOUT_MS",
    "OPFORGE_FS_UAE_POST_START_TIMEOUT_MS", "OPFORGE_FS_UAE_POLL_MS",
)


def image_argument(value: str) -> tuple[str, Path]:
    """Parse LABEL=PATH without allowing labels to escape the log directory."""
    label, separator, path = value.partition("=")
    if not separator or not LABEL.fullmatch(label) or label in {".", ".."}:
        raise argparse.ArgumentTypeError(
            "expected LABEL=PATH; LABEL must be 1-64 ASCII letters, digits, '.', '_' or '-' "
            "and start with a letter or digit"
        )
    if not path:
        raise argparse.ArgumentTypeError("image path cannot be empty")
    return label, Path(path)


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=("Run native_active_include_control_fs_uae serially for each supplied image. "
                     "FS-UAE configuration is inherited from the caller's environment."),
    )
    parser.add_argument("--test-binary", required=True, type=Path,
                        help="compiled Rust test binary containing the native FS-UAE test")
    parser.add_argument("--release-image", action="append", default=[], type=image_argument,
                        metavar="LABEL=PATH", help="un-instrumented Hunk image (repeatable)")
    parser.add_argument("--memory-image", action="append", default=[], type=image_argument,
                        metavar="LABEL=PATH", help="instrumented Hunk image (repeatable)")
    parser.add_argument("--report", required=True, type=Path, help="incrementally written JSON report")
    parser.add_argument("--logs-dir", type=Path,
                        help="directory for per-label logs (default: <report-stem>-logs beside report)")
    args = parser.parse_args(argv)
    if not args.release_image and not args.memory_image:
        parser.error("supply at least one --release-image or --memory-image")
    all_images = [*args.release_image, *args.memory_image]
    labels = [label for label, _ in all_images]
    if len(labels) != len(set(labels)):
        parser.error("image labels must be unique")
    args.images = [(label, path, False) for label, path in args.release_image]
    args.images.extend((label, path, True) for label, path in args.memory_image)
    if args.logs_dir is None:
        args.logs_dir = args.report.parent / f"{args.report.stem}-logs"
    return args


def valid_memory_record(value: object) -> bool:
    """Check the JSON shape emitted only after the Rust MEMD decoder accepts bytes."""
    if not isinstance(value, dict) or not MEMORY_REQUIRED.issubset(value):
        return False
    integer_fields = MEMORY_REQUIRED - {"preparation_stages"}
    if any(type(value.get(field)) is not int or value[field] < 0 for field in integer_fields):
        return False
    stages = value.get("preparation_stages")
    if not isinstance(stages, list) or not stages:
        return False
    return all(isinstance(stage, dict) and isinstance(stage.get("name"), str)
               and type(stage.get("calls")) is int and stage["calls"] >= 0
               and type(stage.get("seconds")) in (int, float)
               and math.isfinite(stage["seconds"]) and stage["seconds"] >= 0
               for stage in stages)


def completed_test(output: str, returncode: int, require_memory: bool = False) -> tuple[float, dict | None, str, int]:
    if returncode != 0:
        raise ValueError(f"test exited with status {returncode}")
    if not re.search(r"^running 1 test$", output, re.MULTILINE):
        raise ValueError("test output did not report exactly one running test")
    heading_pattern = r"^test (?:[\w:]+::)?native_active_include_control_fs_uae \.\.\.(?:[^\n]*)$"
    headings = re.findall(heading_pattern,
                          output, re.MULTILINE)
    if len(headings) != 1:
        raise ValueError(f"{TEST_NAME} did not report success")
    heading = re.search(heading_pattern, output, re.MULTILINE)
    if not re.search(r"^ok$", output[heading.end():], re.MULTILINE):
        raise ValueError(f"{TEST_NAME} did not report success")
    if not re.search(r"^test result: ok\. 1 passed; 0 failed;", output, re.MULTILINE):
        raise ValueError("test summary did not report exactly 1 passed and 0 failed")
    timings = TIMING.findall(output)
    if len(timings) != 1:
        raise ValueError("expected exactly one completed ITERABLE_CASE timing")
    try:
        seconds = float(timings[0][2])
    except ValueError as error:
        raise ValueError("invalid completed ITERABLE_CASE timing") from error
    if not math.isfinite(seconds) or seconds < 0:
        raise ValueError("completed ITERABLE_CASE timing must be finite and nonnegative")
    records = []
    for line in output.splitlines():
        if line.startswith("ITERABLE_MEMORY "):
            try:
                record = json.loads(line[len("ITERABLE_MEMORY "):])
            except json.JSONDecodeError as error:
                raise ValueError("invalid ITERABLE_MEMORY JSON") from error
            if not valid_memory_record(record):
                raise ValueError("ITERABLE_MEMORY JSON does not match the decoded MEMD schema")
            records.append(record)
    if require_memory and len(records) != 1:
        raise ValueError("instrumented image requires exactly one fresh MEMD record")
    if not require_memory and records:
        raise ValueError("release image unexpectedly emitted a MEMD record")
    return seconds, records[0] if records else None, timings[0][0], int(timings[0][1])


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    test_binary = args.test_binary.resolve()
    if not test_binary.is_file() or not os.access(test_binary, os.X_OK):
        raise SystemExit(f"test binary is not an executable file: {test_binary}")
    for label, image, _ in args.images:
        if not image.is_file():
            raise SystemExit(f"{label}: image is not a file: {image}")
    report = args.report.resolve()
    logs_dir = args.logs_dir.resolve()
    if report.exists():
        raise SystemExit(f"report already exists: {report}")
    if logs_dir.exists() and any(logs_dir.iterdir()):
        raise SystemExit(f"logs directory is not empty: {logs_dir}")
    logs_dir.mkdir(parents=True, exist_ok=True)
    report.parent.mkdir(parents=True, exist_ok=True)
    binary_data = test_binary.read_bytes()
    report_metadata = {
        "test_binary": str(test_binary),
        "test_binary_bytes": len(binary_data),
        "test_binary_sha256": hashlib.sha256(binary_data).hexdigest(),
        "test": TEST_NAME,
        "command_suffix": [TEST_NAME, "--ignored", "--test-threads=1", "--nocapture"],
        "fs_uae_environment": {key: os.environ[key] for key in FS_ENV_KEYS if key in os.environ},
    }
    rows: list[dict] = []
    for label, image, instrumented in args.images:
        log_path = logs_dir / f"{label}.log"
        if log_path.exists():
            raise SystemExit(f"log already exists: {log_path}")
        row = {"label": label, "instrumented": instrumented, "image": str(image.resolve()),
               "log": str(log_path), "status": "running"}
        rows.append(row)
        report.write_text(json.dumps({"run": report_metadata, "images": rows}, indent=2) + "\n")
        run_env = os.environ.copy()
        run_env["OPFORGE_TEMPLATE_IMAGE"] = str(image.resolve())
        if instrumented:
            run_env["OPFORGE_ITERABLE_MEMORY"] = "1"
        else:
            run_env.pop("OPFORGE_ITERABLE_MEMORY", None)
        with log_path.open("x", encoding="utf-8") as log:
            result = subprocess.run(
                [str(test_binary), TEST_NAME, "--ignored", "--test-threads=1", "--nocapture"],
                env=run_env, stdout=log, stderr=subprocess.STDOUT, check=False,
            )
        output = log_path.read_text(encoding="utf-8", errors="replace")
        try:
            seconds, memory, cpu, source_bytes = completed_test(output, result.returncode, instrumented)
            data = image.read_bytes()
            row.update(status="completed", cpu=cpu, source_bytes=source_bytes, seconds=seconds, image_bytes=len(data),
                       sha256=hashlib.sha256(data).hexdigest(),
                       linked_reserved_bytes=sum(segment["allocation_bytes"]
                                                 for segment in hunk_segments(data)))
            if memory is not None:
                row["memory"] = memory
        except (OSError, ValueError) as error:
            row.update(status="failed", error=str(error), exit_code=result.returncode)
            report.write_text(json.dumps({"run": report_metadata, "images": rows}, indent=2) + "\n")
            raise SystemExit(f"{label} failed: {error}; inspect {log_path}") from error
        report.write_text(json.dumps({"run": report_metadata, "images": rows}, indent=2) + "\n")
        print(f"{label}: {seconds:.6f} seconds", flush=True)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

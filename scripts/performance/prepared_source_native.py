#!/usr/bin/env python3
"""Bounded real-native baseline for the prepared-source S1 workload."""
import argparse
import json
import os
import platform
from pathlib import Path
import subprocess
import time

import runtime_comparison as runtime
import vm_efficiency as base


def workload(cpu, blocks=8):
    if cpu not in ("m6502", "m68000") or blocks != 8:
        raise ValueError("S1 native baseline supports exactly 8 blocks on m6502 or m68000")
    lines = [f".cpu {cpu}", ".org $0100"]
    expected = bytearray()
    pc = 0x100
    for index in range(blocks):
        lines.extend([
            f"start{index}:",
            "  NOP",
            f".byte end{index} - start{index}",
            f".word end{index} + 1",
            f"end{index}:",
            "  NOP",
        ])
        if cpu == "m6502":
            expected.extend((0xEA, 4))
            expected.extend((pc + 5).to_bytes(2, "little"))
            expected.append(0xEA)
            pc += 5
        else:
            expected.extend((0x4E, 0x71, 5))
            expected.extend((pc + 6).to_bytes(2, "big"))
            expected.extend((0x4E, 0x71))
            pc += 7
    lines.append(".end")
    return "\n".join(lines) + "\n", bytes(expected)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--native-test", type=Path, required=True)
    parser.add_argument("--package", type=Path, default=base.ROOT / "native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm")
    parser.add_argument("--profile", choices=("off", "runtime"), default="off")
    parser.add_argument("--cpus", nargs="+", choices=("m6502", "m68000"), default=["m6502", "m68000"])
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    native_test = args.native_test.resolve(strict=True)
    package = args.package.resolve(strict=True)
    template = Path(os.environ["OPFORGE_FS_UAE_CONFIG_TEMPLATE"]).resolve(strict=True)
    output = (args.output or base.ROOT / "build" / f"prepared-source-native-{time.time_ns()}-{os.getpid()}").resolve()
    output.mkdir(parents=True, exist_ok=False)

    report = {
        "schema_version": 1,
        "complete": False,
        "head": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=base.ROOT, text=True).strip(),
        "working_diff_sha256": base.digest(subprocess.check_output(["git", "diff", "HEAD", "--binary"], cwd=base.ROOT)),
        "host": platform.platform(),
        "profile": args.profile,
        "package": {"path": str(package), "bytes": package.stat().st_size, "sha256": base.digest(package.read_bytes())},
        "native_test": {"path": str(native_test), "bytes": native_test.stat().st_size, "sha256": base.digest(native_test.read_bytes())},
        "native_source_sha256": runtime.native_source_digest(base.ROOT),
        "runner_sha256": base.digest(Path(__file__).read_bytes()),
        "runtime_runner_sha256": base.digest(Path(runtime.__file__).read_bytes()),
        "workload_helper_sha256": base.digest(Path(base.__file__).read_bytes()),
        "emulator_template": {"path": str(template), "sha256": base.digest(template.read_bytes()), "text": template.read_text()},
        "runner_config_overrides": {"zorro_iii_memory_kib": 65536},
        "limits": {"batch_seconds": 150, "invocation_seconds": 60, "guest_seconds": 35, "poll_ms": 20},
        "cases": [],
        "limitations": [
            "Each native case is one observation, not a statistically stable speed ratio.",
            "START-to-DONE includes input, package, assembly, and output work but excludes emulator boot.",
            "Full invocation time includes harness setup/build and emulator startup/teardown.",
            "This is the existing native source path; no native prepared-source candidate exists.",
        ],
    }
    budget = base.Budget(150)
    started = time.monotonic()
    try:
        for cpu in args.cpus:
            case_dir = output / f"{cpu}-8"
            case_dir.mkdir()
            source, expected = workload(cpu)
            source_path = case_dir / "input.asm"
            source_path.write_text(source)
            row = {
                "cpu": cpu,
                "blocks": 8,
                "source_bytes": len(source.encode()),
                "source_lines": len(source.splitlines()),
                "source_sha256": base.digest(source.encode()),
                "output_bytes": len(expected),
                "output_sha256": base.digest(expected),
            }
            report["cases"].append(row)
            receipt = runtime.native(native_test, source_path, package, budget, case_dir / "native.log", args.profile)
            if bytes(receipt["exact_output"]) != expected:
                raise ValueError(f"{cpu} live native/Rust output differs from independent S1 bytes")
            receipt.pop("exact_output")
            row["native"] = receipt
            (output / "summary.json").write_text(json.dumps(report, indent=2) + "\n")
        report["complete"] = True
    except Exception as error:
        report["error"] = str(error)
    finally:
        report["batch_seconds"] = time.monotonic() - started
        (output / "summary.json").write_text(json.dumps(report, indent=2) + "\n")
    print(output / "summary.json")
    return 0 if report["complete"] else 1


if __name__ == "__main__":
    raise SystemExit(main())

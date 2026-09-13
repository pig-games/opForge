#!/usr/bin/env python3
"""Bounded native comparison: mixed instruction work and focused correctness cases."""
import argparse
import json
import os
import platform
from pathlib import Path
import subprocess
import time

import runtime_comparison as runtime
import vm_efficiency as base

BINARY_TEST = "tests::binary_source_experiment::binary_source_fs_uae"


def replay_smoke(cpu, blocks=8):
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


def binding_switch(cpu, blocks):
    """Alternate pipelines, vary values, and exercise equivalent branch aliases."""
    lines = [".org $1000"]
    expected = bytearray()
    other = "m68000" if cpu == "m6502" else "m6502"
    for index in range(blocks):
        for target in (cpu, other, cpu):
            lines.append(f".cpu {target}")
            value = index + 1
            if target == "m6502":
                lines.extend([f"  lda #{value}", "  sta $2000"])
                expected.extend((0xA9, value, 0x8D, 0, 0x20))
            else:
                label = f"next{len(expected)}"
                alias = "bcc.s" if index % 2 else "bhs.s"
                lines.extend([f"  moveq #{value},d0", f"  {alias} {label}", ".word 0", f"{label}:"])
                expected.extend((0x70, value, 0x64, 2, 0, 0))
    lines.append(".end")
    return "\n".join(lines) + "\n", bytes(expected)


def workload(cpu, blocks, kind):
    if kind == "mixed":
        return base.workload(cpu, blocks)
    if kind == "binding-switch":
        return binding_switch(cpu, blocks)
    return replay_smoke(cpu, blocks)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--native-test", type=Path, required=True)
    parser.add_argument("--package", type=Path, default=base.ROOT / "native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm")
    parser.add_argument("--workload", choices=("mixed", "replay-smoke", "binding-switch"), default="mixed",
                        help="mixed measures selection; binding-switch checks invalidation/aliases; replay-smoke checks S1")
    parser.add_argument("--blocks", type=int, choices=(8, 32),
                        help="defaults to 8; 32 is an explicit larger probe subject to the same timeout")
    parser.add_argument("--profile", choices=("off", "runtime"), default="off")
    parser.add_argument("--binary-source", action="store_true",
                        help="also run the opt-in BSP1 binary-source native harness")
    parser.add_argument("--cpus", nargs="+", choices=("m6502", "m68000"), default=["m6502", "m68000"])
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    blocks = args.blocks if args.blocks is not None else 8
    if args.workload == "replay-smoke" and blocks != 8:
        parser.error("replay-smoke supports exactly 8 blocks")
    if args.binary_source and args.workload == "binding-switch":
        parser.error("binary-source currently supports one CPU pipeline per input")
    if args.binary_source and args.profile != "off":
        parser.error("binary-source timing comparison requires --profile off")

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
        "binary_source": args.binary_source,
        "workload": args.workload,
        "package": {"path": str(package), "bytes": package.stat().st_size, "sha256": base.digest(package.read_bytes())},
        "native_test": {"path": str(native_test), "bytes": native_test.stat().st_size, "sha256": base.digest(native_test.read_bytes())},
        "native_source_sha256": runtime.native_source_digest(base.ROOT),
        "runner_sha256": base.digest(Path(__file__).read_bytes()),
        "runtime_runner_sha256": base.digest(Path(runtime.__file__).read_bytes()),
        "workload_helper_sha256": base.digest(Path(base.__file__).read_bytes()),
        "emulator_template": {"path": str(template), "sha256": base.digest(template.read_bytes()), "text": template.read_text()},
        "runner_config_overrides": {"zorro_iii_memory_kib": 65536},
        "limits": {
            "batch_seconds": 150,
            "invocation_seconds": 60,
            "text_guest_seconds": 35,
            "binary_boot_seconds": 60,
            "binary_post_start_seconds": 10,
            "poll_ms": 20,
        },
        "cases": [],
        "limitations": [
            "Each native case is one observation, not a statistically stable speed ratio.",
            "START-to-DONE includes input, package, assembly, and output work but excludes emulator boot.",
            "Full invocation time includes harness setup/build and emulator startup/teardown.",
            "The native field runs source-text processing, including any current numeric package bindings.",
            "The opt-in binary_source field combines native source packing with a Rust-derived single-pipeline BSP1 runtime capsule; it does not isolate tokenization speedup or package preparation cost.",
        ],
    }
    budget = base.Budget(150)
    started = time.monotonic()
    try:
        for cpu in args.cpus:
            case_dir = output / f"{cpu}-{blocks}"
            case_dir.mkdir()
            source, expected = workload(cpu, blocks, args.workload)
            source_path = case_dir / "input.asm"
            source_path.write_text(source)
            row = {
                "cpu": cpu,
                "blocks": blocks,
                "source_bytes": len(source.encode()),
                "source_lines": len(source.splitlines()),
                "source_sha256": base.digest(source.encode()),
                "output_bytes": len(expected),
                "output_sha256": base.digest(expected),
            }
            report["cases"].append(row)
            try:
                receipt = runtime.native(native_test, source_path, package, budget, case_dir / "native.log", args.profile)
                if bytes(receipt["exact_output"]) != expected:
                    raise ValueError(f"{cpu} live native/Rust output differs from independent workload bytes")
                receipt.pop("exact_output")
                row["native"] = receipt
            except Exception as error:
                # A failed case is evidence only for itself; attempt later cases
                # under the same batch deadline and keep the batch fail-closed.
                row["error"] = str(error)
            if args.binary_source:
                try:
                    receipt = runtime.native(
                        native_test, source_path, package, budget,
                        case_dir / "binary-source.log", "off",
                        test=BINARY_TEST,
                        result_prefix="BINARY_SOURCE_COMPARISON ",
                        extra_env={"OPFORGE_COMPARE_CPU": cpu},
                        guest_timeout_ms=60000,
                        post_start_timeout_ms=10000,
                    )
                    if bytes(receipt["exact_output"]) != expected:
                        raise ValueError(
                            f"{cpu} binary-source output differs from independent workload bytes"
                        )
                    receipt.pop("exact_output")
                    row["binary_source"] = receipt
                except Exception as error:
                    row["binary_source_error"] = str(error)
            (output / "summary.json").write_text(json.dumps(report, indent=2) + "\n")
        report["complete"] = all(
            "native" in row and (not args.binary_source or "binary_source" in row)
            for row in report["cases"]
        )
    except Exception as error:
        report["error"] = str(error)
    finally:
        report["batch_seconds"] = time.monotonic() - started
        (output / "summary.json").write_text(json.dumps(report, indent=2) + "\n")
    print(output / "summary.json")
    return 0 if report["complete"] else 1


if __name__ == "__main__":
    raise SystemExit(main())

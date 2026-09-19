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
MEMORY_PROFILE_ENV = "OPFORGE_FS_UAE_MEMORY_PROFILE"
CONSTRAINED_2M_SETTINGS = {
    "cpu": "68020",
    "chip_memory": "2048",
    "slow_memory": "0",
    "fast_memory": "0",
    "motherboard_ram": "0",
    "zorro_iii_memory": "0",
    "graphics_card": "none",
    "graphics_memory": "0",
    "graphics_card_memory": "0",
}


def effective_emulator_config(template_text, memory_profile):
    """Mirror the Rust runner's generated config using a stable mount placeholder."""
    settings = ({"zorro_iii_memory": "65536"} if memory_profile == "existing"
                else CONSTRAINED_2M_SETTINGS)
    seen = set()
    lines = []
    replaced_mount = False
    for line in template_text.splitlines():
        key = line.lstrip().partition("=")[0].strip() if "=" in line else None
        if key == "hard_drive_1":
            lines.append("hard_drive_1 = {ephemeral_work_mount}")
            replaced_mount = True
        elif key in settings:
            lines.append(f"{key} = {settings[key]}")
            seen.add(key)
        else:
            lines.append(line)
    if not replaced_mount:
        lines.append("hard_drive_1 = {ephemeral_work_mount}")
    for key, value in settings.items():
        if key not in seen:
            lines.append(f"{key} = {value}")
    return "\n".join(lines) + "\n"


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


def expression_workload(cpu, blocks, layout=False):
    """Bounded expression-heavy source with an independent byte contract.

    The expressions deliberately stay scalar and use only labels, the current
    address, unary/additive arithmetic, grouping, and (for ``layout``)
    multiplication.  This mirrors the real binary frontend's layout arithmetic
    without depending on modules, macros, or equates.
    """
    if cpu not in ("m6502", "m68000") or blocks not in (8, 32):
        raise ValueError("expression workloads support 8 or 32 m6502/m68000 blocks")
    lines = [f".cpu {cpu}", ".org $1000"]
    expected = bytearray()
    for index in range(blocks):
        lines.append(f"expr_start{index}:")
        if cpu == "m6502":
            lines.extend([
                "  lda #(-(-(5 * 3 - 2)))" if layout
                else f"  lda #(-(-(({index} + 3) - 1)))",
                "  ldx #(($ - $) + 6)",
                f"  sta $2000 + ({index} - {index})",
                f"  bne expr_end{index}",
                "  .byte (($ - $) + 25)" if layout
                else "  .byte (($ - $) + 12)",
                "  .byte (($ - $) + 1)",
                f"  .word (expr_end{index} - expr_start{index}) * 2 + $4000" if layout
                else f"  .word (expr_end{index} - expr_start{index}) + $2000",
                f"expr_end{index}:",
                "  nop",
            ])
            distance = 13
            first = 25 if layout else 12
            lda_value = 13 if layout else index + 2
            expected.extend((0xA9, lda_value, 0xA2, 6,
                             0x8D, 0x00, 0x20, 0xD0, 4, first,
                             1))
            expected.extend((distance * 2 + 0x4000 if layout else distance + 0x2000).to_bytes(2, "little"))
            expected.append(0xEA)
        else:
            lines.extend([
                "  moveq #(-(-(3 * 2 - 1))),d0" if layout
                else f"  moveq #(-(-(({index} + 3) - 1))),d0",
                "  move.w #(($ - $) + 6),d1",
                f"  move.b d0,($2000 + ({index} - {index})).w",
                f"  bne.s expr_end{index}",
                "  .byte (($ - $) + 25)" if layout
                else "  .byte (($ - $) + 12)",
                "  .byte (($ - $) + 1)",
                f"  .word (expr_end{index} - expr_start{index}) * 2 + $4000" if layout
                else f"  .word (expr_end{index} - expr_start{index}) + $2000",
                f"expr_end{index}:",
                "  nop",
            ])
            distance = 16
            first = 25 if layout else 12
            moveq_value = 5 if layout else index + 2
            expected.extend((0x70, moveq_value, 0x32, 0x3C,
                             0, 6,
                             0x11, 0xC0, 0x20, 0x00, 0x66, 4,
                             first, 1))
            expected.extend((distance * 2 + 0x4000 if layout else distance + 0x2000).to_bytes(2, "big"))
            expected.extend((0x4E, 0x71))
    lines.append(".end")
    return "\n".join(lines) + "\n", bytes(expected)


def workload(cpu, blocks, kind):
    if kind == "mixed":
        return base.workload(cpu, blocks)
    if kind == "binding-switch":
        return binding_switch(cpu, blocks)
    if kind == "expression-replay":
        return expression_workload(cpu, blocks)
    if kind == "expression-layout":
        return expression_workload(cpu, blocks, layout=True)
    return replay_smoke(cpu, blocks)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--native-test", type=Path, required=True)
    parser.add_argument("--native-source-root", type=Path, default=base.ROOT,
                        help="native source snapshot used by both live runners (default: repository root)")
    parser.add_argument("--package", type=Path, default=base.ROOT / "native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm")
    parser.add_argument("--workload", choices=("mixed", "replay-smoke", "binding-switch",
                                                "expression-replay", "expression-layout"), default="mixed",
                        help="mixed measures selection; expression workloads exercise bounded label/current-PC arithmetic")
    parser.add_argument("--blocks", type=int, choices=(8, 32),
                        help="defaults to 8; 32 is an explicit larger probe subject to the same timeout")
    parser.add_argument("--profile", choices=("off", "runtime"), default="off")
    parser.add_argument("--binary-source", action="store_true",
                        help="also run the opt-in BSP2 binary-source native harness")
    parser.add_argument("--binary-only", action="store_true",
                        help="run only the binary-source harness; requires --binary-source")
    parser.add_argument("--memory-profile", choices=("existing", "2m"),
                        default=os.environ.get(MEMORY_PROFILE_ENV, "existing"),
                        help="FS-UAE guest RAM profile; 2m selects 68020 with 2 MiB total RAM")
    parser.add_argument("--compare-memory", action="store_true",
                        help="enable binary-harness memory telemetry (requires --binary-source)")
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
    if args.compare_memory and not args.binary_source:
        parser.error("--compare-memory requires --binary-source")
    if args.binary_only and not args.binary_source:
        parser.error("--binary-only requires --binary-source")

    native_test = args.native_test.resolve(strict=True)
    native_source_root = args.native_source_root.resolve(strict=True)
    package = args.package.resolve(strict=True)
    template = Path(os.environ["OPFORGE_FS_UAE_CONFIG_TEMPLATE"]).resolve(strict=True)
    os.environ[MEMORY_PROFILE_ENV] = args.memory_profile
    template_text = template.read_text()
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
        "binary_only": args.binary_only,
        "compare_memory": args.compare_memory,
        "workload": args.workload,
        "package": {"path": str(package), "bytes": package.stat().st_size, "sha256": base.digest(package.read_bytes())},
        "native_test": {"path": str(native_test), "bytes": native_test.stat().st_size, "sha256": base.digest(native_test.read_bytes())},
        "native_source_root": str(native_source_root),
        "native_source_sha256": runtime.native_source_digest(native_source_root),
        "native_source_commit": subprocess.run(
            ["git", "-C", str(native_source_root), "rev-parse", "HEAD"],
            capture_output=True, text=True, check=False).stdout.strip() or None,
        "runner_sha256": base.digest(Path(__file__).read_bytes()),
        "runtime_runner_sha256": base.digest(Path(runtime.__file__).read_bytes()),
        "workload_helper_sha256": base.digest(Path(base.__file__).read_bytes()),
        "emulator_template": {"path": str(template), "sha256": base.digest(template.read_bytes()), "text": template_text},
        "emulator_effective_config": {
            "memory_profile": args.memory_profile,
            "environment": {MEMORY_PROFILE_ENV: args.memory_profile},
            "text": effective_emulator_config(template_text, args.memory_profile),
        },
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
            "The opt-in binary_source field combines native source packing with a Rust-derived single-pipeline BSP2 runtime capsule; it does not isolate tokenization speedup or package preparation cost.",
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
            if not args.binary_only:
                try:
                    receipt = runtime.native(native_test, source_path, package, budget, case_dir / "native.log", args.profile,
                                             native_source_root)
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
                        native_source_root,
                        test=BINARY_TEST,
                        result_prefix="BINARY_SOURCE_COMPARISON ",
                        extra_env={
                            "OPFORGE_COMPARE_CPU": cpu,
                            "OPFORGE_COMPARE_MEMORY": "1" if args.compare_memory else "0",
                        },
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
            (args.binary_only or "native" in row)
            and (not args.binary_source or "binary_source" in row)
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

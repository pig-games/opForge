#!/usr/bin/env python3
"""Deterministic real-CLI performance corpus and fail-closed result ledger.

No assembler implementation selects behavior by benchmark name. The corpus
only supplies ordinary source files, package bytes and public CLI arguments.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
from pathlib import Path, PurePosixPath
import platform
import re
import statistics
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
SCHEMA = 1
REVISION = "native-bridge-corpus-v1"
SOURCE = "opforge_6502_native_cli_smoke.asm"
PACKAGE = "native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm"


def digest(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def canonical(value: object) -> bytes:
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode()


def relative_path(value: str) -> str:
    path = PurePosixPath(value)
    if not value or path.is_absolute() or any(p in ("", ".", "..") for p in value.split("/")) or ":" in value or "\\" in value:
        raise ValueError(f"unsafe corpus path: {value!r}")
    return value


def corpus() -> list[dict]:
    cases = []

    def add(case_id, mechanism, source, *, files=None, outputs=None, reuse=None, coverage=None):
        inputs = {SOURCE: source, **(files or {})}
        case = {
            "id": case_id, "mechanism": mechanism, "cpu": "m6502",
            "files": inputs, "source": SOURCE,
            "argv": [SOURCE, "--cpu", "m6502", "--bin", "output.bin"],
            "outputs": outputs or ["output.bin"], "expected_exit": 0,
            "expected_diagnostics": [], "reuse": reuse or [],
            "coverage": coverage or [mechanism], "native_timeout_seconds": 120,
        }
        case["source_bytes"] = sum(len(text.encode()) for text in inputs.values())
        case["source_lines"] = sum(len(text.splitlines()) for text in inputs.values())
        case["sha256"] = digest(canonical(case))
        cases.append(case)

    add("B01", "fixed startup", ".org $0800\n" + "        nop\n" * 9,
        reuse=["native CLI directive-router fixture instruction/data path"])
    # 2048 * 512 bytes: line length remains below the native 512-byte buffer.
    add("B02", "source I/O", (";" + " " * 510 + "\n") * 2048 + ".byte $42\n")
    add("B03", "statement throughput", ".org $0800\n" + "        nop\n" * 256)
    labels = "".join(f"label{i:03d} .byte {i % 256}\n" for i in range(128))
    uses = "".join(f"        .word label{i:03d}\n" for i in range(128))
    add("B04", "symbol lookup", ".org $0800\n" + labels + uses)
    branches = "".join(f"branch{i:03d} bne target{i:03d}\n        .byte $42\ntarget{i:03d} nop\n" for i in range(64))
    stability = (ROOT / "examples/mos6502/mos_forward_ref_stability.asm").read_text()
    stability = stability.replace("        .end", "        .cpu 6502\n        .org $0800\n" + branches + "        .end")
    add("B05", "forward layout", stability,
        reuse=["examples/mos6502/mos_forward_ref_stability.asm", "examples/ab/mos6502/64tass/m6502/positive/m6502_branch_jump_core.asm"])
    expressions = "".join(f"        .word (BASE + {i}) * 3\n" for i in range(128))
    add("B06", "expression lifecycle", ".org $0800\nBASE .const $100\n" + expressions,
        reuse=["examples/opcore/expr_syntax.asm"])
    flow = ".for 4\n.if 1\n.match 2\n.case 1\n.byte $11\n.case 2\n.byte $42\n.default\n.byte $99\n.endmatch\n.else\n.byte 0\n.endif\n.endfor\n"
    add("B07", "nested flow", ".org $0800\n" + flow * 8,
        reuse=["examples/opcore/for_counter_basic.asm", "examples/opcore/cond_syntax.asm"])
    module_files = {
        "modules/math.asm": ".module math\n.use helper\n.pub\nVALUE .const 7\n.priv\n.endmodule\n",
        "modules/helper.asm": ".module helper\n.endmodule\n",
        "includes/outer.inc": '.byte $22\n',
        "includes/inner.inc": ".byte $11\n",
    }
    module_source = '.module main\n.use math\n.org $0800\n.include "inner.inc"\n.include "outer.inc"\n.byte math.VALUE\n.endmodule\n'
    add("B08", "module include graph", module_source, files=module_files,
        reuse=["examples/opcore/module_use_autoload.asm", "native CLI Item 10 include-path fixtures"],
        coverage=["transitive module use", "two sibling includes", "include path", "module path", "public constant"])
    cases[-1]["argv"] += ["-M", "modules", "-I", "includes"]
    # Independent byte contracts for the small generated mechanisms. These are
    # fixture semantics, never implementation shortcuts or native oracle lookup.
    words = lambda values: b"".join(value.to_bytes(2, "little") for value in values)
    expected_bins = [bytes([0xea]) * 9, bytes([0x42]), bytes([0xea]) * 256,
                     bytes(range(128)) + words(range(0x800, 0x880)),
                     bytes([0xad, 1, 1, 0xea, 0x60]) + bytes(251)
                     + bytes([0x9c, 1, 2, 0xea, 0x60]) + bytes(0x800 - 0x202)
                     + bytes([0xd0, 1, 0x42, 0xea]) * 64,
                     words((0x100 + i) * 3 for i in range(128)), bytes([0x42]) * 32,
                     bytes([0x11, 0x22, 7])]
    for case, expected in zip(cases, expected_bins):
        case["expected_artifacts"] = {"output.bin": {"bytes": len(expected), "sha256": digest(expected)}}

    artifact_source = (ROOT / "examples/opcore/linker_regions_full.asm").read_text()
    # Hunk supports a longword relocation, not this fixture's 16-bit pointer.
    artifact_source = artifact_source.replace(".word data_start", ".long data_start")
    artifact_source = artifact_source.replace(".endmodule", '.output "build/full.hunk", format=hunk, sections=code,data,zero\n.meta\n.output\n.list\n.hex "build/full"\n.endoutput\n.endmeta\n.endmodule')
    artifacts = ["output.bin", "build/full.prg", "build/full-image.bin", "build/full.map",
                 "build/full_sections/code.bin", "build/full_sections/data.bin", "build/full_sections/zero.bin",
                 "build/full.srec", "build/full.hunk", "build/full.hex", SOURCE.removesuffix(".asm") + ".lst"]
    add("B09", "all outputs", artifact_source, outputs=artifacts,
        reuse=["examples/opcore/linker_regions_full.asm", "examples/opcore/module_metadata_outputs.asm"],
        coverage=["BIN", "PRG", "Hunk", "S-record", "HEX", "listing", "map", "metadata", "exported sections"])
    cases[-1]["argv"] += ["--srec", "build/full.srec"]
    integrated = ('.module main\n.use math\n.region rom, $0800, $1fff\n.section code\n.include "inner.inc"\n.include "outer.inc"\nBASE .const $100\n'
                  + "".join(f"entry{i:03d} lda #math.VALUE\n.word (BASE + {i}) * 3\n        bne done{i:03d}\n.byte $99\ndone{i:03d} nop\n" for i in range(256))
                  + flow * 4 + '.endsection\n.pack in rom : code\n.output "build/integrated.prg", format=prg, sections=code\n.mapfile "build/integrated.map", symbols=all\n.endmodule\n')
    add("B10", "integrated production", integrated, files=module_files,
        outputs=["output.bin", "build/integrated.prg", "build/integrated.map"],
        reuse=["B04-B08 mechanisms composed through one real CLI invocation"],
        coverage=["module", "include", "symbols", "expressions", "nested flow", "forward layout", "emission", "BIN", "PRG", "map"])
    cases[-1]["argv"] += ["-M", "modules", "-I", "includes"]
    # Include the final public command in each digest (module flags were added above).
    for case in cases:
        case.pop("sha256")
        case["sha256"] = digest(canonical(case))
    return cases


def manifest() -> dict:
    cases = corpus()
    for case in cases:
        case["files"] = {name: {"bytes": len(source.encode()), "sha256": digest(source.encode())}
                         for name, source in case["files"].items()}
    result = {"schema_version": SCHEMA, "revision": REVISION, "cases": cases,
              "package": {"path": PACKAGE, "sha256": digest((ROOT / PACKAGE).read_bytes()),
                          "bytes": (ROOT / PACKAGE).stat().st_size},
              "policy": {"retained_runs": 7, "warmups": 1,
                         "cache": "fresh process, warm host filesystem after one unmeasured run; no cache flush",
                         "native_envelope_seconds": 120,
                         "physical_a6000_envelope": "target <=120 seconds per integrated run; not hardware-validated"}}
    result["sha256"] = digest(canonical(result))
    return result


def artifact_set(directory: Path, expected: list[str]) -> dict:
    directory = directory.resolve(strict=True)
    result = {}
    for name in expected:
        path = directory / relative_path(name)
        components = [directory.joinpath(*PurePosixPath(name).parts[:index])
                      for index in range(1, len(PurePosixPath(name).parts) + 1)]
        if not path.is_file() or any(part.is_symlink() for part in components):
            raise ValueError(f"missing/invalid required artifact {name}")
        data = path.read_bytes()
        result[name] = {"bytes": len(data), "sha256": digest(data)}
    return result


def summarize(samples: list[int]) -> dict:
    if not samples or any(type(n) is not int or n <= 0 for n in samples):
        raise ValueError("elapsed samples must be nonempty positive integer nanoseconds")
    ordered = sorted(samples)
    return {"runs": len(samples), "median_ns": statistics.median(samples),
            "minimum_ns": ordered[0], "maximum_ns": ordered[-1],
            "p95_ns": ordered[math.ceil(.95 * len(ordered)) - 1]}


def run_rust_case(binary: Path, case: dict, runs: int, *, live_oracle: bool = False) -> dict:
    if type(runs) is not int or runs < 1:
        raise ValueError("runs must be positive")
    samples, reference = [], None
    # Every invocation gets fresh outputs. Equal artifacts are required, not
    # inferred from exit status or accepted from an earlier invocation.
    for index in range(runs + 1):
        with tempfile.TemporaryDirectory(prefix="opforge-corpus-") as tmp:
            directory = Path(tmp)
            for name, source in case["files"].items():
                path = directory / relative_path(name)
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(source.encode())
            for name in case["outputs"]:
                (directory / relative_path(name)).parent.mkdir(parents=True, exist_ok=True)
            # Resolve host paths once so recursive module discovery sees one
            # identity, not both './modules/x.asm' and 'modules/x.asm'.
            args = list(case["argv"])
            args[0] = str(directory / case["source"])
            for i, value in enumerate(args[:-1]):
                if value in ("--bin", "--srec", "-M", "-I"):
                    args[i + 1] = str(directory / args[i + 1])
            argv = [str(binary), *args, "--opasm-package", str(ROOT / PACKAGE)]
            start = time.perf_counter_ns()
            result = subprocess.run(argv, cwd=directory, capture_output=True, timeout=case["native_timeout_seconds"])
            elapsed = time.perf_counter_ns() - start
            if result.returncode != case["expected_exit"] or result.stderr or result.stdout:
                raise ValueError(f'{case["id"]}: CLI failed/diagnostics changed: {result.returncode}\n{result.stderr.decode(errors="replace")}')
            actual = artifact_set(directory, case["outputs"])
            for name, expected in case.get("expected_artifacts", {}).items():
                if actual[name] != expected:
                    raise ValueError(f'{case["id"]}: artifact violates independent fixture semantics: {name}')
            if live_oracle:
                oracle_bytes = {name: list((directory / name).read_bytes()) for name in case["outputs"]}
            if reference is None:
                reference = actual
            elif actual != reference:
                raise ValueError(f'{case["id"]}: nondeterministic artifact bytes')
            if index:
                samples.append(elapsed)
    result = {"id": case["id"], "case_sha256": case["sha256"], "samples_ns": samples,
            "argv": [*case["argv"], "--opasm-package", PACKAGE],
            "statistics": summarize(samples), "artifacts": reference,
            "exit_status": 0, "diagnostics": [], "complete": True}
    if live_oracle:
        result["oracle_bytes"] = oracle_bytes
    return result


def validate_result(result: dict, frozen: dict) -> None:
    if not isinstance(result, dict):
        raise ValueError("result must be an object")
    if type(result.get("schema_version")) is not int or result["schema_version"] != SCHEMA or result.get("corpus_sha256") != frozen["sha256"]:
        raise ValueError("unknown result schema or mismatched corpus")
    cases = {case["id"]: case for case in frozen["cases"]}
    selected = result.get("selected_cases")
    if not isinstance(selected, list) or not selected or any(type(name) is not str or name not in cases for name in selected) or len(selected) != len(set(selected)):
        raise ValueError("missing/invalid declared case selection")
    policy = result.get("policy")
    if not isinstance(policy, dict) or type(policy.get("retained_runs")) is not int or policy["retained_runs"] < 1:
        raise ValueError("invalid retained run policy")
    expected_policy = {**frozen["policy"], "retained_runs": policy["retained_runs"]}
    if policy != expected_policy:
        raise ValueError("unknown measurement policy")
    eligible = policy["retained_runs"] >= frozen["policy"]["retained_runs"] and set(selected) == set(cases)
    if result.get("comparison_eligible") is not eligible:
        raise ValueError("incorrect baseline comparison eligibility")
    if result.get("backend") != "rust-release-unprofiled" or result.get("package") != frozen["package"]:
        raise ValueError("missing/unknown backend or package provenance")
    if not valid_digest(result.get("binary_sha256")) or type(result.get("binary_bytes")) is not int or result["binary_bytes"] <= 0:
        raise ValueError("invalid binary provenance")
    if not isinstance(result.get("head"), str) or not re.fullmatch(r"[0-9a-f]{40}", result["head"]):
        raise ValueError("invalid commit provenance")
    for field in ("host", "rustc", "cargo", "generator_sha256"):
        if not isinstance(result.get(field), str) or not result[field]:
            raise ValueError(f"missing {field} provenance")
    if not valid_digest(result["generator_sha256"]):
        raise ValueError("invalid generator digest")
    build = result.get("build")
    if not isinstance(build, dict) or build.get("command") != BUILD_COMMAND or build.get("profile") != "release" or build.get("default_features") is not True or not valid_digest(build.get("cargo_lock_sha256")) or not isinstance(build.get("environment"), dict):
        raise ValueError("missing/invalid verified release build receipt")
    if not isinstance(build.get("artifact_profile"), dict) or build["artifact_profile"].get("test") is not False or not isinstance(build.get("executable"), str) or not Path(build["executable"]).is_absolute():
        raise ValueError("missing Cargo-selected executable/profile provenance")
    if not isinstance(result.get("cases"), list):
        raise ValueError("missing result cases")
    seen = set()
    for row in result["cases"]:
        if not isinstance(row, dict):
            raise ValueError("result case must be an object")
        name = row.get("id")
        if type(name) is not str or name not in cases or name in seen:
            raise ValueError("unknown or duplicate result case")
        seen.add(name)
        case = cases[name]
        if row.get("case_sha256") != case["sha256"] or row.get("complete") is not True or type(row.get("exit_status")) is not int or row["exit_status"] != 0 or row.get("diagnostics") != []:
            raise ValueError("incomplete/mismatched result evidence")
        if row.get("argv") != [*case["argv"], "--opasm-package", PACKAGE]:
            raise ValueError("result command does not match the frozen public CLI command")
        if not isinstance(row.get("samples_ns"), list) or len(row["samples_ns"]) != policy["retained_runs"]:
            raise ValueError("retained sample count differs from declared policy")
        expected_statistics = summarize(row["samples_ns"])
        if row.get("statistics") != expected_statistics or any(type(row["statistics"][key]) is not type(value) for key, value in expected_statistics.items()):
            raise ValueError("result statistics do not match retained samples")
        if not isinstance(row.get("artifacts"), dict) or set(row["artifacts"]) != set(case["outputs"]):
            raise ValueError("missing or unexpected artifact result")
        for artifact in row["artifacts"].values():
            if not isinstance(artifact, dict) or type(artifact.get("bytes")) is not int or artifact["bytes"] < 0 or not valid_digest(artifact.get("sha256")):
                raise ValueError("malformed artifact result")
        for name, expected in case.get("expected_artifacts", {}).items():
            if row["artifacts"][name] != expected:
                raise ValueError("artifact result violates independent fixture semantics")
    if seen != set(selected):
        raise ValueError("result cases do not match the declared selection")


def valid_digest(value: object) -> bool:
    return isinstance(value, str) and re.fullmatch(r"[0-9a-f]{64}", value) is not None


def read_json(path: Path) -> dict:
    def unique_pairs(pairs):
        result = {}
        for key, value in pairs:
            if key in result:
                raise ValueError(f"duplicate JSON key: {key}")
            result[key] = value
        return result

    def reject_constant(value):
        raise ValueError(f"non-finite JSON number: {value}")

    return json.loads(path.read_text(), object_pairs_hook=unique_pairs, parse_constant=reject_constant)


BUILD_COMMAND = ["cargo", "build", "--release", "--locked", "-p", "cli", "--bin", "opforge", "--message-format=json-render-diagnostics"]


def build_release() -> tuple[Path, dict]:
    # Never label an arbitrary supplied executable as a release measurement.
    environment = {key: value for key, value in os.environ.items()
                   if key.startswith(("CARGO_PROFILE_", "CARGO_TARGET_", "CARGO_BUILD_", "RUSTFLAGS", "CARGO_ENCODED_RUSTFLAGS"))}
    completed = subprocess.run(BUILD_COMMAND, cwd=ROOT, capture_output=True, text=True, timeout=600)
    if completed.returncode:
        raise ValueError(f"release build failed: {completed.stderr}")
    artifacts = [row for line in completed.stdout.splitlines() if (row := json.loads(line)).get("reason") == "compiler-artifact"
                 and row.get("target", {}).get("name") == "opforge" and "bin" in row.get("target", {}).get("kind", []) and row.get("executable")]
    if len(artifacts) != 1 or artifacts[0]["profile"]["test"]:
        raise ValueError("Cargo did not identify exactly one non-test opforge executable")
    binary = Path(artifacts[0]["executable"]).resolve(strict=True)
    return binary, {
        "command": BUILD_COMMAND, "profile": "release", "default_features": True,
        "executable": str(binary), "artifact_profile": artifacts[0]["profile"],
        "cargo_lock_sha256": digest((ROOT / "Cargo.lock").read_bytes()), "environment": environment,
    }


def emulator_config(template: str) -> str:
    # Explicit speed avoids the different <=68020 and >=68030 defaults. Max
    # speed is a host-dependent emulator probe, never an A6000 calibration.
    overrides = {"cpu": "68020", "uae_cpu_speed": "max", "jit_compiler": "0"}
    sections = [line.strip().lower() for line in template.splitlines() if line.strip().startswith("[")]
    if sections != ["[fs-uae]"]:
        raise ValueError("FS-UAE template must contain exactly one unambiguous section")
    lines = [line for line in template.splitlines()
             if line.split("=", 1)[0].strip() not in overrides]
    if any(line.split("=", 1)[0].strip().lower().replace("-", "_") == "uae_cpu_model" for line in lines):
        raise ValueError("template uae_cpu_model would override the pinned CPU")
    return "\n".join([*lines, *(f"{key} = {value}" for key, value in overrides.items())]) + "\n"


def validate_diagnostic_capture(row: dict, case: dict, frozen: dict, visits: int) -> None:
    """A controlled incomplete capture is never a successful corpus result."""
    if not isinstance(row, dict):
        raise ValueError("diagnostic capture must be an object")
    if (row.get("proof_level") != "E" or row.get("complete") is not False
            or row.get("parity_passed") is not False or row.get("protocol_completed") is not True
            or type(row.get("exit_status")) is not int or row["exit_status"] == 0
            or row.get("id") != case["id"] or row.get("case_sha256") != case["sha256"]
            or row.get("corpus_sha256") != frozen["sha256"]
            or row.get("package_sha256") != frozen["package"]["sha256"]):
        raise ValueError("invalid incomplete diagnostic identity/protocol")
    profile = row.get("profile", {})
    if (not isinstance(profile, dict) or profile.get("state") != "incomplete" or profile.get("abort_requested") is not True
            or profile.get("abort_after_visits") != visits or profile.get("statement_visits") != visits
            or type(profile.get("abort_after_visits")) is not int or type(profile.get("statement_visits")) is not int
            or profile.get("exit_status") != row["exit_status"]):
        raise ValueError("capture is not the requested controlled visit abort")
    for group in [profile, *(profile.get(name, {}) for name in
                            ("work_multiplication", "symbol_expression_work", "runtime_execution", "platform_io"))]:
        if not isinstance(group, dict) or type(group.get("overflow_bits")) is not int or group["overflow_bits"] != 0:
            raise ValueError("missing or overflowing diagnostic counter group")


def diagnose(case_id: str, visits: int, frozen: dict, sample_after: int | None = None,
             control_mode: str | None = None, binding_register: str | None = None,
             resample_after: int | None = None, profile_mode: str = "all") -> dict:
    if profile_mode not in ("all", "all-no-io"):
        raise ValueError("diagnostic profile must be all or all-no-io")
    if type(visits) is not int or not 1 <= visits <= 100_000:
        raise ValueError("--abort-visits must be in 1..100000")
    if sample_after is not None and (type(sample_after) is not int or not 1 <= sample_after <= 100):
        raise ValueError("--sample-after-seconds must be in 1..100")
    if resample_after is not None and (sample_after is None or type(resample_after) is not int
                                       or not sample_after + 5 <= resample_after <= 100):
        raise ValueError("resample requires a sample at least 5 seconds earlier and a delay no greater than 100")
    if control_mode is not None and (control_mode not in ("app", "pty", "console") or sample_after is not None):
        raise ValueError("observer control must be app/pty/console, without a sample delay")
    if binding_register is not None and (binding_register not in [f"{kind}{i}" for kind in "da" for i in range(8)]
                                         or sample_after is None):
        raise ValueError("binding register requires a live sample and one d0-d7/a0-a7 register")
    if sample_after is not None and os.environ.get("OPFORGE_FS_UAE_CONSOLE_DEBUGGER_AUTOMATE") != "1":
        raise ValueError("live sampling requires explicit OPFORGE_FS_UAE_CONSOLE_DEBUGGER_AUTOMATE=1")
    case = next((row for row in frozen["cases"] if row["id"] == case_id), None)
    if case is None:
        raise ValueError("diagnosis requires one known frozen case")
    stored = read_json(ROOT / "documentation/performance/results/opforge-corpus-v1-manifest.json")
    if frozen != stored:
        raise ValueError("diagnosis inputs differ from the frozen corpus")
    env = {**os.environ, "OPFORGE_PERFORMANCE_CORPUS": "1",
           "OPFORGE_NATIVE_CORPUS_DIAGNOSTIC": "1", "OPFORGE_NATIVE_CORPUS_CASES": case_id,
           "OPFORGE_NATIVE_CORPUS_PROFILE": profile_mode, "OPFORGE_NATIVE_CORPUS_ABORT_VISITS": str(visits),
           "OPFORGE_FS_UAE_POST_START_TIMEOUT_MS": "120000", "RUST_TEST_THREADS": "1"}
    env.pop("OPFORGE_NATIVE_CORPUS_LIVE_CAPTURE", None)
    env.pop("OPFORGE_NATIVE_CORPUS_SAMPLE_AFTER_SECONDS", None)
    env.pop("OPFORGE_NATIVE_CORPUS_CONTROL_MODE", None)
    env.pop("OPFORGE_NATIVE_CORPUS_BINDING_REGISTER", None)
    env.pop("OPFORGE_NATIVE_CORPUS_RESAMPLE_AFTER_SECONDS", None)
    if resample_after is not None:
        env["OPFORGE_NATIVE_CORPUS_RESAMPLE_AFTER_SECONDS"] = str(resample_after)
    if binding_register is not None:
        env["OPFORGE_NATIVE_CORPUS_BINDING_REGISTER"] = binding_register
    if sample_after is not None:
        env.update(OPFORGE_NATIVE_CORPUS_LIVE_CAPTURE="1", OPFORGE_NATIVE_CORPUS_SAMPLE_AFTER_SECONDS=str(sample_after))
    if control_mode is not None:
        env.update(OPFORGE_NATIVE_CORPUS_LIVE_CAPTURE="1", OPFORGE_NATIVE_CORPUS_SAMPLE_AFTER_SECONDS="0",
                   OPFORGE_NATIVE_CORPUS_CONTROL_MODE=control_mode)
    command = ["cargo", "test", "--locked", "-p", "asm",
               "external_fs_uae_native_production_corpus_diagnostic", "--", "--nocapture", "--test-threads=1"]
    # The existing Rust coordinator owns bounded boot/post-start waits and
    # cleanup, including failure. Do not kill its parent and orphan its guest.
    result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True, text=True)
    transcript = result.stdout + result.stderr
    rows = [json.loads(line.removeprefix("CORPUS_DIAGNOSTIC ")) for line in transcript.splitlines()
            if line.startswith("CORPUS_DIAGNOSTIC ")]
    # Error reporting can repeat a launcher transcript. Preserve one identical
    # observation, never promote it into protocol or parity success.
    live_lines = set(line.removeprefix("CORPUS_LIVE_CAPTURE ") for line in transcript.splitlines()
                     if line.startswith("CORPUS_LIVE_CAPTURE "))
    live_rows = [json.loads(line) for line in live_lines]
    valid = result.returncode == 0 and len(rows) == 1 and len(transcript) <= 1_048_576
    reason = None
    if valid:
        try:
            validate_diagnostic_capture(rows[0], case, frozen, visits)
            if rows[0]["profile"]["platform_io"].get("enabled_groups") != {"io": profile_mode == "all", "bulk": True}:
                raise ValueError("diagnostic counter groups do not match the requested profile mode")
        except ValueError as error:
            valid, reason = False, str(error)
    return {"schema_version": 1, "mode": "native-controlled-abort-diagnostic", "proof_level": "E",
            "capture_ok": valid, "complete": False, "parity_passed": False,
            "comparison_eligible": False, "case_id": case_id, "case_sha256": case["sha256"],
            "corpus_sha256": frozen["sha256"], "abort_visits": visits, "command": command,
            "head": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip(),
            "generator_sha256": digest(Path(__file__).read_bytes()), "host": platform.platform(),
            "test_exit": result.returncode, "validation_error": reason,
            "capture": rows[0] if len(rows) == 1 else None,
            "sample_after_seconds": sample_after,
            "profile_mode": profile_mode,
            "resample_after_seconds": resample_after,
            "control_mode": control_mode,
            "binding_register": binding_register,
            "live_sample": live_rows[0] if (sample_after is not None or control_mode is not None) and len(live_rows) == 1 else None,
            "transcript": transcript[:1_048_576], "transcript_truncated": len(transcript) > 1_048_576}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=["manifest", "rust", "validate", "native-input", "fs-uae-config", "diagnose"])
    parser.add_argument("--runs", type=int, default=7)
    parser.add_argument("--case", action="append")
    parser.add_argument("--result", type=Path)
    parser.add_argument("--output", type=Path, help="write new JSON file; refuses to overwrite")
    parser.add_argument("--template", type=Path)
    parser.add_argument("--abort-visits", type=int)
    parser.add_argument("--sample-after-seconds", type=int, help="opt-in live stop; always incomplete Level E")
    parser.add_argument("--resample-after-seconds", type=int, help="resume the same guest and sample again by 100 seconds")
    parser.add_argument("--diagnostic-profile", choices=("all", "all-no-io"), default="all",
                        help="diagnostic-only existing I/O-counter kill switch; bulk and other counters stay enabled")
    parser.add_argument("--control-mode", choices=("app", "pty", "console"), help="non-interrupting launcher control")
    parser.add_argument("--binding-register", help="one optional code-address candidate if sampled PC is outside the Hunk")
    args = parser.parse_args()
    frozen = manifest()
    exit_status = 0
    try:
        if args.command == "fs-uae-config":
            if not args.template or not args.output:
                raise ValueError("--template and --output are required")
            with args.output.open("x") as destination:
                destination.write(emulator_config(args.template.read_text()))
            print(f"Wrote {args.output}")
            return 0
        if args.command == "diagnose":
            if not args.case or len(args.case) != 1:
                raise ValueError("diagnose requires exactly one --case")
            if args.output and args.output.exists():
                raise ValueError("diagnostic output already exists")
            output = diagnose(args.case[0], args.abort_visits, frozen, args.sample_after_seconds, args.control_mode,
                              args.binding_register, args.resample_after_seconds, args.diagnostic_profile)
            exit_status = 0 if output["capture_ok"] else 1
        elif args.command == "manifest":
            output = frozen
        elif args.command == "validate":
            if not args.result:
                raise ValueError("--result is required")
            validate_result(read_json(args.result), frozen)
            print("PASS: corpus result valid")
            return 0
        else:
            selected = [case for case in corpus() if not args.case or case["id"] in args.case]
            if not selected or (args.case and set(args.case) != {case["id"] for case in selected}):
                raise ValueError("unknown/empty case selection")
            binary, build = build_release()
            if args.command == "native-input":
                if args.output:
                    raise ValueError("live native oracle bytes are stdout-only and must not be persisted")
                output = {"corpus_sha256": frozen["sha256"], "package": frozen["package"],
                          "package_bytes": list((ROOT / PACKAGE).read_bytes()),
                          "cases": [{**case, "live_rust": run_rust_case(binary, case, 1, live_oracle=True)}
                                    for case in selected]}
                if digest(bytes(output["package_bytes"])) != frozen["package"]["sha256"] or digest((ROOT / PACKAGE).read_bytes()) != frozen["package"]["sha256"]:
                    raise ValueError("package changed while building live native oracles")
                print(json.dumps(output, sort_keys=True))
                return 0
            output = {"schema_version": SCHEMA, "corpus_sha256": frozen["sha256"],
                      "backend": "rust-release-unprofiled", "binary_sha256": digest(binary.read_bytes()),
                      "build": build,
                      "binary_bytes": binary.stat().st_size, "host": platform.platform(),
                      "head": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip(),
                      "rustc": subprocess.check_output(["rustc", "--version"], text=True).strip(),
                      "cargo": subprocess.check_output(["cargo", "--version"], text=True).strip(),
                      "generator_sha256": digest(Path(__file__).read_bytes()),
                      "package": frozen["package"],
                      "selected_cases": [case["id"] for case in selected],
                      "comparison_eligible": args.runs >= frozen["policy"]["retained_runs"] and len(selected) == len(frozen["cases"]),
                      "policy": {**frozen["policy"], "retained_runs": args.runs},
                      "cases": [run_rust_case(binary, case, args.runs) for case in selected]}
            validate_result(output, frozen)
        encoded = json.dumps(output, sort_keys=True, indent=2, allow_nan=False) + "\n"
        if args.output:
            with args.output.open("x") as destination:
                destination.write(encoded)
            print(f"Wrote {args.output}")
        else:
            print(encoded, end="")
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        parser.exit(1, f"error: {error}\n")
    return exit_status


if __name__ == "__main__":
    raise SystemExit(main())

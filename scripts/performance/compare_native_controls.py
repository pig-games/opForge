#!/usr/bin/env python3
"""Compare completed controlled-abort timings, never native parity or throughput."""
# @opforge-evidence: level=E; role=diagnostic-tool; authority=none; lifecycle=permanent

import argparse
import hashlib
import json
import math
import re
from pathlib import Path
import statistics

from production_corpus import ROOT, canonical, validate_diagnostic_capture
from decode_native_progress import (decode_progress, decode_work_multiplication,
                                    decode_symbol_expression_work, decode_runtime_execution, decode_platform_io)


def decode_records(records):
    raw = {name: bytes(data) for name, data in records.items()}
    profile = decode_progress(raw["ofpr"])
    common = dict(expected_run_id=profile["run_id"], expected_state=profile["state"],
                  expected_exit_status=profile["exit_status"])
    profile["work_multiplication"] = decode_work_multiplication(raw["ofwk"], **common)
    common.update(expected_phase=profile["phase_id"], expected_pass=profile["pass"])
    for name, key, decoder in (("ofse", "symbol_expression_work", decode_symbol_expression_work),
                               ("ofvm", "runtime_execution", decode_runtime_execution),
                               ("ofio", "platform_io", decode_platform_io)):
        profile[key] = decoder(raw[name], **common)
    return profile


def common_work(profile):
    # I/O observations are unavailable in all-no-io. Compare every other
    # decoded field except run identity and clocks, not a hand-picked VM total.
    result = dict(profile)
    result["platform_io"] = {key: profile["platform_io"][key] for key in
                            ("schema_version", "state", "phase", "phase_id", "pass", "current_range",
                             "current_range_id", "clears", "copies", "bulk_by_range", "bulk_by_phase",
                             "overflow_bits", "exit_status")}
    clocks = {"run_id", "run_start_tick", "last_progress_tick", "total_elapsed_ticks", "phase_elapsed_ticks"}

    def strip(value):
        if isinstance(value, dict):
            return {key: strip(item) for key, item in value.items() if key not in clocks}
        return value
    return strip(result)


def validate_control(row, frozen):
    case = next(case for case in frozen["cases"] if case["id"] == row["case_id"])
    validate_diagnostic_capture(row["capture"], case, frozen, row["abort_visits"])
    if (row["case_sha256"] != case["sha256"] or row["corpus_sha256"] != frozen["sha256"]
            or type(row["test_exit"]) is not int or type(row["abort_visits"]) is not int
            or row["capture_ok"] is not True or row["test_exit"] != 0 or row["comparison_eligible"] is not False
            or row["complete"] is not False or row["parity_passed"] is not False
            or row["control_mode"] != "console" or row["profile_mode"] not in ("all", "all-no-io")):
        raise ValueError("not a valid non-interrupting diagnostic control")
    sample = row["live_sample"]
    capture = row["capture"]
    timing = sample["control_completion"]
    if (sample["source_inventory_matches_frozen"] is not True or sample["source_inventory"] != case["files"]
            or sample["control_mode"] != "console" or sample["profile_mode"] != row["profile_mode"]
            or sample["cleanup"] != "complete" or sample["commands"] != []
            or sample["sample_observed"] is not False or sample["guest_start_observed"] is not True
            or timing["observed_before_cleanup"] is not True):
        raise ValueError("control lacks exact inventory, clean non-interrupting observation or timing")
    protocol = capture["guest_protocol"]
    start = protocol["started"].strip()
    done = protocol["done"].strip()
    exit_text = protocol["exitcode"].strip()
    if (not re.fullmatch(r"OPFORGE-FS-UAE-PROOF-V1 START [A-Za-z0-9_-]+ [A-Za-z0-9_-]+", start)
            or not re.fullmatch(r"-?[0-9]{1,10}", exit_text)
            or type(timing["guest_exit_observed"]) is not int
            or not -(2 ** 31) <= timing["guest_exit_observed"] < 2 ** 31
            or start != sample["expected_start"] or done != start.replace(" START ", " DONE ", 1)
            or done != timing["expected_done_observed"]
            or int(exit_text) != capture["exit_status"]
            or timing["guest_exit_observed"] != capture["exit_status"]):
        raise ValueError("control timing is not bound to its guest protocol")
    elapsed = timing["start_to_done_host_seconds"]
    if type(elapsed) not in (int, float) or not math.isfinite(elapsed) or not 0 < elapsed < 115:
        raise ValueError("control time exceeds the bounded observation window")
    profile = decode_records(capture["raw_records"])
    if profile != capture["profile"]:
        raise ValueError("stored profile disagrees with independently decoded records")
    if profile["platform_io"]["enabled_groups"] != {"io": row["profile_mode"] == "all", "bulk": True}:
        raise ValueError("counter mode mismatch")
    if hashlib.sha256(sample["config"].encode()).hexdigest() != sample["config_sha256"]:
        raise ValueError("configuration digest mismatch")
    config = [line for line in sample["config"].splitlines()
              if line.split("=", 1)[0].strip() != "hard_drive_1"]
    return profile, elapsed, config, start


def compare_controls(rows, frozen):
    if [row["profile_mode"] for row in rows] != ["all", "all-no-io", "all-no-io", "all"]:
        raise ValueError("expected four controls in all/no-io/no-io/all order")
    fingerprints, configs, challenges, identities, times = [], [], [], {}, {"all": [], "all-no-io": []}
    first = rows[0]
    intended = first["capture"]["instrumentation_defines"]
    if "OPFORGE_PROGRESS_PLATFORM_NO_IO" in intended or len(intended) != len(set(intended)):
        raise ValueError("invalid all-counter define set")
    for row in rows:
        if (row["case_id"], row["case_sha256"], row["corpus_sha256"], row["abort_visits"], row["command"]) != (
                first["case_id"], first["case_sha256"], first["corpus_sha256"], first["abort_visits"], first["command"]):
            raise ValueError("controls do not use the same case and abort boundary")
        if (row["generator_sha256"], row["live_sample"]["sampler_sha256"]) != (
                first["generator_sha256"], first["live_sample"]["sampler_sha256"]):
            raise ValueError("host timing or driver code changed between controls")
        if (row["capture"]["command_template"], row["live_sample"]["fs_uae_version"]) != (
                first["capture"]["command_template"], first["live_sample"]["fs_uae_version"]):
            raise ValueError("guest command or emulator version changed between controls")
        defines = row["capture"]["instrumentation_defines"]
        expected = set(intended) | ({"OPFORGE_PROGRESS_PLATFORM_NO_IO"} if row["profile_mode"] == "all-no-io" else set())
        if len(defines) != len(set(defines)) or set(defines) != expected:
            raise ValueError("instrumentation differs beyond the I/O kill switch")
        profile, elapsed, config, challenge = validate_control(row, frozen)
        fingerprints.append(canonical(common_work(profile)))
        configs.append(config)
        challenges.append(challenge)
        sample = row["live_sample"]
        identity = (sample["native_binary_sha256"], sample["native_binary_bytes"], sample["labels_sha256"])
        mode = row["profile_mode"]
        if mode in identities and identities[mode] != identity:
            raise ValueError("native binary or symbols changed within one observer mode")
        identities[mode] = identity
        times[mode].append(elapsed)
    if len(set(challenges)) != 4:
        raise ValueError("duplicate run challenge")
    if any(item != fingerprints[0] for item in fingerprints) or any(item != configs[0] for item in configs):
        raise ValueError("shared work or emulator configuration differs")
    summary = {mode: {"seconds": values, "mean_seconds": statistics.mean(values),
                      "min_seconds": min(values), "max_seconds": max(values)} for mode, values in times.items()}
    return {"proof_level": "E", "comparison": "controlled-abort-only", "complete": False, "parity_passed": False,
            "case_id": first["case_id"], "case_sha256": first["case_sha256"], "abort_visits": first["abort_visits"],
            "shared_work_sha256": hashlib.sha256(fingerprints[0]).hexdigest(), "shared_work": common_work(profile),
            "modes": summary,
            "all_minus_noio_mean_seconds": summary["all"]["mean_seconds"] - summary["all-no-io"]["mean_seconds"],
            "limitations": "Two runs per mode; START-to-DONE includes load/export, not full assembly. No physical-hardware or total-counter-overhead claim."}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("receipts", type=Path, nargs=4)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    try:
        frozen = json.loads((ROOT / "documentation/performance/results/opforge-corpus-v1-manifest.json").read_text())
        result = compare_controls([json.loads(path.read_text()) for path in args.receipts], frozen)
        result["receipts"] = [{"path": str(path), "sha256": hashlib.sha256(path.read_bytes()).hexdigest()} for path in args.receipts]
        text = json.dumps(result, indent=2, sort_keys=True) + "\n"
        if args.output:
            with args.output.open("x") as destination:
                destination.write(text)
        else:
            print(text, end="")
    except (ValueError, KeyError, TypeError, StopIteration, OSError) as error:
        parser.exit(1, f"control comparison rejected: {error}\n")


if __name__ == "__main__":
    main()

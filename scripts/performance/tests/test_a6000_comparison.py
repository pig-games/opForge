import contextlib
import io
import json
from pathlib import Path
import shutil
import sys
import tempfile
import unittest
from unittest import mock

sys.path.insert(0, str(Path(__file__).parents[1]))

import run_a6000_comparison as comparison
import test_a6000_selfhost as fixtures

runner = comparison.runner


class FixedWorkloadComparisonTests(unittest.TestCase):
    def exports(self, root):
        baseline, candidate = root / "baseline", root / "candidate"
        fixture = fixtures.HardwareCompletionTests("runTest")
        for bundle in (baseline, candidate):
            bundle.mkdir()
            fixture.embedded_output_bundle(bundle)
            manifest = json.loads((bundle / "manifest.json").read_text())
            manifest.update({"kind": "compact-self-host-local-export", "manifest_version": 1,
                             "over_classic_limit_components": []})
            self.save_manifest(bundle, manifest)
        # A genuinely different release image, source and own Rust oracle. Its
        # input package remains identical to the baseline's package.
        (candidate / "original/entry.asm").write_bytes(b"candidate source")
        (candidate / "src/entry.asm").write_bytes(b"candidate source")
        package = (candidate / "m68020--motorola68k.bin").read_bytes()
        self.replace_image(candidate, b"candidate executable:" + package)
        self.refresh_sources(candidate)
        return baseline, candidate, baseline / "original"

    def memory_export(self, release, destination, label):
        shutil.copytree(release, destination)
        manifest = json.loads((destination / "manifest.json").read_text())
        package = (destination / manifest.get("runtime_package_file", "p.bin")).read_bytes()
        bootstrap = f"instrumented-{label}:".encode() + package
        (destination / "opforge").write_bytes(bootstrap)
        manifest.update({
            "bootstrap_defines": sorted(comparison.MEMORY_DEFINES),
            "bootstrap_hunk_digest": runner.fnv(bootstrap),
            "telemetry_file": "memory.bin",
        })
        self.save_manifest(destination, manifest)
        return destination

    def save_manifest(self, bundle, manifest):
        (bundle / "manifest.json").write_text(json.dumps(manifest))

    def replace_image(self, bundle, image):
        manifest = json.loads((bundle / "manifest.json").read_text())
        (bundle / "opforge").write_bytes(image)
        (bundle / "oracle.hunk").write_bytes(image)
        manifest.update({"bootstrap_hunk_digest": runner.fnv(image),
                         "release_hunk_digest": runner.fnv(image)})
        self.save_manifest(bundle, manifest)

    def refresh_sources(self, bundle):
        manifest = json.loads((bundle / "manifest.json").read_text())
        identity = bytearray()
        for row in manifest["source_mapping"]:
            data = (bundle / row["staged_path"]).read_bytes()
            row.update({"bytes": len(data), "digest": runner.fnv(data)})
            identity.extend(row["logical_path"].encode() + b"\0" + data + b"\0")
        manifest.update({"source_manifest_digest": runner.fnv(identity),
                         "source_files": len(manifest["source_mapping"]),
                         "source_bytes": sum(row["bytes"] for row in manifest["source_mapping"])})
        self.save_manifest(bundle, manifest)

    def test_both_cases_share_exact_inputs_and_oracle_but_identify_actual_bootstrap(self):
        with tempfile.TemporaryDirectory() as directory:
            baseline, candidate, frozen = self.exports(Path(directory))
            manifest, oracle, cases = comparison.load_comparison(baseline, candidate, frozen)
            before, after = [case[0] for case in cases]
            self.assertEqual({k: v for k, v in before.items() if k != "opforge"},
                             {k: v for k, v in after.items() if k != "opforge"})
            self.assertEqual(after["src/entry.asm"], b"source")
            self.assertEqual(oracle, (baseline / "oracle.hunk").read_bytes())
            self.assertNotEqual(before["opforge"], after["opforge"])
            self.assertEqual(after["opforge"], runner.load_bundle(candidate)[1]["opforge"])
            self.assertNotEqual(cases[0][1], cases[1][1])
            self.assertEqual(cases[0][2]["workload_sha256"], cases[1][2]["workload_sha256"])
            self.assertFalse(cases[1][2]["candidate_source_selfhost_proven"])
            self.assertEqual(manifest["source_root"], str(frozen))

    def test_candidate_must_pass_original_release_identity_guard(self):
        with tempfile.TemporaryDirectory() as directory:
            baseline, candidate, frozen = self.exports(Path(directory))
            manifest = json.loads((candidate / "manifest.json").read_text())
            image = (candidate / "opforge").read_bytes() + b"changed"
            (candidate / "opforge").write_bytes(image)
            manifest["bootstrap_hunk_digest"] = runner.fnv(image)
            self.save_manifest(candidate, manifest)
            with self.assertRaisesRegex(ValueError, "Release bootstrap/oracle mismatch"):
                comparison.load_comparison(baseline, candidate, frozen)

    def test_changed_bootstrap_and_frozen_source_are_rejected(self):
        for mutation, error in (("bootstrap", "Bootstrap digest"), ("source", "Current source differs")):
            with self.subTest(mutation=mutation), tempfile.TemporaryDirectory() as directory:
                baseline, candidate, frozen = self.exports(Path(directory))
                if mutation == "bootstrap":
                    (candidate / "opforge").write_bytes(b"unverified")
                else:
                    (frozen / "entry.asm").write_bytes(b"unverified")
                with self.assertRaisesRegex(ValueError, error):
                    comparison.load_comparison(baseline, candidate, frozen)

    def test_explicit_frozen_root_must_match_baseline_manifest(self):
        with tempfile.TemporaryDirectory() as directory:
            baseline, candidate, frozen = self.exports(Path(directory))
            with self.assertRaisesRegex(ValueError, "requested frozen source root"):
                comparison.load_comparison(baseline, candidate, candidate / "original")

    def test_individually_valid_different_embedded_package_is_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            baseline, candidate, frozen = self.exports(Path(directory))
            package_path = candidate / "m68020--motorola68k.bin"
            package = bytearray(package_path.read_bytes())
            package.extend(b"different-package")
            package[4:8] = len(package).to_bytes(4, "big")
            package_path.write_bytes(package)
            (candidate / "src/experimental/packages/m68020--motorola68k.bin").write_bytes(package)
            manifest = json.loads((candidate / "manifest.json").read_text())
            manifest["runtime_package_digest"] = runner.fnv(package)
            self.save_manifest(candidate, manifest)
            self.replace_image(candidate, b"candidate executable:" + package)
            self.refresh_sources(candidate)
            runner.load_bundle(candidate)  # Valid export, wrong comparison package.
            with self.assertRaisesRegex(ValueError, "exact same runtime package bytes"):
                comparison.load_comparison(baseline, candidate, frozen)

    def test_instrumentation_is_not_a_release_comparison(self):
        with tempfile.TemporaryDirectory() as directory:
            baseline, candidate, frozen = self.exports(Path(directory))
            manifest = json.loads((candidate / "manifest.json").read_text())
            manifest.update({"bootstrap_defines": ["OPFORGE_DEBUG_CONTRACTS", "OPFORGE_MEMORY_TELEMETRY",
                                                    "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT"],
                             "telemetry_file": "memory.bin"})
            self.save_manifest(candidate, manifest)
            with self.assertRaisesRegex(ValueError, "requires release exports"):
                comparison.load_comparison(baseline, candidate, frozen)

    def test_three_dry_runs_have_fresh_challenges_and_current_source_third(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            baseline, candidate, frozen = self.exports(root)
            self.refresh_sources(baseline)
            created = []

            def temporary_directory(prefix):
                path = root / f"{prefix}{len(created)}"
                path.mkdir()
                created.append(path)
                return str(path)

            with mock.patch("tempfile.mkdtemp", side_effect=temporary_directory), contextlib.redirect_stdout(io.StringIO()):
                status = comparison.main(["--baseline-bundle", str(baseline), "--candidate-bundle", str(candidate),
                                          "--baseline-source-root", str(frozen), "--dry-run"])
            self.assertEqual(status, 0)
            stages = [next(path.glob("opforge-*")) for path in created
                      if list(path.glob("opforge-*"))]
            markers = [runner.re.search(r'START ([^"\n]+)', (stage / "run-selfhost").read_text()).group(1)
                       for stage in stages]
            self.assertEqual(len(set(markers)), 3)
            self.assertEqual((stages[0] / "src/entry.asm").read_bytes(), b"source")
            self.assertEqual((stages[1] / "src/entry.asm").read_bytes(), b"source")
            self.assertEqual((stages[2] / "src/entry.asm").read_bytes(), b"candidate source")
            self.assertEqual((stages[1] / "opforge").read_bytes(), (stages[2] / "opforge").read_bytes())
            summary_path = next(root.rglob("comparison.json"))
            summary = json.loads(summary_path.read_text())
            self.assertTrue(summary["dry_run"])
            self.assertFalse(summary["fixed_workload_parity_passed"])
            self.assertFalse(summary["current_source_selfhost_passed"])

    def test_wrong_third_bootstrap_stops_before_any_transport(self):
        with tempfile.TemporaryDirectory() as directory:
            baseline, candidate, frozen = self.exports(Path(directory))
            with mock.patch.object(runner, "run_case") as execute:
                with self.assertRaisesRegex(ValueError, "same candidate bootstrap"):
                    comparison.main(["--baseline-bundle", str(baseline), "--candidate-bundle", str(candidate),
                                     "--baseline-source-root", str(frozen), "--selfhost-bundle", str(baseline)])
                execute.assert_not_called()

    def test_timing_comparison_requires_both_fresh_exact_successes(self):
        result = {"success": True, "guest_overall_seconds": 12}
        candidate = {"success": True, "guest_overall_seconds": 8}
        records = [{"result": result}, {"result": candidate}]
        self.assertEqual(comparison.comparison_summary(records)["guest_speedup"], 1.5)
        candidate["success"] = False
        summary = comparison.comparison_summary(records)
        self.assertFalse(summary["fixed_workload_parity_passed"])
        self.assertNotIn("guest_speedup", summary)
        candidate.update({"success": True, "guest_overall_seconds": None})
        self.assertIsNone(comparison.comparison_summary(records)["guest_speedup"])

    def test_memory_cases_use_baseline_inputs_with_corresponding_instrumented_bootstraps(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            baseline, candidate, frozen = self.exports(root)
            baseline_memory = self.memory_export(baseline, root / "baseline-memory", "baseline")
            candidate_memory = self.memory_export(candidate, root / "candidate-memory", "candidate")
            _, _, release_cases = comparison.load_comparison(baseline, candidate, frozen)
            cases = comparison.load_memory_cases(
                baseline, candidate, baseline_memory, candidate_memory,
                release_cases[0][2]["workload_sha256"],
            )
            self.assertEqual([case[5] for case in cases], ["baseline_memory", "candidate_memory"])
            workload_files = release_cases[0][0]
            for _, manifest, files, oracle, _, _, metadata in cases:
                self.assertEqual({key: value for key, value in files.items() if key != "opforge"},
                                 {key: value for key, value in workload_files.items() if key != "opforge"})
                self.assertEqual(files["src/entry.asm"], b"source")
                self.assertEqual(oracle, (baseline / "oracle.hunk").read_bytes())
                self.assertEqual(files["opforge"], (Path(metadata["bootstrap_bundle"]) / "opforge").read_bytes())
                self.assertEqual(set(manifest["bootstrap_defines"]), comparison.MEMORY_DEFINES)
                self.assertFalse(metadata["candidate_source_selfhost_proven"])
                self.assertEqual(metadata["workload_source_manifest_digest"],
                                 release_cases[0][2]["workload_source_manifest_digest"])
            self.assertNotEqual(cases[0][3], (candidate / "oracle.hunk").read_bytes())
            self.assertNotEqual(cases[0][4], cases[1][4])

    def test_memory_exports_require_exact_light_flags_and_corresponding_release_inputs(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            baseline, candidate, frozen = self.exports(root)
            baseline_memory = self.memory_export(baseline, root / "baseline-memory", "baseline")
            candidate_memory = self.memory_export(candidate, root / "candidate-memory", "candidate")
            _, _, cases = comparison.load_comparison(baseline, candidate, frozen)
            manifest_path = candidate_memory / "manifest.json"
            manifest = json.loads(manifest_path.read_text())
            manifest["bootstrap_defines"].remove("OPFORGE_CAPTURE_TELEMETRY")
            self.save_manifest(candidate_memory, manifest)
            with self.assertRaisesRegex(ValueError, r"exactly the light MEMD\+CAPTURE"):
                comparison.load_memory_cases(baseline, candidate, baseline_memory, candidate_memory,
                                             cases[0][2]["workload_sha256"])

            manifest["bootstrap_defines"] = sorted(comparison.MEMORY_DEFINES)
            self.save_manifest(candidate_memory, manifest)
            (candidate_memory / "oracle.hunk").write_bytes(b"different oracle")
            with self.assertRaisesRegex(ValueError, "Release oracle mismatch"):
                comparison.load_memory_cases(baseline, candidate, baseline_memory, candidate_memory,
                                             cases[0][2]["workload_sha256"])

    def test_memory_command_cannot_differ_from_frozen_workload_command(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            baseline, candidate, frozen = self.exports(root)
            manifest = json.loads((candidate / "manifest.json").read_text())
            manifest["command"] += " -M src"
            self.save_manifest(candidate, manifest)
            (candidate / "command.txt").write_text(manifest["command"] + "\n")
            baseline_memory = self.memory_export(baseline, root / "baseline-memory", "baseline")
            candidate_memory = self.memory_export(candidate, root / "candidate-memory", "candidate")
            runner.load_bundle(candidate_memory)
            _, _, release_cases = comparison.load_comparison(baseline, candidate, frozen)
            with self.assertRaisesRegex(ValueError, "frozen baseline command"):
                comparison.load_memory_cases(
                    baseline, candidate, baseline_memory, candidate_memory,
                    release_cases[0][2]["workload_sha256"],
                )

    def test_memory_storage_metadata_must_match_corresponding_release(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            baseline, candidate, frozen = self.exports(root)
            baseline_memory = self.memory_export(baseline, root / "baseline-memory", "baseline")
            candidate_memory = root / "candidate-memory"
            candidate_memory.mkdir()
            fixtures.HardwareCompletionTests("runTest").bundle(candidate_memory, instrumented=True)
            manifest_path = candidate_memory / "manifest.json"
            manifest = json.loads(manifest_path.read_text())
            manifest.update({
                "kind": "compact-self-host-local-export", "manifest_version": 1,
                "over_classic_limit_components": [],
                "bootstrap_defines": sorted(comparison.MEMORY_DEFINES),
                "release_defines": [], "telemetry_file": "memory.bin",
            })
            self.save_manifest(candidate_memory, manifest)
            # Both exports pass the independent bundle validator, but their
            # package storage configurations differ.
            runner.load_bundle(candidate_memory)
            runner.load_bundle(candidate)
            _, _, release_cases = comparison.load_comparison(baseline, candidate, frozen)
            with self.assertRaisesRegex(ValueError, "storage settings"):
                comparison.load_memory_cases(
                    baseline, candidate, baseline_memory, candidate_memory,
                    release_cases[0][2]["workload_sha256"],
                )

    def test_memory_bundle_pair_is_required_and_all_exports_validate_before_transport(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            baseline, candidate, frozen = self.exports(root)
            baseline_memory = self.memory_export(baseline, root / "baseline-memory", "baseline")
            candidate_memory = self.memory_export(candidate, root / "candidate-memory", "candidate")
            with self.assertRaises(SystemExit):
                comparison.main(["--baseline-bundle", str(baseline), "--candidate-bundle", str(candidate),
                                 "--baseline-source-root", str(frozen), "--baseline-memory-bundle",
                                 str(baseline_memory), "--dry-run"])

            manifest = json.loads((candidate_memory / "manifest.json").read_text())
            manifest["bootstrap_defines"] = sorted(comparison.MEMORY_DEFINES | {"OPFORGE_HOTSPOT_TELEMETRY"})
            self.save_manifest(candidate_memory, manifest)
            with mock.patch.object(runner, "run_case") as execute, contextlib.redirect_stdout(io.StringIO()):
                with self.assertRaisesRegex(ValueError, r"exactly the light MEMD\+CAPTURE"):
                    comparison.main(["--baseline-bundle", str(baseline), "--candidate-bundle", str(candidate),
                                     "--baseline-source-root", str(frozen), "--baseline-memory-bundle",
                                     str(baseline_memory), "--candidate-memory-bundle", str(candidate_memory)])
                execute.assert_not_called()

    def test_five_dry_runs_keep_release_then_memory_then_own_selfhost_order(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            baseline, candidate, frozen = self.exports(root)
            baseline_memory = self.memory_export(baseline, root / "baseline-memory", "baseline")
            candidate_memory = self.memory_export(candidate, root / "candidate-memory", "candidate")
            created = []

            def temporary_directory(prefix):
                path = root / f"{prefix}{len(created)}"
                path.mkdir()
                created.append(path)
                return str(path)

            real_write_summary = comparison.write_comparison_summary
            observed_run_counts = []

            def observe_summary(path, records, dry_run):
                summary = real_write_summary(path, records, dry_run)
                observed_run_counts.append(len(summary["runs"]))
                return summary

            with mock.patch("tempfile.mkdtemp", side_effect=temporary_directory), \
                    mock.patch.object(comparison, "write_comparison_summary", side_effect=observe_summary), \
                    contextlib.redirect_stdout(io.StringIO()):
                status = comparison.main([
                    "--baseline-bundle", str(baseline), "--candidate-bundle", str(candidate),
                    "--baseline-source-root", str(frozen), "--baseline-memory-bundle", str(baseline_memory),
                    "--candidate-memory-bundle", str(candidate_memory), "--dry-run",
                ])
            self.assertEqual(status, 0)
            stages = [next(path.glob("opforge-*")) for path in created
                      if list(path.glob("opforge-*"))]
            markers = [runner.re.search(r'START ([^"\n]+)', (stage / "run-selfhost").read_text()).group(1)
                       for stage in stages]
            self.assertEqual(len(set(markers)), 5)
            expected_sources = [b"source", b"source", b"source", b"source", b"candidate source"]
            expected_bootstraps = [
                (baseline / "opforge").read_bytes(), (candidate / "opforge").read_bytes(),
                (baseline_memory / "opforge").read_bytes(), (candidate_memory / "opforge").read_bytes(),
                (candidate / "opforge").read_bytes(),
            ]
            for stage, source, bootstrap in zip(stages, expected_sources, expected_bootstraps):
                self.assertEqual((stage / "src/entry.asm").read_bytes(), source)
                self.assertEqual((stage / "opforge").read_bytes(), bootstrap)
            summary_path = next(root.rglob("comparison.json"))
            summary = json.loads(summary_path.read_text())
            self.assertEqual([run["role"] for run in summary["runs"]], [
                "baseline", "candidate", "baseline_memory", "candidate_memory", "current_source_selfhost",
            ])
            self.assertNotIn("memory_summary", summary)
            self.assertEqual(observed_run_counts, [0, 1, 2, 3, 4, 5])

    def test_memory_summary_reports_only_telemetry_deltas_when_both_runs_complete(self):
        keys = ("peak_owned_bytes", "live_owned_bytes", "total_allocated_bytes", "total_freed_bytes",
                "packed_source_bytes", "prepared_live_bytes", "assembly_live_bytes")

        def result(offset):
            memory = {key: offset + index for index, key in enumerate(keys)}
            capture = {"counters": {
                "construction_copy": {"retained_append_copy_bytes": offset + 100},
                "retained_growth": {"retained_peak_capacity_including_overlap": offset + 200},
            }}
            return {"success": True, "telemetry_completed": True,
                    "instrumented_memory": memory, "capture_ownership": capture,
                    "guest_overall_seconds": 9000 - offset}

        records = [{"role": "baseline_memory", "result": result(0)},
                   {"role": "candidate_memory", "result": result(20)}]
        summary = comparison.memory_comparison_summary(records)
        self.assertIsNotNone(summary)
        self.assertEqual(summary["metrics"]["peak_owned_bytes"],
                         {"baseline": 0, "candidate": 20, "delta": 20})
        self.assertEqual(summary["metrics"]["capture_retained_peak_capacity_including_overlap_bytes"]["delta"], 20)
        self.assertFalse(summary["instrumented_timings_used_for_speedup"])
        records[1]["result"]["success"] = False
        self.assertIsNone(comparison.memory_comparison_summary(records))
        records[1]["result"]["success"] = True
        records[1]["result"]["telemetry_completed"] = False
        self.assertIsNone(comparison.memory_comparison_summary(records))

    def test_candidate_output_must_match_frozen_oracle_with_own_fresh_challenge(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            baseline, candidate, frozen = self.exports(root)
            _, oracle, cases = comparison.load_comparison(baseline, candidate, frozen)
            files, digest, _ = cases[1]
            capture = root / "capture"
            for name, data in files.items():
                target = capture / name
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_bytes(data)
            marker = f"fresh-candidate-challenge {digest}"
            for name, text in {"start.marker": f"START {marker}", "done.marker": f"DONE {marker}",
                               "exitcode": "0", "start.time": "10:00:00", "end.time": "10:00:01"}.items():
                (capture / name).write_text(text)
            (capture / "output.hunk").write_bytes(oracle)
            self.assertTrue(runner.inspect_result(capture, files, oracle, marker, 2)["success"])
            (capture / "output.hunk").write_bytes((candidate / "oracle.hunk").read_bytes())
            self.assertFalse(runner.inspect_result(capture, files, oracle, marker, 2)["success"])
            (capture / "output.hunk").write_bytes(oracle)
            (capture / "done.marker").write_text(f"DONE stale-challenge {cases[0][1]}")
            with self.assertRaisesRegex(ValueError, "fresh completion evidence"):
                runner.inspect_result(capture, files, oracle, marker, 2)


if __name__ == "__main__":
    unittest.main()

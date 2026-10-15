import argparse
import importlib.util
import json
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch
from types import SimpleNamespace

sys.path.insert(0, str(Path(__file__).parents[1]))
spec = importlib.util.spec_from_file_location(
    "lazy_comparison", Path(__file__).parents[1] / "run_lazy_preparation_comparison.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class LazyComparisonTests(unittest.TestCase):
    def add_work_images(self, images, source_digest):
        path = images / "images.json"
        manifest = json.loads(path.read_text())
        manifest["native_source_fnv1a64"] = source_digest
        for row in list(manifest["images"]):
            if row["file"].startswith("audit-"):
                data = (source_digest + row["file"]).encode()
                work = dict(row, file=row["file"].replace("audit-", "work-"),
                            defines=row["defines"] + ["OPFORGE_LAZY_WORK"],
                            fnv1a64=module.runner.fnv(data), bytes=len(data))
                (images / work["file"]).write_bytes(data)
                manifest["images"].append(work)
        path.write_text(json.dumps(manifest))

    def inputs(self, root):
        workload = root / "workload"
        workload.mkdir()
        rows = []
        for name, data in (("src/main.asm", b"source"), ("src/packages/cpu.bin", b"package")):
            path = workload / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(data)
            rows.append({"path": name, "guest_path": name, "digest": module.runner.fnv(data)})
        oracle = b"whole oracle"
        (workload / "oracle.hunk").write_bytes(oracle)
        case = workload / "case.json"
        case.write_text(json.dumps({"schema": 1, "files": rows,
            "oracle": {"path": "oracle.hunk", "digest": module.runner.fnv(oracle)}}))
        images = root / "images"
        images.mkdir()
        image_rows = []
        for role in ("reference", "audit", "deferred"):
            for mode in ("release", "memory"):
                name = f"{role}-{mode}.hunk"
                data = name.encode()
                (images / name).write_bytes(data)
                defines = list(module.MEMORY_DEFINES) if mode == "memory" else []
                if role == "audit":
                    defines.append("OPFORGE_LAZY_AUDIT")
                if role == "deferred":
                    defines.append("OPFORGE_DEFERRED_MACRO_PLANS")
                image_rows.append({"file": name, "defines": defines,
                                   "fnv1a64": module.runner.fnv(data), "bytes": len(data)})
        (images / "images.json").write_text(json.dumps({"images": image_rows}))
        probe_data = b"deferred-probe-memory.hunk"
        (images / "deferred-probe-memory.hunk").write_bytes(probe_data)
        manifest = json.loads((images / "images.json").read_text())
        manifest["images"].append({"file": "deferred-probe-memory.hunk",
            "defines": ["OPFORGE_DEFERRED_MACRO_PLANS", *module.MEMORY_DEFINES,
                        "OPFORGE_LAZY_AUDIT", "OPFORGE_LAZY_WORK", "OPFORGE_LAZY_COMPONENT_PROBE"],
            "fnv1a64": module.runner.fnv(probe_data), "bytes": len(probe_data)})
        (images / "images.json").write_text(json.dumps(manifest))
        return argparse.Namespace(workload_case=case, images=images,
                                  output=root / "results", repeats=2, test_binary=None,
                                  experiment_define=None, work_attribution=False, baseline_images=None)

    def test_candidate_flags_require_baseline_and_timing_is_memory_only(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            args = self.inputs(root)
            args.candidate_attribution = True
            with self.assertRaisesRegex(ValueError, "requires"):
                module.prepare(args)
            args.work_attribution = True
            baseline_root = root / "baseline"
            baseline_root.mkdir()
            baseline = self.inputs(baseline_root)
            self.add_work_images(args.images, "candidate")
            self.add_work_images(baseline.images, "baseline")
            args.baseline_images = baseline.images
            args.candidate_timing = True
            args.candidate_sample_period = 128
            args.candidate_phase_seed = 37
            manifest_path = args.images / "images.json"
            manifest = json.loads(manifest_path.read_text())
            for row in manifest["images"]:
                if row["file"].startswith("work-"):
                    row["defines"].append("OPFORGE_LAZY_CANDIDATES")
                    if "memory" in row["file"]:
                        row["defines"].extend(["OPFORGE_CANDIDATE_TIMING",
                            "OPFORGE_CANDIDATE_SAMPLE_PERIOD=128", "OPFORGE_CANDIDATE_PHASE_SEED=37"])
            manifest_path.write_text(json.dumps(manifest))
            cases = [json.loads(path.read_text()) for path in module.prepare(args)]
            for case in cases:
                candidate = case["metadata"]["comparison_role"] == "audit"
                memory = case["metadata"]["measurement"] == "memory"
                self.assertEqual("OPFORGE_LAZY_CANDIDATES" in case["bootstrap_defines"], candidate)
                self.assertEqual("OPFORGE_CANDIDATE_TIMING" in case["bootstrap_defines"], candidate and memory)
                for flag in ("OPFORGE_CANDIDATE_SAMPLE_PERIOD=128", "OPFORGE_CANDIDATE_PHASE_SEED=37"):
                    self.assertEqual(flag in case["bootstrap_defines"], candidate and memory)
                self.assertEqual(case["metadata"]["candidate_sample_period"], 128)
                self.assertEqual(case["metadata"]["candidate_phase_seed"], 37)

    def test_candidate_comparison_reconciles_calls_and_template_consumers(self):
        import copy
        total = {field: 3 for field in ("candidate_calls", "descriptor_calls", "fragment_calls", "plan_create_calls")}
        total.update(capture_header_bytes=216, capture_plan_bytes=12, capture_bytes=260,
                     capture_token_bytes=20, capture_lexeme_bytes=12)
        work = {"capture_header_bytes": 72, "metrics": {"total": total}, "template_inventory": {
            "definitions": [{"row": 1, "calls": 2}], "consumer_relationships": [{"callee_row": 1}]}}
        memory = {role: {"audit": {"work_attribution": copy.deepcopy(work)}} for role in ("reference", "audit")}
        candidate_total = dict(total, token_bytes=20, lexeme_bytes=12)
        memory["audit"]["candidate_attribution"] = {"records_created": 3, "plan_bytes_created": 12,
                                                     "metrics": {"total": candidate_total, "not_retained": {
                                                         "records": 0, "plan_bytes": 0, "capture_bytes": 0,
                                                         "token_bytes": 0, "lexeme_bytes": 0}}}
        module.validate_candidate_comparison(memory)
        report = memory["audit"]["candidate_attribution"]
        # Gross acquisitions may include discarded records absent from retained work bytes.
        report["records_created"] += 1
        report["plan_bytes_created"] += 4
        report["metrics"]["not_retained"].update(records=1, plan_bytes=4, capture_bytes=100, token_bytes=12, lexeme_bytes=8)
        for field, value in (("capture_bytes", 100), ("token_bytes", 12), ("lexeme_bytes", 8)):
            report["metrics"]["total"][field] += value
        module.validate_candidate_comparison(memory)
        for key in ("records_created", "plan_bytes_created"):
            report[key] += 1
            with self.assertRaisesRegex(ValueError, "totals disagree"):
                module.validate_candidate_comparison(memory)
            report[key] -= 1
        for key in ("capture_bytes", "token_bytes", "lexeme_bytes"):
            report["metrics"]["total"][key] += 1
            with self.assertRaisesRegex(ValueError, "totals disagree"):
                module.validate_candidate_comparison(memory)
            report["metrics"]["total"][key] -= 1
        memory["audit"]["candidate_attribution"]["metrics"]["total"]["candidate_calls"] += 1
        with self.assertRaisesRegex(ValueError, "totals disagree"):
            module.validate_candidate_comparison(memory)
        memory["audit"]["candidate_attribution"]["metrics"]["total"]["candidate_calls"] -= 1
        memory["audit"]["audit"]["work_attribution"]["template_inventory"]["definitions"][0]["calls"] += 1
        with self.assertRaisesRegex(ValueError, "template definitions"):
            module.validate_candidate_comparison(memory)

    def test_candidate_sampling_options_require_timing_and_valid_seed(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            args.candidate_sample_period = 128
            with self.assertRaisesRegex(ValueError, "requires --candidate-timing"):
                module.prepare(args)
            args.candidate_phase_seed = 128
            with self.assertRaisesRegex(ValueError, "Invalid candidate"):
                module.prepare(args)

    def test_report_sampling_must_match_image_configuration(self):
        report = {"sample_period": 128, "phase_seed": 37, "frequency": 100, "timed_calls": 1}
        metadata = {"candidate_sample_period": 128, "candidate_phase_seed": 37, "candidate_timing": True}
        module.validate_candidate_configuration(report, metadata)
        for field in ("candidate_sample_period", "candidate_phase_seed"):
            wrong = dict(metadata)
            wrong[field] += 1
            with self.assertRaisesRegex(ValueError, "configuration"):
                module.validate_candidate_configuration(report, wrong)
        with self.assertRaisesRegex(ValueError, "unexpectedly"):
            module.validate_candidate_configuration(report, dict(metadata, candidate_timing=False))
        with self.assertRaisesRegex(ValueError, "unexpectedly"):
            module.validate_candidate_configuration(dict(report, frequency=0), dict(metadata, candidate_timing=False))

    def test_candidate_timing_alone_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            args.candidate_timing = True
            with self.assertRaisesRegex(ValueError, "requires"):
                module.prepare(args)

    def test_deferred_mode_uses_feature_images_and_fixed_workload(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            args.experiment_define = "OPFORGE_DEFERRED_MACRO_PLANS"
            cases = [json.loads(path.read_text()) for path in module.prepare(args)]
            self.assertEqual(len(cases), 6)
            self.assertEqual([case["metadata"]["comparison_role"] for case in cases],
                             ["reference", "audit", "audit", "reference", "reference", "audit"])
            self.assertTrue(all(case["metadata"]["macro_binding_fixed_input_comparison"] for case in cases))
            self.assertTrue(all(case["metadata"]["experiment_define"] == args.experiment_define for case in cases))
            for case in cases:
                defines = case["bootstrap_defines"]
                candidate = case["metadata"]["comparison_role"] == "audit"
                self.assertEqual(args.experiment_define in defines, candidate)
                self.assertNotIn("OPFORGE_LAZY_AUDIT", defines)
                expected_image = f"{'deferred' if candidate else 'reference'}-{'memory' if case['metadata']['measurement'] == 'memory' else 'release'}.hunk"
                image_bytes = (args.images / expected_image).read_bytes()
                self.assertEqual(case["image"]["digest"], module.runner.fnv(image_bytes))
            self.assertTrue(all(case["files"] == cases[0]["files"] for case in cases))
            self.assertTrue(all(case["oracle"] == cases[0]["oracle"] for case in cases))

    def test_experiment_mode_rejects_mixed_comparison_modes(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            args.experiment_define = "OPFORGE_DEFERRED_MACRO_PLANS"
            args.work_attribution = True
            with self.assertRaisesRegex(ValueError, "cannot be combined"):
                module.prepare(args)
            args.work_attribution = False
            args.baseline_images = args.images
            with self.assertRaisesRegex(ValueError, "cannot be combined"):
                module.prepare(args)
            self.assertFalse(args.output.exists())

    def test_experiment_result_does_not_require_lazy_audit_report(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            args.experiment_define = "OPFORGE_DEFERRED_MACRO_PLANS"
            args.test_binary = Path("fake-test")
            paths = module.prepare(args)
            for case in paths:
                result_dir = case.parent / "result"
                result_dir.mkdir()
                metadata = json.loads(case.read_text())["metadata"]
                receipt = {"success": True, "metadata": metadata, "start_to_done_host_seconds": 1.0,
                           "linked_static_reserved_bytes": 12}
                (result_dir / "native-result.json").write_text(json.dumps(receipt))
                if metadata["measurement"] == "memory":
                    (result_dir / "memory.bin").write_bytes(b"")
                    if metadata["comparison_role"] == "audit":
                        (result_dir / "assembly.stdout").write_text(
                            "progress p=00000ce4 f=00000008 l=00000005 r=00000003 m=00000000\n"
                            "progress p=00000ce5 f=00000004 l=00000002 r=00000000 m=00000000\n")
            with patch.object(module.subprocess, "run", return_value=SimpleNamespace(returncode=0)), \
                 patch.object(module.memory_telemetry, "decode_memory_telemetry", return_value={
                     "diagnostics": [], "peak_owned_bytes": 20, "total_allocated_bytes": 30}):
                self.assertTrue(module.execute(args, paths))
            saved = json.loads((paths[-1].parent / "result.json").read_text())
            self.assertTrue(saved["success"])
            self.assertNotIn("audit", saved)
            self.assertEqual(saved["deferred_binding"], {
                "created": 8, "bound": 5, "hits": 3, "recipe_bytes": 4,
                "normal_plan_used": 2})
            self.assertTrue((paths[-1].parent / "result" / "assembly.stdout").exists())

    def test_deferred_report_rejects_missing_duplicate_reserved_and_bad_bounds(self):
        first = "progress p=00000ce4 f=00000008 l=00000005 r=00000003 m=00000000"
        second = "progress p=00000ce5 f=00000004 l=00000002 r=00000000 m=00000000"
        self.assertEqual(module.decode_deferred_binding(
            first.replace("m=00000000", "m=0005adc8") + "\n" + second.replace("m=00000000", "m=0005adc8"), 10)["bound"], 5)
        with self.assertRaisesRegex(ValueError, "Missing"):
            module.decode_deferred_binding(first, 10)
        with self.assertRaisesRegex(ValueError, "Duplicate"):
            module.decode_deferred_binding(first + "\n" + first + "\n" + second, 10)
        with self.assertRaisesRegex(ValueError, "reserved"):
            module.decode_deferred_binding(first + "\n" + second.replace("r=00000000", "r=00000001"), 10)
        with self.assertRaisesRegex(ValueError, "created"):
            module.decode_deferred_binding(
                first.replace("l=00000005", "l=00000009") + "\n" + second, 10)
        with self.assertRaisesRegex(ValueError, "allocated"):
            module.decode_deferred_binding(first + "\n" + second, 3)

    def test_identical_workloads_with_reversed_timing_order_and_separate_memory(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            paths = module.prepare(args)
            cases = [json.loads(path.read_text()) for path in paths]
            self.assertEqual([case["metadata"]["comparison_role"] for case in cases],
                             ["reference", "audit", "audit", "reference", "reference", "audit"])
            self.assertEqual(len({case["metadata"]["workload_sha256"] for case in cases}), 1)
            self.assertTrue(all(case["files"] == cases[0]["files"] for case in cases))
            self.assertTrue(all(case["oracle"] == cases[0]["oracle"] for case in cases))
            for case in cases[:4]:
                self.assertNotIn("OPFORGE_MEMORY_TELEMETRY", case["bootstrap_defines"])
            for case in cases[4:]:
                self.assertIn("OPFORGE_MEMORY_TELEMETRY", case["bootstrap_defines"])
            self.assertTrue(all(not case["metadata"]["candidate_source_selfhost_proven"] for case in cases))

    def test_incomplete_inventory_rejects_otherwise_complete_native_receipt(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            args.work_attribution = True
            self.add_work_images(args.images, "candidate")
            case = module.prepare(args)[-1]  # Audit memory case.
            result_dir = case.parent / "result"
            result_dir.mkdir()
            metadata = json.loads(case.read_text())["metadata"]
            (result_dir / "native-result.json").write_text(json.dumps({"success": True, "metadata": metadata}))
            (result_dir / "memory.bin").write_bytes(b"")
            (result_dir / "assembly.stdout").write_text("bounded inventory overflow report")
            audit = {"observer_complete": True, "work_attribution": {
                "accounting_complete": True, "template_inventory": {"complete": False, "errors": 38}}}
            with patch.object(module.subprocess, "run", return_value=SimpleNamespace(returncode=0)), \
                 patch.object(module.memory_telemetry, "decode_memory_telemetry", return_value={"diagnostics": []}), \
                 patch.object(module.lazy_preparation_audit, "decode", return_value=audit):
                self.assertFalse(module.execute(args, [case]))
            saved = json.loads((case.parent / "result.json").read_text())
            self.assertFalse(saved["success"])
            self.assertEqual(saved["error"], "Incomplete template inventory")
            self.assertTrue(saved["audit"]["work_attribution"]["accounting_complete"])
            self.assertEqual(saved["test_exit_code"], 0)

    def test_changed_source_is_rejected_before_staging(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            (args.workload_case.parent / "src/main.asm").write_bytes(b"changed")
            with self.assertRaisesRegex(ValueError, "Changed workload"):
                module.prepare(args)
            self.assertFalse(args.output.exists())

    def test_work_attribution_changes_only_the_audit_build_flag(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            args.work_attribution = True
            self.add_work_images(args.images, "candidate")
            cases = [json.loads(path.read_text()) for path in module.prepare(args)]
            for case in cases:
                self.assertIn("OPFORGE_LAZY_AUDIT", case["bootstrap_defines"])
                self.assertEqual("OPFORGE_LAZY_WORK" in case["bootstrap_defines"],
                                 case["metadata"]["comparison_role"] == "audit")
                self.assertTrue(case["metadata"]["work_attribution_comparison"])

    def test_observer_revisions_keep_flags_and_workload_identical(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            args = self.inputs(root)
            baseline_root = root / "baseline"
            baseline_root.mkdir()
            baseline = self.inputs(baseline_root)
            self.add_work_images(args.images, "candidate")
            self.add_work_images(baseline.images, "baseline")
            args.work_attribution = True
            args.baseline_images = baseline.images
            cases = [json.loads(path.read_text()) for path in module.prepare(args)]
            for case in cases:
                self.assertIn("OPFORGE_LAZY_WORK", case["bootstrap_defines"])
                expected = "baseline" if case["metadata"]["comparison_role"] == "reference" else "candidate"
                self.assertEqual(case["metadata"]["bootstrap_source_fnv1a64"], expected)
                self.assertTrue(case["metadata"]["work_observer_revision_comparison"])
            self.assertTrue(all(case["files"] == cases[0]["files"] for case in cases))
            self.assertTrue(all(case["oracle"] == cases[0]["oracle"] for case in cases))
            self.assertNotEqual(cases[0]["image"]["digest"], cases[1]["image"]["digest"])

    def test_baseline_without_work_attribution_is_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            args.baseline_images = args.images
            with self.assertRaisesRegex(ValueError, "require.*work-attribution"):
                module.prepare(args)
            self.assertFalse(args.output.exists())

    def test_mislabeled_memory_build_cannot_supply_runtime_measurement(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            path = args.images / "images.json"
            manifest = json.loads(path.read_text())
            manifest["images"][0]["defines"] = module.MEMORY_DEFINES
            path.write_text(json.dumps(manifest))
            with self.assertRaisesRegex(ValueError, "Wrong measurement build defines"):
                module.prepare(args)

    def test_changed_image_is_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            (args.images / "audit-release.hunk").write_bytes(b"different")
            with self.assertRaisesRegex(ValueError, "Changed image"):
                module.prepare(args)


if __name__ == "__main__":
    unittest.main()

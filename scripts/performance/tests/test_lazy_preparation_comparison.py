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
    def observer_inputs(self, root, period=64, seed=0, experiment=module.PACKING_EXPERIMENT):
        args = self.inputs(root)
        args.experiment_define = experiment
        args.packing_sample_period = period
        args.packing_phase_seed = seed
        path = args.images / "images.json"
        manifest = json.loads(path.read_text())
        for image in manifest["images"]:
            if not image["file"].startswith(("reference-", "deferred-")):
                continue
            memory = image["file"].endswith("memory.hunk")
            candidate = image["file"].startswith("deferred-")
            image["defines"] = list(module.PACKING_BASELINE) + (module.MEMORY_DEFINES if memory else [])
            if memory and experiment == module.CAPTURE_LIFETIME_EXPERIMENT:
                image["defines"].append("OPFORGE_CAPTURE_TELEMETRY")
            if candidate:
                image["defines"].append(experiment)
                if memory and experiment == module.PACKING_EXPERIMENT:
                    image["defines"].append("OPFORGE_PACKING_TIMING")
                    if period != 64:
                        image["defines"].append(f"OPFORGE_PACKING_SAMPLE_PERIOD={period}")
                    if seed != 0:
                        image["defines"].append(f"OPFORGE_PACKING_PHASE_SEED={seed}")
        path.write_text(json.dumps(manifest))
        return args

    def test_packing_shares_inventory_and_samples_only_candidate_memory(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.observer_inputs(Path(directory), period=128, seed=255)
            cases = [json.loads(path.read_text()) for path in module.prepare(args)]
            for case in cases:
                metadata, defines = case["metadata"], case["bootstrap_defines"]
                candidate_memory = metadata["comparison_role"] == "audit" and metadata["measurement"] == "memory"
                self.assertTrue(metadata["macro_packing_fixed_input_comparison"])
                self.assertEqual(metadata["reference_config"], module.PACKING_REFERENCE)
                self.assertEqual(metadata["packing_sample_period"], 128)
                self.assertEqual(metadata["packing_phase_seed"], 255)
                self.assertTrue(set(module.PACKING_BASELINE) <= set(defines))
                self.assertEqual(module.PACKING_EXPERIMENT in defines, metadata["comparison_role"] == "audit")
                for flag in ("OPFORGE_PACKING_TIMING", "OPFORGE_PACKING_SAMPLE_PERIOD=128", "OPFORGE_PACKING_PHASE_SEED=255"):
                    self.assertEqual(flag in defines, candidate_memory)
            self.assertEqual(len({case["metadata"]["workload_sha256"] for case in cases}), 1)

    def test_capture_lifetime_reuses_inventory_and_capture_telemetry_in_memory_only(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.observer_inputs(Path(directory), experiment=module.CAPTURE_LIFETIME_EXPERIMENT)
            for path in module.prepare(args):
                case = json.loads(path.read_text())
                metadata, defines = case["metadata"], case["bootstrap_defines"]
                self.assertTrue(metadata["capture_lifetime_fixed_input_comparison"])
                self.assertFalse(metadata["macro_packing_fixed_input_comparison"])
                self.assertTrue(set(module.PACKING_BASELINE) <= set(defines))
                self.assertEqual(module.CAPTURE_LIFETIME_EXPERIMENT in defines, metadata["comparison_role"] == "audit")
                self.assertEqual("OPFORGE_CAPTURE_TELEMETRY" in defines, metadata["measurement"] == "memory")
                self.assertNotIn(module.PACKING_EXPERIMENT, defines)
                self.assertFalse(any(define.startswith("OPFORGE_PACKING_") for define in defines))

    def test_capture_lifetime_decodes_both_existing_reports_and_only_candidate_lifetime(self):
        import copy
        from contextlib import redirect_stdout
        import io
        with tempfile.TemporaryDirectory() as directory:
            args = self.observer_inputs(Path(directory), experiment=module.CAPTURE_LIFETIME_EXPERIMENT)
            args.test_binary = Path("fake-test")
            paths = module.prepare(args)
            for path in paths:
                metadata = json.loads(path.read_text())["metadata"]
                result = path.parent / "result"
                result.mkdir()
                (result / "native-result.json").write_text(json.dumps({"success": True, "metadata": metadata,
                    "start_to_done_host_seconds": 1.0, "linked_static_reserved_bytes": 12}))
                if metadata["measurement"] == "memory":
                    (result / "memory.bin").write_bytes(b"")
                    (result / "assembly.stdout").write_text(metadata["comparison_role"])
            inv = {"complete": True, "definitions": [], "consumer_relationships": [], "pool_used_bytes": {},
                   "attributed_payload_bytes": {}, "unattributed_pool_bytes": {}}
            old = {"observer_complete": True, "work_attribution": {"accounting_complete": True,
                   "capture_header_bytes": 60, "metrics": {"total": {}}, "template_inventory": inv}}
            capture = {"version": 1, "boundary_mask": 7}
            new = {"complete": True, "schema": 1}
            memory = {"diagnostics": [], "peak_owned_bytes": 20, "total_allocated_bytes": 30}
            with patch.object(module.subprocess, "run", return_value=SimpleNamespace(returncode=0)), \
                 patch.object(module.memory_telemetry, "decode_memory_telemetry", return_value=memory), \
                 patch.object(module.lazy_preparation_audit, "decode", side_effect=lambda _: copy.deepcopy(old)) as decode_old, \
                 patch.object(module.capture_telemetry, "decode_capture_telemetry", return_value={"capture_summary": capture}) as decode_capture, \
                 patch.object(module.capture_lifetime_audit, "decode", return_value=new) as decode_new, \
                 patch.object(module.macro_packing_audit, "decode", side_effect=AssertionError("wrong report")), \
                 redirect_stdout(io.StringIO()):
                self.assertTrue(module.execute(args, paths))
            self.assertEqual(decode_old.call_count, 2)
            self.assertEqual(decode_capture.call_count, 2)
            self.assertEqual([call.kwargs for call in decode_capture.call_args_list], [{"record_header_bytes": 60}] * 2)
            decode_new.assert_called_once_with("audit", memory, old, capture)
            summary = json.loads((args.output / "summary.json").read_text())
            self.assertEqual(summary["capture_lifetime"], new)
            self.assertEqual(summary["capture_ownership"], {"reference": capture, "audit": capture})

    def test_capture_lifetime_rejects_incomplete_capture_ownership(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.observer_inputs(Path(directory), experiment=module.CAPTURE_LIFETIME_EXPERIMENT)
            args.test_binary = Path("fake-test")
            path = module.prepare(args)[-2]  # Reference memory must qualify too.
            result = path.parent / "result"
            result.mkdir()
            metadata = json.loads(path.read_text())["metadata"]
            (result / "native-result.json").write_text(json.dumps({"success": True, "metadata": metadata}))
            (result / "memory.bin").write_bytes(b"")
            (result / "assembly.stdout").write_text("")
            old = {"observer_complete": True, "work_attribution": {"accounting_complete": True,
                   "capture_header_bytes": 60, "template_inventory": {"complete": True}}}
            with patch.object(module.subprocess, "run", return_value=SimpleNamespace(returncode=0)), \
                 patch.object(module.memory_telemetry, "decode_memory_telemetry", return_value={"diagnostics": []}), \
                 patch.object(module.lazy_preparation_audit, "decode", return_value=old), \
                 patch.object(module.capture_telemetry, "decode_capture_telemetry", return_value={"capture_summary": None}):
                self.assertFalse(module.execute(args, [path]))
            saved = json.loads((path.parent / "result.json").read_text())
            self.assertEqual(saved["error"], "Incomplete capture ownership report")

    def test_packing_default_sampling_omits_numeric_defines(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.observer_inputs(Path(directory))
            for path in module.prepare(args):
                defines = json.loads(path.read_text())["bootstrap_defines"]
                self.assertFalse(any(define.startswith(("OPFORGE_PACKING_SAMPLE_PERIOD=", "OPFORGE_PACKING_PHASE_SEED=")) for define in defines))

    def test_packing_sampling_options_reject_invalid_or_other_profile(self):
        for period, seed in ((63, 0), (64, -1), (64, 256)):
            with tempfile.TemporaryDirectory() as directory:
                args = self.inputs(Path(directory))
                args.packing_sample_period, args.packing_phase_seed = period, seed
                with self.assertRaisesRegex(ValueError, "Invalid packing"):
                    module.prepare(args)
                self.assertFalse(args.output.exists())
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            args.packing_sample_period = 128
            with self.assertRaisesRegex(ValueError, "require OPFORGE_MACRO_PACKING_AUDIT"):
                module.prepare(args)

    def test_packing_report_configuration_and_unchanged_accounting(self):
        import copy
        report = {"sample_period": 128, "phase_seed": 255, "timing_enabled": True}
        metadata = {"packing_sample_period": 128, "packing_phase_seed": 255}
        module.validate_packing_configuration(report, metadata)
        for changed in (dict(report, sample_period=64), dict(report, phase_seed=0), dict(report, timing_enabled=False)):
            with self.assertRaisesRegex(ValueError, "configuration"):
                module.validate_packing_configuration(changed, metadata)
        work = {"metrics": {"total": {"bind_calls": 2}}, "template_inventory": {
            key: [] if key in ("definitions", "consumer_relationships") else {}
            for key in ("definitions", "consumer_relationships", "pool_used_bytes", "attributed_payload_bytes", "unattributed_pool_bytes")}}
        memory = {role: {"audit": {"work_attribution": copy.deepcopy(work)}} for role in ("reference", "audit")}
        module.validate_shared_inventory_comparison(memory, "Packing")
        memory["audit"]["audit"]["work_attribution"]["metrics"]["total"]["bind_calls"] += 1
        with self.assertRaisesRegex(ValueError, "work metrics"):
            module.validate_shared_inventory_comparison(memory, "Packing")
        memory["audit"]["audit"]["work_attribution"]["metrics"]["total"]["bind_calls"] -= 1
        for key in work["template_inventory"]:
            original = copy.deepcopy(memory["audit"]["audit"]["work_attribution"]["template_inventory"][key])
            memory["audit"]["audit"]["work_attribution"]["template_inventory"][key] = {"changed": 1}
            with self.assertRaisesRegex(ValueError, "changed template"):
                module.validate_shared_inventory_comparison(memory, "Packing")
            memory["audit"]["audit"]["work_attribution"]["template_inventory"][key] = original

    def test_packing_work_reconciles_independent_binder_and_body_totals(self):
        import copy
        report = {"gross": {"names": 2, "members": 3, "stored_bytes": 40},
                  "outside_writer_binds": {"names": 5, "members": 7}}
        work = {"metrics": {"total": {"bind_calls": 17}},
                "template_inventory": {"pool_used_bytes": {"body_bytes": 40}}}
        module.validate_packing_work(report, work)
        for section in ("gross", "outside_writer_binds"):
            for field in ("names", "members"):
                changed = copy.deepcopy(report)
                changed[section][field] += 1
                with self.subTest(section=section, field=field), self.assertRaisesRegex(ValueError, "bind totals"):
                    module.validate_packing_work(changed, work)
        report["gross"]["stored_bytes"] += 1
        with self.assertRaisesRegex(ValueError, "stored body byte totals"):
            module.validate_packing_work(report, work)

    def test_packing_decodes_inventory_on_both_memory_profiles(self):
        import copy
        from contextlib import redirect_stdout
        import io
        with tempfile.TemporaryDirectory() as directory:
            args = self.observer_inputs(Path(directory))
            args.test_binary = Path("fake-test")
            paths = module.prepare(args)
            for path in paths:
                metadata = json.loads(path.read_text())["metadata"]
                result = path.parent / "result"
                result.mkdir()
                (result / "native-result.json").write_text(json.dumps({"success": True, "metadata": metadata,
                    "start_to_done_host_seconds": 1.0, "linked_static_reserved_bytes": 12}))
                if metadata["measurement"] == "memory":
                    (result / "memory.bin").write_bytes(b"")
                    (result / "assembly.stdout").write_text(metadata["comparison_role"])
            inv = {"complete": True, "definitions": [], "consumer_relationships": [], "pool_used_bytes": {"body_bytes": 0},
                   "attributed_payload_bytes": {}, "unattributed_pool_bytes": {}}
            old = {"observer_complete": True, "work_attribution": {"accounting_complete": True,
                   "metrics": {"total": {"bind_calls": 0}}, "template_inventory": inv}}
            new = {"complete": True, "sample_period": 64, "phase_seed": 0, "timing_enabled": True,
                   "gross": {"names": 0, "members": 0, "stored_bytes": 0},
                   "outside_writer_binds": {"names": 0, "members": 0}}
            with patch.object(module.subprocess, "run", return_value=SimpleNamespace(returncode=0)), \
                 patch.object(module.memory_telemetry, "decode_memory_telemetry", return_value={
                     "diagnostics": [], "peak_owned_bytes": 20, "total_allocated_bytes": 30}), \
                 patch.object(module.lazy_preparation_audit, "decode", side_effect=lambda _: copy.deepcopy(old)) as decode_old, \
                 patch.object(module.macro_packing_audit, "decode", return_value=new) as decode_new, \
                 patch.object(module, "decode_deferred_binding", side_effect=AssertionError("wrong report")), \
                 redirect_stdout(io.StringIO()):
                self.assertTrue(module.execute(args, paths))
            self.assertEqual(decode_old.call_count, 2)
            decode_new.assert_called_once_with("audit", inv)
            summary = json.loads((args.output / "summary.json").read_text())
            self.assertEqual(summary["macro_packing"], new)
            self.assertIsNotNone(summary["baseline_audit"])
            self.assertEqual(summary["baseline"], module.PACKING_REFERENCE)

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

    def test_binary_interface_is_measured_without_binding_or_audit_reports(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            args.experiment_define = "OPFORGE_BINARY_MACRO_VIEW"
            args.test_binary = Path("fake-test")
            manifest_path = args.images / "images.json"
            manifest = json.loads(manifest_path.read_text())
            for image in manifest["images"]:
                image["native_source_fnv1a64"] = image["file"].split("-")[0] + "-source"
                if image["file"].startswith("deferred-"):
                    image["defines"] = [args.experiment_define if define ==
                        "OPFORGE_DEFERRED_MACRO_PLANS" else define for define in image["defines"]]
            manifest_path.write_text(json.dumps(manifest))
            paths = module.prepare(args)
            for path in paths:
                case = json.loads(path.read_text())
                self.assertTrue(case["metadata"]["macro_interface_fixed_input_comparison"])
                self.assertFalse(case["metadata"]["macro_binding_fixed_input_comparison"])
                role = "deferred" if case["metadata"]["comparison_role"] == "audit" else "reference"
                self.assertEqual(case["metadata"]["bootstrap_source_fnv1a64"], role + "-source")
                self.assertEqual(args.experiment_define in case["bootstrap_defines"],
                                 case["metadata"]["comparison_role"] == "audit")
                self.assertEqual(case["files"], json.loads(paths[0].read_text())["files"])
                result = path.parent / "result"
                result.mkdir()
                (result / "native-result.json").write_text(json.dumps({
                    "success": True, "metadata": case["metadata"],
                    "start_to_done_host_seconds": 1.0, "linked_static_reserved_bytes": 12}))
                if case["metadata"]["measurement"] == "memory":
                    (result / "memory.bin").write_bytes(b"")
            with patch.object(module.subprocess, "run", return_value=SimpleNamespace(returncode=0)), \
                 patch.object(module.memory_telemetry, "decode_memory_telemetry", return_value={
                     "diagnostics": [], "peak_owned_bytes": 20, "total_allocated_bytes": 30}), \
                 patch.object(module, "decode_deferred_binding", side_effect=AssertionError("wrong report")), \
                 patch.object(module, "decode_deferred_acquisition", side_effect=AssertionError("wrong report")), \
                 patch.object(module.lazy_preparation_audit, "decode", side_effect=AssertionError("wrong report")):
                self.assertTrue(module.execute(args, paths))
            for row in json.loads((args.output / "results.json").read_text()):
                self.assertNotIn("audit", row)
                self.assertNotIn("deferred_binding", row)

    def test_stop_on_failure_retains_result_without_running_remaining_samples(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            args.test_binary = Path("fake-test")
            args.stop_on_failure = True
            paths = module.prepare(args)
            with patch.object(module.subprocess, "run", return_value=SimpleNamespace(returncode=1)) as run:
                self.assertFalse(module.execute(args, paths))
            self.assertEqual(run.call_count, 1)
            self.assertFalse((args.output / "summary.json").exists())
            self.assertEqual(len(json.loads((args.output / "results.json").read_text())), 1)

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
                            "progress p=0006ddd0 f=00000008 l=00000005 r=00000003 m=00000000\n"
                            "progress p=0006ddd1 f=00000004 l=00000002 r=00000000 m=00000000\n")
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

    def test_macro_candidate_uses_deferred_stats_without_audit_or_binding_label(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            args.experiment_define = "OPFORGE_DEFERRED_MACRO_CANDIDATES"
            manifest_path = args.images / "images.json"
            manifest = json.loads(manifest_path.read_text())
            for image in manifest["images"]:
                image["defines"] = [args.experiment_define if define ==
                    "OPFORGE_DEFERRED_MACRO_PLANS" else define for define in image["defines"]]
                if image["file"].startswith(("reference-", "deferred-")):
                    image["defines"].extend(["OPFORGE_DEFERRED_MACRO_PLANS", "OPFORGE_BINARY_MACRO_VIEW"])
            manifest_path.write_text(json.dumps(manifest))
            args.test_binary = Path("fake-test")
            paths = module.prepare(args)
            for case in paths:
                result_dir = case.parent / "result"
                result_dir.mkdir()
                spec = json.loads(case.read_text())
                metadata = spec["metadata"]
                self.assertTrue(metadata["macro_candidate_fixed_input_comparison"])
                self.assertFalse(metadata["macro_binding_fixed_input_comparison"])
                self.assertFalse(metadata["macro_interface_fixed_input_comparison"])
                self.assertIn("OPFORGE_DEFERRED_MACRO_PLANS", spec["bootstrap_defines"])
                self.assertIn("OPFORGE_BINARY_MACRO_VIEW", spec["bootstrap_defines"])
                self.assertEqual(args.experiment_define in spec["bootstrap_defines"],
                                 metadata["comparison_role"] == "audit")
                self.assertEqual(metadata["reference_config"],
                                 "eager candidate acquisition with deferred macro binding and binary macro view")
                receipt = {"success": True, "metadata": metadata, "start_to_done_host_seconds": 1.0,
                           "linked_static_reserved_bytes": 12}
                (result_dir / "native-result.json").write_text(json.dumps(receipt))
                if metadata["measurement"] == "memory":
                    (result_dir / "memory.bin").write_bytes(b"")
                    if metadata["comparison_role"] == "audit":
                        (result_dir / "assembly.stdout").write_text(
                            "progress p=0006ddd0 f=00000008 l=00000005 r=00000003 m=00000000\n"
                            "progress p=0006ddd1 f=00000004 l=00000002 r=00000000 m=00000000\n"
                            "progress p=0006ddd2 f=00000003 l=00000002 r=00000000 m=00000001\n")
            with patch.object(module.subprocess, "run", return_value=SimpleNamespace(returncode=0)), \
                 patch.object(module.memory_telemetry, "decode_memory_telemetry", return_value={
                     "diagnostics": [], "peak_owned_bytes": 20, "total_allocated_bytes": 30}):
                self.assertTrue(module.execute(args, paths))
            saved = json.loads((paths[-1].parent / "result.json").read_text())
            self.assertTrue(saved["success"])
            self.assertEqual(saved["deferred_acquisition"], {"created": 3, "bound": 2})
            self.assertEqual(json.loads((args.output / "summary.json").read_text())["deferred_acquisition"],
                             {"created": 3, "bound": 2})
            self.assertNotIn("audit", saved)
            self.assertEqual(saved["deferred_binding"], {
                "created": 8, "bound": 5, "hits": 3, "recipe_bytes": 4,
                "normal_plan_used": 2})
            self.assertTrue((paths[-1].parent / "result" / "assembly.stdout").exists())

    def test_macro_lines_keep_prior_features_and_decode_all_deferred_stats(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            args.experiment_define = "OPFORGE_DEFERRED_MACRO_LINES"
            manifest_path = args.images / "images.json"
            manifest = json.loads(manifest_path.read_text())
            for image in manifest["images"]:
                image["defines"] = [args.experiment_define if define ==
                    "OPFORGE_DEFERRED_MACRO_PLANS" else define for define in image["defines"]]
                if image["file"].startswith(("reference-", "deferred-")):
                    image["defines"].extend(["OPFORGE_DEFERRED_MACRO_PLANS", "OPFORGE_BINARY_MACRO_VIEW", "OPFORGE_DEFERRED_MACRO_CANDIDATES"])
            manifest_path.write_text(json.dumps(manifest))
            args.test_binary = Path("fake-test")
            paths = module.prepare(args)
            for case in paths:
                result_dir = case.parent / "result"
                result_dir.mkdir()
                spec = json.loads(case.read_text())
                metadata = spec["metadata"]
                self.assertTrue(metadata["macro_line_fixed_input_comparison"])
                self.assertFalse(metadata["macro_candidate_fixed_input_comparison"])
                self.assertFalse(metadata["macro_binding_fixed_input_comparison"])
                self.assertFalse(metadata["macro_interface_fixed_input_comparison"])
                self.assertIn("OPFORGE_DEFERRED_MACRO_PLANS", spec["bootstrap_defines"])
                self.assertIn("OPFORGE_BINARY_MACRO_VIEW", spec["bootstrap_defines"])
                self.assertIn("OPFORGE_DEFERRED_MACRO_CANDIDATES", spec["bootstrap_defines"])
                self.assertEqual(args.experiment_define in spec["bootstrap_defines"],
                                 metadata["comparison_role"] == "audit")
                self.assertEqual(metadata["reference_config"],
                                 "eager whole-line construction with deferred macro binding, binary macro view and deferred candidate acquisition")
                receipt = {"success": True, "metadata": metadata, "start_to_done_host_seconds": 1.0,
                           "linked_static_reserved_bytes": 12}
                (result_dir / "native-result.json").write_text(json.dumps(receipt))
                if metadata["measurement"] == "memory":
                    (result_dir / "memory.bin").write_bytes(b"")
                    if metadata["comparison_role"] == "audit":
                        (result_dir / "assembly.stdout").write_text(
                            "progress p=0006ddd0 f=00000008 l=00000005 r=00000003 m=00000000\n"
                            "progress p=0006ddd1 f=00000004 l=00000002 r=00000000 m=00000000\n"
                            "progress p=0006ddd2 f=00000003 l=00000002 r=00000000 m=00000001\n"
                            "progress p=0006ddd3 f=00000004 l=00000003 r=00000000 m=00000001\n")
            with patch.object(module.subprocess, "run", return_value=SimpleNamespace(returncode=0)), \
                 patch.object(module.memory_telemetry, "decode_memory_telemetry", return_value={
                     "diagnostics": [], "peak_owned_bytes": 20, "total_allocated_bytes": 30}):
                self.assertTrue(module.execute(args, paths))
            saved = json.loads((paths[-1].parent / "result.json").read_text())
            self.assertTrue(saved["success"])
            self.assertEqual(saved["deferred_acquisition"], {"created": 3, "bound": 2})
            self.assertEqual(json.loads((args.output / "summary.json").read_text())["deferred_acquisition"],
                             {"created": 3, "bound": 2})
            self.assertEqual(saved["deferred_lines"], {"created": 4, "bound": 3})
            self.assertEqual(json.loads((args.output / "summary.json").read_text())["deferred_lines"],
                             {"created": 4, "bound": 3})
            self.assertNotIn("audit", saved)
            self.assertEqual(saved["deferred_binding"], {
                "created": 8, "bound": 5, "hits": 3, "recipe_bytes": 4,
                "normal_plan_used": 2})
            self.assertTrue((paths[-1].parent / "result" / "assembly.stdout").exists())

    def test_acquisition_report_requires_one_valid_bounded_row(self):
        binding = {"created": 8, "bound": 5}
        row = "progress p=0006ddd2 f=00000003 l=00000002 r=00000000 m=00000001"
        self.assertEqual(module.decode_deferred_acquisition(row, binding), {"created": 3, "bound": 2})
        self.assertEqual(module.decode_deferred_acquisition(
            row.replace("f=00000003 l=00000002", "f=00000000 l=00000000"), binding),
            {"created": 0, "bound": 0})
        for invalid in ("", row + "\n" + row, row.replace(" r=", " bad="),
                        row.replace("r=00000000", "r=00000001"),
                        row.replace("l=00000002", "l=00000004"),
                        row.replace("f=00000003", "f=00000009"),
                        row.replace("f=00000003 l=00000002", "f=00000006 l=00000006")):
            with self.subTest(invalid=invalid), self.assertRaises(ValueError):
                module.decode_deferred_acquisition(invalid, binding)

    def test_line_report_requires_one_valid_bounded_row(self):
        binding = {"created": 8, "bound": 5}
        row = "progress p=0006ddd3 f=00000003 l=00000002 r=00000000 m=00000001"
        self.assertEqual(module.decode_deferred_lines(row, binding), {"created": 3, "bound": 2})
        self.assertEqual(module.decode_deferred_lines(
            row.replace("f=00000003 l=00000002", "f=00000000 l=00000000"), binding),
            {"created": 0, "bound": 0})
        for invalid in ("", row + "\n" + row, row.replace(" r=", " bad="),
                        row.replace("r=00000000", "r=00000001"),
                        row.replace("l=00000002", "l=00000004"),
                        row.replace("f=00000003", "f=00000009"),
                        row.replace("f=00000003 l=00000002", "f=00000006 l=00000006")):
            with self.subTest(invalid=invalid), self.assertRaises(ValueError):
                module.decode_deferred_lines(invalid, binding)

    def test_deferred_report_rejects_missing_duplicate_reserved_and_bad_bounds(self):
        first = "progress p=0006ddd0 f=00000008 l=00000005 r=00000003 m=00000000"
        second = "progress p=0006ddd1 f=00000004 l=00000002 r=00000000 m=00000000"
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

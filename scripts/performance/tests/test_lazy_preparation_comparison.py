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
        for role in ("reference", "audit"):
            for mode in ("release", "memory"):
                name = f"{role}-{mode}.hunk"
                data = name.encode()
                (images / name).write_bytes(data)
                defines = list(module.MEMORY_DEFINES) if mode == "memory" else []
                if role == "audit":
                    defines.append("OPFORGE_LAZY_AUDIT")
                image_rows.append({"file": name, "defines": defines,
                                   "fnv1a64": module.runner.fnv(data), "bytes": len(data)})
        (images / "images.json").write_text(json.dumps({"images": image_rows}))
        return argparse.Namespace(workload_case=case, images=images,
                                  output=root / "results", repeats=2, test_binary=None)

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

import argparse
import importlib.util
import json
from pathlib import Path
import sys
import tempfile
import unittest
from unittest import mock

sys.path.insert(0, str(Path(__file__).parents[1]))
SPEC = importlib.util.spec_from_file_location(
    "fs_comparison", Path(__file__).parents[1] / "run_fs_uae_selfhost_comparison.py")
module = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(module)


class FullComparisonTests(unittest.TestCase):
    def test_plan_metadata_records_hardware_and_excludes_unrelated_environment(self):
        with tempfile.TemporaryDirectory() as directory:
            template = Path(directory) / "configured.fs-uae"
            template.write_text("cpu = 68040\nchip_memory = 2048\nzorro_iii_memory = 32768\nhard_drive_0 = private-drive\n")
            with mock.patch.dict(module.os.environ, {
                "OPFORGE_FS_UAE_CONFIG_TEMPLATE": str(template),
                "OPFORGE_FS_UAE_MEMORY_PROFILE": "existing",
                "OPFORGE_FS_UAE_GUEST_STACK_BYTES": "65536",
                "OPFORGE_FS_UAE_POST_START_TIMEOUT_MS": "7200000",
                "UNRELATED_SECRET": "must-not-appear",
            }, clear=True):
                record = module.reproducibility_metadata(argparse.Namespace(test_binary=None))
            self.assertEqual(record["config_template"]["hardware_settings"]["cpu"], "68040")
            self.assertEqual(record["config_template"]["sha256"], module.hashlib.sha256(template.read_bytes()).hexdigest())
            self.assertEqual(record["environment"]["OPFORGE_FS_UAE_POST_START_TIMEOUT_MS"], "7200000")
            self.assertEqual(record["environment"]["OPFORGE_FS_UAE_GUEST_STACK_BYTES"], "65536")
            serialized = json.dumps(record)
            self.assertNotIn("must-not-appear", serialized)
            self.assertNotIn("private-drive", serialized)
            self.assertEqual(record["existing_profile_zorro_iii_memory_kib"], 65536)

    def test_stages_frozen_workload_and_distinct_bootstrap_layouts(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            args = argparse.Namespace(output=root / "cases", baseline_bundle=root / "baseline",
                                      candidate_bundle=root / "candidate", baseline_source_root=root / "source",
                                      baseline_memory_bundle=root / "old-memory", candidate_memory_bundle=root / "new-memory")
            manifest = {"bootstrap_package_storage": "embedded", "bootstrap_defines": []}
            old = {"src/experimental/opforge_compact_cli.asm": b"old source", "opforge": b"old image"}
            candidate = dict(old, opforge=b"new image")
            releases = [(old, "old-case", {"comparison_role": "baseline", "workload_sha256": "workload"}),
                        (candidate, "new-case", {"comparison_role": "candidate"})]
            memories = []
            for role, size in (("baseline_memory", 64), ("candidate_memory", 56)):
                memories.append((root, {"bootstrap_defines": ["OPFORGE_CAPTURE_TELEMETRY"],
                                        "_validated_capture_record_header_bytes": size,
                                        "bootstrap_package_storage": "embedded",
                                        "embedded_packages": ["m68020--motorola68k.bin"],
                                        "output_package_storage": "embedded",
                                        "output_embedded_packages": ["m68020--motorola68k.bin"],
                                        "native_validation": "not_run"},
                                 dict(old, opforge=role.encode()), b"old oracle", role, role, {}))
            own = dict(candidate)
            own["src/experimental/opforge_compact_cli.asm"] = b"new source"
            with (mock.patch.object(module.comparison, "load_comparison", return_value=(manifest, b"old oracle", releases)),
                  mock.patch.object(module.comparison, "load_memory_cases", return_value=memories),
                  mock.patch.object(module.runner, "load_bundle", return_value=(manifest, own, b"new image", "selfhost"))):
                cases = module.prepare_cases(args)
            self.assertEqual(len(cases), 5)
            specifications = [json.loads(path.read_text()) for path in cases]
            self.assertEqual([spec["record_header_bytes"] for spec in specifications], [None, None, 64, 56, None])
            for spec in specifications[2:4]:
                telemetry = spec["telemetry_manifest"]
                self.assertEqual(telemetry["bootstrap_package_storage"], "embedded")
                self.assertEqual(telemetry["output_package_storage"], "embedded")
                self.assertEqual(telemetry["embedded_packages"], ["m68020--motorola68k.bin"])
                self.assertEqual(telemetry["output_embedded_packages"], ["m68020--motorola68k.bin"])
                self.assertEqual(telemetry["native_validation"], "not_run")
            self.assertEqual([spec["oracle"]["digest"] for spec in specifications[:4]],
                             [module.runner.fnv(b"old oracle")] * 4)
            for path in cases[:4]:
                self.assertEqual((path.parent / "inputs/src/experimental/opforge_compact_cli.asm").read_bytes(), b"old source")
            self.assertEqual((cases[4].parent / "inputs/src/experimental/opforge_compact_cli.asm").read_bytes(), b"new source")

    def test_failed_case_does_not_prevent_later_case_execution(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            paths = []
            for index in range(2):
                case = root / str(index)
                case.mkdir()
                path = case / "case.json"
                path.write_text(json.dumps({"bootstrap_defines": []}))
                paths.append(path)
            def execute(command, *, env, stdout, stderr):
                case = Path(env["OPFORGE_FULL_COMPARISON_CASE"]).parent
                if case.name == "1":
                    (case / "result").mkdir()
                    (case / "result/native-result.json").write_text(json.dumps({"success": True}))
                return mock.Mock(returncode=1 if case.name == "0" else 0)
            args = argparse.Namespace(output=root, test_binary=root / "test-binary")
            with mock.patch.object(module.subprocess, "run", side_effect=execute) as run:
                self.assertFalse(module.execute_cases(args, paths))
            self.assertEqual(run.call_count, 2)
            records = json.loads((root / "results.json").read_text())
            self.assertEqual([row["result"]["success"] for row in records], [False, True])


if __name__ == "__main__":
    unittest.main()

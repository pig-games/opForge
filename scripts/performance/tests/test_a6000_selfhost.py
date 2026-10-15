import argparse
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
import struct
import sys
from unittest import mock

sys.path.insert(0, str(Path(__file__).parents[1]))

SPEC = importlib.util.spec_from_file_location(
    "a6000_selfhost", Path(__file__).parents[1] / "run_a6000_selfhost.py"
)
runner = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(runner)
LEGACY_INSTRUMENTATION_DEFINES = runner.INSTRUMENTATION_DEFINES - {"OPFORGE_HOTSPOT_TELEMETRY"}
from test_hotspot_telemetry import profile_rows, profile_text


class HardwareCompletionTests(unittest.TestCase):
    def test_host_discovery_accepts_named_ipv4_and_ignores_banner(self):
        output = "ApolloExplorer Hosts found:\n\u00a0 V4-A6000 (UniCorn) 192.168.2.4\n"
        with mock.patch.object(runner.subprocess, "run", return_value=mock.Mock(stdout=output)) as run:
            self.assertEqual(runner.resolve_host(None, "/usr/bin/ash"),
                             ("V4-A6000 (UniCorn)", "192.168.2.4"))
        run.assert_called_once_with(["/usr/bin/ash", "-l"], check=True,
                                    capture_output=True, text=True, timeout=30)

    def test_host_discovery_rejects_missing_or_ambiguous_hosts_with_override_guidance(self):
        for output, message in (
            ("ApolloExplorer Hosts found:\n", "No A6000 hosts"),
            ("First 192.168.2.4\nSecond 192.168.2.5\n", "Ambiguous A6000 hosts"),
        ):
            with self.subTest(output=output), self.assertRaisesRegex(ValueError, message) as caught:
                runner.parse_host_discovery(output)
            self.assertIn("--host", str(caught.exception))

    def test_host_discovery_deduplicates_aliases_for_same_ipv4(self):
        output = "First Name 192.168.2.4\nAlias (Room 2) 192.168.2.4\n"
        self.assertEqual(runner.parse_host_discovery(output), ("First Name", "192.168.2.4"))

    def test_host_discovery_rejects_invalid_ipv4(self):
        with self.assertRaisesRegex(ValueError, "Invalid IPv4"):
            runner.parse_host_discovery("V4-A6000 (UniCorn) 192.168.2.999\n")

    def test_explicit_host_bypasses_discovery_and_is_validated(self):
        with mock.patch.object(runner.subprocess, "run", side_effect=AssertionError("discovery ran")):
            self.assertEqual(runner.resolve_host("192.168.2.4", "/usr/bin/ash"),
                             ("192.168.2.4", "192.168.2.4"))
            self.assertEqual(runner.resolve_host("a6000.local", "/usr/bin/ash"),
                             ("a6000.local", "a6000.local"))
        self.assertFalse(runner.valid_host("192.168.2.999"))

    def test_dry_run_does_not_look_up_tools_or_discover_host(self):
        with tempfile.TemporaryDirectory() as directory:
            bundle = Path(directory) / "bundle"
            bundle.mkdir()
            self.bundle(bundle, False)
            manifest, files, oracle, case_digest = runner.load_bundle(bundle)
            manifest["over_classic_limit_components"] = []
            args = argparse.Namespace(dry_run=True, host=None, volume="Development")
            with (mock.patch.object(runner.shutil, "which", side_effect=AssertionError("tool lookup ran")),
                  mock.patch.object(runner.subprocess, "run", side_effect=AssertionError("network ran"))):
                result, _ = runner.run_case(args, bundle, manifest, files, oracle, case_digest)
            self.assertIsNone(result)
            self.assertIsNone(args.host)

    def test_hotspot_numeric_parameters_are_narrow_and_require_gate(self):
        flag = "OPFORGE_HOTSPOT_TELEMETRY"
        for period in (64, 128, 256):
            flags, config = runner.hotspot_configuration(
                [flag, f"OPFORGE_HOTSPOT_SAMPLE_PERIOD={period}", "OPFORGE_HOTSPOT_PHASE_SEED=255"])
            self.assertEqual(flags, {flag})
            self.assertEqual(config, {"sampling_period": period, "phase_seed": 255})
        self.assertEqual(runner.hotspot_configuration([flag])[1],
                         {"sampling_period": 128, "phase_seed": 0})
        bad = ["OPFORGE_HOTSPOT_SAMPLE_PERIOD=32", "OPFORGE_HOTSPOT_SAMPLE_PERIOD",
               "OPFORGE_HOTSPOT_SAMPLE_PERIOD=064", "OPFORGE_HOTSPOT_SAMPLE_PERIOD=64=128",
               "OPFORGE_HOTSPOT_PHASE_SEED=-1", "OPFORGE_HOTSPOT_PHASE_SEED=256",
               "OPFORGE_HOTSPOT_PHASE_SEED=0x1"]
        for define in bad:
            with self.subTest(define=define), self.assertRaises(ValueError):
                runner.hotspot_configuration([flag, define])
        with self.assertRaisesRegex(ValueError, "require OPFORGE_HOTSPOT"):
            runner.hotspot_configuration(["OPFORGE_HOTSPOT_PHASE_SEED=1"])
        with self.assertRaisesRegex(ValueError, "duplicate"):
            runner.hotspot_configuration([flag, "OPFORGE_HOTSPOT_PHASE_SEED=1",
                                          "OPFORGE_HOTSPOT_PHASE_SEED=2"])

    def test_hotspot_bundle_requires_report_and_excludes_dense_flags(self):
        base = {"OPFORGE_DEBUG_CONTRACTS", "OPFORGE_MEMORY_TELEMETRY",
                "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT", "OPFORGE_PREPARATION_PROGRESS",
                "OPFORGE_HOTSPOT_TELEMETRY"}
        for removed in (None, "OPFORGE_PREPARATION_PROGRESS", "OPFORGE_DEBUG_CONTRACTS",
                        "OPFORGE_MEMORY_TELEMETRY", "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT"):
            with self.subTest(removed=removed), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.bundle(root, True)
                manifest = json.loads((root / "manifest.json").read_text())
                manifest["bootstrap_defines"] = sorted(base - ({removed} if removed else set()))
                (root / "manifest.json").write_text(json.dumps(manifest))
                if removed:
                    with self.assertRaises(ValueError):
                        runner.load_bundle(root)
                else:
                    runner.load_bundle(root)
                    for define in ("OPFORGE_HOTSPOT_SAMPLE_PERIOD=64", "OPFORGE_HOTSPOT_PHASE_SEED=255"):
                        manifest["bootstrap_defines"] = sorted(base | {define})
                        (root / "manifest.json").write_text(json.dumps(manifest))
                        runner.load_bundle(root)
                    manifest["bootstrap_defines"] = sorted(base | {"OPFORGE_UNRELATED=64"})
                    (root / "manifest.json").write_text(json.dumps(manifest))
                    with self.assertRaisesRegex(ValueError, "Unsupported bootstrap"):
                        runner.load_bundle(root)
                    for flag in runner.DENSE_TELEMETRY_DEFINES:
                        manifest["bootstrap_defines"] = sorted(base | {flag})
                        (root / "manifest.json").write_text(json.dumps(manifest))
                        with self.assertRaisesRegex(ValueError, "zero dense"):
                            runner.load_bundle(root)

    def test_hotspot_summary_required_and_clock_matches_memory(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            words = [0] * 570
            words[0], words[28] = 0x4D454D44, 1000
            words[19:28] = [0, 1, 0, 0, 1, 50, 0, 1, 100]
            # MEMD stage tick pairs start at word 30. One second at 1 kHz
            # matches the one-second preparation DateStamp interval above.
            words[30:32] = [0, 1000]
            (root / "memory.bin").write_bytes(struct.pack(">570I", *words))
            manifest = {"bootstrap_defines": ["OPFORGE_HOTSPOT_TELEMETRY"]}
            for text, expected in ((None, False), ("no profile", False),
                                   (profile_text(profile_rows(period=128, frequency=2000)), False),
                                   (profile_text(profile_rows(period=64)), False),
                                   (profile_text(profile_rows(period=128, seed=1)), False),
                                   (profile_text(profile_rows(period=128)), True)):
                if text is not None:
                    (root / "assembly.stdout").write_text(text)
                for assembly_success in (True, False):
                    result = {"success": assembly_success}
                    runner.add_telemetry(result, root, manifest)
                    self.assertEqual(result["assembly_success"], assembly_success)
                    self.assertEqual(result["telemetry_completed"], expected)
                    self.assertEqual(result["success"], expected and assembly_success)
                    self.assertEqual(result["hotspot_summary"] is not None, expected)
            manifest["bootstrap_defines"] += ["OPFORGE_HOTSPOT_SAMPLE_PERIOD=64",
                                               "OPFORGE_HOTSPOT_PHASE_SEED=255"]
            (root / "assembly.stdout").write_text(profile_text(profile_rows(period=64, seed=255)))
            result = {"success": True}
            runner.add_telemetry(result, root, manifest)
            self.assertTrue(result["success"])

    def test_native_validation_status_distinguishes_candidate_failure_and_qualification(self):
        self.assertEqual(
            runner.native_validation_status({}),
            "Host-validated candidate; no native qualification.",
        )
        self.assertEqual(
            runner.native_validation_status({"native_validation": "not_run"}),
            "Host-validated candidate; no native qualification.",
        )
        self.assertEqual(
            runner.native_validation_status({"native_validation": "fresh_fs_uae_failure_capture"}),
            "Native failed candidate (fresh FS-UAE capture).",
        )
        qualified = {
            "native_validation": "fresh_fs_uae_exact_hunk",
            "native_protocol_completed": True,
            "native_assembler_exit_code": 0,
            "native_start_to_done_host_seconds": 3512.2,
        }
        status = runner.native_validation_status(qualified)
        self.assertIn("this exact bundle", status)
        self.assertIn("physical A6000 validation pending", status)

    def test_load_bundle_rejects_inconsistent_or_unknown_native_validation(self):
        bad_claims = [
            {"native_validation": "unknown"},
            {"native_validation": "fresh_fs_uae_exact_hunk", "native_protocol_completed": False,
             "native_assembler_exit_code": 0, "native_start_to_done_host_seconds": 1},
            {"native_validation": "fresh_fs_uae_exact_hunk", "native_protocol_completed": True,
             "native_assembler_exit_code": True, "native_start_to_done_host_seconds": 1},
            {"native_validation": "fresh_fs_uae_exact_hunk", "native_protocol_completed": True,
             "native_assembler_exit_code": 1, "native_start_to_done_host_seconds": 1},
            {"native_validation": "fresh_fs_uae_exact_hunk", "native_protocol_completed": True,
             "native_assembler_exit_code": 0, "native_start_to_done_host_seconds": -1},
            {"native_validation": "fresh_fs_uae_exact_hunk", "native_protocol_completed": True,
             "native_assembler_exit_code": 0, "native_start_to_done_host_seconds": float("inf")},
        ]
        for claim in bad_claims:
            with self.subTest(claim=claim), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.bundle(root, False)
                manifest = json.loads((root / "manifest.json").read_text())
                manifest.update(claim)
                (root / "manifest.json").write_text(json.dumps(manifest))
                with self.assertRaisesRegex(ValueError, "native validation"):
                    runner.load_bundle(root)

    def test_hardware_result_metadata_includes_bundle_native_validation(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            manifest = {"native_validation": "fresh_fs_uae_failure_capture"}
            result = {"success": True}
            runner.add_telemetry(result, root, manifest)
            self.assertEqual(
                result["bundle_prior_native_validation"],
                "Native failed candidate (fresh FS-UAE capture).",
            )

    def package(self, target=b""):
        package = bytearray(runner.PACKAGE_HEADER_BYTES)
        package[:4] = runner.PACKAGE_MAGIC
        package[62:64] = (10).to_bytes(2, "big")
        package[runner.NAMED_DS_DIRECTIVE_OFFSET:runner.NAMED_DS_DIRECTIVE_OFFSET + 2] = (4).to_bytes(2, "big")

        def append_aligned(data):
            if len(package) % 2:
                package.append(0)
            offset = len(package)
            package.extend(data)
            return offset

        package[160:164] = runner.PACKAGE_HEADER_BYTES.to_bytes(4, "big")
        package[164:168] = (0).to_bytes(4, "big")
        head_offset = append_aligned(b"HEAD")
        package[168:172] = head_offset.to_bytes(4, "big")
        package[172:176] = (4).to_bytes(4, "big")
        package[176:178] = (2).to_bytes(2, "big")
        declaration_offset = append_aligned(b"D" * 116)
        package[180:184] = declaration_offset.to_bytes(4, "big")
        package[184:188] = (116).to_bytes(4, "big")
        package[188:190] = (2).to_bytes(2, "big")
        expression_offset = append_aligned(b"E")
        package[200:204] = expression_offset.to_bytes(4, "big")
        package[204:208] = (1).to_bytes(4, "big")
        loop_offset = append_aligned(b"\x99\0\0\0\1\0\0\0\2\0\3\0\3\0\4\x83\0")
        package[212:216] = loop_offset.to_bytes(4, "big")
        package[216:220] = (17).to_bytes(4, "big")
        branch_plan = bytearray((0x9A, 8))
        for index in range(8):
            branch_plan.extend((index).to_bytes(2, "big"))
            branch_plan.extend((index + 1, int(index in (0, 1, 4, 5))))
        branch_plan.extend((0x83, 0))
        branch_offset = append_aligned(branch_plan)
        package[runner.BRANCH_PLAN_OFFSET:runner.BRANCH_PLAN_OFFSET + 4] = branch_offset.to_bytes(4, "big")
        package[runner.BRANCH_PLAN_BYTES_OFFSET:runner.BRANCH_PLAN_BYTES_OFFSET + 4] = len(branch_plan).to_bytes(4, "big")
        text_offset = append_aligned(b"T" * runner.TEXT_PLAN_BYTES)
        package[runner.TEXT_PLAN_OFFSET:runner.TEXT_PLAN_OFFSET + 4] = text_offset.to_bytes(4, "big")
        package[runner.TEXT_PLAN_BYTES_OFFSET:runner.TEXT_PLAN_BYTES_OFFSET + 4] = runner.TEXT_PLAN_BYTES.to_bytes(4, "big")
        if target:
            target_offset = append_aligned(target)
            package[124:128] = target_offset.to_bytes(4, "big")
            package[128:130] = len(target).to_bytes(2, "big")
        if len(package) % 2:
            package.append(0)
        runtime_bytes = len(package)
        for header_offset, program in (
            (runner.STATEMENT_COMPILE_OFFSET, b"COMPILE"),
            (runner.STATEMENT_MATCH_OFFSET, b"MATCH"),
            (runner.STATEMENT_HEAD_OFFSET, b"HEADS"),
        ):
            program_offset = append_aligned(program)
            package[header_offset:header_offset + 4] = program_offset.to_bytes(4, "big")
            package[header_offset + 4:header_offset + 8] = len(program).to_bytes(4, "big")
        if len(package) % 2:
            package.append(0)
        package[4:8] = len(package).to_bytes(4, "big")
        package[72:76] = runtime_bytes.to_bytes(4, "big")
        return bytes(package)

    def bundle(self, root, instrumented):
        source = root / "original"
        source.mkdir()
        (source / "entry.asm").write_bytes(b"source")
        (root / "src").mkdir()
        (root / "src/entry.asm").write_bytes(b"source")
        oracle = b"release"
        bootstrap = b"profile" if instrumented else oracle
        package = self.package()
        command = "opforge --runtime-package p.bin -i src/entry.asm --hunk output.hunk"
        manifest = {
            "release_defines": [],
            "filename_mapping": "identity; source include literals are unchanged",
            "classic_filename_compatible": True,
            "source_root": str(source),
            "source_manifest_digest": runner.fnv(b"entry.asm\0source\0"),
            "source_mapping": [{"staged_path": "src/entry.asm", "logical_path": "entry.asm",
                                "bytes": 6, "digest": runner.fnv(b"source")}],
            "release_hunk_digest": runner.fnv(oracle),
            "bootstrap_hunk_digest": runner.fnv(bootstrap),
            "bootstrap_defines": sorted(LEGACY_INSTRUMENTATION_DEFINES) if instrumented else [],
            "telemetry_file": "memory.bin" if instrumented else None,
            "runtime_package_digest": runner.fnv(package), "command": command,
        }
        for name, data in {"opforge": bootstrap, "oracle.hunk": oracle, "p.bin": package,
                           "command.txt": command.encode(),
                           "manifest.json": json.dumps(manifest).encode()}.items():
            (root / name).write_bytes(data)

    def generated_configuration_bundle(self, root):
        self.bundle(root, False)
        logical = "experimental/build_configuration.i"
        original = root / "original" / logical
        staged = root / "src" / logical
        original.parent.mkdir(parents=True)
        staged.parent.mkdir(parents=True)
        original.write_bytes(b"")
        data = (
            b"DEFER_MACRO_PLANS = 1\n"
            b"BINARY_MACRO_VIEW = 0\n"
            b"ACQUIRE_MACRO_CANDIDATES = 0\n"
            b"DEFER_MACRO_LINES = 0\n"
        )
        staged.write_bytes(data)
        manifest = json.loads((root / "manifest.json").read_text())
        manifest["source_mapping"].append({
            "logical_path": logical, "staged_path": "src/" + logical,
            "origin": "generated_build_configuration", "bytes": len(data),
            "digest": runner.fnv(data),
        })
        manifest["source_configuration"] = {
            "file": "src/" + logical,
            "symbol": "DEFER_MACRO_PLANS",
            "value": 1,
        }
        manifest["source_manifest_digest"] = runner.fnv(
            b"entry.asm\0source\0" + logical.encode() + b"\0" + data + b"\0")
        (root / "manifest.json").write_text(json.dumps(manifest))

    def test_generated_build_configuration_is_exact_and_explicit(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.generated_configuration_bundle(root)
            manifest, files, oracle, _ = runner.load_bundle(root)
            self.assertEqual(manifest["release_defines"], [])
            self.assertEqual(manifest["bootstrap_defines"], [])
            self.assertEqual(files["src/experimental/build_configuration.i"],
                             b"DEFER_MACRO_PLANS = 1\n"
                             b"BINARY_MACRO_VIEW = 0\n"
                             b"ACQUIRE_MACRO_CANDIDATES = 0\n"
                             b"DEFER_MACRO_LINES = 0\n")
            self.assertEqual((root / "opforge").read_bytes(), oracle)

    def test_generated_build_configuration_rejects_invalid_metadata_and_origin(self):
        changes = [
            (lambda manifest, row: manifest["source_configuration"].update(extra=1)),
            (lambda manifest, row: manifest["source_configuration"].update(symbol="OTHER")),
            (lambda manifest, row: manifest["source_configuration"].update(file="src/other.i")),
            (lambda manifest, row: manifest["source_configuration"].update(value=True)),
            (lambda manifest, row: manifest["source_configuration"].update(value=2)),
            (lambda manifest, row: manifest.update(source_configuration=None)),
            (lambda manifest, row: manifest.update(source_configuration="unknown")),
            (lambda manifest, row: row.pop("origin")),
            (lambda manifest, row: manifest["source_mapping"].append(dict(row))),
            (lambda manifest, row: row.update(staged_path="src/other.i")),
            (lambda manifest, row: row.update(bytes=row["bytes"] + 1)),
        ]
        for change in changes:
            with self.subTest(change=change), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.generated_configuration_bundle(root)
                manifest = json.loads((root / "manifest.json").read_text())
                row = manifest["source_mapping"][-1]
                change(manifest, row)
                (root / "manifest.json").write_text(json.dumps(manifest))
                with self.assertRaises(ValueError):
                    runner.load_bundle(root)

    def test_generated_build_configuration_rejects_changed_source_bytes(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.generated_configuration_bundle(root)
            manifest = json.loads((root / "manifest.json").read_text())
            row = manifest["source_mapping"][-1]
            data = (
                b"DEFER_MACRO_PLANS = 1\n"
                b"BINARY_MACRO_VIEW = 0\n"
                b"ACQUIRE_MACRO_CANDIDATES = 0\n"
                b"DEFER_MACRO_LINES = 2\n"
            )
            (root / row["staged_path"]).write_bytes(data)
            row["digest"] = runner.fnv(data)
            row["bytes"] = len(data)
            (root / "manifest.json").write_text(json.dumps(manifest))
            with self.assertRaisesRegex(ValueError, "Current source differs"):
                runner.load_bundle(root)

    def test_capture_layout_is_bound_to_validated_bootstrap_sources(self):
        for header_longs, expected in ((14, 56), (16, 64)):
            with self.subTest(header_bytes=expected), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.bundle(root, True)
                source = ("Record .struct\n" + "".join(
                    f"Field{i} .long ?\n" for i in range(header_longs)) +
                    "Tokens .byte ?\n.endstruct\nHEADER_BYTES = Record.Tokens\n").encode()
                logical = "experimental/binary_capture.asm"
                for base in (root / "original", root / "src"):
                    (base / "experimental").mkdir()
                    (base / logical).write_bytes(source)
                manifest = json.loads((root / "manifest.json").read_text())
                manifest["source_mapping"].append({
                    "staged_path": "src/" + logical, "logical_path": logical,
                    "bytes": len(source), "digest": runner.fnv(source)})
                manifest["source_manifest_digest"] = runner.fnv(
                    b"entry.asm\0source\0" + logical.encode() + b"\0" + source + b"\0")
                manifest["_validated_capture_record_header_bytes"] = 123  # Never trust persisted metadata.
                (root / "manifest.json").write_text(json.dumps(manifest))
                validated, files, _, _ = runner.load_bundle(root)
                self.assertEqual(validated["_validated_capture_record_header_bytes"], expected)
                files["src/" + logical] = b"different frozen workload"
                self.assertEqual(validated["_validated_capture_record_header_bytes"], expected)

    def embedded_bundle(self, root):
        self.bundle(root, False)
        target = b"m68020--motorola68k"
        package = self.package(target)
        bootstrap = b"embedded executable:" + package
        command = "opforge --cpu 68020 -i src/entry.asm --hunk output.hunk -M src"
        manifest = json.loads((root / "manifest.json").read_text())
        manifest.update({"bootstrap_package_storage": "embedded",
                         "embedded_packages": [target.decode() + ".bin"],
                         "output_package_storage": "external",
                         "runtime_package_file": target.decode() + ".bin",
                         "runtime_package_digest": runner.fnv(package),
                         "bootstrap_hunk_digest": runner.fnv(bootstrap),
                         "command": command})
        (root / (target.decode() + ".bin")).write_bytes(package)
        (root / "p.bin").unlink()
        (root / "opforge").write_bytes(bootstrap)
        (root / "command.txt").write_text(command)
        (root / "manifest.json").write_text(json.dumps(manifest))

    def replace_package(self, root, package):
        manifest = json.loads((root / "manifest.json").read_text())
        filename = manifest.get("runtime_package_file", "p.bin")
        (root / filename).write_bytes(package)
        manifest["runtime_package_digest"] = runner.fnv(package)
        if manifest.get("bootstrap_package_storage") == "embedded":
            bootstrap = b"embedded executable:" + package
            (root / "opforge").write_bytes(bootstrap)
            manifest["bootstrap_hunk_digest"] = runner.fnv(bootstrap)
        (root / "manifest.json").write_text(json.dumps(manifest))

    def test_runtime_package_rejects_superseded_magic_and_truncated_header(self):
        for magic, size, error in ((b"BS31", 228, "package mismatch"),
                                  (b"BS32", 228, "package mismatch"),
                                  (b"BS33", 294, "package mismatch"),
                                  (b"BS34", 298, "package mismatch"),
                                  (b"BS16", 210, "package mismatch"),
                                   (b"BS17", 210, "package mismatch"),
                                   (b"BS19", 210, "package mismatch"),
                                   (b"BS21", 210, "package mismatch"),
                                   (b"BS22", 210, "package mismatch"),
                                   (b"BS25", 210, "package mismatch"),
                                   (b"BS26", 210, "package mismatch"),
                                   (b"BS27", 210, "package mismatch"),
                                   (b"BS29", 210, "package mismatch"),
                                  (b"BS35", 236, "package mismatch"),
                                  (b"BS36", 366, "package mismatch"),
                                  (b"BS37", runner.PACKAGE_HEADER_BYTES, "package mismatch"),
                                  (runner.PACKAGE_MAGIC, runner.PACKAGE_HEADER_BYTES - 1, "package header")):
            with self.subTest(magic=magic, size=size), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.bundle(root, False)
                package = bytearray(self.package()[:size])
                package[:4] = magic
                package[4:8] = len(package).to_bytes(4, "big")
                self.replace_package(root, package)
                with self.assertRaisesRegex(ValueError, error):
                    runner.load_bundle(root)

    def test_named_ds_directive_id_must_reference_a_package_name(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.bundle(root, False)
            package = bytearray(self.package())
            package[runner.NAMED_DS_DIRECTIVE_OFFSET:runner.NAMED_DS_DIRECTIVE_OFFSET + 2] = (10).to_bytes(2, "big")
            self.replace_package(root, package)
            with self.assertRaisesRegex(ValueError, "named DS directive ID"):
                runner.load_bundle(root)

    def test_statement_program_regions_may_follow_runtime_region(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.bundle(root, False)
            package = (root / "p.bin").read_bytes()
            runtime_bytes = int.from_bytes(package[72:76], "big")
            for header_offset in (
                runner.STATEMENT_COMPILE_OFFSET,
                runner.STATEMENT_MATCH_OFFSET,
                runner.STATEMENT_HEAD_OFFSET,
            ):
                program_offset = int.from_bytes(package[header_offset:header_offset + 4], "big")
                self.assertGreaterEqual(program_offset, runtime_bytes)
            runner.load_bundle(root)

    def test_current_header_rejects_invalid_regions_versions_and_reserved_words(self):
        mutations = [
            (4, 4, 209, "package header"),
            (72, 4, 198, "package region"),
            (72, 4, 0xFFFFFFFF, "package region"),
            (72, 4, 209, "package region"),
            (160, 4, 168, "member-binding table"),
            (160, 4, 201, "member-binding table"),
            (184, 4, 13, "declaration"),
            (184, 4, 23, "declaration"),
            (160, 4, 219, "member-binding table"),
            (164, 4, 6, "member-binding"),
            (164, 4, 0xFFFFFFFF, "member-binding table"),
            (200, 4, 0, "expression program"),
            (200, 4, 206, "expression program"),
            (200, 4, 257, "expression program"),
            (200, 4, 287, "expression program"),
            (204, 4, 0, "expression program"),
            (204, 4, 65536, "expression program"),
            (212, 4, 212, "loop-plan program"),
            (212, 4, 251, "loop-plan program"),
            (212, 4, 284, "loop-plan program"),
            (216, 4, 0, "loop-plan program"),
            (216, 4, 12, "loop-plan program"),
            (runner.BRANCH_PLAN_OFFSET, 4, 4, "branch-plan program"),
            (runner.BRANCH_PLAN_OFFSET, 4, 259, "branch-plan program"),
            (runner.BRANCH_PLAN_OFFSET, 4, 0xFFFFFFFF, "branch-plan program"),
            (runner.BRANCH_PLAN_BYTES_OFFSET, 4, 0, "branch-plan program"),
            (runner.BRANCH_PLAN_BYTES_OFFSET, 4, 20, "branch-plan program"),
            (runner.BRANCH_PLAN_BYTES_OFFSET, 4, 0xFFFFFFFF, "branch-plan program"),
            (runner.TEXT_PLAN_OFFSET, 4, 228, "text-plan program"),
            (runner.TEXT_PLAN_OFFSET, 4, 0xFFFFFFFF, "text-plan program"),
            (runner.TEXT_PLAN_BYTES_OFFSET, 4, 0, "text-plan program"),
            (runner.TEXT_PLAN_BYTES_OFFSET, 4, 33, "text-plan program"),
            (runner.TEXT_PLAN_BYTES_OFFSET, 4, 0xFFFFFFFF, "text-plan program"),
        ]
        for start, label in ((168, "head-policy"), (180, "declaration")):
            mutations.extend([
                (start, 4, 168, label),
                (start, 4, 201, label),
                (start, 4, 227, label),
                (start + 4, 4, 0, label),
                (start + 8, 2, 1, label),
                (start + 10, 2, 1, label),
            ])
        for header_offset, label in (
            (runner.STATEMENT_COMPILE_OFFSET, "statement-compile"),
            (runner.STATEMENT_MATCH_OFFSET, "statement-match"),
            (runner.STATEMENT_HEAD_OFFSET, "statement-head"),
        ):
            mutations.extend([
                (header_offset, 4, runner.PACKAGE_HEADER_BYTES - 2, f"{label} program"),
                (header_offset, 4, 0xFFFFFFFF, f"{label} program"),
                (header_offset + 4, 4, 0, f"{label} program"),
                (header_offset + 4, 4, 0xFFFFFFFF, f"{label} program"),
            ])
            valid_offset = int.from_bytes(self.package()[header_offset:header_offset + 4], "big")
            mutations.append((header_offset, 4, valid_offset + 1, f"{label} program"))
        for offset, value in ((0, 0xFF), (1, 0xFF), (3, 0xFF), (4, 0xFF), (5, 0xFF), (35, 0xFF)):
            mutations.append((
                int.from_bytes(self.package()[runner.BRANCH_PLAN_OFFSET:runner.BRANCH_PLAN_OFFSET + 4], "big") + offset,
                1,
                value,
                "branch-plan program",
            ))
        loop_offset = int.from_bytes(self.package()[212:216], "big")
        for offset, value in ((0, 0xFF), (1, 0xFF), (3, 0xFF), (5, 0xFF),
                              (7, 0xFF), (9, 0xFF), (11, 0xFF), (12, 0xFF)):
            mutations.append((loop_offset + offset, 1, value, "loop-plan program"))
        for offset, width, value, error in mutations:
            with self.subTest(offset=offset, value=value), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.bundle(root, False)
                package = bytearray(self.package())
                package[offset:offset + width] = value.to_bytes(width, "big")
                self.replace_package(root, package)
                with self.assertRaisesRegex(ValueError, error):
                    runner.load_bundle(root)

    def test_member_binding_reserved_word_is_checked_in_current_runtime_region(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.bundle(root, False)
            package = bytearray(self.package())
            package.extend(b"\0" * 8)
            package[4:8] = len(package).to_bytes(4, "big")
            package[72:76] = len(package).to_bytes(4, "big")
            package[160:164] = (len(package) - 8).to_bytes(4, "big")
            package[164:168] = (1).to_bytes(4, "big")
            self.replace_package(root, package)
            runner.load_bundle(root)
            package[-1] = 1
            self.replace_package(root, package)
            with self.assertRaisesRegex(ValueError, "member-binding reserved"):
                runner.load_bundle(root)

    def test_embedded_target_must_be_inside_runtime_region_after_current_header(self):
        for offset in (168, len(self.package(b"m68020--motorola68k"))):
            with self.subTest(offset=offset), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.embedded_bundle(root)
                package = bytearray(self.package(b"m68020--motorola68k"))
                package[124:128] = offset.to_bytes(4, "big")
                self.replace_package(root, package)
                with self.assertRaisesRegex(ValueError, "embedded package identity"):
                    runner.load_bundle(root)

    def source_selected_embedded_bundle(
        self, root, module_line=b"\t.module main", cpu_line=b"\t.cpu 68020",
        comments=(
            b"; Shell configuration only; packed preparation and execution belong to app.",
            b"; @opforge-owner: experimental.amigaos.compact_cli",
        ),
        preceding=None,
    ):
        self.embedded_bundle(root)
        manifest = json.loads((root / "manifest.json").read_text())
        lines = [*comments]
        if preceding is not None:
            lines.append(preceding)
        lines.append(module_line)
        if cpu_line is not None:
            lines.append(cpu_line)
        source = b"\n".join(lines) + b"\n"
        (root / "original/entry.asm").write_bytes(source)
        (root / "src/entry.asm").write_bytes(source)
        row = manifest["source_mapping"][0]
        row.update({"bytes": len(source), "digest": runner.fnv(source)})
        manifest["source_manifest_digest"] = runner.fnv(b"entry.asm\0" + source + b"\0")
        manifest["entry"] = "src/entry.asm"
        command = "opforge -i src/entry.asm --hunk output.hunk -M src"
        manifest["command"] = command
        (root / "command.txt").write_text(command)
        (root / "manifest.json").write_text(json.dumps(manifest))

    def test_source_selected_embedded_command_requires_m68020_entry_preamble(self):
        accepted = [
            {},
            {
                "module_line": b" .MODULE    Main ; selected module",
                "cpu_line": b"  .CPU   M68020 ; selected target",
                "comments": (b"; changed comment", b"", b"  ; another comment"),
            },
        ]
        for options in accepted:
            with self.subTest(options=options), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.source_selected_embedded_bundle(root, **options)
                manifest, _, _, _ = runner.load_bundle(root)
                self.assertEqual(manifest["command"], "opforge -i src/entry.asm --hunk output.hunk -M src")

        rejected = [
            {"cpu_line": b"\t.cpu 6502"},
            {"cpu_line": None},
            {"preceding": b".define EARLY 1"},
        ]
        for options in rejected:
            with self.subTest(options=options), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.source_selected_embedded_bundle(root, **options)
                with self.assertRaisesRegex(ValueError, "mapped m68020 entry preamble"):
                    runner.load_bundle(root)

    def embedded_output_bundle(self, root):
        self.embedded_bundle(root)
        manifest = json.loads((root / "manifest.json").read_text())
        package_file = manifest["runtime_package_file"]
        package = (root / package_file).read_bytes()
        original = b'.include "package_catalog.i"\n.entry native\n'
        (root / "original/experimental").mkdir()
        (root / "original/experimental/opforge_compact_cli.asm").write_bytes(original)
        sources = [
            ("experimental/opforge_compact_cli.asm", "configured_entry", original.replace(b"package_catalog.i", b"catalog.i")),
            ("experimental/catalog.i", "generated_catalog", f'.incbin "packages/{package_file}"\n'.encode()),
            (f"experimental/packages/{package_file}", "package_asset", package),
        ]
        identity = bytearray(b"entry.asm\0source\0")
        for logical, origin, data in sources:
            path = root / "src" / logical
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(data)
            manifest["source_mapping"].append({"logical_path": logical, "staged_path": "src/" + logical,
                                               "origin": origin, "bytes": len(data), "digest": runner.fnv(data)})
            identity.extend(logical.encode() + b"\0" + data + b"\0")
        oracle = (root / "opforge").read_bytes()
        (root / "oracle.hunk").write_bytes(oracle)
        manifest.update({"filename_mapping": "identity; generated inputs have explicit origins",
                         "output_package_storage": "embedded", "output_embedded_packages": [package_file],
                         "release_hunk_digest": runner.fnv(oracle), "source_manifest_digest": runner.fnv(identity)})
        (root / "manifest.json").write_text(json.dumps(manifest))

    def test_embedded_output_transfers_verified_sources_and_matches_oracle(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.embedded_output_bundle(root)
            manifest, files, oracle, case = runner.load_bundle(root)
            self.assertEqual(files["opforge"], oracle)
            asset = "src/experimental/packages/" + manifest["runtime_package_file"]
            self.assertEqual(files[asset], (root / manifest["runtime_package_file"]).read_bytes())
            self.assertNotIn("p.bin", files)
            transfer = root / "transfer"
            for name, data in files.items():
                path = transfer / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(data)
            runner.verify_files(transfer, files)
            for name, data in {"start.marker": b"START fresh", "done.marker": b"DONE fresh",
                               "exitcode": b"0", "start.time": b"10:00:00", "end.time": b"10:00:01",
                               "output.hunk": oracle}.items():
                (transfer / name).write_bytes(data)
            self.assertTrue(runner.inspect_result(transfer, files, oracle, "fresh", 1.1)["success"])
            (transfer / "output.hunk").write_bytes(oracle + b"changed")
            self.assertFalse(runner.inspect_result(transfer, files, oracle, "fresh", 1.1)["success"])
            (transfer / asset).write_bytes(b"corrupt")
            with self.assertRaisesRegex(ValueError, "Remote copy differs"):
                runner.verify_files(transfer, files)
            result = {"success": True}
            runner.add_telemetry(result, root, manifest)
            self.assertEqual(result["output_package_storage"], "embedded")
            self.assertEqual(result["output_embedded_packages"], [manifest["runtime_package_file"]])

    def test_embedded_output_rejects_invalid_origins_assets_and_catalogs(self):
        mutations = [
            ("configured_entry", "unknown", None, "origin"),
            ("configured_entry", "native", None, "Current source"),
            ("generated_catalog", None, b'.incbin "/host/package.bin"\n', "catalog"),
            ("generated_catalog", None, b'.incbin "packages/m68020--motorola68k.bin"\n' * 2, "catalog"),
            ("package_asset", None, b"BS35corrupt", "asset mismatch"),
        ]
        for origin, replacement_origin, data, error in mutations:
            with self.subTest(origin=origin, error=error), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.embedded_output_bundle(root)
                manifest = json.loads((root / "manifest.json").read_text())
                row = next(row for row in manifest["source_mapping"] if row.get("origin") == origin)
                if replacement_origin:
                    row["origin"] = replacement_origin
                if data is not None:
                    (root / row["staged_path"]).write_bytes(data)
                    row.update({"bytes": len(data), "digest": runner.fnv(data)})
                identity = bytearray()
                for item in manifest["source_mapping"]:
                    identity.extend(item["logical_path"].encode() + b"\0" + (root / item["staged_path"]).read_bytes() + b"\0")
                manifest["source_manifest_digest"] = runner.fnv(identity)
                (root / "manifest.json").write_text(json.dumps(manifest))
                with self.assertRaisesRegex(ValueError, error):
                    runner.load_bundle(root)

    def test_embedded_output_rejects_missing_duplicate_and_misplaced_mapping(self):
        for mutation in ("missing", "duplicate", "case_duplicate", "misplaced", "external"):
            with self.subTest(mutation=mutation), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.embedded_output_bundle(root)
                manifest = json.loads((root / "manifest.json").read_text())
                row = manifest["source_mapping"][-1]
                if mutation == "missing":
                    manifest["source_mapping"].pop()
                elif mutation == "duplicate":
                    manifest["source_mapping"].append(dict(row))
                elif mutation == "case_duplicate":
                    copied = dict(row)
                    copied["logical_path"] = copied["logical_path"].upper()
                    copied["staged_path"] = "src/" + copied["logical_path"]
                    path = root / copied["staged_path"]
                    path.parent.mkdir(parents=True, exist_ok=True)
                    path.write_bytes((root / row["staged_path"]).read_bytes())
                    manifest["source_mapping"].append(copied)
                elif mutation == "misplaced":
                    row["staged_path"] = "packages/" + manifest["runtime_package_file"]
                else:
                    manifest.update({"output_package_storage": "external", "output_embedded_packages": []})
                identity = bytearray()
                for item in manifest["source_mapping"]:
                    path = root / "src" / item["logical_path"]
                    identity.extend(item["logical_path"].encode() + b"\0" + path.read_bytes() + b"\0")
                manifest["source_manifest_digest"] = runner.fnv(identity)
                (root / "manifest.json").write_text(json.dumps(manifest))
                with self.assertRaises(ValueError):
                    runner.load_bundle(root)

    def test_embedded_output_rejects_missing_or_duplicate_package_in_oracle(self):
        for copies in (0, 2):
            with self.subTest(copies=copies), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.embedded_output_bundle(root)
                manifest = json.loads((root / "manifest.json").read_text())
                oracle = b"executable" + (root / manifest["runtime_package_file"]).read_bytes() * copies
                (root / "oracle.hunk").write_bytes(oracle)
                manifest.update({"release_hunk_digest": runner.fnv(oracle),
                                 "bootstrap_defines": sorted(LEGACY_INSTRUMENTATION_DEFINES), "telemetry_file": "memory.bin"})
                (root / "manifest.json").write_text(json.dumps(manifest))
                with self.assertRaisesRegex(ValueError, "release oracle"):
                    runner.load_bundle(root)

    def test_embedded_output_rejects_stale_native_dependency(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.embedded_output_bundle(root)
            (root / "original/entry.asm").write_bytes(b"new source")
            with self.assertRaisesRegex(ValueError, "Current source differs"):
                runner.load_bundle(root)

    def test_embedded_case_transfers_only_bootstrap_and_binds_its_bytes(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.embedded_bundle(root)
            manifest, files, oracle, case = runner.load_bundle(root)
            self.assertEqual(manifest["embedded_packages"], ["m68020--motorola68k.bin"])
            self.assertNotIn("p.bin", files)
            self.assertEqual(oracle, b"release")
            bootstrap = files["opforge"] + b"changed code"
            (root / "opforge").write_bytes(bootstrap)
            manifest["bootstrap_hunk_digest"] = runner.fnv(bootstrap)
            (root / "manifest.json").write_text(json.dumps(manifest))
            self.assertNotEqual(case, runner.load_bundle(root)[3])
            result = {"success": True}
            runner.add_telemetry(result, root, manifest)
            self.assertEqual(result["bootstrap_package_storage"], "embedded")

    def test_embedded_case_rejects_extra_targets_missing_payload_or_external_override(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.embedded_bundle(root)
            manifest = json.loads((root / "manifest.json").read_text())
            for changes, error in [
                ({"embedded_packages": manifest["embedded_packages"] + ["m6502--transparent.bin"]}, "exactly"),
                ({"bootstrap_package_storage": "unknown"}, "storage"),
            ]:
                (root / "manifest.json").write_text(json.dumps(manifest | changes))
                with self.assertRaisesRegex(ValueError, error):
                    runner.load_bundle(root)
            bootstrap = b"missing payload"
            (root / "opforge").write_bytes(bootstrap)
            changed = manifest | {"bootstrap_hunk_digest": runner.fnv(bootstrap)}
            (root / "manifest.json").write_text(json.dumps(changed))
            with self.assertRaisesRegex(ValueError, "exact package"):
                runner.load_bundle(root)
            (root / "opforge").write_bytes(b"embedded executable:" + (root / manifest["runtime_package_file"]).read_bytes())
            for base in (manifest["command"], "opforge -i src/entry.asm --hunk output.hunk -M src"):
                for option in (" -P src", " --cpu 6502", " -d zilog"):
                    command = base + option
                    (root / "command.txt").write_text(command)
                    (root / "manifest.json").write_text(json.dumps(manifest | {"command": command}))
                    with self.assertRaisesRegex(ValueError, "override"):
                        runner.load_bundle(root)

    def test_embedded_transfer_rejects_unexpected_external_package_files(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            files = {"opforge": b"embedded"}
            (root / "opforge").write_bytes(files["opforge"])
            runner.verify_files(root, files)
            for name in ("p.bin", "packages"):
                (root / name).write_bytes(b"external")
                with self.assertRaisesRegex(ValueError, "external package"):
                    runner.verify_files(root, files)
                (root / name).unlink()

    def test_instrumented_bootstrap_is_distinct_from_release_oracle_and_case(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.bundle(root, False)
            release_case = runner.load_bundle(root)[3]
            manifest = json.loads((root / "manifest.json").read_text())
            manifest.update({"bootstrap_defines": sorted(LEGACY_INSTRUMENTATION_DEFINES),
                             "bootstrap_hunk_digest": runner.fnv(b"profile"),
                             "telemetry_file": "memory.bin"})
            (root / "manifest.json").write_text(json.dumps(manifest))
            (root / "opforge").write_bytes(b"profile")
            _, files, oracle, case = runner.load_bundle(root)
            self.assertEqual(oracle, b"release")
            self.assertEqual(files["opforge"], b"profile")
            self.assertNotIn("oracle.hunk", files)
            self.assertNotEqual(case, release_case)
            (root / "opforge").write_bytes(b"corrupt")
            with self.assertRaisesRegex(ValueError, "Bootstrap digest"):
                runner.load_bundle(root)

    def test_release_cannot_use_different_bootstrap_even_with_valid_digest(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.bundle(root, True)
            manifest = json.loads((root / "manifest.json").read_text())
            manifest.update({"bootstrap_defines": [], "telemetry_file": None})
            (root / "manifest.json").write_text(json.dumps(manifest))
            with self.assertRaisesRegex(ValueError, "Release bootstrap/oracle"):
                runner.load_bundle(root)

    def test_preexisting_output_or_telemetry_stops_before_execution(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            runner.verify_fresh_directory(root)
            for name in ("output.hunk", "memory.bin", "done.marker"):
                (root / name).write_bytes(b"old")
                with self.assertRaisesRegex(ValueError, "old result"):
                    runner.verify_fresh_directory(root)
                (root / name).unlink()

    def test_telemetry_required_but_cannot_replace_successful_assembly(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            manifest = {"bootstrap_defines": sorted(
                LEGACY_INSTRUMENTATION_DEFINES - {"OPFORGE_CAPTURE_TELEMETRY"}
            )}
            result = {"success": True}
            runner.add_telemetry(result, root, manifest)
            self.assertFalse(result["success"])
            self.assertTrue(result["assembly_success"])
            words = [0] * 570
            words[0], words[28] = 0x4D454D44, 1000
            words[19:28] = [0, 1, 0, 0, 1, 50, 0, 1, 100]
            words[31] = 1000
            for native_success, errors, accepted in [(True, 0, True), (False, 0, False), (True, 32, False)]:
                words[29] = errors
                (root / "memory.bin").write_bytes(struct.pack(">570I", *words))
                result = {"success": native_success}
                runner.add_telemetry(result, root, manifest)
                self.assertEqual(result["success"], accepted)
                self.assertEqual(result["assembly_success"], native_success)
            words[29], words[31] = 0, 1100
            (root / "memory.bin").write_bytes(struct.pack(">570I", *words))
            result = {"success": True}
            runner.add_telemetry(result, root, manifest)
            self.assertFalse(result["telemetry_completed"])

    def test_capture_summary_is_required_and_cannot_replace_artifact_proof(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            words = [0] * 570
            words[0], words[28] = 0x4D454D44, 1000
            words[19:28] = [0, 1, 0, 0, 1, 50, 0, 1, 100]
            words[31] = 1000
            (root / "memory.bin").write_bytes(struct.pack(">570I", *words))
            manifest = {"bootstrap_defines": sorted(LEGACY_INSTRUMENTATION_DEFINES)}
            # Exercise report/artifact authority with a fixed layout fixture;
            # keep live source-layout parsing separate from this fixture.
            record_fields = [f"Field{i} .long ?" for i in range(13)]
            capture_source = ("Record .struct\n" + "\n".join(record_fields) +
                              "\nLine .word ?\nCount .byte ?\nRootFile .byte ?\n"
                              "Tokens .byte ?\n.endstruct\nHEADER_BYTES = Record.Tokens\n")
            files = {"src/experimental/binary_capture.asm": capture_source.encode()}
            result = {"success": True}
            runner.add_telemetry(result, root, manifest, files)
            self.assertFalse(result["success"])
            self.assertIsNone(result["capture_ownership"])
            phases = [*range(400, 407), *range(416, 421),
                      *range(424, 429), *range(432, 437)]
            rows = []
            for phase in phases:
                f, l, r = (1, 0, 7) if phase == 400 else (0, 0, 0)
                rows.append(f"progress p={phase:08x} f={f:08x} l={l:08x} r={r:08x} m=00000000")
            (root / "assembly.stdout").write_text("\n".join(rows))
            for native_success in (True, False):
                result = {"success": native_success}
                runner.add_telemetry(result, root, manifest, files)
                self.assertTrue(result["telemetry_completed"])
                self.assertEqual(result["success"], native_success)
                self.assertEqual(result["capture_ownership"]["boundary_mask"], 7)
            (root / "assembly.stdout").write_text("\n".join(rows[:-1]))
            result = {"success": True}
            runner.add_telemetry(result, root, manifest, files)
            self.assertFalse(result["success"])
            self.assertFalse(result["telemetry_completed"])

    def test_capture_bundle_requires_its_report_gate_before_transfer(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.bundle(root, True)
            manifest = json.loads((root / "manifest.json").read_text())
            manifest["bootstrap_defines"] = [
                "OPFORGE_DEBUG_CONTRACTS", "OPFORGE_MEMORY_TELEMETRY",
                "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT", "OPFORGE_CAPTURE_TELEMETRY",
            ]
            (root / "manifest.json").write_text(json.dumps(manifest))
            with self.assertRaisesRegex(ValueError, "requires preparation progress"):
                runner.load_bundle(root)

    def test_incompatible_filename_bundle_stops_before_transfer(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "manifest.json").write_text(json.dumps({
                "release_defines": [],
                "filename_mapping": "identity; source include literals are unchanged",
                "classic_filename_compatible": False,
            }))
            with self.assertRaisesRegex(ValueError, "filenames over 30 bytes"):
                runner.load_bundle(root)

    def result(self, *, rc=0, marker="fresh case", output=b"oracle", missing=False):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for name, data in {
                "input": b"source",
                "start.marker": f"START {marker}".encode(),
                "done.marker": f"DONE {marker}".encode(),
                "exitcode": str(rc).encode(),
                "start.time": b"Thursday 01-Oct-26 23:59:58",
                "end.time": b"Friday 02-Oct-26 00:00:03",
                "output.hunk": output,
            }.items():
                (root / name).write_bytes(data)
            if missing:
                (root / "input").unlink()
            return runner.inspect_result(root, {"input": b"source"}, b"oracle", "fresh case", 5.5)

    def test_exact_completed_case_has_guest_time_across_midnight(self):
        result = self.result()
        self.assertTrue(result["success"])
        self.assertEqual(result["guest_overall_seconds"], 5)

    def test_launcher_success_cannot_hide_assembler_failure_or_mismatch(self):
        self.assertFalse(self.result(rc=20)["success"])
        self.assertFalse(self.result(output=b"different")["success"])

    def test_stale_markers_or_incomplete_copy_cannot_prove_completion(self):
        for options in ({"marker": "old case"}, {"missing": True}):
            with self.subTest(options=options), self.assertRaises(ValueError):
                self.result(**options)

    def test_guest_clock_failure_is_not_reported_as_zero(self):
        self.assertIsNone(runner.guest_seconds("no clock", "no clock", 5))
        self.assertIsNone(runner.guest_seconds("10:00:00", "09:00:00", 5))

    def test_guest_script_preserves_exit_before_date_and_binds_markers(self):
        script = runner.guest_script("Development:opforge-fresh", "fresh case", "opforge --runtime-package p.bin -i src/entry.asm --hunk output.hunk")
        self.assertIn("Echo $RC >exitcode\nC:Date >end.time", script)
        self.assertIn('Echo "START fresh case"', script)
        self.assertIn('Echo "DONE fresh case"', script)
        self.assertLessEqual(max(map(len, script.splitlines())), 255)

    def test_invocation_uses_classic_shell_redirection_without_extra_arguments(self):
        command = "opforge --runtime-package p.bin -i src/entry.asm --hunk output.hunk -M src -I src/debug"
        script = runner.guest_script("Development:opforge-fresh", "fresh case", command)
        self.assertIn("opforge >assembly.stdout --runtime-package p.bin -i src/entry.asm --hunk output.hunk -M src -I src/debug\n", script)
        self.assertNotIn("*>", script)
        self.assertIn("If EXISTS C:CPU\nC:CPU >>environment.txt\nEndIf\n", script)


if __name__ == "__main__":
    unittest.main()

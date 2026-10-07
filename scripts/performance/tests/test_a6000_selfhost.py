import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
import struct
import sys

sys.path.insert(0, str(Path(__file__).parents[1]))

SPEC = importlib.util.spec_from_file_location(
    "a6000_selfhost", Path(__file__).parents[1] / "run_a6000_selfhost.py"
)
runner = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(runner)


class HardwareCompletionTests(unittest.TestCase):
    def package(self, target=b""):
        package = bytearray(212)
        package[:4] = b"BS31"
        package[168:172] = (212).to_bytes(4, "big")
        package[172:176] = (4).to_bytes(4, "big")
        package[176:178] = (2).to_bytes(2, "big")
        package.extend(b"\0" * 4)
        package[180:184] = (216).to_bytes(4, "big")
        package[184:188] = (23).to_bytes(4, "big")
        package[188:190] = (2).to_bytes(2, "big")
        package.extend(b"\0" * 24)
        package[160:164] = (212).to_bytes(4, "big")
        package[200:204] = len(package).to_bytes(4, "big")
        package[204:208] = (1).to_bytes(4, "big")
        package.extend(b"\0\0")
        if target:
            package[124:128] = len(package).to_bytes(4, "big")
            package[128:130] = len(target).to_bytes(2, "big")
            package.extend(target)
            if len(package) % 2:
                package.append(0)
        package[4:8] = len(package).to_bytes(4, "big")
        package[72:76] = len(package).to_bytes(4, "big")
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
            "bootstrap_defines": sorted(runner.INSTRUMENTATION_DEFINES) if instrumented else [],
            "telemetry_file": "memory.bin" if instrumented else None,
            "runtime_package_digest": runner.fnv(package), "command": command,
        }
        for name, data in {"opforge": bootstrap, "oracle.hunk": oracle, "p.bin": package,
                           "command.txt": command.encode(),
                           "manifest.json": json.dumps(manifest).encode()}.items():
            (root / name).write_bytes(data)

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
        for magic, size, error in ((b"BS16", 210, "package mismatch"),
                                   (b"BS17", 210, "package mismatch"),
                                   (b"BS19", 210, "package mismatch"),
                                   (b"BS21", 210, "package mismatch"),
                                   (b"BS22", 210, "package mismatch"),
                                   (b"BS25", 210, "package mismatch"),
                                   (b"BS26", 210, "package mismatch"),
                                   (b"BS27", 210, "package mismatch"),
                                   (b"BS29", 210, "package mismatch"),
                                   (b"BS31", 211, "package header")):
            with self.subTest(magic=magic, size=size), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.bundle(root, False)
                package = bytearray(self.package()[:size])
                package[:4] = magic
                package[4:8] = len(package).to_bytes(4, "big")
                self.replace_package(root, package)
                with self.assertRaisesRegex(ValueError, error):
                    runner.load_bundle(root)

    def test_current_header_rejects_invalid_regions_versions_and_reserved_words(self):
        mutations = [
            (4, 4, 209, "package header"),
            (72, 4, 198, "package region"),
            (72, 4, 244, "package region"),
            (72, 4, 209, "package region"),
            (160, 4, 168, "member-binding table"),
            (160, 4, 201, "member-binding table"),
            (184, 4, 13, "declaration"),
            (160, 4, 219, "member-binding table"),
            (164, 4, 4, "member-binding table"),
            (164, 4, 0xFFFFFFFF, "member-binding table"),
            (200, 4, 0, "expression program"),
            (200, 4, 206, "expression program"),
            (200, 4, 241, "expression program"),
            (200, 4, 242, "expression program"),
            (204, 4, 0, "expression program"),
            (204, 4, 65536, "expression program"),
            (204, 4, 3, "expression program"),
        ]
        for start, label in ((168, "head-policy"), (180, "declaration")):
            mutations.extend([
                (start, 4, 168, label),
                (start, 4, 201, label),
                (start, 4, 242, label),
                (start + 4, 4, 0, label),
                (start + 8, 2, 1, label),
                (start + 10, 2, 1, label),
            ])
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
        for offset, runtime in ((168, 242), (242, 242)):
            with self.subTest(offset=offset, runtime=runtime), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                self.embedded_bundle(root)
                package = bytearray(self.package(b"m68020--motorola68k"))
                package[124:128] = offset.to_bytes(4, "big")
                package[72:76] = runtime.to_bytes(4, "big")
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
            ("package_asset", None, b"BS31corrupt", "asset mismatch"),
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
                                 "bootstrap_defines": sorted(runner.INSTRUMENTATION_DEFINES), "telemetry_file": "memory.bin"})
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
            manifest.update({"bootstrap_defines": sorted(runner.INSTRUMENTATION_DEFINES),
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
            manifest = {"bootstrap_defines": sorted(runner.INSTRUMENTATION_DEFINES)}
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

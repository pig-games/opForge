import contextlib
import io
import json
from pathlib import Path
import sys
import tempfile
import unittest
from unittest import mock

sys.path.insert(0, str(Path(__file__).parents[1]))
sys.path.insert(0, str(Path(__file__).parent))
import run_a6000_hotspots as hotspots
import test_a6000_selfhost as fixtures

runner = hotspots.runner


class A6000HotspotTests(unittest.TestCase):
    def exports(self, root, external=False):
        bundles = [root / name for name in ("baseline", "profile128", "profile256")]
        instrumented = fixtures.HardwareCompletionTests("runTest")
        for i, bundle in enumerate(bundles):
            bundle.mkdir()
            if external:
                instrumented.external_source_directed_bundle(bundle)
            else:
                instrumented.embedded_output_bundle(bundle)
            manifest = json.loads((bundle / "manifest.json").read_text())
            package_file = manifest["runtime_package_file"]
            package = (bundle / package_file).read_bytes()
            oracle = b"R" * (706916 - len(package)) + package
            bootstrap = (b"baseline" if i == 0 else f"profile{i}".encode()) + b" executable:" + package
            (bundle / "oracle.hunk").write_bytes(oracle)
            (bundle / "opforge").write_bytes(bootstrap)
            common = {"OPFORGE_DEBUG_CONTRACTS", "OPFORGE_MEMORY_TELEMETRY",
                      "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT", "OPFORGE_PREPARATION_PROGRESS"}
            defines = common | ({"OPFORGE_CAPTURE_TELEMETRY"} if i == 0 else {
                "OPFORGE_CAPTURE_TELEMETRY",
                "OPFORGE_HOTSPOT_TELEMETRY", f"OPFORGE_HOTSPOT_SAMPLE_PERIOD={128 if i == 1 else 256}",
                f"OPFORGE_HOTSPOT_PHASE_SEED={0 if i == 1 else 37}"})
            manifest.update({"kind": "compact-self-host-local-export", "manifest_version": 1,
                             "over_classic_limit_components": [], "bootstrap_defines": sorted(defines),
                             "telemetry_file": "memory.bin", "bootstrap_hunk_digest": runner.fnv(bootstrap),
                             "release_hunk_digest": runner.fnv(oracle),
                             "cpu": "m68020"})
            (bundle / "manifest.json").write_text(json.dumps(manifest))
        return bundles

    def test_external_runtime_package_is_kept_for_each_hotspot_profile(self):
        with tempfile.TemporaryDirectory() as directory:
            bundles = self.exports(Path(directory), external=True)
            manifest, cases = hotspots.load_hotspot_cases(bundles[0], bundles[1:])
            runtime = manifest["runtime_package_file"]
            self.assertEqual(runtime, "packages/m68020--motorola68k.bin")
            for case in cases:
                self.assertEqual(case[2][runtime], (bundles[0] / runtime).read_bytes())

    def test_rejects_changed_nonbootstrap_bytes_before_running(self):
        with tempfile.TemporaryDirectory() as directory:
            bundles = self.exports(Path(directory))
            # Keep this export valid under load_bundle while changing one staged input and its manifest.
            profile = bundles[1]
            (profile / "src/entry.asm").write_bytes(b"changed")
            (profile / "original/entry.asm").write_bytes(b"changed")
            manifest = json.loads((profile / "manifest.json").read_text())
            row = next(row for row in manifest["source_mapping"] if row["logical_path"] == "entry.asm")
            row.update(bytes=7, digest=runner.fnv(b"changed"))
            identity = bytearray()
            for source_row in manifest["source_mapping"]:
                data = (profile / source_row["staged_path"]).read_bytes()
                identity.extend(source_row["logical_path"].encode() + b"\0" + data + b"\0")
            manifest["source_manifest_digest"] = runner.fnv(identity)
            (profile / "manifest.json").write_text(json.dumps(manifest))
            with self.assertRaisesRegex(ValueError, "non-bootstrap workload"):
                hotspots.load_hotspot_cases(bundles[0], bundles[1:])

    def test_profile_must_retain_capture_instrumentation(self):
        with tempfile.TemporaryDirectory() as directory:
            bundles = self.exports(Path(directory))
            profile = bundles[1]
            manifest = json.loads((profile / "manifest.json").read_text())
            manifest["bootstrap_defines"].remove("OPFORGE_CAPTURE_TELEMETRY")
            (profile / "manifest.json").write_text(json.dumps(manifest))
            with self.assertRaisesRegex(ValueError, r"retain baseline MEMD\+CAPTURE"):
                hotspots.load_hotspot_cases(bundles[0], bundles[1:])

    def test_three_dry_runs_use_fresh_stages_and_record_configurations(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            bundles = self.exports(root)
            created = []

            def temporary_directory(prefix):
                path = root / f"{prefix}{len(created)}"
                path.mkdir()
                created.append(path)
                return str(path)

            args = ["--baseline-bundle", str(bundles[0])]
            for bundle in bundles[1:]:
                args += ["--profile-bundle", str(bundle)]
            args += ["--dry-run"]
            with mock.patch("tempfile.mkdtemp", side_effect=temporary_directory), contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(hotspots.main(args), 0)
            run_roots = [path for path in created if list(path.glob("opforge-*/run-selfhost"))]
            stages = [next(path.glob("opforge-*/")) for path in run_roots]
            markers = [runner.re.search(r'START ([^"\n]+)', (stage / "run-selfhost").read_text()).group(1)
                       for stage in stages]
            self.assertEqual(len(set(markers)), 3)
            self.assertNotEqual((stages[0] / "opforge").read_bytes(), (stages[1] / "opforge").read_bytes())
            self.assertEqual((stages[0] / "src/entry.asm").read_bytes(), b"source")
            summary = json.loads(next(path / "hotspots.json" for path in created
                                      if (path / "hotspots.json").is_file()).read_text())
            self.assertEqual([row["role"] for row in summary["runs"]],
                             ["light-baseline", "hotspot-period-128-seed-0", "hotspot-period-256-seed-37"])
            self.assertIsNone(summary["runs"][1]["result"])


if __name__ == "__main__":
    unittest.main()

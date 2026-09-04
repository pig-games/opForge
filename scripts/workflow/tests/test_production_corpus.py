"""Level A/B corpus and fail-closed ledger checks; no native execution proof."""
import copy
import importlib.util
import json
from pathlib import Path
import tempfile
from types import SimpleNamespace
import unittest
from unittest import mock


SCRIPT = Path(__file__).resolve().parents[2] / "performance/production_corpus.py"
SPEC = importlib.util.spec_from_file_location("production_corpus", SCRIPT)
corpus = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(corpus)


class ProductionCorpusTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.frozen = corpus.manifest()

    def result(self, runs=7):
        frozen = self.frozen
        return {
            "schema_version": 1, "corpus_sha256": frozen["sha256"],
            "backend": "rust-release-unprofiled", "binary_sha256": "a" * 64,
            "binary_bytes": 1, "head": "a" * 40, "host": "test host",
            "rustc": "test rustc", "cargo": "test cargo", "generator_sha256": "b" * 64,
            "package": frozen["package"], "policy": {**frozen["policy"], "retained_runs": runs},
            "build": {"command": corpus.BUILD_COMMAND, "profile": "release", "default_features": True,
                      "executable": "/test/target/release/opforge", "artifact_profile": {"test": False},
                      "cargo_lock_sha256": "d" * 64, "environment": {}},
            "selected_cases": [case["id"] for case in frozen["cases"]],
            "comparison_eligible": runs >= 7,
            "cases": [{"id": case["id"], "case_sha256": case["sha256"],
                       "argv": [*case["argv"], "--opasm-package", corpus.PACKAGE],
                       "samples_ns": [100] * runs, "statistics": corpus.summarize([100] * runs),
                       "exit_status": 0, "diagnostics": [], "complete": True,
                       "artifacts": {name: copy.deepcopy(case.get("expected_artifacts", {}).get(name, {"bytes": 1, "sha256": "c" * 64})) for name in case["outputs"]}}
                      for case in frozen["cases"]],
        }

    def test_manifest_is_deterministic_and_digest_covers_final_commands(self):
        self.assertEqual(self.frozen, corpus.manifest())
        self.assertEqual([case["id"] for case in self.frozen["cases"]], [f"B{i:02}" for i in range(1, 11)])
        for case in corpus.corpus():
            raw = {key: value for key, value in case.items() if key != "sha256"}
            self.assertEqual(case["sha256"], corpus.digest(corpus.canonical(raw)))
            for name in [*case["files"], *case["outputs"]]:
                self.assertEqual(corpus.relative_path(name), name)
        self.assertGreaterEqual(self.frozen["cases"][1]["source_bytes"], 1024 * 1024)
        self.assertIn("nested flow", self.frozen["cases"][-1]["coverage"])

    def test_valid_full_and_explicit_smoke_results(self):
        corpus.validate_result(self.result(), self.frozen)
        result = self.result(1)
        corpus.validate_result(result, self.frozen)
        result["comparison_eligible"] = True
        with self.assertRaises(ValueError):
            corpus.validate_result(result, self.frozen)

    def test_missing_duplicate_unexpected_cases_fail_closed(self):
        for edit in (lambda rows: rows.pop(), lambda rows: rows.append(copy.deepcopy(rows[0])),
                     lambda rows: rows[0].update(id="B99")):
            result = self.result()
            edit(result["cases"])
            with self.assertRaises(ValueError):
                corpus.validate_result(result, self.frozen)

    def test_malformed_evidence_rejected(self):
        edits = [lambda r: r.update(corpus_sha256="0" * 64),
                 lambda r: r.update(schema_version=True), lambda r: r.update(build={}),
                 lambda r: r["cases"][0]["artifacts"]["output.bin"].update(bytes=0, sha256=corpus.digest(b"")),
                 lambda r: r.update(package={}), lambda r: r.update(policy={}),
                 lambda r: r.update(selected_cases=[]), lambda r: r.update(backend="native"),
                 lambda r: r["cases"][0].update(complete=False),
                 lambda r: r["cases"][0].update(exit_status=False),
                 lambda r: r["cases"][0].update(diagnostics=["warning"]),
                 lambda r: r["cases"][0].update(argv=[]),
                 lambda r: r["cases"][0].update(samples_ns=[1]),
                 lambda r: r["cases"][0].update(statistics={}),
                 lambda r: r["cases"][0]["statistics"].update(runs=True),
                 lambda r: r["cases"][0].update(artifacts={}),
                 lambda r: r["cases"][0]["artifacts"]["output.bin"].update(sha256="z" * 64),
                 lambda r: r["cases"][0]["artifacts"]["output.bin"].update(bytes=True)]
        for edit in edits:
            result = self.result()
            edit(result)
            with self.assertRaises(ValueError):
                corpus.validate_result(result, self.frozen)

    def test_missing_artifact_symlink_and_unsafe_paths(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            with self.assertRaises(ValueError):
                corpus.artifact_set(root, ["missing.bin"])
            (root / "real.bin").write_bytes(b"ok")
            self.assertEqual(corpus.artifact_set(root, ["real.bin"])["real.bin"]["bytes"], 2)
            (root / "link.bin").symlink_to("real.bin")
            (root / "linkdir").symlink_to(root, target_is_directory=True)
            for name in ("link.bin", "linkdir/real.bin", "../real.bin", "/real.bin", "Work:real.bin", "a//b"):
                with self.assertRaises(ValueError):
                    corpus.artifact_set(root, [name])

    def test_invalid_samples_and_json_rejected(self):
        for samples in ([], [0], [-1], [True], [1.0]):
            with self.assertRaises(ValueError):
                corpus.summarize(samples)
        self.assertEqual(corpus.summarize([7, 1, 5, 3, 4, 6, 2]),
                         {"runs": 7, "median_ns": 4, "minimum_ns": 1, "maximum_ns": 7, "p95_ns": 7})
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "result.json"
            for text in ('{"a":1,"a":2}', '{"a":NaN}', '{"a":Infinity}'):
                path.write_text(text)
                with self.assertRaises(ValueError):
                    corpus.read_json(path)
            path.write_text(json.dumps(self.result()))
            corpus.validate_result(corpus.read_json(path), self.frozen)

    def test_emulator_config_pins_cpu_speed_without_changing_boot_mount(self):
        template = "[fs-uae]\ncpu = 68040\nuae_cpu_speed = real\nhard_drive_0 = boot\n"
        output = corpus.emulator_config(template)
        self.assertIn("cpu = 68020\n", output)
        self.assertIn("uae_cpu_speed = max\n", output)
        self.assertIn("jit_compiler = 0\n", output)
        self.assertIn("hard_drive_0 = boot\n", output)
        self.assertNotIn("68040", output)
        with self.assertRaises(ValueError):
            corpus.emulator_config(template + "uae_cpu_model = 68040\n")
        with self.assertRaises(ValueError):
            corpus.emulator_config(template + "[fs-uae]\n")

    def test_build_uses_cargo_artifact_not_an_assumed_target_path(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "Cargo.lock").write_text("test lock")
            selected = root / "configured-output/opforge"
            selected.parent.mkdir()
            selected.write_bytes(b"new executable")
            stale = root / "target/release/opforge"
            stale.parent.mkdir(parents=True)
            stale.write_bytes(b"stale executable")
            row = {"reason": "compiler-artifact", "target": {"name": "opforge", "kind": ["bin"]},
                   "executable": str(selected), "profile": {"test": False, "opt_level": "3"}}
            output = SimpleNamespace(returncode=0, stderr="", stdout=json.dumps(row))
            with mock.patch.object(corpus, "ROOT", root), mock.patch.object(corpus.subprocess, "run", return_value=output):
                binary, receipt = corpus.build_release()
                self.assertEqual(binary, selected.resolve())
                self.assertEqual(receipt["executable"], str(selected.resolve()))
                output.stdout = json.dumps(row) + "\n" + json.dumps(row)
                with self.assertRaises(ValueError):
                    corpus.build_release()


if __name__ == "__main__":
    unittest.main()

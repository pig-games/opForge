"""Exercise link resolution and staged-change isolation in temporary repositories."""
import importlib.util
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location("links", ROOT / "scripts/workflow/check_workflow_links.py")
links = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(links)
RELEASE_SPEC = importlib.util.spec_from_file_location("release", ROOT / "scripts/workflow/check_release_notes_policy.py")
release = importlib.util.module_from_spec(RELEASE_SPEC)
RELEASE_SPEC.loader.exec_module(release)


class WorkflowHelpersTests(unittest.TestCase):
    def test_clean_is_scoped_to_its_repository_despite_cwd_and_target_override(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary).resolve()
            repo = root / "repo with spaces"
            peer = root / "neighbor"
            tools = root / "tools"
            for path in (repo, peer, tools):
                path.mkdir()
            shutil.copyfile(ROOT / "Makefile", repo / "Makefile")
            (peer / "keep").write_text("unrelated work")
            command_log = root / "cargo-arguments"
            cargo = tools / "cargo"
            cargo.write_text('#!/bin/sh\nprintf "%s\\n" "$@" > "$CLEAN_COMMAND_LOG"\n')
            cargo.chmod(0o755)
            env = dict(os.environ, PATH=str(tools) + os.pathsep + os.environ["PATH"],
                       CARGO_TARGET_DIR=str(peer), CLEAN_COMMAND_LOG=str(command_log))
            result = subprocess.run(["make", "-f", str(repo / "Makefile"), "clean"],
                                    cwd=peer, env=env, capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(command_log.read_text().splitlines(), [
                "clean", "--manifest-path", str(repo / "Cargo.toml"),
                "--target-dir", str(repo / "target"), "--offline",
            ])
            self.assertEqual((peer / "keep").read_text(), "unrelated work")
            self.assertTrue((repo / "target").is_dir())
            self.assertEqual(list((repo / "target").iterdir()), [])

    def test_links_resolve_relative_paths_and_report_missing_targets(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "a b.md").touch()
            source = root / "index.md"
            source.write_text("[ok](a%20b.md#section) [missing](gone.md) [web](https://example.com)\n```\n[example](placeholder)\n```\n")
            errors = links.check(source)
            self.assertEqual(len(errors), 1)
            self.assertIn("gone.md", errors[0])

    def make_repo(self, root):
        self.git(root, "init", "-q")
        self.git(root, "config", "user.name", "Workflow Test")
        self.git(root, "config", "user.email", "workflow@example.invalid")
        target = root / "scripts/workflow/stage_and_commit.sh"
        target.parent.mkdir(parents=True)
        shutil.copyfile(ROOT / "scripts/workflow/stage_and_commit.sh", target)
        for name in ("selected file", "unrelated"):
            (root / name).write_text("original")
        self.git(root, "add", ".")
        self.git(root, "commit", "-qm", "baseline")
        return target

    def git(self, root, *args):
        return subprocess.check_output(["git", *args], cwd=root, stderr=subprocess.STDOUT)

    def test_unrelated_staged_work_is_preserved_and_rejected(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            helper = self.make_repo(root)
            (root / "unrelated").write_text("staged work")
            self.git(root, "add", "unrelated")
            (root / "selected file").write_text("checkpoint")
            before = self.git(root, "diff", "--cached")
            head = self.git(root, "rev-parse", "HEAD")
            result = subprocess.run(["bash", str(helper), "--message", "checkpoint", "selected file"], cwd=root, capture_output=True)
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(self.git(root, "diff", "--cached"), before)
            self.assertEqual(self.git(root, "rev-parse", "HEAD"), head)

    def test_tagged_release_notes_remain_protected_without_prose_receipts(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            self.make_repo(root)
            self.git(root, "tag", "v0.1.0")
            self.assertTrue(release.validate_policy(root, ["RELEASE_NOTES_v0.1.0.md"]))
            self.assertEqual(release.validate_policy(root, ["RELEASE_NOTES_v0.2.0.md"]), [])
            self.assertEqual(release.validate_policy(root, ["documentation/notes.md"]), [])

    def test_explicit_checkpoint_commits_only_selected_path(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            helper = self.make_repo(root)
            (root / "selected file").write_text("checkpoint")
            (root / "unrelated").write_text("unstaged work")
            result = subprocess.run(["bash", str(helper), "--message", "checkpoint", "selected file"], cwd=root, capture_output=True)
            self.assertEqual(result.returncode, 0, result.stderr.decode())
            self.assertEqual(self.git(root, "diff-tree", "--no-commit-id", "--name-only", "-r", "HEAD").decode().strip(), "selected file")
            self.assertEqual((root / "unrelated").read_text(), "unstaged work")


if __name__ == "__main__":
    unittest.main()

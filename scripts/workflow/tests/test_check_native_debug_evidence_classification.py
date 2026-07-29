import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_native_debug_evidence_classification import ROOT, validate


class NativeDebugEvidenceClassificationTests(unittest.TestCase):
    def write_case(self, manifest: str, marker: str = "MARKER") -> tuple[Path, Path]:
        temporary = Path(tempfile.mkdtemp())
        source = temporary / "artifact.txt"
        source.write_text(marker, encoding="utf-8")
        manifest_path = temporary / "classification.toml"
        manifest_path.write_text(manifest, encoding="utf-8")
        return temporary, manifest_path

    def manifest(self, *, level: str = "E", authority: str = "none", lifecycle: str = "permanent", deletion: str = "") -> str:
        return f'''\
[policy]
schema_version = 1
macro_artifact_parity_authority = "examples/opcore/macro_invocation_native.asm"
level_e_can_close_completion = false

[[artifacts]]
id = "one"
path = "artifact.txt"
marker = "MARKER"
proof_level = "{level}"
role = "diagnostic"
authority = "{authority}"
lifecycle = "{lifecycle}"
deletion_condition = "{deletion}"
'''

    def test_repository_classification_is_complete(self):
        self.assertEqual(validate(ROOT), [])

    def test_level_e_cannot_be_completion_authority(self):
        root, manifest = self.write_case(self.manifest(authority="focused-contract"))
        self.assertIn("Level E evidence cannot be completion authority", "\n".join(validate(root, manifest, ("one",))))

    def test_temporary_artifact_requires_deletion_condition(self):
        root, manifest = self.write_case(self.manifest(lifecycle="temporary"))
        self.assertIn("temporary evidence requires a deletion condition", "\n".join(validate(root, manifest, ("one",))))

    def test_declared_marker_must_exist_in_artifact(self):
        root, manifest = self.write_case(self.manifest(), marker="different")
        self.assertIn("classification marker is missing", "\n".join(validate(root, manifest, ("one",))))

    def test_macro_artifact_authority_is_fixed(self):
        root, manifest = self.write_case(self.manifest().replace("examples/opcore/macro_invocation_native.asm", "debug.asm"))
        self.assertIn("sole macro artifact parity authority", "\n".join(validate(root, manifest, ("one",))))


if __name__ == "__main__":
    unittest.main()

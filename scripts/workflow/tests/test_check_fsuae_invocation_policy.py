import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_fsuae_invocation_policy import validate_invocation


class FsUaePolicyTests(unittest.TestCase):
    def test_invalid_invocations_fail(self):
        cases = (
            "FS-UAE cargo test external_fs_uae_x",
            "FS-UAE OPFORGE_FS_UAE_TESTS=1 cargo test x -- --test-threads=1",
            "FS-UAE reduced fixture cargo test x -- --test-threads=1",
        )
        for text in cases:
            with self.subTest(text=text):
                self.assertTrue(validate_invocation(text))

    def test_valid_invocation_passes(self):
        text = "Known-good invocation FS-UAE cargo test x -- --nocapture --test-threads=1"
        self.assertEqual(validate_invocation(text), [])


if __name__ == "__main__":
    unittest.main()

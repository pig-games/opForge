from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from decode_assembly_work import AssemblyWorkDecodeError, decode_assembly_work


LINE = "progress p=0000012c f=00000002 l=00000039 r=00000000 m=00000000"


class AssemblyWorkTests(unittest.TestCase):
    def test_work_is_localization_and_does_not_imply_completion(self):
        self.assertEqual(decode_assembly_work("other output\n" + LINE + "\n"), {
            "scheduled_source_sweeps": 2, "selected_record_visits": 57,
            "localization_only": True,
        })
        self.assertIsNone(decode_assembly_work("progress p=00000006"))

    def test_ambiguous_or_invalid_captures_fail_closed(self):
        for capture in (LINE + "\n" + LINE, LINE.replace("r=00000000", "r=00000001"),
                        LINE.replace("f=00000002", "f=bad")):
            with self.subTest(capture=capture), self.assertRaises(AssemblyWorkDecodeError):
                decode_assembly_work(capture)


if __name__ == "__main__":
    unittest.main()

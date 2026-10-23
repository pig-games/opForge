from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from native_case_runner import hunk_segments


def words(values):
    return b"".join(value.to_bytes(4, "big") for value in values)


class HunkSegmentsTests(unittest.TestCase):
    def image(self, offset=8):
        return (words([0x3f3, 0, 1, 0, 0, 16, 0x3e9, 16])
                + bytes(range(64)) + words([0x3ec, 1, 0, offset, 0, 0x3f2]))

    def test_relocations_and_payload_are_preserved_including_odd_offsets(self):
        for offset in (8, 9):
            segment, = hunk_segments(self.image(offset))
            self.assertEqual(segment["bytes"], bytes(range(64)))
            self.assertEqual(segment["allocation_bytes"], 64)
            self.assertEqual(segment["relocations"], {offset: 0})

    def test_bss_has_allocation_without_payload(self):
        segment, = hunk_segments(words([0x3f3, 0, 1, 0, 0, 4, 0x3eb, 4, 0x3f2]))
        self.assertEqual(segment["kind"], 0x3eb)
        self.assertEqual(segment["bytes"], b"")
        self.assertEqual(segment["allocation_bytes"], 16)

    def test_invalid_or_partial_load_files_fail_closed(self):
        image = self.image()
        invalid = (b"", image[:-1], image + b"extra", image[:30], self.image(61),
                   image[:-12] + words([8, 1, 0, 8, 0, 0x3f2]))
        for data in invalid:
            with self.subTest(length=len(data)), self.assertRaises(ValueError):
                hunk_segments(data)


if __name__ == "__main__":
    unittest.main()

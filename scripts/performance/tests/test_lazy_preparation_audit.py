from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).parents[1]))
import lazy_preparation_audit as module


def report():
    rows = {700: [1, 0, 15], 701: [60516, 256, 1], 702: [1, 1, 4],
            703: [1, 1, 0], 704: [0, 0, 1], 705: [1, 1, 100],
            706: [256, 1, 0], 707: [1, 1, 256], 708: [16, 2048, 256],
            710: [1, 0, 0], 711: [0, 1, 0], 712: [262184, 4096, 16],
            713: [256, 256, 0], 714: [1, 0, 0],
            720: [0, 100, 1], 721: [100, 256, 1], 722: [1, 1, 16],
            1120: [10, 20, 7], 1121: [0, 1, 0],
            1400: [0, 100, 1], 1401: [0, 20, 0],
            1700: [10, 20, 0], 1701: [0, 80, 1], 1702: [2, 0, 10],
            1703: [20, 0, 1], 1704: [0, 1, 1],
            1800: [1, 100, 256], 1801: [256, 0, 0]}
    return rows


def stdout(rows):
    return "\n".join(f"progress p={phase:08X} f={row[0]:08X} l={row[1]:08X} r={row[2]:08X} m=0" for phase, row in sorted(rows.items()))


class DecoderTests(unittest.TestCase):
    def test_prediction_is_separate_from_actual_memory_and_guest_completion(self):
        decoded = module.decode(stdout(report()))
        self.assertTrue(decoded["observer_complete"])
        self.assertEqual(decoded["potentially_retireable_capacity_bytes"], 256)
        self.assertEqual(decoded["observer_static_table_bytes"], 322700)
        self.assertNotIn("guest_protocol_completed", decoded)
        self.assertNotIn("peak_owned_bytes", decoded)

    def test_error_and_unmatched_relationship_do_not_claim_complete_coverage(self):
        rows = report()
        rows[700][1] = 2
        rows[710] = [0, 0, 1]
        decoded = module.decode(stdout(rows))
        self.assertFalse(decoded["observer_complete"])
        self.assertFalse(decoded["ordinary_unit_reconciliation_complete"])

    def test_missing_boundaries_do_not_claim_complete_coverage(self):
        rows = report()
        rows[700][2] = 0
        decoded = module.decode(stdout(rows))
        self.assertFalse(decoded["observer_complete"])
        self.assertEqual(decoded["observed_boundaries_mask"], 0)

    def test_missing_duplicate_and_unbalanced_report_are_rejected(self):
        text = stdout(report())
        with self.assertRaisesRegex(ValueError, "Duplicate"):
            module.decode(text + "\nprogress p=000002BC f=1 l=0 r=0")
        rows = report()
        del rows[712]
        with self.assertRaisesRegex(ValueError, "Missing"):
            module.decode(stdout(rows))
        rows = report()
        rows[713][2] = 1
        with self.assertRaisesRegex(ValueError, "Inconsistent"):
            module.decode(stdout(rows))


if __name__ == "__main__":
    unittest.main()

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


class WorkDecoderTests(unittest.TestCase):
    def work_report(self):
        rows = report()
        rows.update({2000: [1, 0, 1], 2001: [100, 16, 1], 2002: [60, 4, 0], 2060: [1, 1, 0]})
        for part in range(6):
            for start in (2010, 2020, 2030, 2040, 2050):
                rows[start + part] = [1, 2, 3] if start in (2010, 2020) else [0, 0, 0]
            rows[2061 + part] = [1, 2, 3]
        for metric, value in enumerate((60, 20, 3, 4, 1, 88), 8):
            for start in (2010, 2020, 2061):
                rows[start + metric // 3][metric % 3] = value
        rows[2003] = [88, 88, 4]
        return rows

    def test_exclusive_partition_and_completion_are_distinct(self):
        rows = self.work_report()
        work = module.decode(stdout(rows))["work_attribution"]
        self.assertTrue(work["coverage_complete"])
        self.assertNotIn("guest_protocol_completed", work)
        rows[2002][2] = 1
        work = module.decode(stdout(rows))["work_attribution"]
        self.assertTrue(work["accounting_complete"])
        self.assertFalse(work["coverage_complete"])
        rows[2000][1] = 1
        self.assertFalse(module.decode(stdout(rows))["work_attribution"]["accounting_complete"])

    def test_structural_details_may_exclude_shared_and_unknown_residuals(self):
        rows = self.work_report()
        rows[2010][0] += 7
        rows[2040][0] += 3
        rows[2050][0] += 4
        decoded = module.decode(stdout(rows))["work_attribution"]
        self.assertEqual(decoded["metrics"]["total"]["bind_calls"], 8)
        self.assertEqual(decoded["details"][0]["exclusive"]["bind_calls"], 1)
        self.assertTrue(decoded["accounting_complete"])
        self.assertFalse(decoded["coverage_complete"])

    def test_rejects_missing_schema_and_nonconservation(self):
        for phase in (2001, 2003, 2015, 2055, 2066):
            rows = self.work_report()
            del rows[phase]
            with self.assertRaisesRegex(ValueError, "Missing"):
                module.decode(stdout(rows))
        rows = self.work_report()
        rows[2000][0] = 2
        with self.assertRaisesRegex(ValueError, "schema"):
            module.decode(stdout(rows))
        for phase in (2020, 2061):
            rows = self.work_report()
            rows[phase][0] += 1
            with self.assertRaisesRegex(ValueError, "conservation"):
                module.decode(stdout(rows))


if __name__ == "__main__":
    unittest.main()

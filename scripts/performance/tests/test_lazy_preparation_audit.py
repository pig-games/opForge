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

    def unknown_report(self):
        rows = self.work_report()
        rows[2060][2] = 16
        rows[2002][2] = 16
        for part in range(6):
            rows[2050 + part] = rows[2020 + part][:]
            rows[2020 + part] = [0, 0, 0]
            rows[2210 + part] = rows[2050 + part][:]
            rows[2220 + part] = [0, 0, 0]
        rows[2041][2] = rows[2051][2]
        rows[2051][2] = rows[2211][2] = rows[2062][2] = 0
        rows[2282] = [3, 2, 0]
        rows[2283] = [0, 0, 0]
        rows[2284] = [0, 0, 0]
        rows[2285] = [0, 0, 0]
        rows[2200] = [1, 3, 36]
        rows[2230] = [1, 0, 0]
        rows[2231] = [0, 0, 0]
        for index in range(4):
            start = 2270 + index * 3
            rows[start] = [1, 1, 1] if index == 0 else [0, 0, 0]
            rows[start + 1] = [100, 90, 1] if index == 0 else [0, 0, 0]
            rows[start + 2] = [5, 0, 200] if index == 0 else [0, 0, 0]
        for index, offset in enumerate((52, 8, 16)):
            start = 2240 + index * 9
            rows[start] = [offset, 1, 1]
            rows[start + 1] = [16, 100, 90]
            rows[start + 2] = [1, 5, 10]
            for part in range(6):
                rows[start + 3 + part] = rows[2061 + part][:]
        return rows

    def test_unknown_split_and_numeric_provenance_preserve_baseline(self):
        self.assertNotIn("unknown_ownership", module.decode(stdout(self.work_report()))["work_attribution"])
        rows = self.unknown_report()
        rows[2010][0] += 7
        rows[2050][0] += 7
        rows[2220][0] += 7
        work = module.decode(stdout(rows))["work_attribution"]
        ownership = work["unknown_ownership"]
        self.assertEqual(ownership["reason_marked_units"]["bind_calls"], 1)
        self.assertEqual(ownership["global_or_unassigned_residual"]["bind_calls"], 7)
        self.assertEqual(ownership["largest_contributors"][0]["capture_close"], 90)
        self.assertFalse(work["coverage_complete"])

    def test_unknown_extension_rejects_missing_split_and_false_winner(self):
        for phase in (2200, 2212, 2225, 2230, 2266):
            rows = self.unknown_report()
            del rows[phase]
            with self.assertRaisesRegex(ValueError, "Missing"):
                module.decode(stdout(rows))
        for phase in (2210, 2240, 2243):
            rows = self.unknown_report()
            rows[phase][0] += 1
            with self.assertRaises(ValueError):
                module.decode(stdout(rows))

    def path_report(self):
        rows = self.unknown_report()
        for slot in range(7):
            start = 2300 + slot * 24
            origin = rows[2242 + slot * 9][0] if slot < 3 else rows[2271 + (slot - 3) * 3][2]
            data = "src/é.asm".encode("utf-8") if origin else b""
            rows[start] = [origin, len(data), slot + 1]
            if data:
                padded = data + bytes((-len(data)) % 12)
                rows[start + 1] = [int.from_bytes(padded[index:index + 4], "big") for index in (0, 4, 8)]
        return rows

    def test_source_paths_decode_bounded_utf8_and_reject_false_provenance(self):
        rows = self.path_report()
        ownership = module.decode(stdout(rows))["work_attribution"]["unknown_ownership"]
        self.assertEqual(ownership["largest_contributors"][0]["source_path"], "src/é.asm")
        self.assertEqual(ownership["range_examples"][3]["source_path"], "")
        for phase, field, value in ((2300, 0, 2), (2300, 1, 256), (2301, 2, 1), (2301, 0, 0xff000000)):
            bad = self.path_report()
            bad[phase][field] = value
            with self.assertRaises(ValueError):
                module.decode(stdout(bad))
        for phase in (2301, 2444):
            bad = self.path_report()
            del bad[phase]
            with self.assertRaisesRegex(ValueError, "Missing"):
                module.decode(stdout(bad))

    def test_bind_contexts_conserve_calls_and_bytes_independently(self):
        rows = self.unknown_report()
        ownership = module.decode(stdout(rows))["work_attribution"]["unknown_ownership"]
        self.assertEqual(ownership["bind_contexts"]["without_unit_definition"], {"calls": 3, "bound_bytes": 2})
        for field in (0, 1):
            bad = self.unknown_report()
            bad[2282][field] += 1
            with self.assertRaisesRegex(ValueError, "context conservation"):
                module.decode(stdout(bad))

    def test_range_examples_require_matching_counts_and_numeric_cause(self):
        for phase, field, value in ((2230, 0, 0), (2271, 1, 110), (2273, 0, 1)):
            rows = self.unknown_report()
            rows[phase][field] = value
            with self.assertRaisesRegex(ValueError, "[Rr]ange example"):
                module.decode(stdout(rows))

    def test_region_cuts_are_observations_without_unknown_ownership_claim(self):
        rows = self.unknown_report()
        # A unit can begin in an earlier region and end at this segment boundary.
        # These numeric observations do not themselves assign BAD_RANGE.
        rows[2002][2] = rows[2060][2] = 0
        for part in range(6):
            rows[2020 + part] = rows[2050 + part][:]
            rows[2050 + part] = rows[2210 + part] = [0, 0, 0]
        for index in range(3):
            start = 2240 + index * 9
            rows[start][1:] = [0, 0]
            for phase in range(start + 1, start + 9):
                rows[phase] = [0, 0, 0]
        rows[2230] = [0, 1, 0]
        rows[2231] = [1, 0, 0]
        rows[2270] = rows[2271] = rows[2272] = [0, 0, 0]
        for index in (1, 3):
            start = 2270 + index * 3
            rows[start] = [1, 1, index + 1]
            rows[start + 1] = [50, 200, 1]
            rows[start + 2] = [5, 100, 200]
        work = module.decode(stdout(rows))["work_attribution"]
        observations = work["unknown_ownership"]["range_observations"]
        self.assertEqual(observations["close_at_or_beyond_region_end"], 1)
        self.assertEqual(observations["open_before_region_base"], 1)
        self.assertEqual(work["unknown_reason_mask"], 0)
        self.assertTrue(work["coverage_complete"])
        self.assertNotIn("bad_range_causes", work["unknown_ownership"])

    def test_observer_error_cannot_claim_reason_unit_split(self):
        rows = self.unknown_report()
        rows[2000][1] = 1
        with self.assertRaisesRegex(ValueError, "Invalid observer"):
            module.decode(stdout(rows))

    def test_truncated_unknown_details_cannot_claim_complete_winner_verification(self):
        rows = self.unknown_report()
        rows[2000][2] = 17
        rows[2001][2] = 16
        for index in range(1, 16):
            rows[2060 + index * 7] = [index + 1, 0, 0]
            for part in range(6):
                rows[2061 + index * 7 + part] = [0, 0, 0]
        work = module.decode(stdout(rows))["work_attribution"]
        self.assertTrue(work["detail_truncated"])
        self.assertEqual(len(work["details"]), 16)

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

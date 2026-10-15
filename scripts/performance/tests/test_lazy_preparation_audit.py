from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).parents[1]))
import lazy_preparation_audit as module
import run_lazy_preparation_comparison as comparison


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


class TemplateInventoryTests(WorkDecoderTests):
    def inventory_report(self):
        rows = self.unknown_report()
        rows[2011][2] = rows[2041][2] = 108
        rows.update({3000: [1, 0, 1], 3001: [238760, 2048, 4096], 3002: [3, 3, 1],
                     3003: [3, 2, 4], 3004: [1, 2, 3], 3005: [40, 32, 4],
                     3006: [12, 20, 40], 3007: [40, 32, 4], 3008: [12, 20, 3],
                     3009: [0, 0, 0], 3010: [0, 0, 40], 3011: [0, 0, 0],
                     3012: [0, 4, 4], 3013: [3, 2, 2], 3014: [3, 0, 0]})
        definitions = [
            [1, 1, 0, 1, 10, 1, 2, 1000, 100, 120, 24, 4, 8, 1, 3, 1, 2, 0, 1, 10, 2, 2, 3, 0],
            [1, 1, 1, 2, 20, 2, 3, 1001, 200, 200, 32, 0, 4, 1, 1, 0, 0, 0, 2, 20, 1, 0, 0, 0],
            [1, 2, 0, 0, 30, 1, 4, 0, 300, 300, 16, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]
        for index, values in enumerate(definitions):
            for part in range(8):
                rows[3100 + index * 8 + part] = values[part * 3:part * 3 + 3]
        for index, values in enumerate(([0, 1, 1, 10, 2, 1, 1, 0, 0],
                                         [2, 1, 2, 11, 1, 1, 2, 0, 0],
                                         [0, 2, 2, 20, 1, 0, 0, 0, 0])):
            for part in range(3):
                rows[20000 + index * 3 + part] = values[part * 3:part * 3 + 3]
        return rows

    def inventory(self, rows=None):
        return module.decode(stdout(self.inventory_report() if rows is None else rows))["work_attribution"]["template_inventory"]

    def test_optional_inventory_and_complete_scalar_relationships(self):
        self.assertNotIn("template_inventory", module.decode(stdout(self.unknown_report()))["work_attribution"])
        inventory = self.inventory()
        self.assertTrue(inventory["complete"])
        self.assertEqual(inventory["direct_calls"], 3)
        self.assertEqual(inventory["classification"]["never_invoked"]["rows"], [3])
        self.assertEqual(inventory["classification"]["never_invoked"]["payload_bytes"], 16)
        self.assertEqual(inventory["definitions"][1]["body_bytes"], 0)
        self.assertEqual(inventory["definitions"][1]["calls"], 1)  # Accepted empty invocation.
        self.assertEqual(inventory["unattributed_pool_bytes"]["plan_bytes"], 40)
        self.assertEqual(inventory["observed_definition_committed_bound_bytes"], 2)
        self.assertNotIn("source_path", inventory["definitions"][0])
        self.assertNotIn("guest_protocol_completed", inventory)

    def test_missing_truncated_duplicate_and_extra_rows_are_rejected(self):
        for phase in (3000, 3014, 3107, 20008):
            bad = self.inventory_report()
            del bad[phase]
            with self.assertRaisesRegex(ValueError, "Missing"):
                self.inventory(bad)
        bad = self.inventory_report()
        bad[3124] = [0, 0, 0]
        with self.assertRaisesRegex(ValueError, "Unexpected"):
            self.inventory(bad)
        text = stdout(self.inventory_report()) + "\nprogress p=00000C1C f=1 l=1 r=0"
        with self.assertRaisesRegex(ValueError, "Duplicate"):
            module.decode(text)

    def test_definition_identity_and_edge_grouping_are_unique(self):
        bad = self.inventory_report()
        for part in range(8):
            bad[3116 + part] = bad[3100 + part][:]
        with self.assertRaisesRegex(ValueError, "duplicate template definition"):
            self.inventory(bad)
        bad = self.inventory_report()
        bad[20003] = bad[20000][:]
        with self.assertRaisesRegex(ValueError, "duplicate template consumer"):
            self.inventory(bad)

    def extended_inventory_report(self, count):
        rows = self.inventory_report()
        added_bytes = (count - 3) * 184
        rows[3002][0] = rows[3008][2] = count
        rows[3005][0] += added_bytes
        rows[3007][0] += added_bytes
        rows[2011][2] += added_bytes
        rows[2041][2] += added_bytes
        for index in range(3, count):
            values = [1, index + 10, 0, 0, 0, 1, 0, 0, 0, 0, 184, 0, 0, 1] + [0] * 10
            for part in range(8):
                rows[3100 + index * 8 + part] = values[part * 3:part * 3 + 3]
        return rows

    def test_deferred_summaries_coexist_with_template_ordinal_25(self):
        # The 26th definition (zero-based ordinal 25) owns phases 3300..3307.
        rows = self.extended_inventory_report(26)
        self.assertIn(3300, rows)
        before = self.inventory(rows)
        rows.update({450000: [8, 5, 3], 450001: [4, 2, 0],
                     450002: [3, 2, 0], 450003: [4, 3, 0]})
        self.assertEqual(self.inventory(rows), before)
        text = stdout(rows).replace(" m=0", " m=00000000")
        binding = comparison.decode_deferred_binding(text, 10)
        self.assertEqual(binding, {"created": 8, "bound": 5, "hits": 3,
                                   "recipe_bytes": 4, "normal_plan_used": 2})
        self.assertEqual(comparison.decode_deferred_acquisition(text, binding), {"created": 3, "bound": 2})
        self.assertEqual(comparison.decode_deferred_lines(text, binding), {"created": 4, "bound": 3})
        # Template words at the superseded phases cannot satisfy this contract.
        with self.assertRaisesRegex(ValueError, "Missing deferred binding"):
            comparison.decode_deferred_binding(stdout(self.extended_inventory_report(26)), 10)

    def test_full_inventory_crosses_former_definition_capacity_without_phase_overlap(self):
        count = 1361
        rows = self.extended_inventory_report(count)
        inventory = self.inventory(rows)
        self.assertTrue(inventory["complete"])
        self.assertEqual(inventory["definition_count"], count)
        self.assertEqual(inventory["classification"]["never_invoked"]["definition_count"], count - 2)
        self.assertEqual(inventory["edge_count"], 3)

    def overlap_report(self):
        rows = self.inventory_report()
        rows[3000][2] = rows[3002][2] = 2
        rows[3108][0] = rows[3116][0] = 2
        rows[3116][1] = 1  # Pool/handle reused in a later epoch.
        rows[20003][0] = 1  # Recursive consumer stays within epoch 1.
        return rows

    def test_overlapping_finished_epochs_keep_distinct_owner_identities(self):
        inventory = self.inventory(self.overlap_report())
        self.assertTrue(inventory["complete"])
        self.assertEqual([row["epoch"] for row in inventory["definitions"]], [1, 2, 2])
        self.assertEqual(inventory["finished_sessions"], 2)
        self.assertEqual(inventory["calls"], 4)
        bad = self.overlap_report()
        bad[20003][0] = 2
        with self.assertRaisesRegex(ValueError, "consumer relationship"):
            self.inventory(bad)

    def test_owner_count_and_static_table_bounds_are_checked(self):
        for phase, field, value in ((3001, 0, 0), (3001, 0, 238761),
                                     (3002, 2, 2), (3014, 1, 5)):
            bad = self.inventory_report()
            bad[phase][field] = value
            with self.assertRaisesRegex(ValueError, "bounds", msg=(phase, field, value)):
                self.inventory(bad)
        for active in range(1, 5):
            bad = self.inventory_report()
            bad[3014][1] = active
            inventory = self.inventory(bad)
            self.assertEqual(inventory["active_session"], active)
            self.assertFalse(inventory["complete"])

    def test_bad_scalar_contracts_and_cross_epoch_parent_are_rejected(self):
        for phase, field, value in ((3000, 0, 2), (3001, 1, 2047), (3002, 0, 2049),
                                     (3014, 2, 1), (3101, 0, 3), (3108, 2, 0),
                                     (3104, 1, 2), (3103, 0, 99), (20000, 1, 4),
                                     (20001, 1, 0), (20002, 1, 1)):
            bad = self.inventory_report()
            bad[phase][field] = value
            with self.assertRaises(ValueError, msg=(phase, field, value)):
                self.inventory(bad)
        bad = self.inventory_report()
        bad[3000][2] = bad[3108][0] = 2
        with self.assertRaisesRegex(ValueError, "consumer relationship"):
            self.inventory(bad)

    def test_count_payload_and_binding_conservation_rejects_malformed_totals(self):
        for phase, field in ((3003, 0), (3003, 1), (3003, 2), (3004, 0), (3004, 1),
                             (3004, 2), (3005, 0), (3007, 0), (3008, 2), (3009, 1),
                             (3010, 2), (3012, 1), (3012, 2), (3013, 0), (3013, 1),
                             (3013, 2), (3014, 0), (3104, 2), (3105, 0), (3107, 1),
                             (3106, 2)):
            bad = self.inventory_report()
            bad[phase][field] += 1
            with self.assertRaisesRegex(ValueError, "conservation", msg=(phase, field)):
                self.inventory(bad)

    def test_errors_or_unfinished_observation_never_classify_usage(self):
        for phase, field in ((3000, 1), (3011, 0), (3011, 1), (3014, 1)):
            bad = self.inventory_report()
            bad[phase][field] = 1
            # Invalid observed totals may be reported, never certified.
            bad[3003][2] += 1
            inventory = self.inventory(bad)
            self.assertFalse(inventory["complete"])
            self.assertFalse(inventory["classification_complete"])
            self.assertIsNone(inventory["classification"])
        bad = self.inventory_report()
        bad[700][1] = 1
        self.assertFalse(self.inventory(bad)["complete"])

    def test_pool_residuals_and_missing_row_binding_are_explicit(self):
        rows = self.inventory_report()
        rows[3005][0] += 8
        rows[3009][0] += 8
        rows[2011][2] += 8
        rows[2041][2] += 8
        rows[3106][2] -= 1
        rows[3013][0] -= 1
        rows[3011][2] += 1
        inventory = self.inventory(rows)
        self.assertTrue(inventory["complete"])
        self.assertEqual(inventory["unattributed_pool_bytes"]["internal_definition_bytes"], 8)
        self.assertEqual(inventory["unattributed_definition_plan_binds"], 1)

    def test_per_definition_consumers_and_definition_contexts_must_conserve(self):
        bad = self.inventory_report()
        bad[3104][2] -= 1
        bad[3108 + 4][2] += 1
        with self.assertRaisesRegex(ValueError, "per-definition consumers"):
            self.inventory(bad)
        bad = self.inventory_report()
        bad[2282] = [2, 2, 1]
        bad[2283][0] = 0
        with self.assertRaisesRegex(ValueError, "definition bind contexts"):
            self.inventory(bad)

    def template_path_report(self):
        rows = self.inventory_report()
        rows[34999] = [1, 3, 3]
        for slot in range(6):
            start = 35000 + slot * 24
            origin = rows[3101 + slot * 8][2] if slot < 3 else rows[20000 + (slot - 3) * 3][2]
            data = "src/é.asm".encode("utf-8") if slot != 2 else b""
            rows[start] = [origin, len(data), slot + 1]
            if data:
                padded = data + bytes((-len(data)) % 12)
                rows[start + 1] = [int.from_bytes(padded[index:index + 4], "big") for index in (0, 4, 8)]
        return rows

    def test_optional_template_source_paths_match_definition_and_edge_origins(self):
        inventory = self.inventory(self.template_path_report())
        self.assertEqual(inventory["definitions"][0]["source_path"], "src/é.asm")
        self.assertEqual(inventory["definitions"][2]["source_path"], "")
        self.assertEqual(inventory["consumer_relationships"][1]["source_path"], "src/é.asm")
        self.assertTrue(inventory["complete"])

    def test_template_source_dictionary_rejects_missing_counts_chunks_and_provenance(self):
        for phase in (34999, 35001, 35072, 35120):
            bad = self.template_path_report()
            del bad[phase]
            with self.assertRaisesRegex(ValueError, "Missing template source path"):
                self.inventory(bad)
        for phase, field, value in ((34999, 0, 2), (34999, 1, 2), (34999, 2, 2),
                                     (35000, 0, 2), (35000, 1, 256), (35000, 2, 2),
                                     (35001, 2, 1), (35001, 0, 0xff000000), (35072, 0, 2)):
            bad = self.template_path_report()
            bad[phase][field] = value
            with self.assertRaises(ValueError, msg=(phase, field, value)):
                self.inventory(bad)
        bad = self.template_path_report()
        bad[35049] = [0, 0, 0]  # Extra chunk for an unavailable path.
        with self.assertRaisesRegex(ValueError, "Unexpected template source path chunk"):
            self.inventory(bad)
        bad = self.template_path_report()
        bad[3101][2] = bad[35000][0] = 0
        with self.assertRaisesRegex(ValueError, "template source path provenance"):
            self.inventory(bad)

    def test_no_rows_is_valid_for_finished_failed_or_clear_probe(self):
        rows = self.inventory_report()
        rows = {phase: value for phase, value in rows.items() if phase < 3100}
        for phase in (3002, 3003, 3004, 3007, 3008, 3011, 3012, 3013, 3014):
            rows[phase] = [0, 0, 0]
        rows[3002][2] = 1
        rows[3009] = rows[3005][:]
        rows[3010] = rows[3006][:]
        rows[2282] = [0, 0, 3]
        rows[2283] = [2, 0, 0]
        inventory = self.inventory(rows)
        self.assertTrue(inventory["complete"])
        self.assertEqual(inventory["classification"]["never_invoked"]["definition_count"], 0)
        rows[3000][2] = rows[3002][2] = 0
        rows[2011][2] = rows[2041][2] = 0
        rows[3005] = rows[3006] = rows[3009] = rows[3010] = [0, 0, 0]
        self.assertTrue(self.inventory(rows)["complete"])  # No template session.


if __name__ == "__main__":
    unittest.main()

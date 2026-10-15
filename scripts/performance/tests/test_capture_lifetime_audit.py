from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).parents[1]))
import capture_lifetime_audit as module


def fixture():
    rows = {phase: [0, 0, 0] for phase in module.FIXED}
    rows.update({600000: [1, 0, 0], 600001: [30000, 10, 256], 600002: [0, 768, 0],
                 600003: [256, 768, 768], 600004: [2, 2, 1], 600005: [4, 9, 1],
                 600010: [2, 1, 768], 600011: [256, 0, 0],
                 600012: [2, 1, 768], 600013: [256, 0, 0],
                 600020: [1, 1, 84], 600021: [76, 0, 0], 600022: [60, 4, 0],
                 600023: [1, 3, 4], 600024: [1, 0, 0], 600025: [3, 0, 3],
                 600026: [1, 256, 256], 600027: [60, 20, 12],
                 600102: [2, 1, 768], 600103: [256, 0, 0],
                 601000: [1, 2, 4], 601001: [768, 256, 256], 601002: [256, 0, 1],
                 602000: [0, 1, 1], 602001: [1, 84, 76], 602002: [256, 0, 0]})
    memory = {"diagnostics": [], "live_owned_bytes": 0, "peak_owned_bytes": 768,
              "total_allocated_bytes": 768, "total_freed_bytes": 768}
    audit = {"observer_complete": True, "capture_used_bytes": 84, "capture_capacity_bytes": 256,
             "regions": 1, "completed_spans": 1, "scheduled_spans": 1,
             "max_parent_pins": 0,
             "potentially_retireable_capacity_bytes": 256,
             "details": {"residency": [[1, 84, 256, 256, 0, 0]],
                         "regions": [[0, 84, 1, 84, 256, 1, 1, 1, 16]]},
             "work_attribution": {"accounting_complete": True, "capture_header_bytes": 60,
                 "template_inventory": {"complete": True}, "metrics": {"total": {
                     "capture_bytes": 84, "capture_header_bytes": 60, "capture_token_bytes": 20,
                     "capture_lexeme_bytes": 4, "capture_plan_bytes": 0, "capture_padding_bytes": 0}}}}
    capture = {"version": 1, "boundary_mask": 7, "counters": {
        "retained_composition": {"records": 1, "header_bytes": 60, "token_bytes": 20},
        "construction": {"retained_source_bytes": 3},
        "preparation": {"lexeme_bytes": 4, "plan_bytes": 0, "padding_bytes": 0},
        "construction_copy": {"retained_append_copy_bytes": 84},
        "retained_growth": {"retained_peak_capacity_including_overlap": 256}},
        "stages": {"replay": {"retained_used": 84, "retained_capacity": 256, "retained_region_count": 1},
                   "release": {"retained_used": 0, "retained_capacity": 0}}}
    return rows, memory, audit, capture


def encode(rows):
    return "\n".join(f"progress p={phase:08x} f={words[0]:08x} l={words[1]:08x} r={words[2]:08x} m=00000000"
                     for phase, words in sorted(rows.items()))


def decode(case):
    rows, *context = case
    return module.decode(encode(rows), *context)


class CaptureLifetimeTests(unittest.TestCase):
    def test_complete_report_reconciles_existing_observers_and_budget(self):
        report = decode(fixture())
        self.assertTrue(report["complete"])
        self.assertEqual(report["heap_peak"], 768)
        self.assertEqual(report["original_bytes"] - report["compact_bytes"], 8)
        self.assertEqual(report["actual_capacity"], report["compact_capacity"])
        self.assertEqual(report["curves"][0]["span"], 1)
        self.assertNotIn("guest_protocol_completed", report)

    def test_empty_inspection_and_fixed_partition_capacity_rounding(self):
        _rows, memory, audit, capture = fixture()
        rows = {phase: [0, 0, 0] for phase in module.FIXED}
        rows.update({600000: [1, 0, 0], 600001: [18868, 10, 256], 600005: [0, 9, 0],
                     600027: [0, 20, 12]})
        for key in memory:
            if key != "diagnostics":
                memory[key] = 0
        for key in ("capture_used_bytes", "capture_capacity_bytes", "regions", "completed_spans",
                    "scheduled_spans", "potentially_retireable_capacity_bytes"):
            audit[key] = 0
        audit["details"] = {}
        audit["work_attribution"]["metrics"]["total"] = dict.fromkeys(audit["work_attribution"]["metrics"]["total"], 0)
        for section in capture["counters"].values():
            section.update(dict.fromkeys(section, 0))
        for stage in capture["stages"].values():
            stage.update(dict.fromkeys(stage, 0))
        self.assertTrue(module.decode(encode(rows), memory, audit, capture)["complete"])
        self.assertEqual([module.compact_capacity(used) for used in (0, 1, 256, 257, 512)],
                         [0, 256, 256, 512, 512])

    def test_whole_record_fallback_preserves_wide_budget_and_reserved_u16(self):
        case = fixture()
        rows = case[0]
        rows[600024][1] = 65535  # Reserved remains full width and does not force fallback.
        self.assertEqual(decode(case)["fallback_records"], 0)
        rows[600025][2] = 65536
        rows[600021] = [84, 1, 1]
        rows[602001][2] = 84
        rows[602002][1:] = [1, 1]
        report = decode(case)
        self.assertEqual(report["original_bytes"], report["compact_bytes"])
        rows[600025][2] = 3
        with self.assertRaisesRegex(ValueError, "fallback/maxima"):
            decode(case)

    def test_raw_shadow_eligibility_can_exceed_remaining_live_capture(self):
        case = fixture()
        case[0][600110] = [3, 5, 512]
        case[0][600111] = [0, 256, 1]
        peak = decode(case)["phase_peaks"]["capture_release"]
        self.assertEqual(peak["eligible"], 256)
        self.assertEqual(peak["instantaneous_eligible_upper_bound"], 0)

    def test_missing_duplicate_unknown_malformed_and_extra_details_reject(self):
        rows, *context = fixture()
        text = encode(rows)
        for invalid in (text.split("\n", 1)[1], text + "\n" + text.splitlines()[0],
                        text.replace("m=00000000", "m=bad", 1)):
            with self.assertRaises(ValueError):
                module.decode(invalid, *context)
        for phase in (600006, 600028, 600120, 601003, 601768, 602003, 602768, 619999):
            bad = fixture()
            bad[0][phase] = [0, 0, 0]
            with self.subTest(phase=phase), self.assertRaises(ValueError):
                decode(bad)
        for phase in (600100, 601001, 602002):
            bad = fixture()
            del bad[0][phase]
            with self.assertRaises(ValueError):
                decode(bad)

    def test_header_heap_mode_event_phase_peak_curve_and_region_bounds(self):
        changes = ((600000, 0, 2), (600000, 1, 1), (600000, 2, 1), (600001, 0, 4),
                   (600001, 1, 9), (600001, 2, 255), (600002, 0, 1), (600002, 2, 1),
                   (600003, 2, 767), (600004, 0, 3), (600004, 2, 257), (600005, 1, 10),
                   (600005, 2, 257), (600010, 0, 5), (600010, 1, 10), (600011, 0, 769),
                   (600011, 2, 2), (600103, 0, 0), (600102, 1, 2), (601000, 0, 2),
                   (601000, 1, 5), (601001, 2, 257), (601002, 0, 255), (601002, 2, 2),
                   (602000, 0, 1), (602001, 1, 257), (602001, 2, 75), (602002, 0, 128),
                   (600021, 0, 75), (600021, 1, 2), (600023, 0, 65), (600024, 1, 65536),
                   (600026, 1, 255), (600027, 0, 56), (600027, 1, 12), (600027, 2, 20))
        for phase, column, value in changes:
            case = fixture()
            case[0][phase][column] = value
            with self.subTest(phase=phase, column=column), self.assertRaises(ValueError):
                decode(case)

    def test_independent_memory_capture_l1_and_work_conservation(self):
        for field in ("live_owned_bytes", "peak_owned_bytes", "total_allocated_bytes", "total_freed_bytes"):
            case = fixture()
            case[1][field] += 1
            with self.subTest(field=field), self.assertRaisesRegex(ValueError, "MEMD"):
                decode(case)
        changes = (("retained_composition", "records"), ("retained_composition", "header_bytes"),
                   ("retained_composition", "token_bytes"), ("preparation", "lexeme_bytes"),
                   ("preparation", "plan_bytes"), ("preparation", "padding_bytes"),
                   ("construction_copy", "retained_append_copy_bytes"),
                   ("retained_growth", "retained_peak_capacity_including_overlap"))
        for section, field in changes:
            case = fixture()
            case[3]["counters"][section][field] += 1
            with self.subTest(section=section, field=field), self.assertRaises(ValueError):
                decode(case)
        for field in ("capture_used_bytes", "capture_capacity_bytes", "regions", "completed_spans",
                      "scheduled_spans", "potentially_retireable_capacity_bytes"):
            case = fixture()
            case[2][field] += 1
            with self.subTest(field=field), self.assertRaisesRegex(ValueError, "L1"):
                decode(case)
        for field in fixture()[2]["work_attribution"]["metrics"]["total"]:
            case = fixture()
            case[2]["work_attribution"]["metrics"]["total"][field] += 1
            with self.subTest(field=field), self.assertRaisesRegex(ValueError, "work"):
                decode(case)

    def test_incomplete_old_observers_and_capture_boundaries_reject(self):
        for component in ("memory", "audit", "work", "inventory", "capture"):
            case = fixture()
            if component == "memory":
                case[1]["diagnostics"] = ["failure"]
            elif component == "audit":
                case[2]["observer_complete"] = False
            elif component == "work":
                case[2]["work_attribution"]["accounting_complete"] = False
            elif component == "inventory":
                case[2]["work_attribution"]["template_inventory"]["complete"] = False
            else:
                case[3]["boundary_mask"] = 3
            with self.subTest(component=component), self.assertRaises(ValueError):
                decode(case)


if __name__ == "__main__":
    unittest.main()

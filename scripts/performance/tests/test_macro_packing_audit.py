import copy
from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).parents[1]))
import macro_packing_audit as audit


def inventory():
    return {"complete": True, "definitions": [
        {"row": 1, "kind": 1, "calls": 0, "first": 20, "last": 28, "closed": 1, "payload_bytes": 40},
        {"row": 2, "kind": 1, "calls": 3, "first": 28, "last": 32, "closed": 1, "payload_bytes": 44},
        {"row": 3, "kind": 0, "calls": 0, "first": 32, "last": 32, "closed": 1, "payload_bytes": 12},
    ]}


def put_vector(rows, start, **metrics):
    values = [metrics.get(field, 0) for field in audit.FIELDS]
    for offset in range(6):
        rows[start + offset] = values[offset * 3:offset * 3 + 3]


def fixture():
    rows = {500000: [1, 0, 0], 500001: [2049, 72, 160000],
            500002: [64, 0, 100], 500003: [5, 2, 1], 500004: [3, 0, 0]}
    put_vector(rows, 500010, writes=5, stored_records=2, stored_bytes=12,
               names=5, samples=3, ticks_high=1, ticks_low=1)
    put_vector(rows, 500020, names=100, source_binds=30)
    put_vector(rows, 500100, writes=1, names=1, samples=1, ticks_low=0xffffffff)
    put_vector(rows, 500106, writes=2, names=2, stored_records=1, stored_bytes=8,
               samples=1, ticks_low=1)
    put_vector(rows, 500112, writes=2, names=2, stored_records=1, stored_bytes=4,
               samples=1, ticks_low=1)
    return rows


def encode(rows):
    return "\n".join(f"progress p={phase:08x} f={values[0]:08x} l={values[1]:08x} r={values[2]:08x} m=12345678"
                     for phase, values in sorted(rows.items()))


class MacroPackingTests(unittest.TestCase):
    def test_sparse_owner_join_classification_and_tick_carry(self):
        report = audit.decode(encode(fixture()), inventory())
        self.assertTrue(report["complete"])
        self.assertEqual(report["gross"]["ticks"], 0x100000001)
        self.assertEqual(report["gross"]["estimated_writer_seconds"], 0x100000001 / 100 * 5 / 3)
        self.assertEqual(report["outside_writer_binds"]["names"], 100)
        self.assertEqual(report["gross"]["names"], 5)
        self.assertEqual(report["groups"]["never_invoked_macro"]["definition_count"], 1)
        self.assertEqual(report["groups"]["never_invoked_macro"]["observed_template_body_bytes"], 8)
        self.assertEqual(report["groups"]["invoked_macro"]["metrics"]["writes"], 2)
        # Empty non-macro templates remain counted without a writer row.
        self.assertEqual(report["groups"]["other_templates"]["closed_definition_count"], 1)
        self.assertIsNone(report["groups"]["other_templates"]["metrics"]["estimated_writer_seconds"])

    def test_empty_no_sample_and_disabled_timing(self):
        rows = {500000: [1, 0, 0], 500001: [2049, 72, 160000],
                500002: [256, 255, 0], 500003: [0, 0, 0], 500004: [0, 0, 0]}
        put_vector(rows, 500010)
        put_vector(rows, 500020)
        report = audit.decode(encode(rows), {"complete": True, "definitions": []})
        self.assertIsNone(report["gross"]["estimated_writer_seconds"])
        self.assertEqual(report["used_rows"], 0)

    def test_missing_duplicate_unknown_partial_and_malformed_rows(self):
        text = encode(fixture())
        for invalid in (text.split("\n", 1)[1], text + "\n" + text.splitlines()[0],
                        text.replace("m=12345678", "m=bad", 1)):
            with self.subTest(invalid=invalid), self.assertRaises(ValueError):
                audit.decode(invalid, inventory())
        for phase in (500005, 500099, 512394, 519999):
            rows = fixture()
            rows[phase] = [0, 0, 0]
            with self.subTest(phase=phase), self.assertRaisesRegex(ValueError, "Unexpected"):
                audit.decode(encode(rows), inventory())
        rows = fixture()
        del rows[500107]
        with self.assertRaisesRegex(ValueError, "Missing"):
            audit.decode(encode(rows), inventory())

    def test_schema_errors_depth_reserved_sampling_and_count_bounds(self):
        for phase, column, value in ((500000, 0, 2), (500000, 1, 1), (500000, 2, 1),
                                      (500001, 0, 2048), (500001, 1, 68), (500001, 2, 4),
                                      (500002, 0, 65), (500002, 1, 256), (500003, 1, 33),
                                      (500003, 2, 2), (500004, 0, 2049), (500004, 1, 1)):
            rows = fixture()
            rows[phase][column] = value
            with self.subTest(phase=phase, column=column), self.assertRaises(ValueError):
                audit.decode(encode(rows), inventory())
        # Seeds cover 0..255 for every supported period.
        for period in (64, 128, 256):
            rows = fixture()
            rows[500002][:2] = [period, 255]
            self.assertTrue(audit.decode(encode(rows), inventory())["complete"])

    def test_vector_bounds_conservation_completed_and_outside_separation(self):
        for start, field, value in ((500010, "writes", 6), (500106, "reserved", 1),
                                     (500106, "failures", 1), (500106, "samples", 3),
                                     (500106, "prefix_writes", 3), (500106, "samples", 0),
                                     (500020, "writes", 1)):
            rows = fixture()
            offset = audit.FIELDS.index(field)
            rows[start + offset // 3][offset % 3] = value
            with self.subTest(start=start, field=field), self.assertRaises(ValueError):
                audit.decode(encode(rows), inventory())
        for phase, column, value in ((500003, 0, 4), (500002, 2, 0), (500003, 2, 0)):
            rows = fixture()
            rows[phase][column] = value
            with self.assertRaises(ValueError):
                audit.decode(encode(rows), inventory())

    def test_bad_owner_payload_inventory_and_missing_stored_owner(self):
        for change in ("unknown", "payload", "missing", "incomplete"):
            rows, inv = fixture(), copy.deepcopy(inventory())
            if change == "unknown":
                for offset in range(6):
                    rows[500124 + offset] = rows.pop(500112 + offset)
            elif change == "payload":
                inv["definitions"][0]["last"] += 1
            elif change == "missing":
                for offset in range(6):
                    del rows[500106 + offset]
                rows[500004][0] -= 1
            else:
                inv["complete"] = False
            with self.subTest(change=change), self.assertRaises(ValueError):
                audit.decode(encode(rows), inv)


if __name__ == "__main__":
    unittest.main()

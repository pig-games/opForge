import contextlib
import io
import json
from pathlib import Path
import struct
import tempfile
import unittest
from unittest import mock

from scripts.performance import analyze_ofm_name_index as audit


def make_ofms(indexed: bool, name: str = "unit.member", bad_index: bool = False) -> bytes:
    header = 48 if indexed else 40
    exports_off = header
    members_off = exports_off + 28
    index_count = 1 if indexed else 0
    index_off = members_off
    names_off = members_off + index_count * 8
    raw_name = name.encode("ascii")
    arena_off = (names_off + len(raw_name) + 7) & ~7
    total = arena_off
    values = bytearray(total)
    if indexed:
        struct.pack_into(">12I", values, 0, int.from_bytes(b"OFMS", "big"), total, 1,
                         exports_off, 0, members_off, names_off, len(raw_name),
                         arena_off, 0, index_count, index_off)
    else:
        struct.pack_into(">10I", values, 0, int.from_bytes(b"OFMS", "big"), total, 1,
                         exports_off, 0, members_off, names_off, len(raw_name),
                         arena_off, 0)
    struct.pack_into(">7I", values, exports_off, 7, names_off, len(raw_name), 0, 0, 0, 0)
    if indexed:
        ordinal = 1 if bad_index else 0
        struct.pack_into(">2I", values, index_off, ordinal, 0)
    values[names_off:names_off + len(raw_name)] = raw_name
    return bytes(values)


def make_ofmi(ofms: bytes) -> bytes:
    ofma = bytearray(48 + len(ofms))
    struct.pack_into(">12I", ofma, 0, int.from_bytes(b"OFMA", "big"), len(ofma), 48,
                     len(ofms), 0, 0, 0, 48, 0, 48, 48, 0)
    ofma[48:] = ofms
    total = 40 + len(ofma)
    ofmi = bytearray(total)
    struct.pack_into(">9I", ofmi, 0, int.from_bytes(b"OFMI", "big"), total, 0,
                     36, 0, 36, 36, 40, len(ofma))
    ofmi[40:] = ofma
    return bytes(ofmi)


class OfmNameIndexTests(unittest.TestCase):
    def test_legacy_and_indexed_header_layouts(self):
        old, _ = audit.parse_ofmi(make_ofmi(make_ofms(False)), Path("old.ofm"))
        old_exports, old_size, old_indexes = audit.parse_ofms(old, Path("old.ofm"))
        self.assertEqual((old_size, old_indexes, old_exports[0]["name"]),
                         (len(old), None, "unit.member"))

        new, _ = audit.parse_ofmi(make_ofmi(make_ofms(True)), Path("new.ofm"))
        new_exports, new_size, indexes = audit.parse_ofms(new, Path("new.ofm"))
        self.assertEqual(new_size, len(new))
        self.assertEqual(indexes, {"full": [0], "leaf": [0]})
        self.assertEqual(new_exports[0]["name"], "unit.member")

    def test_rejects_invalid_stored_index(self):
        data, _ = audit.parse_ofmi(make_ofmi(make_ofms(True, bad_index=True)), Path("bad.ofm"))
        with self.assertRaisesRegex(ValueError, "permutation"):
            audit.parse_ofms(data, Path("bad.ofm"))

    def test_models_length_gate_empty_leaf_and_per_file_header_cost(self):
        exports = [{"id": 1, "name": "unit.member"}]
        length_miss = audit.simulate(exports, "this-name-is-longer", "missing")
        self.assertEqual(length_miss["scan_byte_comparisons"], 0)
        empty_leaf = audit.simulate([{"id": 1, "name": "unit."}], "", "unique_leaf_key")
        self.assertEqual(empty_leaf["result"], "INVALID")

        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            (root / "a.ofm").write_bytes(make_ofmi(make_ofms(False)))
            (root / "b.ofm").write_bytes(make_ofmi(make_ofms(False)))
            output = io.StringIO()
            with contextlib.redirect_stdout(output):
                with mock.patch("sys.argv", ["analyze", str(root)]):
                    audit.main()
            report = json.loads(output.getvalue())
        self.assertEqual(report["proposed_index"]["aggregate_extra_bytes"], 32)


if __name__ == "__main__":
    unittest.main()

import sys
from pathlib import Path
import unittest
sys.path.insert(0, str(Path(__file__).parents[1]))
import candidate_preparation_audit as audit


def fixture():
    rows = {phase: [0, 0, 0] for phase in audit.REQUIRED}
    rows[400000] = [1, 0, 512]
    rows[400002] = [0, 64, 0]
    return rows


def encode(rows):
    return '\n'.join(f'progress p={phase:08x} f={values[0]:08x} l={values[1]:08x} r={values[2]:08x} m=00001234' for phase, values in sorted(rows.items()))


class CandidateAuditTests(unittest.TestCase):
    def test_empty_has_no_extrapolation(self):
        report = audit.decode(encode(fixture()))
        self.assertTrue(report['complete'])
        self.assertIsNone(report['metrics']['total']['estimated_preparation_seconds'])

    def test_tickpair_conserves_across_low_word_carry(self):
        rows = fixture()
        rows[400001] = [2, 0, 0]
        rows[400002] = [100, 64, 0]
        rows[400003] = [2, 2, 0]
        for start, count, high, low in ((400010, 2, 1, 1), (400015, 1, 0, 0xffffffff), (400020, 1, 0, 2)):
            rows[start][0] = count
            rows[start + 3][1:] = [count, high]
            rows[start + 4][0] = low
        self.assertEqual(audit.decode(encode(rows))['metrics']['total']['ticks'], 0x100000001)

    def test_rejects_missing_duplicate_malformed_reserved_and_conservation(self):
        base = encode(fixture())
        for text in (base.split('\n', 1)[1], base + '\n' + base.splitlines()[0],
                     base.replace('m=00001234', 'm=bad', 1)):
            with self.assertRaises(ValueError):
                audit.decode(text)
        for phase, column in ((400014, 1), (400010, 0), (400001, 1)):
            rows = fixture()
            rows[phase][column] = 1
            with self.assertRaises(ValueError):
                audit.decode(encode(rows))

    def test_gross_discarded_acquisitions_conserve_as_sixth_group(self):
        rows = fixture()
        rows[400001] = [1, 4, 0]
        for start in (400010, 400035):
            rows[start] = [1, 100, 4]
        report = audit.decode(encode(rows))
        self.assertEqual(report['metrics']['not_retained']['records'], 1)
        self.assertEqual(report['records_created'], 1)
        del rows[400039]
        with self.assertRaisesRegex(ValueError, 'Missing'):
            audit.decode(encode(rows))

    def test_enabled_timing_before_first_sample_has_no_frequency(self):
        rows = fixture()
        rows[400001][0] = 1
        rows[400002][2] = 37
        rows[400003][0] = 1
        rows[400010][0] = 1
        rows[400015][0] = 1
        report = audit.decode(encode(rows))
        self.assertTrue(report['complete'])
        self.assertEqual(report['timed_calls'], 1)
        self.assertIsNone(report['metrics']['total']['estimated_preparation_seconds'])

    def test_duplicate_replays_cannot_claim_complete_without_error_bit(self):
        rows = fixture()
        rows[400001][2] = 1
        self.assertFalse(audit.decode(encode(rows))['complete'])

    def test_timing_period_and_unknown_owner_bounds(self):
        for period in (0, 1, 63, 65):
            rows = fixture()
            rows[400002][1] = period
            with self.assertRaises(ValueError):
                audit.decode(encode(rows))
        for period in (64, 128, 256):
            rows = fixture()
            rows[400002][1] = period
            self.assertTrue(audit.decode(encode(rows))['complete'])
            rows[400002][2] = period
            with self.assertRaises(ValueError):
                audit.decode(encode(rows))
        rows = fixture()
        rows[400003][2] = 1
        with self.assertRaisesRegex(ValueError, 'owner'):
            audit.decode(encode(rows))

    def test_errors_do_not_claim_complete(self):
        rows = fixture()
        rows[400000][1] = 1
        self.assertFalse(audit.decode(encode(rows))['complete'])

import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).parents[1]))
from hotspot_telemetry import (decode_hotspot_telemetry, expected_samples,
                               HotspotTelemetryError, LABELS, WORK_COUNTERS)


def profile_rows(period=64, seed=0, calls=128, frequency=1000):
    rows = {500: (1, 0, period), 501: (frequency, 3, 0), 502: (32, seed, 0)}
    for index in range(32):
        samples = expected_samples(calls, index, period, seed)
        rows[512 + 3 * index] = (index, calls, samples)
        rows[513 + 3 * index] = (samples, 0, 100 * samples)
        rows[514 + 3 * index] = (0, 40 * samples, 0)
    for index in range(16):
        rows[608 + index] = (index, index * 11, 0)
    return rows


def profile_text(rows):
    return '\n'.join(f'progress p={phase:08x} f={f:08x} l={l:08x} r={r:08x} m=00000000'
                     for phase, (f, l, r) in rows.items())


class HotspotTelemetryTests(unittest.TestCase):
    def decode(self, rows):
        return decode_hotspot_telemetry(profile_text(rows))['hotspot_summary']

    def test_no_rows_and_partial_and_duplicate(self):
        self.assertIsNone(decode_hotspot_telemetry('assembly done')['hotspot_summary'])
        rows = profile_rows()
        del rows[623]
        with self.assertRaisesRegex(HotspotTelemetryError, 'incomplete'):
            self.decode(rows)
        text = profile_text(profile_rows())
        with self.assertRaisesRegex(HotspotTelemetryError, 'duplicate'):
            decode_hotspot_telemetry(text + '\n' + text.splitlines()[0])

    def test_sampling_phase_matches_literal_selection_for_all_periods(self):
        for period in (64, 128, 256):
            for seed in (0, 1, 0xffffffff):
                for index in range(32):
                    for calls in (0, 1, 63, 64, 65, 127, 500):
                        literal = sum(((call - 1 + 17 * index + seed) & (period - 1)) == 0
                                      for call in range(1, calls + 1))
                        self.assertEqual(expected_samples(calls, index, period, seed), literal)
                self.decode(profile_rows(period, seed))

    def test_metadata_and_row_invariants(self):
        invalid = [
            (500, (2, 0, 64)), (500, (1, 1, 64)), (500, (1, 0, 63)),
            (501, (0, 3, 0)), (501, (1000, 33, 0)), (501, (1000, 3, 1)),
            (502, (31, 0, 0)), (502, (32, 0, 1)),
            (512, (1, 128, 2)), (512, (0, 128, 1)),
            (513, (1, 0, 200)), (513, (129, 0, 200)),
            (514, (0, 201, 0)), (514, (0, 80, 1)),
            (608, (1, 10, 0)), (609, (2, 10, 0)), (623, (15, 10, 1)),
        ]
        for phase, values in invalid:
            with self.subTest(phase=phase, values=values):
                rows = profile_rows()
                rows[phase] = values
                with self.assertRaises(HotspotTelemetryError):
                    self.decode(rows)

    def test_latest_protocol_requires_all_slots_and_exposes_work_counters(self):
        rows = profile_rows()
        summary = self.decode(rows)
        self.assertEqual(len(summary['hotspots']), 32)
        self.assertEqual(summary['hotspots'][-1]['label'], LABELS[-1])
        self.assertEqual(summary['work_counters'], {
            label: index * 11 for index, label in enumerate(WORK_COUNTERS)
        })
        # A prior 16-slot profile is incomplete under the current protocol.
        for phase in range(560, 624):
            rows.pop(phase, None)
        with self.assertRaisesRegex(HotspotTelemetryError, 'incomplete'):
            self.decode(rows)

    def test_child_only_windows_do_not_become_own_samples_or_ticks(self):
        rows = profile_rows(calls=1, seed=1)
        # ID zero's first selected invocation is call 64: this single parent
        # invocation can still have a timing window because a child was sampled.
        rows[513] = (1, 0, 0)
        summary = self.decode(rows)
        parent = summary['hotspots'][0]
        self.assertEqual(parent['timing_windows'], 1)
        self.assertEqual(parent['own_samples'], 0)
        self.assertEqual(parent['sample_exclusive_seconds'], 0)
        self.assertIsNone(parent['estimated_total_exclusive_seconds'])
        rows[513] = (1, 0, 10)
        with self.assertRaisesRegex(HotspotTelemetryError, 'own-sample ticks'):
            self.decode(rows)

    def test_u64_ticks_estimates_and_independent_ranking(self):
        rows = profile_rows()
        rows[513] = (128, 1, 10)
        rows[514] = (0, 20, 0)
        rows[516] = (2, 0, 1000)
        rows[517] = (0, 900, 0)
        summary = self.decode(rows)
        first = summary['hotspots'][0]
        self.assertEqual(first['sample_inclusive_ticks'], 2**32 + 10)
        self.assertEqual(first['sample_exclusive_seconds'], .02)
        self.assertEqual(first['estimated_total_exclusive_seconds'], 1.28)
        self.assertEqual(summary['hottest_inclusive'][0], LABELS[0])
        self.assertEqual(summary['hottest_exclusive'][0], LABELS[1])
        self.assertIn('Fixed-period sampling can be biased', summary['interpretation'])
        self.assertIn('not measured full-runtime', summary['interpretation'])
        self.assertNotIn('total_seconds', summary)

    def test_failed_assembly_can_preserve_partial_selection_counts(self):
        rows = profile_rows()
        rows[512] = (0, 128, 1)
        summary = decode_hotspot_telemetry(profile_text(rows), terminal_complete=False)['hotspot_summary']
        self.assertFalse(summary['terminal_sample_counts_validated'])

    def test_malformed_fields_are_rejected_as_u32(self):
        text = profile_text(profile_rows())
        for bad in ('100000000', '-0000001', '0000000g'):
            with self.subTest(bad=bad), self.assertRaises(HotspotTelemetryError):
                decode_hotspot_telemetry(text.replace('f=00000001', 'f=' + bad, 1))


if __name__ == '__main__':
    unittest.main()

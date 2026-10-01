import importlib.util
from pathlib import Path
import struct
import unittest


SPEC = importlib.util.spec_from_file_location(
    "memory_telemetry", Path(__file__).parents[1] / "memory_telemetry.py")
decoder = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(decoder)


class MemoryTelemetryTests(unittest.TestCase):
    def record(self):
        words = [0] * 570
        words[0] = decoder.RECORD_MAGIC
        return words

    def decode(self, words):
        return decoder.decode_memory_telemetry(struct.pack(">570I", *words))

    def test_rejects_wrong_magic_and_any_other_length(self):
        with self.assertRaisesRegex(ValueError, "magic"):
            self.decode([0] * 570)
        valid = struct.pack(">570I", *self.record())
        for record in (b"", valid[:-1], valid + b"x"):
            with self.assertRaisesRegex(ValueError, "2280 bytes"):
                decoder.decode_memory_telemetry(record)

    def test_offsets_endian_nested_scopes_and_date_rollover(self):
        words = self.record()
        words[1:19] = range(1, 19)
        words[19:28] = [2, 1439, 2999, 3, 0, 1, 3, 0, 126]
        words[28] = 1000
        for i in range(7):
            words[30 + 2 * i:32 + 2 * i] = [1, i + 100]
            words[44 + i] = 20 + i
            words[528 + 2 * i:530 + 2 * i] = [0, 100 * (i + 1)]
            words[542 + i] = 30 + i
        words[51], words[71], words[72 + 21 + 2] = 7, 9, 11
        words[513:520] = range(101, 108)
        words[520:524] = [0, 900, 0, 200]
        words[524:528] = [2, 4096, 2048, 1024]
        words[549:553] = [0, 125, 128, 2]
        words[553:565] = range(201, 213)
        words[565:570] = [1, 123, 41, 0x01020304, 43]
        result = self.decode(words)
        self.assertAlmostEqual(result["instrumented_preparation_seconds"], .04)
        self.assertEqual(result["instrumented_assembly_seconds"], 2.5)
        for i, name in enumerate(decoder.STAGES):
            self.assertEqual(result["preparation_stages"][name]["ticks"], (1 << 32) + i + 100)
            self.assertEqual(result["preparation_stages"][name]["calls"], 20 + i)
        self.assertEqual(result["tokenizer"]["opcode_total"], 16)
        self.assertEqual(result["tokenizer"]["opcode_pairs"],
                         [{"previous_opcode": 1, "opcode": 2, "count": 11}])
        self.assertEqual(result["eclock_hz"], 1000)
        self.assertEqual(result["tokenizer"]["taken_class"], 107)
        self.assertEqual(result["tokenizer"]["helpers_seconds"], .9)
        self.assertEqual(result["tokenizer"]["commit_seconds"], .2)
        for i, name in enumerate(decoder.BINDING_SCOPES):
            self.assertEqual(result["binding_detail"]["scopes"][name]["calls"], 30 + i)
            self.assertAlmostEqual(result["binding_detail"]["scopes"][name]["seconds"], (i + 1) / 10)
        self.assertEqual(result["binding_detail"]["binding_estimated_seconds"], 8)
        self.assertEqual(result["template_work"]["string_plan_captures"], 212)
        self.assertEqual(result["input_collection"]["seconds"], ((1 << 32) + 123) / 1000)
        self.assertEqual(result["input_collection"]["bytes"], 0x01020304)
        self.assertEqual(result["last_failed_request_bytes"], 4096)
        self.assertEqual(result["compiled_program_bytes"], 18)

    def test_missing_clocks_preserve_counts_without_zero_times(self):
        words = self.record()
        words[30], words[44], words[552], words[567] = 1, 9, 1, 17
        result = self.decode(words)
        self.assertIsNone(result["instrumented_preparation_seconds"])
        self.assertIsNone(result["preparation_stages"][decoder.STAGES[0]]["seconds"])
        self.assertIsNone(result["tokenizer"]["helpers_seconds"])
        self.assertIsNone(result["binding_detail"]["binding_estimated_seconds"])
        self.assertIsNone(result["input_collection"]["seconds"])
        self.assertEqual(result["input_collection"]["calls"], 17)
        self.assertEqual(result["preparation_stages"][decoder.STAGES[0]]["ticks"], 1 << 32)

    def test_zero_observation_and_no_binding_samples_are_distinct(self):
        words = self.record()
        words[28] = 1000
        words[19:28] = [1, 0, 2] * 3
        result = self.decode(words)
        self.assertEqual(result["instrumented_preparation_seconds"], 0)
        self.assertEqual(result["input_collection"]["seconds"], 0)
        self.assertIsNone(result["binding_detail"]["binding_estimated_seconds"])
        words[22:25] = [1, 0, 1]
        self.assertIsNone(self.decode(words)["instrumented_preparation_seconds"])

    def test_failed_terminal_record_keeps_partial_measurements(self):
        words = self.record()
        words[1], words[3], words[4], words[11] = 100, 500, 400, 100
        words[28], words[29], words[524], words[525] = 1000, 16 | 32 | 128, 1, 2048
        words[31], words[44] = 250, 3
        result = self.decode(words)
        self.assertEqual(result["allocation_failure_flags"], 128)
        self.assertEqual(result["profiling_errors"], 176)
        self.assertEqual(len(result["diagnostics"]), 3)
        self.assertEqual(result["preparation_stages"][decoder.STAGES[0]]["seconds"], .25)


if __name__ == "__main__":
    unittest.main()

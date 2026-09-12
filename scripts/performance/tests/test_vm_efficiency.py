import importlib.util
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
import unittest

SPEC = importlib.util.spec_from_file_location('vm_efficiency', Path(__file__).parents[1] / 'vm_efficiency.py')
runner = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(runner)


class VmEfficiencyTests(unittest.TestCase):
    def test_distinct_family_oracles_include_forward_branch_and_label_values(self):
        # Independent first-block instruction bytes, including endian-sensitive address data.
        known = {'m6502': 'a901a2018d0020d00100ea0110',
                 'z80': '3e010601320020200100000110',
                 'm68000': '7001720111c02000660200004e7100001002'}
        for cpu, encoded in known.items():
            source, expected = runner.workload(cpu, 1)
            self.assertEqual(expected.hex(), encoded)
            self.assertIn('next0', source)
            self.assertIn('block0+', source)

    def test_profile_requires_vm_attribution_and_rejects_host_bypass(self):
        good = '[opforge execution profile]\npass1.line_route\n  vm.encode 1.234 ms 50.00%  (40x)\n  vm.parse 1.000 ms 10.00%  (40x)\n'
        self.assertEqual(runner.parse_profile(good)[0]['count'], 40)
        for invalid in ('', good.replace('vm.encode', 'other'),
                        good + '  rust.encode 1.000 ms 5.00%  (1x)\n'):
            with self.assertRaises(ValueError):
                runner.parse_profile(invalid)

    def test_vm_work_repetition_and_ratios(self):
        import json
        report = {'schema': 1, 'overflow': False,
                  'programs': [{'id': 0, 'engine': 'parser', 'version': 2, 'bytes_hex': '0102'}],
                  'rows': [{'program': 0, 'phase': 'pass1', 'calls': 2, 'steps': 5,
                            'repeated_within_call': 1,
                            'positions': [{'position': 0, 'opcode': 1, 'count': 3},
                                          {'position': 1, 'opcode': 2, 'count': 2}]}], 'events': []}
        text = '[opforge vm work] ' + json.dumps(report)
        aggregate = runner.parse_vm_work(text, 1, 10, 3)['aggregate']
        self.assertEqual(aggregate['steps_per_assembly_instruction'], 1)
        self.assertEqual(aggregate['top_programs'][0]['repeated_between_calls'], 2)
        for invalid in ('', text + '\n' + text, text.replace('false', 'true'),
                        text.replace('"steps": 5', '"steps": 6')):
            with self.assertRaises(ValueError):
                runner.parse_vm_work(invalid, 1, 10, 3)

    def test_tokenizer_mode_comparison_rejects_different_inputs_or_work(self):
        import copy
        auto = {'cpu': 'fixture', 'blocks': 1, 'tokenizer_mode': 'auto',
                'source_sha256': 'source', 'output_sha256': 'output', 'median_seconds': 1,
                'vm_work': {'events': [{'label': 'tokenizer.fast.logical_budget_steps', 'count': 7}]}}
        generic = {'cpu': 'fixture', 'blocks': 1, 'tokenizer_mode': 'generic',
                   'source_sha256': 'source', 'output_sha256': 'output', 'median_seconds': 2,
                   'vm_work': {'aggregate': {'by_engine': {'tokenizer': 7}}}}
        self.assertTrue(runner.compare_tokenizer_modes([auto, generic])[0]['identical_input_and_output'])
        for key in ('source_sha256', 'output_sha256'):
            changed = copy.deepcopy(generic)
            changed[key] = 'different'
            with self.assertRaises(ValueError):
                runner.compare_tokenizer_modes([auto, changed])
        generic['vm_work']['aggregate']['by_engine']['tokenizer'] = 8
        with self.assertRaises(ValueError):
            runner.compare_tokenizer_modes([auto, generic])
        self.assertEqual(runner.clean_env(tokenizer_mode='generic')['OPFORGE_TOKENIZER_FORCE_GENERIC'], '1')

    def test_ambient_experiment_environment_is_not_inherited(self):
        from unittest.mock import patch
        with patch.dict(os.environ, {'OPFORGE_OPASM_PACKAGE': 'wrong', 'OPFORGE_PROFILE_PHASES': '1', 'OPFORGE_TOKENIZER_FORCE_GENERIC': '1'}):
            self.assertNotIn('OPFORGE_OPASM_PACKAGE', runner.clean_env())
            self.assertNotIn('OPFORGE_PROFILE_PHASES', runner.clean_env())
            self.assertNotIn('OPFORGE_PROFILE_VM_WORK', runner.clean_env())
            self.assertNotIn('OPFORGE_TOKENIZER_FORCE_GENERIC', runner.clean_env())
            self.assertEqual(runner.clean_env(True)['OPFORGE_PROFILE_VM_WORK'], '1')
            self.assertEqual(runner.clean_env()['NO_COLOR'], '1')

    def test_exhausted_batch_does_not_launch_child(self):
        with tempfile.TemporaryDirectory() as tmp:
            marker = Path(tmp) / 'launched'
            budget = runner.Budget(0)
            with self.assertRaises(TimeoutError):
                runner.run_process([sys.executable, '-c', f'open({str(marker)!r}, "w").close()'],
                                   tmp, runner.clean_env(), budget)
            self.assertFalse(marker.exists())

    def test_success_exit_alone_is_not_output_evidence(self):
        from unittest.mock import patch
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            def wrong_output(*args, **kwargs):
                (directory / 'output.bin').write_bytes(b'wrong')
                return 0, b'', b'', 0.01
            with patch.object(runner, 'run_process', side_effect=wrong_output):
                with self.assertRaisesRegex(ValueError, 'output contract'):
                    runner.assemble(Path('/unused'), directory, 'm6502', runner.Budget(), b'expected')

    def test_signal_is_not_an_expected_negative_result(self):
        from unittest.mock import patch
        with tempfile.TemporaryDirectory() as tmp:
            with patch.object(runner, 'run_process', return_value=(-9, b'', b'ERROR missing_symbol', 0.01)):
                with self.assertRaisesRegex(ValueError, 'crashed'):
                    runner.assemble(Path('/unused'), Path(tmp), 'm6502', runner.Budget(), b'', negative=True)

    def test_unrelated_error_showing_source_symbol_is_not_expected_failure(self):
        from unittest.mock import patch
        with tempfile.TemporaryDirectory() as tmp:
            with patch.object(runner, 'run_process', return_value=(1, b'', b'ERROR: invalid directive\n.byte missing_symbol', 0.01)):
                with self.assertRaisesRegex(ValueError, 'expected undefined-symbol'):
                    runner.assemble(Path('/unused'), Path(tmp), 'm6502', runner.Budget(), b'', negative=True)

    @unittest.skipUnless(os.name == 'posix', 'POSIX process-group runner')
    def test_timeout_terminates_descendants_holding_pipes(self):
        with tempfile.TemporaryDirectory() as tmp:
            marker = Path(tmp) / 'leaked-child'
            child = f'import time; time.sleep(0.4); open({str(marker)!r}, "w").close()'
            parent = f'import subprocess,sys,time; subprocess.Popen([sys.executable,"-c",{child!r}]); time.sleep(10)'
            start = time.monotonic()
            with self.assertRaises(subprocess.TimeoutExpired):
                runner.run_process([sys.executable, '-c', parent], tmp, runner.clean_env(),
                                   runner.Budget(0.15), cap=10)
            self.assertLess(time.monotonic() - start, 2)
            time.sleep(0.5)
            self.assertFalse(marker.exists())


if __name__ == '__main__':
    unittest.main()

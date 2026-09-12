import importlib.util
import os
from pathlib import Path
import unittest

SPEC = importlib.util.spec_from_file_location(
    'package_boundaries', Path(__file__).parents[1] / 'package_boundaries.py')
runner = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(runner)


class PackageBoundariesTests(unittest.TestCase):
    def test_callback_report_schema_and_mode_are_strict(self):
        import json
        payload = {'schema': 1, 'mode': 'report', 'attempts': [
            {'boundary': 'operand_surface', 'family': 'm6502', 'cpu': 'm6502',
             'detail': '', 'count': 2}], 'first_refusal': None, 'overflow': False}
        text = runner.CALLBACK_PREFIX + json.dumps(payload)
        self.assertEqual(runner.parse_callbacks(text, 'report')['attempts'][0]['count'], 2)
        for invalid in ('', text + '\n' + text,
                        text.replace('"schema": 1', '"schema": 2'),
                        text.replace('"count": 2', '"count": 0'),
                        text.replace('"first_refusal": null', '"first_refusal": false'),
                        text.replace('"overflow": false', '"overflow": true')):
            with self.assertRaises(ValueError):
                runner.parse_callbacks(invalid, 'report')
        with self.assertRaises(ValueError):
            runner.parse_callbacks(text, 'refuse')

    def test_normalization_removes_instrumentation_and_multiline_profile(self):
        baseline = ('ERROR [asm401] same error\n[opforge phase profile]\n'
                    'assembly_total 1.0 ms 100%\n[opforge execution profile]\n'
                    'pass1.line_route\n  vm.encode 1.0 ms 10% (2x)\n'
                    'global:\n  vm.model.bootstrap 1.0 ms 10% (1x)\n'
                    '[opforge vm work] {"schema":1}\nERROR [asm402] after instrumentation\n')
        report = ('ERROR [asm401] same error\n[opforge target callbacks] {"schema":1}\n'
                  '[opforge phase profile]\nassembly_total 2.0 ms 100%\n'
                  '[opforge execution profile]\npass1.line_route\n'
                  '  vm.encode 9.0 ms 90% (2x)\n[opforge vm work] {"schema":1}\n'
                  'ERROR [asm402] after instrumentation\n')
        self.assertEqual(runner.normalize_diagnostics(baseline),
                         'ERROR [asm401] same error\nERROR [asm402] after instrumentation')
        self.assertEqual(runner.normalize_diagnostics(baseline), runner.normalize_diagnostics(report))

    def test_environment_clears_ambient_callback_setting(self):
        from unittest.mock import patch
        with patch.dict(os.environ, {'OPFORGE_TARGET_CALLBACKS': 'refuse',
                                     'OPFORGE_PROFILE_VM_WORK': '0'}):
            self.assertNotIn('OPFORGE_TARGET_CALLBACKS', runner.clean_env())
            self.assertEqual(runner.clean_env(callback_mode='report')['OPFORGE_TARGET_CALLBACKS'], 'report')
            self.assertEqual(runner.clean_env(profile=True)['OPFORGE_PROFILE_VM_WORK'], '1')

    def test_refusal_validator_requires_nonzero_no_output_and_refusal(self):
        import tempfile
        from unittest.mock import patch
        import json
        payload = {'schema': 1, 'mode': 'refuse', 'attempts': [
            {'boundary': 'operand_surface', 'family': 'z80', 'cpu': 'z80',
             'detail': '', 'count': 1}], 'first_refusal': 'operand_surface', 'overflow': False}
        report = runner.CALLBACK_PREFIX + json.dumps(payload)
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            (directory / 'input.asm').write_text('.cpu z80\n')
            with patch.object(runner.vm, 'run_process', return_value=(1, b'useful diagnostic', report.encode(), 0.01)):
                result = runner.assemble(Path('/unused'), directory, 'z80', 8, '.cpu z80\n', b'',
                                         runner.vm.Budget(), 'refuse')
            self.assertEqual(result['callbacks']['first_refusal'], 'operand_surface')
            with patch.object(runner.vm, 'run_process', return_value=(0, b'', report.encode(), 0.01)):
                with self.assertRaisesRegex(ValueError, 'fail closed'):
                    runner.assemble(Path('/unused'), directory, 'z80', 8, '.cpu z80\n', b'',
                                     runner.vm.Budget(), 'refuse')

    def test_refusal_mode_accepts_verified_callback_free_success(self):
        import json
        import tempfile
        from unittest.mock import patch
        payload = {'schema': 1, 'mode': 'refuse', 'attempts': [],
                   'first_refusal': None, 'overflow': False}
        report = runner.CALLBACK_PREFIX + json.dumps(payload)
        expected = bytes.fromhex('4e71')
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            (directory / 'input.asm').write_text(' nop\n.end\n')

            def successful(command, cwd, env, budget, cap=60):
                (Path(cwd) / 'output.bin').write_bytes(expected)
                return 0, b'', report.encode(), 0.01

            with patch.object(runner.vm, 'run_process', side_effect=successful):
                result = runner.assemble(Path('/unused'), directory, 'm68000', 1,
                                         ' nop\n.end\n', expected,
                                         runner.vm.Budget(), 'refuse')
            self.assertEqual(result['callbacks']['attempts'], [])
            self.assertEqual(result['output_sha256'], runner.digest(expected))

    def test_refusal_mode_does_not_accept_success_after_callback_attempt(self):
        import json
        import tempfile
        from unittest.mock import patch
        payload = {'schema': 1, 'mode': 'refuse', 'attempts': [
            {'boundary': 'operand_surface', 'family': 'm68000', 'cpu': 'm68000',
             'detail': '', 'count': 1}], 'first_refusal': 'operand_surface', 'overflow': False}
        report = runner.CALLBACK_PREFIX + json.dumps(payload)
        expected = bytes.fromhex('4e71')
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            (directory / 'input.asm').write_text(' nop\n.end\n')

            def unsafe_success(command, cwd, env, budget, cap=60):
                (Path(cwd) / 'output.bin').write_bytes(expected)
                return 0, b'', report.encode(), 0.01

            with patch.object(runner.vm, 'run_process', side_effect=unsafe_success):
                with self.assertRaisesRegex(ValueError, 'fail closed'):
                    runner.assemble(Path('/unused'), directory, 'm68000', 1,
                                    ' nop\n.end\n', expected,
                                    runner.vm.Budget(), 'refuse')


if __name__ == '__main__':
    unittest.main()

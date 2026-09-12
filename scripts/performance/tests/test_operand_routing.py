import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

SPEC = importlib.util.spec_from_file_location(
    'operand_routing', Path(__file__).parents[1] / 'operand_routing.py')
runner = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(runner)


class OperandRoutingTests(unittest.TestCase):
    def test_timing_reference_must_match_captured_oracle(self):
        identity = {'binary_sha256': 'binary', 'package_sha256': 'package'}
        runner.validate_reference_identity({'reference': identity}, identity)
        for key in identity:
            changed = dict(identity, **{key: 'changed'})
            with self.assertRaisesRegex(ValueError, 'captured oracle'):
                runner.validate_reference_identity({'reference': identity}, changed)

    def test_timeout_preserves_incomplete_summary(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            binary, package = root / 'binary', root / 'package'
            binary.write_bytes(b'placeholder')
            package.write_bytes(b'placeholder')
            output = root / 'result'
            args = ['operand_routing', '--reference-binary', str(binary),
                    '--reference-package', str(package), '--output', str(output)]
            with patch('sys.argv', args), patch('builtins.print'), patch.object(runner.vm, 'run_process',
                    side_effect=TimeoutError('owned process deadline')):
                self.assertEqual(runner.main(), 1)
            summary = json.loads((output / 'summary.json').read_text())
            self.assertEqual(summary['status'], 'incomplete')
            self.assertIn('deadline', summary['error'])


if __name__ == '__main__':
    unittest.main()

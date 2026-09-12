import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_benchmark_selectors import scan, violations


class BenchmarkSelectorsTests(unittest.TestCase):
    def test_rust_dispatch_and_fixture_paths_are_rejected(self):
        for source in ('if name == "B01" { shortcut(); }',
                       'match name { "B10" => shortcut(), _ => normal() }',
                       'let path = r#"scripts/performance/fixtures/corpus.json"#;',
                       'if path.ends_with("documentation/performance/results/run.json") {}'):
            with self.subTest(source=source):
                self.assertTrue(violations(source, ".rs"))

    def test_native_selectors_are_rejected_but_hex_constants_are_allowed(self):
        self.assertTrue(violations('name dc.b "B10",0', ".asm"))
        self.assertTrue(violations("name dc.b 'B01',0", ".asm"))
        self.assertEqual(violations('move.w #$B01,d0\nmove.w #$B10,d1', '.asm'), [])

    def test_comments_do_not_trigger_but_strings_with_comment_characters_do(self):
        self.assertEqual(violations('// B01\n/* B10 */\nlet x = 1;', '.rs'), [])
        self.assertEqual(violations('; B01\nmoveq #0,d0', '.asm'), [])
        self.assertTrue(violations('let path = "https://host/fixtures/performance/input";', '.rs'))
        self.assertTrue(violations('dc.b "prefix;B10",0', '.asm'))
        self.assertEqual(violations('// note\nlet id = "B01";', '.rs')[0][0], 2)

    def test_scan_excludes_test_files_and_harnesses_not_production(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            for name in ['crates/demo/src/tests.rs', 'crates/demo/src/tests/cases.rs',
                         'native/test-harnesses/case.asm', 'crates/demo/src/runtime.rs']:
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text('"B01"')
            checked, errors = scan(root)
            self.assertEqual(checked, 1)
            self.assertEqual(len(errors), 1)
            self.assertIn('runtime.rs', errors[0])


if __name__ == '__main__':
    unittest.main()

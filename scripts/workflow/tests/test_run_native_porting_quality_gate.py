import sys
import unittest
from contextlib import redirect_stderr
from io import StringIO
from pathlib import Path
from unittest.mock import Mock, patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from run_native_porting_quality_gate import CHECKS, commands, run_gate


class NativePortingGateTests(unittest.TestCase):
    def test_order_is_deterministic_and_default_has_no_external_launch(self):
        result = commands(Path("/repo"), True, None)
        self.assertEqual([Path(command[1]).name for command in result[:-1]], list(CHECKS))
        joined = "\n".join(" ".join(command) for command in result)
        self.assertIn("check_native_runtime_no_growth.py", CHECKS)
        self.assertNotIn("fs-uae", joined.lower())
        self.assertNotIn("curl", joined.lower())
        self.assertEqual(result[-1], ["make", "native-68000-format-check"])

    def test_first_failure_stops_and_reports_the_check(self):
        fake_commands = [["python", "/repo/first.py"], ["python", "/repo/second.py"]]
        stderr = StringIO()
        with (
            patch("run_native_porting_quality_gate.commands", return_value=fake_commands),
            patch(
                "run_native_porting_quality_gate.subprocess.run",
                side_effect=[Mock(returncode=7), AssertionError("second check must not run")],
            ) as runner,
            redirect_stderr(stderr),
        ):
            self.assertEqual(run_gate(Path("/repo"), True, None), 7)
        self.assertEqual(runner.call_count, 1)
        self.assertIn("first.py", stderr.getvalue())


if __name__ == "__main__":
    unittest.main()

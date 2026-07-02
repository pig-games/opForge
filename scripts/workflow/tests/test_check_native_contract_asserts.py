import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_native_contract_asserts import validate_contracts


def doc(name="CONTRACT_X", stability="stable"):
    return f"""### `{name}`

Rust reference: r
Native boundary: n
Condition: c
Failure meaning: f
Stability: {stability}
"""


class ContractAssertTests(unittest.TestCase):
    def test_valid_contract_and_approved_macro_pass(self):
        self.assertEqual(
            validate_contracts(
                "CONTRACT_X = 1",
                doc(),
                {"x.asm": ".DEBUG_ASSERT_SPAN_IN_TEXT contracts.CONTRACT_X"},
            ),
            [],
        )

    def test_definition_and_documentation_failures(self):
        cases = (
            ("", doc(), {"x.asm": "CONTRACT_X"}),
            ("CONTRACT_X = 1", "", {}),
            ("CONTRACT_X = 1\nCONTRACT_Y = 1", doc() + doc("CONTRACT_Y"), {}),
            ("CONTRACT_X = 1", doc(stability="forever"), {}),
            ("CONTRACT_X = 1", "### `CONTRACT_X`\nStability: stable", {}),
            ("CONTRACT_X = 1", doc(), {"x.asm": ".DEBUG_ASSERT_MAGIC CONTRACT_X"}),
        )
        for ids, docs, asm in cases:
            with self.subTest(ids=ids, docs=docs, asm=asm):
                self.assertTrue(validate_contracts(ids, docs, asm))


if __name__ == "__main__":
    unittest.main()

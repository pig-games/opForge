"""Keep disabled telemetry callable as the enabled macro API evolves."""
from pathlib import Path
import re
import unittest


ROOT = Path(__file__).resolve().parents[3]
DEBUG = ROOT / "native/motorola68000/amigaos/debug"
MACRO = re.compile(
    r"^([^\n]+[ \t]+\.macro[^\n]*)\n(.*?)^\.endmacro$",
    re.MULTILINE | re.DOTALL,
)
ALWAYS_CALLABLE = {
    "binding_telemetry": {"BINDING_REMAP_FAILURE"},
    "hotspot_telemetry": {"HOTSPOT_CALL", "HOTSPOT_BSR"},
}


class NativeTelemetryDefinitionGuardsTests(unittest.TestCase):
    def test_noop_inventories_preserve_complete_macro_signatures(self):
        for stem in (
            "memory_telemetry", "telemetry_macros", "hotspot_telemetry",
            "capture_telemetry", "binding_telemetry",
        ):
            with self.subTest(stem=stem):
                source = (DEBUG / f"{stem}.i").read_text()
                noop_path = DEBUG / f"{stem}_noop.i"
                enabled = {}
                for match in MACRO.finditer(source):
                    signature = " ".join(match[1].split())
                    name = signature.split()[0]
                    if name in ALWAYS_CALLABLE.get(stem, set()):
                        continue
                    if name in enabled:
                        self.assertEqual(enabled[name], signature)
                    enabled[name] = signature
                disabled = {}
                for match in MACRO.finditer(noop_path.read_text()):
                    signature = " ".join(match[1].split())
                    name = signature.split()[0]
                    self.assertNotIn(name, disabled)
                    self.assertFalse(match[2].strip(), name)
                    disabled[name] = signature
                self.assertTrue(enabled)
                self.assertEqual(enabled, disabled)
                self.assertLessEqual(len(noop_path.name), 30)


if __name__ == "__main__":
    unittest.main()

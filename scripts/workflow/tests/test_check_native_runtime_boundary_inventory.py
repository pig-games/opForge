import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import check_native_runtime_boundary_inventory as inventory
from check_native_runtime_boundary_inventory import TARGETS, validate


class NativeRuntimeBoundaryInventoryTests(unittest.TestCase):
    def test_inventory_matches_all_audited_sources(self):
        self.assertEqual(validate(), [])

    def test_audit_set_covers_promoted_orchestration_packages_and_vms(self):
        self.assertEqual(len(TARGETS), 25)
        for module in (
            "opforge.assembler.driver", "opforge.assembler.prepare.frontend",
            "opforge.assembler.layout.assembly", "opforge.packages.loader",
            "opforge.packages.validation", "opforge.vm.exvm.runtime",
            "opforge.vm.encoding.runtime", "opforge.vm.value.runtime",
            "opforge.vm.exprvm.i64_math", "opforge.cli.entry",
        ):
            self.assertIn(module, TARGETS)


class SourceClosureTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        self.module = "audit.runtime"
        self.source = "native/vm/runtime.asm"
        self.write(self.source, '''.module audit.runtime
.include "helpers.i"
.include "../debug/telemetry.i"
run .block
 .bend
 .section code, kind=code
''')
        self.write("native/vm/helpers.i", "helper .block\n .bend\n")
        self.write("native/debug/telemetry.i", '''.ifdef DEBUG
.use audit.debug as debug
.else
.include "noop.i"
.endif
TRACE .macro
inside .block
 .use audit.template_only
 .section template_data
 .bend
.endmacro
''')
        self.write("native/debug/noop.i", "; disabled definitions\n")
        surface = inventory.source_surface(self.source, self.root)
        self.write(inventory.INVENTORY.relative_to(inventory.ROOT),
                   "# Audit\n\n" + inventory.inventory_section(
                       self.module, self.source, "run", surface))
        for name, value in (
            ("TARGETS", {self.module: (self.source, "run", ("audit.debug",))}),
            ("SNAPSHOTS", {self.module: surface.snapshot()}),
        ):
            replacement = patch.object(inventory, name, value)
            replacement.start()
            self.addCleanup(replacement.stop)

    def write(self, relative, text):
        path = self.root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")

    def errors(self):
        return "\n".join(validate(self.root))

    def test_relative_and_nested_includes_cover_possible_gated_imports(self):
        self.assertEqual(validate(self.root), [])
        surface = inventory.source_surface(self.source, self.root)
        self.assertEqual(surface.blocks, ["run", "helper"])
        self.assertEqual(surface.imports, ["audit.debug"])
        self.assertEqual(surface.sections, ["code"])
        self.assertEqual(len(surface.sources), 4)

    def test_helper_only_edits_invalidate_snapshot(self):
        self.write("native/vm/helpers.i", "helper .block\n nop\n .bend\n")
        self.assertIn("source/include changed", self.errors())

    def test_disabled_branch_and_macro_body_edits_invalidate_snapshot(self):
        for name in ("native/debug/noop.i", "native/debug/telemetry.i"):
            with self.subTest(name=name):
                path = self.root / name
                original = path.read_text()
                changed = (original.replace("inside .block", "changedInside .block")
                           if name.endswith("telemetry.i") else original + "; changed\n")
                path.write_text(changed)
                self.assertIn("source/include changed", self.errors())
                path.write_text(original)

    def test_duplicate_include_is_one_ownership_surface(self):
        path = self.root / self.source
        path.write_text(path.read_text() + '.include "./helpers.i"\n')
        surface = inventory.source_surface(self.source, self.root)
        self.assertEqual(surface.blocks, ["run", "helper"])
        self.assertEqual(len(surface.sources), 4)
        self.assertIn("source/include changed", self.errors())

    def test_missing_include_fails_closed(self):
        (self.root / "native/debug/noop.i").unlink()
        self.assertIn("missing source/include: native/debug/noop.i", self.errors())

    def test_include_cycle_fails_closed(self):
        self.write("native/vm/helpers.i", '.include "runtime.asm"\n')
        self.assertIn("include cycle:", self.errors())

    def test_unsupported_include_contexts_fail_closed(self):
        for text, error in (
            (".include path\n", "unsupported nonliteral include"),
            ('M .macro\n.include "helpers.i"\n.endmacro\n', "template-local include"),
            ('.include "../../../outside.i"\n', "include outside repository"),
        ):
            with self.subTest(text=text):
                self.write(self.source, text)
                self.assertIn(error, self.errors())

    def test_entry_and_module_are_definitions_not_substrings(self):
        self.write(self.source, '''; .module audit.runtime run
.module audit.runtime_extra
.include "../debug/telemetry.i"
runner .block
.bend
''')
        errors = self.errors()
        self.assertIn("module declaration missing or mismatched", errors)
        self.assertIn("representative routine `run` missing", errors)

    def test_documented_imports_and_routines_must_match(self):
        path = self.root / inventory.INVENTORY.relative_to(inventory.ROOT)
        path.write_text(path.read_text().replace("`helper`", "`obsolete`"))
        self.assertIn("documented source surface drifted", self.errors())

    def test_duplicate_inventory_section_is_rejected(self):
        path = self.root / inventory.INVENTORY.relative_to(inventory.ROOT)
        text = path.read_text()
        path.write_text(text + "\n" + text[text.index("## "):])
        self.assertIn("duplicate inventory module section", self.errors())

    def test_unbalanced_macro_is_rejected(self):
        self.write("native/vm/helpers.i", "M .macro\n")
        self.assertIn("unclosed .macro", self.errors())


if __name__ == "__main__":
    unittest.main()

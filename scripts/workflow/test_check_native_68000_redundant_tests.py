#!/usr/bin/env python3

from __future__ import annotations

import contextlib
import importlib.util
import io
import sys
import tempfile
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = REPO_ROOT / "scripts/workflow/check_native_68000_redundant_tests.py"
SPEC = importlib.util.spec_from_file_location("check_native_68000_redundant_tests", SCRIPT_PATH)
assert SPEC is not None and SPEC.loader is not None
checker = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = checker
SPEC.loader.exec_module(checker)


class Native68000RedundantTestsInventoryTest(unittest.TestCase):
    def test_inventory_classifies_requested_categories(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            audit_path = root / "native/motorola68000/test.asm"
            retained_path = root / "native/motorola68000/amigaos/opforge-cli/run.asm"

            audit_path.parent.mkdir(parents=True, exist_ok=True)
            retained_path.parent.mkdir(parents=True, exist_ok=True)

            audit_path.write_text(
                "\n".join(
                    [
                        "main\t.block",
                        "    move.l d0, d2",
                        "    tst.l d2",
                        "    bne.s done",
                        "    move.l d0, d1",
                        "    tst.b d1",
                        "    beq.s widthSensitive",
                        "    jsr copyToken",
                        "    tst.l d0",
                        "    beq.s reviewed",
                        "    jsr unknownHelper",
                        "    tst.l d0",
                        "    bne.s unknown",
                        "    tst.b (a0)",
                        "    beq.s memoryProbe",
                        "    movea.l a2, a3",
                        "    movea.l a4, a5",
                        "    move.l token_len(a0), d3",
                        "    movea.l a0, a1",
                        "    tst.l d3",
                        "    beq.s lengthProbe",
                        "    movea.l a0, a1",
                        "    tst.l d4",
                        "    bne.s semanticProbe",
                    ]
                )
                + "\n",
                encoding="utf-8",
            )

            retained_path.write_text(
                "\n".join(
                    [
                        "opforgeNativeCliRun\t.block",
                        "    jsr dos.openInput",
                        "    tst.l d0",
                        "    bne.s inputOpened",
                    ]
                )
                + "\n",
                encoding="utf-8",
            )

            sites = checker.find_inventory_sites(audit_path, audit_path.read_text(encoding="utf-8").splitlines(keepends=True))
            retained_sites = checker.find_inventory_sites(
                retained_path,
                retained_path.read_text(encoding="utf-8").splitlines(keepends=True),
            )
            categories = {site.tst_line.strip(): site.category for site in sites}

            self.assertEqual(categories["tst.l d2"], checker.INVENTORY_CATEGORY_POST_WRITE_REDUNDANT)
            self.assertEqual(categories["tst.b d1"], checker.INVENTORY_CATEGORY_WIDTH_SENSITIVE)
            self.assertEqual(categories["tst.b (a0)"], checker.INVENTORY_CATEGORY_MEMORY_PROBE)
            self.assertEqual(categories["tst.l d3"], checker.INVENTORY_CATEGORY_LENGTH_OR_COUNT_PROBE)
            self.assertEqual(categories["tst.l d4"], checker.INVENTORY_CATEGORY_SEMANTIC_REGISTER_PROBE)

            post_call_categories = [site.category for site in sites if site.tst_line.strip() == "tst.l d0"]
            self.assertIn(checker.INVENTORY_CATEGORY_POST_CALL_REVIEWED, post_call_categories)
            self.assertIn(checker.INVENTORY_CATEGORY_POST_CALL_UNKNOWN, post_call_categories)
            self.assertEqual(
                retained_sites[0].category,
                checker.INVENTORY_CATEGORY_POST_CALL_RETAINED,
            )

    def test_inventory_summary_reports_category_counts(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            audit_path = root / "native/motorola68000/test.asm"
            audit_path.parent.mkdir(parents=True, exist_ok=True)
            audit_path.write_text(
                "\n".join(
                    [
                        "main\t.block",
                        "    move.l d0, d2",
                        "    tst.l d2",
                        "    bne.s done",
                        "    jsr unknownHelper",
                        "    tst.l d0",
                        "    bne.s fail",
                    ]
                )
                + "\n",
                encoding="utf-8",
            )

            stdout = io.StringIO()
            with contextlib.redirect_stdout(stdout):
                rc = checker.run([str(audit_path.parent), "--inventory-summary"])

            output = stdout.getvalue()
            self.assertEqual(rc, 0)
            self.assertIn("Inventory category counts:", output)
            self.assertIn(checker.INVENTORY_CATEGORY_POST_WRITE_REDUNDANT, output)
            self.assertIn(checker.INVENTORY_CATEGORY_POST_CALL_UNKNOWN, output)

    def test_inventory_ignores_block_headers_as_instruction_context(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            audit_path = root / "native/motorola68000/test.asm"
            audit_path.parent.mkdir(parents=True, exist_ok=True)
            audit_path.write_text(
                "\n".join(
                    [
                        "first\t.block",
                        "    jsr helperA",
                        "    tst.l d0",
                        "    bne.s failA",
                        "second\t.block",
                        "    tst.l d0",
                        "    bne.s failB",
                    ]
                )
                + "\n",
                encoding="utf-8",
            )

            sites = checker.find_inventory_sites(
                audit_path,
                audit_path.read_text(encoding="utf-8").splitlines(keepends=True),
            )

            self.assertEqual(len(sites), 2)
            self.assertEqual(sites[0].prev_line, "    jsr helperA")
            self.assertEqual(sites[1].prev_line, None)
            self.assertEqual(sites[1].next_line, "    bne.s failB")

    def test_write_mode_remains_conservative(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            audit_path = root / "native/motorola68000/test.asm"
            audit_path.parent.mkdir(parents=True, exist_ok=True)
            original_text = (
                "\n".join(
                    [
                        "main\t.block",
                        "    jsr unknownHelper",
                        "    tst.l d0",
                        "    bne.s fail",
                    ]
                )
                + "\n"
            )
            audit_path.write_text(original_text, encoding="utf-8")

            stdout = io.StringIO()
            with contextlib.redirect_stdout(stdout):
                rc = checker.run([str(audit_path), "--write"])

            self.assertEqual(rc, 0)
            self.assertEqual(audit_path.read_text(encoding="utf-8"), original_text)


if __name__ == "__main__":
    unittest.main()

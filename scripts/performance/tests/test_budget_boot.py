import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

SPEC = importlib.util.spec_from_file_location(
    "budget_boot", Path(__file__).parents[1] / "prepare_fs_uae_budget_boot.py")
boot = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(boot)


class BudgetBootTests(unittest.TestCase):
    def test_workbench_is_loaded_before_measurement_and_hook(self):
        original = b"SetPatch\nExecute S:User-Startup\nLoadWB\nEndCLI >NIL:\n"
        for mode in ("workbench", "standard-workbench", "installed-workbench"):
            result = boot.startup_sequence(original, mode)
            self.assertLess(result.index(b"LoadWB"), result.index(b"C:Avail"))
            self.assertLess(result.index(b"C:Avail"), result.index(b"tkpkg_debug_cli.hunk"))
            self.assertLess(result.index(b"tkpkg_debug_cli.hunk"), result.index(b"EndCLI"))
            self.assertEqual(result.count(b"tkpkg_debug_cli.hunk"), 1)
            self.assertIn(b"LoadWB\nIf VAL $RC EQ 0\n", result)
            self.assertIn(b"Quit 20\n", result)
            self.assertLess(result.index(b"workbench-start.txt"), result.index(b"C:Avail"))
        shell = boot.startup_sequence(original, "shell")
        self.assertNotIn(b"LoadWB", shell)
        self.assertIn(b"SetPatch", shell)

    def test_copy_preserves_installed_files_and_removes_only_old_hook(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            system = root / "installed"
            (system / "S").mkdir(parents=True)
            startup = b"SetPatch\nExecute S:User-Startup\nLoadWB\nEndCLI >NIL:\n"
            user = b"KeepThis\n;BEGIN OPFORGE_TKPKG_SMOKE\nOldHook\n;END OPFORGE_TKPKG_SMOKE\n"
            (system / "S/Startup-Sequence").write_bytes(startup)
            (system / "S/user-startup").write_bytes(user)
            (system / "WBStartup").mkdir()
            (system / "WBStartup/Utility").write_bytes(b"optional desktop utility")
            template = root / "input.fs-uae"
            template.write_text(f"[fs-uae]\nhard_drive_0 = {system}\ncpu = 68040\n")
            output = root / "copy"
            generated = boot.prepare(template, output, "workbench")
            self.assertEqual((system / "S/Startup-Sequence").read_bytes(), startup)
            self.assertEqual((system / "S/user-startup").read_bytes(), user)
            self.assertEqual((output / "system/S/user-startup").read_bytes(), b"KeepThis\n")
            self.assertEqual(list((output / "system/WBStartup").iterdir()), [])
            self.assertTrue((system / "WBStartup/Utility").exists())
            self.assertIn(f"hard_drive_0 = {output.resolve() / 'system'}", generated.read_text())
            with self.assertRaises(FileExistsError):
                boot.prepare(template, output, "shell")
            with self.assertRaisesRegex(ValueError, "outside"):
                boot.prepare(template, system / "recursive", "shell")
            boot.prepare(template, root / "desktop-copy", "installed-workbench")
            self.assertTrue((root / "desktop-copy/system/WBStartup/Utility").exists())

    def test_standard_workbench_preserves_os_and_disables_only_custom_startup(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            system = root / "installed"
            (system / "S").mkdir(parents=True)
            startup = (b"SetPatch QUIET\nBindDrivers\nMount DEVS:DOSDrivers/~(#?.info)\n"
                       b"LoadMonDrvs\nAddDataTypes REFRESH QUIET\nIPrefs\nConClip\n"
                       b"SYS:System/RexxMast\nExecute S:User-Startup\nLoadWB\nEndCLI >NIL:\n")
            user = b"Run CustomUtility\n"
            fixtures = {"S/Startup-Sequence": startup, "S/User-Startup": user,
                        "WBStartup/Utility": b"optional utility",
                        "Prefs/Env-Archive/Sys/wbpattern.prefs": b"desktop background",
                        "Devs/Monitors/Driver": b"installed display driver"}
            for name, data in fixtures.items():
                path = system / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(data)
            template = root / "input.fs-uae"
            template_text = f"[fs-uae]\nhard_drive_0 = {system}\ncpu = 68040\n"
            template.write_text(template_text)
            output = root / "standard"
            boot.prepare(template, output, "standard-workbench")
            copied = output / "system"
            expected = boot.startup_sequence(startup, "standard-workbench")
            self.assertEqual((copied / "S/Startup-Sequence").read_bytes(), expected)
            for line in startup.splitlines():
                self.assertIn(line, expected)
            self.assertNotIn(user, (copied / "S/User-Startup").read_bytes())
            self.assertEqual(list((copied / "WBStartup").iterdir()), [])
            for name, data in fixtures.items():
                self.assertEqual((system / name).read_bytes(), data)
                if name.startswith(("Prefs/", "Devs/")):
                    self.assertEqual((copied / name).read_bytes(), data)
            self.assertEqual(template.read_text(), template_text)
            receipt = json.loads((output / "boot.json").read_text())
            self.assertEqual(receipt["boot_mode"], "standard-workbench")
            self.assertEqual(receipt["wbstartup_programs"], "not run")
            self.assertEqual(receipt["user_startup"], "disabled")
            installed = root / "complete"
            boot.prepare(template, installed, "installed-workbench")
            self.assertEqual((installed / "system/S/User-Startup").read_bytes(), user)
            receipt = json.loads((installed / "boot.json").read_text())
            self.assertEqual(receipt["wbstartup_programs"], "installed")
            self.assertEqual(receipt["user_startup"], "installed without smoke hook")
            self.assertEqual(receipt["desktop_background"], "installed")

    def test_plain_background_removes_only_copied_pattern_preferences(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            system = root / "installed"
            fixtures = {"S/Startup-Sequence": b"SetPatch\nIPrefs\nLoadWB\nEndCLI\n",
                        "pReFs/ENV-ARCHIVE/sYs/WbPattern.Prefs": b"custom backdrop",
                        "pReFs/ENV-ARCHIVE/sYs/WbPattern.Prefs.UAEM": b"metadata",
                        "pReFs/ENV-ARCHIVE/sYs/screenmode.prefs": b"display mode",
                        "Prefs/Presets/Backdrops/image.png": b"backdrop image",
                        "Devs/Monitors/Driver": b"display driver"}
            for name, data in fixtures.items():
                path = system / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(data)
            template = root / "input.fs-uae"
            template_text = f"[fs-uae]\nhard_drive_0 = {system}\n"
            template.write_text(template_text)
            plain = root / "plain"
            boot.prepare(template, plain, "standard-workbench", plain_background=True)
            default = root / "default"
            boot.prepare(template, default, "standard-workbench")
            for name, data in fixtures.items():
                self.assertEqual((system / name).read_bytes(), data)
                if "wbpattern.prefs" in name.lower():
                    self.assertFalse((plain / "system" / name).exists())
                    self.assertEqual((default / "system" / name).read_bytes(), data)
                elif name != "S/Startup-Sequence":
                    self.assertEqual((plain / "system" / name).read_bytes(), data)
            self.assertEqual(template.read_text(), template_text)
            receipt = json.loads((plain / "boot.json").read_text())
            self.assertEqual(receipt["desktop_background"], "plain")
            receipt = json.loads((default / "boot.json").read_text())
            self.assertEqual(receipt["desktop_background"], "installed")
            with self.assertRaisesRegex(ValueError, "Workbench"):
                boot.prepare(template, root / "shell", "shell", plain_background=True)

    def test_ambiguous_boot_is_rejected(self):
        with self.assertRaises(ValueError):
            boot.startup_sequence(b"EndCLI\nLoadWB\n", "installed-workbench")
        with self.assertRaises(ValueError):
            boot.without_smoke_hook(b";BEGIN OPFORGE_TKPKG_SMOKE\n")


if __name__ == "__main__":
    unittest.main()

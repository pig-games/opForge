#!/usr/bin/env python3
"""Make disposable Shell/Workbench boot copies for native memory qualification.

The installed system and template are read-only inputs. Select the generated
template with OPFORGE_FS_UAE_CONFIG_TEMPLATE and the maintained 68020-8m profile
with OPFORGE_FS_UAE_MEMORY_PROFILE. The ordinary runner still stages Work: and
owns fresh START/DONE, explicit exit and exact-output verification.
"""
import argparse
import hashlib
import json
from pathlib import Path
import shutil


SMOKE_BEGIN = b";BEGIN OPFORGE_TKPKG_SMOKE"
SMOKE_END = b";END OPFORGE_TKPKG_SMOKE"


def without_smoke_hook(data):
    """Remove only the local test hook; leave all other installed setup intact."""
    if SMOKE_BEGIN not in data and SMOKE_END not in data:
        return data
    if data.count(SMOKE_BEGIN) != 1 or data.count(SMOKE_END) != 1:
        raise ValueError("ambiguous existing smoke hook")
    before, _, rest = data.partition(SMOKE_BEGIN)
    body, _, after = rest.partition(SMOKE_END)
    if SMOKE_BEGIN in body or SMOKE_END in before:
        raise ValueError("invalid existing smoke hook")
    return before + after.lstrip(b"\r\n")


def qualification_hook(mode):
    # Invoke the same staged shim as the maintained boot. No new proof protocol.
    return (f'FailAt 999\nEcho "{mode}" >Work:boot-mode.txt\n'
            'C:Version >Work:boot-memory.txt\n'
            'C:Avail >>Work:boot-memory.txt\n'
            'Work:build/tkpkg_debug_cli.hunk >Work:opforge_fsuae_smoke.stdout\n')


def workbench_start():
    # Check immediately: an Echo between LoadWB and this test would replace RC.
    return (b"LoadWB\nIf VAL $RC EQ 0\n"
            b'Echo "LoadWB returned 0" >Work:workbench-start.txt\n'
            b"Else\nEcho \"LoadWB failed\" >Work:boot-error.txt\nQuit 20\nEndIf\n")


def startup_sequence(original, mode):
    hook = qualification_hook(mode).encode()
    if mode in ("shell", "workbench"):
        desktop = workbench_start() + b"Wait 2 SECS\n" if mode == "workbench" else b""
        return (b"FailAt 999\nMakeDir RAM:T RAM:ENV\n"
                b"Assign T: RAM:T\nAssign ENV: RAM:ENV\n"
                b"SetPatch QUIET\nPath C: SYS:System\n" + desktop + hook + b"EndCLI >NIL:\n")
    # Keep normal SetPatch, drivers, preferences and user setup. The shim must
    # run AFTER LoadWB, unlike the installed user-startup test hook.
    lines = original.replace(b"\r\n", b"\n").splitlines(keepends=True)
    load = [i for i, line in enumerate(lines) if line.strip().lower() == b"loadwb"]
    if len(load) != 1:
        raise ValueError("Workbench boot requires exactly one standalone LoadWB")
    end = [i for i, line in enumerate(lines) if line.strip().lower().startswith(b"endcli")]
    if len(end) != 1 or end[0] <= load[0]:
        raise ValueError("Workbench boot requires EndCLI after LoadWB")
    lines[load[0]] = workbench_start()
    # Allow the newly started Workbench process to finish opening its screen.
    lines.insert(end[0], b"Wait 2 SECS\n" + hook)
    return b"".join(lines)


def prepare(template, output, mode, plain_background=False):
    if plain_background and mode == "shell":
        raise ValueError("plain background requires a Workbench qualification mode")
    template = template.resolve(strict=True)
    settings = {}
    for line in template.read_text().splitlines():
        key, separator, value = line.partition("=")
        if separator:
            settings[key.strip()] = value.strip()
    source = Path(settings["hard_drive_0"]).resolve(strict=True)
    if not source.is_dir():
        raise ValueError("budget boot requires a directory system drive")
    output = output.resolve()
    if output == source or source in output.parents:
        raise ValueError("output must be outside the installed system drive")
    original = (source / "S/Startup-Sequence").read_bytes()
    startup = startup_sequence(original, mode)
    user = source / "S/User-Startup"
    if not user.exists():
        user = source / "S/user-startup"
    user_data = without_smoke_hook(user.read_bytes()) if user.exists() else None
    output.mkdir(parents=True, exist_ok=False)
    system = output / "system"
    shutil.copytree(source, system)
    if plain_background:
        # AmigaOS paths are case-insensitive, unlike some host filesystems.
        for path in system.rglob("*"):
            relative = path.relative_to(system).as_posix().lower()
            if relative in ("prefs/env-archive/sys/wbpattern.prefs",
                            "prefs/env-archive/sys/wbpattern.prefs.uaem") and path.is_file():
                path.unlink()
    # Optional desktop utilities are outside the OS baseline. Keep the installed
    # configuration available as an explicit comparison, never edit the original.
    if mode in ("workbench", "standard-workbench"):
        startup_dir = system / "WBStartup"
        if startup_dir.exists():
            shutil.rmtree(startup_dir)
        startup_dir.mkdir()
    (system / "S/Startup-Sequence").write_bytes(startup)
    if user_data is not None:
        (system / user.relative_to(source)).write_bytes(
            b"; Custom User-Startup disabled for standard Workbench qualification.\n"
            if mode == "standard-workbench" else user_data)
    lines = []
    for line in template.read_text().splitlines():
        key, separator, _ = line.partition("=")
        lines.append(f"hard_drive_0 = {system}" if separator and key.strip() == "hard_drive_0" else line)
    (output / "template.fs-uae").write_text("\n".join(lines) + "\n")
    metadata = {"boot_mode": mode, "system_source": str(source),
                "desktop_background": "plain" if plain_background else "installed",
                "wbstartup_programs": "installed" if mode == "installed-workbench" else "not run",
                "user_startup": ("absent" if user_data is None else
                                 "disabled" if mode == "standard-workbench" else
                                 "installed without smoke hook" if mode == "installed-workbench" else
                                 "not run"),
                "source_startup_sha256": hashlib.sha256(original).hexdigest(),
                "generated_startup_sha256": hashlib.sha256(startup).hexdigest(),
                "memory_profile": "68020-8m",
                "proof": "ordinary native runner; boot snapshots are environment evidence only"}
    (output / "boot.json").write_text(json.dumps(metadata, indent=2) + "\n")
    return output / "template.fs-uae"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--template", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--mode", choices=("shell", "workbench", "standard-workbench", "installed-workbench"), required=True,
                        help="shell: minimal Shell; workbench: minimal Workbench; "
                             "standard-workbench: installed OS startup and desktop preferences "
                             "with optional WBStartup and custom User-Startup disabled; "
                             "installed-workbench: complete installed desktop")
    parser.add_argument("--plain-background", action="store_true",
                        help="remove only the copied Workbench WBPattern preference and its "
                             "host metadata; retain all other installed preferences")
    args = parser.parse_args()
    print(prepare(args.template, args.output, args.mode, plain_background=args.plain_background))


if __name__ == "__main__":
    main()

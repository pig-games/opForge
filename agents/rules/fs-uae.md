# Running FS-UAE checks

Use this guide for emulator-backed tests. The [native parity contract](native-rust-parity-porting.md)
is the sole definition of valid native evidence, including expected failures and
runner cleanup. The [repository workflow](../../documentation/workflow/README.md)
sets validation cadence. Prefer fast focused checks during development; return to
real-native confirmation when it answers the relevant correctness question.

## Environment and invocation

The maintained local setup uses the macOS FS-UAE application and requires GUI/process
access. Distinguish a host permission or initialization failure from a guest failure;
request needed execution permissions before interpreting a launch abort as a product bug.

Choose a focused test filter. The known-good invocation for this local setup is:

```sh
OPFORGE_FS_UAE_SMOKE=1 \
OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' \
OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' \
OPFORGE_FS_UAE_ARGS='{fsuae_config}' \
cargo test -p asm external_fs_uae_hunk_smoke -- --nocapture --test-threads=1
```

Set these values for the actual host; do not assume the example paths exist
elsewhere. Broader filters such as `external_fs_uae_` are deliberate qualification
choices. Keep environment settings attached to the test process and serialize
emulator execution with `--test-threads=1`. The coordinator lock is local to
one host process: also serialize separate test-binary or runner invocations.
Concurrent processes can interrupt each other's emulator; interrupted batches
provide no qualification or timing evidence and must be rerun serially.

## Guest stack for native CLI checks

`OPFORGE_FS_UAE_GUEST_STACK_BYTES` optionally sets the guest shell stack for
native CLI parity, prebuilt-image, correctness-only breadth and self-host checks.
It accepts decimal bytes from `4096` through `1048576`, aligned to 16 bytes.
Invalid values fail before emulator launch. The runner inserts `Stack <bytes>`
immediately after `FailAt 999`, before changing to `Work:` or emitting START.
When unset, no `Stack` command is inserted and the guest shell's stack is inherited.
The maintained local template's current shell default is 4096 bytes. This setting
does not change the saved FS-UAE template.

Use `OPFORGE_FS_UAE_GUEST_STACK_BYTES=65536` for current full self-host checks.
A current-source release self-build completed with fresh START/DONE, exit zero
and exact output at 65536 bytes after the inherited-stack run failed to complete.
This establishes a working stack setting, not the smallest required stack.
The breadth runner shares this setting and its validation, including isolated
retries. A shared-template control completed with exact Rust binary/Hunk output
through both transports at 65536 bytes; breadth previously ignored the override
and timed out after assembly printed its completion message without returning
to the shell. Do not treat that message or partial output as fresh DONE evidence.

## Inspecting failures

Host launch failures usually stop before guest output. Guest-side failures expose
stdout, stderr, exit and protocol details through the runner result; do not rely
on a removed run directory or a previous capture. Use the [triage guide](native-parity-failure-triage.md)
to choose the next discriminator. A stopped debugger or incomplete prefix scan is
localization evidence, not completed parity.

The [console debugger](../../documentation/fs-uae-console-debugger.md) is a separate,
explicitly enabled diagnostic tool with PTY and GUI-entry constraints. Its transcript
is not a native test result. Ordinary parity checks do not require debugger entry.

## Native memory qualification

The product target is an **8 MiB Amiga (8,388,608 bytes total RAM)**. Use
`OPFORGE_FS_UAE_MEMORY_PROFILE=68020-8m` for fit qualification: 2 MiB chip plus
4 MiB and 2 MiB fast banks, with no additional RAM or RTG memory. FS-UAE 3.1.66
rejects a single 6 MiB fast bank. The profile pins unrestricted CPU speed and
disables cycle-exact emulation; it measures memory fit, not hardware timing.
Existing expanded-profile results cannot establish fit. Require a completed
exact live Rust comparison and report remaining margin, including the OS,
executable/embedded package and stack, not just tracked heap.

Create disposable system-drive copies with
`scripts/performance/prepare_fs_uae_budget_boot.py --template <saved-template>
--output <fresh-directory> --mode shell` or `--mode workbench`. Select the
generated `template.fs-uae` through `OPFORGE_FS_UAE_CONFIG_TEMPLATE`. The latter
loads Workbench before invoking the existing runner shim, without desktop
preferences or WBStartup utilities. `--mode installed-workbench` preserves the
installed startup and utilities. Add `--plain-background` to remove only the
copied Workbench backdrop preference, retaining other preferences and startup
utilities. Use that combination for the ordinary 8 MiB desktop baseline; a
large decoded backdrop can consume several MiB independently of the assembler.
Report the desktop configuration with each fit result. JPEG file size alone
does not describe decoded display memory.

`--mode standard-workbench` retains the installed OS startup, drivers and
preferences but disables optional WBStartup programs and custom User-Startup.
It is a separate diagnostic configuration, not equivalent to the complete
installed desktop. The `boot.json` receipt records startup and backdrop choices.
The saved system drive and template remain unchanged. Copies and receipts
belong outside `target`; remove them when no longer needed.

The normal native runner retains `guest-memory.txt` before assembler launch
under this profile, including release runs. Disposable boots also capture
`boot-memory.txt` before the runner changes the guest stack. These snapshots
describe environment availability, not the assembler's peak. A6000 is the
reference for runtime/repeatability and disk I/O; its installed RAM is not
restricted by the current hardware launcher, so hardware success is not 8 MiB
fit evidence. The confirmed split is 6.5 MiB for the entire process and 1.5 MiB for OS; see
[the current performance plan](../../documentation/plans/native-performance-diagnosis.md).

## Other diagnostic profiles

For the binary-source harness, `OPFORGE_FS_UAE_MEMORY_PROFILE=2m` generates a
68020 configuration with 2 MiB chip RAM and zero fast/slow/motherboard/Zorro/RTG
memory. The template's machine model, ROM and system disk remain selected; use a
compatible local template. Unset or `existing` retains the normal expanded-memory
profile. The saved template is never modified.

`OPFORGE_FS_UAE_MEMORY_PROFILE=68020-10m` keeps the same 68020 and 2 MiB chip
RAM but adds 8 MiB fast RAM. Use it to investigate functional self-host parity
with more RAM than the product budget. It is an investigation profile,
not evidence that the 8 MiB product target has been met.

`OPFORGE_FS_UAE_MEMORY_PROFILE=68020-74m` keeps that 68020, 2 MiB chip and
8 MiB fast setup and adds 64 MiB Zorro III memory, matching the repository's
normal expanded-memory configuration. This diagnostic profile is useful when
investigating allocation failures caused by retained storage. It does not prove the
product memory budget or fit within the 8 MiB product target. Do not combine
measurements from this profile with the `68020-10m` native clock comparison;
keep that comparison on its existing profile.

`scripts/performance/prepared_source_native.py --binary-source --binary-only
--memory-profile 2m --blocks 32 --native-test <test-binary>` explicitly selects
binary-only qualification; the existing text runtime is not claimed to fit.
Add `--compare-memory` for a separate gated accounting run. It captures guest
Version/CPU/Stack/Avail before START and validates native allocation/free accounting.
Release timings must come from runs without that flag. Both modes retain fresh
challenge, completion, exact-output and ephemeral-artifact requirements.

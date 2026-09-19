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
emulator execution with `--test-threads=1`.

## Inspecting failures

Host launch failures usually stop before guest output. Guest-side failures expose
stdout, stderr, exit and protocol details through the runner result; do not rely
on a removed run directory or a previous capture. Use the [triage guide](native-parity-failure-triage.md)
to choose the next discriminator. A stopped debugger or incomplete prefix scan is
localization evidence, not completed parity.

The [console debugger](../../documentation/fs-uae-console-debugger.md) is a separate,
explicitly enabled diagnostic tool with PTY and GUI-entry constraints. Its transcript
is not a native test result. Ordinary parity checks do not require debugger entry.

## Experimental constrained-memory comparison

For the binary-source harness, `OPFORGE_FS_UAE_MEMORY_PROFILE=2m` generates a
68020 configuration with 2 MiB chip RAM and zero fast/slow/motherboard/Zorro/RTG
memory. The template's machine model, ROM and system disk remain selected; use a
compatible local template. Unset or `existing` retains the normal expanded-memory
profile. The saved template is never modified.

`scripts/performance/prepared_source_native.py --binary-source --binary-only
--memory-profile 2m --blocks 32 --native-test <test-binary>` explicitly selects
binary-only qualification; the existing text runtime is not claimed to fit.
Add `--compare-memory` for a separate gated accounting run. It captures guest
Version/CPU/Stack/Avail before START and validates native allocation/free accounting.
Release timings must come from runs without that flag. Both modes retain fresh
challenge, completion, exact-output and ephemeral-artifact requirements.

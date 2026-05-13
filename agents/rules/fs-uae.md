# FS-UAE Testing Rule Pack

Load this only when running or debugging FS-UAE-backed tests.

## Environment

FS-UAE tests launch the macOS FS-UAE application and need GUI/process access.
In sandboxed agent environments, request the required escalation before treating
an FS-UAE `SIGABRT` during initialization as a project failure.

## Known-good invocation

Prefer this one-shot form so the FS-UAE environment stays attached to the exact
`cargo test` process:

```sh
OPFORGE_FS_UAE_SMOKE=1 \
OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' \
OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' \
OPFORGE_FS_UAE_ARGS='{fsuae_config}' \
cargo test -p asm external_fs_uae_ -- --nocapture --test-threads=1
```

For a focused check, replace `external_fs_uae_` with a specific filter such as
`external_fs_uae_hunk_smoke`.

## Failure triage

- First distinguish host launch failures from Amiga-side payload failures.
- Host launch failures usually stop before guest output is captured.
- Amiga-side failures normally leave `Work/opforge_fsuae_*` files under the
  generated `target/fs-uae-*` directory. Inspect those before changing production code.

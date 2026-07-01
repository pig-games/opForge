# FS-UAE Testing Rule Pack

Load this only when running or debugging FS-UAE-backed tests.

For native Rust-to-68000 parity work, also load
`agents/rules/native-rust-parity-porting.md` and
`agents/rules/native-parity-failure-triage.md`.

FS-UAE is the Level D reality and confirmation gate. It is not the default
inner-loop microscope. Reproduce the real failure once, capture the exact
source/request/session evidence, move the discriminator to a focused host-side
boundary test when possible, and return to FS-UAE after the focused proof and
required quality gates pass.

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
- Run focused parity confirmations with `--test-threads=1`.
- Classify FS-UAE results as proof Level D and state what each test proves and
  does not prove.
- Do not treat a moved failure, a reduced fixture, or a prefix scan as proof of
  the corrected invariant.

# Step 36 compact shaped memo — early-screen no-go — 2026-09-08

Status: rejected, restored, and independently qualified; only the focused documentation commit and safe integration remain pending. The approved two-entry shaped-result prototype reached its first mandatory B10 stop/go boundary. Both the shaped-disabled reference and shaped candidate produced a fresh `CORPUS_START`, then exceeded the unchanged 120000 ms post-start timeout without `DONE`, an explicit guest exit, artifact comparison, or native-image identity. The experiment therefore supplies no parity, timing ratio, or performance gain.

Both attempts used B10 case SHA-256 `0e8c45ef53ca91471524bbb030720fc4bb4e7601fa30f5b12b0e94d02ec9484c`, corpus aggregate SHA-256 `8483da832f779eca8a4544580ef48f66feb55c7344311c0d14e43f0b04a9b46d`, shipped package SHA-256 `4d6a03d5718bc9380f02452294c404ec82e4e3716471ad5c95b349c8c6e25872`, profile off, longword/live/buffered modes, 250 ms polling, and the same command:

```sh
cargo test --locked -p asm external_fs_uae_native_production_corpus_parity -- --nocapture --test-threads=1
```

The screened prototype source aggregate was `a2389518cdc2c68970009826ae52255ec85751653e34700a31e99f8df458bd19`: `tkpkg_buffers.asm` `783a09f71959f0067d7536410ef226607954f7f204b5eb9f07b6d9f1cb747818`, `tkpkg_compact_table.asm` `cf170168470f482d67114d14e102134d1e120a6891380d6ed180c53fbe4dc235`, and `tkpkg_pipeline.asm` `f2691dd244fe46b52bc5e771f77d4b59411015eaaee9fe194151979170534ee5`. Reference and candidate used these identical prototype sources. The reference set `OPFORGE_COMPACT_SHAPED_LOOKUP=uncached`, which selected the assembly-time `OPFORGE_COMPACT_SHAPED_UNCACHED_REFERENCE` bypass; the candidate set it to `memo`. Zero lookup remained `memo` in both. The reference wrapper wall time was `163.44564370880835` seconds, including a 13.15-second compile, and its log SHA-256 is `2f6a54571832b91bcb0bee1d018483b2dadb54af60457be5a75ca74335f86cd1`. The candidate wrapper wall time was `149.5522035418544` seconds and its log SHA-256 is `8b5da22f0fbd12c101b57c12b63b7b3c62d1bc9737f729097261f5bf5428aea7`. Both test processes exited 101 because the fail-closed Rust test rejected the timeout. Whole-wrapper times are not native runtime and are not compared; neither run produced a native-image identity.

Before the screen, independent source/ABI review passed for the frozen prototype. Candidate and shaped-disabled host assemblies also passed in `28.98758858302608` and `29.02644579205662` seconds. Those host checks establish assembly and reviewed control-flow/storage contracts only. They do not establish native lifecycle behavior, cache hits, output parity, or performance.

The bounded static audit of the actual B10 main-source generator found 256 `lda #math.VALUE`/`immediate`, 256 `bne doneNNN`/`relative`, and 256 zero-shape `nop` requests. A separate two-entry shaped cache therefore had an ideal source-derived estimate of 510 hits among 512 shaped lookups after two cold fills. This is not a measured hit rate and did not predict completion. Module/include traffic was outside the count.

After the no-go, all three native prototype files and the corpus selector were restored byte-for-byte to Step35 commit `132e31b30018687ce547405d547aeff6777eaeef`. The final native and Rust production/test diff is empty. Expanded lifecycle, mixed CLI, B01, non-LSP Rust, and native gates were intentionally not run because the plan required stopping after an unsuccessful first screen. B10, Phase A closure, and all recorded output/frontend/metadata debts remain open.

The prior 1,608-to-8 source DOS-read reduction and Step31 test-profile Rust map gains remain separate unchanged measurements. Total Phase A end-to-end runtime remains unmeasured.

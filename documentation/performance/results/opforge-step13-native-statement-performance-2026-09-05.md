<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->

# Step13 native statement initialization performance report

This report records the Step13 driver evidence for the live statement-row
initialization change. It is an evidence report, not a qualification or final
performance claim. The machine-readable collection is
[`opforge-step13-native-statement-comparison-2026-09-05.json`](opforge-step13-native-statement-comparison-2026-09-05.json).

## Recorded result

All six B01 full/live runs completed with exact native/Rust artifact parity and
zero guest exit. Full mode median START-to-DONE time was **44.755913 s**
(range **44.723545–44.834725 s**, relative range **0.2484%**). Live mode median
was **37.055650 s** (range **37.016192–37.258021 s**, relative range
**0.6526%**). The observed live-vs-full improvement is **17.2050%**. Relative
noise is **0.6526%** and the recorded threshold is `max(5%, noise) = 5%`, so
this completed B01 comparison clears its measured threshold. The summed driver
wall time for all 16 attempts was **2,180.531798 s**; this is an overall test
cycle cost, not a per-case product speed claim.

The stable native image identities were full
`fnv1a64:ab0695d869853727` and live `fnv1a64:baa52910ef0385a4`, with distinct
mode identities. The six B03 attempts and both B10 attempts all reached the
unchanged 120 s post-start bound and remain failed/incomplete timeout evidence;
they provide no B03/B10 speed, parity, or non-regression claim. The fresh
513-statement capacity case proved fresh guest completion, guest exit 0, and
exact Rust-authoritative output. The early-error test proved its fresh protocol,
expected diagnostic, and expected guest exit 1. Both Cargo test processes exited
0. Their driver receipts are preserved in the collector’s per-row and raw-log fields.

## Mechanism and proof boundary

The implementation omits **30,800,000 bytes** of unused statement-arena startup
clear work while preserving the **308-byte** row size and the historical
six-byte declared/emitted range tail. The Level C host model covers split-range
arithmetic, all 24 fields, row reuse, count boundaries, and reset/retry ordering.
It models the store wrapper and call ordering with `storeStatementRecord` stubbed.
Rollback reuse and the caller’s pre-commit owner staging are source-audited only.
Fresh Level D B01 and 513-row results execute the real CLI/store/pass/emission
path; the early-error result proves its diagnostic boundary.

Source identities used by the collection are:

- engine `0160c9f210d8f2721e3a1ca0284b45aba06f73bee5e1bbf3789b73e16f1b0f75`
- native corpus harness `e337eb053a57d8b939f4d97fead6af1976cfff006f61d6d9a671ba617d650d19`
- host proof `4f939af00264164a6b21d7d9cf6d186580fac915f8d42c9b190a27a05c46aefe`

The successful host artifact is the r3 native image, **554,956 bytes**, SHA-256
`62df5815e259d731c3126a737aff6b6c513ad2fba4038d639bb3d6b1f045db9a`. The
reference image is **554,932 bytes**, SHA-256
`67830e9c1b31484dc6f7b900cda4b77ad94abcdb1838642c403ef01a30269298`.
Host-gate, build, and log identities are retained in the collector output;
the host-gates receipt records the session-clear host checks, statement
initialization host checks, and workflow gate as exit 0.
The host-gates receipt hash is
`5c678fc2da11d5c289bee6b32ecf5447fd9f1715b0cbcb96681faf646be24ce2`.
The build receipt hashes are `37d4abe1649fead4db568243ce37859852e332d55b636407d67522f7e2a40dfc`
(initial), `2f1592427576a600c69c0cf5fa8f54e61e7eea9e1cc47d3779b673521216921a`
(r2), `cbb6b73889f643e19c06b7533507647e357f6e0e96008fe3be8a9c4de3051232`
(r3), and `8680dab31d6e8cfe7c234b2c273b48cba112c9d0d26146c682304defe3caeb17`
(reference). The corresponding retained build-log hashes are
`e3fe400721574fdb9f0dfeb092d5e596e5ac893d3858627fd94752fcf5ab0803`,
`de3ae5c2cbc70b0ea59aa77cecbd7fc74bbfd0a23927e485f202fd08e0e5416c`, and
`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`.

## Provisional disposition

The independently reviewed foundation exception is applicable to focused
provisional retention under the amended Step13 plan: completed B01 evidence,
fresh live-513 and early-error Level D PASS, existing 24-field host proof and
native guards, and the retained B03/B10 failures. The first non-LSP gate’s
visibility snapshot was corrected after the three constants were made private,
restoring the original export budgets. The second 81,790 gate attempt was
canceled with exit **-15** after **213.3 s**; this is not treated as a gate
failure. The final non-LSP gate passed; final independent compliance review and the
focused commit remain pending. Full/live B03 completion
at the unchanged bound and full B01–B10 A-close qualification remain mandatory.
Step13 is provisionally qualified, subject to final compliance and its focused
commit. Phase A remains open.

## Rust gate qualification attempts

The first non-LSP Rust gate ended with terminal exit **101** after **1,588
passed tests**. Its export budget observation was **6,578 declarations** versus
6,575 and **128,416 name bytes** versus 128,293. Three new engine constants
independently account for the **123 packed name bytes**; declaration and name
capacities remain unchanged at **8,192** and **262,144**. The focused corrected
export-budget test passed, with the observed values retained in the evidence
JSON. The first receipt and raw log hashes are
`27960a314157f2517bbcc289d0f56a4863d055499de6f4593c2ed4f8b151f8bb` and
`b1fa0c59c0a3f57b755c81e2d79502fa13d9fdb32c642649e3f87be7bb25fcf0`.

The canceled corrected-gate log hash is
`fa16b5a556e98f3a6d6dec2ea8c21324c24254ebae85055ae0643dee6b3f83a0`.
The private-image equivalence receipt is
`c07b90e0cf3df63b6754209a9bd0c2c01728a81aac85d8aaddeaf7e7dac95cb8`; it
records byte-identical rebuilt images: live **554,956 bytes** and full
**554,932 bytes**. Independently recomputed FNV bridges remain full
`fnv1a64:ab0695d869853727` and live `fnv1a64:baa52910ef0385a4`, matching the
exact corpus receipts and preserving the prior measurement source hash while
recording corrected engine hash
`d608a3efddc98d9395e7b78508cdbcf1fb91e7464c643718fb760bd87e769b0c`.
Focused Item38 (6 tests), host proof (12 tests), formatter, and runtime-boundary
inventory all passed; their receipt/log hashes and the separate qualification
attempts are recorded under `qualification_attempts` in the machine-readable
evidence file. The final non-LSP gate result follows.

The final reviewed non-LSP Rust gate then passed with terminal exit **0** after
**1,589 assembler tests**; the receipt ran for **1,903.892450 s** and reported
**1,815.19 s** for the assembler test suite. Receipt SHA-256 is
`0a744c4eaed8389e55dfd5fd278334d6eb070a7dc776234b3ae61891328dfcf4` and log
SHA-256 is
`50cf884e2e1aed2d1c873f6f4f5adb2ecd3991c20970e5971542b28ef0e1b988`.
Final independent compliance review and the focused commit remain pending.

# Step 33 native leading-underscore label result — 2026-09-08

Status: focused host, measured capacity, native assembly/format, both fresh Level D proofs, full non-LSP Rust, staged-native, plan, workflow and independent compliance pass. Only the focused commit remains pending.

The authoritative source is the exact retained 243-byte Step32 fixture, SHA-256 `e0418c29e4f6d1b387760b2dc8d00a70f4e35f7fa7b47bbd69665f47adb46e63`, using `_private_name_longer_than_fifteen`. It selects CPU 68020, the shipped package `4d6a03d5718bc9380f02452294c404ec82e4e3716471ad5c95b349c8c6e25872`, exact BIN and whole-listing output. The prior fresh run failed before rendering with `OPC-NCLI038`/`039`/`010`; that receipt remains in the Step32 result.

The final native source `preprocessor_invocation.asm` is `e7027a8ee6df58c6fff13607c474b2928ed35692568823d220021b7df3ec25b7`. The correction adds only underscore acceptance before the existing first-byte ASCII letter range. The same retained source now completes a fresh guest protocol with explicit exit 0 and exact live Rust BIN/listing artifacts. The unchanged macro-invocation fixture independently completes with exit 0.

Measured capacity passes without relaxing limits: 1,729,053 loadable source bytes, 94,708 processed rows/3,582,109 bytes, 426 imports, and 6,506 public declarations/127,952 name bytes. No persistent storage was added. Runtime gain and total Phase A runtime remain unmeasured. Cumulative Step31 test-profile map gains of 63.17–77.51% and the earlier 1,608→8 source DOS read reduction remain unchanged and are not Step33 gains.

This slice does not close compact symbol/operator grammar, mutable/non-root metadata, generated/ORG footer, zero-image emission, section-start behavior, other A-close debt or Phase A itself. Exact receipts are retained in the adjacent JSON.

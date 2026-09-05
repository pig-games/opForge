# Step19 / A-source-buffer: bounded source reads

Retain this change under the user's explicit instruction that reducing source DOS calls from 1,608 to 8 is worthwhile. The active AGENTS.md remains binding. The ordinary 5% timing threshold is **not** marked passed. Full non-LSP Rust, native staged, workflow and focused checks pass; independent Step19 compliance and Step20 plan-quality review pass.

The native source reader now uses a private 512-byte buffer per active stream, with 34 slots covering the existing recursion limits (about 17 KiB). Parent unread bytes and cursors survive module/include calls. The byte-reference define preserves the original inline DOS reads; bootstrap and module-discovery readers are unchanged. The only product runtime claim here is for the completed focused fixture.

| Guest START-to-DONE | Byte reference | Buffered |
|---|---:|---:|
| Run 1 | 35.425082s | 34.155539s |
| Run 2 | 35.865896s | 34.053164s |
| Run 3 | 35.567552s | 34.072005s |
| Median | 35.567552s | 34.072005s |
| Range / median | 1.2394% | 0.3005% |

The median reduction is **4.2048%**. All three matched pairs pass, with stable and distinct mode images, the same inputs, exact live Rust binary output `[17, 34]`, fresh protocol completion and guest exit zero. Order alternates byte/buffered, buffered/byte, byte/buffered. Whole command/build/boot durations are preserved separately and are not used as performance samples.

Separate observer-on runs pass the strict approved decoder. Source DOS reads fall **1,608 → 8** (99.50% fewer calls). Physical source bytes read increase **1,606 → 1,654** because of 48 bytes of read-ahead, within the 511-byte bound. Both modes process 22 logical lines and emit the exact same output. The unused trailing module makes erroneous selected-range overconsumption observable; it is not silently omitted from the fixture.

The full source/include/module fixture is Level D binary-artifact parity. It covers a CR/LF split across byte 511/512, module prefix/refill and selected end, parent resumption after module and include processing, a short final refill, and EOF without a final newline. Both modes also require the exact existing native stdout `INCLUDE-LINE 1 1\n` and empty stderr. That message is a pre-existing Rust/native stdout difference; full stdout parity remains A-close debt. The existing native include success and missing-include cases also pass (128.38s whole command). Five restricted source-driven helper tests pass at Level C for modeled short reads, EOF/error, slot isolation and register restoration; modeled entry/exit is not proof of the production caller or real DOS.

Both frozen B10 modes were attempted once at the unchanged 120,000ms post-start limit and timed out. There is no completed B10, B10 speedup, non-regression or Phase A completion claim. Those results are preserved and do not become passing evidence under the user's retention instruction.

Two preliminary attempts remain excluded: the original larger fixture timed out with an unfair helper-call byte reference; the complete 512-byte control subsequently produced exact bytes but failed an invalid empty-stdout expectation. The final control restores inline reads, and the final complete fixture explicitly asserts the existing native message. Neither preliminary attempt enters the matched timing set. The frozen B10 input was never resized.

The companion JSON is generated from raw receipts and includes exact inputs, source/config/image identities, complete timing/counter records and failure dispositions. Rust harness formatting after measurement does not change fixture bytes or native production source. Broader B01–B10 and current native-group qualification remain at A-close.

The first full non-LSP Rust gate failed and is retained. The native source-budget snapshots needed their exact new values (1,672,135 loadable bytes; 92,032 processed rows; 3,524,240 processed bytes), all below unchanged capacity limits. An existing Item7 host test also reported a missing oracle file and passed in isolation. Source review found that timestamp-based temporary names were accepted through non-exclusive directory creation, permitting shared cleanup ownership. The helper now creates each directory exclusively and retries only collisions, with a fixed retry bound. This fixes the ownership defect without serializing tests; a collision is consistent with the missing file, but that particular failure was not directly reproduced. The corrected full non-LSP Rust gate passed in 267.05 seconds. The first failed gate remains separate; no running gate was restarted.

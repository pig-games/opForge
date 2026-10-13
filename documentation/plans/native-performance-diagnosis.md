# Native runtime and memory diagnosis

Current assignment: measure and analyse resource growth before selecting further
optimizations. Parity implementation is paused for this pass. Preserve shared VM,
numeric source and package boundaries; no CPU-specific shortcuts in generic code.

## Fixed workload and baseline

Use source `7a903cf8`, manifest `fnv1a64:ec940cb60e2202f6`: 194 mapped inputs,
2,580,296 bytes including the unchanged 454,890-byte m68020 package. Bootstrap
and output both embed that package. Complete expected Hunk is 706,888 bytes,
SHA-256 `5d5e6f10c15dacf4b8b3840119380032755551d51eafe897bcb668985f4f19ee`.

- Release FS-UAE 68020/74 MiB: 3,512.267856959 seconds (58m32.27s), fresh exit
  zero and exact complete live Rust output.
- Release A6000: 118 seconds, fresh exit zero and the same complete output;
  guest resolution is one second, transfer excluded. Hardware receipt and exact
  case identity are in the [self-host evidence](compact-frontend-vm-boundary.md#physical-a6000-execution).
- Release image: 706,888 bytes; linked reservation: 740,660 bytes. Neither is
  peak process memory.

Reuse this current release baseline. A separate full instrumented run uses the
identical source/package/command/oracle, with allocation tracking, phase clocks,
binding samples, template work counts and input-reader counts. Dense tokenizer
opcode telemetry is disabled. Instrumented time measures attribution and overhead,
not release speed. Require fresh completion, exit zero and the complete Rust match.
Retain measurements outside `target`, then clean the completed build/test batch.

## Analysis to complete

Earlier successful release A6000 receipts show a workload trend, not isolated
change costs: 103 files / 1,509,170 input bytes took 35 seconds; 121 files /
1,780,288 bytes took 49 seconds; the current 194 files / 2,580,296 bytes took
118 seconds. Input totals include assets, and implementations/capabilities differ.
Growth in time exceeds growth in input bytes across these observations, which
supports investigating repeated work; it does not establish a specific cause.

1. Report preparation, discovery, binding, tokenization, expression preparation,
   finalization and assembly costs; nested observations must not be added to the
   exclusive stage totals.
2. Separate tracked heap live/peak/cumulative allocations from executable/static
   reservation. Identify the peak phase and preparation-to-assembly lifetime drop.
3. Examine captured source, composed names, macro/binding plans, numeric execution
   state and output ownership. Distinguish used bytes, capacity slack, duplicate
   owners and transient growth overlap. Existing aggregate telemetry cannot assign
   peak bytes or copy traffic to individual owners.
4. Compare earlier identified measurements only where input and mode match.
   Smaller older projects and failures after about 40 minutes are not full-build
   baselines. Normalized trends can guide investigation but cannot isolate causes.
5. Rank candidates by measured importance, expected runtime/RAM benefit, code
   complexity and correctness risk. Identify missing evidence before choosing a
   change; use focused representative controls for subsequent before/after runs.

## Initial hypotheses, not optimization decisions

- Binding: repeated template candidate selection and scope/name resolution.
- Memory: composed spelling and plan arenas, geometric slack and growth copies,
  preparation/export overlap, and retained captured source regions.
- Discovery: declaration-only byte/line scanning followed by full preparation.
- Assembly: repeated structural traversal within necessary semantic/layout replay.
- Finalization: binding candidate validation, remapping and compaction.

Preserve failure-atomic allocation, stable numeric identity, longest-owner lookup,
active branch/loop semantics, include provenance and relocation/layout behavior.
Each selected optimization needs its own unchanged-input correctness comparison,
release timing, separate heap measurement and code-quality review. This pass does
not claim complete language/family parity or fit within the 2 MiB target.

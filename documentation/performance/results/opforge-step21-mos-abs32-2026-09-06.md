# Step21 — MOS absolute-long relocation package

Step21 / A-mos-abs32 adds the missing MOS-family `fix.abs32` role using the existing neutral semantic VM. The program emits four little-endian bytes and an absolute portable relocation for target-bearing values. Generic native code now preserves package-owned encoding through explicit-section addend normalization and Hunk emission; no CPU-specific semantic branch is added.

Performance contribution: this repair enables the linker workload to execute through a previously rejected package operation, allowing qualification of earlier performance work. No runtime speedup is claimed.

## Evidence

The current baseline `f1b00e32` reproduced B09 with fresh guest completion and exit one, `OPC-NCLI022` followed by `OPC-NCLI020`. This is a completed failure, not a timeout. The original workload converts its pointer to `.long data_start` for Hunk relocation. Native requests `fix.abs32`, and deliberately rejects a symbolic target when the active package lacks that role. MOS supplied only `enc.u8`, `enc.u16le` and `fix.rel8` before this repair.

The Level B host proof executes the serialized MOS program against a live Rust cross-section `.long target + 5` oracle, with a nonzero target offset/addend. It passes byte-order and full portable fixup comparisons. This proves the package contract, not native execution or timing. An initial test compile error used a u32 slice offset; converting it to usize corrected the harness. Both logs are retained.

The generated package increases from 368,278 to 368,316 bytes. The original corpus manifest is preserved. The separate candidate manifest records the new package/corpus hashes; a direct comparison confirms the complete cases arrays are identical, including source identities, arguments and all expected artifacts. No old measurement is relabeled under the new package. Future matched runtime measurements must hold the repaired package constant.

Fresh B09 candidate completed far enough to compare all eleven artifacts, with empty captured streams, but eight artifacts differed. All eleven were present; none was reported missing. This is not B09 parity. The original unresolved-label failure moved to output differences, which remain open. A subsequent source audit found directive metadata handling overwrote package bytes using host byte order; the current candidate now preserves the exact four package bytes across that call. The first focused native BIN/Hunk case, `.long target + 5` with target at data+1, completed with guest exit one and an unresolved-label event during the second pass-two visit. That separate compound-expression failure is retained; it is not evidence against the host package bytecode proof. The next exact-artifact case uses B09's bare-symbol `.long target` with target at data+1, preserving a nonzero endian/relative-addend discriminator. The corrected native path passed a fresh exact BIN/Hunk test with a nonzero addend (63.89 seconds whole test). Final private-visibility confirmation and B09 disposition are recorded below.

## Hypothesis ledger

| ID | Hypothesis | Evidence | Status | Next discriminator |
|---|---|---|---|---|
| S21-1 | Missing finalized flags cause lookup failure. | Exact engine name lookup does not read those flags; the actual B09 path uses `.long`, not the initially inspected `.word`. | falsified as the proposed direct cause | No finality edit. |
| S21-2 | Missing MOS `fix.abs32` rejects the symbolic long directive. | Current package definitions lack the role; native explicitly fails for a target-bearing missing role; baseline B09 reproduces; serialized replacement matches Rust. | confirmed at package boundary | Candidate reaches all eleven outputs but eight differ; byte-preservation and relative-addend boundaries require direct proof. |

Phase A remains open. Other corpus failures, negative diagnostic debt and full qualification belong to A-close. No formal finding closure is claimed here.

## Final native boundary confirmation

The approved correction uses a 24-byte nonoverlapping frame, accepts a single zero-offset four-byte fixup, and sends the layout-normalized scalar through the same package program without a target reference. Re-encoding must return exactly four bytes and zero new fixups. A bounded per-slot flag tells selected-section Hunk output to preserve these bytes. Allocation clears each flag, count resets hide stale slots, and flat-source records cannot be marked. The two helper APIs are public; their branch labels and the 256-byte flag array are private. No instruction/flat transport behavior or CPU-specific native semantics were added.

The final build passed fresh Level D BIN/Hunk parity in 64.10 seconds whole-test time. This proves the bare-symbol nonzero-addend invariant, not compound expressions or runtime speed. The host oracle also proves non-target re-encoding creates no extra relocation.

Full B09 then compared all eleven artifacts: eight match exactly; `full.hex`, `full.map`, and the listing differ. This reduces the earlier eight mismatches to three but does not pass B09. All original failures remain recorded. The compound-expression failure also remains open. The non-LSP Rust gate passed in 307.155961584 seconds after the stale package digest pin was updated to the explicit candidate identity. The original failed gate (1,594 assembler PASS, one digest-pin failure) remains recorded. The corrected focused test and all 1,595 assembler tests pass; LSP stays deferred. Workflow/native gates and independent review pass; commit follows final artifact validation.

Step performance gain: no measured speedup; the repair removes a correctness barrier to qualification. Total Phase A runtime gain: unmeasured for the current build, because no matched initial/current series exists and package identity changed. The retained source-I/O mechanism reduces DOS calls from 1,608 to 8 on the Step19 fixture; this is not a whole-product runtime total.

The next item fixes the layout accumulator clobbered by table-pointer helpers: the retained sizes explain B09 used25 versus live Rust13 (correcting the initial source-audit estimate17) and BSS base$800D versus$8009. HEX segmentation follows; symbol-map and listing differences remain separate debt. No performance percentage is inferred from this source audit.

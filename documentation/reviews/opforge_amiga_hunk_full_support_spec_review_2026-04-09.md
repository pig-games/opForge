# Review Report

## Scope

Review [documentation/opForge-amiga-hunk-full-support-spec-v0_2.md](documentation/opForge-amiga-hunk-full-support-spec-v0_2.md) for AmigaOS regular executable Hunk-format correctness against the current opForge implementation in [crates/opforge-vm/src/output_hunk.rs](crates/opforge-vm/src/output_hunk.rs), [crates/opforge-vm/src/output_model.rs](crates/opforge-vm/src/output_model.rs), the Hunk coverage in [crates/opforge-asm/src/tests.rs](crates/opforge-asm/src/tests.rs), and the shipped AmigaOS examples [examples/motorola68000/amigaos/helloworld.asm](examples/motorola68000/amigaos/helloworld.asm) and [examples/motorola68000/amigaos/writefile.asm](examples/motorola68000/amigaos/writefile.asm). This was a full spec review, not a branch-diff review.

## Findings

### RVW-2026-04-09-001

- Severity: medium
- File: [documentation/opForge-amiga-hunk-full-support-spec-v0_2.md](documentation/opForge-amiga-hunk-full-support-spec-v0_2.md), [crates/opforge-vm/src/output_hunk.rs](crates/opforge-vm/src/output_hunk.rs), [crates/opforge-asm/src/tests.rs](crates/opforge-asm/src/tests.rs)
- Issue: The spec frames the current blocker mainly as the Hunk writer's assigned-base requirement, then requires `format=hunk` to succeed for sections without pre-assigned final bases. The current live path has two separate gates: pass1 still rejects sections referenced by `.output` unless they are explicitly placed, and the writer separately rejects missing `base_addr`. The boundary case that says `.output "x", format=hunk, sections=code,data` must succeed without assigned final bases does not say whether `.place` remains mandatory or whether truly unplaced sections become legal.
- Why it matters: A derived implementation can remove only the writer-side base check and still fail the stated v0.2 success condition because the earlier placement gate remains active. That is a correctness problem for the spec's main follow-on goal, not just a wording nit.
- Fix direction (one direction only; resolve competing options before finalizing): Tighten v0.2 to keep explicit `.place` as a requirement while removing dependence on fixed final base addresses, and state that the pass1 placement gate remains in scope while the writer-side assigned-base prerequisite is removed for supported relocatable Hunk emission.

### RVW-2026-04-09-002

- Severity: medium
- File: [documentation/opForge-amiga-hunk-full-support-spec-v0_2.md](documentation/opForge-amiga-hunk-full-support-spec-v0_2.md), [crates/opforge-vm/src/output_hunk.rs](crates/opforge-vm/src/output_hunk.rs), [documentation/opForge-amiga-hunk-output-component-spec-v0_1.md](documentation/opForge-amiga-hunk-output-component-spec-v0_1.md), [documentation/opForge-amiga-hunk-output-component-implementation-plan-v0_1.md](documentation/opForge-amiga-hunk-output-component-implementation-plan-v0_1.md)
- Issue: The spec makes `HUNK_RELOC32` target-segment ordering and preserved `sections=` order normative, but it drops the current baseline rule that empty selected non-BSS sections are omitted deterministically before Hunk emission. The shipped writer still skips those sections, and that choice affects emitted segment count and the target indices used in relocation groups.
- Why it matters: If a follow-on implementation changes empty-section handling without the spec saying so, segment numbering, `segment_count`, first-segment validation, and relocation target indices can all change while the implementation still believes it preserved order. That makes the claimed shipped subset an unstable floor.
- Fix direction (one direction only; resolve competing options before finalizing): Carry the current empty non-BSS omission rule forward explicitly and define `HUNK_RELOC32` target indices in terms of the emitted segment list so the shipped subset remains the compatibility floor.

### RVW-2026-04-09-003

- Severity: medium
- File: [documentation/opForge-amiga-hunk-full-support-spec-v0_2.md](documentation/opForge-amiga-hunk-full-support-spec-v0_2.md), [examples/motorola68000/amigaos/helloworld.asm](examples/motorola68000/amigaos/helloworld.asm), [examples/motorola68000/amigaos/writefile.asm](examples/motorola68000/amigaos/writefile.asm), [crates/opforge-asm/src/tests.rs](crates/opforge-asm/src/tests.rs)
- Issue: The notation section treats forms such as `LEA label,A1`, `PEA label`, and `MOVE.L D0,label` as covered cases where the intended encoding is unambiguously a relocatable absolute long. The shipped baseline does not prove that today: the current examples still use explicit `.L` absolute-address notation for those forms, while the already-landed bare-symbol support is a different class such as `MOVE.L #label,D1` and longword data references. On baseline 68000 syntax, the bare absolute-address forms are the ones that remain ambiguous if absolute word selection is still available.
- Why it matters: As written, the spec can drive an implementation that silently changes addressing-mode resolution rules under the label of “natural notation,” instead of either requiring explicit syntax or first defining a precise family-level disambiguation rule. That is a format-correctness risk because it can change which relocation-bearing encoding gets emitted.
- Fix direction (one direction only; resolve competing options before finalizing): Split the notation contract into a baseline-versus-target matrix, keep currently ambiguous bare absolute-symbol forms explicit until the spec defines one canonical disambiguation rule, and reserve immediate bare-symbol and longword data forms as the first unambiguous covered cases.

### RVW-2026-04-09-004

- Severity: medium
- File: [documentation/opForge-amiga-hunk-full-support-spec-v0_2.md](documentation/opForge-amiga-hunk-full-support-spec-v0_2.md)
- Issue: The spec still ends with an `Open Questions` section whose listed choices materially affect implementation scope: how broad the first notation slice is, whether anything beyond `HUNK_RELOC32` is needed before object-file work, and whether memory-type customization stays deferred. Those are planning decisions, not optional editorial follow-ups.
- Why it matters: In this repository, unresolved open-question sections are a workflow quality problem because they leave materially different implementation paths available and weaken the artifact as a planning input.
- Fix direction (one direction only; resolve competing options before finalizing): Replace the `Open Questions` section with explicit decisions that keep the first notation slice tightly enumerated, defer additional relocation kinds beyond `HUNK_RELOC32`, and keep memory-type customization out of scope until after generic fixup and notation work lands.

## Testing Gaps

- There is no byte-level test that proves multi-target or multi-offset `HUNK_RELOC32` serialization order, even though the spec makes target-segment ordering part of the contract.
- There is no focused Hunk test for addended data expressions such as `.long label + constant` or `.long label - constant`, even though the spec elevates that shape into the acceptance criteria.
- There is no negative notation test that pins current behavior for bare `LEA label,A1`, `PEA label`, or bare absolute destination forms, so the baseline for future notation work is still partly implicit.
- The current AmigaOS examples are covered by the general example-assembly workflow, but this review did not find default-gate tests that validate their emitted Hunk relocation payloads at byte level or under a loader.

## Residual Risks

- This review relied on repository code and test evidence, not on fresh FS-UAE or real AmigaOS execution, so runtime loader behavior for broader relocation layouts remains indirectly validated.
- The v0.1 Hunk spec and implementation-plan documents still describe relocation-hunk emission as deferred, while the current code already emits a narrow `HUNK_RELOC32` path. That stale documentation can confuse future readers even if this v0.2 spec is corrected.
- I did not audit every 68000 operand family that might eventually participate in relocation capture, so the exact edge of the shipped instruction-relocation subset still depends partly on implementation inspection rather than a single published capability matrix.

## Brief Summary

The spec is directionally aligned with the current regular-executable Hunk implementation and correctly identifies the important follow-on themes: generic fixup capture, section-relative addends, broader `HUNK_RELOC32` coverage, and notation cleanup. The main problems are baseline-definition gaps that affect correctness of derived implementation work: the document does not yet distinguish the pass1 placement gate from the writer's assigned-base gate, it drops the shipped empty-section emission rule that affects relocation target indices, and it overstates the current unambiguity of several bare-symbol 68000 address forms.
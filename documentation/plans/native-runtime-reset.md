# Native runtime direction and migration plan

Status: active direction. The compact binary-source runtime is a successful
experimental foundation, but it implements only a bounded language subset and is
not yet the normal native CLI path. Its current representation, measurements and
reproduction details are in the
[binary-source runtime note](prepared-source-experiment.md). Git history contains
the completed W1–W3, R1 and M1–M8 investigation records.

The active [operating contract](../../AGENTS.md),
[workflow](../workflow/README.md),
[native assembly guide](../../agents/rules/native-68000.md) and
[native parity contract](../../agents/rules/native-rust-parity-porting.md) govern
the work. This document records the current direction and active migration order;
it does not grant authority for future steps by itself.

## Product goal

The native assembler should be able to assemble itself on any Amiga with a 68020,
AmigaOS 3.1 or newer and 2 MiB installed RAM in at most 15 minutes, preferably much
faster. Qualification must include source loading, preparation, layout and output
in a runnable executable. Emulator results remain development evidence until a
baseline clock, storage and physical machine are chosen and measured.

Optimize the shared VM-based design across supported source targets. The 68020 is
the execution-platform floor, not a reason to move CPU-family semantics into the
native core. Rust remains the executable semantic reference and fast measurement
laboratory. Native layouts should use simple bounded memory blocks suited to the
platform rather than mirror Rust data structures.

Canonical packages remain authoritative. A derived runtime package may later
trade preparation time, size or CPU-specific layout for execution speed, but it
must be reproducible from the current canonical package and may not become a
second hand-maintained source of target semantics. Before 1.0, update producer and
consumers together and retain only the latest bytecode or package contract.

## Current architecture direction

Use controlled replacement: keep the existing native CLI as a correctness
reference while a compact path assumes one complete responsibility at a time.
Each increment must assemble a meaningful case end to end, compare with live Rust,
and identify which old work it makes unnecessary. Once the replacement is
qualified and integrated, remove the superseded path rather than maintaining two
native products indefinitely.

The compact path has established these principles:

- Tokenization produces the authoritative binary source line by line. Later
  phases consume numeric identities, structured values and compact expression
  programs; source text may be reread only to produce diagnostics.
- Stored binary representations contain offsets or numeric IDs, never process
  pointers. Every offset has a declared base and validated bounds, so moving a
  block does not require patching it.
- Instruction identities are normalized package-owned IDs. Aliases share an ID;
  semantic qualifiers remain explicit and IDs are unrelated to machine opcodes.
- Symbol identity, scoped binding and pass-dependent value are separate. Immutable
  package/source preparation is kept apart from mutable layout, CPU state and
  fixups.
- Preparation scratch is released before assembly. Allocations use measured
  requirements and bounded growth, with transient overlap included in accounting.
- Conditional telemetry uses reusable macros and contributes no code, data or
  imports to release builds. Instrumented work counts and release timing are
  reported separately.

New assembly should use opForge structs, macros, lists, loops and other language
features where they improve clarity. Names inside a module should be short rather
than repeat module qualification. Compact code and small files are design results,
not line-count exercises.

## Breadth migration plan

The current priority is language breadth rather than another isolated hot-spot
optimization. Each item is an inspectable increment, refined from evidence before
implementation:

1. **F1 — implemented: named constants and required expressions.** Added `name = expression`
   for practical standalone routines. Resolve immutable constants at their pass-one
   definition and verify the same value in pass two. Earlier constants, labels and
   the program counter may be referenced; forward or otherwise deferred constant
   dependencies reject explicitly in this fixed two-pass increment. Existing unary
   `+`/`-` and binary `+`, `-`, `*` form the initial expression set.
2. **F2 — package-owned operand shapes and predicates.** Carry the structural and
   register and rejection predicates required by indexed/register operands through
   the capsule and native selection boundary. Prove the same package-owned decision as Rust; do not accept
   unchecked token pairs or add CPU-specific recognition to generic native code.
3. **Expression and deferred-binding breadth.** Add the operators and dependency
   resolution required by the next real cases, including explicit cycle behavior,
   without freezing layout-dependent values.
4. **Files, modules and scopes.** Give symbols stable scoped identities and define
   include/module lifetime without restoring string lookup during assembly.
5. **Source expansion.** Encode macro, conditional and loop syntax once as binary
   records. Expansion and control flow that depend on layout, the program counter
   or symbols must evaluate against the appropriate pass state without falling
   back to source text. Preserve macro-instance and source provenance for binding
   and diagnostics.
6. **Structured language values.** Add structs, lists and the expression/value
   forms needed by representative sources, using compact runtime representations.
7. **General layout and emission.** Support sections, discontiguous origins,
   relocation/fixups and convergence-sensitive instruction selection without
   caching state-dependent results.
8. **Product integration.** Feed the compact preparation/execution path from normal
   native package loading and the CLI, qualify representative projects, then remove
   the old responsibilities it replaces.

Select cases for semantic coverage and realistic work, not because they happen to
fit the implementation. Use more than one source-target family where the boundary
is intended to be generic. A complete small case is preferable to a wide set of
helpers that cannot produce an artifact Erik can inspect.

## Increment contract

For every increment, state the hypothesis, reference behavior, resource budget
and stop condition. Preserve a working reference path during the experiment.
Correctness evidence includes fresh native completion, explicit zero or expected
failure exit, exact live-Rust artifacts and cleanup checks. Production behavior
must never depend on a fixture, benchmark name, path or expected result.

Measurements use focused, reasonably complex inputs. Do not run the non-completing
native self-host case or extend timeouts to obtain a result. The existing limits
are 10 seconds after guest `START`, 60 seconds per invocation and 150 seconds per
batch. Report source and binary-record bytes, executable and linked size, retained
and peak owned memory, preparation work and uninstrumented elapsed time. A timeout,
launcher success or partial capture is not completion.

At each coherent checkpoint, keep the tree runnable, record remaining limits and
make a local recovery commit. Broad qualification belongs at meaningful integration
boundaries. Pushes remain separately authorized.

## Current decision boundary

F1 is implemented and its bounded native correctness checks pass. Its positive cases are a purpose-written 6502
byte-reversal routine (`reverse-byte.asm`, 10 bytes of code, using zero-page
destructive scratch) and a 68000 range-check routine, plus generic data cases for
negative constants, chains, label differences and program-counter references. They are
standalone representative routines, not claims that an existing full application
now assembles.

The hypothesis is that definition-order immutable constants fit the current
numeric-symbol and compact-expression boundary without reintroducing textual
lookup or another retained buffer. Success requires exact live-Rust/native output
for both routines, the focused constant/data contracts, and unchanged completion
of the existing bounded workload. Reject unsupported dependencies explicitly.
Stop and review if the cases require a general dependency resolver, new text lookup
or additional owned preparation storage; those would change the scope rather than
finish F1. Review the complete cases, representation cost and correctness evidence
before starting F2.

The attempted 6502 page-copy case remains an expected native rejection. Its indexed
operands depend on `direct_x`/`direct_y` recognition currently owned by the Rust
family parser; the capsule does not yet carry the corresponding package-owned
structural and register predicates. Mapping an unchecked token pair or recognizing
6502 syntax in generic native code would violate the architecture boundary. This
defines F2 rather than expanding F1.

F1 also exposed two expression issues. Loading a negative symbol value through the
shared evaluator treated it as unsigned; F1 applies the signed correction only in
the compact path. Cyclic constants currently pass in the Rust 6502 reference due to
provisional zero values and pass-two updates, while native fails closed. Record this
as a reference gap for the later dependency resolver; do not imitate that behavior
or broaden F1 to repair the full Rust contract.

A second F2 case is `move.l d0,d1`. The package already contains the valid
register-copy recipe, but a higher-priority rejection candidate has an unlowered
register-class predicate. Native must evaluate or safely exclude that predicate
before trying the valid row; it must not skip unsupported candidates wholesale.
The register-pair case is retained as an expected rejection. The F1 range check
uses D0 directly for its calculation and return value (22 bytes of code), so it
does not claim register-copy coverage.

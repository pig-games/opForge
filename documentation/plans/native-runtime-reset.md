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
2. **F2 — implemented: package-owned operand shapes and predicates.** Carry the structural and
   register and rejection predicates required by indexed/register operands through
   the capsule and native selection boundary. Prove the same package-owned decision as Rust; do not accept
   unchecked token pairs or add CPU-specific recognition to generic native code.
3. **F3 — active: bit operations and forward absolute constants.** Add `&`, `|`,
   `^`, `~`, `<<` and `>>` with canonical precedence, and resolve forward immutable
   constant chains that are independent of layout. Detect cycles and missing names.
   Keep definition-site PC/label-dependent constants source-ordered; broader
   dependency/layout convergence follows as a separate increment.
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

F1 and F2 cover complete standalone byte-reversal, page-copy and range-check
routines, plus indexed-address and register-pair boundary cases. These are
representative small sources, not full-application or native CLI qualification.
The [runtime note](prepared-source-experiment.md) records current proof and costs.

F2 adds canonical package predicates for indexed operands and lowers the required
register names/classes into numeric runtime metadata. Native can exclude a
higher-priority rejection only when a known package register conclusively fails
one of its match predicates. Unknown names and unsupported predicates remain
fail-closed. Semantic operand encoding and table opcode emission both execute;
only a verified identity table is elided. The normal Rust m6502 route consumes the
same new package rows; other MOS CPU variants retain their existing specialized
selection routes until migrated and qualified.

The next breadth decision is expression and deferred-binding coverage, selected
from complete practical sources. The current fixed two-pass experiment rejects
forward constant dependencies and cycles. Rust currently accepts some cyclic
6502 constants through provisional zeros and pass-two updates; define explicit
cycle behavior before extending that boundary, rather than copying the defect.
Files/modules, expansion and product integration remain later increments. Do not
start them automatically from this plan.

## F3 increment contract

Hypothesis: masks/shifts and forward absolute constants let practical routines use
clear symbolic configuration without repeated text parsing or repeated full-source
resolution passes. Index numeric definitions once, walk compact expression
references with an explicit dependency stack, and evaluate each absolute constant
after its dependencies resolve. Reuse symbol storage during this prelayout phase;
release bounded offset-based scratch before layout. Never publish an expression
offset or provisional value as a resolved symbol.

Prove complete small mask/configuration routines for m6502 and m68000 against
live Rust, including reversed declaration order, shared dependencies and signed
boundaries. Explicit negative coverage includes cycles (including PC-tainted
cycles), missing names, duplicate/colliding definitions, arithmetic range failure
and unsupported forward layout dependencies. Preserve earlier-label and
definition-site-PC behavior. Rust's existing cyclic-constant defect is not an
oracle to reproduce.

The compact expression path retains its checked signed32 range. Operator
precedence and shift-count behavior follow the canonical Rust language; results
outside the supported range reject rather than truncate. Source/package lookups
remain numeric after preparation. New measurement code uses existing gated macros.

Baseline: F2 `6decb730`. Compare identical expression-layout workloads under the
same 68020 / 2 MiB emulator settings; CPU clock is not calibrated, so interpret
relative results only. Report complete routine output, release image size,
retained/peak owned memory and fresh completion/cleanup. Existing 10-second guest,
60-second invocation and 150-second batch limits remain unchanged. No self-host run.

Stop for review if this requires source reconstruction, a general layout solver,
full-source repeated convergence sweeps or a new unbounded storage structure.
Keep normal native package loading/CLI integration and other source-target
qualification out of this increment. Finish with a local checkpoint and an F3-only
review; no push is authorized.

### F3 checkpoint: reference defect and scope decision

The native bit operators and absolute-constant dependency walk are implemented.
Fresh native proof passes the 82-case compact/canonical evaluator batch, the
control-word routine, precedence/definition-site-PC data, and ordinary plus
PC-tainted cycle rejection. This is partial qualification, not F3 completion.

Two new live-Rust tests deliberately fail: a reversed 128-definition chain emits
`2,2,1` instead of `128,65,1`; the pixel-mask routine computes mask `3` instead of
`48`. Production Rust publishes provisional assignment values in pass one and
updates them only once in pass two for these CPUs. Its CPU-gated, eight-retry
layout loop is not an efficient dependency resolver.

The open scope decision is whether to include a Rust dependency phase now or
complete the operator increment separately. A coherent Rust repair must cover
active immutable `=` and `.const` definitions, scoped binding, synchronized scalar
and value-symbol state, explicit cycles, and layout refresh when corrected values
change instruction widths. Do not reorder oracle input, reproduce incorrect
values, or raise pass counts to conceal the defect. No Rust resolver changes have
been made pending that decision. Relative F3 performance qualification is pending.

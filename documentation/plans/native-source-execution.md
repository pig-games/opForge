# Resumable native source execution

Status: named preparation-known acquisition and the private frozen-input assembly
lifecycle have focused native proof. Growing-source execution remains a design
target.
The [completion plan](native-runtime-reset.md) records current evidence and completion order.

## Responsibility boundary

Active acquisition must use real assembly state when its guards depend on the
program counter, selected instruction sizes or section placement. Preparation's
known-value evaluator must not become another assembler. Keep the existing
shared expression compiler, statement dispatcher, data emission, encoders,
repetition snapshots and section/mapping owners authoritative.

`binary_source_cursor.i::drainPrepared` already provides a useful boundary:
publish a complete numeric record, execute it, then resume source acquisition.
Retained numeric captures supply replay; original text remains tokenizer and
diagnostic input. Acquired bodies that introduce module/use graphs remain a
separate gap until graph scheduling is explicitly supported.

## Assembly session and stable publication

Extract the existing assembly lifecycle into `beginSizing`, `advance`,
`finishSizing`, `emit` and `release`. `advance` suspends only between complete
records at the published frontier. An unfinished control envelope means more
source is required, not successful end of assembly. Finish with the existing
bounded convergence and strict emission checks; remove the superseded traversal
after callers migrate.

Store cursors, loop headers/bodies and inventory references as offsets. Sessions
borrow allocation owners and refresh interior views after growth. Never leave
an expression VM or encoder suspended across record relocation.

The first lifecycle checkpoint pins the full record allocation and assembly
context, including count, value/defined/section buffers, typed owner and member
keys. It stores the traversal cursor as an offset and permits record-budget
yield/resume only against the complete frozen frontier. It rejects a changed
base, partial frontier or relocated context before using borrowed views. This
checkpoint does not support incremental publication or relocation; the existing
branch, repetition and inventory views must migrate before either is allowed.

The sole `assemble` driver now uses these lifecycle APIs; the temporary reference
entry and mode are removed. Twelve fresh collection cases exercise one-record
yield/resume, failed initialization cleanup, premature sealing, invalid
base/frontier, rejection without stealing an active owner and repeated release.
Twelve composed-binding positives and two malformed-name rejections also pass
on 68020 and 6502. The isolated unchanged release control adds 0.76% on one
sample and 952 bytes of linked static storage; peak heap and a current full
self-host are unmeasured. These checks do not qualify actual-state acquisition.

`binary_symbol_storage` now owns Values, Defined, SectionIds and canonical
member keys independently. Growth stages only required replacement blocks,
including an allocated typed-kinds table. It stages allocations and copies before
committing views and Count; failure releases staged blocks and preserves the
published context. Existing cells survive and only appended bytes are cleared.
Kinds remain lazy when absent. Preparation-exported keys and kinds can precede
the first scalar allocation. Count remains bounded at 65,536. A caller-provided
reserve callback follows the normal block allocation ABI and permits component
failure injection; production uses bounded geometric block growth.

The CLI uses this owner, but frozen sessions still reject changed Count or views.
Growing source publication is not enabled. The callable component probe covers
small initial allocations, each of five allocation failures with unchanged
published cells/pointers/counts, prepared keys/kinds, lazy kinds, unchanged and
shrinking counts, successful maximum-count growth and repeated release. Fresh
component and production matrices each pass twelve exact positive cases and two
expected malformed-input rejections. The unchanged release control takes
27.064863 seconds before and 26.944213 after (-0.45%, one sample); linked static
reservation grows by 568 bytes. Peak heap and current full self-host are
unmeasured. Preparation retention and incremental inventory/binding publication
remain necessary before actual-state acquisition can use these growing buffers.

Retained repetition headers and bodies now use offsets, including the pending
header. The PRVM operand was already header-relative. Guard declarations are
reacquired through the stable inventory owner; transient dependency frames are
reconstructed per call. Resume refreshes borrowed views after the existing
frozen-frame checks. Active header spans and body/frontier limits are checked
before publication. The isolated BS35 probe passes twelve fresh collection cases
on both packages, including nested rebasing, truncated headers, overflow/null
bases, unchanged views after rejected refresh and one-record lifecycle traversal.
The production candidate host-assembles at 663,456 image bytes and 688,164
linked static bytes (504 more than the matching storage checkpoint). Its separate
native matrix passes twelve exact positives and two expected rejections on both
packages. The unchanged release control takes 26.848584 seconds before and
27.116166 after (+1.00%, one sample). Peak heap remains unmeasured. No incremental
inventory or source acquisition is enabled, and the public session still rejects
a changed record allocation.

Separate incremental binding publication from final source sealing. Already
consumed references must not be redirected by later import/ancestor rewriting.
Use stable occurrence/proxy identities and existing numeric binding plans;
publish deltas rather than copying entire tables per statement. Final sealing
checks unresolved references. Source controls compile through the shared grammar
and evaluate against the actual assembly Context, without mirroring values into
the preparation evaluator or inventing a provisional PC.

Inventories need begin/append/seal operations. Newly acquired declarations belong
to the current lexical owner. Repetition frames must enroll a newly published
declaration and save its enclosing value once; avoid repeatedly reallocating a
rectangular table for all existing iterations.

## Named acquired collections

Named `.bfor` and `.bwhile` results are typed collections of iteration address
records, including empty collections. Materialized records must preserve that
meaning, not replace a named collection with an ordinary label.

Use private numeric collection/iteration boundary records owned by the prepared
source layout. Centralize their named fields, actions and bounds validation.
Inventory and repetition/snapshot owners consume them; source control schedules
them without implementing another collection system. Publish each bounded record
batch once. Already expanded bodies must not be repeated again during assembly.
Preparation-known acquisition now uses these boundaries. Twelve fresh exact
controls cover counted/list/while/empty/nested collections, macro/segment includes
and colonless/explicit-colon labels on both 68020 and 6502 packages. Inventory and
runtime independently validate nesting, labels, iteration indices and balanced
boundaries. Initializers compile into full scratch storage before a bounded batch
copy. Actual assembly-dependent guards still require the session above.

## PC and replay semantics

Keep emission PC separate from the previous ordinary statement's start. Reuse
existing repetition observation and `.org` behavior. Acquisition callbacks and
private markers must not overwrite that observation. Current Rust rejects `*`
as the loop PC expression; pin `$` behavior with live oracles rather than adding
syntax.

Retain a compact numeric acquisition transcript containing source identity,
invocation/iteration path, compiled guard, selected outcome and materialized span.
Later sizing/emission must agree with it. Changed traversal is an instability
error. Capture selected file contents once per acquisition occurrence; replay
must not reread mutable host files. Preserve strict unavailable-value rejection
and short-circuiting of unevaluated operands.

## Mapped and placed sections

Establish flat source-order execution first. Source order is the graph-ordered
module schedule, not file discovery order. Then replace filtered semantic sweeps
with one source-order traversal per sizing round and section-owned cursors.
Output routing chooses storage, not whether language effects execute.

Reuse mapping freeze/check/merge, placement, section bounds and relocation owners.
Preserve section provenance through values and repetition records. A provisional
numeric address is not automatically absolute. Mapping changes require another
bounded complete sizing round, never prefix reassembly after each statement.
Remove existing mapped-loop capability rejections only after positive and
negative fresh native proof for the replacement.

## Implementation and evidence

1. Preserve the focused composed-binding and named-acquisition checkpoints;
   dynamic physical binding ownership remains a source-session gap.
2. Extract lifecycle and offset traversal with unchanged frozen-input behavior.
   Qualify budgeted yield/resume, invalid base/frontier, premature sealing and
   failure cleanup, then compare the complete CLI with the preserved reference.
3. Publish actual assembly context while retaining frontend/captures until seal.
   Force small allocation capacities to exercise growth with live nested controls.
   Qualify flat strict acquisition. First pin Rust for `.org 0`, `.while $<2`,
   `.include "one.inc"`, `.endwhile`, with `.byte 7` in the leaf. The expected
   statement-start result is three bytes; the live oracle must establish it.
   Add false missing-file, `.org`, alignment and package-encoded-size controls.
4. Cover named acquired snapshots, nested macros/includes and forced relocation;
   validate unavailable or changed activity diagnostics.
5. Migrate mapped/placed controls to section-owned source-order traversal.
6. Requalify shared corpus, engineering guards and full current-source self-host
   before claiming opcore completion and moving through the CPU families.

Keep explicit existing limits: nesting 16, iterations 65,536, record storage
2 MiB and ordinary owned blocks 1 MiB. Check overflow before allocation. Measure
release time separately from telemetry. Instrument published records, cursor
resumes, sizing rounds, captures and peak owned bytes through existing macros.
Retained captures and live context overlap until sealing, so heap growth needs
actual measurement; image reservation alone cannot establish peak memory.

### Text integration in progress

The bounded text owner (`3377a53d`) and shared packed text grammar
(`8d5cff22`) have separate fresh native component proofs. They do not establish
compact CLI text-directive parity. The current recovery work migrates the capsule
to BS36, appending TextPlan at byte 228 and TextPlanBytes at byte 232, for a
236-byte header. BS35 consumers and generated packages are incompatible.
Tokenizer name roles come from that descriptor. Frontend preparation registers
global ASCII/PETSCII source-origin IDs and normalizes encoding names once using
UTF-8 lossy decoding, Unicode trimming and ASCII lowercase. This normalizer has
host structural assembly evidence only; its native behavior is not qualified.

Pending work: relay the frontend numeric builtin IDs to assembly; retain every
standalone decoded string length until encoding determines scalar versus raw
aggregate emission; execute validated text spans; rebuild definitions on every
complete source replay, reset selection only on module entry, and release state
on every exit. String-expression preparation still erases decoded bytes.
The existing shared ExprVM PushStringLiteral opcode (0x13, little-endian u16
length and decoded bytes) and Rust eval_string_literal context provide the
contract for native runtime encoding; no replacement opcode is needed.
Neither that native provider nor assembly/emitter integration has landed.

### Conditional binding ownership

The unconditional longest-declaration repair and schema-only code pruning are
saved with host assembly evidence, but have no fresh native proof yet.
Rust now rebuilds executed declaration owners, references, reachable blocks and
schema uses on each layout pass. A forward-enabled conditional introducing the
longer dotted declaration on a later pass produces `[22, 66]`, retaining its
physical owner's code and preserving the pruned schema metadata. An inactive
longer declaration cannot retain ownership or references from an earlier pass.

Possible longer `.const` bindings behind unresolved guards remain separate from
executed declarations. Initial sizing may defer their values, including arithmetic
and typed aliases; they grant no visibility or immutable certificate. Stable
scalar-member errors and final evaluation stay strict. This provisional capture
currently covers `.const`, not skipped assignment or schema declarations.
Ninety-eight focused Rust tests pass across member/schema, constant dependencies,
reachability and owner controls. The native counterpart, shared opcore completion
and a new full current-source self-host remain unqualified.

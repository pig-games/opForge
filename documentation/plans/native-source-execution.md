# Resumable native source execution

Status: proposed implementation sequence within the active opcore parity work.
The interfaces below are private design targets, not implemented capabilities.
The [completion plan](native-runtime-reset.md) records qualified checkpoints.

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

Values, Defined and SectionIds currently share Count-dependent allocation slices.
Give them independent owned blocks, preserving existing cells and initializing
only appended cells. Grow typed values and member keys coherently, then publish
Context pointers and Count together. Allocation failure must not expose a partial
context. Preserve the 65,536 numeric identity bound.

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
Initially, preparation-known acquisition can use these boundaries; actual
assembly-dependent guards require the session above.

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

1. Preserve current include checkpoint, then complete composed binding and named
   acquisition collections using existing expression/repetition owners.
2. Extract lifecycle and offset traversal with unchanged frozen-input behavior.
   Force small allocation capacities to exercise growth with live nested controls.
3. Publish actual assembly context while retaining frontend/captures until seal.
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

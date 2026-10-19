# Module preparation and relocatable objects

## Current status

The maintained CLI still performs final binding, layout and emission globally.
Dependency-first module publication, capture retirement and bounded disk storage
are implemented. Standalone image/request/leaf-compiler components are implemented;
they are not a general CLI module-object path or a persistent `.ofm` cache.

The 2026-10-19 CLI integration attempt was removed after failing its memory gate.
It converted nine modules and saved 9,820 prepared bytes, while heap peak and
allocation traffic rose. Exact native output alone did not justify retaining it.
Checkpoint `cf78c259` preserves that attempt; its results and qualification are in
[the measurement report](native-performance-diagnosis.md#configured-module-cli-integration-not-retained).
Parity remains paused. The 8 MiB machine target has a 6.5/1.5 MiB process/OS split;
full-build fit remains unproven.

## Implemented publication boundary

A dependency-complete module can replay before discovery acquires another file.
Successful replay publishes owned numeric records, definitions and binary templates,
then acknowledges the graph node and retires its raw captures. Failed replay does
not acknowledge it. Whole-block fallthrough, configuration, source order and normal
reachability remain intact. Late section-map recipes defer section geometry until
all maps are known.

Configuration and semantic sessions have separate scratch/graph ownership.
Switching restores shared VM control at the stable frontend address. Complete,
immutable parameter snapshots transfer by spelling; repeated imports must agree
on all keys/values, independent of key order. Once configuration closes, its
owners retire before remaining semantic replay. Disk policy retains the existing
complete-graph schedule to avoid overlapping two growing sessions.

Prepared modules own checked logical ranges. Disk policy traverses them through
bounded views during global completion, binding and reachability; frontend scratch
retires before ordered contiguous assembly input is materialized. This reduces
overlap but still prepares applicable bodies and retains build-wide semantic owners.
See [capture and storage ownership](native-lazy-preparation.md).

## First-reference module assembly proposal

This remains the intended endpoint, not completed CLI behavior. It is an in-build
module image first; persistent fingerprints and cross-build reuse come later.

On the first actual use of a configured module, process its body as one assembly
batch, publish a near-output relocatable image, then retire its private captures,
lexical state and prepared records. Later references consume that image. Preserve
dependency-first scheduling for the first integrated boundary; changing activation
and discovery order simultaneously would make equivalence harder to qualify.

Actual references include exported symbols, typed definitions and binary template
consumers. An inactive branch or unexpanded template does not activate its contents.
`.use` discovery alone does not make every block reachable. Mandatory module-level
effects and validation retain their existing execution/source order. Referenced
whole `.block` units include internal labels and fallthrough; macros and segments
are templates, not dead-code-elimination units.

### Saved image and semantic interface

An assembled image contains fixed code/data plus only unresolved binding, layout
and observation recipes. Every fragment supports relocation. Numeric IDs, relative
offsets and bounded regions are serialized; process pointers never are. Resident
and disk-backed forms share the same meaning, with selected fragments linked only
when final placement is known. Large-memory builds can keep images resident;
low-memory builds write complete units sequentially to a real disk.

A module export interface must own scalar and typed definitions/schemas, binary
macro/segment templates, configuration, imports and dependency identities. Temporary
global symbol IDs are a bridge, not a reusable object identity. Local IDs need
explicit remapping. Declaration context, visibility, member provenance, encoding
state and definition-relative include paths must survive retirement.

Unresolved-symbol indexes identify byte patches **and** dependent conditions,
repetition, relaxation and layout observations. Both conditional branches may have
binary fragments, but choosing branches and placing code still require convergence:
relocation alone does not resolve a branch that changes its own condition's address.
Keep the shared VM/package grammar and encoding services authoritative. Unsupported
recipes must preserve the complete reference module path without replaying file
effects, or explicitly reject an experimental request; never silently freeze them.

Includes suspend their parent with origin and expansion context intact. Active
file effects happen once. Published templates own their binary bodies/defaults.
Errors, recursion, missing imports, partial publication, I/O failure and cleanup
must retain explicit owners. A published image may not borrow frontend pointers.

### Memory and runtime expectations

The useful bound is shared runtime/package plus one active module and its required
dependency closure, retained public interfaces/templates, compact saved payloads,
link recipes and bounded readers. Unresolved cycles can enlarge that active set;
it must not quietly become every module's old prepared state. Spilling images
helps only if private owners retire before the measured peak.

The failed integration demonstrates why raw-byte compression alone is insufficient:
its restricted retained stream shrank, but dominant lexical/type/template owners
were untouched and temporary execution added allocations. The next change needs
an explicit ownership/lifetime saving, not a broader compression percentage.
No numerical full-build saving or speedup is forecast before that boundary is
sized. The earlier 1 MiB peak-saving target remains a useful acceptance threshold.

Sequential module images aim to avoid repeated disk paging and source-oriented
work. Current hardware observations are 123/124 s resident versus 1,292/1,254 s disk
on one older frozen input. They motivate the design; they do not predict its speed.
Retain the reference, compare identical inputs against live Rust, measure release
runtime separately from heap, and qualify a full self-build before promotion.

## Native source producer: restricted leaf boundary

These standalone components remain as the checked image/publication contract:

- `binary_module_image.asm`: immutable 28-byte header, 12-byte block directory rows,
  16-byte BE32 relocation rows and raw payload, using offsets and numeric IDs.
- `binary_module_request.asm`: one first-request compile callback, checked publication,
  reuse, sticky failure and explicit release. Views borrow the published image.
- `binary_module_compile.asm`: shared tokenizer/frontend/assembly execution for a
  restricted leaf, followed by private-owner retirement and image publication.

The leaf producer supports up to eight nonnested blocks, each in a distinct code
section with contiguous initialized output from local PC zero. Same-block relative
branches use existing package/Hunk proof. General imports, exported definitions/
templates, absolute cross-section relocations and placement-dependent recipes are
not implemented there. Unsupported sources reject the component request.

Native probes cover moved images, poisoned/freed originals, selected whole blocks,
one compilation across repeated requests, different imported link addresses,
corrupt/truncated input, overflow, recursive failure and balanced cleanup. A scaled
leaf produced exact 5,148-byte selected output; its 14,362 prepared bytes became
a 2,639-byte image. That component compression is **not** a CLI peak reduction,
a speedup over the reference, or a full-build 8 MiB fit result.

## Next coherent integration boundary

Complete the module semantic interface before retiring storage: resolve local
immutable/typed dependencies, retain public/template values, and describe every
remaining address/layout dependency with explicit numeric recipes. Then consume
one such configured module through ordinary final layout without reconstructing
its instruction stream. Preserve shared sections, imports and complete blocks.

First qualify a representative module that exercises the previously excluded
binding/relocation cases, then use the frozen full/small/macro comparisons. Stop
if retained interfaces and temporary overlap replace the old peak. Do not increase
limits or add switches simply to keep the failed instruction-only converter alive.
Discuss this larger completion/export boundary before implementing it.

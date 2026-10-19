# Module preparation and relocatable objects

## Current status

The maintained CLI still performs final binding, layout and emission globally.
Dependency-first module publication, capture retirement and bounded disk storage
are implemented. Standalone image/request/leaf-compiler components now publish
owned scalar,
list/range and struct-schema exports after private-owner retirement. They are
not a general CLI module-object path or a persistent `.ofm` cache.

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

- `binary_module_image.asm`: immutable 36-byte header, 12-byte block directory rows,
  28-byte bounded relocation rows, raw payload and an optional aligned semantic image.
  Every serialized reference is a bounded offset or module-local numeric identity.
- `binary_module_request.asm`: one first-request compile callback, checked publication,
  reuse, sticky failure and explicit release. Views borrow the published image.
- `binary_module_compile.asm`: shared tokenizer/frontend/assembly execution for a
  restricted leaf, followed by private-owner retirement and image publication.
- `binary_module_layout.asm`: owns numeric block geometry, exact label ownership
  and package-program relocation recipes; no source grammar or CPU decoder.
- `binary_module_exports.asm`: captures owned public spellings and member keys
  before lexical retirement, then obtains final values from the shared engine.
- `binary_module_interface.asm`: owns compact scalar/list/range/schema exports,
  validates their bounded representation, and provides numeric readers.

The leaf producer supports nonnested blocks sharing selected code sections. Block
count is bounded by owned storage and 16-bit local identities, independently of
the eight-section Hunk limit. Each block owns contiguous initialized output;
empty blocks and initialized bytes outside blocks reject. Exact label identities
preserve ownership even when an end label shares the next block's entry address.

Ordinary relative branches save their package program, selected candidate and
numeric arguments. Linking replays that same VM program at the new source/target
addresses; package-defined widths, endian, reserved values and ranges remain
authoritative. No instruction decoding or new VM version dispatch is introduced.
Same-block relative branches need no saved recipe. Absolute data references to
owned labels use bounded BE32 relocations; incompatible package byte order rejects.
The VM reports PC dependence separately from the existing Hunk cancellation
proof. Numeric PC-dependent branches without an owned target cannot publish a
recipe and reject. Other instruction fixups, unowned
aliases and placement-dependent candidate relaxation reject. Linking preflights
all patches before copying bytes; resolver/patch callbacks must be pure and stable.
The private image requires the matching producer package; persistent fingerprints
are still future work.

Immutable scalar dependencies resolve through the shared engine, including private
forward constants. Public scalar, list/range and struct-schema definitions survive
in the saved image. Spellings retain source case; symbol matching remains ASCII
case insensitive. IDs are local to the image, including member and nominal
identities; later integration needs explicit remapping, not reuse as build-wide
IDs. Scalar section provenance is retained; mapping exported addresses to independently
placed fragment IDs remains an explicit CLI-integration boundary.

General imports, public templates, mutable/deferred exports, concrete address/
struct values, nested compound cells, absolute instruction fixups and
placement-dependent relaxation recipes remain unsupported. Concrete nominal values reject
until their schema dependencies can be exported completely. Unsupported sources
reject the component request; the normal CLI continues to use its existing engine.
The producer-only evaluator policy also rejects positional observations that lose
affine provenance, including intermediate cancellation, comparisons and positional
conditions. Even same-block differences conservatively reject until a stronger
proof or explicit layout recipe can represent them. Normal CLI policy remains zero.
The same policy rejects `.align` in the shared assembly handler, including zero
original padding: independently moving a block can change its required padding.
An alignment recipe or explicit placement constraint is needed before accepting it.

Native probes cover moved images, poisoned/freed originals, selected whole blocks,
one compilation across repeated requests, different imported link addresses,
corrupt/truncated input, overflow, recursive failure and balanced cleanup. The
semantic consumer also moves the complete image, poisons source and original
image bytes, releases the request, then reads scalar, struct-member and list values
through the retained image only. Its expected bytes come from a live Rust consumer.
Malformed semantic kind/name offsets and truncated images must reject. Public
templates and unsupported nominal values must fail without leaking owners.

The real `binary_text_names` component produces 528 code bytes across three
shared-section blocks and two inter-block recipes. Moving the image, poisoning
its source/original image and linking in order 3/1/2 at different addresses matches
fresh Rust exactly. A second fixture combines absolute data, relative branches
and block-local end labels. Missing callbacks, out-of-range placement, unknown
programs and malformed spans reject before changing guarded output. Separate
MEMD runs finish with zero ownership. See the [measurement report](native-performance-diagnosis.md)
for resource scope; this is not a CLI peak reduction or an 8 MiB full-build result.

## Next coherent integration boundary

The preimplementation capacity check does **not** establish the requested 1 MiB
net full-build saving for code-only modules. Imports and absolute instruction
relocations alone leave ordinary data/BSS and aligned modules on the reference
path. The resulting 106-module textual upper bound could remove about 1.7 MiB
of capacity at the observed peak, before replacement owners. Its public export
rows and spellings alone would cost about 343 KiB if duplicated. Compiled payloads,
recipes, typed/member data, remapping and a complete active private session still
need space. Do not count the gross opportunity as an achieved or qualified net
saving. See the [capacity assessment](native-performance-diagnosis.md#private-module-cli-capacity-gate).

The recommended scope correction is an ordinary-module completion boundary:

- Consume captured binary input at dependency-first publication, before private
  identities and records enter the global semantic frontend.
- Preserve independently reachable whole code blocks, mandatory initialized data,
  BSS extents, section order and explicit alignment constraints in final layout.
  Do not freeze original padding when fragments can move.
- Complete imported scalar/schema/member remapping, exported fragment addresses,
  absolute instruction relocations and whole-block dependencies through the shared
  VM/package services.
- Transfer one owned canonical public interface. Do not retain both a global
  lexical/type copy and a second complete semantic export table. Retire the
  private frontend, names, template plans and records before later preparation.
- Keep public templates, unsupported nominal closures and unresolved control/layout
  recipes on the complete reference path initially. Admission/fallback must not
  repeat file effects or partially publish a module.

This is a broader design decision, not implemented CLI behavior. Canonical-interface
transfer alone does not enlarge the cohort; it belongs with ordinary data/BSS and
alignment support rather than another standalone foundation checkpoint. Size the
supported cohort and replacement owners before asserting its net saving. Preserve
the existing path, then compare identical full/small/macro inputs, measure release
runtime separately from memory, and qualify a full self-build before promotion.
Do not increase limits or add switches to retain a converter that misses its gate.

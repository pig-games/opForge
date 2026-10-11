# Consolidate current VM contracts

Status: pending structural work within opcore completion. This document describes
the migration target; it does not claim current executors already meet it.

The requirement is one current v1 contract per VM, with no native VM version
fields, parameters, checks or dispatch. Preserve package/container integrity and
bounds validation. Before 1.0, migrate generation and execution together and
remove superseded formats rather than adding compatibility executors.

## Distinguish kind from version

The current SEMV version field combines unrelated program kinds with successive
formats. Its values must not all be treated as obsolete historical programs:

| Current values | Program kind | Consolidated capabilities |
|---|---|---|
| 1 | Fixed emission | Current fixed emission operations |
| 2, 6, 8 | Encoding | Current union, including input fields and integer-to-IEEE conversion |
| 3 | Structured encoding | Current structured operations |
| 4, 7 | Fixup | Current transformed layout, with explicit identity transforms where appropriate |
| 5 | Branch | Current branch operations |

Introduce a package-owned semantic kind enum. SEMV has one current v1 contract
covering all these kinds. Dispatch by kind at the generic boundary; individual
VM execution APIs take current bytecode and inputs, without a version parameter.

VALUE v1/v2 and OPRD v1/v2/v3 are real retained alternatives. Consolidate all
current operations into their respective v1 contracts. PRVM v1 must contain the
current statement, macro, file, resume and expression-request capabilities;
entry and request kinds remain meaningful dispatch information. TKVM, EXVM,
EXPR, selector and state keep their single current capabilities.

## Serialized and native boundaries

The native compiled capsule currently stores `Kind:u16, Version:u16,
Offset:u32, Bytes:u32`. Replace it with explicit kind, offset and length; migrate
table strides, builders and validators together. Keep unknown-kind rejection,
stage/kind compatibility, overflow, alignment, span and opcode validation.

Removing compact capsule fields alone is insufficient: TKPKG services consume
portable CSEM/CPRD descriptors directly. Remove their per-program version fields
as part of the same migration. Semantic descriptors carry owner, ID, kind,
length and bytes; chunks already identifying VALUE or OPRD need no extra kind.
Update both ordinary and compact codecs and companion equivalence checks.
The package/container revision identifies the fixed current contract set and
rejects incompatible containers. Document each VM's own v1 contract explicitly.

Remove native PRVM frame, parser, resume and expression-request version words,
setup instructions and comparisons, plus macro/head/declaration/route/iterator
version fields and TKPKG caches. Repack every producer, consumer and harness
initializer together. Preserve actual frame sizes, capacities, magic/context
discrimination, token strides, entry kinds, budgets, modes and status contracts.
Do not leave version-shaped padding or constant assignments behind.

## Coherent implementation sequence

1. Introduce semantic kinds and consolidate encoding/fixup formats across
   package codecs, family builders, Rust runtime, compiled capsule generation,
   compact native and TKPKG services. The newest IEEE operation is currently a
   native implementation gap and must be retained, not renamed away.
2. Consolidate VALUE and OPRD operations across codecs, builders and all affected
   execution services, removing old APIs and dispatch branches.
3. Consolidate PRVM and repack native request/resume/expression boundaries;
   remove remaining native version overhead in other VM boundaries.
4. Update contract documentation and generated package fixtures, audit current
   production owners and qualify all affected consumers.

Each slice is an atomic generation/execution recovery point. Contract completion
is claimed only after the entire migration is qualified. Keep the existing
frozen working executable/package set outside build cache for comparisons; it
does not justify preserving obsolete production executors.

## Evidence

Exercise package round trips, malformed and truncated descriptors, duplicate
ownership, companion equivalence and obsolete-container rejection. Cover every
current operation, especially input fields/IEEE conversion, identity/transformed
fixups and composite/nested operand constructors. Include malformed unselected
programs to verify validation is not restricted to the path executed.

Use live Rust assembly oracles and fresh native completion for compact and
TKPKG services, plus PRVM suspension/resume, bad spans and capacity failures.
Run architecture and proof guards, and audit native production source for VM
version storage/setup/dispatch. Measure each coherent change against an unchanged
release control, with separate telemetry for heap cost. Requalify current-source
self-hosting at the completion boundary. Numeric renaming alone is not proof.

; Actual compiler, retired source/image, and moved section-fragment replay.
	.module tests.amigaos.module_geometry_probe
	.cpu 68020
	.use experimental.amigaos.binary_module_compile as compiler
	.use experimental.amigaos.binary_module_image as image
	.use experimental.amigaos.binary_module_geometry as geometry
	.use experimental.amigaos.binary_module_records as records
	.use experimental.amigaos.binary_module_registry as registry
	.use experimental.amigaos.binary_module_carrier as carrier
	.use experimental.amigaos.binary_block_index as blockIndex
	.use experimental.amigaos.binary_scope_layout as carrierScope
	.use experimental.amigaos.binary_scopes as carrierScopes
	.use experimental.amigaos.binary_lexical_store as carrierLexical
	.use experimental.amigaos.binary_modules as carrierModules
	.use experimental.amigaos.binary_imports as carrierImports
	.use experimental.amigaos.binary_graph as carrierGraph
	.use experimental.amigaos.binary_values as carrierValues
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_sections as sections
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_encoding as encoding
	.use experimental.amigaos.binary_dependencies as retainedDependencies
	.use experimental.amigaos.binary_dependency_layout as retainedGraph
	.use experimental.amigaos.binary_package as retainedPackage
	.use opasm.amigaos.binary_expression as retainedExpression
	.use exprvm.amigaos.runtime as retainedRuntime
	.include "module_geometry_fixture.i"
	.priv
EXEC_BASE = 4
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
FAIL_ARITHMETIC = 21
FAIL_RECORDS = 22
FAIL_MOVE = 24
FAIL_GEOMETRY = 25
FAIL_LINK = 26
FAIL_REGISTRY = 27
Expectation	.struct
Owner	.long ?
Section	.long ?
Kind	.long ?
Alignment	.long ?
Extent	.long ?
	.endstruct
EXPECTATION_BYTES = Expectation.Extent+4
Endpoint	.struct
Code	.long ?
Data	.long ?
Bss	.long ?
	.endstruct
ENDPOINT_BYTES = Endpoint.Bss+4
ENDPOINTS = Result+CODE_BYTES+DATA_BYTES
	.section entry, kind=code
	.pub
; Entry preserves callee-saved registers; success requires the output to close.
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	move.l #20, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l EXEC_BASE.w, a6
	jsr EXEC_OPEN_LIBRARY(a6)
	move.l d0, DosBase
	beq.w cleanup
	jsr cases
	bne.w closeLibrary
	movea.l DosBase, a6
	move.l #OutputName, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, Handle
	beq.w closeLibrary
	move.l d0, d1
	move.l #Result, d2
	move.l #RESULT_BYTES, d3
	jsr DOS_WRITE(a6)
	cmpi.l #RESULT_BYTES, d0
	bne.w closeFile
	clr.l ReturnCode
closeFile
	move.l Handle, d1
	jsr DOS_CLOSE(a6)
	tst.l d0
	bne.w closeLibrary
	move.l #20, ReturnCode
closeLibrary
	jsr releaseOwners
	movea.l DosBase, a1
	movea.l EXEC_BASE.w, a6
	jsr EXEC_CLOSE_LIBRARY(a6)
cleanup
	jsr releaseOwners
	move.l ReturnCode, d0
	movem.l (sp)+, d2-d7/a2-a6
	tst.l d0
	rts
	.bend  ; start
	.endsection
	.section code, kind=code
	.priv
cases	.block
	move.l #FAIL_ARITHMETIC, ReturnCode
	bsr.w arithmetic
	bne.w bad
	move.l #FAIL_RECORDS, ReturnCode
	bsr.w preparedRecords
	bne.w bad
	bsr.w carrierChecks
	bne.w bad
	bsr.w branchWidthChecks
	bne.w bad
	bsr.w retainedAddressChecks
	bne.w bad
	lea CompileContext, a0
	lea Original, a1
	jsr compiler.compile
	beq.w compiled
	move.l d0, ReturnCode
	bra.w bad
compiled
	move.l #FAIL_MOVE, ReturnCode
	bsr.w moveImage
	bne.w bad
	movea.l Moved+memory.Block.Pointer, a0
	move.l Moved+memory.Block.Used, d0
	jsr image.validate
	bne.w bad
	cmpi.l #FRAGMENT_COUNT, image.Header.BlockCount(a0)
	bne.w bad
	move.l a0, Link+image.LinkFrame.Image
	move.l Moved+memory.Block.Used, Link+image.LinkFrame.ImageBytes
	lea Result, a1
	move.l #RESULT_BYTES, d0
clear
	clr.b (a1)+
	subq.l #1, d0
	bne.w clear
	move.l #FAIL_GEOMETRY, ReturnCode
	bsr.w placeFragments
	bne.w bad
	move.l #FAIL_LINK, ReturnCode
	bsr.w linkFragments
	bne.w bad
	move.l Cursors+Endpoint.Code, ENDPOINTS+Endpoint.Code
	move.l Cursors+Endpoint.Data, ENDPOINTS+Endpoint.Data
	move.l Cursors+Endpoint.Bss, ENDPOINTS+Endpoint.Bss
	move.l #FAIL_REGISTRY, ReturnCode
	bsr.w registryLifecycle
	bne.w bad
	bsr.w releaseOwners
	tst.l Moved+memory.Block.Pointer
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; cases

; No source, assembly scratch, or original-image pointer survives this step.
moveImage	.block
	lea Moved, a0
	move.l Original+memory.Block.Used, d0
	jsr memory.reserveExact
	bne.w done
	move.l Original+memory.Block.Used, d1
	move.l d1, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	movea.l Original+memory.Block.Pointer, a0
copy
	move.b (a0), (a1)+
	move.b #$a5, (a0)+
	subq.l #1, d1
	bne.w copy
	lea Original, a0
	jsr memory.release
	lea Source, a0
	move.l #SourceEnd-Source, d1
poison
	move.b #$cc, (a0)+
	subq.l #1, d1
	bne.w poison
	clr.l CompileContext+compiler.Context.Source
	clr.l CompileContext+compiler.Context.Bytes
	moveq #0, d0
done
	tst.l d0
	rts
	.bend  ; moveImage

; Compare source ownership and numeric geometry before placing every fragment.
; Root data is unconditional; nested block 2 belongs to outer whole block 1.
placeFragments	.block
	movea.l Moved+memory.Block.Pointer, a4
	adda.l image.Header.Blocks(a4), a4
	lea Expectations, a5
	moveq #0, d7
next
	move.l d7, d0
	addq.l #1, d0
	cmp.l image.Block.Id(a4), d0
	bne.w bad
	move.l Expectation.Owner(a5), d0
	cmp.l image.Block.Owner(a4), d0
	bne.w bad
	move.l Expectation.Section(a5), d6
	cmp.l image.Block.Section(a4), d6
	bne.w bad
	move.l Expectation.Kind(a5), d0
	cmp.l image.Block.Kind(a4), d0
	bne.w bad
	move.l Expectation.Alignment(a5), d0
	cmp.l image.Block.Alignment(a4), d0
	bne.w bad
	move.l Expectation.Extent(a5), d3
	cmp.l image.Block.Extent(a4), d3
	bne.w bad
	cmpi.l #image.KIND_BSS, image.Block.Kind(a4)
	beq.w bss
	cmp.l image.Block.Bytes(a4), d3
	bne.w bad
	bra.w place
bss
	tst.l image.Block.Bytes(a4)
	bne.w bad
place
	move.l d6, d0
	lsl.l #2, d0
	lea Cursors, a0
	move.l 0(a0, d0.l), d1
	move.l image.Block.Alignment(a4), d2
	jsr geometry.place
	bne.w bad
	move.l d6, d0
	lsl.l #2, d0
	move.l d2, 0(a0, d0.l)
	move.l d7, d0
	lsl.l #2, d0
	lea Bases, a0
	move.l d1, 0(a0, d0.l)
	adda.w #image.BLOCK_BYTES, a4
	adda.w #EXPECTATION_BYTES, a5
	addq.l #1, d7
	cmpi.l #FRAGMENT_COUNT, d7
	blo.w next
	cmpi.l #CODE_BASE+CODE_BYTES, Cursors+Endpoint.Code
	bne.w bad
	cmpi.l #DATA_BASE+DATA_BYTES, Cursors+Endpoint.Data
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; placeFragments

; Link only initialized bytes; allocation-only BSS must publish Used=0.
linkFragments	.block
	movea.l Moved+memory.Block.Pointer, a4
	adda.l image.Header.Blocks(a4), a4
	moveq #0, d7
next
	lea Link, a0
	move.l image.Block.Id(a4), image.LinkFrame.BlockId(a0)
	move.l d7, d0
	lsl.l #2, d0
	lea Bases, a1
	move.l 0(a1, d0.l), d1
	move.l d1, image.LinkFrame.Base(a0)
	move.l image.Block.Bytes(a4), image.LinkFrame.Capacity(a0)
	clr.l image.LinkFrame.Output(a0)
	cmpi.l #image.KIND_BSS, image.Block.Kind(a4)
	beq.w replayFragment
	move.l image.Block.Section(a4), d0
	lsl.l #2, d0
	lea InitialCursors, a1
	sub.l 0(a1, d0.l), d1
	bcs.w bad
	lea OutputOffsets, a1
	add.l 0(a1, d0.l), d1
	bcs.w bad
	lea Result, a1
	adda.l d1, a1
	move.l a1, image.LinkFrame.Output(a0)
replayFragment
	jsr image.linkValidated
	bne.w bad
	move.l image.Block.Bytes(a4), d0
	cmp.l image.LinkFrame.Used(a0), d0
	bne.w bad
	adda.w #image.BLOCK_BYTES, a4
	addq.l #1, d7
	cmpi.l #FRAGMENT_COUNT, d7
	blo.w next
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; linkFragments

; Resolver maps opaque fragment IDs to independently recomputed placements.
resolve	.block
	cmpi.l #1, d1
	blo.w bad
	cmpi.l #FRAGMENT_COUNT, d1
	bhi.w bad
	move.l a1, -(sp)
	subq.l #1, d1
	lsl.l #2, d1
	lea Bases, a1
	move.l 0(a1, d1.l), d1
	movea.l (sp)+, a1
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; resolve
patch	.block
	move.l a0, -(sp)
	movea.l compiler.Context.Package(a0), a0
	jsr encoding.patchPosition
	movea.l (sp)+, a0
	tst.l d0
	rts
	.bend  ; patch

; Placement must reject invalid alignment and both alignment/allocation wrap.
arithmetic	.block
	move.l #$ffffffff, d1
	moveq #2, d2
	moveq #0, d3
	jsr geometry.place
	beq.w bad
	cmpi.l #$ffffffff, d1
	bne.w bad
	cmpi.l #2, d2
	bne.w bad
	move.l #$fffffffc, d1
	moveq #4, d2
	moveq #8, d3
	jsr geometry.place
	beq.w bad
	moveq #0, d1
	moveq #3, d2
	moveq #0, d3
	jsr geometry.place
	beq.w bad
	moveq #0, d2
	jsr geometry.place
	beq.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; arithmetic
; Builders validate numeric identities before publishing one bounded record.
preparedRecords	.block
	lea RecordOwner, a0
	moveq #17, d1
	moveq #23, d2
	moveq #9, d3
	jsr records.appendFragment
	bne.w bad
	lea GlobalIds, a1
	moveq #3, d1
	moveq #10, d3
	jsr records.appendReferences
	bne.w bad
	cmpi.l #records.FRAGMENT_BYTES+records.REFERENCE_HEADER_BYTES+3*2, memory.Block.Used(a0)
	bne.w bad
	movea.l memory.Block.Pointer(a0), a0
	moveq #records.FRAGMENT_BYTES, d0
	jsr records.fragmentView
	bne.w bad
	cmpi.l #17, d1
	bne.w bad
	cmpi.l #23, d2
	bne.w bad
	adda.w #records.FRAGMENT_BYTES, a0
	moveq #records.REFERENCE_HEADER_BYTES+3*2, d0
	jsr records.referencesView
	bne.w bad
	cmpi.l #3, d1
	bne.w bad
	tst.w (a1)+
	bne.w bad
	cmpi.w #65535, (a1)+
	bne.w bad
	cmpi.w #42, (a1)+
	bne.w bad
	move.b #4, records.ReferenceList.Count(a0)
	moveq #records.REFERENCE_HEADER_BYTES+3*2, d0
	jsr records.referencesView
	beq.w bad
	move.b #3, records.ReferenceList.Count(a0)
	lea RecordOwner, a0
	lea BadGlobalId, a1
	moveq #1, d1
	moveq #10, d3
	jsr records.appendReferences
	beq.w bad
	move.l #records.REFERENCE_LIMIT+1, d1
	jsr records.appendReferences
	beq.w bad
	move.l #65536, d1
	moveq #23, d2
	jsr records.appendFragment
	beq.w bad
	cmpi.l #records.FRAGMENT_BYTES+records.REFERENCE_HEADER_BYTES+3*2, memory.Block.Used(a0)
	bne.w bad
	moveq #0, d1
	suba.l a1, a1
	jsr records.appendReferences
	bne.w bad
	movea.l memory.Block.Pointer(a0), a0
	adda.w #records.FRAGMENT_BYTES+records.REFERENCE_HEADER_BYTES+3*2, a0
	moveq #records.REFERENCE_HEADER_BYTES, d0
	jsr records.referencesView
	bne.w bad
	tst.l d1
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; preparedRecords
; Publication consumes one owner only after allocation; rollback frees it.
registryLifecycle	.block
	lea RegistryState, a0
	lea RegistryMark, a1
	jsr registry.checkpoint
	lea Moved, a1
	jsr registry.add
	bne.w bad
	tst.l d1
	bne.w bad
	tst.l Moved+memory.Block.Pointer
	bne.w bad
	tst.l Moved+memory.Block.Capacity
	bne.w bad
	tst.l Moved+memory.Block.Used
	bne.w bad
	move.l #REGISTRY_FRAGMENT, d2
	jsr registry.locate
	bne.w bad
	cmpi.l #REGISTRY_KIND, image.Block.Kind(a2)
	bne.w bad
	cmpi.l #REGISTRY_EXTENT, image.Block.Extent(a2)
	bne.w bad
	tst.l registry.Placement.Known(a3)
	bne.w bad
	cmpi.l #registry.UNMAPPED, registry.Placement.Section(a3)
	bne.w bad
	move.l #REGISTRY_EXTENT, d3
	move.l #65535, d4
	jsr registry.bindAddress
	bne.w bad
	jsr registry.bindAddress
	bne.w bad
	cmpi.l #registry.ADDRESS_BYTES, RegistryState+registry.State.Addresses+memory.Block.Used
	bne.w bad
	move.l #REGISTRY_EXTENT+1, d3
	jsr registry.bindAddress
	beq.w bad
	cmpi.l #registry.ADDRESS_BYTES, RegistryState+registry.State.Addresses+memory.Block.Used
	bne.w bad
	lea RegistryMark, a1
	jsr registry.rollback
	bne.w bad
	tst.l RegistryState+registry.State.Modules+memory.Block.Used
	bne.w bad
	tst.l RegistryState+registry.State.Placements+memory.Block.Used
	bne.w bad
	tst.l RegistryState+registry.State.Addresses+memory.Block.Used
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; registryLifecycle
releaseOwners	.block
	lea Original, a0
	jsr memory.release
	lea Moved, a0
	jsr memory.release
	lea RecordOwner, a0
	jsr memory.release
	lea RegistryState, a0
	jsr registry.release
	rts
	.bend  ; releaseOwners
	.include "module_carrier_checks.i"
	.include "module_branch_width_checks.i"
	.include "module_retained_address_checks.i"
	.endsection
	.section data, kind=data
	.align 4
CompileContext
	.long Package, Source, SourceEnd-Source, 0, 0, 0, 0, 0, 0
Link
	.long 0, 0, 1, 0, 0, 0, 0, resolve, CompileContext, patch
; Expectation rows use owner/section/kind/alignment/allocation, never payload
; offsets or CPU recipe IDs. Empty alignment anchors are real fragments.
Expectations
	.include "module_geometry_expectations.i"
GlobalIds
	.long 0, 65535, 42
BadGlobalId
	.long 65536
InitialCursors
	.long CODE_BASE, DATA_BASE, BSS_BASE
Cursors
	.long CODE_BASE, DATA_BASE, BSS_BASE
OutputOffsets
	.long 0, CODE_BYTES, 0
Package
	.incbin "module_geometry_package.bin"
	.align 4
Source
	.incbin "module_geometry_source.txt"
SourceEnd
DosName	.byte "dos.library", 0
OutputName	.byte "output.bin", 0
	.endsection
	.section bss, kind=bss
	.align 4
Original	.res byte, memory.Block.Used+4
Moved	.res byte, memory.Block.Used+4
RecordOwner	.res byte, memory.Block.Used+4
RegistryState	.res byte, registry.STATE_BYTES
RegistryMark	.res byte, registry.CHECKPOINT_BYTES
Bases	.res long, FRAGMENT_COUNT
Result	.res byte, RESULT_BYTES
DosBase	.res long, 1
Handle	.res long, 1
ReturnCode	.res long, 1
	.endsection
	.output "module-geometry-probe.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule

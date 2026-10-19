; Restricted real-source producer for independently relocatable leaf blocks.
; @opforge-owner: experimental.amigaos.binary_module_compile
	.module experimental.amigaos.binary_module_compile
	.cpu 68020
	.use experimental.amigaos.binary_frontend as frontend
	.use experimental.amigaos.binary_assembly as assembly
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_module_image as image
	.use experimental.amigaos.binary_module_exports as exports
	.use experimental.amigaos.binary_module_layout as layout
	.use experimental.amigaos.binary_block_index as blocks
	.use experimental.amigaos.binary_symbol_storage as storage
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_sections as sections
	.use experimental.amigaos.binary_metadata_prepare as metadata
	.use experimental.amigaos.binary_source as source
	.use prvm.amigaos.packed_branch as branch
	.use prvm.amigaos.packed_loop as loops
	.use prvm.amigaos.abi as abi
	.include "memory_telemetry.i"
	.pub
Context	.struct
Package	.long ?
Source	.long ?
Bytes	.long ?
	.endstruct
CONTEXT_BYTES = Context.Bytes+4
LINE_CAPACITY = 256
INVALID = 1
EXPORT_INVALID = 2
EXECUTION_INVALID = 3
PUBLICATION_INVALID = 4
IMAGE_INVALID = 5
	.priv
; The shared native assembly engine is itself a single active session. This
; producer is sequential, never callable during another assembly session.
	.section bss, kind=bss
Active	.res long, 1
Caller	.res long, 1
Destination	.res long, 1
Started	.res long, 1
Cursor	.res long, 1
After	.res long, 1
	.align 4
Front	.res byte, frontend.FRAME_BYTES
Work	.res byte, assembly.FRAME_BYTES
Cells	.res byte, package.CONTEXT_BYTES
Symbols	.res byte, storage.STATE_BYTES
ValueOwner	.res byte, values.OWNER_BYTES
Build	.res byte, image.FRAME_BYTES
Scratch	.res byte, memory.Block.Used+4
Line	.res byte, memory.Block.Used+4
Records	.res byte, memory.Block.Used+4
Index	.res byte, memory.Block.Used+4
Parameters	.res byte, memory.Block.Used+4
Output	.res byte, memory.Block.Used+4
ImageOwner	.res byte, memory.Block.Used+4
SemanticOwner	.res byte, memory.Block.Used+4
PublicState	.res byte, exports.STATE_BYTES
LayoutState	.res byte, layout.STATE_BYTES
Metadata	.res byte, metadata.CONFIG_BYTES
TransientEnd
	.endsection
	.section code, kind=code
	.pub
; request compiler callback: A0=Context,A1=empty owned memory.Block.
; D0/CCR=0 success, otherwise one of the named *_INVALID statuses; others kept.
; Text is processed by shared frontend, macros by shared template execution,
; instruction/data bytes by one shared assembly session. No source parser here.
; Image IDs are declaration-order ordinals 1..N, independent of lexical IDs.
; Nonnested blocks may share a selected Hunk code section. Initialized bytes
; must belong to contiguous whole blocks. Package-owned branch recipes preserve
; the chosen instruction width when blocks move; shared affine label identity
; preserves absolute data references. Instruction absolute fixups and aliases
; without an exact label owner reject. Runtime conditionals/loops reject.
; Constant source preprocessing and private macros may complete before assembly.
; Publishes immutable scalars, lists/ranges and struct schemas with local IDs.
; Scalars retain image-local section provenance; concrete nominal values reject.
; Public templates, mutable/deferred exports, external imports, unresolved layout
; recipes and disk spilling remain unsupported by this private producer.
; Package/source are borrowed for this call. All private owners retire on return;
; success transfers only the immutable image. Existing gated telemetry owns
; resource reporting; no release-only observer or metrics owner is retained.
compile	.block
	movem.l d1-d7/a0-a6, -(sp)
	tst.l Active
	bne.w busy
	move.l #1, Active
	move.l a0, Caller
	move.l a1, Destination
	tst.l memory.Block.Pointer(a1)
	bne.w bad
	tst.l memory.Block.Capacity(a1)
	bne.w bad
	tst.l memory.Block.Used(a1)
	bne.w bad
	movea.l Context.Package(a0), a0
	jsr frontend.scratchSize
	bne.w bad
	move.l d1, d0
	lea Scratch, a0
	jsr memory.reserveExact
	bne.w bad
	move.l Scratch+memory.Block.Pointer, Front+frontend.Frame.Scratch
	movea.l Caller, a0
	move.l Context.Package(a0), Front+frontend.Frame.Package
	move.l Context.Source(a0), Cursor
	move.l Context.Source(a0), d0
	beq.w bad
	add.l Context.Bytes(a0), d0
	bcs.w bad
	move.l d0, After
	move.l #LINE_CAPACITY, d0
	lea Line, a0
	jsr memory.reserveExact
	bne.w bad
	move.l Line+memory.Block.Pointer, Front+frontend.Frame.Output
	move.l #LINE_CAPACITY, Front+frontend.Frame.Capacity
	lea Metadata, a0
	jsr metadata.clear
	move.l #Metadata, Front+frontend.Frame.Metadata
	move.w #1, Front+frontend.Frame.RootFile
	move.l #1, Started
	lea Front, a0
	jsr frontend.begin
	bne.w bad
nextLine
	move.l Cursor, d0
	cmp.l After, d0
	bhs.w prepared
	movea.l d0, a1
	movea.l a1, a2
scanLine
	cmpa.l After, a2
	bhs.w lineReady
	cmpi.b #10, (a2)
	beq.w lineReady
	cmpi.b #13, (a2)
	beq.w lineReady
	addq.l #1, a2
	bra.w scanLine
lineReady
	move.l a1, Front+frontend.Frame.Source
	move.l a2, d0
	sub.l a1, d0
	move.l d0, Front+frontend.Frame.SourceBytes
skipEndings
	cmpa.l After, a2
	bhs.w cursorReady
	move.b (a2), d0
	cmpi.b #10, d0
	beq.w skipOne
	cmpi.b #13, d0
	bne.w cursorReady
skipOne
	addq.l #1, a2
	; Blank physical lines remain lines; consume only CR followed by LF.
	cmpi.b #13, d0
	bne.w cursorReady
	cmpa.l After, a2
	bhs.w cursorReady
	cmpi.b #10, (a2)
	bne.w cursorReady
	addq.l #1, a2
cursorReady
	move.l a2, Cursor
	lea Front, a0
	jsr frontend.line
	bne.w bad
drain
	tst.w Front+frontend.Frame.SourcePending
	bne.w bad
	bsr.w appendRecord
	bne.w bad
	lea Front, a0
	jsr frontend.expansionDepth
	tst.l d0
	beq.w nextLine
	lea Front, a0
	jsr frontend.nextExpansion
	bne.w bad
	bra.w drain
prepared
	lea Front, a0
	jsr frontend.endFile
	bne.w bad
	movea.l Records+memory.Block.Pointer, a1
	move.l Records+memory.Block.Used, d0
	jsr frontend.complete
	bne.w bad
	bsr.w rejectControls
	bne.w bad
	movea.l Records+memory.Block.Pointer, a0
	move.l Records+memory.Block.Used, d0
	lea Index, a1
	jsr blocks.index
	bne.w bad
	tst.l d1
	beq.w bad
	lea LayoutState, a0
	movea.l Records+memory.Block.Pointer, a1
	move.l Records+memory.Block.Used, d0
	movea.l Index+memory.Block.Pointer, a2
	move.l Front+frontend.Frame.NameCount, d1
	jsr layout.begin
	bne.w bad
	bsr.w exportState
	bne.w bad
	lea PublicState, a0
	lea Front, a1
	jsr exports.prepare
	bne.w exportBad
	; No source-oriented owner remains when assembly begins.
	bsr.w retireFrontend
	bne.w bad
	move.l Front+frontend.Frame.NameCount, d0
	lea Cells, a1
	move.l #ValueOwner, package.Context.Owner(a1)
	lea Symbols, a0
	jsr storage.grow
	bne.w bad
	movea.l Caller, a0
	.MEMORY_LAYOUT #0, Records+memory.Block.Used, Context.Bytes(a0)
	move.l Context.Package(a0), Cells+package.Context.Package
	move.l Parameters+memory.Block.Pointer, Cells+package.Context.Parameters
	move.l Records+memory.Block.Pointer, Work+assembly.Frame.Records
	move.l Records+memory.Block.Used, Work+assembly.Frame.RecordBytes
	; A fixed image cannot freeze layout-dependent absolute observations.
	move.w #package.POSITION_OBSERVATION_POLICY, Cells+package.Context.ExpressionPolicy
	move.l #Cells, Work+assembly.Frame.Context
	move.l #allocateOutput, Work+assembly.Frame.Allocate
	move.l #captureAbsolute, Work+assembly.Frame.AddReloc
	move.l #captureInstruction, Work+assembly.Frame.Instruction
	move.l #emitted, Work+assembly.Frame.Emitted
	move.l Symbols+storage.State.MemberKeys+memory.Block.Pointer, Work+assembly.Frame.MemberKeys
	lea Work, a0
	jsr assembly.assemble
	bne.w executionBad
	movea.l Work+assembly.Frame.Sections, a0
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a0)
	bne.w bad
	tst.w sections.State.OutputSeen(a0)
	beq.w bad
	lea LayoutState, a0
	lea Work, a1
	jsr layout.finish
	bne.w bad
	lea PublicState, a0
	lea Cells, a1
	lea SemanticOwner, a2
	jsr exports.publish
	bne.w publicationBad
	bsr.w saveImage
	bne.w imageBad
	movea.l Destination, a1
	move.l ImageOwner+memory.Block.Pointer, memory.Block.Pointer(a1)
	move.l ImageOwner+memory.Block.Capacity, memory.Block.Capacity(a1)
	move.l ImageOwner+memory.Block.Used, memory.Block.Used(a1)
	clr.l ImageOwner+memory.Block.Pointer
	clr.l ImageOwner+memory.Block.Capacity
	clr.l ImageOwner+memory.Block.Used
	moveq #0, d7
	bra.w done
exportBad
	moveq #EXPORT_INVALID, d7
	bra.w done
executionBad
	moveq #EXECUTION_INVALID, d7
	bra.w done
publicationBad
	moveq #PUBLICATION_INVALID, d7
	bra.w done
imageBad
	moveq #IMAGE_INVALID, d7
	bra.w done
bad
	moveq #INVALID, d7
done
	bsr.w cleanup
	clr.l Active
	move.l d7, d0
	bra.w exit
busy
	moveq #1, d0
exit
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; compile

	.priv
appendRecord	.block
	move.l Front+frontend.Frame.Used, d2
	beq.w good
	lea Records, a0
	move.l memory.Block.Used(a0), d3
	move.l d3, d0
	add.l d2, d0
	bcs.w bad
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l d3, a1
	movea.l Front+frontend.Frame.Output, a0
	move.l d2, d0
	bsr.w copy
	add.l d2, Records+memory.Block.Used
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; appendRecord

; Only bounded record extents are inspected here. PRVM owns control grammar.
rejectControls	.block
	movea.l Records+memory.Block.Pointer, a3
	movea.l a3, a4
	adda.l Records+memory.Block.Used, a4
	movea.l Front+frontend.Frame.Package, a2
next
	cmpa.l a4, a3
	beq.w good
	bhi.w bad
	moveq #0, d6
	move.b source.RecordHeader.Length(a3), d6
	addq.l #1, d6
	cmpi.l #source.RECORD_HEADER_BYTES, d6
	blo.w bad
	move.l a4, d0
	sub.l a3, d0
	cmp.l d0, d6
	bhi.w bad
	movea.l a3, a0
	move.l d6, d0
	movea.l a2, a1
	adda.l package.Header.BranchPlan(a1), a1
	jsr branch.classify
	bne.w bad
	tst.l d1
	bne.w bad
	movea.l a3, a0
	move.l d6, d0
	movea.l a2, a1
	move.l package.Header.LoopPlanBytes(a1), d1
	adda.l package.Header.LoopPlan(a1), a1
	jsr loops.classify
	cmpi.l #abi.PRVM_LOOP_ORDINARY, d0
	bne.w bad
	adda.l d6, a3
	bra.w next
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; rejectControls

exportState	.block
	move.l Front+frontend.Frame.NameCount, d0
	lea ValueOwner, a0
	jsr values.init
	bne.w done
	lea Front, a0
	lea ValueOwner, a1
	jsr frontend.exportBindings
	bne.w done
	jsr frontend.exportSeeds
	bne.w done
	lea Front, a0
	jsr frontend.parameterBytes
	move.l d1, Cells+package.Context.ParameterCount
	move.l d0, Cells+package.Context.ParameterBytes
	tst.l d0
	beq.w members
	lea Parameters, a0
	jsr memory.reserveExact
	bne.w done
	move.l Cells+package.Context.ParameterBytes, d0
	move.l d0, Parameters+memory.Block.Used
	movea.l Parameters+memory.Block.Pointer, a1
	lea Front, a0
	jsr frontend.copyParameters
	bne.w done
members
	move.l Front+frontend.Frame.NameCount, d0
	add.l d0, d0
	lea Symbols+storage.State.MemberKeys, a0
	jsr memory.reserveExact
	bne.w done
	move.l Front+frontend.Frame.NameCount, d0
	add.l d0, d0
	move.l d0, Symbols+storage.State.MemberKeys+memory.Block.Used
	movea.l Symbols+storage.State.MemberKeys+memory.Block.Pointer, a1
	lea Front, a0
	jsr frontend.copyMemberKeys
	move.w Front+frontend.Frame.AsciiName, Work+assembly.Frame.AsciiName
	move.w Front+frontend.Frame.PetsciiName, Work+assembly.Frame.PetsciiName
done
	tst.l d0
	rts
	.bend  ; exportState

retireFrontend	.block
	tst.l Started
	beq.w good
	lea Front, a0
	jsr frontend.finish
	move.l d0, -(sp)
	clr.l Started
	lea Scratch, a0
	jsr memory.release
	lea Line, a0
	jsr memory.release
	move.l (sp)+, d0
	tst.l d0
	rts
good
	moveq #0, d0
	rts
	.bend  ; retireFrontend

; Assembly artifact callbacks preserve every register except status.
allocateOutput	.block
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	lea Output, a0
	jsr memory.reserveExact
	move.l Output+memory.Block.Pointer, assembly.Frame.Output(a1)
	move.l Output+memory.Block.Capacity, assembly.Frame.Capacity(a1)
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; allocateOutput
captureAbsolute	.block
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	lea LayoutState, a0
	jsr layout.absolute
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; captureAbsolute

captureInstruction	.block
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	lea LayoutState, a0
	jsr layout.instruction
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; captureInstruction

; The layout owner maps final emission to indexed lexical block spans.
emitted	.block
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	lea LayoutState, a0
	jsr layout.emitted
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; emitted

saveImage	.block
	move.l SemanticOwner+memory.Block.Pointer, Build+image.Frame.Semantics
	move.l SemanticOwner+memory.Block.Used, Build+image.Frame.SemanticsBytes
	move.l LayoutState+layout.State.Inputs+memory.Block.Pointer, Build+image.Frame.Blocks
	move.l LayoutState+layout.State.Count, Build+image.Frame.Count
	move.l LayoutState+layout.State.Relocs+memory.Block.Pointer, Build+image.Frame.Relocs
	move.l LayoutState+layout.State.Relocs+memory.Block.Used, d0
	moveq #image.RELOC_BYTES, d1
	divu.l d1, d0
	move.l d0, Build+image.Frame.RelocCount
	clr.l Build+image.Frame.Output
	lea Build, a0
	jsr image.build
	bne.w bad
	move.l Build+image.Frame.Used, d0
	lea ImageOwner, a0
	jsr memory.reserveExact
	bne.w bad
	move.l Build+image.Frame.Used, d0
	move.l d0, ImageOwner+memory.Block.Used
	move.l ImageOwner+memory.Block.Pointer, Build+image.Frame.Output
	move.l ImageOwner+memory.Block.Capacity, Build+image.Frame.Capacity
	lea Build, a0
	jsr image.build
	rts
bad
	moveq #1, d0
	rts
	.bend  ; saveImage

cleanup	.block
	bsr.w retireFrontend
	lea LayoutState, a0
	jsr layout.release
	lea PublicState, a0
	jsr exports.release
	lea SemanticOwner, a0
	jsr memory.release
	lea Metadata, a0
	jsr metadata.release
	lea Symbols, a0
	jsr storage.release
	lea ValueOwner, a0
	jsr values.release
	lea Scratch, a0
	jsr memory.release
	lea Line, a0
	jsr memory.release
	lea Records, a0
	jsr memory.release
	lea Index, a0
	jsr memory.release
	lea Parameters, a0
	jsr memory.release
	lea Output, a0
	jsr memory.release
	lea ImageOwner, a0
	jsr memory.release
	; Owners have released. Retire every transient borrow and counter, including
	; image input pointers, while Active remains set until the compiler exits.
	lea Caller, a0
	move.l #TransientEnd-Caller, d0
clear
	cmpi.l #4, d0
	blo.w tail
	clr.l (a0)+
	subq.l #4, d0
	bra.w clear
tail
	tst.l d0
	beq.w done
	clr.b (a0)+
	subq.l #1, d0
	bra.w tail
done
	rts
	.bend  ; cleanup

copy	.block
longs
	cmpi.l #4, d0
	blo.w tail
	move.l (a0)+, (a1)+
	subq.l #4, d0
	bra.w longs
tail
	tst.l d0
	beq.w done
next
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w next
done
	rts
	.bend  ; copy
	.endsection
	.endmodule

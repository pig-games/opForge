; Restricted real-source producer for independently relocatable leaf blocks.
; @opforge-owner: opforge.assembler.modules.compiler
	.module opforge.assembler.modules.compiler
	.cpu 68020
	.use opforge.assembler.prepare.frontend as frontend
	.use opforge.assembler.layout.assembly as assembly
	.use opforge.assembler.layout.encoding as encoder
	.use opforge.assembler.layout.hunk_references as references
	.use opforge.assembler.prepare.expression as expression
	.use opforge.vm.exprvm.runtime as exprvm
	.use opforge.packages.format as package
	.use opforge.platform.memory as memory
	.use opforge.assembler.modules.image as image
	.use opforge.assembler.modules.assembly_interface as assemblyInterface
	.use opforge.assembler.modules.exports as exports
	.use opforge.assembler.modules.layout as layout
	.use opforge.assembler.modules.publication as publication
	.use opforge.assembler.modules.admission as admission
	.use opforge.assembler.modules.templates as moduleTemplates
	.use opforge.assembler.symbols.scope_layout as scope
	.use opforge.assembler.modules.bindings as modules
	.use opforge.assembler.layout.block_index as blocks
	.use opforge.assembler.symbols.storage as storage
	.use opforge.assembler.values.values as values
	.use opforge.assembler.layout.sections as sections
	.use opforge.assembler.prepare.metadata as metadata
	.use opforge.assembler.source.records as source
	.use opforge.vm.prvm.packed_branch as branch
	.use opforge.vm.prvm.packed_loop as loops
	.use opforge.vm.prvm.abi as abi
	.include "memory_telemetry.i"
	.pub
Context	.struct
Package	.long ?
Source	.long ?
Bytes	.long ?
Initialize	.long ?  ; optional A0=private frontend,A1=Context -> D0 status
Replay	.long ?  ; optional A0=frontend,A1=owned records block,A2=Context
Seed	.long ?  ; optional A0=assembly frame,A1=Context -> D0 status
Opaque	.long ?  ; callback-owned borrowed request state
Resolve	.long ?  ; optional exact external address resolver, A0=Opaque
Inventory	.long ?  ; A0=frontend,A1=Index,A2=Context,A3=Records, copy inventory
Facets	.long ?  ; optional empty Block; receives owned public template capsule
CloseTemplates	.long ?  ; A0=owned OFTM Block,A1=Context -> checked identity closure
CloseValues	.long ?  ; A0=OFMS Block,A1=Context -> normalize dependency identities
Dependencies	.long ?  ; optional borrowed Block of assemblyInterface.Dependency rows
Imports	.long ?  ; optional borrowed Block of assemblyInterface.Import rows
Seal	.long ?  ; A0=frontend,A1=Context -> retire caller's lexical borrows
	.endstruct
CONTEXT_BYTES = Context.Seal+4
LINE_CAPACITY = 256
INVALID = 1
EXPORT_INVALID = 2
EXECUTION_INVALID = 3
PUBLICATION_INVALID = 4
IMAGE_INVALID = 5
UNSUPPORTED = 6
ADDRESS_INVALID = 7; public address cannot be bound to an owned fragment
REPLAY_UNSUPPORTED = 2
	.priv
; Stack-owned scalar callback transaction; no pointer survives its return.
DataFrame	.struct
Delta	.res layout.DATA_DELTA_BYTES
Cursor	.long ?
After	.long ?
Value	.long ?
Unknown	.long ?
Policy	.word ?
Mode	.word ?
	.endstruct
DATA_FRAME_BYTES = DataFrame.Mode+2
PAIR_DATA = 0
PAIR_SCALAR = 1
; The shared native assembly engine is itself a single active session. This
; producer is sequential, never callable during another assembly session.
	.section bss, kind=bss
Active	.res long, 1
	; Align the complete transient owner once. Its prefix is long-aligned,
	; so Front needs no interior alignment boundary and cleanup spans one unit.
	.align 4
Caller	.res long, 1
Destination	.res long, 1
Started	.res long, 1
Cursor	.res long, 1
After	.res long, 1
SectionMask	.res long, 1
SectionCount	.res long, 1
InterfaceOnly	.res long, 1
SectionOrder	.res byte, sections.HUNK_SLOT_COUNT
FirstLexical	.res long, 1
OwnModule	.res long, 1
InitialModules	.res byte, memory.Block.Used+4
Front	.res byte, frontend.FRAME_BYTES
Work	.res byte, assembly.FRAME_BYTES
Cells	.res byte, package.CONTEXT_BYTES
Symbols	.res byte, storage.STATE_BYTES
ValueOwner	.res byte, values.OWNER_BYTES
Build	.res byte, image.FRAME_BYTES
InterfaceBuild	.res byte, assemblyInterface.FRAME_BYTES
Scratch	.res byte, memory.Block.Used+4
Line	.res byte, memory.Block.Used+4
Records	.res byte, memory.Block.Used+4
Index	.res byte, memory.Block.Used+4
Parameters	.res byte, memory.Block.Used+4
Output	.res byte, memory.Block.Used+4
ImageOwner	.res byte, memory.Block.Used+4
SemanticOwner	.res byte, memory.Block.Used+4
InterfaceOwner	.res byte, memory.Block.Used+4
FacetOwner	.res byte, memory.Block.Used+4
FacetRequest	.res byte, moduleTemplates.CAPTURE_BYTES
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
; Declaration-only interfaces publish zero blocks without synthetic sections
; or output selection. Their optional CPU reset remains a caller-owned preamble.
; Nested whole blocks and mandatory owner-zero fragments share selected Hunk
; code/data/BSS sections. Package-owned branch recipes preserve
; the chosen instruction width when blocks move; shared affine label identity
; preserves absolute instruction/data references. References without an exact
; source identity reject. Counted loops require known placement-independent
; counts; runtime conditionals and iterable/scoped loops reject.
; Constant source preprocessing and private macros may complete before assembly.
; Publishes immutable scalars, lists/ranges and struct schemas with local IDs.
; Public addresses retain exact fragment/offset identity; compound values retain
; their canonical schema/member closure. Facets requests closed public templates
; and statements in the stored assembly interface, and receives an optional owned
; capsule for component callers. Normal CLI dependencies consume the stored
; interface through bounded readers, without installing a shared frontend copy.
; Mutable/deferred exports, warnings and unsupported layout recipes reject.
; Optional callbacks seed imports, configure backed lexical storage and replay
; captured binary records without a source-text dependency.
; Replay closes the file/expansion context; this owner completes preparation.
; Replay status REPLAY_UNSUPPORTED returns UNSUPPORTED after private cleanup.
; Package/source/callback state are borrowed for this call. Private owners retire;
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
	move.l Context.Facets(a0), d0
	beq.w destinationsDistinct
	cmp.l a1, d0
	beq.w bad
destinationsDistinct
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
	tst.l Context.Replay(a0)
	bne.w inputReady
	move.l Context.Source(a0), Cursor
	move.l Context.Source(a0), d0
	beq.w bad
	add.l Context.Bytes(a0), d0
	bcs.w bad
	move.l d0, After
inputReady
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
	movea.l Caller, a1
	movea.l Context.Initialize(a1), a3
	cmpa.w #0, a3
	beq.w initialized
	lea Front, a0
	jsr (a3)
	tst.l d0
	bne.w callbackBad
initialized
	movea.l Front+frontend.Frame.Scratch, a0
	moveq #0, d0
	move.w frontend.SCOPE_STATE+scope.State.Count(a0), d0
	move.l d0, FirstLexical
	bsr.w captureModules
	bne.w bad
	movea.l Caller, a2
	movea.l Context.Replay(a2), a3
	cmpa.w #0, a3
	beq.w nextLine
	lea Front, a0
	lea Records, a1
	jsr (a3)
	tst.l d0
	bne.w callbackBad
	tst.w Front+frontend.Frame.SourcePending
	bne.w notSupported
	; A replay borrower may swap the stable CLI frontend while draining the
	; captured span. Restore this owner's active grammar before completion.
	lea Front, a0
	jsr frontend.activate
	bne.w bad
	bra.w complete
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
complete
	movea.l Records+memory.Block.Pointer, a0
	move.l Records+memory.Block.Used, d0
	movea.l Caller, a1
	movea.l Context.Package(a1), a1
	jsr admission.interfaceOnly
	tst.l d0
	beq.w interfaceRecords
	cmpi.l #admission.UNSUPPORTED, d0
	bne.w bad
	movea.l Caller, a0
	tst.l Context.Replay(a0)
	beq.w completeRecords
	; The private selection belongs to ordinary layout normalization. Append it
	; before completion validates unplaced concrete sections against the output.
	bsr.w selectPrivateSections
	bne.w notSupported
	bra.w completeRecords
interfaceRecords
	move.l #1, InterfaceOnly
completeRecords
	lea Front, a0
	movea.l Records+memory.Block.Pointer, a1
	move.l Records+memory.Block.Used, d0
	jsr frontend.complete
	bne.w bad
	movea.l Caller, a0
	tst.l Context.Replay(a0)
	beq.w recordsReady
	bsr.w findOwnModule
	bne.w notSupported
recordsReady
	bsr.w rejectControls
	bne.w bad
	movea.l Records+memory.Block.Pointer, a0
	move.l Records+memory.Block.Used, d0
	lea Index, a1
	jsr blocks.index
	bne.w bad
	lea LayoutState, a0
	movea.l Records+memory.Block.Pointer, a1
	move.l Records+memory.Block.Used, d0
	movea.l Index+memory.Block.Pointer, a2
	move.l Front+frontend.Frame.NameCount, d1
	jsr layout.begin
	bne.w bad
	movea.l Caller, a2
	tst.l Context.Resolve(a2)
	beq.w layoutReady
	lea LayoutState, a0
	lea resolveExternal, a1
	jsr layout.configureExternal
	bne.w bad
layoutReady
	movea.l Caller, a2
	movea.l Context.Inventory(a2), a3
	cmpa.w #0, a3
	beq.w inventoryReady
	lea Front, a0
	lea Index, a1
	movea.l a3, a4
	lea Records, a3
	jsr (a4)
	tst.l d0
	bne.w callbackBad
inventoryReady
	bsr.w exportState
	bne.w bad
	lea PublicState, a0
	lea Front, a1
	move.l OwnModule, d0
	movea.l Caller, a2
	tst.l Context.Facets(a2)
	beq.w strictExports
	movea.l Context.Facets(a2), a0
	move.l memory.Block.Pointer(a0), d0
	or.l memory.Block.Capacity(a0), d0
	or.l memory.Block.Used(a0), d0
	bne.w exportBad
	move.l #Front, FacetRequest+moduleTemplates.CaptureFrame.Frontend
	move.l OwnModule, FacetRequest+moduleTemplates.CaptureFrame.Module
	move.l #FacetOwner, FacetRequest+moduleTemplates.CaptureFrame.Destination
	lea FacetRequest, a0
	jsr moduleTemplates.capture
	bne.w exportBad
	movea.l Caller, a1
	movea.l Context.CloseTemplates(a1), a2
	cmpa.w #0, a2
	beq.w templateClosureReady
	lea FacetOwner, a0
	jsr (a2)
	tst.l d0
	bne.w exportBad
templateClosureReady
	lea PublicState, a0
	lea Front, a1
	move.l OwnModule, d0
	jsr exports.prepareModuleValues
	bne.w exportBad
	lea PublicState, a0
	lea Front, a1
	movea.l FacetOwner+memory.Block.Pointer, a2
	move.l FacetOwner+memory.Block.Used, d0
	jsr exports.prepareTemplateClosure
	bra.w exportsReady
strictExports
	jsr exports.prepareModule
exportsReady
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
	move.l #seedSymbols, Work+assembly.Frame.Seed
	move.l #captureReserved, Work+assembly.Frame.Reserved
	move.l #captureAlignment, Work+assembly.Frame.Aligned
	move.l #captureDataExpression, Work+assembly.Frame.DataExpression
	move.l #emitted, Work+assembly.Frame.Emitted
	move.l Symbols+storage.State.MemberKeys+memory.Block.Pointer, Work+assembly.Frame.MemberKeys
	lea captureScalarExpression, a0
	jsr encoder.configureScalarCapture
	lea Work, a0
	jsr assembly.assemble
	; The encoder borrows this session only. Retire the callback before any
	; failure path frees Work, numeric values or the pending proof owner.
	move.l d0, -(sp)
	suba.l a0, a0
	jsr encoder.configureScalarCapture
	move.l (sp)+, d0
	tst.l d0
	bne.w executionBad
	; Reference replay owns diagnostics and --Werror handling exactly once.
	tst.l Work+assembly.Frame.Warnings
	bne.w notSupported
	tst.l InterfaceOnly
	bne.w interfaceAssembled
	movea.l Work+assembly.Frame.Sections, a0
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a0)
	bne.w bad
	tst.w sections.State.OutputSeen(a0)
	beq.w bad
	bra.w finishLayout
interfaceAssembled
	; The numeric proof admits no runtime effect. Check its assembled result
	; before publishing a zero-block image; never manufacture a section or PC.
	tst.l Work+assembly.Frame.Used
	bne.w notSupported
	tst.l LayoutState+layout.State.Count
	bne.w notSupported
	tst.l LayoutState+layout.State.Pending+memory.Block.Used
	bne.w notSupported
	tst.l LayoutState+layout.State.Relocs+memory.Block.Used
	bne.w notSupported
finishLayout
	lea LayoutState, a0
	lea Work, a1
	jsr layout.finish
	bne.w bad
	lea PublicState, a0
	lea Cells, a1
	lea SemanticOwner, a2
	jsr exports.publish
	bne.w publicationBad
	lea SemanticOwner, a0
	lea LayoutState, a1
	jsr publication.finalize
	bne.w addressBad
	movea.l Caller, a1
	movea.l Context.CloseValues(a1), a2
	cmpa.w #0, a2
	beq.w valueClosureReady
	lea SemanticOwner, a0
	jsr (a2)
	tst.l d0
	bne.w publicationBad
valueClosureReady
	bsr.w saveImage
	bne.w imageBad
	movea.l Destination, a1
	move.l ImageOwner+memory.Block.Pointer, memory.Block.Pointer(a1)
	move.l ImageOwner+memory.Block.Capacity, memory.Block.Capacity(a1)
	move.l ImageOwner+memory.Block.Used, memory.Block.Used(a1)
	clr.l ImageOwner+memory.Block.Pointer
	clr.l ImageOwner+memory.Block.Capacity
	clr.l ImageOwner+memory.Block.Used
	movea.l Caller, a0
	movea.l Context.Facets(a0), a1
	cmpa.w #0, a1
	beq.w transferred
	move.l FacetOwner+memory.Block.Pointer, memory.Block.Pointer(a1)
	move.l FacetOwner+memory.Block.Capacity, memory.Block.Capacity(a1)
	move.l FacetOwner+memory.Block.Used, memory.Block.Used(a1)
	clr.l FacetOwner+memory.Block.Pointer
	clr.l FacetOwner+memory.Block.Capacity
	clr.l FacetOwner+memory.Block.Used
transferred
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
addressBad
	moveq #ADDRESS_INVALID, d7
	bra.w done
imageBad
	moveq #IMAGE_INVALID, d7
	bra.w done
callbackBad
	cmpi.l #REPLAY_UNSUPPORTED, d0
	beq.w notSupported
	bra.w bad
notSupported
	moveq #UNSUPPORTED, d7
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
; Configuration can intern the owning module's name before replay opens it.
; Snapshot module roles rather than assuming every older identity is an import.
; D0/CCR=status; other registers scratch. The bitmap owns no source spelling.
captureModules	.block
	move.l FirstLexical, d0
	addq.l #7, d0
	lsr.l #3, d0
	lea InitialModules, a0
	jsr memory.reserveExact
	bne.w bad
	move.l FirstLexical, d0
	addq.l #7, d0
	lsr.l #3, d0
	move.l d0, InitialModules+memory.Block.Used
	movea.l InitialModules+memory.Block.Pointer, a2
	movea.l a2, a0
clear
	tst.l d0
	beq.w flags
	clr.b (a0)+
	subq.l #1, d0
	bra.w clear
flags
	moveq #0, d5
	tst.l FirstLexical
	beq.w good
	movea.l Front+frontend.Frame.Scratch, a0
	movea.l frontend.SCOPE_STATE+scope.MODULE_STATE+modules.FLAGS_POINTER(a0), a1
	cmpa.w #0, a1
	beq.w bad
next
	cmp.l FirstLexical, d5
	bhs.w good
	move.w (a1)+, d1
	andi.w #modules.MODULE_ID, d1
	beq.w advance
	move.l d5, d0
	lsr.l #3, d0
	move.l d5, d1
	andi.l #7, d1
	lea 0(a2, d0.l), a3
	bset d1, (a3)
advance
	addq.l #1, d5
	bra.w next
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; captureModules

; Exactly one module role must become active during replay, so its interface
; cannot re-export seeded dependencies. OwnModule is lexical index+1.
; D0/CCR=status; other registers scratch.
findOwnModule	.block
	clr.l OwnModule
	movea.l Front+frontend.Frame.Scratch, a0
	lea frontend.SCOPE_STATE(a0), a0
	moveq #0, d6
	move.w scope.State.Count(a0), d6
	move.l FirstLexical, d0
	cmp.l d6, d0
	bhi.w bad
	moveq #0, d5
	movea.l InitialModules+memory.Block.Pointer, a2
	movea.l scope.MODULE_STATE+modules.FLAGS_POINTER(a0), a1
	cmpa.w #0, a1
	beq.w bad
next
	cmp.l d6, d5
	bhs.w complete
	move.l d5, d0
	add.l d0, d0
	move.w 0(a1, d0.l), d1
	andi.w #modules.MODULE_ID, d1
	beq.w advance
	cmp.l FirstLexical, d5
	bhs.w newModule
	move.l d5, d0
	lsr.l #3, d0
	move.l d5, d1
	andi.l #7, d1
	lea 0(a2, d0.l), a3
	btst d1, (a3)
	bne.w advance
newModule
	tst.l OwnModule
	bne.w bad
	move.l d5, d0
	addq.l #1, d0
	move.l d0, OwnModule
advance
	addq.l #1, d5
	bra.w next
complete
	tst.l OwnModule
	beq.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; findOwnModule

; Add a private geometry selection before shared completion resolves pending
; section recipes. Its empty artifact path never reaches an output writer.
; Declaration order is retained; only concrete section slots are admitted.
; D0/CCR=status. Registers scratch; Records may move while appending.
selectPrivateSections	.block
	clr.l SectionMask
	clr.l SectionCount
	movea.l Records+memory.Block.Pointer, a3
	movea.l a3, a4
	adda.l Records+memory.Block.Used, a4
next
	cmpa.l a4, a3
	beq.w append
	bhi.w bad
	move.l a4, d0
	sub.l a3, d0
	cmpi.l #source.RECORD_HEADER_BYTES, d0
	blo.w bad
	moveq #0, d6
	move.b source.RecordHeader.Length(a3), d6
	addq.l #1, d6
	cmpi.l #source.RECORD_HEADER_BYTES, d6
	blo.w bad
	cmp.l d0, d6
	bhi.w bad
	btst #source.FLAG_LAYOUT_BIT, source.RecordHeader.Flags(a3)
	beq.w advance
	cmpi.l #source.LayoutRecord.Opcode+1, d6
	blo.w bad
	moveq #0, d0
	move.b source.LayoutRecord.Opcode(a3), d0
	cmpi.b #sections.CONTROL_OUTPUT, d0
	beq.w bad
	cmpi.b #21, d0  ; shared flat artifact descriptor
	beq.w bad
	cmpi.b #sections.CONTROL_LOGICAL_SECTION, d0
	beq.w bad
	cmpi.b #sections.CONTROL_SECOND_LOGICAL_SECTION, d0
	beq.w bad
	cmpi.b #sections.CONTROL_MAPPED_CONCRETE, d0
	beq.w bad
	cmpi.b #sections.CONTROL_SECOND_MAPPED_CONCRETE, d0
	beq.w bad
	cmpi.b #source.LAYOUT_PENDING_LOGICAL, d0
	beq.w bad
	cmpi.b #source.LAYOUT_PENDING_CONCRETE, d0
	beq.w section
	cmpi.b #sections.CONTROL_FIRST_SECTION, d0
	beq.w section
	cmpi.b #sections.CONTROL_SECOND_SECTION, d0
	beq.w section
	cmpi.b #sections.CONTROL_EXTRA_SECTION, d0
	bne.w advance
section
	cmpi.l #source.CONCRETE_SECTION_BYTES, d6
	bne.w bad
	moveq #0, d1
	move.b source.ConcreteSection.Slot(a3), d1
	cmpi.l #sections.HUNK_SLOT_COUNT, d1
	bhs.w bad
	move.l SectionMask, d2
	btst d1, d2
	bne.w advance
	bset d1, d2
	move.l d2, SectionMask
	move.l SectionCount, d0
	lea SectionOrder, a0
	move.b d1, 0(a0, d0.l)
	addq.l #1, SectionCount
advance
	adda.l d6, a3
	bra.w next
append
	move.l SectionCount, d6
	beq.w bad
	move.l d6, d7
	add.l d7, d7
	addq.l #8, d7
	lea Records, a0
	move.l memory.Block.Used(a0), d5
	move.l d5, d0
	add.l d7, d0
	bcs.w bad
	jsr memory.reserve
	bne.w bad
	movea.l Records+memory.Block.Pointer, a1
	adda.l d5, a1
	move.l d7, d0
	subq.l #1, d0
	move.b d0, source.RecordHeader.Length(a1)
	move.b #source.FLAG_LAYOUT, source.RecordHeader.Flags(a1)
	clr.w source.RecordHeader.Line(a1)
	move.b #sections.CONTROL_OUTPUT, source.LayoutRecord.Opcode(a1)
	move.b d6, source.LayoutRecord.Opcode+1(a1)
	adda.w #source.LayoutRecord.Opcode+2, a1
	lea SectionOrder, a0
slots
	moveq #0, d0
	move.b (a0)+, d0
	move.w d0, (a1)+
	subq.l #1, d6
	bne.w slots
	move.b #2, (a1)+  ; shared output kind: Hunk
	clr.b (a1)  ; private geometry has no artifact path
	add.l d7, Records+memory.Block.Used
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; selectPrivateSections

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
	btst #source.FLAG_LAYOUT_BIT, source.RecordHeader.Flags(a3)
	beq.w ordinaryControl
	cmpi.l #source.LayoutRecord.Opcode+1, d6
	blo.w bad
	move.b source.LayoutRecord.Opcode(a3), d0
	cmpi.b #sections.CONTROL_FIRST_SECTION, d0
	beq.w nextRecord
	cmpi.b #sections.CONTROL_SECOND_SECTION, d0
	beq.w nextRecord
	cmpi.b #sections.CONTROL_EXTRA_SECTION, d0
	beq.w nextRecord
	cmpi.b #sections.CONTROL_END_SECTION, d0
	beq.w nextRecord
	cmpi.b #sections.CONTROL_OUTPUT, d0
	bne.w bad
	bra.w nextRecord
ordinaryControl
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
	beq.w nextRecord
	cmpi.l #abi.PRVM_LOOP_ENDFOR, d0
	beq.w nextRecord
	; The ordinary repetition owner proves each reached count against seeded
	; values under POSITION_OBSERVATION_POLICY before it emits any body bytes.
	; Iterables, scoped repetition and while controls retain reference handling.
	cmpi.l #abi.PRVM_LOOP_MATCHED, d0
	bne.w bad
	cmpi.l #-1, d1
	bne.w bad
nextRecord
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
	movea.l Caller, a1
	movea.l Context.Seal(a1), a2
	cmpa.w #0, a2
	beq.w finish
	lea Front, a0
	jsr (a2)
	tst.l d0
	bne.w returned
finish
	lea Front, a0
	jsr frontend.finish
	move.l d0, -(sp)
	clr.l Started
	lea Scratch, a0
	jsr memory.release
	lea Line, a0
	jsr memory.release
	move.l (sp)+, d0
returned
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
; A0=wrapper,A1=end,A2=Context,A3=package,D0=actual field PC,D6=span.
; DATA_UNHANDLED leaves A0 unchanged. DATA_HANDLED returns ExprVM cursor,
; D1=value,D2=unknown; DATA_INVALID fails closed. D3-D7/A1-A6 preserved.
; Only an exact whole-symbol pair can temporarily leave strict policy. Final
; selected fields must capture a complete layout-owned recipe or fail.
captureDataExpression	.block
	move.l d7, -(sp)
	moveq #PAIR_DATA, d7
	bsr.w capturePair
	move.l (sp)+, d7
	tst.l d0
	rts
	.bend  ; captureDataExpression

; Encoder-owned scalar projection ABI. Only exact pairs may leave strict
; evaluation; finish must prove their cancellation and mandatory owner lifetime.
captureScalarExpression	.block
	movem.l d6-d7, -(sp)
	move.l package.Context.Pc(a2), d0
	moveq #0, d6
	moveq #PAIR_SCALAR, d7
	bsr.w capturePair
	movem.l (sp)+, d6-d7
	tst.l d0
	rts
	.bend  ; captureScalarExpression

capturePair	.block
	movem.l d3-d7/a1-a6, -(sp)
	suba.w #DATA_FRAME_BYTES, sp
	movea.l sp, a4
	movea.l a2, a5
	move.l a0, DataFrame.Cursor(a4)
	move.l d0, DataFrame.Delta+layout.Delta.FieldPc(a4)
	move.l d6, DataFrame.Delta+layout.Delta.Span(a4)
	move.w d7, DataFrame.Mode(a4)
	tst.w d7
	bne.w recognize
	cmpi.l #2, d6
	beq.w recognize
	cmpi.l #4, d6
	bne.w unhandled
recognize
	moveq #0, d0
	tst.w DataFrame.Mode(a4)
	bne.w endianReady
	move.w package.Header.LittleEndian(a3), d0
endianReady
	move.l d0, DataFrame.Delta+layout.Delta.LittleEndian(a4)
	jsr references.scalarDifference
	cmpi.l #references.STATUS_CLEAR, d0
	beq.w unhandled
	cmpi.l #references.STATUS_SECTION, d0
	bne.w invalid
	tst.w DataFrame.Mode(a4)
	beq.w pairReady
	cmpa.l a1, a0
	bne.w unhandled  ; scalar capture must own the complete operand span
pairReady
	move.l d1, DataFrame.Delta+layout.Delta.TargetSymbol(a4)
	move.l d2, DataFrame.Delta+layout.Delta.BaseSymbol(a4)
	; Ordinary absolute subtraction remains on the strict shared evaluator.
	; Unknown pass-one pairs may reserve their fixed field, but final pairs
	; require two real section labels before any policy is relaxed.
	movea.l package.Context.Defined(a5), a6
	tst.b 0(a6, d1.l)
	beq.w provisional
	tst.b 0(a6, d2.l)
	beq.w provisional
	jsr references.baseSection
	bne.w unhandled
	subq.l #1, d1
	move.l d1, DataFrame.Delta+layout.Delta.TargetSection(a4)
	move.l d2, d1
	jsr references.baseSection
	bne.w unhandled
	subq.l #1, d1
	move.l d1, DataFrame.Delta+layout.Delta.BaseSection(a4)
	bra.w evaluate
provisional
	cmpi.w #1, package.Context.Pass(a5)
	bne.w invalid
evaluate
	movea.l DataFrame.Cursor(a4), a0
	move.w package.Context.ExpressionPolicy(a5), DataFrame.Policy(a4)
	bclr #package.POSITION_OBSERVATION_POLICY_BIT, package.Context.ExpressionPolicy+1(a5)
	jsr expression.evaluate
	movea.l a5, a2
	move.w DataFrame.Policy(a4), package.Context.ExpressionPolicy(a2)
	tst.l d0
	bne.w invalid
	move.l a0, DataFrame.After(a4)
	move.l d1, DataFrame.Value(a4)
	move.l d2, DataFrame.Unknown(a4)
	cmpi.w #2, package.Context.Pass(a2)
	bne.w handled
	tst.l d2
	bne.w invalid
	movea.l Work+assembly.Frame.Sections, a6
	tst.w sections.State.HunkSelected(a6)
	beq.w handled
	; Bind exact current anchor values, never the evaluated difference.
	movea.l package.Context.Values(a2), a6
	move.l DataFrame.Delta+layout.Delta.TargetSymbol(a4), d3
	lsl.l #3, d3
	tst.l exprvm.Value.High(a6, d3.l)
	bne.w invalid
	move.l exprvm.Value.Low(a6, d3.l), DataFrame.Delta+layout.Delta.TargetValue(a4)
	move.l DataFrame.Delta+layout.Delta.BaseSymbol(a4), d3
	lsl.l #3, d3
	tst.l exprvm.Value.High(a6, d3.l)
	bne.w invalid
	move.l exprvm.Value.Low(a6, d3.l), DataFrame.Delta+layout.Delta.BaseValue(a4)
	lea LayoutState, a0
	lea Work, a1
	lea DataFrame.Delta(a4), a2
	tst.w DataFrame.Mode(a4)
	beq.w dataRecipe
	jsr layout.scalarDifference
	bra.w captured
dataRecipe
	jsr layout.dataDelta
captured
	tst.l d0
	bne.w invalid
handled
	movea.l DataFrame.After(a4), a0
	move.l DataFrame.Value(a4), d1
	move.l DataFrame.Unknown(a4), d2
	moveq #assembly.DATA_HANDLED, d0
	bra.w done
unhandled
	movea.l DataFrame.Cursor(a4), a0
	moveq #assembly.DATA_UNHANDLED, d0
	bra.w done
invalid
	movea.l DataFrame.Cursor(a4), a0
	moveq #assembly.DATA_INVALID, d0
done
	movea.l a5, a2
	adda.w #DATA_FRAME_BYTES, sp
	movem.l (sp)+, d3-d7/a1-a6
	tst.l d0
	rts
	.bend  ; capturePair

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

; Exact import resolver: A0=compiler Context,D1=private symbol,D2=source
; section,D3=value. D0/CCR=status,D1=bit31|global identity,D2=addend.
; The caller-owned callback receives A0=Context.Opaque. Others preserved.
resolveExternal	.block
	movem.l d3-d7/a0-a6, -(sp)
	movea.l Context.Resolve(a0), a3
	cmpa.w #0, a3
	beq.w bad
	movea.l Context.Opaque(a0), a0
	jsr (a3)
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d3-d7/a0-a6
	tst.l d0
	rts
	.bend  ; resolveExternal

; Seed the fresh assembly cells after the shared runtime initializes them.
; A0=assembly frame. D0/CCR=status; all other caller registers preserved.
seedSymbols	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l Caller, a1
	movea.l Context.Seed(a1), a3
	cmpa.w #0, a3
	beq.w good
	jsr (a3)
	bra.w done
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; seedSymbols

captureReserved	.block
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	lea LayoutState, a0
	jsr layout.reserved
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; captureReserved

captureAlignment	.block
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	lea LayoutState, a0
	jsr layout.aligned
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; captureAlignment

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
	; Keep the complete assembly interface in the object. Separate consumer
	; windows will borrow its values and templates after this compiler retires.
	move.l SemanticOwner+memory.Block.Pointer, InterfaceBuild+assemblyInterface.Frame.Values
	move.l SemanticOwner+memory.Block.Used, InterfaceBuild+assemblyInterface.Frame.ValueBytes
	move.l FacetOwner+memory.Block.Pointer, InterfaceBuild+assemblyInterface.Frame.Templates
	move.l FacetOwner+memory.Block.Used, InterfaceBuild+assemblyInterface.Frame.TemplateBytes
	movea.l Caller, a2
	movea.l Context.Dependencies(a2), a1
	cmpa.w #0, a1
	beq.w imports
	move.l memory.Block.Pointer(a1), InterfaceBuild+assemblyInterface.Frame.Dependencies
	move.l memory.Block.Used(a1), d0
	lsr.l #3, d0
	move.l d0, InterfaceBuild+assemblyInterface.Frame.DependencyCount
imports
	movea.l Context.Imports(a2), a1
	cmpa.w #0, a1
	beq.w buildInterface
	move.l memory.Block.Pointer(a1), InterfaceBuild+assemblyInterface.Frame.Imports
	move.l memory.Block.Used(a1), d0
	lsr.l #3, d0
	move.l d0, InterfaceBuild+assemblyInterface.Frame.ImportCount
buildInterface
	clr.l InterfaceBuild+assemblyInterface.Frame.Output
	lea InterfaceBuild, a0
	jsr assemblyInterface.build
	bne.w bad
	move.l InterfaceBuild+assemblyInterface.Frame.Used, d0
	lea InterfaceOwner, a0
	jsr memory.reserveExact
	bne.w bad
	move.l InterfaceBuild+assemblyInterface.Frame.Used, InterfaceOwner+memory.Block.Used
	move.l InterfaceOwner+memory.Block.Pointer, InterfaceBuild+assemblyInterface.Frame.Output
	move.l InterfaceOwner+memory.Block.Capacity, InterfaceBuild+assemblyInterface.Frame.Capacity
	lea InterfaceBuild, a0
	jsr assemblyInterface.build
	bne.w bad
	move.l InterfaceOwner+memory.Block.Pointer, Build+image.Frame.Semantics
	move.l InterfaceOwner+memory.Block.Used, Build+image.Frame.SemanticsBytes
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
	lea InterfaceOwner, a0
	jsr memory.release
	lea FacetOwner, a0
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
	lea InitialModules, a0
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

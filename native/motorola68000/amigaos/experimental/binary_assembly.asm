; Two bounded assembly passes over binary source, with no source-text interface.
; Mapped Hunk layout first measures concrete prefixes in a disposable pass.
; @opforge-owner: experimental.amigaos.binary_assembly

	.module experimental.amigaos.binary_assembly
	.cpu 68020
	.use experimental.amigaos.binary_package as pkg
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_parameters as parameters
	.use experimental.amigaos.binary_state as state
	.use opasm.amigaos.binary_expression as expr
	.use exprvm.amigaos.runtime as exprvm
	.use prvm.amigaos.abi as dataabi
	.use prvm.amigaos.macro_runtime as datavm
	.use experimental.amigaos.binary_encoding as encoding
	.use experimental.amigaos.binary_dependencies as dependencies
	.use experimental.amigaos.binary_mutable as mutable
	.use experimental.amigaos.binary_constants as constants
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_sections as sections
	.use experimental.amigaos.binary_hunk_mapping as mapping
	.use experimental.amigaos.binary_hunk_placement as placement
	.use experimental.amigaos.binary_hunk_references as hunkrefs
	.use experimental.amigaos.binary_repetition as repetition
	.include "memory_telemetry.i"
	.pub

Frame	.struct
Records	.long ?
RecordBytes	.long ?
Context	.long ?
Output	.long ?
Capacity	.long ?
Used	.long ?
Line	.word ?
Failure	.word ?
Allocate	.long ?
RecordOffset	.long ?
Sections	.long ?
AddReloc	.long ?
Emitted	.long ?
	.endstruct

OutputReloc	.struct
Source	.long ?
Target	.long ?
Offset	.long ?
	.endstruct
OUTPUT_RELOC_BYTES = OutputReloc.Offset+4
FRAME_BYTES = Frame.Emitted+4
FAILURE_NONE = 0
FAILURE_MUTABLE_LAYOUT = 1
MAX_LAYOUT_ROUNDS = 8

	.section bss, kind=bss
	.priv
Active
	.res long, 1
LayoutRounds
	.res word, 1
	.align 4
DataPc	.res long, 1
DataBytes
	.res byte, 4
SectionState
	.res byte, sections.SCRATCH_BYTES
RepeatState
	.res byte, repetition.STATE_BYTES
HunkInstructionRefs
	.res word, 1
HunkInstructionRefCount
	.res word, 1
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	.pub
TraversalPasses
	.res long, 1
TraversalRecords
	.res long, 1
	.priv
.endif
.endif
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	.pub
FailureStage
	.res long, 1
FailureName
	.res long, 1
Position
	.res byte, AssemblyPosition.Count+4
	.priv
.endif
.endif
.endif
	.endsection
	.section code, kind=code
	.pub

; A0=Frame. Context values/defined arrays cover Count entries. The caller owns
; all buffers. Returns D0=0 only for two complete passes; Used=output bytes.
; Allocate callback: A0=Frame, D0=pass-one output size; returns D0/CCR status,
; preserves other registers, supplies Frame.Output/Capacity before pass two.
; Optional Emitted(A0=Frame,D1=address,D2=buffer offset,D3=bytes) runs only
; for nonempty initialized final-pass emissions, before advancing state. It returns D0/CCR
; status and preserves other registers. No text/CPU grammar enters the callback.
; Other registers preserved; CCR reflects D0. No text/dictionary pointer enters
; this module. Variable-size convergence and discontiguous origins are unsupported.
assemble	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	move.l a0, Active
	.ASSEMBLY_FAILURE_STAGE FailureStage, #0
	.ASSEMBLY_POSITION_CLEAR Position
	.MEMORY_COUNTER_CLEAR TraversalPasses
	.MEMORY_COUNTER_CLEAR TraversalRecords
	clr.l Frame.Used(a5)
	clr.w LayoutRounds
	clr.w Frame.Failure(a5)
	move.l #-1, Frame.RecordOffset(a5)
	movea.l Frame.Context(a5), a6
	movea.l pkg.Context.Owner(a6), a0
	move.l a0, d0
	beq.w scalarOwner
	jsr values.reset
	bne.w fail
scalarOwner
	move.l pkg.Context.Count(a6), d0
	beq.w fail
	cmpi.l #65536, d0
	bhi.w fail
	movea.l pkg.Context.Values(a6), a0
	movea.l pkg.Context.Defined(a6), a1
	movea.l pkg.Context.SectionIds(a6), a2
clearSymbols
	clr.l (a0)+
	clr.l (a0)+
	clr.b (a1)+
	clr.b (a2)+
	subq.l #1, d0
	bne.w clearSymbols
	movea.l a6, a0
	jsr parameters.load
	bne.w fail
	movea.l Frame.Records(a5), a0
	move.l Frame.RecordBytes(a5), d0
	movea.l a6, a2
	jsr dependencies.resolve
	tst.l d0
	bne.w fail
	movea.l Frame.Records(a5), a0
	move.l Frame.RecordBytes(a5), d0
	lea SectionState, a1
	movea.l pkg.Context.Package(a6), a2
	move.l pkg.Header.MaxAddress(a2), d1
	jsr sections.scan
	bne.w fail
	lea SectionState+placement.BASES, a0
	move.l a0, pkg.Context.SectionBases(a6)
	lea SectionState, a1
	movea.l Frame.Records(a5), a0
	move.l Frame.RecordBytes(a5), d0
	movea.l pkg.Context.Package(a6), a2
	move.l pkg.Header.MaxAddress(a2), d1
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a1)
	bne.w placementReady
	jsr placement.scan
	bne.w fail
placementReady
	move.l #SectionState, Frame.Sections(a5)
	movea.l Frame.Records(a5), a0
	move.l Frame.RecordBytes(a5), d0
	lea SectionState, a1
	move.w sections.State.Mode(a1), d2
	jsr mutable.validateLayout
	beq.w layoutReady
	move.l d1, Frame.RecordOffset(a5)
	cmpi.l #mutable.UNSUPPORTED_LAYOUT, d0
	bne.w fail
	move.w #FAILURE_MUTABLE_LAYOUT, Frame.Failure(a5)
	bra.w fail
layoutReady
	clr.w pkg.Context.Relocatable(a6)
	lea SectionState, a0
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a0)
	bne.w outputReady
	move.w #1, pkg.Context.Relocatable(a6)
	jsr mapping.validate
	bne.w fail
outputReady
	moveq #1, d7
pass
	move.w d7, pkg.Context.Pass(a6)
	clr.l pkg.Context.Pc(a6)
	clr.l Frame.Used(a5)
	lea SectionState, a0
	movea.l a6, a1
	jsr sections.beginPass
	moveq #0, d5  ; ordinary single sweep
	lea SectionState, a0
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a0)
	beq.w sweep
	cmpi.w #2, sections.State.Mode(a0)
	beq.w oneMap
	cmpi.w #4, sections.State.Mode(a0)
	bne.w sweep
	moveq #3, d5  ; two maps: four section sweeps, then outside controls
	bra.w sweep
oneMap
	moveq #1, d5  ; explicit map: concrete sweep, then remaining records
	bra.w sweep
sweep
	moveq #0, d4  ; section selection state for mapped sweeps
sweepRecords
	movem.l a2, -(sp)
	movea.l pkg.Context.Package(a6), a2
	jsr state.reset
	movem.l (sp)+, a2
	bne.w fail
	.MEMORY_COUNTER_INC TraversalPasses
	.ASSEMBLY_POSITION Position, d7, d5, SectionState, sections.State.Mode, sections.State.HunkCurrent, sections.State.OrderCount
	lea RepeatState, a0
	jsr repetition.begin
	movea.l Frame.Records(a5), a4
	move.l a4, d0
	add.l Frame.RecordBytes(a5), d0
	bcs.w fail
	movea.l d0, a3
line
	.ASSEMBLY_FAILURE_STAGE FailureStage, #1
	move.l a4, d0
	sub.l Frame.Records(a5), d0
	move.l d0, Frame.RecordOffset(a5)
	cmpa.l a3, a4
	beq.w passDone
	bhi.w fail
	moveq #0, d6
	move.b (a4), d6
	addq.w #1, d6
	cmpi.w #4, d6
	blo.w fail
	move.l a3, d0
	sub.l a4, d0
	cmp.l d0, d6
	bhi.w fail
	move.w 2(a4), Frame.Line(a5)
	moveq #0, d0
	move.b 1(a4), d0
	cmpi.b #source.FLAG_ALLOWED, d0
	bhi.w fail
	btst #4, d0
	bne.w controlsReady
	btst #3, d0
	bne.w omitted
	; Loop traversal precedes output selection, so skipped/repeated bodies also
	; govern state directives in every mapped sweep.
	movea.l a4, a0
	movea.l a3, a1
	movea.l a6, a2
	lea RepeatState, a3
	.ASSEMBLY_FAILURE_STAGE FailureStage, #4
	jsr repetition.step
	movea.l a1, a3
	cmpi.l #2, d0
	beq.w fail
	tst.l d0
	beq.w controlsReady
	movea.l a0, a4
	bra.w line
controlsReady
	tst.w d5
	beq.w stateReady
	; Replay state in source order even when section sweeps filter byte emission.
	; Omitted conditional/reachability records never change package state.
	movem.l a0-a2, -(sp)
	movea.l a4, a0
	movea.l a4, a1
	adda.w d6, a1
	movea.l pkg.Context.Package(a6), a2
	bsr.w stateRecord
	movem.l (sp)+, a0-a2
	tst.l d0
	bne.w fail
stateReady
	cmpi.w #3, d5
	bhs.w pairedSelect
	; An explicit map needs concrete bytes and labels before the imported
	; logical section. Filter the same packed records into two ordered sweeps.
	tst.w d5
	beq.w selected
	btst #4, 1(a4)
	beq.w selectStatement
	cmpi.w #5, d6
	blo.w fail
	cmpi.b #6, 4(a4)
	beq.w concreteOpen
	cmpi.b #3, 4(a4)
	bne.w selectOther
	tst.w d4
	beq.w selectOther
	moveq #0, d4
	bra.w selectConcrete
concreteOpen
	tst.w d4
	bne.w fail
	moveq #1, d4
selectConcrete
	cmpi.w #1, d5
	beq.w selected
	bra.w omitted
selectOther
	cmpi.w #1, d5
	beq.w omitted
	tst.w d4
	bne.w omitted
	bra.w selected
selectStatement
	cmpi.w #1, d5
	bne.w selectRemaining
	tst.w d4
	beq.w omitted
	bra.w selected
selectRemaining
	tst.w d4
	bne.w omitted
	bra.w selected
pairedSelect
	btst #4, 1(a4)
	beq.w pairedStatement
	cmpi.w #5, d6
	blo.w fail
	moveq #0, d0
	move.b 4(a4), d0
	cmpi.w #3, d0
	beq.w pairedClose
	cmpi.w #1, d0
	beq.w pairedOpen
	cmpi.w #2, d0
	beq.w pairedOpen
	cmpi.w #6, d0
	beq.w pairedOpen
	cmpi.w #7, d0
	beq.w pairedOpen
	cmpi.w #10, d0
	beq.w pairedOpen
	cmpi.w #11, d0
	beq.w pairedOpen
	cmpi.w #7, d5
	beq.w selected  ; region and place controls run after all section bodies
	bra.w omitted
pairedOpen
	tst.w d4
	bne.w fail
	moveq #2, d4
	cmpi.w #3, d5
	bne.w pairedFirstLogical
	cmpi.w #6, d0
	bne.w pairedOpenDone
	bra.w pairedChosen
pairedFirstLogical
	cmpi.w #4, d5
	bne.w pairedSecondConcrete
	cmpi.w #1, d0
	bne.w pairedOpenDone
	bra.w pairedChosen
pairedSecondConcrete
	cmpi.w #5, d5
	bne.w pairedSecondLogical
	cmpi.w #11, d0
	bne.w pairedOpenDone
	bra.w pairedChosen
pairedSecondLogical
	cmpi.w #6, d5
	bne.w pairedOpenDone
	cmpi.w #10, d0
	bne.w pairedOpenDone
pairedChosen
	moveq #1, d4
pairedOpenDone
	cmpi.w #1, d4
	beq.w selected
	bra.w omitted
pairedClose
	tst.w d4
	beq.w fail
	move.w d4, d0
	moveq #0, d4
	cmpi.w #1, d0
	beq.w selected
	bra.w omitted
pairedStatement
	cmpi.w #1, d4
	beq.w selected
	cmpi.w #7, d5
	bne.w omitted
	tst.w d4
	bne.w omitted
	bra.w selected
selected
	.MEMORY_COUNTER_INC TraversalRecords
	.ASSEMBLY_FAILURE_STAGE FailureStage, #3
	move.w 2(a4), Frame.Line(a5)
	moveq #0, d0
	move.b 1(a4), d0
	cmpi.b #source.FLAG_ALLOWED, d0
	bhi.w fail
	btst #4, d0
	bne.w layoutControl
	btst #3, d0
	bne.w omitted
ordinaryStatement
	moveq #0, d0
	move.b 1(a4), d0
	andi.w #source.FLAG_INDENT, d0
	lea 4(a4), a0
	movea.l a4, a1
	adda.l d6, a1
	movea.l a6, a2
	.ASSEMBLY_FAILURE_STAGE FailureStage, #5
	bsr.w statement
	cmpi.l #2, d0
	beq.w passDone
	tst.l d0
	bne.w fail
	bra.w omitted
layoutControl
	.ASSEMBLY_FAILURE_STAGE FailureStage, #6
	lea SectionState, a0
	movea.l a6, a1
	movea.l a4, a2
	jsr sections.control
	bne.w fail
	.ASSEMBLY_POSITION Position, d7, d5, SectionState, sections.State.Mode, sections.State.HunkCurrent, sections.State.OrderCount
omitted
	adda.l d6, a4
	bra.w line
passDone
	.ASSEMBLY_FAILURE_STAGE FailureStage, #7
	lea RepeatState, a0
	jsr repetition.end
	bne.w fail
	; Continue the same assembly pass at the current PC/output offset.
	cmpi.w #3, d5
	bhs.w pairedPassDone
	cmpi.w #1, d5
	bne.w sweepDone
	tst.w d4
	bne.w fail
	moveq #2, d5
	bra.w sweep
pairedPassDone
	tst.w d4
	bne.w fail
	cmpi.w #7, d5
	beq.w sweepDone
	addq.w #1, d5
	bra.w sweep
sweepDone
	.ASSEMBLY_FAILURE_STAGE FailureStage, #9
	tst.w d4
	bne.w fail
	lea SectionState, a0
	movea.l a6, a1
	jsr sections.finishPass
	bne.w fail
	lea SectionState, a0
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a0)
	bne.w finishedLayout
	move.l d1, Frame.Used(a5)
	moveq #0, d2  ; provisional geometry changed, requiring a fresh pass one
	tst.w sections.State.MapCount(a0)
	beq.w placedBounds
	tst.w sections.State.MapReady(a0)
	beq.w mappedFreeze
	jsr mapping.check
	beq.w placedBounds
	cmpi.w #1, d7
	bne.w fail
mappedFreeze
	jsr mapping.freeze
	bne.w fail
	moveq #1, d2
placedBounds
	tst.w sections.State.PlaceCount(a0)
	beq.w layoutCompared
	jsr placement.freeze
	bne.w fail
	or.l d1, d2
layoutCompared
	tst.l d2
	beq.w stableLayout
	cmpi.w #1, d7
	bne.w fail
	addq.w #1, LayoutRounds
	cmpi.w #MAX_LAYOUT_ROUNDS, LayoutRounds
	bhs.w fail
	jsr dependencies.resetLayout
	bne.w fail
	bra.w pass
stableLayout
	tst.w sections.State.MapCount(a0)
	beq.w finishedLayout
	cmpi.w #2, d7
	bne.w finishedLayout
	jsr mapping.merge
	bne.w fail
finishedLayout
	cmpi.w #1, d7
	bne.w nextPass
	.ASSEMBLY_FAILURE_STAGE FailureStage, #18
	movea.l Frame.Allocate(a5), a1
	movea.l a5, a0
	move.l Frame.Used(a5), d0
	jsr (a1)
	tst.l d0
	bne.w fail
nextPass
	addq.w #1, d7
	cmpi.w #3, d7
	blo.w pass
	moveq #0, d0
	bra.w done
fail
	clr.l Frame.Used(a5)
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; assemble

	.priv

; A0=valid prepared record,A1=end,A2=package. D0/CCR=status; others preserved.
; Inspect only the packed statement prefix; expressions and encodings stay in VMs.
stateRecord	.block
	movem.l d1/a0-a1, -(sp)
	move.b 1(a0), d0
	andi.w #source.FLAG_LAYOUT+source.FLAG_OMIT, d0
	bne.w ok
	addq.l #4, a0
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #5, d0
	blo.w ok
	cmpi.b #1, (a0)
	bhi.w directive
	cmpi.b #5, 4(a0)
	bne.w ok
	addq.l #5, a0
directive
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #5, d0
	blo.w ok
	cmpi.b #7, (a0)+
	bne.w ok
	cmpi.b #1, (a0)
	bhi.w ok
	tst.b 3(a0)
	bne.w ok
	moveq #0, d0
	move.w 1(a0), d0
	addq.l #4, a0
	cmp.w pkg.Header.CpuDirective(a2), d0
	beq.w resetCpu
	jsr state.apply
	cmpi.l #2, d0
	beq.w ok
	bra.w done
resetCpu
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	bne.w bad
	cmpi.b #1, (a0)
	bhi.w bad
	tst.b 3(a0)
	bne.w bad
	move.w 1(a0), d0
	cmp.w pkg.Header.CpuName(a2), d0
	bne.w bad
	jsr state.reset
	bra.w done
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; stateRecord

; A0/A1=bounded token range, A2=Context, D0=indent flag. D0 status, 2=.end.
statement	.block
	.ASSEMBLY_FAILURE_STAGE FailureStage, #10
	movem.l d1-d7/a0-a6, -(sp)
	move.l d0, d7
	movea.l pkg.Context.Package(a2), a3
	cmpa.l a1, a0
	beq.w ok
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #5, d0
	blo.w dispatch
	cmpi.b #1, (a0)
	bhi.w dispatch
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, 4(a0)
	beq.w constant
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, 4(a0)
	beq.w mutableDeclaration
	cmpi.b #source.TOKEN_CONDITIONAL_DECLARATION, 4(a0)
	beq.w mutableDeclaration
	cmpi.b #source.TOKEN_IGNORED_DECLARATION, 4(a0)
	beq.w ok
	cmpi.b #5, 4(a0)
	bne.w dispatch
	lea SectionState, a4
	tst.w sections.State.Mode(a4)
	beq.w labelSectionReady
	tst.w sections.State.Active(a4)
	beq.w bad
labelSectionReady
	.ASSEMBLY_FAILURE_STAGE FailureStage, #11
	tst.b 3(a0)
	bne.w bad
	moveq #0, d0
	move.w 1(a0), d0
	cmp.w pkg.Header.NameCount(a3), d0
	blo.w bad  ; package spellings are reserved in this bounded experiment
	cmp.l pkg.Context.Count(a2), d0
	bhs.w bad
	movea.l pkg.Context.Defined(a2), a4
	movea.l pkg.Context.Values(a2), a5
	move.l d0, d1
	lsl.l #3, d1
	cmpi.w #1, pkg.Context.Pass(a2)
	bne.w existingLabel
	tst.b 0(a4, d0.l)
	bne.w bad
	move.b #1, 0(a4, d0.l)
	move.l pkg.Context.Pc(a2), exprvm.Value.Low(a5, d1.l)
	clr.l exprvm.Value.High(a5, d1.l)
	lea SectionState, a4
	cmpi.w #5, sections.State.Mode(a4)
	bne.w labelReady
	movea.l pkg.Context.SectionIds(a2), a5
	moveq #0, d1
	move.w sections.State.HunkCurrent(a4), d1
	addq.b #1, d1
	move.b d1, 0(a5, d0.l)
	bra.w labelReady
existingLabel
	tst.l exprvm.Value.High(a5, d1.l)
	bne.w bad
	move.l pkg.Context.Pc(a2), d2
	cmp.l exprvm.Value.Low(a5, d1.l), d2
	bne.w bad  ; fail closed if this subset needs another layout iteration
labelReady
	addq.l #5, a0
	moveq #1, d7  ; an explicit label permits an instruction on the same line
dispatch
	cmpa.l a1, a0
	beq.w ok
	cmpi.b #7, (a0)
	beq.w directive
	tst.l d7
	beq.w bad
	bsr.w name
	bne.w bad
	; Name returns the numeric identity and qualifier without source reconstruction.
	clr.w HunkInstructionRefs
	clr.w HunkInstructionRefCount
	lea SectionState, a4
	cmpi.w #5, sections.State.Mode(a4)
	bne.w instructionReady
	cmpi.w #2, pkg.Context.Pass(a2)
	bne.w instructionReady
	move.l d0, d5
	movea.l a0, a5
	move.w d1, -(sp)  ; retain the mnemonic qualifier across reference counting
	jsr hunkrefs.tokens
	move.w d1, HunkInstructionRefCount
	move.w (sp)+, d1
	cmpi.l #hunkrefs.STATUS_BAD, d0
	beq.w bad
	move.w d0, HunkInstructionRefs
	movea.l a5, a0
	move.l d5, d0
instructionReady
	.ASSEMBLY_FAILURE_STAGE FailureStage, #12
	.ASSEMBLY_FAILURE_STAGE FailureName, d0
	jsr encoding.encode
	tst.l d0
	bne.w bad
	movea.l a1, a5
	move.l d1, d5
	.ASSEMBLY_FAILURE_STAGE FailureStage, #14
	bsr.w markInstructionRelocs
	bne.w bad
	movea.l a1, a0
	move.l d1, d0
	.ASSEMBLY_FAILURE_STAGE FailureStage, #17
	bsr.w emit
	bra.w done
mutableDeclaration
	jsr mutable.execute
	bra.w done
constant
	.ASSEMBLY_FAILURE_STAGE FailureStage, #15
	moveq #0, d0
	move.b -3(a0), d0
	jsr constants.execute
	bra.w done
directive
	.ASSEMBLY_FAILURE_STAGE FailureStage, #16
	addq.l #1, a0
	bsr.w name
	bne.w bad
	tst.l d1
	bne.w bad
	cmp.w pkg.Header.CpuDirective(a3), d0
	beq.w cpu
	cmp.w pkg.Header.OrgDirective(a3), d0
	beq.w origin
	cmp.w pkg.Header.EndDirective(a3), d0
	beq.w end
	cmp.w pkg.Header.AlignDirective(a3), d0
	beq.w align
	cmp.w pkg.Header.ResDirective(a3), d0
	beq.w reserve
	cmp.w pkg.Header.EmitDirective(a3), d0
	beq.w packedData
	moveq #1, d6
	cmp.w pkg.Header.ByteDirective(a3), d0
	beq.w data
	moveq #2, d6
	cmp.w pkg.Header.WordDirective(a3), d0
	beq.w data
	moveq #4, d6
	cmp.w pkg.Header.LongDirective(a3), d0
	beq.w data
	movem.l a2, -(sp)
	movea.l a3, a2
	jsr state.apply
	movem.l (sp)+, a2
	tst.l d0
	beq.w ok
	bra.w bad
packedData
	bsr.w emitData
	bra.w done
cpu
	bsr.w name
	bne.w bad
	tst.l d1
	bne.w bad
	cmp.w pkg.Header.CpuName(a3), d0
	bne.w bad  ; the capsule declares this experiment's single pipeline
	cmpa.l a1, a0
	bne.w bad
	movem.l a2, -(sp)
	movea.l a3, a2
	jsr state.reset
	movem.l (sp)+, a2
	bra.w done
origin
	lea SectionState, a4
	tst.w sections.State.Mode(a4)
	bne.w bad
	movea.l a2, a6
	jsr expr.evaluate
	movea.l a6, a2
	tst.l d0
	bne.w bad
	tst.l d2
	bne.w bad
	tst.l pkg.Context.High(a2)
	bne.w bad
	tst.l d1
	bmi.w bad
	cmpa.l a1, a0
	bne.w bad
	cmp.l pkg.Header.MaxAddress(a3), d1
	bhi.w bad
	movea.l Active, a4
	tst.l Frame.Used(a4)
	beq.w setOrigin
	cmp.l pkg.Context.Pc(a2), d1
	bne.w bad
setOrigin
	move.l d1, pkg.Context.Pc(a2)
	bra.w ok
end
	cmpa.l a1, a0
	bne.w bad
	moveq #2, d0
	bra.w done
align
	movea.l a2, a6
	jsr expr.evaluate
	movea.l a6, a2
	tst.l d0
	bne.w bad
	tst.l d2
	bne.w bad
	tst.l pkg.Context.High(a2)
	bne.w bad
	cmpa.l a1, a0
	bne.w bad
	tst.l d1
	ble.w bad
	move.l d1, d6
	subq.l #1, d6
	move.l d6, d0
	and.l d1, d0
	bne.w bad
	move.l pkg.Context.Pc(a2), d3
	move.l d3, d0
	add.l d6, d0
	bcs.w bad
	not.l d6
	and.l d0, d6
	sub.l d3, d6
	move.l d6, d0
	lea SectionState, a4
	cmpi.w #3, sections.State.ActiveKind(a4)
	bne.w alignBytes
	movea.l a4, a0
	movea.l a2, a1
	jsr sections.reserve
	bra.w done
alignBytes
	bsr.w emitGap
	bra.w done
reserve
	bsr.w name
	bne.w bad
	tst.l d1
	bne.w bad
	moveq #1, d6
	cmp.w pkg.Header.ByteDirective(a3), d0
	beq.w reserveCount
	moveq #2, d6
	cmp.w pkg.Header.WordDirective(a3), d0
	beq.w reserveCount
	moveq #4, d6
	cmp.w pkg.Header.LongDirective(a3), d0
	bne.w bad
reserveCount
	cmpa.l a1, a0
	bhs.w bad
	cmpi.b #4, (a0)+
	bne.w bad
	movea.l a2, a6
	jsr expr.evaluate
	movea.l a6, a2
	tst.l d0
	bne.w bad
	tst.l d2
	bne.w bad
	tst.l pkg.Context.High(a2)
	bne.w bad
	cmpa.l a1, a0
	bne.w bad
	tst.l d1
	bmi.w bad
	move.l d1, d0
	mulu.w d6, d0
	; MULU.W would truncate a large count; reject instead of reserving less.
	move.l d1, d3
	swap d3
	tst.w d3
	bne.w bad
	lea SectionState, a0
	movea.l a2, a1
	jsr sections.reserve
	bra.w done
data
	move.l pkg.Context.Pc(a2), DataPc
dataValueStart
	cmpa.l a1, a0
	bhs.w bad
	cmpi.b #3, (a0)
	bne.w dataExpression
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #3, d0
	blo.w bad
	moveq #0, d5
	move.b 1(a0), d5
	cmpi.w #2, d5
	blo.w bad  ; one-byte strings were lowered to scalar expressions
	move.l d5, d1
	addq.l #2, d1
	cmp.l d1, d0
	blo.w bad
	movea.l a0, a5
	lea 2(a0), a0
	move.l d5, d0
	bsr.w emit
	tst.l d0
	bne.w bad
	movea.l a5, a0
	adda.l d5, a0
	addq.l #2, a0
	bra.w dataNext
dataExpression
	movea.l a0, a5
	movea.l a2, a6
	move.l pkg.Context.Pc(a2), -(sp)
	move.l DataPc, pkg.Context.Pc(a2)
	jsr expr.evaluate
	movea.l a6, a2
	move.l (sp)+, pkg.Context.Pc(a2)
	tst.l d0
	bne.w bad
	tst.l d2
	beq.w dataValue
	cmpi.w #1, pkg.Context.Pass(a2)
	bne.w bad
	; Fixed-width unresolved data reserves layout in pass one. Its provisional
	; value and incomplete section provenance cannot prove range or relocation.
	moveq #0, d1
	bra.w dataEmitValue
dataValue
	cmpi.w #4, d6
	beq.w dataRangeOk
	; Word data accepts a signed 16-bit value or an unsigned 16-bit value.
	move.l pkg.Context.High(a2), d0
	beq.w unsignedData
	cmpi.l #-1, d0
	bne.w bad
	cmpi.w #2, d6
	bne.w bad
	tst.l d1
	bpl.w bad
	cmpi.l #-32768, d1
	blt.w bad
	bra.w dataRangeOk
unsignedData
	tst.l d1
	bmi.w bad
	cmpi.w #1, d6
	bne.w wordRange
	cmpi.l #255, d1
	bhi.w bad
	bra.w dataRangeOk
wordRange
	cmpi.l #65535, d1
	bhi.w bad
dataRangeOk
	bsr.w markDataReloc
	bne.w bad
dataEmitValue
	movea.l a0, a5
	lea DataBytes, a4
	move.l d6, d5
	tst.w pkg.Header.LittleEndian(a3)
	beq.w bigEndian
littleLoop
	move.b d1, (a4)+
	lsr.l #8, d1
	subq.w #1, d5
	bne.w littleLoop
	bra.w dataReady
bigEndian
	move.l d6, d4
	subq.l #1, d4
	lsl.l #3, d4
bigLoop
	move.l d1, d3
	lsr.l d4, d3
	move.b d3, (a4)+
	subi.l #8, d4
	subq.w #1, d5
	bne.w bigLoop
dataReady
	lea DataBytes, a0
	move.l d6, d0
	bsr.w emit
	tst.l d0
	bne.w bad
	movea.l a5, a0
dataNext
	cmpa.l a1, a0
	beq.w ok
	cmpi.b #4, (a0)+
	bne.w bad
	bra.w dataValueStart
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; statement

; Translate package-VM absolute-long output fixups to section-offset Hunk
; records. Instruction bytes and their fixup offsets came from the selected
; package program; no opcode or operand spelling is interpreted here.
; A2=Context,D5=instruction bytes,A5=output. Preserves caller registers.
markInstructionRelocs	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea SectionState, a4
	cmpi.w #5, sections.State.Mode(a4)
	bne.w good
	cmpi.w #2, pkg.Context.Pass(a2)
	bne.w good
	tst.w sections.State.HunkSelected(a4)
	beq.w good
	jsr encoding.outputFixupCount
	move.l d0, d6
	cmpi.w #hunkrefs.STATUS_SECTION, HunkInstructionRefs
	bne.w countReady
	jsr encoding.outputPositionProof
	tst.l d0
	beq.w absoluteOnly
	; Mixed positional and absolute fixups need per-reference accounting.
	; This bounded Hunk slice accepts one PC cancellation only when it is the
	; instruction's sole section reference and emits no Hunk relocation.
	cmpi.l #1, d0
	bne.w bad
	tst.l d6
	bne.w bad
	cmpi.w #1, HunkInstructionRefCount
	bne.w bad
	; Projection binds an opaque target directly to its canonical section.
	; The package VM proved cancellation; it never interprets this identity.
	move.w d1, d2
	moveq #0, d3
	move.w sections.State.HunkCurrent(a4), d3
	addq.w #1, d3
	cmp.w d3, d2
	bne.w bad
	bra.w countReady
absoluteOnly
	tst.l d6
	beq.w bad  ; fail closed without a package-proven instruction fixup
countReady
	moveq #0, d7
nextFixup
	cmp.l d6, d7
	bhs.w good
	move.l d7, d0
	jsr encoding.outputFixup
	tst.l d0
	bne.w bad
	cmpi.l #4, d2
	bne.w bad
	move.l d1, d2
	addq.l #4, d2
	bcs.w bad
	cmp.l d5, d2
	bhi.w bad
	move.l d1, -(sp)  ; package field offset
	move.l d4, -(sp)  ; encoded absolute address
	move.l d3, d1
	jsr hunkrefs.sectionBase
	bne.w fixupBad
	move.l (sp)+, d4
	; Instruction fixups permit signed addends represented in the long field.
	sub.l d1, d4
	move.l d3, -(sp)  ; canonical target section
	move.l d4, d1
	move.l d7, d0
	jsr encoding.patchOutputFixup
	bne.w targetBad
	move.l (sp)+, d4
	subq.l #1, d4
	move.l (sp)+, d2
	add.l pkg.Context.Pc(a2), d2
	bcs.w bad
	moveq #0, d1
	move.w sections.State.HunkCurrent(a4), d1
	addq.l #1, d1
	jsr hunkrefs.sectionBase
	bne.w bad
	sub.l d1, d2
	bcs.w bad
	move.l d4, d1
	moveq #0, d0
	move.w sections.State.HunkCurrent(a4), d0
	movea.l Active, a0
	movea.l Frame.AddReloc(a0), a1
	move.l a1, d3
	beq.w bad
	jsr (a1)
	tst.l d0
	bne.w bad
	addq.l #1, d7
	bra.w nextFixup
	; Keep the bounded fixup stack balanced on every validation failure.
targetBad
fixupBad
	addq.l #4, sp
	addq.l #4, sp
	bra.w bad
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; markInstructionRelocs

; Classify shared DATA expressions after the caller evaluates their scalar value.
; Same-section address differences are absolute; one surviving section base
; requires a long and records its section relocation only in pass two.
; A5=expression start,A1=bounded end,A2=Context,D6=unit bytes. D0/CCR=status.
; D1 returns the normalized scalar; other registers are preserved.
; ExprVM evaluation and unresolved handling stay caller-owned.
markDataReloc	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea SectionState, a4
	cmpi.w #5, sections.State.Mode(a4)
	bne.w good
	; Unselected sections evaluate data normally but have no Hunk payload.
	tst.w sections.State.HunkSelected(a4)
	beq.w good
	movea.l a5, a0
	jsr hunkrefs.affineTarget
	cmpi.l #hunkrefs.STATUS_CLEAR, d0
	beq.w good
	cmpi.l #hunkrefs.STATUS_SECTION, d0
	bne.w bad
	cmpi.w #4, d6
	bne.w bad
	cmpi.w #2, pkg.Context.Pass(a2)
	bne.w good
	jsr hunkrefs.baseSection
	bne.w bad
	move.l d1, d7
	jsr hunkrefs.sectionBase
	bne.w bad
	move.l (sp), d2  ; caller's evaluated scalar
	sub.l d1, d2
	bcs.w bad
	move.l d2, (sp)  ; returned value is now section relative
	moveq #0, d1
	move.w sections.State.HunkCurrent(a4), d1
	addq.l #1, d1
	jsr hunkrefs.sectionBase
	bne.w bad
	move.l pkg.Context.Pc(a2), d2
	sub.l d1, d2
	bcs.w bad
	move.l d7, d1
	subq.l #1, d1
	moveq #0, d0
	move.w sections.State.HunkCurrent(a4), d0
	movea.l Active, a0
	movea.l Frame.AddReloc(a0), a1
	move.l a1, d3
	beq.w bad
	jsr (a1)
	tst.l d0
	bne.w bad
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; markDataReloc

; Consume one bounded identifier token. D0=u16 ID, D1=u8 qualifier, A0 advances.
; CCR signals failure; on failure D0=-1. Other registers preserved.
name	.block
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w fail
	cmpi.b #1, (a0)
	bhi.w fail
	moveq #0, d0
	move.w 1(a0), d0
	moveq #0, d1
	move.b 3(a0), d1
	addq.l #4, a0
	cmp.l d0, d0  ; explicit successful CCR, independent of the numeric ID
	rts
fail
	moveq #-1, d0
	rts
	.bend  ; name

; D0=padding count,A2=Context. Flat address gaps are uninitialized; placed
; section payloads retain initialized padding, as in Rust's section replay.
; Bin/Hunk storage stays zero-filled. D0/CCR=status; other registers preserved.
; An already-aligned statement is a no-op.
emitGap	.block
	tst.l d0
	beq.w done
	movem.l d1/a0-a1, -(sp)
	movea.l Active, a1
	move.l Frame.Emitted(a1), d1
	tst.w SectionState+sections.State.Mode
	bne.w sectionPadding
	clr.l Frame.Emitted(a1)
sectionPadding
	suba.l a0, a0
	bsr.w emit
	move.l d1, Frame.Emitted(a1)
	movem.l (sp)+, d1/a0-a1
done
	tst.l d0
	rts
	.bend  ; emitGap

; A0=initialized bytes (zero means initialized zero-fill),D0=count,A2=Context.
; Updates PC and Used; only pass two copies output.
; D0=status; other registers preserved. CCR reflects D0.
emit	.block
	movem.l d1-d4/a0-a3, -(sp)
	movea.l a0, a3
	move.l d0, d4
	lea SectionState, a0
	movea.l a2, a1
	jsr sections.checkEmit
	bne.w fail
	movea.l a3, a0
	move.l d4, d0
	movea.l Active, a3
	move.l Frame.Used(a3), d1
	lea SectionState, a1
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a1)
	bne.w ordinaryOffset
	movem.l d0/a0-a1, -(sp)
	movea.l a1, a0
	movea.l a2, a1
	jsr sections.routeHunk
	movem.l (sp)+, d0/a0-a1
	bne.w fail
	tst.w d2
	beq.w unselectedEmit
	bra.w routedOffset
ordinaryOffset
	move.l Frame.Used(a3), d1
routedOffset
	move.l Frame.Used(a3), d2
	add.l d0, d2
	bcs.w fail
	cmpi.w #1, pkg.Context.Pass(a2)
	beq.w capacityReady
	cmp.l Frame.Capacity(a3), d2
	bhi.w fail
	move.l d1, d4
	add.l d0, d4
	bcs.w fail
	cmp.l Frame.Capacity(a3), d4
	bhi.w fail
capacityReady
	move.l pkg.Context.Pc(a2), d3
	add.l d0, d3
	bcs.w fail
	movea.l pkg.Context.Package(a2), a1
	move.l d3, d4
	tst.l d0
	beq.w checked
	subq.l #1, d4
	cmp.l pkg.Header.MaxAddress(a1), d4
	bhi.w fail
checked
	; Passive numeric observation is requested only by artifact consumers.
	cmpi.w #1, pkg.Context.Pass(a2)
	beq.w observed
	tst.l d0
	beq.w observed
	move.l Frame.Emitted(a3), d4
	beq.w observed
	movem.l d0-d4/a0-a3, -(sp)
	movea.l d4, a1
	move.l d0, d3
	move.l d1, d2
	move.l pkg.Context.Pc(a2), d1
	movea.l a3, a0
	jsr (a1)
	tst.l d0
	movem.l (sp)+, d0-d4/a0-a3
	bne.w fail
observed
	move.l d3, pkg.Context.Pc(a2)
	move.l d2, Frame.Used(a3)
	cmpi.w #1, pkg.Context.Pass(a2)
	beq.w ok
	movea.l Frame.Output(a3), a1
	adda.l d1, a1
	tst.l d0
	beq.w ok
	move.l a0, d3
	beq.w zeroCopy
copy
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copy
ok
	moveq #0, d0
	bra.w done
zeroCopy
	clr.b (a1)+
	subq.l #1, d0
	bne.w zeroCopy
	bra.w ok
unselectedEmit
	; Omitted payload still executes with a section-local PC and symbol state.
	move.l pkg.Context.Pc(a2), d3
	add.l d0, d3
	bcs.w fail
	move.l d3, pkg.Context.Pc(a2)
	bra.w ok
fail
	moveq #1, d0
done
	movem.l (sp)+, d1-d4/a0-a3
	tst.l d0
	rts
	.bend  ; emit

	.include "binary_data_emit.asm"
	.endsection
	.endmodule

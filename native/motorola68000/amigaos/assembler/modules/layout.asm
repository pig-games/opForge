; Owned numeric geometry and placement recipes for one prepared module.
; @opforge-owner: opforge.assembler.modules.layout
	.module opforge.assembler.modules.layout
	.cpu 68020
	.use opforge.platform.memory as memory
	.use opforge.packages.format as package
	.use opforge.assembler.modules.image as image
	.use opforge.assembler.layout.block_index as blocks
	.use opforge.assembler.source.records as source
	.use opforge.assembler.layout.sections as sections
	.use opforge.assembler.layout.assembly as assembly
	.use opforge.assembler.layout.encoding as encoding
	.use opforge.vm.encoding.runtime as execution
	.use opforge.assembler.layout.hunk_references as hunkrefs
	.use opforge.vm.exprvm.runtime as exprvm
	.pub
State	.struct
Records	.long ?
Bytes	.long ?
Index	.long ?
Count	.long ?  ; serialized fragment count
BlockCount	.long ?  ; whole lexical block count
Names	.long ?
Geometry	.res memory.Block.Used+4
Inputs	.res memory.Block.Used+4
Owners	.res memory.Block.Used+4
Labels	.res memory.Block.Used+4
Pending	.res memory.Block.Used+4
Relocs	.res memory.Block.Used+4
External	.long ?  ; transient exact-identity import resolver
ExternalContext	.long ?
	.endstruct
STATE_BYTES = State.ExternalContext+4
; Resolved shared-data pair. Exact anchors, never already-subtracted values.
Delta	.struct
TargetSymbol	.long ?
TargetSection	.long ?
TargetValue	.long ?
BaseSymbol	.long ?
BaseSection	.long ?
BaseValue	.long ?
FieldPc	.long ?
Span	.long ?
LittleEndian	.long ?  ; zero BE, one LE
	.endstruct
DATA_DELTA_BYTES = Delta.LittleEndian+4
Row	.struct
Section	.long ?
StartPc	.long ?
NextPc	.long ?
Offset	.long ?
Bytes	.long ?
StartRecord	.long ?
EndRecord	.long ?
	.endstruct
ROW_BYTES = Row.EndRecord+4
OK = 0
INVALID = 1
EXTERNAL_TARGET = $80000000
	.priv
PendingRow	.struct
Source	.long ?  ; whole owner, including unconditional zero
SourceRecord	.long ?
SourceSection	.long ?
SourcePc	.long ?
Symbol	.long ?
Section	.long ?
Value	.long ?
Span	.long ?
Program	.long ?
Arguments	.long ?
Position	.long ?
Requested	.long ?  ; original package request, encoding.REQUEST_AUTO or ID
BaseSymbol	.long ?  ; exact local base anchor for data/scalar cancellation
BaseValue	.long ?
	.endstruct
PENDING_BYTES = PendingRow.BaseValue+4
POSITION_REPLAY = 1
POSITION_CANCELLATION = 2; transient proof, never a serialized relocation
DATA_DELTA = 3
SCALAR_CANCELLATION = 4; proof-only row; never serialized
POSITION_FIELD = 5
InstructionFrame	.struct
Pending	.res PENDING_BYTES
Field	.res execution.POSITION_FIELD_BYTES
	.endstruct
INSTRUCTION_FRAME_BYTES = InstructionFrame.Field+execution.POSITION_FIELD_BYTES
MovementFrame	.struct
Forward	.word ?
	.endstruct
MOVEMENT_FRAME_BYTES = MovementFrame.Forward+2
	.section code, kind=code
	.pub
; A0=State,A1=external callback,A2=borrowed callback context. D0/CCR=OK.
; Other registers preserved. Configure after begin and retain through finish.
; External(A0=context,D1=exact source symbol,D2=section,D3=original value)
; returns D0/CCR=status,D1=EXTERNAL_TARGET|global identity,D2=unsigned addend;
; all other registers preserved. It must prove the source import identity and
; reject negative addends. Neither callback nor context enters the saved image.
configureExternal	.block
	move.l a1, State.External(a0)
	move.l a2, State.ExternalContext(a0)
	moveq #OK, d0
	rts
	.bend  ; configureExternal

; A0=zero State,A1=records,D0=bytes,A2=index,D1=name count.
; Records/index are borrowed until finish. Failed begin retains owned cleanup.
; Observers preserve registers except D0/CCR; release is idempotent.
begin	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l a1, State.Records(a6)
	move.l d0, State.Bytes(a6)
	move.l a2, State.Index(a6)
	move.l d1, State.Names(a6)
	cmpi.l #65536, d1
	bhi.w bad
	move.l blocks.Scratch.Count(a2), State.BlockCount(a6)
	cmpi.l #65535, blocks.Scratch.Count(a2)
	bhi.w bad
	move.l d1, d0
	add.l d0, d0
	lea State.Owners(a6), a0
	jsr memory.reserveExact
	bne.w bad
	move.l State.Names(a6), d0
	lsl.l #2, d0
	lea State.Labels(a6), a0
	jsr memory.reserveExact
	bne.w bad
	movea.l State.Labels+memory.Block.Pointer(a6), a0
	move.l State.Names(a6), d0
	beq.w records
clearLabels
	move.l #$ffffffff, (a0)+
	subq.l #1, d0
	bne.w clearLabels
records
	moveq #0, d7
record
	cmp.l State.Bytes(a6), d7
	bhs.w good
	move.l State.Bytes(a6), d0
	sub.l d7, d0
	cmpi.l #source.RECORD_HEADER_BYTES, d0
	blo.w bad
	movea.l State.Records(a6), a2
	adda.l d7, a2
	moveq #0, d6
	move.b source.RecordHeader.Length(a2), d6
	addq.l #1, d6
	cmpi.l #source.RECORD_HEADER_BYTES, d6
	blo.w bad
	cmp.l d0, d6
	bhi.w bad
	cmpi.l #source.DECLARATION_HEADER_BYTES, d6
	blo.w nextRecord
	cmpi.b #source.TOKEN_LABEL_DECLARATION, source.DeclarationRecord.Marker(a2)
	bne.w nextRecord
	cmpi.b #source.TOKEN_NAME_MAX, source.DeclarationRecord.Name+source.NameToken.Kind(a2)
	bhi.w nextRecord
	moveq #0, d5
	move.w source.DeclarationRecord.Name+source.NameToken.Id(a2), d5
	cmp.l State.Names(a6), d5
	bhs.w bad
	move.l d7, d0
	bsr.w owner
	movea.l State.Owners+memory.Block.Pointer(a6), a1
	move.l d5, d1
	add.l d1, d1
	move.w d0, 0(a1, d1.l)
	movea.l State.Labels+memory.Block.Pointer(a6), a1
	lsl.l #2, d5
	move.l d7, 0(a1, d5.l)
nextRecord
	add.l d6, d7
	bra.w record
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; begin

; A0=State,A1=assembly Frame,D1=PC,D2=output offset,D3=bytes.
; Preserves all registers except D0/CCR=status. Initialized bytes own payload.
emitted	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	bsr.w current
	bne.w bad
	cmpi.l #image.KIND_BSS, image.Input.Kind(a4)
	beq.w bad
	tst.l Row.Bytes(a3)
	bne.w existing
	move.l d2, Row.Offset(a3)
existing
	move.l Row.Offset(a3), d0
	add.l Row.Bytes(a3), d0
	bcs.w bad
	cmp.l d2, d0
	bne.w bad
	add.l d3, Row.Bytes(a3)
	bcs.w bad
	move.l Row.Bytes(a3), image.Input.Bytes(a4)
	move.l Row.Bytes(a3), image.Input.Extent(a4)
	add.l d3, Row.NextPc(a3)
	bcs.w bad
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; emitted

; Same observer ABI, D2 ignored. BSS advances allocation without payload.
reserved	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	bsr.w current
	bne.w bad
	cmpi.l #image.KIND_BSS, image.Input.Kind(a4)
	bne.w bad
	add.l d3, image.Input.Extent(a4)
	bcs.w bad
	add.l d3, Row.NextPc(a3)
	bcs.w bad
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; reserved

; D1=PC before padding,D2=alignment,D3=original padding. Called even at zero.
; The shared engine suppresses emitted/reserved callbacks for that padding.
; Starts a constraint fragment; labels on either side retain record identity.
aligned	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	tst.l d2
	beq.w bad
	move.l d2, d0
	subq.l #1, d0
	and.l d2, d0
	bne.w bad
	bsr.w current
	bne.w bad
	move.l assembly.Frame.RecordOffset(a5), Row.EndRecord(a3)
	add.l d3, d1
	bcs.w bad
	move.l d2, d7
	bsr.w appendFragment
	bne.w bad
	move.l d7, image.Input.Alignment(a4)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; aligned

; A0=State,A1=Frame,D1=PC,D2=section,D3=instruction length.
instruction	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #INSTRUCTION_FRAME_BYTES, sp
	movea.l a0, a6
	movea.l a1, a5
	move.l d1, PendingRow.SourcePc(sp)
	move.l d2, PendingRow.SourceSection(sp)
	move.l assembly.Frame.RecordOffset(a5), PendingRow.SourceRecord(sp)
	move.l d3, PendingRow.Span(sp)
	clr.l PendingRow.BaseSymbol(sp)
	clr.l PendingRow.BaseValue(sp)
	jsr encoding.outputPositionDependent
	tst.l d0
	bne.w positionRecipe
	; Non-position output, including instructions without fixups, must not
	; freeze a movable scalar. Whole-instruction recipes own their PC/target
	; through the separate requested-candidate and fragment proof below.
	jsr encoding.outputGeometryProof
	bne.w bad
	bra.w good
positionRecipe
	jsr encoding.outputPositionRecipe
	bne.w cancellation
	move.l d1, PendingRow.Program(sp)
	move.l d2, PendingRow.Arguments(sp)
	move.l d3, PendingRow.Value(sp)
	jsr encoding.outputPositionRequestedCandidate
	bne.w bad
	move.l d1, PendingRow.Requested(sp)
	move.l #POSITION_REPLAY, PendingRow.Position(sp)
	bra.w positionTarget
cancellation
	lea InstructionFrame.Field(sp), a0
	jsr encoding.outputRelativeField
	bne.w wholeCancellation
	move.l d1, d4
	move.l d2, d5
	move.l InstructionFrame.Field+execution.RelativeField.Value(sp), PendingRow.Value(sp)
	move.l InstructionFrame.Field+execution.RelativeField.Offset(sp), d0
	add.l d0, PendingRow.SourcePc(sp)
	bcs.w bad
	sub.l InstructionFrame.Field+execution.RelativeField.Adjustment(sp), d0
	bvs.w bad
	move.l d0, PendingRow.Arguments(sp)
	move.l InstructionFrame.Field+execution.RelativeField.Width(sp), d0
	cmpi.l #1, d0
	beq.w fieldWidthReady
	cmpi.l #2, d0
	beq.w fieldWidthReady
	cmpi.l #4, d0
	bne.w bad
fieldWidthReady
	move.l d0, PendingRow.Span(sp)
	move.l #image.PROGRAM_SIGNED_DELTA_BE, PendingRow.Program(sp)
	move.l InstructionFrame.Field+execution.RelativeField.LittleEndian(sp), d0
	beq.w fieldReady
	cmpi.l #1, d0
	bne.w bad
	move.l #image.PROGRAM_SIGNED_DELTA_LE, PendingRow.Program(sp)
fieldReady
	move.l #POSITION_FIELD, PendingRow.Position(sp)
	clr.l PendingRow.Requested(sp)
	bra.w positionTarget
wholeCancellation
	jsr encoding.outputPositionCancellation
	bne.w bad
	move.l d1, d4
	move.l d2, d5
	clr.l PendingRow.Program(sp)
	clr.l PendingRow.Arguments(sp)
	clr.l PendingRow.Value(sp)
	clr.l PendingRow.Requested(sp)
	move.l #POSITION_CANCELLATION, PendingRow.Position(sp)
positionTarget
	subq.l #1, d4
	cmpi.l #sections.HUNK_SLOT_COUNT, d4
	bhs.w bad
	move.l d4, PendingRow.Section(sp)
	cmpi.l #POSITION_FIELD, PendingRow.Position(sp)
	bne.w sectionReady
	cmp.l PendingRow.SourceSection(sp), d4
	bne.w bad
sectionReady
	cmp.l State.Names(a6), d5
	bhs.w bad
	move.l d5, PendingRow.Symbol(sp)
	move.l d5, d0
	lsl.l #2, d0
	movea.l State.Labels+memory.Block.Pointer(a6), a0
	cmpi.l #$ffffffff, 0(a0, d0.l)
	bne.w identityReady
	cmpi.l #POSITION_FIELD, PendingRow.Position(sp)
	beq.w bad
	cmpi.l #POSITION_CANCELLATION, PendingRow.Position(sp)
	beq.w bad  ; only a known local anchor can prove equal fragment movement
	tst.l State.External(a6)
	beq.w bad
identityReady
	move.l assembly.Frame.RecordOffset(a5), d0
	bsr.w owner
	move.l d0, PendingRow.Source(sp)
	movea.l sp, a1
	bsr.w appendPending
	bra.w done
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	adda.w #INSTRUCTION_FRAME_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; instruction

; A0=State,A1=Frame,D0=source section,D1=target section,
; D2=field section offset,D3=original symbol,D4=normalized addend.
absolute	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #PENDING_BYTES, sp
	movea.l a0, a6
	movea.l a1, a5
	move.l d0, d7
	move.l d0, PendingRow.SourceSection(sp)
	move.l assembly.Frame.RecordOffset(a5), PendingRow.SourceRecord(sp)
	; The current absolute image recipe writes BE32. Other data byte orders
	; require their own explicit recipe, rather than silently patching wrongly.
	movea.l assembly.Frame.Context(a5), a2
	movea.l package.Context.Package(a2), a0
	tst.w package.Header.LittleEndian(a0)
	bne.w bad
	cmp.l State.Names(a6), d3
	bhs.w bad
	move.l d3, PendingRow.Symbol(sp)
	move.l d3, d0
	lsl.l #2, d0
	movea.l State.Labels+memory.Block.Pointer(a6), a0
	cmpi.l #$ffffffff, 0(a0, d0.l)
	bne.w identityReady
	tst.l State.External(a6)
	beq.w bad
identityReady
	move.l d1, PendingRow.Section(sp)
	addq.l #1, d1
	movea.l assembly.Frame.Context(a5), a2
	jsr hunkrefs.sectionBase
	bne.w bad
	add.l d4, d1
	bcs.w bad
	move.l d1, PendingRow.Value(sp)
	move.l d7, d1
	addq.l #1, d1
	jsr hunkrefs.sectionBase
	bne.w bad
	add.l d2, d1
	bcs.w bad
	move.l d1, PendingRow.SourcePc(sp)
	move.l #4, PendingRow.Span(sp)
	move.l #image.PROGRAM_ABSOLUTE, PendingRow.Program(sp)
	clr.l PendingRow.Arguments(sp)
	clr.l PendingRow.Position(sp)
	clr.l PendingRow.Requested(sp)
	clr.l PendingRow.BaseSymbol(sp)
	clr.l PendingRow.BaseValue(sp)
	move.l assembly.Frame.RecordOffset(a5), d0
	bsr.w owner
	move.l d0, PendingRow.Source(sp)
	movea.l sp, a1
	bsr.w appendPending
	bra.w done
bad
	moveq #INVALID, d0
done
	adda.w #PENDING_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; absolute

; A0=State,A1=assembly Frame,A2=resolved Delta. D0/CCR=OK/INVALID;
; other registers preserved. Final pass two only; descriptor is borrowed for
; this call. Both local anchors must share the field's private section. Finish
; proves field/base share the exact fragment; linking rechecks global section
; identity before emitting a fixed-width, loader-invariant difference.
dataDelta	.block
	moveq #DATA_DELTA, d0
	bra.w appendDifference
	.bend  ; dataDelta

; Same Delta descriptor and preservation ABI, but no output field is patched.
; A resolved scalar pair may be frozen only when both anchors share one exact
; fragment whose owner is mandatory or already retained with the instruction.
scalarDifference	.block
	moveq #SCALAR_CANCELLATION, d0
	bra.w appendDifference
	.bend  ; scalarDifference
	.priv
appendDifference	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #PENDING_BYTES, sp
	move.l d0, PendingRow.Position(sp)
	movea.l a0, a6
	movea.l a1, a5
	movea.l a2, a4
	movea.l assembly.Frame.Context(a5), a3
	cmpi.w #2, package.Context.Pass(a3)
	bne.w bad
	moveq #0, d0
	move.w package.Context.CurrentSection(a3), d0
	subq.l #1, d0
	cmpi.l #sections.HUNK_SLOT_COUNT, d0
	bhs.w bad
	move.l d0, PendingRow.SourceSection(sp)
	move.l Delta.TargetSection(a4), d0
	cmpi.l #sections.HUNK_SLOT_COUNT, d0
	bhs.w bad
	cmp.l Delta.BaseSection(a4), d0
	bne.w bad
	move.l d0, PendingRow.Section(sp)
	cmpi.l #SCALAR_CANCELLATION, PendingRow.Position(sp)
	beq.w scalarReady
	cmp.l PendingRow.SourceSection(sp), d0
	bne.w bad  ; reject before publication, not later during final linking
	move.l Delta.Span(a4), d0
	cmpi.l #2, d0
	beq.w spanReady
	cmpi.l #4, d0
	bne.w bad
spanReady
	move.l d0, PendingRow.Span(sp)
	move.l #image.PROGRAM_DATA_DELTA_BE, PendingRow.Program(sp)
	move.l Delta.LittleEndian(a4), d0
	beq.w endianReady
	cmpi.l #1, d0
	bne.w bad
	move.l #image.PROGRAM_DATA_DELTA_LE, PendingRow.Program(sp)
	bra.w endianReady
scalarReady
	clr.l PendingRow.Span(sp)
	clr.l PendingRow.Program(sp)
endianReady
	move.l Delta.TargetSymbol(a4), d1
	move.l d1, PendingRow.Symbol(sp)
	bsr.w localAnchor
	bne.w bad
	move.l Delta.BaseSymbol(a4), d1
	move.l d1, PendingRow.BaseSymbol(sp)
	bsr.w localAnchor
	bne.w bad
	move.l Delta.TargetValue(a4), PendingRow.Value(sp)
	move.l Delta.BaseValue(a4), PendingRow.BaseValue(sp)
	move.l Delta.FieldPc(a4), PendingRow.SourcePc(sp)
	move.l assembly.Frame.RecordOffset(a5), d0
	move.l d0, PendingRow.SourceRecord(sp)
	bsr.w owner
	move.l d0, PendingRow.Source(sp)
	clr.l PendingRow.Arguments(sp)
	clr.l PendingRow.Requested(sp)
	movea.l sp, a1
	cmpi.l #SCALAR_CANCELLATION, PendingRow.Position(sp)
	bne.w append
	bsr.w reuseScalarProof
	beq.w done
append
	bsr.w appendPending
	bra.w done
bad
	moveq #INVALID, d0
done
	adda.w #PENDING_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; appendDifference
	.pub

; A0=State,A1=final assembly Frame. Materialize pointers only after geometry closes.
finish	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	movea.l State.Geometry+memory.Block.Pointer(a6), a3
	movea.l State.Inputs+memory.Block.Pointer(a6), a4
	move.l State.Count(a6), d7
	beq.w pendingStart
rows
	move.l Row.Offset(a3), d0
	add.l Row.Bytes(a3), d0
	bcs.w bad
	cmp.l assembly.Frame.Used(a5), d0
	bhi.w bad
	clr.l image.Input.Data(a4)
	tst.l Row.Bytes(a3)
	beq.w nextRow
	move.l assembly.Frame.Output(a5), d0
	add.l Row.Offset(a3), d0
	bcs.w bad
	move.l d0, image.Input.Data(a4)
nextRow
	adda.w #ROW_BYTES, a3
	adda.w #image.INPUT_BYTES, a4
	subq.l #1, d7
	bne.w rows
pendingStart
	moveq #0, d7
pending
	cmp.l State.Pending+memory.Block.Used(a6), d7
	bhs.w good
	movea.l State.Pending+memory.Block.Pointer(a6), a2
	adda.l d7, a2
	cmpi.l #SCALAR_CANCELLATION, PendingRow.Position(a2)
	beq.w scalarProof
	move.l PendingRow.Source(a2), d0
	move.l PendingRow.SourceSection(a2), d1
	move.l PendingRow.SourcePc(a2), d2
	move.l PendingRow.SourceRecord(a2), d3
	bsr.w locate
	bne.w bad
	move.l d4, d6
	move.l d5, -(sp)
	add.l PendingRow.Span(a2), d5
	bcs.w sourceBad
	cmp.l image.Input.Bytes(a4), d5
	bhi.w sourceBad
	cmpi.l #DATA_DELTA, PendingRow.Position(a2)
	bne.w baseReady
	movea.l a6, a0
	move.l PendingRow.BaseSymbol(a2), d1
	move.l PendingRow.SourceSection(a2), d2
	move.l PendingRow.BaseValue(a2), d3
	bsr.w symbol
	bne.w sourceBad
	cmp.l d1, d6
	bne.w sourceBad  ; equal PCs on different alignment sides do not cancel
	move.l PendingRow.SourcePc(a2), d0
	sub.l PendingRow.BaseValue(a2), d0
	bvs.w sourceBad
	move.l d0, PendingRow.Arguments(a2)
baseReady
	move.l PendingRow.Symbol(a2), d1
	move.l d1, d0
	lsl.l #2, d0
	movea.l State.Labels+memory.Block.Pointer(a6), a0
	cmpi.l #$ffffffff, 0(a0, d0.l)
	beq.w externalTarget
	move.l d1, d0
	lsl.l #3, d0
	movea.l assembly.Frame.Context(a5), a0
	movea.l package.Context.Values(a0), a0
	tst.l exprvm.Value.High(a0, d0.l)
	bne.w sourceBad
	move.l exprvm.Value.Low(a0, d0.l), d3
	move.l PendingRow.Section(a2), d2
	movea.l a6, a0
	bsr.w symbol
	bne.w sourceBad
	cmpi.l #POSITION_CANCELLATION, PendingRow.Position(a2)
	bne.w targetGeometry
	; The successful VM already checked the field's range. Preserve its bytes
	; only when the exact label anchor and instruction move by the same amount.
	; The label's fragment, not its label-plus-addend numeric address, owns this
	; test. No relocation or new image program is emitted for cancellation.
	cmp.l d1, d6
	bne.w sourceBad
	addq.l #4, sp
	bra.w advance
targetGeometry
	move.l d1, d0
	bsr.w geometry
	; Locate the exact anchor label, then retain its full normalized addend.
	; A positive addend may legally address beyond this fragment's extent.
	move.l PendingRow.Value(a2), d4
	sub.l Row.StartPc(a3), d4
	bcs.w sourceBad
	move.l (sp)+, d3
	move.l d1, d5
	bra.w targetReady
externalTarget
	cmpi.l #POSITION_CANCELLATION, PendingRow.Position(a2)
	beq.w sourceBad
	move.l State.External(a6), d0
	beq.w sourceBad
	movea.l d0, a1
	movea.l State.ExternalContext(a6), a0
	move.l PendingRow.Section(a2), d2
	move.l PendingRow.Value(a2), d3
	jsr (a1)
	tst.l d0
	bne.w sourceBad
	tst.l d1
	bpl.w sourceBad
	move.l d1, d0
	andi.l #$7fff0000, d0
	bne.w sourceBad
	move.l d1, d5
	move.l d2, d4
	move.l (sp)+, d3
targetReady
	; Alignment between fragments can change even inside one whole block.
	; Position recipes are omitted only when both endpoints share a fragment.
	cmpi.l #POSITION_REPLAY, PendingRow.Position(a2)
	bne.w retain
	cmp.l d5, d6
	beq.w advance
retain
	cmpi.l #POSITION_FIELD, PendingRow.Position(a2)
	beq.w fieldInterval
	; Replaying a chosen automatic candidate across moving fragments would
	; freeze a width that the shared assembler may select differently. Explicit
	; requests retain fixed package geometry; same-fragment deltas stay invariant.
	cmpi.l #POSITION_REPLAY, PendingRow.Position(a2)
	bne.w fixedRecipe
	cmpi.l #encoding.REQUEST_AUTO, PendingRow.Requested(a2)
	bne.w fixedRecipe
	; Selected endpoints keep their whole owners live through this retained
	; recipe. Other owners may disappear; certify the complete displacement
	; interval through the package before freezing the selected AUTO candidate.
	movem.l d1-d4, -(sp)
	move.l d6, d1
	move.l d5, d2
	bsr.w movementBounds
	bne.w intervalDone
	cmp.l d2, d1
	beq.w intervalDone  ; rigid span: every candidate sees the original delta
	move.l d1, d3
	move.l d2, d4
	move.l PendingRow.Program(a2), d1
	move.l PendingRow.Arguments(a2), d2
	movea.l assembly.Frame.Context(a5), a0
	movea.l package.Context.Package(a0), a0
	jsr encoding.positionInterval
intervalDone
	movem.l (sp)+, d1-d4
	tst.l d0
	bne.w bad
	bra.w fixedRecipe
fieldInterval
	movem.l d1-d4, -(sp)
	move.l d6, d1
	move.l d5, d2
	bsr.w movementBounds
	bne.w fieldIntervalDone
	add.l PendingRow.Arguments(a2), d1
	bvs.w fieldIntervalBad
	add.l PendingRow.Arguments(a2), d2
	bvs.w fieldIntervalBad
	move.l PendingRow.Span(a2), d0
	bsr.w signedFieldBounds
	bra.w fieldIntervalDone
fieldIntervalBad
	moveq #INVALID, d0
fieldIntervalDone
	movem.l (sp)+, d1-d4
	tst.l d0
	bne.w bad
fixedRecipe
	move.l State.Relocs+memory.Block.Used(a6), d2
	move.l d2, d0
	addi.l #image.RELOC_BYTES, d0
	bcs.w bad
	lea State.Relocs(a6), a0
	jsr memory.reserve
	bne.w bad
	movea.l State.Relocs+memory.Block.Pointer(a6), a0
	adda.l d2, a0
	move.l d6, image.Reloc.Block(a0)
	move.l d3, image.Reloc.Offset(a0)
	move.l d5, image.Reloc.Target(a0)
	move.l d4, image.Reloc.Addend(a0)
	move.l PendingRow.Span(a2), image.Reloc.Span(a0)
	move.l PendingRow.Program(a2), image.Reloc.Program(a0)
	move.l PendingRow.Arguments(a2), image.Reloc.Arguments(a0)
	addi.l #image.RELOC_BYTES, State.Relocs+memory.Block.Used(a6)
	bra.w advance
scalarProof
	movea.l a6, a0
	move.l PendingRow.Symbol(a2), d1
	move.l PendingRow.Section(a2), d2
	move.l PendingRow.Value(a2), d3
	bsr.w symbol
	bne.w bad
	move.l d1, d6
	move.l PendingRow.BaseSymbol(a2), d1
	move.l PendingRow.Section(a2), d2
	move.l PendingRow.BaseValue(a2), d3
	bsr.w symbol
	bne.w bad
	cmp.l d1, d6
	bne.w bad
	move.l d1, d0
	bsr.w geometry
	move.l image.Input.Owner(a4), d0
	beq.w advance
	cmp.l PendingRow.Source(a2), d0
	bne.w bad  ; no serialized edge exists to retain another prunable owner
advance
	addi.l #PENDING_BYTES, d7
	bra.w pending
sourceBad
	addq.l #4, sp
bad
	moveq #INVALID, d0
	bra.w done
good
	moveq #OK, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; finish

; D0=field width,D1/D2=signed inclusive bounds after PC adjustment.
; D0/CCR=status; other registers preserved. The fixed-field package contract
; supplies signed range semantics; no opcode or candidate width is inferred.
signedFieldBounds	.block
	cmpi.l #4, d0
	beq.w good
	cmpi.l #2, d0
	beq.w wordRange
	cmpi.l #1, d0
	bne.w bad
	cmpi.l #-128, d1
	blt.w bad
	cmpi.l #127, d2
	bgt.w bad
	bra.w good
wordRange
	cmpi.l #-32768, d1
	blt.w bad
	cmpi.l #32767, d2
	bgt.w bad
good
	moveq #OK, d0
	rts
bad
	moveq #INVALID, d0
	rts
	.bend  ; signedFieldBounds

; A6=State,A2=pending recipe,D1/D2=local source/target fragment IDs.
; D0/CCR=OK,D1/D2=signed target-minus-PC minimum/maximum; others preserved.
; The caller MUST retain the target recipe/dependency: source selection then
; guarantees both endpoint owners survive. Owner0 is mandatory. Conservatively
; permit every other owner's extent to disappear, bounding all pruning subsets.
; Section excursions, gaps or later alignment reject; the first row's alignment
; precedes both anchors. Exact label identity, rather than the addended numeric
; address, determines which endpoint moves when an intermediate owner disappears.
movementBounds	.block
	movem.l d3-d7/a0-a4, -(sp)
	suba.w #MOVEMENT_FRAME_BYTES, sp
	clr.w MovementFrame.Forward(sp)
	tst.l d1
	beq.w bad
	cmp.l State.Count(a6), d1
	bhi.w bad
	tst.l d2
	beq.w bad
	cmp.l State.Count(a6), d2
	bhi.w bad
	move.l d1, d0
	bsr.w geometry
	move.l image.Input.Owner(a4), d3
	move.l Row.Section(a3), d4
	move.l d2, d0
	bsr.w geometry
	move.l image.Input.Owner(a4), d5
	move.l d1, d6
	move.l d2, d7
	cmp.l d7, d6
	bhs.w backward
	move.w #1, MovementFrame.Forward(sp)
	bra.w first
backward
	exg d6, d7
first
	moveq #0, d2  ; total extent whose whole owner may be omitted
	move.l d6, d0
	bsr.w geometry
	bra.w checkRow
next
	move.l d6, d0
	bsr.w geometry
	cmpi.l #1, image.Input.Alignment(a4)
	bne.w bad
	cmp.l Row.StartPc(a3), d1
	bne.w bad
checkRow
	cmp.l Row.Section(a3), d4
	bne.w bad
	move.l image.Input.Owner(a4), d0
	beq.w ownerReady
	cmp.l d3, d0
	beq.w ownerReady
	cmp.l d5, d0
	beq.w ownerReady
	add.l image.Input.Extent(a4), d2
	bcs.w bad
	tst.l d2
	bmi.w bad  ; removable extent must remain a nonnegative signed bound
ownerReady
	move.l Row.NextPc(a3), d1
	move.l d1, d0
	sub.l Row.StartPc(a3), d0
	bcs.w bad
	cmp.l image.Input.Extent(a4), d0
	bne.w bad
	cmp.l d7, d6
	beq.w bounds
	addq.l #1, d6
	bra.w next
bounds
	move.l PendingRow.Value(a2), d1
	sub.l PendingRow.SourcePc(a2), d1
	bvs.w bad
	tst.w MovementFrame.Forward(sp)
	beq.w backwardBounds
	move.l d1, d0
	sub.l d2, d1
	bvs.w bad
	move.l d0, d2
	bra.w good
backwardBounds
	add.l d1, d2
	bvs.w bad
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	adda.w #MOVEMENT_FRAME_BYTES, sp
	movem.l (sp)+, d3-d7/a0-a4
	tst.l d0
	rts
	.bend  ; movementBounds

; A0=State,D1=symbol ID,D2=section (zero based),D3=original final value.
; D0/CCR=status; success D1=fragment ID,D2=offset. Others preserved.
; Label record identity distinguishes equal original PCs around an alignment.
symbol	.block
	movem.l d3-d6/a0-a1/a3-a4/a6, -(sp)
	movea.l a0, a6
	cmp.l State.Names(a6), d1
	bhs.w bad
	move.l d1, d0
	add.l d0, d0
	movea.l State.Owners+memory.Block.Pointer(a6), a0
	moveq #0, d6
	move.w 0(a0, d0.l), d6
	movea.l State.Labels+memory.Block.Pointer(a6), a0
	lsl.l #2, d1
	move.l 0(a0, d1.l), d0
	cmpi.l #$ffffffff, d0
	beq.w bad
	move.l d2, d1
	move.l d3, d2
	move.l d0, d3
	move.l d6, d0
	bsr.w locate
	bne.w bad
	move.l d4, d1
	move.l d5, d2
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d3-d6/a0-a1/a3-a4/a6
	tst.l d0
	rts
	.bend  ; symbol

release	.block
	movem.l d1-d2/a0-a2, -(sp)
	movea.l a0, a2
	lea State.Geometry(a2), a0
	jsr memory.release
	lea State.Inputs(a2), a0
	jsr memory.release
	lea State.Owners(a2), a0
	jsr memory.release
	lea State.Labels(a2), a0
	jsr memory.release
	lea State.Pending(a2), a0
	jsr memory.release
	lea State.Relocs(a2), a0
	jsr memory.release
	movea.l a2, a0
	move.l #STATE_BYTES/4, d1
clear
	clr.l (a0)+
	subq.l #1, d1
	bne.w clear
	moveq #OK, d0
	movem.l (sp)+, d1-d2/a0-a2
	rts
	.bend  ; release
	.priv
; A6=State,D0=record offset. D0=topmost whole block, or zero at root.
; Ordinal source order guarantees an outer span precedes all its children.
owner	.block
	movem.l d1/a0, -(sp)
	movea.l State.Index(a6), a0
	adda.w #blocks.HEADER_BYTES, a0
	moveq #1, d1
next
	cmp.l State.BlockCount(a6), d1
	bhi.w absent
	cmp.l blocks.Span.Start(a0), d0
	blo.w advance
	cmp.l blocks.Span.End(a0), d0
	blo.w found
advance
	adda.w #blocks.SPAN_BYTES, a0
	addq.l #1, d1
	bra.w next
found
	move.l d1, d0
	bra.w done
absent
	moveq #0, d0
done
	movem.l (sp)+, d1/a0
	rts
	.bend  ; owner

; A6=State,D0=validated one-based fragment ordinal. A3=row,A4=input.
geometry	.block
	move.l d0, -(sp)
	subq.l #1, d0
	move.l d0, -(sp)
	mulu.l #ROW_BYTES, d0
	movea.l State.Geometry+memory.Block.Pointer(a6), a3
	adda.l d0, a3
	move.l (sp)+, d0
	mulu.l #image.INPUT_BYTES, d0
	movea.l State.Inputs+memory.Block.Pointer(a6), a4
	adda.l d0, a4
	move.l (sp)+, d0
	rts
	.bend  ; geometry

; A5=Frame,A6=State,D1=PC. Return current matching fragment A3/A4.
; D4=whole owner,D5=section,D6=kind. D0=status. D1-D3 are preserved.
current	.block
	movea.l assembly.Frame.Sections(a5), a0
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a0)
	bne.w bad
	tst.w sections.State.HunkSelected(a0)
	beq.w bad
	moveq #0, d5
	move.w sections.State.HunkCurrent(a0), d5
	moveq #0, d6
	move.w sections.State.ActiveKind(a0), d6
	move.l assembly.Frame.RecordOffset(a5), d0
	bsr.w owner
	move.l d0, d4
	move.l State.Count(a6), d0
	beq.w appendFragment
	bsr.w geometry
	cmp.l image.Input.Owner(a4), d4
	bne.w close
	cmp.l Row.Section(a3), d5
	bne.w close
	cmp.l image.Input.Kind(a4), d6
	bne.w bad
	cmp.l Row.NextPc(a3), d1
	bne.w close
	moveq #OK, d0
	rts
close
	move.l assembly.Frame.RecordOffset(a5), Row.EndRecord(a3)
	bra.w appendFragment
bad
	moveq #INVALID, d0
	rts
	.bend  ; current

; A5/A6=Frame/State,D1=PC,D4=owner,D5=section,D6=kind. A3/A4=new row.
; D1-D7 preserved. Allocations can move; no pointer survives reserve.
appendFragment	.block
	movem.l d1-d7, -(sp)
	move.l State.Count(a6), d0
	cmpi.l #memory.LIMIT/image.INPUT_BYTES, d0
	bhs.w bad
	addq.l #1, d0
	mulu.l #ROW_BYTES, d0
	lea State.Geometry(a6), a0
	jsr memory.reserve
	bne.w bad
	move.l State.Count(a6), d0
	addq.l #1, d0
	mulu.l #image.INPUT_BYTES, d0
	lea State.Inputs(a6), a0
	jsr memory.reserve
	bne.w bad
	addq.l #1, State.Count(a6)
	move.l State.Count(a6), d0
	bsr.w geometry
	move.l d5, Row.Section(a3)
	move.l d1, Row.StartPc(a3)
	move.l d1, Row.NextPc(a3)
	clr.l Row.Offset(a3)
	clr.l Row.Bytes(a3)
	move.l assembly.Frame.RecordOffset(a5), Row.StartRecord(a3)
	move.l State.Bytes(a6), Row.EndRecord(a3)
	move.l d0, image.Input.Id(a4)
	clr.l image.Input.Data(a4)
	clr.l image.Input.Bytes(a4)
	move.l d4, image.Input.Owner(a4)
	move.l d5, image.Input.Section(a4)
	move.l d6, image.Input.Kind(a4)
	move.l #1, image.Input.Alignment(a4)
	clr.l image.Input.Extent(a4)
	; A declared section alignment constrains its first fragment only. Later
	; fragments must not acquire fresh section padding when whole blocks move.
	move.l State.Count(a6), d7
	subq.l #1, d7
	movea.l State.Inputs+memory.Block.Pointer(a6), a0
priorSection
	tst.l d7
	beq.w firstSection
	cmp.l image.Input.Section(a0), d5
	beq.w alignmentReady
	adda.w #image.INPUT_BYTES, a0
	subq.l #1, d7
	bra.w priorSection
firstSection
	movea.l assembly.Frame.Sections(a5), a0
	move.l d5, d0
	mulu.l #sections.HUNK_SLOT_BYTES, d0
	adda.l d0, a0
	move.l sections.HUNK_SLOTS+sections.HunkSlot.Align(a0), image.Input.Alignment(a4)
alignmentReady
	move.l State.Count(a6), d0
	mulu.l #ROW_BYTES, d0
	move.l d0, State.Geometry+memory.Block.Used(a6)
	move.l State.Count(a6), d0
	mulu.l #image.INPUT_BYTES, d0
	move.l d0, State.Inputs+memory.Block.Used(a6)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7
	tst.l d0
	rts
	.bend  ; appendFragment

; A6=State,D0=whole owner,D1=section,D2=original PC,D3=record offset.
; Success D4=fragment ID,D5=local offset,A3/A4=row/input. D0=status.
; D1-D3/A0-A2 preserved. Search keeps the last matching boundary at or before
; the exact record, rather than conflating equal PCs on opposite align sides.
locate	.block
	movem.l d6-d7/a0-a1, -(sp)
	move.l d0, d6
	moveq #0, d4
	moveq #1, d7
next
	cmp.l State.Count(a6), d7
	bhi.w complete
	move.l d7, d0
	bsr.w geometry
	cmp.l image.Input.Owner(a4), d6
	bne.w advance
	cmp.l Row.Section(a3), d1
	bne.w advance
	cmp.l Row.StartPc(a3), d2
	blo.w advance
	cmp.l Row.NextPc(a3), d2
	bhi.w advance
	tst.l d4
	beq.w choose
	cmp.l Row.StartRecord(a3), d3
	blo.w advance
choose
	move.l d7, d4
advance
	addq.l #1, d7
	bra.w next
complete
	tst.l d4
	beq.w bad
	move.l d4, d0
	bsr.w geometry
	move.l d2, d5
	sub.l Row.StartPc(a3), d5
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d6-d7/a0-a1
	tst.l d0
	rts
	.bend  ; locate

; A6=State,D1=source identity. D0/CCR=status; other registers preserved.
localAnchor	.block
	movem.l d1/a0, -(sp)
	cmp.l State.Names(a6), d1
	bhs.w bad
	lsl.l #2, d1
	movea.l State.Labels+memory.Block.Pointer(a6), a0
	cmpi.l #$ffffffff, 0(a0, d1.l)
	beq.w bad
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; localAnchor

; A6=State,A1=new scalar proof. D0/CCR=OK reused,INVALID absent;
; other registers preserved. Repeated candidate/fixup evaluations share one
; source-record/canonical-pair proof. Refresh values for the final layout pass.
reuseScalarProof	.block
	movem.l d1-d2/a0, -(sp)
	moveq #0, d2
next
	cmp.l State.Pending+memory.Block.Used(a6), d2
	bhs.w absent
	movea.l State.Pending+memory.Block.Pointer(a6), a0
	adda.l d2, a0
	cmpi.l #SCALAR_CANCELLATION, PendingRow.Position(a0)
	bne.w advance
	move.l PendingRow.SourceRecord(a1), d1
	cmp.l PendingRow.SourceRecord(a0), d1
	bne.w advance
	move.l PendingRow.Source(a1), d1
	cmp.l PendingRow.Source(a0), d1
	bne.w advance
	move.l PendingRow.Symbol(a1), d1
	cmp.l PendingRow.Symbol(a0), d1
	bne.w advance
	move.l PendingRow.BaseSymbol(a1), d1
	cmp.l PendingRow.BaseSymbol(a0), d1
	bne.w advance
	move.l PendingRow.Section(a1), d1
	cmp.l PendingRow.Section(a0), d1
	bne.w advance
	move.l PendingRow.Value(a1), PendingRow.Value(a0)
	move.l PendingRow.BaseValue(a1), PendingRow.BaseValue(a0)
	moveq #OK, d0
	bra.w done
advance
	addi.l #PENDING_BYTES, d2
	bra.w next
absent
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d2/a0
	tst.l d0
	rts
	.bend  ; reuseScalarProof

; A6=State,A1=pending row. Appends an owned copy; preserves other registers.
appendPending	.block
	movem.l d1-d3/a0-a2, -(sp)
	move.l State.Pending+memory.Block.Used(a6), d2
	move.l d2, d0
	addi.l #PENDING_BYTES, d0
	bcs.w bad
	lea State.Pending(a6), a0
	jsr memory.reserve
	bne.w bad
	movea.l State.Pending+memory.Block.Pointer(a6), a0
	adda.l d2, a0
	move.l #PENDING_BYTES/4, d1
copy
	move.l (a1)+, (a0)+
	subq.l #1, d1
	bne.w copy
	addi.l #PENDING_BYTES, State.Pending+memory.Block.Used(a6)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; appendPending
	.endsection
	.endmodule

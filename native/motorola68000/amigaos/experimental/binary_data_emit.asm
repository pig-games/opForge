; @opforge-owner: experimental.amigaos.binary_assembly
; Shared built-in data execution. Included inside binary_assembly so output and
; relocation accounting retain a single owner; grammar belongs to packed_data.
DataCall	.struct
Request	.res dataabi.PRVM_REQUEST_FRAME_SIZE
Result	.res 32
Record	.res 256
End	.long ?
Cursor	.long ?
	.endstruct
DATA_CALL_BYTES = DataCall.Cursor+4

; A0=.emit operands,A1=bounded end,A2=Context,A3=package.
; D0/CCR=status; all other registers preserved. Uses bounded stack scratch.
emitData	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #DATA_CALL_BYTES, sp
	movea.l sp, a6
	lea -5(a0), a4
	move.l a1, d6
	sub.l a4, d6
	bcs.w bad
	addq.l #4, d6
	cmpi.l #256, d6
	bhi.w bad
	lea DataCall.Record(a6), a5
	move.l d6, d0
	subq.l #1, d0
	move.b d0, (a5)+
	clr.b (a5)+
	clr.w (a5)+
	move.l d6, d0
	subq.l #4, d0
copyRecord
	move.b (a4)+, (a5)+
	subq.l #1, d0
	bne.w copyRecord
	lea DataCall.Request(a6), a0
	moveq #dataabi.PRVM_REQUEST_FRAME_SIZE/4-1, d0
clearRequest
	clr.l (a0)+
	dbra d0, clearRequest
	lea DataCall.Request(a6), a0
	move.l #dataabi.PRVM_MAGIC_OPRP, dataabi.PRVM_FRAME_MAGIC(a0)
	move.w #dataabi.PRVM_ABI_VERSION_V1, dataabi.PRVM_FRAME_ABI_VERSION(a0)
	move.w #dataabi.PRVM_REQUEST_FRAME_SIZE, dataabi.PRVM_FRAME_FRAME_SIZE(a0)
	move.w #dataabi.PRVM_ENTRY_KIND_PACKED_DATA, dataabi.PRVM_FRAME_ENTRY_KIND(a0)
	lea DataCall.Record(a6), a4
	move.l a4, dataabi.PRVM_FRAME_SOURCE_PTR(a0)
	move.l d6, dataabi.PRVM_FRAME_SOURCE_LEN(a0)
	move.l pkg.Header.DataPlan(a3), d0
	lea 0(a3, d0.l), a5
	move.l a5, dataabi.PRVM_FRAME_PROGRAM_PTR(a0)
	move.l pkg.Header.DataPlanBytes(a3), dataabi.PRVM_FRAME_PROGRAM_LEN(a0)
	lea DataCall.Result(a6), a5
	move.l a5, dataabi.PRVM_FRAME_RESULT_PTR(a0)
	move.l #32, dataabi.PRVM_FRAME_RESULT_CAPACITY(a0)
	move.l #dataabi.PRVM_PARSER_CONTRACT_VERSION_V2, dataabi.PRVM_FRAME_PARSER_CONTRACT_VERSION(a0)
	move.l #1024, dataabi.PRVM_FRAME_STEP_BUDGET(a0)
	moveq #dataabi.PRVM_REQUEST_FRAME_SIZE, d0
	movem.l a2-a3, -(sp)
	jsr datavm.run
	movem.l (sp)+, a2-a3
	bne.w bad
	cmpi.l #1, d1
	bne.w bad
	cmpi.l #32, d3
	bne.w bad
	cmpi.w #dataabi.PRVM_RESULT_PACKED_DATA, (a5)
	bne.w bad
	tst.l dataabi.PRVM_DATA_PREFIX_BYTES(a5)
	bne.w bad
	move.l dataabi.PRVM_DATA_UNIT_OFFSET(a5), d0
	cmpi.l #9, d0
	bne.w bad
	move.l dataabi.PRVM_DATA_UNIT_BYTES(a5), d1
	beq.w bad
	add.l d0, d1
	bcs.w bad
	cmp.l d6, d1
	bhs.w bad
	cmpi.b #4, 0(a4, d1.l)
	bne.w bad
	addq.l #1, d1
	cmp.l dataabi.PRVM_DATA_VALUES_OFFSET(a5), d1
	bne.w bad
	move.l dataabi.PRVM_DATA_VALUES_BYTES(a5), d2
	beq.w bad
	add.l d1, d2
	bcs.w bad
	cmp.l d6, d2
	bne.w bad
	lea 0(a4, d1.l), a0
	move.l a0, DataCall.Cursor(a6)
	lea 0(a4, d2.l), a1
	move.l a1, DataCall.End(a6)
	move.l dataabi.PRVM_DATA_FIXED_WIDTH(a5), d6
	bne.w widthReady
	lea 0(a4, d0.l), a0
	lea -1(a4, d1.l), a1
	jsr expr.evaluateWithSymbols
	bne.w bad
	tst.l d2
	bne.w bad
	tst.l pkg.Context.High(a2)
	bne.w bad
	cmpa.l a1, a0
	bne.w bad
	tst.l d1
	beq.w bad
	lea SectionState, a4
	cmpi.w #5, sections.State.Mode(a4)
	bne.w unitReady
	tst.l d4
	bne.w bad
unitReady
	move.l d1, d6
widthReady
value
	movea.l DataCall.Cursor(a6), a0
	movea.l DataCall.End(a6), a1
	movea.l a0, a5
	jsr expr.evaluate
	bne.w bad
	tst.l d2
	beq.w resolved
	cmpi.w #1, pkg.Context.Pass(a2)
	bne.w bad
	moveq #0, d1
	bra.w rangeReady
resolved
	; Rust scalar data semantics keep the low u32, including signed values.
	cmpi.l #4, d6
	bhs.w reloc
	move.l d6, d0
	lsl.l #3, d0
	move.l d1, d3
	lsr.l d0, d3
	tst.l d3
	bne.w bad
reloc
	; Relocation classification uses long width comparison, avoiding truncation
	; of an arbitrary positive u32 unit into a legacy word-sized unit.
	cmpi.l #4, d6
	beq.w mark
	lea SectionState, a4
	cmpi.w #5, sections.State.Mode(a4)
	bne.w rangeReady
	movem.l d1/a0, -(sp)
	movea.l a5, a0
	jsr hunkrefs.affineTarget
	movem.l (sp)+, d1/a0
	tst.l d0
	bne.w bad
	bra.w rangeReady
mark
	bsr.w markDataReloc
	bne.w bad
rangeReady
	move.l a0, DataCall.Cursor(a6)
	move.l d6, d5
	cmpi.l #4, d5
	bls.w scalarWidth
	moveq #4, d5
scalarWidth
	lea DataBytes, a4
	move.l d5, d4
	tst.w pkg.Header.LittleEndian(a3)
	beq.w big
little
	move.b d1, (a4)+
	lsr.l #8, d1
	subq.l #1, d4
	bne.w little
	bra.w scalarReady
big
	subq.l #1, d4
	lsl.l #3, d4
bigLoop
	move.l d1, d3
	lsr.l d4, d3
	move.b d3, (a4)+
	subq.l #8, d4
	subq.l #1, d5
	bne.w bigLoop
scalarReady
	move.l d6, d7
	cmpi.l #4, d7
	bls.w writeScalar
	subq.l #4, d7
	tst.w pkg.Header.LittleEndian(a3)
	bne.w writeScalar
	suba.l a0, a0
	move.l d7, d0
	bsr.w emit
	bne.w bad
writeScalar
	move.l d6, d0
	cmpi.l #4, d0
	bls.w scalarCount
	moveq #4, d0
scalarCount
	lea DataBytes, a0
	bsr.w emit
	bne.w bad
	cmpi.l #4, d6
	bls.w next
	tst.w pkg.Header.LittleEndian(a3)
	beq.w next
	suba.l a0, a0
	move.l d7, d0
	bsr.w emit
	bne.w bad
next
	movea.l DataCall.Cursor(a6), a0
	movea.l DataCall.End(a6), a1
	cmpa.l a1, a0
	beq.w good
	cmpa.l a1, a0
	bhi.w bad
	cmpi.b #4, (a0)+
	bne.w bad
	cmpa.l a1, a0
	bhs.w bad
	move.l a0, DataCall.Cursor(a6)
	bra.w value
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	adda.w #DATA_CALL_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; emitData

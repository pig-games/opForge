; Package-selected raw generated-call recipes; no binding or rescanning inserts.
	.module prvm.amigaos.macro_fragments
	.cpu 68020
	.use prvm.amigaos.abi as abi
	.pub
State	.struct
Budget	.long ?
Count	.long ?
Capacity	.long ?
Pos	.long ?
Literal	.long ?
Start	.long ?
End	.long ?
Kind	.long ?
NameStart	.long ?
NameEnd	.long ?
Aux	.long ?
Stage	.res 64*32
	.endstruct
STATE_BYTES = State.Stage+64*32
	.section code, kind=code
	.pub
; A0 shared request, D0 available frame bytes. Returns D0 status, D1 count,
; D2 error byte offset, D3 published bytes. Preserves D4-D7/A3-A6.
; Stack stages at most 64 records; failures never touch caller result memory.
; CCR reflects D0. Thin macro_runtime wrapper owns optional profiling.
run	.block
	movem.l d4-d7/a3-a6, -(sp)
	move.l a0, d1
	beq.w invalidFrame
	andi.l #1, d1
	bne.w invalidFrame
	cmpi.l #abi.PRVM_REQUEST_FRAME_SIZE, d0
	bcs.w invalidFrame
	movea.l a0, a4
	cmpi.l #abi.PRVM_MAGIC_OPRP, abi.PRVM_FRAME_MAGIC(a4)
	bne.w invalidFrame
	cmpi.w #abi.PRVM_ABI_VERSION_V1, abi.PRVM_FRAME_ABI_VERSION(a4)
	bne.w invalidFrame
	cmpi.w #abi.PRVM_REQUEST_FRAME_SIZE, abi.PRVM_FRAME_FRAME_SIZE(a4)
	bcs.w invalidFrame
	cmpi.w #abi.PRVM_ENTRY_KIND_MACRO_FRAGMENTS, abi.PRVM_FRAME_ENTRY_KIND(a4)
	bne.w invalidFrame
	tst.w abi.PRVM_FRAME_CALL_MODE(a4)
	bne.w invalidFrame
	cmpi.l #abi.PRVM_PARSER_CONTRACT_VERSION_V2, abi.PRVM_FRAME_PARSER_CONTRACT_VERSION(a4)
	bne.w invalidFrame
	tst.l abi.PRVM_FRAME_FLAGS(a4)
	bne.w invalidFrame
	tst.l abi.PRVM_FRAME_TOKEN_COUNT(a4)
	bne.w invalidFrame
	move.l abi.PRVM_FRAME_SOURCE_LEN(a4), d7
	cmpi.l #253, d7
	bhi.w invalidFrame
	movea.l abi.PRVM_FRAME_SOURCE_PTR(a4), a6
	tst.l d7
	beq.w sourceReady
	move.l a6, d1
	beq.w invalidFrame
	add.l d7, d1
	bcs.w invalidFrame
sourceReady
	cmpi.l #10, abi.PRVM_FRAME_PROGRAM_LEN(a4)
	bne.w invalidProgramFrame
	movea.l abi.PRVM_FRAME_PROGRAM_PTR(a4), a5
	move.l a5, d1
	beq.w invalidFrame
	addi.l #10, d1
	bcs.w invalidFrame
	cmpi.b #$86, (a5)
	bne.w invalidProgramFrame
	cmpi.b #1, 1(a5)
	bne.w invalidProgramFrame
	cmpi.b #$83, 8(a5)
	bne.w invalidProgramFrame
	tst.b 9(a5)
	bne.w invalidProgramFrame
	cmpi.b #'1', 4(a5)
	bcs.w invalidProgramFrame
	cmpi.b #'9', 5(a5)
	bhi.w invalidProgramFrame
	move.b 4(a5), d0
	cmp.b 5(a5), d0
	bhi.w invalidProgramFrame
	; Four distinct ASCII punctuation operands select sigils and braces.
	moveq #2, d4
validateMarker
	moveq #0, d1
	move.b 0(a5, d4.l), d1
	bsr.w punctuation
	tst.l d0
	beq.w invalidProgramFrame
	moveq #2, d5
checkDuplicate
	cmp.l d4, d5
	beq.w markerValid
	cmp.b 0(a5, d5.l), d1
	beq.w invalidProgramFrame
	addq.l #1, d5
	cmpi.l #4, d5
	bne.w checkDuplicate
	moveq #6, d5
	bra.w checkDuplicate
markerValid
	addq.l #1, d4
	cmpi.l #4, d4
	bne.w markerNext
	moveq #6, d4
markerNext
	cmpi.l #8, d4
	bne.w validateMarker
	suba.l #STATE_BYTES, sp
	movea.l sp, a3
	movea.l a3, a0
	moveq #State.Stage/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	move.l abi.PRVM_FRAME_STEP_BUDGET(a4), State.Budget(a3)
	move.l abi.PRVM_FRAME_RESULT_CAPACITY(a4), d0
	bmi.w argumentFailure
	lsr.l #5, d0
	cmpi.l #64, d0
	bls.w capacityReady
	moveq #64, d0
capacityReady
	move.l d0, State.Capacity(a3)
	moveq #10, d0
	bsr.w tick
	bne.w failure
scan
	move.l State.Pos(a3), d4
	cmp.l d7, d4
	bcc.w finish
	moveq #1, d0
	bsr.w tick
	bne.w failure
	move.l d4, State.Start(a3)
	move.l d4, d5
	addq.l #1, d5
	move.l d5, State.End(a3)
	clr.l State.Kind(a3)
	clr.l State.Aux(a3)
	clr.l State.NameStart(a3)
	clr.l State.NameEnd(a3)
	cmp.l d7, d5
	bcc.w literalByte
	moveq #0, d0
	move.b 0(a6, d4.l), d0
	cmp.b 2(a5), d0
	beq.w marker
	cmp.b 3(a5), d0
	bne.w literalByte
marker
	moveq #0, d1
	move.b 0(a6, d5.l), d1
	cmp.b 4(a5), d1
	bcs.w nonPositional
	cmp.b 5(a5), d1
	bhi.w nonPositional
	moveq #0, d0
	move.b 4(a5), d0
	sub.l d0, d1
	move.l d1, State.Aux(a3)
	move.l #abi.PRVM_RESULT_MACRO_POSITIONAL, State.Kind(a3)
	addq.l #1, State.End(a3)
	bra.w recognized
nonPositional
	cmp.b 3(a5), d0
	bne.w literalByte
	cmp.b 2(a5), d1
	bne.w named
	move.l #abi.PRVM_RESULT_MACRO_SUPPLIED_LIST, State.Kind(a3)
	addq.l #1, State.End(a3)
	bra.w recognized
named
	moveq #0, d6
	cmp.b 6(a5), d1
	bne.w nameBegin
	moveq #1, d6
	addq.l #1, d5
nameBegin
	move.l d5, State.NameStart(a3)
nameLoop
	cmp.l d7, d5
	bcc.w nameReady
	moveq #0, d1
	move.b 0(a6, d5.l), d1
	bsr.w nameByte
	tst.l d0
	beq.w nameReady
	moveq #1, d0
	bsr.w tick
	bne.w nameBudgetFailure
	addq.l #1, d5
	bra.w nameLoop
nameBudgetFailure
	move.l d5, State.Pos(a3)
	bra.w failure
nameReady
	cmp.l State.NameStart(a3), d5
	beq.w literalByte
	move.l d5, State.NameEnd(a3)
	tst.l d6
	beq.w nameAccepted
	cmp.l d7, d5
	bcc.w literalByte
	move.b 7(a5), d0
	cmp.b 0(a6, d5.l), d0
	bne.w literalByte
	addq.l #1, d5
nameAccepted
	move.l d5, State.End(a3)
	move.l #abi.PRVM_RESULT_MACRO_NAMED, State.Kind(a3)
recognized
	move.l State.Literal(a3), d4
	cmp.l State.Start(a3), d4
	beq.w emitReference
	moveq #abi.PRVM_RESULT_MACRO_LITERAL, d0
	move.l State.Start(a3), d5
	bsr.w emit
	bne.w failure
emitReference
	move.l State.Kind(a3), d0
	move.l State.Start(a3), d4
	move.l State.End(a3), d5
	bsr.w emit
	bne.w failure
	move.l State.End(a3), State.Literal(a3)
	move.l State.End(a3), State.Pos(a3)
	bra.w scan
literalByte
	addq.l #1, State.Pos(a3)
	bra.w scan
finish
	move.l State.Literal(a3), d4
	cmp.l d7, d4
	beq.w publish
	move.l d7, d5
	moveq #abi.PRVM_RESULT_MACRO_LITERAL, d0
	bsr.w emit
	bne.w failure
publish
	move.l State.Count(a3), d3
	lsl.l #5, d3
	beq.w success
	move.l abi.PRVM_FRAME_RESULT_PTR(a4), d0
	beq.w argumentFailure
	move.l d0, d1
	andi.l #1, d1
	bne.w argumentFailure
	move.l d0, d1
	add.l d3, d1
	bcs.w argumentFailure
	movea.l d0, a1
	lea State.Stage(a3), a0
	move.l d3, d0
	lsr.l #2, d0
	subq.l #1, d0
copy
	move.l (a0)+, (a1)+
	dbra d0, copy
success
	move.l State.Count(a3), d1
	clr.l d2
	moveq #abi.PRVM_STATUS_OK, d0
	bra.w done
argumentFailure
	clr.l State.Pos(a3)
	moveq #abi.PRVM_STATUS_INVALID_ARGUMENT, d0
failure
	move.l State.Pos(a3), d2
	clr.l d1
	clr.l d3
done
	adda.l #STATE_BYTES, sp
	movem.l (sp)+, d4-d7/a3-a6
	tst.l d0
	rts
invalidProgramFrame
	moveq #abi.PRVM_STATUS_INVALID_PROGRAM, d0
	bra.w frameFailure
invalidFrame
	moveq #abi.PRVM_STATUS_INVALID_ARGUMENT, d0
frameFailure
	clr.l d1
	clr.l d2
	clr.l d3
	movem.l (sp)+, d4-d7/a3-a6
	tst.l d0
	rts
	.bend  ; run
	.priv
; D0 charge. Returns status in D0/CCR. Clobbers D1.
tick	.block
	move.l State.Budget(a3), d1
	sub.l d0, d1
	bcs.w exhausted
	move.l d1, State.Budget(a3)
	moveq #0, d0
	rts
exhausted
	moveq #abi.PRVM_STATUS_BUDGET_EXCEEDED, d0
	rts
	.bend  ; tick
; D1 byte, D0 boolean: ASCII punctuation ranges.
punctuation	.block
	cmpi.b #33, d1
	bcs.w no
	cmpi.b #47, d1
	bls.w yes
	cmpi.b #58, d1
	bcs.w no
	cmpi.b #64, d1
	bls.w yes
	cmpi.b #91, d1
	bcs.w no
	cmpi.b #96, d1
	bls.w yes
	cmpi.b #123, d1
	bcs.w no
	cmpi.b #126, d1
	bls.w yes
no
	moveq #0, d0
	rts
yes
	moveq #1, d0
	rts
	.bend  ; punctuation
; D1 ASCII byte, D0 boolean. ASCII alphanumeric/underscore only.
nameByte	.block
	cmpi.b #'_', d1
	beq.w yes
	cmpi.b #'0', d1
	bcs.w no
	cmp.b 5(a5), d1
	bls.w yes
	cmpi.b #'A', d1
	bcs.w no
	cmpi.b #'Z', d1
	bls.w yes
	cmpi.b #'a', d1
	bcs.w no
	cmpi.b #'z', d1
	bls.w yes
no
	moveq #0, d0
	rts
yes
	moveq #1, d0
	rts
	.bend  ; nameByte
; D0 kind, D4/D5 source range. Returns status/CCR; clobbers D1/A0.
emit	.block
	move.l d0, -(sp)
	moveq #1, d0
	bsr.w tick
	bne.w emitFailed
	move.l State.Count(a3), d1
	cmp.l State.Capacity(a3), d1
	bcc.w overflow
	lsl.l #5, d1
	lea State.Stage(a3), a0
	adda.l d1, a0
	moveq #7, d1
clearRecord
	clr.l (a0)+
	dbra d1, clearRecord
	suba.l #32, a0
	move.l (sp)+, d0
	move.w d0, (a0)
	move.l d4, 12(a0)
	move.l d5, 16(a0)
	cmpi.w #abi.PRVM_RESULT_MACRO_POSITIONAL, d0
	bne.w emitName
	move.l State.Aux(a3), 20(a0)
	bra.w emitted
emitName
	cmpi.w #abi.PRVM_RESULT_MACRO_NAMED, d0
	bne.w emitted
	move.l State.NameStart(a3), 20(a0)
	move.l State.NameEnd(a3), 24(a0)
emitted
	addq.l #1, State.Count(a3)
	moveq #0, d0
	rts
overflow
	moveq #abi.PRVM_STATUS_OUTPUT_OVERFLOW, d0
emitFailed
	move.l d4, State.Pos(a3)
	addq.l #4, sp
	tst.l d0
	rts
	.bend  ; emit
	.endsection
	.endmodule

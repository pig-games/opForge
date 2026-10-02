; Package-selected inline metadata operand spans over immutable compact records.
; No source spelling or host file operation enters this service.
; @opforge-owner: prvm.amigaos.packed_metadata
	.module prvm.amigaos.packed_metadata
	.cpu 68020
	.use prvm.amigaos.abi as abi
	.include "telemetry_macros.i"
	.pub
State	.struct
Budget	.long ?
Policy	.long ?
Stage	.res 32
	.endstruct
STATE_BYTES = State.Stage+32
ROLE = State.Stage+abi.PRVM_METADATA_ROLE
KEY = State.Stage+abi.PRVM_METADATA_KEY
VALUE_OFFSET = State.Stage+abi.PRVM_METADATA_VALUE_OFFSET
VALUE_BYTES = State.Stage+abi.PRVM_METADATA_VALUE_BYTES
VALUE_KIND = State.Stage+abi.PRVM_METADATA_VALUE_KIND
	.section code, kind=code
; A0=request,D0=available bytes; standard PRVM status/count/offset/bytes.
; Stage one result atomically. Offsets refer to the complete packed record.
run	.block
	movem.l d4-d7/a3-a6, -(sp)
	move.l a0, d1
	beq.w invalidFrame
	btst #0, d1
	bne.w invalidFrame
	cmpi.l #abi.PRVM_REQUEST_FRAME_SIZE, d0
	blo.w invalidFrame
	movea.l a0, a4
	cmpi.l #abi.PRVM_MAGIC_OPRP, abi.PRVM_FRAME_MAGIC(a4)
	bne.w invalidFrame
	cmpi.w #abi.PRVM_ABI_VERSION_V1, abi.PRVM_FRAME_ABI_VERSION(a4)
	bne.w invalidFrame
	cmpi.w #abi.PRVM_REQUEST_FRAME_SIZE, abi.PRVM_FRAME_FRAME_SIZE(a4)
	blo.w invalidFrame
	cmpi.w #abi.PRVM_ENTRY_KIND_PACKED_METADATA, abi.PRVM_FRAME_ENTRY_KIND(a4)
	bne.w invalidFrame
	tst.w abi.PRVM_FRAME_CALL_MODE(a4)
	bne.w invalidFrame
	cmpi.l #abi.PRVM_PARSER_CONTRACT_VERSION_V2, abi.PRVM_FRAME_PARSER_CONTRACT_VERSION(a4)
	bne.w invalidFrame
	tst.l abi.PRVM_FRAME_FLAGS(a4)
	bne.w invalidFrame
	tst.l abi.PRVM_FRAME_TOKEN_PTR(a4)
	bne.w invalidFrame
	tst.l abi.PRVM_FRAME_TOKEN_COUNT(a4)
	bne.w invalidFrame
	move.l abi.PRVM_FRAME_STEP_BUDGET(a4), d0
	bmi.w invalidFrame
	move.l abi.PRVM_FRAME_RESULT_CAPACITY(a4), d0
	bmi.w invalidFrame
	move.l abi.PRVM_FRAME_SOURCE_PTR(a4), d0
	beq.w invalidFrame
	movea.l d0, a6
	move.l abi.PRVM_FRAME_SOURCE_LEN(a4), d6
	cmpi.l #4, d6
	blo.w invalidFrame
	cmpi.l #256, d6
	bhi.w invalidFrame
	moveq #0, d0
	move.b (a6), d0
	addq.l #1, d0
	cmp.l d6, d0
	bne.w invalidFrame
	move.b 1(a6), d0
	andi.b #$c0, d0
	bne.w invalidFrame
	move.l abi.PRVM_FRAME_PROGRAM_PTR(a4), d0
	beq.w invalidFrame
	movea.l d0, a5
	cmpi.l #34, abi.PRVM_FRAME_PROGRAM_LEN(a4)
	bne.w invalidProgram
	cmpi.b #$96, (a5)
	bne.w invalidProgram
	cmpi.b #6, 1(a5)
	bne.w invalidProgram
	cmpi.b #$83, 32(a5)
	bne.w invalidProgram
	tst.b 33(a5)
	bne.w invalidProgram
	lea 2(a5), a2
	moveq #5, d5
validateRows
	cmpi.b #1, 2(a2)
	blo.w invalidProgram
	cmpi.b #2, 2(a2)
	bhi.w invalidProgram
	cmpi.b #1, 3(a2)
	blo.w invalidProgram
	cmpi.b #5, 3(a2)
	bhi.w invalidProgram
	cmpi.b #2, 4(a2)
	bhi.w invalidProgram
	addq.l #5, a2
	dbra d5, validateRows
	suba.w #STATE_BYTES, sp
	movea.l sp, a3
	movea.l a3, a0
	moveq #STATE_BYTES/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	move.l abi.PRVM_FRAME_STEP_BUDGET(a4), State.Budget(a3)
	clr.l d2
	bsr.w tick
	bne.w failure
	move.l #$96, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	move.b 1(a6), d0
	andi.b #24, d0
	bne.w noMatch
	cmpi.l #4, d6
	beq.w noMatch
	moveq #4, d2
	bsr.w tick
	bne.w failure
	move.l #$96, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	cmpi.l #9, d6
	blo.w noMatch
	cmpi.b #7, 4(a6)
	bne.w noMatch
	cmpi.b #1, 5(a6)
	bhi.w noMatch
	moveq #0, d0
	move.b 6(a6), d0
	lsl.w #8, d0
	move.b 7(a6), d0
	lea 2(a5), a2
	moveq #5, d5
findHead
	moveq #0, d1
	move.b (a2), d1
	lsl.w #8, d1
	move.b 1(a2), d1
	cmp.w d1, d0
	beq.w matched
	addq.l #5, a2
	dbra d5, findHead
	bra.w noMatch
matched
	moveq #8, d2
	tst.b 8(a6)
	bne.w tokenFailure
	move.b 1(a6), d0
	andi.b #6, d0
	bne.w tokenFailure
	moveq #0, d0
	move.b 2(a2), d0
	move.w d0, ROLE(a3)
	move.b 3(a2), d0
	move.l d0, KEY(a3)
	moveq #0, d0
	move.b 4(a2), d0
	move.l d0, State.Policy(a3)
	moveq #9, d2
	bsr.w tick
	bne.w failure
	moveq #0, d5
	cmpi.l #9, d6
	bne.w operand
	cmpi.l #1, State.Policy(a3)
	beq.w validated
	bra.w tokenFailure
operand
	cmpi.l #11, d6
	blo.w tokenFailure
	cmpi.b #3, 9(a6)
	bne.w tokenFailure
	move.b 10(a6), d5
	moveq #11, d2
	move.l d2, d0
	add.l d5, d0
	cmp.l d6, d0
	bne.w tokenFailure
	cmpi.l #2, State.Policy(a3)
	bne.w stringReady
	cmpi.l #2, d5
	bne.w tokenFailure
stringReady
	move.l #3, VALUE_KIND(a3)
validated
	bsr.w tick
	bne.w failure
	move.l #$83, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	cmpi.l #32, abi.PRVM_FRAME_RESULT_CAPACITY(a4)
	blo.w overflow
	move.l abi.PRVM_FRAME_RESULT_PTR(a4), d0
	beq.w argumentFailure
	btst #0, d0
	bne.w argumentFailure
	bsr.w tick
	bne.w failure
	clr.l d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	move.w #abi.PRVM_RESULT_PACKED_METADATA, State.Stage(a3)
	move.l d2, VALUE_OFFSET(a3)
	move.l d5, VALUE_BYTES(a3)
	lea State.Stage(a3), a0
	movea.l abi.PRVM_FRAME_RESULT_PTR(a4), a1
	moveq #7, d0
publish
	move.l (a0)+, (a1)+
	dbra d0, publish
	moveq #1, d1
	moveq #32, d3
	clr.l d2
	moveq #0, d0
	bra.w finish
noMatch
	clr.l d2
	moveq #0, d0
	bra.w failure
argumentFailure
	moveq #abi.PRVM_STATUS_INVALID_ARGUMENT, d0
	bra.w failure
tokenFailure
	moveq #abi.PRVM_STATUS_INVALID_TOKEN, d0
	bra.w failure
overflow
	moveq #abi.PRVM_STATUS_OUTPUT_OVERFLOW, d0
failure
	clr.l d1
	clr.l d3
finish
	adda.w #STATE_BYTES, sp
	bra.w done
invalidProgram
	moveq #abi.PRVM_STATUS_INVALID_PROGRAM, d0
	bra.w invalid
invalidFrame
	moveq #abi.PRVM_STATUS_INVALID_ARGUMENT, d0
invalid
	clr.l d1
	clr.l d2
	clr.l d3
done
	movem.l (sp)+, d4-d7/a3-a6
	tst.l d0
	rts
	.bend  ; run
	.priv
; One budget unit per interpreted envelope/match/operand/publication/end opcode.
tick	.block
	tst.l State.Budget(a3)
	beq.w exhausted
	subq.l #1, State.Budget(a3)
	moveq #0, d0
	rts
exhausted
	moveq #abi.PRVM_STATUS_BUDGET_EXCEEDED, d0
	rts
	.bend  ; tick
	.endsection
	.endmodule

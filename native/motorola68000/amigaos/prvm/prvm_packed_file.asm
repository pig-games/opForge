; Package-selected file operand spans over immutable compact records.
; No source spelling or host file operation enters this service.
; @opforge-owner: prvm.amigaos.packed_file
	.module prvm.amigaos.packed_file
	.cpu 68020
	.use prvm.amigaos.abi as abi
	.include "telemetry_macros.i"
	.pub
State	.struct
Budget	.long ?
Prefix	.long ?
Stage	.res 32
	.endstruct
STATE_BYTES = State.Stage+32
PATH_OFFSET = State.Stage+abi.PRVM_FILE_PATH_OFFSET
PATH_BYTES = State.Stage+abi.PRVM_FILE_PATH_BYTES
PREFIX_BYTES = State.Stage+abi.PRVM_FILE_PREFIX_BYTES
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
	cmpi.w #abi.PRVM_ENTRY_KIND_PACKED_FILE, abi.PRVM_FRAME_ENTRY_KIND(a4)
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
	cmpi.l #8, abi.PRVM_FRAME_PROGRAM_LEN(a4)
	bne.w invalidProgram
	cmpi.b #$90, (a5)
	bne.w invalidProgram
	move.b 1(a5), d0
	andi.b #$fe, d0
	bne.w invalidProgram
	cmpi.b #$91, 2(a5)
	bne.w invalidProgram
	cmpi.b #$92, 5(a5)
	bne.w invalidProgram
	cmpi.b #$83, 6(a5)
	bne.w invalidProgram
	tst.b 7(a5)
	bne.w invalidProgram
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
	move.l #$90, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	move.b 1(a6), d0
	andi.b #24, d0
	bne.w noMatch
	cmpi.l #4, d6
	beq.w noMatch
	moveq #4, d2
	btst #0, 1(a5)
	beq.w matchHead
	cmpi.l #9, d6
	blo.w matchHead
	cmpi.b #1, 4(a6)
	bhi.w matchHead
	cmpi.b #5, 8(a6)
	bne.w matchHead
	moveq #9, d2
	move.l #5, State.Prefix(a3)
matchHead
	bsr.w tick
	bne.w failure
	move.l #$91, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	move.l d6, d0
	sub.l d2, d0
	cmpi.l #5, d0
	blo.w noMatch
	cmpi.b #7, 0(a6, d2.l)
	bne.w noMatch
	cmpi.b #1, 1(a6, d2.l)
	bhi.w noMatch
	tst.b 4(a6, d2.l)
	bne.w noMatch
	moveq #0, d0
	move.b 2(a6, d2.l), d0
	lsl.w #8, d0
	move.b 3(a6, d2.l), d0
	moveq #0, d1
	move.b 3(a5), d1
	lsl.w #8, d1
	move.b 4(a5), d1
	cmp.w d1, d0
	bne.w noMatch
	tst.l State.Prefix(a3)
	beq.w operand
	tst.b 7(a6)
	beq.w operand
	moveq #7, d2
	bra.w tokenFailure
operand
	addq.l #5, d2
	bsr.w tick
	bne.w failure
	move.l #$92, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	move.l d6, d0
	sub.l d2, d0
	cmpi.l #2, d0
	blo.w tokenFailure
	cmpi.b #3, 0(a6, d2.l)
	bne.w tokenFailure
	moveq #0, d5
	move.b 1(a6, d2.l), d5
	addq.l #2, d2
	tst.l d5
	beq.w tokenFailure
	move.l d2, d0
	add.l d5, d0
	cmp.l d6, d0
	bne.w tokenFailure
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
	move.w #abi.PRVM_RESULT_PACKED_FILE, State.Stage(a3)
	move.l d2, PATH_OFFSET(a3)
	move.l d5, PATH_BYTES(a3)
	move.l State.Prefix(a3), PREFIX_BYTES(a3)
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

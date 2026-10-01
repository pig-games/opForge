; Package-selected built-in data operand plans over immutable compact records.
; @opforge-owner: prvm.amigaos.packed_data
; No source spelling or value evaluation or host operation enters this service.
	.module prvm.amigaos.packed_data
	.cpu 68020
	.use prvm.amigaos.abi as abi
	.include "telemetry_macros.i"
	.pub
State	.struct
Budget	.long ?
Prefix	.long ?
Unit	.long ?
Comma	.long ?
Depth	.long ?
Stage	.res 32
	.endstruct
STATE_BYTES = State.Stage+32
UNIT_OFFSET = State.Stage+abi.PRVM_DATA_UNIT_OFFSET
UNIT_BYTES = State.Stage+abi.PRVM_DATA_UNIT_BYTES
VALUES_OFFSET = State.Stage+abi.PRVM_DATA_VALUES_OFFSET
VALUES_BYTES = State.Stage+abi.PRVM_DATA_VALUES_BYTES
FIXED_WIDTH = State.Stage+abi.PRVM_DATA_FIXED_WIDTH
PREFIX_BYTES = State.Stage+abi.PRVM_DATA_PREFIX_BYTES
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
	cmpi.w #abi.PRVM_ENTRY_KIND_PACKED_DATA, abi.PRVM_FRAME_ENTRY_KIND(a4)
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
	cmpi.l #16, abi.PRVM_FRAME_PROGRAM_LEN(a4)
	bne.w invalidProgram
	cmpi.b #$93, (a5)
	bne.w invalidProgram
	move.b 1(a5), d0
	andi.b #$fe, d0
	bne.w invalidProgram
	cmpi.b #$94, 2(a5)
	bne.w invalidProgram
	cmpi.b #$95, 5(a5)
	bne.w invalidProgram
	cmpi.b #$83, 14(a5)
	bne.w invalidProgram
	tst.b 15(a5)
	bne.w invalidProgram
	moveq #0, d0
	move.b 12(a5), d0
	lsl.w #8, d0
	move.b 13(a5), d0
	tst.w d0
	beq.w invalidProgram
wordWidthValid
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
	move.l #$93, d7
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
	move.l #$94, d7
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
	move.l d2, State.Unit(a3)
	move.l #-1, State.Comma(a3)
	move.l #$95, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
scan
	cmp.l d6, d2
	bcc.w scanned
	bsr.w tick
	bne.w failure
	moveq #0, d0
	move.b 0(a6, d2.l), d0
	cmpi.b #1, d0
	bls.w nameToken
	cmpi.b #2, d0
	beq.w numberToken
	cmpi.b #3, d0
	beq.w stringToken
	cmpi.b #$81, d0
	beq.w compiledToken
	cmpi.b #4, d0
	beq.w commaToken
	cmpi.b #14, d0
	beq.w openToken
	cmpi.b #15, d0
	beq.w closeToken
	cmpi.b #6, d0
	beq.w singleToken
	cmpi.b #7, d0
	beq.w singleToken
	cmpi.b #9, d0
	beq.w singleToken
	cmpi.b #16, d0
	blo.w tokenFailure
	cmpi.b #39, d0
	bhi.w tokenFailure
singleToken
	addq.l #1, d2
	bra.w extent
nameToken
	addq.l #4, d2
	bra.w extent
numberToken
	addq.l #5, d2
	bra.w extent
stringToken
	tst.l State.Comma(a3)
	bmi.w tokenFailure
	move.l d2, d0
	addq.l #2, d0
	cmp.l d6, d0
	bhi.w tokenFailure
	moveq #0, d1
	move.b 1(a6, d2.l), d1
	add.l d1, d0
	move.l d0, d2
	bra.w extent
compiledToken
	move.l d2, d0
	addq.l #2, d0
	cmp.l d6, d0
	bhi.w tokenFailure
	moveq #0, d1
	move.b 1(a6, d2.l), d1
	beq.w tokenFailure
	add.l d1, d0
	move.l d0, d2
	bra.w extent
commaToken
	tst.l State.Depth(a3)
	bne.w tokenFailure
	tst.l State.Comma(a3)
	bpl.w singleToken
	cmp.l State.Unit(a3), d2
	beq.w tokenFailure
	move.l d2, State.Comma(a3)
	bra.w singleToken
openToken
	addq.l #1, State.Depth(a3)
	cmpi.l #16, State.Depth(a3)
	bhi.w tokenFailure
	bra.w singleToken
closeToken
	tst.l State.Depth(a3)
	beq.w tokenFailure
	subq.l #1, State.Depth(a3)
	bra.w singleToken
extent
	cmp.l d6, d2
	bhi.w tokenFailure
	bra.w scan
scanned
	tst.l State.Depth(a3)
	bne.w tokenFailure
	move.l State.Comma(a3), d5
	bmi.w tokenFailure
	move.l State.Unit(a3), d4
	move.l d4, UNIT_OFFSET(a3)
	move.l d5, d0
	sub.l d4, d0
	move.l d0, UNIT_BYTES(a3)
	addq.l #1, d5
	cmp.l d6, d5
	bcc.w tokenFailure
	move.l d5, VALUES_OFFSET(a3)
	move.l d6, d1
	sub.l d5, d1
	move.l d1, VALUES_BYTES(a3)
	cmpi.l #4, d0
	bne.w publishCheck
	cmpi.b #1, 0(a6, d4.l)
	bhi.w publishCheck
	tst.b 3(a6, d4.l)
	bne.w publishCheck
	moveq #0, d0
	move.b 1(a6, d4.l), d0
	lsl.w #8, d0
	move.b 2(a6, d4.l), d0
	moveq #6, d5
	bsr.w unitId
	beq.w byteWidth
	moveq #8, d5
	bsr.w unitId
	beq.w wordWidth
	moveq #10, d5
	bsr.w unitId
	bne.w publishCheck
	moveq #4, d0
	bra.w fixedWidth
byteWidth
	moveq #1, d0
	bra.w fixedWidth
wordWidth
	moveq #12, d5
	bsr.w programId
	move.l d1, d0
fixedWidth
	move.l d0, FIXED_WIDTH(a3)
publishCheck
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
	move.w #abi.PRVM_RESULT_PACKED_DATA, State.Stage(a3)
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
; D5 program byte offset -> D1 decoded big-endian ID; D0 preserved.
programId	.block
	moveq #0, d1
	move.b 0(a5, d5.l), d1
	lsl.w #8, d1
	move.b 1(a5, d5.l), d1
	rts
	.bend  ; programId
; Compare D0 numeric unit with package ID at D5; CCR reflects comparison.
unitId	.block
	bsr.w programId
	cmp.w d1, d0
	rts
	.bend  ; unitId
; One budget unit per envelope/match/token/publication/end operation.
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

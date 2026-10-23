; Package-selected inline metadata operand spans over immutable compact records.
; No source spelling or host file operation enters this service.
; @opforge-owner: opforge.vm.prvm.packed_metadata
	.module opforge.vm.prvm.packed_metadata
	.cpu 68020
	.use opforge.vm.prvm.abi as abi
	.include "telemetry_macros.i"
	.pub
State	.struct
Budget	.long ?
Policy	.long ?
Stage	.res abi.PRVM_RESULT_RECORD_SIZE
Context	.word ?
Target	.word ?
HeadSeen	.word ?
Fallback	.word ?
	.endstruct
STATE_BYTES = State.Fallback+2
ROLE = State.Stage+abi.PRVM_METADATA_ROLE
KEY = State.Stage+abi.PRVM_METADATA_KEY
TARGET = State.Stage+abi.PRVM_METADATA_TARGET
VALUE_OFFSET = State.Stage+abi.PRVM_METADATA_VALUE_OFFSET
VALUE_BYTES = State.Stage+abi.PRVM_METADATA_VALUE_BYTES
VALUE_KIND = State.Stage+abi.PRVM_METADATA_VALUE_KIND
RANGE_TEXT_OFFSET = State.Stage+abi.PRVM_METADATA_RANGE_TEXT_OFFSET
RANGE_TEXT_BYTES = State.Stage+abi.PRVM_METADATA_RANGE_TEXT_BYTES
RANGE_START = State.Stage+abi.PRVM_METADATA_RANGE_START
RANGE_END = State.Stage+abi.PRVM_METADATA_RANGE_END
FILL_VALUE = State.Stage+abi.PRVM_METADATA_FILL_VALUE
RECORD_HEAD = abi.PRVM_PACKED_RECORD_BYTES
RECORD_HEAD_PREFIX = RECORD_HEAD+abi.PackedDot.Name+abi.PackedName.Kind
RECORD_HEAD_ID = RECORD_HEAD+abi.PackedDot.Name+abi.PackedName.Id
RECORD_HEAD_QUALIFIER = RECORD_HEAD+abi.PackedDot.Name+abi.PackedName.Qualifier
RECORD_OPERAND = RECORD_HEAD+abi.PRVM_PACKED_DOT_BYTES
RECORD_VALUE = RECORD_OPERAND+abi.PRVM_PACKED_STRING_HEADER_BYTES
IMMUTABLE_FLAGS = abi.PRVM_PACKED_FLAG_ALLOWED-abi.PRVM_PACKED_FLAG_MUTABLE_SNAPSHOT
BLOCK_FLAGS = abi.PRVM_PACKED_FLAG_BLOCK_OPEN+abi.PRVM_PACKED_FLAG_BLOCK_CLOSE
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
	cmpi.l #abi.PRVM_PACKED_RECORD_BYTES, d6
	blo.w invalidFrame
	cmpi.l #abi.PRVM_PACKED_MAX_RECORD_BYTES, d6
	bhi.w invalidFrame
	moveq #0, d0
	move.b (a6), d0
	addq.l #1, d0
	cmp.l d6, d0
	bne.w invalidFrame
	move.b abi.PackedRecord.Flags(a6), d0
	andi.b #$ff-IMMUTABLE_FLAGS, d0
	bne.w invalidFrame
	move.l abi.PRVM_FRAME_PROGRAM_PTR(a4), d0
	beq.w invalidFrame
	movea.l d0, a5
	bsr.w validateEnvelope
	bne.w invalidProgram
	move.l abi.PRVM_FRAME_FLAGS(a4), d0
	move.l d0, d1
	andi.l #$ffff, d0
	cmpi.l #abi.PRVM_METADATA_CONTEXT_TARGET, d0
	bhi.w invalidFrame
	swap d1
	andi.l #$ffff, d1
	cmp.l d7, d1
	bhi.w invalidFrame
	tst.l d1
	beq.w noContextTarget
	cmpi.w #abi.PRVM_METADATA_CONTEXT_TARGET, d0
	bne.w invalidFrame
	bra.w contextReady
noContextTarget
	cmpi.w #abi.PRVM_METADATA_CONTEXT_TARGET, d0
	beq.w invalidFrame
contextReady
	suba.w #STATE_BYTES, sp
	movea.l sp, a3
	movea.l a3, a0
	moveq #STATE_BYTES/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	move.l abi.PRVM_FRAME_STEP_BUDGET(a4), State.Budget(a3)
	move.w abi.PRVM_FRAME_FLAGS+2(a4), State.Context(a3)
	move.w abi.PRVM_FRAME_FLAGS(a4), State.Target(a3)
	clr.l d2
	bsr.w tick
	bne.w failure
	move.l #abi.OP_METADATA_SELECT, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	move.b abi.PackedRecord.Flags(a6), d0
	andi.b #abi.PRVM_PACKED_FLAG_INACTIVE, d0
	bne.w noMatch
	cmpi.l #abi.PRVM_PACKED_RECORD_BYTES, d6
	beq.w noMatch
	moveq #RECORD_HEAD, d2
	bsr.w tick
	bne.w failure
	move.l #abi.OP_METADATA_SELECT, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	cmpi.l #RECORD_OPERAND, d6
	blo.w unrelated
	cmpi.b #abi.PRVM_TOKEN_KIND_DOT, RECORD_HEAD(a6)
	bne.w unrelated
	cmpi.b #1, RECORD_HEAD_PREFIX(a6)
	bhi.w unrelated
	moveq #0, d0
	move.b RECORD_HEAD_ID(a6), d0
	lsl.w #8, d0
	move.b RECORD_HEAD_ID+1(a6), d0
	moveq #0, d4  ; lower inclusive row index
	moveq #0, d5
	move.w abi.MetadataProgram.Count(a5), d5  ; upper exclusive
findHead
	cmp.l d5, d4
	bhs.w scanStart
	move.l d4, d1
	add.l d5, d1
	lsr.l #1, d1
	move.l d1, d7
	lsl.l #abi.METADATA_ROW_SHIFT, d7
	lea abi.METADATA_PROGRAM_ROWS(a5, d7.l), a2
	moveq #0, d7
	move.b abi.MetadataRow.HeadHigh(a2), d7
	lsl.w #8, d7
	move.b abi.MetadataRow.HeadLow(a2), d7
	cmp.w d7, d0
	bls.w lower
	move.l d1, d4
	addq.l #1, d4
	bra.w findHead
lower
	move.l d1, d5
	bra.w findHead
scanStart
	move.l d4, d1
	lsl.l #abi.METADATA_ROW_SHIFT, d1
	lea abi.METADATA_PROGRAM_ROWS(a5, d1.l), a2
	moveq #3, d5  ; disjoint four-context rows bound duplicate heads
scanHead
	cmp.w abi.MetadataProgram.Count(a5), d4
	bhs.w headEnd
	moveq #0, d1
	move.b abi.MetadataRow.HeadHigh(a2), d1
	lsl.w #8, d1
	move.b abi.MetadataRow.HeadLow(a2), d1
	cmp.w d1, d0
	bne.w headEnd
	move.w #1, State.HeadSeen(a3)
	moveq #0, d1
	move.w State.Context(a3), d1
	btst d1, abi.MetadataRow.Contexts(a2)
	bne.w matched
	cmpi.b #abi.PRVM_METADATA_OUTPUT_OPEN, abi.MetadataRow.Role(a2)
	bne.w nextContext
	move.w #1, State.Fallback(a3)
nextContext
	addq.l #abi.METADATA_ROW_BYTES, a2
	addq.l #1, d4
	dbra d5, scanHead
headEnd
	tst.w State.Context(a3)
	bne.w tokenFailure
	tst.w State.Fallback(a3)
	bne.w noMatch
	tst.w State.HeadSeen(a3)
	bne.w tokenFailure
	bra.w noMatch
unrelated
	tst.w State.Context(a3)
	bne.w tokenFailure
	bra.w noMatch
matched
	moveq #0, d7
	move.w abi.MetadataProgram.TargetCount(a5), d7
	bsr.w validateRow
	bne.w rowFailure
	moveq #RECORD_HEAD_QUALIFIER, d2
	tst.b RECORD_HEAD_QUALIFIER(a6)
	bne.w tokenFailure
	move.b abi.PackedRecord.Flags(a6), d0
	andi.b #BLOCK_FLAGS, d0
	bne.w tokenFailure
	moveq #0, d0
	move.b abi.MetadataRow.Role(a2), d0
	move.w d0, ROLE(a3)
	move.b abi.MetadataRow.Key(a2), d0
	move.w d0, KEY(a3)
	move.w abi.MetadataRow.Target(a2), TARGET(a3)
	cmpi.w #abi.PRVM_METADATA_TARGET_CLOSE, ROLE(a3)
	bne.w inheritTarget
	move.w TARGET(a3), d0
	cmp.w State.Target(a3), d0
	bne.w tokenFailure
inheritTarget
	cmpi.w #abi.PRVM_METADATA_OUTPUT, ROLE(a3)
	bne.w operandPolicy
	tst.w TARGET(a3)
	bne.w operandPolicy
	cmpi.w #abi.PRVM_METADATA_CONTEXT_TARGET, State.Context(a3)
	bne.w operandPolicy
	move.w State.Target(a3), TARGET(a3)
operandPolicy
	moveq #0, d0
	move.b abi.MetadataRow.Operand(a2), d0
	move.l d0, State.Policy(a3)
	moveq #RECORD_OPERAND, d2
	bsr.w tick
	bne.w failure
	moveq #0, d5
	cmpi.l #RECORD_OPERAND, d6
	bne.w operand
	cmpi.l #abi.PRVM_METADATA_OPERAND_OPTIONAL, State.Policy(a3)
	beq.w validated
	cmpi.l #abi.PRVM_METADATA_OPERAND_BIN, State.Policy(a3)
	beq.w validated
	cmpi.l #abi.PRVM_METADATA_OPERAND_NONE, State.Policy(a3)
	beq.w validated
	bra.w tokenFailure
operand
	cmpi.l #abi.PRVM_METADATA_OPERAND_NONE, State.Policy(a3)
	beq.w tokenFailure
	cmpi.l #RECORD_VALUE, d6
	blo.w tokenFailure
	cmpi.b #abi.PRVM_TOKEN_KIND_STRING, RECORD_OPERAND+abi.PackedString.Kind(a6)
	bne.w tokenFailure
	move.b RECORD_OPERAND+abi.PackedString.Length(a6), d5
	moveq #RECORD_VALUE, d2
	move.l d2, d0
	add.l d5, d0
	cmp.l d6, d0
	bne.w tokenFailure
	move.l #abi.PRVM_TOKEN_KIND_STRING, VALUE_KIND(a3)
	cmpi.l #abi.PRVM_METADATA_OPERAND_BIN, State.Policy(a3)
	beq.w binaryOperand
	cmpi.l #abi.PRVM_METADATA_OPERAND_FILL, State.Policy(a3)
	bne.w validated
	cmpi.l #2, d5
	bne.w tokenFailure
	lea 0(a6, d2.l), a0
	move.l d5, d0
	bsr.w hexValue
	bne.w tokenFailure
	move.l d1, FILL_VALUE(a3)
	bra.w validated
binaryOperand
	bsr.w binaryValue
	bne.w tokenFailure
validated
	bsr.w tick
	bne.w failure
	move.l #abi.OP_PUBLISH, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	cmpi.l #abi.PRVM_RESULT_RECORD_SIZE, abi.PRVM_FRAME_RESULT_CAPACITY(a4)
	blo.w overflow
	move.l abi.PRVM_FRAME_RESULT_PTR(a4), d0
	beq.w argumentFailure
	btst #0, d0
	bne.w argumentFailure
	bsr.w tick
	bne.w failure
	clr.l d7  ; abi.OP_END
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	move.w #abi.PRVM_RESULT_PACKED_METADATA, State.Stage(a3)
	move.l d2, VALUE_OFFSET(a3)
	move.l d5, VALUE_BYTES(a3)
	lea State.Stage(a3), a0
	movea.l abi.PRVM_FRAME_RESULT_PTR(a4), a1
	moveq #abi.PRVM_RESULT_RECORD_SIZE/4-1, d0
publish
	move.l (a0)+, (a1)+
	dbra d0, publish
	moveq #1, d1
	moveq #abi.PRVM_RESULT_RECORD_SIZE, d3
	clr.l d2
	moveq #0, d0
	bra.w finish
noMatch
	clr.l d2
	moveq #0, d0
	bra.w failure
rowFailure
	clr.l d2
	moveq #abi.PRVM_STATUS_INVALID_PROGRAM, d0
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
	.include "metadata_program.i"

; A0=literal,D0=width. D0/CCR=status,D1=u32; clobbers D3/A0.
; Callers enforce their operation's width before decoding.
hexValue	.block
	moveq #0, d1
	tst.l d0
	beq.w badHex
hexDigit
	moveq #0, d3
	move.b (a0)+, d3
	cmpi.b #'0', d3
	blo.w badHex
	cmpi.b #'9', d3
	bls.w decimal
	ori.b #$20, d3
	cmpi.b #'a', d3
	blo.w badHex
	cmpi.b #'f', d3
	bhi.w badHex
	subi.b #'a'-10, d3
	bra.w accumulate
decimal
	subi.b #'0', d3
accumulate
	lsl.l #4, d1
	or.l d3, d1
	subq.l #1, d0
	bne.w hexDigit
	moveq #0, d0
	rts
badHex
	moveq #1, d0
	rts
	.bend  ; hexValue

; D2/D5=decoded literal offset/width. Decode the last two colon components;
; preceding colons belong to the name, matching the shared Rust output model.
; D0/CCR=status, D5=name width; clobbers D1/D3/D4/D7/A0/A1/A2.
binaryValue	.block
	lea 0(a6, d2.l), a2
	moveq #-1, d4  ; final separator
	moveq #-1, d7  ; penultimate separator
	move.l d5, d0
scan
	tst.l d0
	beq.w scanned
	subq.l #1, d0
	cmpi.b #':', 0(a2, d0.l)
	bne.w scan
	tst.l d4
	bpl.w second
	move.l d0, d4
	bra.w scan
second
	move.l d0, d7
scanned
	tst.l d4
	bmi.w binaryGood
	move.l d7, d0
	addq.l #1, d0
	lea 0(a2, d0.l), a0
	move.l d2, d1
	add.l d0, d1
	move.w d1, RANGE_TEXT_OFFSET(a3)
	move.l d4, d1
	sub.l d0, d1
	cmpi.l #4, d1
	blo.w binaryBad
	cmpi.l #8, d1
	bhi.w binaryBad
	move.w d1, RANGE_TEXT_BYTES(a3)
	move.l d1, d0
	bsr.w hexValue
	bne.w binaryBad
	move.l d1, RANGE_START(a3)
	move.l d4, d0
	addq.l #1, d0
	lea 0(a2, d0.l), a0
	move.l d5, d1
	sub.l d0, d1
	cmpi.l #4, d1
	blo.w binaryBad
	cmpi.l #8, d1
	bhi.w binaryBad
	move.l d1, d0
	bsr.w hexValue
	bne.w binaryBad
	move.l d1, RANGE_END(a3)
	cmp.l RANGE_START(a3), d1
	blo.w binaryBad
	moveq #0, d5
	tst.l d7
	bmi.w binaryGood
	move.l d7, d5
binaryGood
	moveq #0, d0
	rts
binaryBad
	moveq #1, d0
	rts
	.bend  ; binaryValue

; One unit per selector envelope/head lookup/operand/publication/end operation.
; Successful match needs five units; inactive records need one, unrelated two.
; Literal scans stay within the validated <=256-byte record; no per-byte ticks.
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

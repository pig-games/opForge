; Shared scalar declaration spans over immutable compact records.
; No source spelling or expression evaluation enters this service.
; @opforge-owner: opforge.vm.prvm.packed_declaration
	.module opforge.vm.prvm.packed_declaration
	.cpu 68020
	.use opforge.vm.prvm.abi as abi
	.include "telemetry_macros.i"
	.pub
State	.struct
Budget	.long ?
Stage	.res 32
	.endstruct
STATE_BYTES = State.Stage+32
ROLE = State.Stage+abi.PRVM_DECLARATION_ROLE
VALUE_OFFSET = State.Stage+abi.PRVM_DECLARATION_VALUE_OFFSET
VALUE_BYTES = State.Stage+abi.PRVM_DECLARATION_VALUE_BYTES
UPDATE_ACTION = State.Stage+abi.PRVM_DECLARATION_UPDATE_ACTION
TOKEN_NAME_LAST = 1
TOKEN_EQUALS = abi.PRVM_TOKEN_KIND_OP_EQ
ACTION_REPEAT = 17
ASSIGNMENT_PREFIX_BYTES = abi.PackedLabel.Qualifier+1
REPEAT_BYTES = 5
REPEAT_NAME_HIGH = 1
REPEAT_NAME_LOW = 2
REPEAT_QUALIFIER = 3
REPEAT_OPERATOR = 4
PLAN_REPEAT_NAME = 11
PLAN_OPERATOR_ROWS = 13
PLAN_RECIPE_COUNT = 23
PLAN_RECIPES = 24
PLAN_RECIPE_BYTES = 5
PLAN_RECIPE_ACTION = 0
PLAN_RECIPE_COUNT_FIELD = 1
PLAN_RECIPE_TOKENS = 2
PLAN_BYTES = 116
PLAN_MUTABLE_ROLE = 14
PLAN_MUTABLE_TOKENS = 16
PLAN_CONDITIONAL_ROLE = 18
PLAN_CONDITIONAL_TOKENS = 20

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
	cmpi.w #abi.PRVM_ENTRY_KIND_PACKED_DECLARATION, abi.PRVM_FRAME_ENTRY_KIND(a4)
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
	movea.l a5, a0
	move.l abi.PRVM_FRAME_PROGRAM_LEN(a4), d0
	bsr.w validateProgram
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
	move.l #$98, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	move.b 1(a6), d0
	andi.b #24, d0
	bne.w noMatch
	cmpi.l #9, d6
	blo.w noMatch
	cmpi.b #1, 4(a6)
	bhi.w noMatch
	moveq #8, d2
	lea 8(a6), a0
	move.l d6, d0
	subq.l #8, d0
	movea.l a5, a1
	bsr.w classifyUpdate
	tst.l d0
	beq.w ordinaryOperator
	move.w d1, UPDATE_ACTION(a3)
	move.l d2, d4
	moveq #8, d2
	moveq #abi.PRVM_DECLARATION_UPDATE, d0
	bra.w matched
ordinaryOperator
	moveq #8, d2
	; Ordinary declaration operators precede optional label colon handling.
	cmpi.l #10, d6
	blo.w directive
	move.b 8(a6), d0
	cmp.b PLAN_MUTABLE_TOKENS(a5), d0
	bne.w directive
	move.b 9(a6), d0
	cmp.b PLAN_MUTABLE_TOKENS+1(a5), d0
	bne.w conditionalOperator
	moveq #0, d0
	move.b PLAN_MUTABLE_ROLE(a5), d0
	moveq #2, d4
	bra.w matched
conditionalOperator
	cmpi.l #11, d6
	blo.w directive
	move.b 9(a6), d0
	cmp.b PLAN_CONDITIONAL_TOKENS+1(a5), d0
	bne.w directive
	move.b 10(a6), d0
	cmp.b PLAN_CONDITIONAL_TOKENS+2(a5), d0
	bne.w directive
	moveq #0, d0
	move.b PLAN_CONDITIONAL_ROLE(a5), d0
	moveq #3, d4
	bra.w matched
directive
	cmpi.b #5, 8(a6)
	bne.w matchHead
	addq.l #1, d2
matchHead
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
	movea.l a5, a0
	bsr.w selectIdentity
	tst.l d0
	beq.w noMatch
	moveq #5, d4
matched
	move.w d0, ROLE(a3)
	bsr.w tick
	bne.w failure
	cmpi.b #abi.PRVM_SOURCE_NAME_EXPLICIT, abi.PackedLabel.Qualifier(a6)
	bls.w operand
	moveq #abi.PackedLabel.Qualifier, d2
	bra.w tokenFailure
operand
	add.l d4, d2
	move.l d6, d5
	sub.l d2, d5
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
	move.w #abi.PRVM_RESULT_PACKED_DECLARATION, State.Stage(a3)
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
; A0=bounded numeric token bytes,D0=count,A1=validated current plan.
; D0/CCR=matched1/0,D1=action,D2=consumed width; other registers kept.
; The same package recipes select configuration records and packed updates.
classifyUpdate	.block
	movem.l d3-d6/a0-a3, -(sp)
	movea.l a0, a3
	move.l d0, d6
	cmpi.l #REPEAT_BYTES, d6
	blo.w punctuation
	cmpi.b #TOKEN_NAME_LAST, (a3)
	bhi.w punctuation
	moveq #0, d3
	move.b REPEAT_NAME_HIGH(a3), d3
	lsl.w #8, d3
	move.b REPEAT_NAME_LOW(a3), d3
	moveq #0, d4
	move.b PLAN_REPEAT_NAME(a1), d4
	lsl.w #8, d4
	move.b PLAN_REPEAT_NAME+1(a1), d4
	cmp.w d3, d4
	bne.w punctuation
	tst.b REPEAT_QUALIFIER(a3)
	bne.w punctuation
	cmpi.b #TOKEN_EQUALS, REPEAT_OPERATOR(a3)
	bne.w punctuation
	moveq #ACTION_REPEAT, d1
	moveq #REPEAT_BYTES, d2
	moveq #1, d0
	bra.w done
punctuation
	lea PLAN_RECIPES(a1), a0
	moveq #0, d5
	move.b PLAN_RECIPE_COUNT(a1), d5
	subq.w #1, d5
recipe
	moveq #0, d2
	move.b PLAN_RECIPE_COUNT_FIELD(a0), d2
	cmp.l d2, d6
	blo.w next
	lea PLAN_RECIPE_TOKENS(a0), a1
	movea.l a3, a2
	move.l d2, d3
	subq.w #1, d3
token
	move.b (a2)+, d0
	cmp.b (a1)+, d0
	bne.w next
	dbra d3, token
	moveq #0, d1
	move.b PLAN_RECIPE_ACTION(a0), d1
	moveq #1, d0
	bra.w done
next
	addq.l #PLAN_RECIPE_BYTES, a0
	dbra d5, recipe
	moveq #0, d0
	moveq #0, d1
	moveq #0, d2
done
	movem.l (sp)+, d3-d6/a0-a3
	tst.l d0
	rts
	.bend  ; classifyUpdate
; Same bounded token/plan inputs as classifyUpdate; D0/CCR=assignment1/0.
; All other registers preserved. Includes mutable and conditional plan rows.
classifyAssignment	.block
	movem.l d1-d3/a0-a2, -(sp)
	move.l d0, d3
	bsr.w classifyUpdate
	tst.l d0
	bne.w done
	cmpi.l #2, d3
	blo.w done
	move.b (a0), d1
	cmp.b PLAN_MUTABLE_TOKENS(a1), d1
	bne.w done
	move.b 1(a0), d1
	cmp.b PLAN_MUTABLE_TOKENS+1(a1), d1
	beq.w matched
	cmpi.l #3, d3
	blo.w done
	cmp.b PLAN_CONDITIONAL_TOKENS+1(a1), d1
	bne.w done
	move.b 2(a0), d1
	cmp.b PLAN_CONDITIONAL_TOKENS+2(a1), d1
	bne.w done
matched
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; classifyAssignment
; A0=readable program,D0=byte count. D0/CCR=status; other registers kept.
; Validate the current numeric declaration policy without executing source.
validateProgram	.block
	movem.l d1-d3/a0-a1/a5, -(sp)
	movea.l a0, a5
	cmpi.l #PLAN_BYTES, d0
	bne.w bad
	cmpi.b #$98, (a5)
	bne.w bad
	lea PLAN_OPERATOR_ROWS(a5), a0
	lea PolicyTail, a1
	moveq #PLAN_BYTES-PLAN_OPERATOR_ROWS-1, d1
validatePolicy
	move.b (a0)+, d0
	cmp.b (a1)+, d0
	bne.w bad
	dbra d1, validatePolicy
	cmpi.b #3, 1(a5)
	bne.w bad
	lea 2(a5), a0
	moveq #2, d1
validateRole
	cmpi.b #1, 2(a0)
	blo.w bad
	cmpi.b #2, 2(a0)
	bhi.w bad
	lea 2(a5), a1
	moveq #0, d2
	move.b (a0), d2
	lsl.w #8, d2
	move.b 1(a0), d2
validateUnique
	cmpa.l a0, a1
	beq.w nextRole
	moveq #0, d3
	move.b (a1), d3
	lsl.w #8, d3
	move.b 1(a1), d3
	cmp.w d2, d3
	beq.w bad
	addq.l #3, a1
	bra.w validateUnique
nextRole
	addq.l #3, a0
	dbra d1, validateRole
	moveq #0, d2
	move.b PLAN_REPEAT_NAME(a5), d2
	lsl.w #8, d2
	move.b PLAN_REPEAT_NAME+1(a5), d2
	lea 2(a5), a0
	moveq #2, d1
repeatUnique
	moveq #0, d3
	move.b (a0), d3
	lsl.w #8, d3
	move.b 1(a0), d3
	cmp.w d2, d3
	beq.w bad
	addq.l #3, a0
	dbra d1, repeatUnique
	moveq #abi.PRVM_STATUS_OK, d0
	bra.w done
bad
	moveq #abi.PRVM_STATUS_INVALID_PROGRAM, d0
done
	movem.l (sp)+, d1-d3/a0-a1/a5
	tst.l d0
	rts
	.bend  ; validateProgram
	.pub
; A0=packed prefix,A1=current cursor,A2=plan,D0=name ID,D1=plan bytes.
; D2=next raw TKVM kind; recognition requires an equals token.
; D0/CCR=0 contextual repeat name,1 absent; all other registers preserved.
contextKeyword	.block
	movem.l d1-d3, -(sp)
	cmpi.l #PLAN_BYTES, d1
	bne.w absent
	cmpi.l #TOKEN_EQUALS, d2
	bne.w absent
	move.l a1, d1
	sub.l a0, d1
	cmpi.l #ASSIGNMENT_PREFIX_BYTES, d1
	bne.w absent
	cmpi.b #TOKEN_NAME_LAST, abi.PackedLabel.Kind(a0)
	bhi.w absent
	moveq #0, d2
	move.b PLAN_REPEAT_NAME(a2), d2
	lsl.w #8, d2
	move.b PLAN_REPEAT_NAME+1(a2), d2
	cmp.l d2, d0
	bne.w absent
	moveq #0, d0
	bra.w done
absent
	moveq #1, d0
done
	movem.l (sp)+, d1-d3
	tst.l d0
	rts
	.bend  ; contextKeyword
; A0/A1=packed prefix/end,A2=validated plan,D1=plan bytes.
; D0/CCR=0 assignment value context,1 absent; all other registers preserved.
; Partial RHS prefixes use the same classifier as materialized declarations.
valueContext	.block
	movem.l d1-d2/a0-a2, -(sp)
	cmpi.l #PLAN_BYTES, d1
	bne.w absent
	move.l a1, d0
	sub.l a0, d0
	sub.l #ASSIGNMENT_PREFIX_BYTES, d0
	bcs.w absent
	cmpi.b #TOKEN_NAME_LAST, abi.PackedLabel.Kind(a0)
	bhi.w absent
	adda.w #ASSIGNMENT_PREFIX_BYTES, a0
	movea.l a2, a1
	bsr.w classifyAssignment
	tst.l d0
	beq.w absent
	moveq #0, d0
	bra.w done
absent
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a2
	tst.l d0
	rts
	.bend  ; valueContext
; A0=validated declaration program,D0=numeric package head identity.
; D0/CCR=immutable/mutable role,0 otherwise; other registers preserved.
; Selection is a hint only; run retains authoritative declaration grammar.
selectIdentity	.block
	movem.l d1-d3/a0, -(sp)
	move.l d0, d3
	addq.l #2, a0
	moveq #2, d2
row
	moveq #0, d1
	move.b (a0), d1
	lsl.w #8, d1
	move.b 1(a0), d1
	cmp.w d1, d3
	beq.w selected
	addq.l #3, a0
	dbra d2, row
	moveq #0, d0
	bra.w done
selected
	moveq #0, d0
	move.b 2(a0), d0
done
	movem.l (sp)+, d1-d3/a0
	tst.l d0
	rts
	.bend  ; selectIdentity
	.priv
; One budget unit per envelope, matched declaration and atomic publication.
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
PolicyTail
	.byte 2, 2, 2, 5, 34, 3, 3, 5, 9, 34, 18
	.byte 0, 2, 18, 34, 0, 1, 2, 19, 34, 0, 2, 2, 20, 34, 0
	.byte 3, 2, 22, 34, 0, 4, 2, 23, 34, 0, 5, 2, 21, 34, 0
	.byte 6, 2, 29, 34, 0, 7, 2, 30, 34, 0, 8, 2, 28, 34, 0
	.byte 9, 2, 32, 34, 0, 10, 2, 31, 34, 0, 11, 2, 24, 34, 0
	.byte 12, 2, 25, 34, 0, 13, 1, 17, 0, 0, 13, 3, 7, 7, 34
	.byte 14, 3, 39, 9, 34, 15, 3, 37, 9, 34, 16, 2, 7, 34, 0
	.byte $83, 0
	.align 2
	.endsection
	.endmodule

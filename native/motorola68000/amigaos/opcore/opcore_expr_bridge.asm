; Native scalar-expression frontend for the ExprVM runtime.
;
; This module has one cohesive responsibility: compile the established bounded
; scalar-expression text grammar into versioned ExprVM bytecode and execute that
; bytecode through exprvm.amigaos.runtime. The tkpkg expression service owns
; request envelopes and diagnostic projection; the ExprVM runtime owns bytecode
; evaluation. Literal parsing and symbol-name-to-index resolution stay here as
; compiler work and are not duplicated by either boundary.
;
; Ownership decision (Item 5.10): retain this frontend intact. Its parser,
; bytecode emitter, and private program buffer share one cursor/register ABI and
; no independent state or dependency boundary justifies a split. It remains the
; long-term owner until a package parser supplies ExprVM bytecode directly.

	.module opcore.amigaos.expr_bridge
	.cpu 68020
	.pub
	.use exprvm.amigaos.runtime
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS
	.use debug.amigaos.symbol_expr_profile as symbol_expr_profile
.endif

; Matches opasm's label-only row: 107 Rust-valid fully scoped name bytes plus
; the terminating NUL.
LABEL_NAME_CAPACITY             = 108
OPCORE_EXPRVM_PROGRAM_CAPACITY  = 128
EXVM_OPCODE_END                 = $00
EXVM_OPCODE_PARSE_EXPRESSION    = $01
EXVM_OPCODE_EMIT_DIAG           = $02
EXVM_OPCODE_FAIL                = $03

	.section code, kind=code
	.pub

; ---------------------------------------------------------------------------
; Evaluate one scalar operand expression through the native EXVM default path.
;
; opcoreExprEvalOperandV1 is the compatibility entry used by the current
; native selector/pass code. It now runs the same EXVM default program shape as
; the Rust path (`ParseExpression`, `End`) by compiling the current native
; scalar subset into ExprVM bytecode and then executing the ExprVM evaluator.
;
; Supported input forms:
; - optional immediate prefix '#'
; - '$' and '0x' prefixed hexadecimal numbers
; - '%' prefixed binary numbers
; - decimal numbers
; - unary '+' and '-'
; - additive/subtractive expressions over scalar terms
; - '$' current-address terms matching the portable VM syntax
; - '*' current-address terms accepted as compatibility syntax
; - labels resolved through the caller-supplied label tables
;
; Inputs:
; - A0/D0: operand text pointer and byte length.
; - A1: fixed-width label-name table pointer.
; - A2: label-value table pointer parallel to A1.
; - D1: number of label entries.
; - D2: current assembly PC for '$' current-address terms.
; - D4: EXVM parser opcode version.
; - D5: expression evaluator opcode version (1 or 2).
; - D6: current assembler pass number.
;
; Outputs:
; - D0: 0 on success, 1 on parse/lookup failure.
; - D3: resolved scalar value on success.
; - D4: nonzero when the evaluated program referenced at least one symbol.
; - D5: nonzero when the evaluated program referenced a symbol that is
;   unstable for the current pass.
;
; Clobbers:
; - D0/D3-D5/CCR
;
; CCR:
; - Reflects D0 on return.
; ---------------------------------------------------------------------------
opcoreExprEvalOperandV1	.block
	moveq #1, d4
	.bend  ; opcoreExprEvalOperandV1

opcoreExvmEvalOperandV1	.block
	clr.l OpcoreExvmSymbolResolverPtr
	bra.s opcoreExvmEvalOperandCommon
	.bend  ; opcoreExvmEvalOperandV1

; Evaluate with an optional CPU-neutral lexical resolver fallback.
; Inputs match opcoreExvmEvalOperandV1; A5 is a callback receiving A0/D0 token
; text and returning D0 status plus D3 value.
; @opforge-owner: opcore.amigaos.expr_bridge
; @opforge-slice: documentation/plans/slices/native-porting-slice-scalar-register-encoding-v3.toml
; @opforge-role: facade
opcoreExvmEvalOperandWithResolverV1	.block
	move.l a5, OpcoreExvmSymbolResolverPtr
	.bend  ; opcoreExvmEvalOperandWithResolverV1

	.priv
; Shared implementation for snapshot-only and lexical-context entry points.
; @opforge-owner: opcore.amigaos.expr_bridge
; @opforge-slice: documentation/plans/slices/native-porting-slice-scalar-register-encoding-v3.toml
; @opforge-role: delegation
opcoreExvmEvalOperandCommon	.block
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS
	movem.l d0-d2, -(sp)
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EVENT_REQUEST, d0
	moveq #0, d1
	moveq #0, d2
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	movem.l (sp)+, d0-d2
.endif
	move.w d4, OpcoreExvmSelectedOpcodeVersion
	movem.l d1-d2/d6-d7/a0-a5, -(sp)
	cmpi.w #2, d5
	beq.s selectedVersionReady
	moveq #1, d5

selectedVersionReady
	move.w d5, runtime.ExprvmSelectedOpcodeVersion
	clr.l d5
	clr.b OpcoreExvmSawUnresolvedSymbol
	move.w d6, runtime.ExprvmCurrentPass
	movea.l a1, a3  ; label-name table base kept stable across parse helpers
	movea.l a2, a4  ; label-value table base kept stable across parse helpers
	movea.l d2, a5  ; A5 carries the current PC for '*' terms
	move.w d1, d7  ; D7 is the label count consumed by the resolver loop
	clr.l d3
	bsr.w skipWhitespace
	beq.w fail
	cmpi.b #'#', (a0)
	bne.s operandPrefixDone
	addq.l #1, a0  ; strip immediate marker; addressing mode was selected elsewhere
	subq.l #1, d0  ; keep remaining text length aligned with A0
	bsr.w skipWhitespace

operandPrefixDone
	tst.l d0
	beq.w fail
	move.l d0, -(sp)
	bsr.w selectProgram
	move.l d0, d2
	move.l (sp)+, d0
	tst.l d2
	bne.w fail
	bsr.w runEvalProgram
	tst.l d0
	bne.w return
	tst.b OpcoreExvmSawUnresolvedSymbol
	beq.w return
	moveq #1, d4
	moveq #1, d5
	bra.w return

fail
	moveq #1, d0

return
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS
	movem.l d0-d4, -(sp)
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EVENT_FAILURE, d4
	tst.l d0
	bne.s expressionOutcomeReady
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EVENT_SUCCESS, d4
expressionOutcomeReady
	move.w d4, d0
	moveq #0, d1
	moveq #0, d2
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	movem.l (sp)+, d0-d4
	tst.l d0  ; restore the public CCR-reflects-D0 result contract
.endif
	movem.l (sp)+, d1-d2/d6-d7/a0-a5
	rts
	.bend  ; opcoreExvmEvalOperandCommon

selectProgram	.block
	moveq #0, d0
	move.w OpcoreExvmSelectedOpcodeVersion, d6
	cmpi.w #1, d6
	beq.s version1
	moveq #1, d0
	rts

version1
	lea OpcoreExvmDefaultProgram(PC), a1
	moveq #OPCORE_EXVM_DEFAULT_PROGRAM_LEN, d1
	rts
	.bend  ; selectProgram

runEvalProgram	.block
	moveq #0, d4  ; output expression count, matching Rust's stack-depth 1 contract

loop
	tst.l d1
	beq.w missingEnd
	moveq #0, d6
	move.b (a1)+, d6
	subq.l #1, d1
	cmpi.b #EXVM_OPCODE_END, d6
	beq.w opcodeEnd
	cmpi.b #EXVM_OPCODE_PARSE_EXPRESSION, d6
	beq.w opcodeParseExpression
	cmpi.b #EXVM_OPCODE_EMIT_DIAG, d6
	beq.w programFail
	cmpi.b #EXVM_OPCODE_FAIL, d6
	beq.w programFail
	bra.w programFail

opcodeParseExpression
	tst.l d4
	bne.w programFail
	movem.l d1/d4/a1, -(sp)
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS
	movem.l d0-d2, -(sp)
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EVENT_PARSE, d0
	moveq #0, d1
	moveq #0, d2
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EVENT_COMPILE, d0
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	movem.l (sp)+, d0-d2
.endif
	bsr.w compileExpression
	move.l d0, d2
	beq.s compileOk
	cmpi.l #2, d2
	bhs.s restore
	moveq #3, d2
	bra.s restore

compileOk
	bsr.w finalizeProgram
	move.l d0, d2
	beq.s finalizeOk
	moveq #4, d2
	bra.s restore

finalizeOk
	moveq #runtime.EXPRVM_OPCODE_END, d6
	bsr.w emitU8D6
	move.l d0, d2
	beq.s ensureEndOk
	moveq #4, d2
	bra.s restore

ensureEndOk
	lea OpcoreExprVmProgramBuffer, a0
	moveq #0, d0
	move.w OpcoreExprVmProgramLen, d0
	movea.l a3, a1
	movea.l a4, a2
	move.l d7, d1
	move.l a5, d2
	moveq #0, d6
	move.w runtime.ExprvmSelectedOpcodeVersion, d6
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS
	movem.l d0-d2, -(sp)
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EVENT_EVALUATE, d0
	moveq #0, d1
	moveq #0, d2
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	movem.l (sp)+, d0-d2
.endif
	jsr runtime.exprvmEvalProgramV1
	move.l d0, d2
	beq.s restore
	cmpi.l #51, d2
	bhs.s restore
	moveq #5, d2

restore
	movem.l (sp)+, d1/d4/a1
	tst.l d2
	bne.w failWithCode
	addq.l #1, d4
	bra.w loop

opcodeEnd
	cmpi.l #1, d4
	bne.s programFail
	moveq #0, d0
	rts

missingEnd
programFail
	moveq #1, d0
	rts

failWithCode
	move.l d2, d0
	rts
	.bend  ; runEvalProgram

compileExpression	.block
	bsr.w resetProgram
	bsr.w compileHighLow
	move.l d5, d0
	rts
	.bend  ; compileExpression

compileHighLow
	bsr.w skipWhitespace
	beq.s highLowFail
	cmpi.b #'<', (a0)
	beq.s low
	cmpi.b #'>', (a0)
	beq.s high
	bra.w compileTernary

low
	moveq #runtime.EXPRVM_UNARY_LOW, d6
	bra.s highLowOperator

high
	moveq #runtime.EXPRVM_UNARY_HIGH, d6

highLowOperator
	move.l d6, -(sp)
	addq.l #1, a0
	subq.l #1, d0
	bsr.w compileHighLow
	move.l (sp)+, d6
	tst.l d5
	bne.s highLowReturn
	move.l d0, -(sp)
	bsr.w emitApplyUnaryD6
	move.l d0, d5
	move.l (sp)+, d0

highLowReturn
	rts

highLowFail
	moveq #1, d5
	rts

compileTernary	.block
	bsr.w compileLogicalOr
	tst.l d5
	bne.w fail
	bsr.w skipWhitespace
	beq.w ok
	cmpi.b #'?', (a0)
	bne.w ok
	addq.l #1, a0
	subq.l #1, d0
	bsr.w compileHighLow
	tst.l d5
	bne.w fail
	bsr.w skipWhitespace
	beq.w missingSeparator
	cmpi.b #':', (a0)
	bne.w missingSeparator
	addq.l #1, a0
	subq.l #1, d0
	bsr.w compileHighLow
	tst.l d5
	bne.w fail
	moveq #runtime.EXPRVM_TERNARY_SELECT, d6
	move.l d0, -(sp)
	bsr.w emitApplyBinaryD6
	move.l d0, d5
	move.l (sp)+, d0
	tst.l d5
	bne.w fail

ok
	moveq #0, d5
	rts

missingSeparator
	moveq #33, d5
	rts

fail
	rts
	.bend  ; compileTernary

compileLogicalOr	.block
	bsr.w logicalAnd
	tst.l d5
	bne.w fail

loop
	bsr.w skipWhitespace
	cmpi.l #2, d0
	bcs.w ok
	cmpi.b #'|', (a0)
	beq.s logicalOr
	cmpi.b #'^', (a0)
	beq.s logicalXor
	bra.w ok

logicalOr
	cmpi.b #'|', 1(a0)
	bne.w ok
	moveq #runtime.EXPRVM_BINARY_LOGIC_OR, d6
	bra.s operator

logicalXor
	cmpi.b #'^', 1(a0)
	bne.w ok
	moveq #runtime.EXPRVM_BINARY_LOGIC_XOR, d6

operator
	move.l d6, -(sp)
	addq.l #2, a0
	subq.l #2, d0
	bsr.w logicalAnd
	move.l (sp)+, d6
	tst.l d5
	bne.w fail
	move.l d0, -(sp)
	bsr.w emitApplyBinaryD6
	move.l d0, d5
	move.l (sp)+, d0
	tst.l d5
	bne.w fail
	bra.w loop

ok
	moveq #0, d5
	rts

fail
	rts

logicalAnd
	bsr.w bitOr
	tst.l d5
	bne.w logicalAndFail

logicalAndLoop
	bsr.w skipWhitespace
	cmpi.l #2, d0
	bcs.w logicalAndOk
	cmpi.b #'&', (a0)
	bne.w logicalAndOk
	cmpi.b #'&', 1(a0)
	bne.w logicalAndOk
	addq.l #2, a0
	subq.l #2, d0
	bsr.w bitOr
	tst.l d5
	bne.w logicalAndFail
	moveq #runtime.EXPRVM_BINARY_LOGIC_AND, d6
	move.l d0, -(sp)
	bsr.w emitApplyBinaryD6
	move.l d0, d5
	move.l (sp)+, d0
	tst.l d5
	bne.w logicalAndFail
	bra.w logicalAndLoop

logicalAndOk
	moveq #0, d5
	rts

logicalAndFail
	tst.l d5
	bne.s logicalAndReturn
	moveq #1, d5

logicalAndReturn
	rts

bitOr
	bsr.w bitXor
	tst.l d5
	bne.w bitOrFail

bitOrLoop
	bsr.w skipWhitespace
	beq.w bitOrOk
	cmpi.b #'|', (a0)
	bne.w bitOrOk
	cmpi.l #2, d0
	bcs.s bitOrApply
	cmpi.b #'|', 1(a0)
	beq.w bitOrOk

bitOrApply
	addq.l #1, a0
	subq.l #1, d0
	bsr.w bitXor
	tst.l d5
	bne.w bitOrFail
	moveq #runtime.EXPRVM_BINARY_BIT_OR, d6
	move.l d0, -(sp)
	bsr.w emitApplyBinaryD6
	move.l d0, d5
	move.l (sp)+, d0
	tst.l d5
	bne.w bitOrFail
	bra.w bitOrLoop

bitOrOk
	moveq #0, d5
	rts

bitOrFail
	tst.l d5
	bne.s bitOrReturn
	moveq #1, d5

bitOrReturn
	rts

bitXor
	bsr.w bitAnd
	tst.l d5
	bne.w bitXorFail

bitXorLoop
	bsr.w skipWhitespace
	beq.w bitXorOk
	cmpi.b #'^', (a0)
	bne.w bitXorOk
	cmpi.l #2, d0
	bcs.s bitXorApply
	cmpi.b #'^', 1(a0)
	beq.w bitXorOk

bitXorApply
	addq.l #1, a0
	subq.l #1, d0
	bsr.w bitAnd
	tst.l d5
	bne.w bitXorFail
	moveq #runtime.EXPRVM_BINARY_BIT_XOR, d6
	move.l d0, -(sp)
	bsr.w emitApplyBinaryD6
	move.l d0, d5
	move.l (sp)+, d0
	tst.l d5
	bne.w bitXorFail
	bra.w bitXorLoop

bitXorOk
	moveq #0, d5
	rts

bitXorFail
	tst.l d5
	bne.s bitXorReturn
	moveq #1, d5

bitXorReturn
	rts

bitAnd
	bsr.w compare
	tst.l d5
	bne.w bitAndFail

bitAndLoop
	bsr.w skipWhitespace
	beq.w bitAndOk
	cmpi.b #'&', (a0)
	bne.w bitAndOk
	cmpi.l #2, d0
	bcs.s bitAndApply
	cmpi.b #'&', 1(a0)
	beq.w bitAndOk

bitAndApply
	addq.l #1, a0
	subq.l #1, d0
	bsr.w compare
	tst.l d5
	bne.w bitAndFail
	moveq #runtime.EXPRVM_BINARY_BIT_AND, d6
	move.l d0, -(sp)
	bsr.w emitApplyBinaryD6
	move.l d0, d5
	move.l (sp)+, d0
	tst.l d5
	bne.w bitAndFail
	bra.w bitAndLoop

bitAndOk
	moveq #0, d5
	rts

bitAndFail
	tst.l d5
	bne.s bitAndReturn
	moveq #1, d5

bitAndReturn
	rts

compare
	bsr.w compileShift
	tst.l d5
	bne.w compareFail

compareLoop
	bsr.w skipWhitespace
	beq.w compareOk
	cmpi.b #'=', (a0)
	beq.w compareEq
	cmpi.b #'!', (a0)
	beq.w compareNe
	cmpi.b #'<', (a0)
	beq.w compareLt
	cmpi.b #'>', (a0)
	beq.w compareGt
	bra.w compareOk

compareEq
	cmpi.l #2, d0
	bcs.w compareOk
	cmpi.b #'=', 1(a0)
	bne.w compareOk
	moveq #runtime.EXPRVM_BINARY_EQ, d6
	moveq #2, d4
	bra.w compareApply

compareNe
	cmpi.l #2, d0
	bcs.w compareOk
	cmpi.b #'=', 1(a0)
	bne.w compareOk
	moveq #runtime.EXPRVM_BINARY_NE, d6
	moveq #2, d4
	bra.w compareApply

compareLt
	moveq #runtime.EXPRVM_BINARY_LT, d6
	moveq #1, d4
	cmpi.l #2, d0
	bcs.w compareApply
	cmpi.b #'=', 1(a0)
	beq.s compareLe
	cmpi.b #'>', 1(a0)
	beq.s compareNeAlt
	cmpi.b #'<', 1(a0)
	beq.w compareOk
	bra.w compareApply

compareLe
	moveq #runtime.EXPRVM_BINARY_LE, d6
	moveq #2, d4
	bra.w compareApply

compareNeAlt
	moveq #runtime.EXPRVM_BINARY_NE, d6
	moveq #2, d4
	bra.w compareApply

compareGt
	moveq #runtime.EXPRVM_BINARY_GT, d6
	moveq #1, d4
	cmpi.l #2, d0
	bcs.w compareApply
	cmpi.b #'=', 1(a0)
	beq.s compareGe
	cmpi.b #'>', 1(a0)
	beq.w compareOk
	bra.w compareApply

compareGe
	moveq #runtime.EXPRVM_BINARY_GE, d6
	moveq #2, d4

compareApply
	adda.l d4, a0
	sub.l d4, d0
	move.l d6, -(sp)
	bsr.w compileShift
	move.l (sp)+, d6
	tst.l d5
	bne.w compareFail
	move.l d0, -(sp)
	bsr.w emitApplyBinaryD6
	move.l d0, d5
	move.l (sp)+, d0
	tst.l d5
	bne.w compareFail
	bra.w compareLoop

compareOk
	moveq #0, d5
	rts

compareFail
	tst.l d5
	bne.s compareReturn
	moveq #1, d5

compareReturn
	rts
	.bend  ; compileLogicalOr

compileAdditive	.block
	bsr.w compileMultiplicative
	tst.l d5
	bne.w fail

loop
	bsr.w skipWhitespace
	beq.w ok
	moveq #0, d6
	move.b (a0), d6
	cmpi.b #'+', d6
	beq.w operator
	cmpi.b #'-', d6
	beq.w operator
	cmpi.b #'|', d6
	beq.w ok
	cmpi.b #'?', d6
	beq.w ok
	cmpi.b #':', d6
	beq.w ok
	cmpi.b #')', d6
	beq.w ok
	cmpi.b #'<', d6
	beq.w ok
	cmpi.b #'>', d6
	beq.w ok
	cmpi.b #'=', d6
	beq.w ok
	cmpi.b #'!', d6
	beq.w ok
	cmpi.b #'&', d6
	beq.w ok
	cmpi.b #'^', d6
	beq.w ok
	bra.w trailingFail

operator
	move.l d6, -(sp)
	addq.l #1, a0
	subq.l #1, d0
	bsr.w compileMultiplicative
	move.l (sp)+, d6
	tst.l d5
	bne.w fail
	cmpi.b #'+', d6
	beq.w add
	moveq #runtime.EXPRVM_BINARY_SUBTRACT, d6
	move.l d0, -(sp)
	bsr.w emitApplyBinaryD6
	move.l d0, d5
	move.l (sp)+, d0
	tst.l d5
	bne.w fail
	bra.w loop

add
	moveq #runtime.EXPRVM_BINARY_ADD, d6
	move.l d0, -(sp)
	bsr.w emitApplyBinaryD6
	move.l d0, d5
	move.l (sp)+, d0
	tst.l d5
	bne.w fail
	bra.w loop

trailingFail
	moveq #33, d5
	bra.w fail

ok
	moveq #0, d5
	rts

fail
	tst.l d5
	bne.w failReady
	moveq #1, d5

failReady
	rts
	.bend  ; compileAdditive

compileShift	.block
	bsr.w compileAdditive
	tst.l d5
	bne.w fail

loop
	bsr.w skipWhitespace
	cmpi.l #2, d0
	bcs.w ok
	cmpi.b #'<', (a0)
	beq.s left
	cmpi.b #'>', (a0)
	beq.s right
	bra.w ok

left
	cmpi.b #'<', 1(a0)
	bne.w ok
	moveq #runtime.EXPRVM_BINARY_SHIFT_LEFT, d6
	bra.s operator

right
	cmpi.b #'>', 1(a0)
	bne.w ok
	moveq #runtime.EXPRVM_BINARY_SHIFT_RIGHT, d6

operator
	move.l d6, -(sp)
	addq.l #2, a0
	subq.l #2, d0
	bsr.w compileAdditive
	move.l (sp)+, d6
	tst.l d5
	bne.w fail
	move.l d0, -(sp)
	bsr.w emitApplyBinaryD6
	move.l d0, d5
	move.l (sp)+, d0
	tst.l d5
	bne.w fail
	bra.w loop

ok
	moveq #0, d5
	rts

fail
	tst.l d5
	bne.s return
	moveq #1, d5

return
	rts
	.bend  ; compileShift

compileMultiplicative	.block
	bsr.w power
	tst.l d5
	bne.w fail

loop
	bsr.w skipWhitespace
	beq.w ok
	moveq #0, d6
	move.b (a0), d6
	cmpi.b #'*', d6
	beq.s operator
	cmpi.b #'/', d6
	beq.s operator
	cmpi.b #'%', d6
	beq.s operator
	bra.w ok

operator
	move.l d6, -(sp)
	addq.l #1, a0
	subq.l #1, d0
	bsr.w power
	move.l (sp)+, d6
	tst.l d5
	bne.w fail
	cmpi.b #'*', d6
	beq.s multiply
	cmpi.b #'/', d6
	beq.s divide
	moveq #runtime.EXPRVM_BINARY_MOD, d6
	bra.s apply

multiply
	moveq #runtime.EXPRVM_BINARY_MULTIPLY, d6
	bra.s apply

divide
	moveq #runtime.EXPRVM_BINARY_DIVIDE, d6

apply
	move.l d0, -(sp)
	bsr.w emitApplyBinaryD6
	move.l d0, d5
	move.l (sp)+, d0
	tst.l d5
	bne.w fail
	bra.w loop

ok
	moveq #0, d5
	rts

fail
	tst.l d5
	bne.s return
	moveq #1, d5

return
	rts

power
	bsr.w compileSingleTerm
	tst.l d5
	bne.w powerFail
	bsr.w skipWhitespace
	cmpi.l #2, d0
	bcs.s powerOk
	cmpi.b #'*', (a0)
	bne.s powerOk
	cmpi.b #'*', 1(a0)
	bne.s powerOk
	addq.l #2, a0
	subq.l #2, d0
	bsr.w power
	tst.l d5
	bne.s powerFail
	moveq #runtime.EXPRVM_BINARY_POWER, d6
	move.l d0, -(sp)
	bsr.w emitApplyBinaryD6
	move.l d0, d5
	move.l (sp)+, d0
	tst.l d5
	bne.s powerFail

powerOk
	moveq #0, d5
	rts

powerFail
	tst.l d5
	bne.s powerReturn
	moveq #1, d5

powerReturn
	rts
	.bend  ; compileMultiplicative

compileSingleTerm	.block
	movem.l d4, -(sp)
	moveq #0, d4
	moveq #0, d5
	bsr.w skipWhitespace
	beq.w fail
	cmpi.b #'+', (a0)
	beq.w unaryPlus
	cmpi.b #'-', (a0)
	beq.w unaryMinus
	cmpi.b #'~', (a0)
	beq.w unaryBitNot
	cmpi.b #'!', (a0)
	beq.w unaryLogicNot

body
	tst.l d0
	beq.w fail
	cmpi.b #'(', (a0)
	bne.s notParenthesized
	bra.w parenthesized

notParenthesized
	cmpi.b #'*', (a0)
	beq.w currentPc
	cmpi.b #39, (a0)
	beq.w stringLiteral
	cmpi.b #'"', (a0)
	beq.w stringLiteral
	cmpi.b #'$', (a0)
	beq.w dollar
	cmpi.b #'%', (a0)
	beq.w binaryLiteral
	cmpi.b #'0', (a0)
	bne.w numberOrLabel
	cmpi.l #2, d0
	bcs.w numberOrLabel
	cmpi.b #'x', 1(a0)
	beq.w hex0x
	cmpi.b #'X', 1(a0)
	beq.w hex0x

numberOrLabel
	moveq #0, d1
	move.b (a0), d1
	cmpi.b #'0', d1
	bcs.w label
	cmpi.b #'9', d1
	bhi.w label
	bra.w decimal

unaryPlus
	moveq #runtime.EXPRVM_UNARY_PLUS + 1, d4
	bra.s unaryOperator

unaryMinus
	moveq #runtime.EXPRVM_UNARY_MINUS + 1, d4
	bra.s unaryOperator

unaryBitNot
	moveq #runtime.EXPRVM_UNARY_BIT_NOT + 1, d4
	bra.s unaryOperator

unaryLogicNot
	moveq #runtime.EXPRVM_UNARY_LOGIC_NOT + 1, d4

unaryOperator
	addq.l #1, a0
	subq.l #1, d0
	bsr.w compileSingleTerm
	bra.w maybeApplyUnary

parenthesized
	addq.l #1, a0
	subq.l #1, d0
	move.l d4, -(sp)
	bsr.w compileHighLow
	move.l (sp)+, d4
	tst.l d5
	bne.w return
	bsr.w skipWhitespace
	beq.w fail
	cmpi.b #')', (a0)
	bne.w fail
	addq.l #1, a0
	subq.l #1, d0
	bra.w maybeApplyUnary

currentPc
	addq.l #1, a0
	subq.l #1, d0
	move.l d0, -(sp)
	bsr.w emitPushCurrent
	move.l d0, d5
	move.l (sp)+, d0
	bra.w maybeApplyUnary

dollar
	cmpi.l #1, d0
	beq.w currentPc
	moveq #0, d1
	move.b 1(a0), d1
	cmpi.b #'0', d1
	blo.w dollarUpperHex
	cmpi.b #'9', d1
	bls.w hex

dollarUpperHex
	cmpi.b #'A', d1
	blo.w dollarLowerHex
	cmpi.b #'F', d1
	bls.w hex

dollarLowerHex
	cmpi.b #'a', d1
	blo.w currentPc
	cmpi.b #'f', d1
	bls.w hex
	bra.w currentPc

hex
	addq.l #1, a0
	subq.l #1, d0
	bsr.w parseHex
	tst.l d5
	beq.s hexParsed
	moveq #31, d5
	bra.w maybeApplyUnary

hexParsed
	move.l d0, -(sp)
	bsr.w emitPushLiteralD3
	move.l d0, d5
	move.l (sp)+, d0
	tst.l d5
	beq.s hexEmitOk
	moveq #32, d5
	bra.w maybeApplyUnary

hexEmitOk
	moveq #0, d5
	bra.w maybeApplyUnary

hex0x
	addq.l #2, a0
	subq.l #2, d0
	bsr.w parseHex
	tst.l d5
	bne.w maybeApplyUnary
	move.l d0, -(sp)
	bsr.w emitPushLiteralD3
	move.l d0, d5
	move.l (sp)+, d0
	bra.w maybeApplyUnary

binaryLiteral
	addq.l #1, a0
	subq.l #1, d0
	bsr.w parseBinary
	tst.l d5
	bne.w maybeApplyUnary
	move.l d0, -(sp)
	bsr.w emitPushLiteralD3
	move.l d0, d5
	move.l (sp)+, d0
	bra.w maybeApplyUnary

stringLiteral
	moveq #0, d6
	move.b (a0)+, d6
	subq.l #1, d0
	clr.l d3
	clr.l d2

stringScan
	tst.l d0
	beq.w stringFail
	moveq #0, d1
	move.b (a0)+, d1
	subq.l #1, d0
	cmp.b d6, d1
	beq.w stringClose
	cmpi.b #92, d1
	bne.w stringAppend
	tst.l d0
	beq.w stringFail
	moveq #0, d1
	move.b (a0)+, d1
	subq.l #1, d0
	cmpi.b #'n', d1
	beq.w stringEscapedNewline
	cmpi.b #'r', d1
	beq.w stringEscapedReturn
	cmpi.b #'t', d1
	beq.w stringEscapedTab
	cmpi.b #'0', d1
	beq.w stringEscapedZero
	cmpi.b #'x', d1
	beq.w stringEscapedHex
	bra.w stringAppend

stringEscapedNewline
	moveq #10, d1
	bra.w stringAppend

stringEscapedReturn
	moveq #13, d1
	bra.w stringAppend

stringEscapedTab
	moveq #9, d1
	bra.w stringAppend

stringEscapedZero
	moveq #0, d1
	bra.w stringAppend

stringEscapedHex
	cmpi.l #2, d0
	bcs.w stringFail
	move.l d0, -(sp)
	move.l d3, -(sp)
	moveq #2, d0
	bsr.w parseHex
	move.l d3, d1
	move.l (sp)+, d3
	move.l (sp)+, d0
	tst.l d5
	bne.w stringFail
	subq.l #2, d0

stringAppend
	cmpi.l #2, d2
	bhs.w stringFail
	lsl.l #8, d3
	or.l d1, d3
	addq.l #1, d2
	bra.w stringScan

stringClose
	tst.l d2
	beq.w stringFail
	moveq #0, d5
	move.l d0, -(sp)
	bsr.w emitPushLiteralD3
	move.l d0, d5
	move.l (sp)+, d0
	bra.w maybeApplyUnary

stringFail
	moveq #1, d5
	bra.w maybeApplyUnary

decimal
	bsr.w parseSuffixedNumber
	tst.l d5
	bne.w maybeApplyUnary
	move.l d0, -(sp)
	bsr.w emitPushLiteralD3
	move.l d0, d5
	move.l (sp)+, d0
	bra.w maybeApplyUnary

label
	move.l d0, d6
	bsr.w termLength
	move.l d0, d2
	; Rust resolves through the active lexical evaluation context before the
	; broad immutable snapshot.  Preserve that ordering when a native caller
	; supplied the neutral resolver callback.
	movea.l OpcoreExvmSymbolResolverPtr, a1
	move.l a1, d5
	beq.s labelSnapshot
	movem.l d2/d6/a0, -(sp)
	move.l d2, d0
	jsr (a1)
	movem.l (sp)+, d2/d6/a0
	tst.l d0
	bne.s labelSnapshot
	move.l d0, -(sp)
	bsr.w emitPushLiteralD3
	move.l d0, d5
	move.l (sp)+, d0
	bne.w maybeApplyUnary
	adda.l d2, a0
	move.l d6, d0
	sub.l d2, d0
	bra.w maybeApplyUnary

labelSnapshot
	; The lexical resolver returns its status in D0.  Snapshot fallback still
	; consumes the complete parsed term, matching Rust's lookup order, so restore
	; the term length retained in D2 before scanning the immutable table.
	move.l d2, d0
	bsr.w resolveLabelIndex
	move.l d0, d5
	beq.s labelResolved
	moveq #0, d3
	move.w runtime.ExprvmCurrentPass, d3
	cmpi.w #1, d3
	bne.w maybeApplyUnary
	move.b #1, OpcoreExvmSawUnresolvedSymbol
	clr.l d3
	bsr.w emitPushLiteralD3
	move.l d0, d5
	bne.w maybeApplyUnary
	adda.l d2, a0
	move.l d6, d0
	sub.l d2, d0
	bra.w maybeApplyUnary

labelResolved
	bsr.w emitPushSymbolD3
	move.l d0, d5
	bne.w maybeApplyUnary
	adda.l d2, a0
	move.l d6, d0
	sub.l d2, d0

maybeApplyUnary
	tst.l d5
	bne.w return
	tst.l d4
	beq.s ok
	move.l d4, d6
	subq.l #1, d6
	move.l d0, -(sp)
	bsr.w emitApplyUnaryD6
	move.l d0, d5
	move.l (sp)+, d0
	bne.w return

ok
	moveq #0, d5
	bra.s return

fail
	moveq #34, d5

return
	movem.l (sp)+, d4
	rts
	.bend  ; compileSingleTerm
resetProgram	.block
	clr.w OpcoreExprVmProgramLen
	rts
	.bend  ; resetProgram

; Append the final ExprVM end/require-scalar sequence for the selected opcode version.
; Inputs: runtime.ExprvmSelectedOpcodeVersion = evaluator opcode version.
; Outputs: D0 = 0 on success or 1 on program-buffer overflow.
; Clobbers: D3/D6/CCR.
; CCR: reflects D0 on return.
finalizeProgram	.block
	move.w runtime.ExprvmSelectedOpcodeVersion, d3
	cmpi.w #2, d3
	bne.s version1
	moveq #runtime.EXPRVM_V2_OPCODE_REQUIRE_SCALAR, d6
	bsr.w emitU8D6
	bne.s return
	moveq #runtime.EXPRVM_V2_OPCODE_END, d6
	bra.s return

version1
	moveq #runtime.EXPRVM_OPCODE_END, d6

return
	bsr.w emitU8D6
	rts
	.bend  ; finalizeProgram

; Append the selected-version PushCurrentAddress opcode.
; Inputs: runtime.ExprvmSelectedOpcodeVersion = evaluator opcode version.
; Outputs: D0 = 0 on success or 1 on program-buffer overflow.
; Clobbers: D3/D6/CCR.
; CCR: reflects D0 on return.
emitPushCurrent	.block
	move.w runtime.ExprvmSelectedOpcodeVersion, d3
	cmpi.w #2, d3
	bne.w version1
	moveq #runtime.EXPRVM_V2_OPCODE_PUSH_CURRENT_ADDR, d6
	bra.w ready

version1
	moveq #runtime.EXPRVM_OPCODE_PUSH_CURRENT_ADDR, d6

ready
	bra.w emitU8D6
	.bend  ; emitPushCurrent

; Append the selected-version unary opcode plus operand kind from D6.
; Inputs: D6 = ExprVM unary operator id; runtime.ExprvmSelectedOpcodeVersion =
; evaluator opcode version.
; Outputs: D0 = 0 on success or 1 on program-buffer overflow.
; Clobbers: D3/CCR.
; CCR: reflects D0 on return.
emitApplyUnaryD6	.block
	movem.l d6, -(sp)
	move.l d6, d3
	move.w runtime.ExprvmSelectedOpcodeVersion, d6
	cmpi.w #2, d6
	bne.s version1
	moveq #runtime.EXPRVM_V2_OPCODE_APPLY_UNARY, d6
	bra.s ready

version1
	moveq #runtime.EXPRVM_OPCODE_APPLY_UNARY, d6

ready
	bsr.w emitU8D6
	bne.s return
	move.l d3, d6
	bsr.w emitU8D6

return
	movem.l (sp)+, d6
	rts
	.bend  ; emitApplyUnaryD6

; Append the selected-version binary opcode plus operand kind from D6.
; Inputs: D6 = ExprVM binary operator id; runtime.ExprvmSelectedOpcodeVersion =
; evaluator opcode version.
; Outputs: D0 = 0 on success or 1 on program-buffer overflow.
; Clobbers: D3/CCR.
; CCR: reflects D0 on return.
emitApplyBinaryD6	.block
	movem.l d6, -(sp)
	move.l d6, d3
	move.w runtime.ExprvmSelectedOpcodeVersion, d6
	cmpi.w #2, d6
	bne.s version1
	moveq #runtime.EXPRVM_V2_OPCODE_APPLY_BINARY, d6
	bra.s ready

version1
	moveq #runtime.EXPRVM_OPCODE_APPLY_BINARY, d6

ready
	bsr.w emitU8D6
	bne.s return
	move.l d3, d6
	bsr.w emitU8D6

return
	movem.l (sp)+, d6
	rts
	.bend  ; emitApplyBinaryD6

; Append the selected-version PushSymbol opcode plus the symbol index in D3.
; Inputs: D3 = symbol index; runtime.ExprvmSelectedOpcodeVersion = evaluator
; opcode version.
; Outputs: D0 = 0 on success or 1 on program-buffer overflow.
; Clobbers: D2/D6/CCR.
; CCR: reflects D0 on return.
emitPushSymbolD3	.block
	movem.l d2-d3/d6, -(sp)
	move.w runtime.ExprvmSelectedOpcodeVersion, d6
	cmpi.w #2, d6
	bne.s version1
	moveq #runtime.EXPRVM_V2_OPCODE_PUSH_SYMBOL, d6
	bra.s ready

version1
	moveq #runtime.EXPRVM_OPCODE_PUSH_SYMBOL, d6

ready
	bsr.w emitU8D6
	bne.s return
	movem.l (sp), d2-d3/d6
	bsr.w emitU16D3

return
	movem.l (sp)+, d2-d3/d6
	rts
	.bend  ; emitPushSymbolD3

; Append the selected-version PushLiteral opcode plus the 64-bit literal with
; the low 32 bits from D3 and a zero high word.
; Inputs: D3 = low 32 literal bits; runtime.ExprvmSelectedOpcodeVersion =
; evaluator opcode version.
; Outputs: D0 = 0 on success or 1 on program-buffer overflow.
; Clobbers: D2/D6/CCR.
; CCR: reflects D0 on return.
emitPushLiteralD3	.block
	movem.l d2-d3/d6, -(sp)
	move.w runtime.ExprvmSelectedOpcodeVersion, d6
	cmpi.w #2, d6
	bne.s version1
	moveq #runtime.EXPRVM_V2_OPCODE_PUSH_LITERAL, d6
	bra.s ready

version1
	moveq #runtime.EXPRVM_OPCODE_PUSH_LITERAL, d6

ready
	bsr.w emitU8D6
	bne.s return
	movem.l (sp), d2-d3/d6
	bsr.w emitU32D3
	bne.s return
	clr.l d3
	bsr.w emitU32D3

return
	movem.l (sp)+, d2-d3/d6
	rts
	.bend  ; emitPushLiteralD3

; Append D3 as four little-endian bytes to the private ExprVM program buffer.
; Inputs: D3 = value to encode.
; Outputs: D0 = 0 on success or 1 on program-buffer overflow.
; Clobbers: D6/CCR.
; CCR: reflects D0 on return.
emitU32D3	.block
	move.l d3, d6
	bsr.w emitU8D6
	bne.s return
	move.l d3, d6
	lsr.l #8, d6
	bsr.w emitU8D6
	bne.s return
	move.l d3, d6
	lsr.l #8, d6
	lsr.l #8, d6
	bsr.w emitU8D6
	bne.s return
	move.l d3, d6
	lsr.l #8, d6
	lsr.l #8, d6
	lsr.l #8, d6
	bsr.w emitU8D6

return
	rts
	.bend  ; emitU32D3

; Append D3 as two little-endian bytes to the private ExprVM program buffer.
; Inputs: D3 = value to encode.
; Outputs: D0 = 0 on success or 1 on program-buffer overflow.
; Clobbers: D6/CCR.
; CCR: reflects D0 on return.
emitU16D3	.block
	move.l d3, d6
	bsr.w emitU8D6
	bne.s return
	move.l d3, d6
	lsr.l #8, d6
	bsr.w emitU8D6

return
	rts
	.bend  ; emitU16D3

; Append the low byte of D6 to the private ExprVM program buffer.
; Inputs: D6 = byte value to append; OpcoreExprVmProgramLen = current length.
; Outputs: D0 = 0 on success or 1 on buffer overflow; OpcoreExprVmProgramLen
; incremented on success.
; Clobbers: D1/A2/CCR.
; CCR: reflects D0 on return.
emitU8D6	.block
	movem.l d1/a2, -(sp)
	moveq #0, d0
	move.w OpcoreExprVmProgramLen, d0
	cmpi.l #OPCORE_EXPRVM_PROGRAM_CAPACITY, d0
	bhs.s fail
	lea OpcoreExprVmProgramBuffer, a2
	move.b d6, 0(a2, d0.l)
	move.w OpcoreExprVmProgramLen, d1
	addq.w #1, d1
	move.w d1, OpcoreExprVmProgramLen
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1/a2
	rts
	.bend  ; emitU8D6

; Inputs:
; - A0/D0: expression text pointer and remaining length.
;
; Outputs:
; - A0/D0: advanced pointer and remaining length after skipping leading spaces/tabs.
;
; Clobbers:
; - D0-D1/A0/CCR
;
; CCR:
; - Reflects D0 on return.
skipWhitespace	.block
	tst.l d0
	beq.s done
	moveq #0, d1
	move.b (a0), d1
	cmpi.b #' ', d1
	beq.s one
	cmpi.b #9, d1
	bne.s done

one
	addq.l #1, a0
	subq.l #1, d0
	bra.s skipWhitespace

done
	tst.l d0
	rts
	.bend  ; skipWhitespace

termLength	.block
	movem.l d1-d2/a0, -(sp)
	clr.l d2

loop
	cmp.l d0, d2
	bhs.s done
	moveq #0, d1
	move.b 0(a0, d2.l), d1
	cmpi.b #' ', d1
	beq.s done
	cmpi.b #9, d1
	beq.s done
	cmpi.b #'+', d1
	beq.s done
	cmpi.b #'-', d1
	beq.s done
	cmpi.b #')', d1
	beq.s done
	addq.l #1, d2
	bra.s loop

done
	move.l d2, d0
	movem.l (sp)+, d1-d2/a0
	rts
	.bend  ; termLength

parseHex	.block
	movem.l d1-d2, -(sp)
	clr.l d3
	clr.l d2

loop
	tst.l d0
	beq.w ok
	moveq #0, d1
	move.b (a0)+, d1
	subq.l #1, d0
	cmpi.b #'+', d1
	beq.w endBeforeOperator
	cmpi.b #'-', d1
	beq.w endBeforeOperator
	cmpi.b #'|', d1
	beq.w endBeforeOperator
	cmpi.b #'?', d1
	beq.w endBeforeOperator
	cmpi.b #':', d1
	beq.w endBeforeOperator
	cmpi.b #')', d1
	beq.w endBeforeOperator
	cmpi.b #' ', d1
	beq.s ok
	cmpi.b #9, d1
	beq.s ok
	cmpi.b #'_', d1
	beq.s loop
	cmpi.b #'0', d1
	bcs.s fail
	cmpi.b #'9', d1
	bls.s digit
	cmpi.b #'A', d1
	bcs.s lower
	cmpi.b #'F', d1
	bhi.s lower
	subi.b #'A' - 10, d1
	bra.s haveDigit

lower
	cmpi.b #'a', d1
	bcs.s fail
	cmpi.b #'f', d1
	bhi.s fail
	subi.b #'a' - 10, d1
	bra.s haveDigit

digit
	subi.b #'0', d1

haveDigit
	moveq #1, d2
	lsl.l #4, d3
	or.b d1, d3
	bra.w loop

ok
	tst.l d2
	beq.s fail
	moveq #0, d5
	bra.s return

endBeforeOperator
	subq.l #1, a0
	addq.l #1, d0
	bra.s ok

fail
	moveq #1, d5

return
	movem.l (sp)+, d1-d2
	rts
	.bend  ; parseHex

parseBinary	.block
	movem.l d1-d2, -(sp)
	clr.l d3
	clr.l d2

loop
	tst.l d0
	beq.s ok
	moveq #0, d1
	move.b (a0)+, d1
	subq.l #1, d0
	cmpi.b #'+', d1
	beq.s endBeforeOperator
	cmpi.b #'-', d1
	beq.s endBeforeOperator
	cmpi.b #' ', d1
	beq.s ok
	cmpi.b #9, d1
	beq.s ok
	cmpi.b #'_', d1
	beq.s loop
	cmpi.b #'0', d1
	beq.s digit
	cmpi.b #'1', d1
	bne.s fail

digit
	subi.b #'0', d1
	moveq #1, d2
	lsl.l #1, d3
	or.b d1, d3
	bra.s loop

ok
	tst.l d2
	beq.s fail
	moveq #0, d5
	bra.s return

endBeforeOperator
	subq.l #1, a0
	addq.l #1, d0
	bra.s ok

fail
	moveq #1, d5

return
	movem.l (sp)+, d1-d2
	rts
	.bend  ; parseBinary

; Parse a digit-led scalar token with an optional 64tass-style base suffix.
; Inputs: A0/D0 = token text and remaining expression length.
; Outputs: A0/D0 advanced past exactly one token; D3 = value; D5 = status.
; Clobbers: D1-D2/A1/CCR.
; CCR: reflects D5 on return.
parseSuffixedNumber	.block
	movem.l d1-d2/a1, -(sp)
	movea.l a0, a1
	move.l d0, d2

scanToken
	tst.l d2
	beq.s tokenScanned
	moveq #0, d1
	move.b (a1)+, d1
	subq.l #1, d2
	cmpi.b #'+', d1
	beq.s tokenDelimiter
	cmpi.b #'-', d1
	beq.s tokenDelimiter
	cmpi.b #'*', d1
	beq.s tokenDelimiter
	cmpi.b #'/', d1
	beq.s tokenDelimiter
	cmpi.b #'%', d1
	beq.s tokenDelimiter
	cmpi.b #'&', d1
	beq.s tokenDelimiter
	cmpi.b #'|', d1
	beq.s tokenDelimiter
	cmpi.b #'^', d1
	beq.s tokenDelimiter
	cmpi.b #'?', d1
	beq.s tokenDelimiter
	cmpi.b #':', d1
	beq.s tokenDelimiter
	cmpi.b #')', d1
	beq.s tokenDelimiter
	cmpi.b #' ', d1
	beq.s tokenDelimiter
	cmpi.b #9, d1
	beq.s tokenDelimiter
	bra.s scanToken

tokenDelimiter
	subq.l #1, a1
	addq.l #1, d2

tokenScanned
	move.b -1(a1), d1
	ori.b #32, d1
	cmpi.b #'h', d1
	beq.s parseHexSuffix
	cmpi.b #'b', d1
	beq.s parseBinarySuffix
	cmpi.b #'o', d1
	beq.s parseOctalSuffix
	cmpi.b #'q', d1
	beq.s parseOctalSuffix
	cmpi.b #'d', d1
	beq.s parseDecimalSuffix
	bsr.w parseDecimal
	bra.s return

parseHexSuffix
	move.l d0, d1
	sub.l d2, d1
	subq.l #1, d1
	move.l d2, -(sp)
	move.l d1, d0
	bsr.w parseHex
	move.l (sp)+, d2
	bra.s consumeSuffix

parseBinarySuffix
	move.l d0, d1
	sub.l d2, d1
	subq.l #1, d1
	move.l d2, -(sp)
	move.l d1, d0
	bsr.w parseBinary
	move.l (sp)+, d2
	bra.s consumeSuffix

parseOctalSuffix
	move.l d0, d1
	sub.l d2, d1
	subq.l #1, d1
	move.l d2, -(sp)
	move.l d1, d0
	bsr.w parseOctal
	move.l (sp)+, d2
	bra.s consumeSuffix

parseDecimalSuffix
	move.l d0, d1
	sub.l d2, d1
	subq.l #1, d1
	move.l d2, -(sp)
	move.l d1, d0
	bsr.w parseDecimal
	move.l (sp)+, d2

consumeSuffix
	tst.l d5
	bne.s return
	addq.l #1, a0
	move.l d2, d0

return
	movem.l (sp)+, d1-d2/a1
	rts
	.bend  ; parseSuffixedNumber

; Parse an octal literal body with its suffix already removed.
; Inputs: A0/D0 = octal digits and remaining expression length.
; Outputs: A0/D0 advanced; D3 = value; D5 = status.
; Clobbers: D1/CCR.
; CCR: reflects D5 on return.
parseOctal	.block
	movem.l d1-d2, -(sp)
	clr.l d3
	clr.l d2

scanDigit
	tst.l d0
	beq.s ok
	moveq #0, d1
	move.b (a0)+, d1
	subq.l #1, d0
	cmpi.b #'_', d1
	beq.s scanDigit
	cmpi.b #'0', d1
	bcs.s fail
	cmpi.b #'7', d1
	bhi.s fail
	subi.b #'0', d1
	moveq #1, d2
	lsl.l #3, d3
	or.b d1, d3
	bra.s scanDigit

ok
	tst.l d2
	beq.s fail
	moveq #0, d5
	bra.s return

fail
	moveq #1, d5

return
	movem.l (sp)+, d1-d2
	rts
	.bend  ; parseOctal

parseDecimal	.block
	movem.l d1-d2/d4, -(sp)
	clr.l d3
	clr.l d4

loop
	tst.l d0
	beq.w ok
	moveq #0, d1
	move.b (a0)+, d1
	subq.l #1, d0
	cmpi.b #'+', d1
	beq.w endBeforeOperator
	cmpi.b #'-', d1
	beq.w endBeforeOperator
	cmpi.b #'*', d1
	beq.w endBeforeOperator
	cmpi.b #'/', d1
	beq.w endBeforeOperator
	cmpi.b #'%', d1
	beq.w endBeforeOperator
	cmpi.b #'<', d1
	beq.w endBeforeOperator
	cmpi.b #'>', d1
	beq.w endBeforeOperator
	cmpi.b #'|', d1
	beq.w endBeforeOperator
	cmpi.b #'?', d1
	beq.w endBeforeOperator
	cmpi.b #':', d1
	beq.w endBeforeOperator
	cmpi.b #')', d1
	beq.w endBeforeOperator
	cmpi.b #' ', d1
	beq.s ok
	cmpi.b #9, d1
	beq.s ok
	cmpi.b #'_', d1
	beq.s loop
	cmpi.b #'0', d1
	bcs.s fail
	cmpi.b #'9', d1
	bhi.s fail
	subi.b #'0', d1
	moveq #1, d4
	move.l d3, d2
	lsl.l #3, d3
	add.l d2, d3
	add.l d2, d3
	add.l d1, d3
	bra.w loop

ok
	tst.l d4
	beq.s fail
	moveq #0, d5
	bra.s return

endBeforeOperator
	subq.l #1, a0
	addq.l #1, d0
	bra.s ok

fail
	moveq #1, d5

return
	movem.l (sp)+, d1-d2/d4
	rts
	.bend  ; parseDecimal

resolveLabelIndex	.block
	movem.l d1-d2/d4-d6/a0-a2, -(sp)
	movea.l a0, a2
	move.l d0, d6
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS
	moveq #0, d0
	move.l d0, -(sp)  ; compared-byte count
	move.l d0, -(sp)  ; candidate count
.endif
	clr.w d4

loop
	cmp.w d7, d4
	bhs.s fail
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS
	addq.l #1, (sp)
.endif
	moveq #0, d5
	move.w d4, d5
	move.l d5, d1
	move.l d5, d2
	move.l d5, d0
	lsl.l #2, d5
	lsl.l #3, d1
	lsl.l #5, d2
	lsl.l #6, d0
	add.l d1, d5
	add.l d2, d5
	add.l d0, d5
	movea.l a3, a0
	adda.l d5, a0
	movea.l a2, a1
	move.l d6, d0
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL
	bsr.w labelEqualsCounted
	add.l d1, 4(sp)
	tst.l d0
.else
	bsr.w labelEquals
.endif
	bne.s found
	addq.w #1, d4
	bra.s loop

found
	moveq #0, d3
	move.w d4, d3
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EVENT_BIND, d0
	move.l (sp), d1
	move.l 4(sp), d2
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	addq.l #8, sp
.endif
	moveq #0, d0
	bra.s return

fail
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EVENT_BIND, d0
	move.l (sp), d1
	move.l 4(sp), d2
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	addq.l #8, sp
.endif
	moveq #1, d0

return
	movem.l (sp)+, d1-d2/d4-d6/a0-a2
	rts
	.bend  ; resolveLabelIndex

labelEquals	.block
	movem.l d1-d3/a0-a1, -(sp)
	move.l d0, d3
	beq.s no

loop
	move.b (a0)+, d1
	move.b (a1)+, d2
	cmp.b d2, d1
	bne.s no
	subq.l #1, d3
	bne.s loop
	tst.b (a0)
	bne.s no
	moveq #1, d0
	bra.s return

no
	moveq #0, d0

return
	movem.l (sp)+, d1-d3/a0-a1
	rts
	.bend  ; labelEquals

; Snapshot comparison variant that reports exact compared-byte positions.
; Inputs: A0/A1 = text; D0.L = requested length.
; Outputs: D0 = boolean match; D1.L = compared byte positions.
; Clobbers: D0-D1/CCR. A0/A1 and other registers preserved.
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL
; @opforge-owner: opcore.amigaos.expr_bridge
; @opforge-slice: documentation/plans/slices/native-porting-slice-symbol-expression-work-v1.toml
; @opforge-role: delegation
labelEqualsCounted	.block
	movem.l d2-d4/a0-a1, -(sp)
	move.l d0, d3
	moveq #0, d4
	beq.s no
loop
	addq.l #1, d4
	move.b (a0)+, d1
	move.b (a1)+, d2
	cmp.b d2, d1
	bne.s no
	subq.l #1, d3
	bne.s loop
	addq.l #1, d4
	tst.b (a0)
	bne.s no
	moveq #1, d0
	bra.s done
no
	moveq #0, d0
done
	move.l d4, d1
	movem.l (sp)+, d2-d4/a0-a1
	rts
	.bend  ; labelEqualsCounted
.endif

OpcoreExvmDefaultProgram
	.byte EXVM_OPCODE_PARSE_EXPRESSION, EXVM_OPCODE_END
OPCORE_EXVM_DEFAULT_PROGRAM_END

OPCORE_EXVM_DEFAULT_PROGRAM_LEN = OPCORE_EXVM_DEFAULT_PROGRAM_END - OpcoreExvmDefaultProgram
	.priv

	.endsection

	.section bss, kind=bss

OpcoreExvmSelectedOpcodeVersion
	.res word, 1
OpcoreExvmSawUnresolvedSymbol
	.res byte, 1
	.align 2
OpcoreExvmSymbolResolverPtr
	.res long, 1
OpcoreExprVmProgramLen
	.res word, 1
OpcoreExprVmProgramBuffer
	.res byte, OPCORE_EXPRVM_PROGRAM_CAPACITY
	.align 4

	.endsection
	.endmodule

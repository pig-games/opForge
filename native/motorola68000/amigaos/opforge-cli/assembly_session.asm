; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.assembly_session
	.cpu 68020

	.use opasm.amigaos.engine
	.use tkpkg.amigaos.buffers

	.use opforge.cli.state

	.use opforge.cli.constants

	.use opforge.cli.strings
	.use opforge.cli.dos
	.use opforge.cli.text_output
	.use opforge.cli.copy
	.use opforge.cli.line_text

	.section code, kind=code
	.pub

; Record the current logical source line in the session tables.
opforgeNativeCliRecordSourceLine	.block
	movem.l d0-d1, -(sp)
	move.l state.NativeCliSourceLineNum, d0
	moveq #0, d1
	move.w state.NativeCliSourceLineLen, d1
	jsr engine.opasmEngineRecordSourceLineV1
	movem.l (sp)+, d0-d1
	rts
	.bend  ; opforgeNativeCliRecordSourceLine

opforgeNativeCliRecordPrvmStatementLine	.block
	movem.l d1-d7/a0-a2, -(sp)
	tst.l state.NativeCliPrvmRouteStatus
	beq.s routeOk
	cmpi.l #constants.PRVM_STATUS_EXPR_REQUEST, state.NativeCliPrvmRouteStatus
	bne.w sourceOnly

routeOk
	clr.l state.NativeCliStmtLabelStart
	clr.l state.NativeCliStmtLabelEnd
	clr.l state.NativeCliStmtLabelOff
	clr.l state.NativeCliStmtLabelLen
	clr.l state.NativeCliStmtMnemStart
	clr.l state.NativeCliStmtMnemEnd
	clr.l state.NativeCliStmtMnemOff
	clr.l state.NativeCliStmtMnemLen
	clr.l state.NativeCliStmtOperandStart
	clr.l state.NativeCliStmtOperandEnd
	clr.l state.NativeCliStmtExprOperandIndex
	clr.l state.NativeCliStmtExprSlotIndex
	clr.l state.NativeCliStmtExprStartToken
	clr.l state.NativeCliStmtExprEndToken
	clr.l state.NativeCliStmtExprSpanLine
	clr.l state.NativeCliStmtExprSpanStart
	clr.l state.NativeCliStmtExprSpanEnd
	clr.w state.NativeCliStmtMnemFound
	clr.w state.NativeCliStmtExprFound
	moveq #constants.NCLI_PARSER_DIRECTIVE_NONE, d0
	move.w d0, state.NativeCliStmtDirectiveKind
	move.w state.NativeCliPrvmResultCount, d7
	cmpi.l #constants.PRVM_STATUS_EXPR_REQUEST, state.NativeCliPrvmRouteStatus
	bne.s haveCount
	move.w #constants.PRVM_RESULT_RECORD_COUNT, d7

haveCount
	beq.w done
	subq.w #1, d7
	lea state.OpforgeNativeCliPrvmResultBuffer, a2

scan
	tst.w 0(a2)
	beq.w finalize
	cmpi.w #constants.PRVM_RESULT_LABEL_TEXT, 0(a2)
	beq.w haveLabel
	cmpi.w #constants.PRVM_RESULT_MNEMONIC_TEXT, 0(a2)
	beq.w haveMnemonic
	cmpi.w #constants.PRVM_RESULT_DIRECTIVE_TEXT, 0(a2)
	beq.w haveDirective
	cmpi.w #constants.PRVM_RESULT_OPERAND_TEXT, 0(a2)
	beq.w haveOperandText
	cmpi.w #constants.PRVM_RESULT_OPERAND_EXPR_SLOT, 0(a2)
	beq.w haveOperandExpr

next
	adda.l #constants.PRVM_RESULT_RECORD_SIZE, a2
	dbra d7, scan

finalize
	cmpi.l #constants.PRVM_STATUS_EXPR_REQUEST, state.NativeCliPrvmRouteStatus
	bne.s checkMnemonic
	bsr.w opforgeNativeCliRecordPrvmExpressionRequest

checkMnemonic
	tst.l state.NativeCliStmtLabelLen
	beq.s checkMnemonicFound
	tst.w state.NativeCliStmtMnemFound
	beq.w maybeLabelOnly
	tst.l state.NativeCliStmtOperandStart
	bne.s checkMnemonicFound
	move.l state.NativeCliStmtLabelLen, d0
	cmp.l state.NativeCliStmtMnemLen, d0
	beq.s clearBareMnem
	move.l state.NativeCliStmtLabelStart, d0
	cmp.l state.NativeCliStmtMnemStart, d0
	bne.s checkMnemonicFound
	move.l state.NativeCliStmtLabelEnd, d0
	cmp.l state.NativeCliStmtMnemEnd, d0
	bne.s checkMnemonicFound

clearBareMnem
	clr.l state.NativeCliStmtMnemStart
	clr.l state.NativeCliStmtMnemEnd
	clr.l state.NativeCliStmtMnemOff
	clr.l state.NativeCliStmtMnemLen
	clr.w state.NativeCliStmtMnemFound
	bra.s maybeLabelOnly

checkMnemonicFound
	tst.w state.NativeCliStmtMnemFound
	beq.s maybeLabelOnly
	tst.l state.NativeCliStmtOperandStart
	bne.s checkStore
	tst.l state.NativeCliStmtLabelLen
	bne.s checkStore
	clr.l state.NativeCliStmtMnemStart
	clr.l state.NativeCliStmtMnemEnd
	clr.l state.NativeCliStmtMnemOff
	clr.l state.NativeCliStmtMnemLen
	clr.w state.NativeCliStmtMnemFound
	bra.w trySourceFallback

maybeLabelOnly
	tst.l state.NativeCliStmtLabelLen
	bne.s checkStore
trySourceFallback
	bsr.w opforgeNativeCliRecordSourceStatementFallback
	tst.w state.NativeCliStmtMnemFound
	bne.s checkStore
	tst.l state.NativeCliStmtLabelLen
	beq.w done
checkStore
	bsr.w opforgeNativeCliStoreStatementRecord
	tst.l d0
	bne.w fail
	jsr engine.opasmEngineGetStatementCountV1
	tst.w d0
	bpl.s skipEmit
	bsr.w opforgeNativeCliEmitStatementRecord

skipEmit
	jsr engine.opasmEngineCommitStatementRecordV1
	bra.w done
	
sourceOnly
	clr.l state.NativeCliStmtLabelStart
	clr.l state.NativeCliStmtLabelEnd
	clr.l state.NativeCliStmtLabelOff
	clr.l state.NativeCliStmtLabelLen
	clr.l state.NativeCliStmtMnemStart
	clr.l state.NativeCliStmtMnemEnd
	clr.l state.NativeCliStmtMnemOff
	clr.l state.NativeCliStmtMnemLen
	clr.l state.NativeCliStmtOperandStart
	clr.l state.NativeCliStmtOperandEnd
	clr.l state.NativeCliStmtExprOperandIndex
	clr.l state.NativeCliStmtExprSlotIndex
	clr.l state.NativeCliStmtExprStartToken
	clr.l state.NativeCliStmtExprEndToken
	clr.l state.NativeCliStmtExprSpanLine
	clr.l state.NativeCliStmtExprSpanStart
	clr.l state.NativeCliStmtExprSpanEnd
	clr.w state.NativeCliStmtMnemFound
	clr.w state.NativeCliStmtExprFound
	moveq #constants.NCLI_PARSER_DIRECTIVE_NONE, d0
	move.w d0, state.NativeCliStmtDirectiveKind
	bra.w trySourceFallback

haveLabel
	move.l 8(a2), state.NativeCliStmtLabelStart
	move.l 12(a2), state.NativeCliStmtLabelEnd
	move.l 16(a2), state.NativeCliStmtLabelOff
	move.l 20(a2), state.NativeCliStmtLabelLen
	bra.w next

haveMnemonic
	move.l 8(a2), state.NativeCliStmtMnemStart
	move.l 12(a2), state.NativeCliStmtMnemEnd
	move.l 16(a2), state.NativeCliStmtMnemOff
	move.l 20(a2), state.NativeCliStmtMnemLen
	move.w #1, state.NativeCliStmtMnemFound
	bra.w next

haveDirective
	move.w #constants.NCLI_PARSER_DIRECTIVE_GENERIC, state.NativeCliStmtDirectiveKind
	bra.s haveMnemonic

haveOperandText
	move.l 8(a2), state.NativeCliStmtOperandStart
	move.l 12(a2), state.NativeCliStmtOperandEnd
	bra.w next

haveOperandExpr
	move.l 4(a2), state.NativeCliStmtExprSpanLine
	move.l 8(a2), state.NativeCliStmtExprSpanStart
	move.l 12(a2), state.NativeCliStmtExprSpanEnd
	move.l 16(a2), state.NativeCliStmtExprOperandIndex
	move.l 20(a2), state.NativeCliStmtExprSlotIndex
	move.l 24(a2), state.NativeCliStmtExprStartToken
	move.l 28(a2), state.NativeCliStmtExprEndToken
	move.w #1, state.NativeCliStmtExprFound
	bra.w next

done
	moveq #0, d0
	bra.w return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d7/a0-a2
	rts
	.bend  ; opforgeNativeCliRecordPrvmStatementLine

	.priv

opforgeNativeCliEmitStatementRecord	.block
	movem.l d0-d7/a0-a1, -(sp)
	move.l #strings.StatementText, d1
	jsr dos.putStr
	jsr engine.opasmEngineGetStatementCountV1
	jsr text_output.opforgeNativeCliPutDecU16
	jsr text_output.opforgeNativeCliPutSpace
	move.l state.NativeCliSourceLineNum, d0
	jsr text_output.opforgeNativeCliPutDecU16
	jsr text_output.opforgeNativeCliPutSpace
	move.w state.NativeCliStmtDirectiveKind, d0
	jsr text_output.opforgeNativeCliPutDecU16
	jsr text_output.opforgeNativeCliPutSpace
	move.l state.NativeCliStmtLabelStart, d0
	jsr text_output.opforgeNativeCliPutDecU16
	jsr text_output.opforgeNativeCliPutSpace
	move.l state.NativeCliStmtLabelEnd, d0
	jsr text_output.opforgeNativeCliPutDecU16
	jsr text_output.opforgeNativeCliPutSpace
	move.l state.NativeCliStmtMnemStart, d0
	jsr text_output.opforgeNativeCliPutDecU16
	jsr text_output.opforgeNativeCliPutSpace
	move.l state.NativeCliStmtMnemEnd, d0
	jsr text_output.opforgeNativeCliPutDecU16
	jsr text_output.opforgeNativeCliPutSpace
	move.l state.NativeCliStmtLabelLen, d0
	jsr text_output.opforgeNativeCliPutDecU16
	jsr text_output.opforgeNativeCliPutSpace
	move.l state.NativeCliStmtMnemLen, d0
	jsr text_output.opforgeNativeCliPutDecU16
	jsr text_output.opforgeNativeCliPutSpace
	lea buffers.tokenScratchBuffer, a0
	move.l state.NativeCliStmtMnemOff, d0
	adda.l d0, a0
	lea state.NativeCliArgToken, a1
	move.l state.NativeCliStmtMnemLen, d0
	jsr copy.copyFixedString
	clr.b (a1)
	move.l #state.NativeCliArgToken, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	tst.w state.NativeCliStmtExprFound
	beq.s done
	jsr opforgeNativeCliEmitStatementExprRequest

done
	movem.l (sp)+, d0-d7/a0-a1
	rts
	.bend  ; opforgeNativeCliEmitStatementRecord

opforgeNativeCliRecordPrvmExpressionRequest	.block
	lea state.OpforgeNativeCliPrvmExprRequest, a2
	cmpi.w #1, 0(a2)
	bne.s done
	move.l 4(a2), state.NativeCliStmtExprOperandIndex
	move.l 8(a2), state.NativeCliStmtExprSlotIndex
	move.l 12(a2), state.NativeCliStmtExprStartToken
	move.l 16(a2), state.NativeCliStmtExprEndToken
	move.l 20(a2), state.NativeCliStmtExprSpanLine
	move.l 24(a2), state.NativeCliStmtExprSpanStart
	move.l 28(a2), state.NativeCliStmtExprSpanEnd
	move.w #1, state.NativeCliStmtExprFound

done
	rts
	.bend  ; opforgeNativeCliRecordPrvmExpressionRequest

opforgeNativeCliEmitStatementExprRequest	.block
	move.l #strings.StatementExprText, d1
	jsr dos.putStr
	jsr engine.opasmEngineGetStatementCountV1
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace
	move.l state.NativeCliStmtExprOperandIndex, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace
	move.l state.NativeCliStmtExprSlotIndex, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace
	move.l state.NativeCliStmtExprStartToken, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace
	move.l state.NativeCliStmtExprEndToken, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace
	move.l state.NativeCliStmtExprSpanLine, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace
	move.l state.NativeCliStmtExprSpanStart, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace
	move.l state.NativeCliStmtExprSpanEnd, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	rts
	.bend  ; opforgeNativeCliEmitStatementExprRequest

opforgeNativeCliRecordSourceStatementFallback	.block
	movem.l d0-d7/a0-a3, -(sp)
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.w return
	movea.l a0, a2
	move.l d0, d2
	bsr.w opforgeNativeCliFallbackTokenLen
	tst.w d0
	beq.w return
	move.w d0, d3
	moveq #0, d4
	move.w state.NativeCliSourceLineLen, d4
	sub.w d2, d4
	addq.w #1, d4
	movea.l a2, a3
	adda.w d3, a3
	move.l d2, d5
	sub.w d3, d5
	tst.l d5
	beq.s firstToken
	cmpi.b #':', (a3)
	beq.s labelToken

firstToken
	cmpi.l #1, d4
	bne.s firstTokenMnemonic
	tst.l d5
	beq.s bareLabel
	movea.l a3, a0
	move.l d5, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.s bareLabel
	tst.b (a0)
	beq.s bareLabel
	cmpi.b #10, (a0)
	beq.s bareLabel
	cmpi.b #13, (a0)
	beq.s bareLabel
	cmpi.b #';', (a0)
	beq.s bareLabel

firstTokenMnemonic
	bsr.w opforgeNativeCliRecordSourceStatementMnemonic
	bra.w return

bareLabel
	move.l d4, state.NativeCliStmtLabelStart
	move.l d4, d0
	add.w d3, d0
	move.l d0, state.NativeCliStmtLabelEnd
	move.l d3, state.NativeCliStmtLabelLen
	clr.l state.NativeCliStmtLabelOff
	bra.w return

labelToken
	move.l d4, state.NativeCliStmtLabelStart
	move.l d4, d0
	add.w d3, d0
	move.l d0, state.NativeCliStmtLabelEnd
	move.l d3, state.NativeCliStmtLabelLen
	clr.l state.NativeCliStmtLabelOff
	addq.l #1, a3
	subq.l #1, d5
	movea.l a3, a0
	move.l d5, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.w return
	movea.l a0, a2
	move.l d0, d2
	bsr.w opforgeNativeCliFallbackTokenLen
	tst.w d0
	beq.w return
	move.w d0, d3
	moveq #0, d4
	move.w state.NativeCliSourceLineLen, d4
	sub.w d2, d4
	addq.w #1, d4
	bsr.w opforgeNativeCliRecordSourceStatementMnemonic

return
	movem.l (sp)+, d0-d7/a0-a3
	rts
	.bend  ; opforgeNativeCliRecordSourceStatementFallback
	
opforgeNativeCliRecordSourceStatementMnemonic	.block
	move.l d4, state.NativeCliStmtMnemStart
	move.l d4, d0
	add.w d3, d0
	move.l d0, state.NativeCliStmtMnemEnd
	clr.l state.NativeCliStmtMnemOff
	move.l d3, state.NativeCliStmtMnemLen
	lea buffers.tokenScratchBuffer, a1
	movea.l a2, a0
	move.w d3, d0
	jsr copy.copyFixedString
	clr.b (a1)
	move.w #1, state.NativeCliStmtMnemFound
	movea.l a2, a0
	adda.w d3, a0
	move.l d2, d0
	sub.w d3, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.s done
	tst.b (a0)
	beq.s done
	cmpi.b #10, (a0)
	beq.s done
	cmpi.b #13, (a0)
	beq.s done
	cmpi.b #';', (a0)
	beq.s done
	moveq #0, d5
	move.w state.NativeCliSourceLineLen, d5
	sub.w d0, d5
	addq.w #1, d5
	move.l d5, state.NativeCliStmtOperandStart
	bsr.w opforgeNativeCliFallbackOperandLen
	tst.w d0
	beq.s done
	add.w d0, d5
	move.l d5, state.NativeCliStmtOperandEnd

done
	rts
	.bend  ; opforgeNativeCliRecordSourceStatementMnemonic

opforgeNativeCliFallbackTokenLen	.block
	movem.l d1-d2/a0, -(sp)
	moveq #0, d1

loop
	tst.l d0
	beq.s done
	move.b (a0), d2
	beq.s done
	cmpi.b #10, d2
	beq.s done
	cmpi.b #13, d2
	beq.s done
	cmpi.b #' ', d2
	beq.s done
	cmpi.b #9, d2
	beq.s done
	cmpi.b #':', d2
	beq.s done
	cmpi.b #';', d2
	beq.s done
	addq.l #1, a0
	subq.l #1, d0
	addq.w #1, d1
	bra.s loop

done
	move.w d1, d0
	movem.l (sp)+, d1-d2/a0
	rts
	.bend  ; opforgeNativeCliFallbackTokenLen

opforgeNativeCliFallbackOperandLen	.block
	movem.l d1-d3/a0, -(sp)
	moveq #0, d1
	moveq #0, d2

loop
	tst.l d0
	beq.s done
	move.b (a0)+, d3
	beq.s done
	cmpi.b #10, d3
	beq.s done
	cmpi.b #13, d3
	beq.s done
	cmpi.b #';', d3
	beq.s done
	addq.w #1, d1
	cmpi.b #' ', d3
	beq.s next
	cmpi.b #9, d3
	beq.s next
	move.w d1, d2

next
	subq.l #1, d0
	bra.s loop

done
	move.w d2, d0
	movem.l (sp)+, d1-d3/a0
	rts
	.bend  ; opforgeNativeCliFallbackOperandLen

opforgeNativeCliStoreStatementRecord	.block
	suba.l #engine.OPASM_ENGINE_STMT_RECORD_REQUEST_BYTES, sp
	movea.l sp, a2
	move.l state.NativeCliSourceLineNum, engine.OPASM_ENGINE_STMT_REQ_SOURCE_LINE_NUM(a2)
	move.w state.NativeCliSourceLineLen, engine.OPASM_ENGINE_STMT_REQ_SOURCE_LINE_LEN(a2)
	move.w state.NativeCliStmtDirectiveKind, engine.OPASM_ENGINE_STMT_REQ_DIRECTIVE_KIND(a2)
	move.l state.NativeCliStmtLabelStart, engine.OPASM_ENGINE_STMT_REQ_LABEL_START(a2)
	move.l state.NativeCliStmtLabelLen, engine.OPASM_ENGINE_STMT_REQ_LABEL_LEN(a2)
	move.l state.NativeCliStmtMnemStart, engine.OPASM_ENGINE_STMT_REQ_MNEM_START(a2)
	move.l state.NativeCliStmtMnemOff, engine.OPASM_ENGINE_STMT_REQ_MNEM_OFF(a2)
	move.l state.NativeCliStmtMnemLen, engine.OPASM_ENGINE_STMT_REQ_MNEM_LEN(a2)
	move.l state.NativeCliStmtOperandStart, engine.OPASM_ENGINE_STMT_REQ_OPERAND_START(a2)
	move.l state.NativeCliStmtOperandEnd, engine.OPASM_ENGINE_STMT_REQ_OPERAND_END(a2)
	move.w state.NativeCliStmtExprFound, engine.OPASM_ENGINE_STMT_REQ_EXPR_FOUND(a2)
	move.l state.NativeCliStmtExprOperandIndex, engine.OPASM_ENGINE_STMT_REQ_EXPR_OPERAND_INDEX(a2)
	move.l state.NativeCliStmtExprSlotIndex, engine.OPASM_ENGINE_STMT_REQ_EXPR_SLOT_INDEX(a2)
	move.l state.NativeCliStmtExprStartToken, engine.OPASM_ENGINE_STMT_REQ_EXPR_START_TOKEN(a2)
	move.l state.NativeCliStmtExprEndToken, engine.OPASM_ENGINE_STMT_REQ_EXPR_END_TOKEN(a2)
	move.l state.NativeCliStmtExprSpanLine, engine.OPASM_ENGINE_STMT_REQ_EXPR_SPAN_LINE(a2)
	move.l state.NativeCliStmtExprSpanStart, engine.OPASM_ENGINE_STMT_REQ_EXPR_SPAN_START(a2)
	move.l state.NativeCliStmtExprSpanEnd, engine.OPASM_ENGINE_STMT_REQ_EXPR_SPAN_END(a2)
	lea state.NativeCliSourceLine, a0
	lea buffers.tokenScratchBuffer, a1
	jsr engine.opasmEngineStoreStatementRecordV1
	adda.l #engine.OPASM_ENGINE_STMT_RECORD_REQUEST_BYTES, sp
	rts
	.bend  ; opforgeNativeCliStoreStatementRecord

	.endsection
	.endmodule

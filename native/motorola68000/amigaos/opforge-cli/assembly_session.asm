; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.assembly_session
	.cpu 68020

	.use opasm.amigaos.engine (opasmEngineRecordSourceLineV1, opasmEngineStoreStatementRecordV1, opasmEngineCommitStatementRecordV1)
	.use opasm.amigaos.engine (opasmEngineGetStatementCountV1, OPASM_ENGINE_STMT_RECORD_REQUEST_BYTES)
	.use opasm.amigaos.engine (OPASM_ENGINE_STMT_REQ_SOURCE_LINE_NUM, OPASM_ENGINE_STMT_REQ_SOURCE_LINE_LEN, OPASM_ENGINE_STMT_REQ_DIRECTIVE_KIND, OPASM_ENGINE_STMT_REQ_LABEL_START, OPASM_ENGINE_STMT_REQ_LABEL_LEN, OPASM_ENGINE_STMT_REQ_MNEM_START, OPASM_ENGINE_STMT_REQ_MNEM_OFF, OPASM_ENGINE_STMT_REQ_MNEM_LEN, OPASM_ENGINE_STMT_REQ_OPERAND_START, OPASM_ENGINE_STMT_REQ_OPERAND_END, OPASM_ENGINE_STMT_REQ_EXPR_FOUND)
	.use opasm.amigaos.engine (OPASM_ENGINE_STMT_REQ_EXPR_OPERAND_INDEX, OPASM_ENGINE_STMT_REQ_EXPR_SLOT_INDEX, OPASM_ENGINE_STMT_REQ_EXPR_START_TOKEN, OPASM_ENGINE_STMT_REQ_EXPR_END_TOKEN, OPASM_ENGINE_STMT_REQ_EXPR_SPAN_LINE, OPASM_ENGINE_STMT_REQ_EXPR_SPAN_START, OPASM_ENGINE_STMT_REQ_EXPR_SPAN_END)
	.use tkpkg.amigaos.buffers (tokenScratchBuffer)

	.use opforge.cli.state (OpforgeNativeCliPrvmResultBuffer, OpforgeNativeCliPrvmExprRequest, NativeCliSourceLine, NativeCliSourceLineNum, NativeCliSourceLineLen, NativeCliPrvmRouteStatus, NativeCliPrvmResultCount, NativeCliStmtLabelStart, NativeCliStmtLabelEnd, NativeCliStmtLabelOff, NativeCliStmtLabelLen, NativeCliStmtMnemStart, NativeCliStmtMnemEnd, NativeCliStmtMnemOff, NativeCliStmtMnemLen, NativeCliStmtOperandStart, NativeCliStmtOperandEnd, NativeCliStmtExprOperandIndex, NativeCliStmtExprSlotIndex, NativeCliStmtExprStartToken, NativeCliStmtExprEndToken, NativeCliStmtExprSpanLine, NativeCliStmtExprSpanStart, NativeCliStmtExprSpanEnd, NativeCliStmtMnemFound, NativeCliStmtExprFound, NativeCliStmtDirectiveKind, NativeCliArgToken)

	.use opforge.cli.constants (NCLI_PARSER_DIRECTIVE_NONE, NCLI_PARSER_DIRECTIVE_GENERIC, PRVM_STATUS_EXPR_REQUEST, PRVM_RESULT_RECORD_COUNT, PRVM_RESULT_RECORD_SIZE, PRVM_RESULT_LABEL_TEXT, PRVM_RESULT_MNEMONIC_TEXT, PRVM_RESULT_DIRECTIVE_TEXT, PRVM_RESULT_OPERAND_TEXT, PRVM_RESULT_OPERAND_EXPR_SLOT)

	.use opforge.cli.strings (StatementText, StatementExprText, NewlineText)
	.use opforge.cli.dos (opforgeNativeCliPutStr)
	.use opforge.cli.text_output (opforgeNativeCliPutDecU16, opforgeNativeCliPutSpace)
	.use opforge.cli.copy (opforgeNativeCliCopyFixedString)
	.use opforge.cli.line_text (opforgeNativeCliSkipLineWhitespace)

	.section code, kind=code
	.pub

; Record the current logical source line in the session tables.
opforgeNativeCliRecordSourceLine	.block
	movem.l d0-d1, -(sp)
	move.l NativeCliSourceLineNum, d0
	moveq #0, d1
	move.w NativeCliSourceLineLen, d1
	jsr opasmEngineRecordSourceLineV1
	movem.l (sp)+, d0-d1
	rts
	.bend  ; opforgeNativeCliRecordSourceLine

opforgeNativeCliRecordPrvmStatementLine	.block
	movem.l d1-d7/a0-a2, -(sp)
	tst.l NativeCliPrvmRouteStatus
	beq.s routeOk
	cmpi.l #PRVM_STATUS_EXPR_REQUEST, NativeCliPrvmRouteStatus
	bne.w sourceOnly

routeOk
	clr.l NativeCliStmtLabelStart
	clr.l NativeCliStmtLabelEnd
	clr.l NativeCliStmtLabelOff
	clr.l NativeCliStmtLabelLen
	clr.l NativeCliStmtMnemStart
	clr.l NativeCliStmtMnemEnd
	clr.l NativeCliStmtMnemOff
	clr.l NativeCliStmtMnemLen
	clr.l NativeCliStmtOperandStart
	clr.l NativeCliStmtOperandEnd
	clr.l NativeCliStmtExprOperandIndex
	clr.l NativeCliStmtExprSlotIndex
	clr.l NativeCliStmtExprStartToken
	clr.l NativeCliStmtExprEndToken
	clr.l NativeCliStmtExprSpanLine
	clr.l NativeCliStmtExprSpanStart
	clr.l NativeCliStmtExprSpanEnd
	clr.w NativeCliStmtMnemFound
	clr.w NativeCliStmtExprFound
	moveq #NCLI_PARSER_DIRECTIVE_NONE, d0
	move.w d0, NativeCliStmtDirectiveKind
	move.w NativeCliPrvmResultCount, d7
	cmpi.l #PRVM_STATUS_EXPR_REQUEST, NativeCliPrvmRouteStatus
	bne.s haveCount
	move.w #PRVM_RESULT_RECORD_COUNT, d7

haveCount
	beq.w done
	subq.w #1, d7
	lea OpforgeNativeCliPrvmResultBuffer, a2

scan
	tst.w 0(a2)
	beq.w finalize
	cmpi.w #PRVM_RESULT_LABEL_TEXT, 0(a2)
	beq.w haveLabel
	cmpi.w #PRVM_RESULT_MNEMONIC_TEXT, 0(a2)
	beq.w haveMnemonic
	cmpi.w #PRVM_RESULT_DIRECTIVE_TEXT, 0(a2)
	beq.w haveDirective
	cmpi.w #PRVM_RESULT_OPERAND_TEXT, 0(a2)
	beq.w haveOperandText
	cmpi.w #PRVM_RESULT_OPERAND_EXPR_SLOT, 0(a2)
	beq.w haveOperandExpr

next
	adda.l #PRVM_RESULT_RECORD_SIZE, a2
	dbra d7, scan

finalize
	cmpi.l #PRVM_STATUS_EXPR_REQUEST, NativeCliPrvmRouteStatus
	bne.s checkMnemonic
	bsr.w opforgeNativeCliRecordPrvmExpressionRequest

checkMnemonic
	tst.l NativeCliStmtLabelLen
	beq.s checkMnemonicFound
	tst.w NativeCliStmtMnemFound
	beq.w maybeLabelOnly
	tst.l NativeCliStmtOperandStart
	bne.s checkMnemonicFound
	move.l NativeCliStmtLabelLen, d0
	cmp.l NativeCliStmtMnemLen, d0
	beq.s clearBareMnem
	move.l NativeCliStmtLabelStart, d0
	cmp.l NativeCliStmtMnemStart, d0
	bne.s checkMnemonicFound
	move.l NativeCliStmtLabelEnd, d0
	cmp.l NativeCliStmtMnemEnd, d0
	bne.s checkMnemonicFound

clearBareMnem
	clr.l NativeCliStmtMnemStart
	clr.l NativeCliStmtMnemEnd
	clr.l NativeCliStmtMnemOff
	clr.l NativeCliStmtMnemLen
	clr.w NativeCliStmtMnemFound
	bra.s maybeLabelOnly

checkMnemonicFound
	tst.w NativeCliStmtMnemFound
	beq.s maybeLabelOnly
	tst.l NativeCliStmtOperandStart
	bne.s checkStore
	tst.l NativeCliStmtLabelLen
	bne.s checkStore
	clr.l NativeCliStmtMnemStart
	clr.l NativeCliStmtMnemEnd
	clr.l NativeCliStmtMnemOff
	clr.l NativeCliStmtMnemLen
	clr.w NativeCliStmtMnemFound
	bra.w trySourceFallback

maybeLabelOnly
	tst.l NativeCliStmtLabelLen
	bne.s checkStore
trySourceFallback
	bsr.w opforgeNativeCliRecordSourceStatementFallback
	tst.w NativeCliStmtMnemFound
	bne.s checkStore
	tst.l NativeCliStmtLabelLen
	beq.w done
checkStore
	bsr.w opforgeNativeCliStoreStatementRecord
	tst.l d0
	bne.w fail
	jsr opasmEngineGetStatementCountV1
	tst.w d0
	bpl.s skipEmit
	bsr.w opforgeNativeCliEmitStatementRecord

skipEmit
	jsr opasmEngineCommitStatementRecordV1
	bra.w done
	
sourceOnly
	clr.l NativeCliStmtLabelStart
	clr.l NativeCliStmtLabelEnd
	clr.l NativeCliStmtLabelOff
	clr.l NativeCliStmtLabelLen
	clr.l NativeCliStmtMnemStart
	clr.l NativeCliStmtMnemEnd
	clr.l NativeCliStmtMnemOff
	clr.l NativeCliStmtMnemLen
	clr.l NativeCliStmtOperandStart
	clr.l NativeCliStmtOperandEnd
	clr.l NativeCliStmtExprOperandIndex
	clr.l NativeCliStmtExprSlotIndex
	clr.l NativeCliStmtExprStartToken
	clr.l NativeCliStmtExprEndToken
	clr.l NativeCliStmtExprSpanLine
	clr.l NativeCliStmtExprSpanStart
	clr.l NativeCliStmtExprSpanEnd
	clr.w NativeCliStmtMnemFound
	clr.w NativeCliStmtExprFound
	moveq #NCLI_PARSER_DIRECTIVE_NONE, d0
	move.w d0, NativeCliStmtDirectiveKind
	bra.w trySourceFallback

haveLabel
	move.l 8(a2), NativeCliStmtLabelStart
	move.l 12(a2), NativeCliStmtLabelEnd
	move.l 16(a2), NativeCliStmtLabelOff
	move.l 20(a2), NativeCliStmtLabelLen
	bra.w next

haveMnemonic
	move.l 8(a2), NativeCliStmtMnemStart
	move.l 12(a2), NativeCliStmtMnemEnd
	move.l 16(a2), NativeCliStmtMnemOff
	move.l 20(a2), NativeCliStmtMnemLen
	move.w #1, NativeCliStmtMnemFound
	bra.w next

haveDirective
	move.w #NCLI_PARSER_DIRECTIVE_GENERIC, NativeCliStmtDirectiveKind
	bra.s haveMnemonic

haveOperandText
	move.l 8(a2), NativeCliStmtOperandStart
	move.l 12(a2), NativeCliStmtOperandEnd
	bra.w next

haveOperandExpr
	move.l 4(a2), NativeCliStmtExprSpanLine
	move.l 8(a2), NativeCliStmtExprSpanStart
	move.l 12(a2), NativeCliStmtExprSpanEnd
	move.l 16(a2), NativeCliStmtExprOperandIndex
	move.l 20(a2), NativeCliStmtExprSlotIndex
	move.l 24(a2), NativeCliStmtExprStartToken
	move.l 28(a2), NativeCliStmtExprEndToken
	move.w #1, NativeCliStmtExprFound
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
	move.l #StatementText, d1
	jsr opforgeNativeCliPutStr
	jsr opasmEngineGetStatementCountV1
	jsr opforgeNativeCliPutDecU16
	jsr opforgeNativeCliPutSpace
	move.l NativeCliSourceLineNum, d0
	jsr opforgeNativeCliPutDecU16
	jsr opforgeNativeCliPutSpace
	move.w NativeCliStmtDirectiveKind, d0
	jsr opforgeNativeCliPutDecU16
	jsr opforgeNativeCliPutSpace
	move.l NativeCliStmtLabelStart, d0
	jsr opforgeNativeCliPutDecU16
	jsr opforgeNativeCliPutSpace
	move.l NativeCliStmtLabelEnd, d0
	jsr opforgeNativeCliPutDecU16
	jsr opforgeNativeCliPutSpace
	move.l NativeCliStmtMnemStart, d0
	jsr opforgeNativeCliPutDecU16
	jsr opforgeNativeCliPutSpace
	move.l NativeCliStmtMnemEnd, d0
	jsr opforgeNativeCliPutDecU16
	jsr opforgeNativeCliPutSpace
	move.l NativeCliStmtLabelLen, d0
	jsr opforgeNativeCliPutDecU16
	jsr opforgeNativeCliPutSpace
	move.l NativeCliStmtMnemLen, d0
	jsr opforgeNativeCliPutDecU16
	jsr opforgeNativeCliPutSpace
	lea tokenScratchBuffer, a0
	move.l NativeCliStmtMnemOff, d0
	adda.l d0, a0
	lea NativeCliArgToken, a1
	move.l NativeCliStmtMnemLen, d0
	jsr opforgeNativeCliCopyFixedString
	clr.b (a1)
	move.l #NativeCliArgToken, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	tst.w NativeCliStmtExprFound
	beq.s done
	jsr opforgeNativeCliEmitStatementExprRequest

done
	movem.l (sp)+, d0-d7/a0-a1
	rts
	.bend  ; opforgeNativeCliEmitStatementRecord

opforgeNativeCliRecordPrvmExpressionRequest	.block
	lea OpforgeNativeCliPrvmExprRequest, a2
	cmpi.w #1, 0(a2)
	bne.s done
	move.l 4(a2), NativeCliStmtExprOperandIndex
	move.l 8(a2), NativeCliStmtExprSlotIndex
	move.l 12(a2), NativeCliStmtExprStartToken
	move.l 16(a2), NativeCliStmtExprEndToken
	move.l 20(a2), NativeCliStmtExprSpanLine
	move.l 24(a2), NativeCliStmtExprSpanStart
	move.l 28(a2), NativeCliStmtExprSpanEnd
	move.w #1, NativeCliStmtExprFound

done
	rts
	.bend  ; opforgeNativeCliRecordPrvmExpressionRequest

opforgeNativeCliEmitStatementExprRequest	.block
	move.l #StatementExprText, d1
	jsr opforgeNativeCliPutStr
	jsr opasmEngineGetStatementCountV1
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprOperandIndex, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprSlotIndex, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprStartToken, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprEndToken, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprSpanLine, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprSpanStart, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprSpanEnd, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	rts
	.bend  ; opforgeNativeCliEmitStatementExprRequest

opforgeNativeCliRecordSourceStatementFallback	.block
	movem.l d0-d7/a0-a3, -(sp)
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	jsr opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.w return
	movea.l a0, a2
	move.l d0, d2
	bsr.w opforgeNativeCliFallbackTokenLen
	tst.w d0
	beq.w return
	move.w d0, d3
	moveq #0, d4
	move.w NativeCliSourceLineLen, d4
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
	jsr opforgeNativeCliSkipLineWhitespace
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
	move.l d4, NativeCliStmtLabelStart
	move.l d4, d0
	add.w d3, d0
	move.l d0, NativeCliStmtLabelEnd
	move.l d3, NativeCliStmtLabelLen
	clr.l NativeCliStmtLabelOff
	bra.w return

labelToken
	move.l d4, NativeCliStmtLabelStart
	move.l d4, d0
	add.w d3, d0
	move.l d0, NativeCliStmtLabelEnd
	move.l d3, NativeCliStmtLabelLen
	clr.l NativeCliStmtLabelOff
	addq.l #1, a3
	subq.l #1, d5
	movea.l a3, a0
	move.l d5, d0
	jsr opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.w return
	movea.l a0, a2
	move.l d0, d2
	bsr.w opforgeNativeCliFallbackTokenLen
	tst.w d0
	beq.w return
	move.w d0, d3
	moveq #0, d4
	move.w NativeCliSourceLineLen, d4
	sub.w d2, d4
	addq.w #1, d4
	bsr.w opforgeNativeCliRecordSourceStatementMnemonic

return
	movem.l (sp)+, d0-d7/a0-a3
	rts
	.bend  ; opforgeNativeCliRecordSourceStatementFallback
	
opforgeNativeCliRecordSourceStatementMnemonic	.block
	move.l d4, NativeCliStmtMnemStart
	move.l d4, d0
	add.w d3, d0
	move.l d0, NativeCliStmtMnemEnd
	clr.l NativeCliStmtMnemOff
	move.l d3, NativeCliStmtMnemLen
	lea tokenScratchBuffer, a1
	movea.l a2, a0
	move.w d3, d0
	jsr opforgeNativeCliCopyFixedString
	clr.b (a1)
	move.w #1, NativeCliStmtMnemFound
	movea.l a2, a0
	adda.w d3, a0
	move.l d2, d0
	sub.w d3, d0
	jsr opforgeNativeCliSkipLineWhitespace
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
	move.w NativeCliSourceLineLen, d5
	sub.w d0, d5
	addq.w #1, d5
	move.l d5, NativeCliStmtOperandStart
	bsr.w opforgeNativeCliFallbackOperandLen
	tst.w d0
	beq.s done
	add.w d0, d5
	move.l d5, NativeCliStmtOperandEnd

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
	tst.b d2
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
	tst.b d3
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
	suba.l #OPASM_ENGINE_STMT_RECORD_REQUEST_BYTES, sp
	movea.l sp, a2
	move.l NativeCliSourceLineNum, OPASM_ENGINE_STMT_REQ_SOURCE_LINE_NUM(a2)
	move.w NativeCliSourceLineLen, OPASM_ENGINE_STMT_REQ_SOURCE_LINE_LEN(a2)
	move.w NativeCliStmtDirectiveKind, OPASM_ENGINE_STMT_REQ_DIRECTIVE_KIND(a2)
	move.l NativeCliStmtLabelStart, OPASM_ENGINE_STMT_REQ_LABEL_START(a2)
	move.l NativeCliStmtLabelLen, OPASM_ENGINE_STMT_REQ_LABEL_LEN(a2)
	move.l NativeCliStmtMnemStart, OPASM_ENGINE_STMT_REQ_MNEM_START(a2)
	move.l NativeCliStmtMnemOff, OPASM_ENGINE_STMT_REQ_MNEM_OFF(a2)
	move.l NativeCliStmtMnemLen, OPASM_ENGINE_STMT_REQ_MNEM_LEN(a2)
	move.l NativeCliStmtOperandStart, OPASM_ENGINE_STMT_REQ_OPERAND_START(a2)
	move.l NativeCliStmtOperandEnd, OPASM_ENGINE_STMT_REQ_OPERAND_END(a2)
	move.w NativeCliStmtExprFound, OPASM_ENGINE_STMT_REQ_EXPR_FOUND(a2)
	move.l NativeCliStmtExprOperandIndex, OPASM_ENGINE_STMT_REQ_EXPR_OPERAND_INDEX(a2)
	move.l NativeCliStmtExprSlotIndex, OPASM_ENGINE_STMT_REQ_EXPR_SLOT_INDEX(a2)
	move.l NativeCliStmtExprStartToken, OPASM_ENGINE_STMT_REQ_EXPR_START_TOKEN(a2)
	move.l NativeCliStmtExprEndToken, OPASM_ENGINE_STMT_REQ_EXPR_END_TOKEN(a2)
	move.l NativeCliStmtExprSpanLine, OPASM_ENGINE_STMT_REQ_EXPR_SPAN_LINE(a2)
	move.l NativeCliStmtExprSpanStart, OPASM_ENGINE_STMT_REQ_EXPR_SPAN_START(a2)
	move.l NativeCliStmtExprSpanEnd, OPASM_ENGINE_STMT_REQ_EXPR_SPAN_END(a2)
	lea NativeCliSourceLine, a0
	lea tokenScratchBuffer, a1
	jsr opasmEngineStoreStatementRecordV1
	adda.l #OPASM_ENGINE_STMT_RECORD_REQUEST_BYTES, sp
	rts
	.bend  ; opforgeNativeCliStoreStatementRecord

	.endsection
	.endmodule

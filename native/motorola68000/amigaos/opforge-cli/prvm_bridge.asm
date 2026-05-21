; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.prvm_bridge
	.cpu 68020

	.use tkpkg.amigaos.abi (ENTRY_ORD_PARSE_LINE)
	.use tkpkg.amigaos.buffers (ControlBlockV1, lastErrorBuffer, packageStorage)
	.use tkpkg.amigaos.buffers (tokenRecordBuffer, tokenScratchBuffer, lastTokenCount, lastLexemeLen, TOKEN_RECORD_SIZE)
	.use tkpkg.amigaos.buffers (ActiveParserVmOffsetLo, LAST_ERROR_BUFFER_PTR_V1)
	.use tkpkg.amigaos.service as svc
	.use opforge.cli.state (NativeCliLineRequestLen, NativeCliPrvmRouteStatus, NativeCliPrvmResultCount)
	.use opforge.cli.state (OpforgeNativeCliPrvmRouteFrame, OpforgeNativeCliPrvmResultBuffer)
	.use opforge.cli.state (OpforgeNativeCliPrvmDiagBuffer, OpforgeNativeCliPrvmResumeBuffer)
	.use opforge.cli.state (OpforgeNativeCliPrvmExprRequest, OpforgeNativeCliPrvmExprResultSlot)
	.use opforge.cli.state (NativeCliSourceLine, NativeCliSourceLineNum, NativeCliSourceLineLen)
	.use opforge.cli.constants (PRVM_ROUTE_MAGIC_OPLR, PRVM_ROUTE_FRAME_SIZE, PRVM_ROUTE_ABI_VERSION_V1)
	.use opforge.cli.constants (PRVM_ROUTE_RESULT_CAPACITY, PRVM_ROUTE_DIAG_CAPACITY, PRVM_ROUTE_RESUME_CAPACITY)
	.use opforge.cli.constants (PRVM_ROUTE_EXPR_REQUEST_SIZE, PRVM_ROUTE_EXPR_RESULT_SIZE, PRVM_ROUTE_EXPR_RESULT_CAPACITY)
	.use opforge.cli.constants (PRVM_ROUTE_EXPR_RESULT_COUNT, PRVM_ROUTE_STEP_BUDGET, PRVM_STATUS_EXPR_REQUEST)
	.use opforge.cli.constants (PRVM_EXPR_SLOT_READY, PRVM_RESULT_MNEMONIC_TEXT, PRVM_RESULT_DIRECTIVE_TEXT)
	.use opforge.cli.constants (PRVM_PARSER_CONTRACT_VERSION_V2)
	.use opforge.cli.constants (NCLI_PARSER_DIRECTIVE_NONE, NCLI_PARSER_DIRECTIVE_MODULE)
	.use opforge.cli.constants (NCLI_PARSER_DIRECTIVE_ENDMODULE, NCLI_PARSER_DIRECTIVE_USE)
	.use opforge.cli.strings (ProcessorAsmText, KindStatementText, ModuleMnemonicText, EndmoduleMnemonicText, UseMnemonicText)
	.use opforge.cli.strings (ModuleDirectiveText, EndmoduleDirectiveText, UseDirectiveText)
	.use opforge.cli.copy
	.use opforge.cli.line_text (opforgeNativeCliSkipLineWhitespace, opforgeNativeCliLineStartsWith)
	.use opforge.cli.tkpkg_control_block (opforgeNativeCliWriteInputWindow, opforgeNativeCliReadStatus)

	.section code, kind=code
	.pub

opforgeNativeCliDispatchParseLineEnvelope	.block
	bsr.w opforgeNativeCliPrepareParseLineServiceRequest
	tst.l d0
	bne.s done
	bsr.w opforgeNativeCliDispatchPreparedParseLineEnvelope

done
	rts
	.bend  ; opforgeNativeCliDispatchParseLineEnvelope

opforgeNativeCliDispatchParseLineUntilReady	.block
	bsr.w opforgeNativeCliPrepareParseLineServiceRequest
	tst.l d0
	bne.s done

loop
	bsr.w opforgeNativeCliDispatchPreparedParseLineEnvelope
	tst.l d0
	bne.s done
	cmpi.l #PRVM_STATUS_EXPR_REQUEST, NativeCliPrvmRouteStatus
	bne.s done
	bsr.w opforgeNativeCliServicePrvmExpressionRequest
	tst.l d0
	bne.s done
	bra.s loop

done
	rts
	.bend  ; opforgeNativeCliDispatchParseLineUntilReady

opforgeNativeCliParserDirectiveKind	.block
	lea OpforgeNativeCliPrvmResultBuffer, a2
	cmpi.w #PRVM_RESULT_MNEMONIC_TEXT, 32(a2)
	beq.s haveText
	cmpi.w #PRVM_RESULT_DIRECTIVE_TEXT, 32(a2)
	bne.w fallback

haveText
	move.l 48(a2), d0
	lea tokenScratchBuffer, a0
	adda.l d0, a0
	move.l 52(a2), d0
	lea ModuleMnemonicText, a1
	moveq #6, d1
	bsr.w opforgeNativeCliParserMnemonicEquals
	tst.l d0
	bne.w module
	move.l 48(a2), d0
	lea tokenScratchBuffer, a0
	adda.l d0, a0
	move.l 52(a2), d0
	lea EndmoduleMnemonicText, a1
	moveq #9, d1
	bsr.w opforgeNativeCliParserMnemonicEquals
	tst.l d0
	bne.w endmodule
	move.l 48(a2), d0
	lea tokenScratchBuffer, a0
	adda.l d0, a0
	move.l 52(a2), d0
	lea UseMnemonicText, a1
	moveq #3, d1
	bsr.w opforgeNativeCliParserMnemonicEquals
	tst.l d0
	bne.w use

fallback
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea ModuleDirectiveText, a1
	moveq #7, d1
	bsr.w opforgeNativeCliParserMnemonicEquals
	tst.l d0
	bne.s module
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea EndmoduleDirectiveText, a1
	moveq #10, d1
	bsr.w opforgeNativeCliParserMnemonicEquals
	tst.l d0
	bne.s endmodule
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea UseDirectiveText, a1
	moveq #4, d1
	bsr.w opforgeNativeCliParserMnemonicEquals
	tst.l d0
	bne.s use
	moveq #NCLI_PARSER_DIRECTIVE_NONE, d0
	rts

module
	moveq #NCLI_PARSER_DIRECTIVE_MODULE, d0
	rts

endmodule
	moveq #NCLI_PARSER_DIRECTIVE_ENDMODULE, d0
	rts

use
	moveq #NCLI_PARSER_DIRECTIVE_USE, d0
	rts
	.bend  ; opforgeNativeCliParserDirectiveKind

	.priv

opforgeNativeCliDispatchPreparedParseLineEnvelope	.block
	bsr.w opforgeNativeCliWritePrvmRouteFrameInput
	tst.l d0
	bne.s done
	lea ControlBlockV1, a0
	move.w #LAST_ERROR_BUFFER_PTR_V1, d0
	move.w NativeCliLineRequestLen, d1
	bsr.w opforgeNativeCliWriteInputWindow
	moveq #ENTRY_ORD_PARSE_LINE, d0
	jsr svc.dispatchV1
	move.l d0, NativeCliPrvmRouteStatus
	move.w d1, NativeCliPrvmResultCount
	lea ControlBlockV1, a0
	bsr.w opforgeNativeCliReadStatus

done
	rts
	.bend  ; opforgeNativeCliDispatchPreparedParseLineEnvelope

opforgeNativeCliPrepareParseLineServiceRequest	.block
	bsr.w opforgeNativeCliBuildPrvmRouteFrame
	tst.l d0
	bne.s done
	bsr.w opforgeNativeCliWritePrvmRouteFrameInput

done
	rts
	.bend  ; opforgeNativeCliPrepareParseLineServiceRequest

opforgeNativeCliWritePrvmRouteFrameInput	.block
	lea OpforgeNativeCliPrvmRouteFrame, a1
	lea lastErrorBuffer, a2
	move.w #PRVM_ROUTE_FRAME_SIZE, d0
	bsr.w copy.copyBytes
	move.w #PRVM_ROUTE_FRAME_SIZE, NativeCliLineRequestLen
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliWritePrvmRouteFrameInput

opforgeNativeCliServicePrvmExpressionRequest	.block
	movem.l d1-d4/a0-a2, -(sp)
	lea OpforgeNativeCliPrvmExprRequest, a0
	lea OpforgeNativeCliPrvmExprResultSlot, a1
	move.w 0(a0), d0
	cmpi.w #1, d0
	bne.s fail
	tst.w 2(a0)
	bne.s fail
	clr.l d0
	move.l 8(a0), d0
	cmpi.l #PRVM_ROUTE_EXPR_RESULT_CAPACITY, d0
	bhs.s fail
	move.l d0, d3
	lsl.l #5, d3
	lea OpforgeNativeCliPrvmExprResultSlot, a1
	adda.l d3, a1
	move.w #PRVM_EXPR_SLOT_READY, 0(a1)
	clr.w 2(a1)
	move.l d0, 4(a1)
	move.l 20(a0), 8(a1)
	move.l 24(a0), 12(a1)
	move.l 28(a0), 16(a1)
	move.l d0, 20(a1)
	move.l #$FFFFFFFF, 24(a1)
	clr.l 28(a1)
	lea OpforgeNativeCliPrvmRouteFrame, a2
	move.l #OpforgeNativeCliPrvmExprResultSlot, 96(a2)
	addq.l #1, d0
	move.l d0, 100(a2)
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d4/a0-a2
	rts
	.bend  ; opforgeNativeCliServicePrvmExpressionRequest

opforgeNativeCliBuildPrvmRouteFrame	.block
	lea OpforgeNativeCliPrvmRouteFrame, a0
	move.l #PRVM_ROUTE_MAGIC_OPLR, 0(a0)
	move.w #PRVM_ROUTE_ABI_VERSION_V1, 4(a0)
	move.w #PRVM_ROUTE_FRAME_SIZE, 6(a0)
	lea ProcessorAsmText, a1
	move.l a1, 8(a0)
	move.l #3, 12(a0)
	lea KindStatementText, a1
	move.l a1, 16(a0)
	move.l #9, 20(a0)
	move.l NativeCliSourceLineNum, 24(a0)
	lea NativeCliSourceLine, a1
	move.l a1, 28(a0)
	clr.l d0
	move.w NativeCliSourceLineLen, d0
	move.l d0, 32(a0)
	lea tokenRecordBuffer, a1
	move.l a1, 36(a0)
	clr.l d0
	move.w lastTokenCount, d0
	move.l d0, 40(a0)
	move.w #TOKEN_RECORD_SIZE, 44(a0)
	clr.w 46(a0)
	lea tokenScratchBuffer, a1
	move.l a1, 48(a0)
	clr.l d0
	move.w lastLexemeLen, d0
	move.l d0, 52(a0)
	bsr.w opforgeNativeCliLoadActivePrvmProgram
	tst.l d0
	bne.w done
	lea OpforgeNativeCliPrvmResultBuffer, a1
	movea.l a1, a0
	move.l #PRVM_ROUTE_RESULT_CAPACITY, d0
	bsr.w copy.clearBytes
	lea OpforgeNativeCliPrvmRouteFrame, a0
	lea OpforgeNativeCliPrvmResultBuffer, a1
	move.l a1, 64(a0)
	move.l #PRVM_ROUTE_RESULT_CAPACITY, 68(a0)
	lea OpforgeNativeCliPrvmDiagBuffer, a1
	move.l a1, 72(a0)
	move.l #PRVM_ROUTE_DIAG_CAPACITY, 76(a0)
	lea OpforgeNativeCliPrvmResumeBuffer, a1
	move.l a1, 80(a0)
	move.l #PRVM_ROUTE_RESUME_CAPACITY, 84(a0)
	lea OpforgeNativeCliPrvmExprRequest, a1
	move.l a1, 88(a0)
	move.l #PRVM_ROUTE_EXPR_REQUEST_SIZE, 92(a0)
	clr.l 96(a0)
	move.l #PRVM_ROUTE_EXPR_RESULT_COUNT, 100(a0)
	move.l #PRVM_PARSER_CONTRACT_VERSION_V2, 104(a0)
	move.l #PRVM_ROUTE_STEP_BUDGET, 108(a0)
	clr.l 112(a0)
	moveq #0, d0

done
	rts
	.bend  ; opforgeNativeCliBuildPrvmRouteFrame

opforgeNativeCliLoadActivePrvmProgram	.block
	movem.l d1-d4/a1-a4, -(sp)
	movea.l a0, a4
	lea ActiveParserVmOffsetLo, a1
	moveq #0, d0
	move.b (a1)+, d0
	moveq #0, d1
	move.b (a1)+, d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d2
	move.b (a1)+, d2
	moveq #0, d1
	move.b (a1)+, d1
	lsl.w #8, d1
	or.w d1, d2
	tst.w d2
	beq.s fail
	lea packageStorage, a2
	lea 0(a2, d0.W), a2
	movea.l a2, a3
	adda.l d2, a3
	moveq #1, d0
	bsr.w opforgeNativeCliActivePrvmRequireBytes
	tst.l d0
	bne.s fail
	addq.w #1, a2
	bsr.w opforgeNativeCliActivePrvmReadU32
	tst.l d1
	bne.s fail
	move.l d0, d3
	bsr.w opforgeNativeCliActivePrvmRequireBytes
	tst.l d0
	bne.s fail
	adda.l d3, a2
	moveq #2, d0
	bsr.w opforgeNativeCliActivePrvmRequireBytes
	tst.l d0
	bne.s fail
	moveq #0, d0
	move.b (a2)+, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.w #8, d1
	or.w d1, d0
	cmpi.w #PRVM_PARSER_CONTRACT_VERSION_V2, d0
	bne.s fail
	bsr.w opforgeNativeCliActivePrvmReadU32
	tst.l d1
	bne.s fail
	tst.l d0
	beq.s fail
	move.l d0, d3
	bsr.w opforgeNativeCliActivePrvmRequireBytes
	tst.l d0
	bne.s fail
	move.l a2, 56(a4)
	move.l d3, 60(a4)
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d4/a1-a4
	rts
	.bend  ; opforgeNativeCliLoadActivePrvmProgram

opforgeNativeCliActivePrvmReadU32	.block
	moveq #4, d0
	bsr.w opforgeNativeCliActivePrvmRequireBytes
	tst.l d0
	bne.s fail
	moveq #0, d0
	move.b (a2)+, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	rts

fail
	moveq #1, d1
	rts
	.bend  ; opforgeNativeCliActivePrvmReadU32

opforgeNativeCliActivePrvmRequireBytes	.block
	movea.l a2, a1
	adda.l d0, a1
	cmpa.l a3, a1
	bhi.s fail
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliActivePrvmRequireBytes

opforgeNativeCliParserMnemonicEquals	.block
	bsr.w opforgeNativeCliLineStartsWith
	rts
	.bend  ; opforgeNativeCliParserMnemonicEquals

	.endsection
	.endmodule

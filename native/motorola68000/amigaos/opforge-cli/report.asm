; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.report
	.cpu 68020

	.use opasm.amigaos.engine (opasmEngineStmtCount, opasmEngineSessionPass)
	.use opasm.amigaos.engine (opasmEngineSourceRecordCount, opasmEngineLabelCount)
	.use opasm.amigaos.engine (opasmEngineImageByteCount, opasmEngineSessionCpuName)
	.use opasm.amigaos.engine (opasmEngineSessionOrigin, opasmEngineSessionCurrentPc)

	.use opforge.cli.constants (*)
	.use opforge.cli.state (NativeCliSourceLineNum, NativeCliCurrentPath)
	.use opforge.cli.state (NativeCliIncludeDepth, NativeCliParseStatus)
	.use opforge.cli.state (NativeCliArgToken)
	.use opforge.cli.dos (opforgeNativeCliPutStr)
	.use opforge.cli.text_output (opforgeNativeCliPutDecU16, opforgeNativeCliPutHexU32)
	.use opforge.cli.strings (IncludeLineText, SpaceText, NewlineText)
	.use opforge.cli.strings (SessionStageText, SessionCpuText, SessionPassText)
	.use opforge.cli.strings (SessionOriginText, SessionPcText, SessionSourceCountText)
	.use opforge.cli.strings (SessionStmtCountText, SessionLabelCountText)
	.use opforge.cli.strings (SessionImageBytesText, SessionReadyText)
	.use opforge.cli.strings (UsageText, QuotedText, UnsupportedText)
	.use opforge.cli.strings (NativeSubsetHelpText, UnknownFlagText, MissingValueText)
	.use opforge.cli.strings (NoInputText, HunkRequiredText, MixedInputText)
	.use opforge.cli.strings (MultiplePositionalText, ModulePathCapacityText)

	.section code, kind=code
	.pub

opforgeNativeCliEmitIncludeLineRecord	.block
	movem.l d0-d1, -(sp)
	move.l #IncludeLineText, d1
	jsr opforgeNativeCliPutStr
	move.w NativeCliIncludeDepth, d0
	jsr opforgeNativeCliPutDecU16
	move.l #SpaceText, d1
	jsr opforgeNativeCliPutStr
	move.l NativeCliSourceLineNum, d0
	jsr opforgeNativeCliPutDecU16
	move.l #SpaceText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliCurrentPath, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	movem.l (sp)+, d0-d1
	rts
	.bend  ; opforgeNativeCliEmitIncludeLineRecord

opforgeNativeCliEmitAssemblySessionSummary	.block
	movem.l d0-d2/a0-a1, -(sp)
	move.l #SessionStageText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionCpuText, d1
	jsr opforgeNativeCliPutStr
	move.l #opasmEngineSessionCpuName, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionPassText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineSessionPass.l, d0
	jsr opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionOriginText, d1
	jsr opforgeNativeCliPutStr
	move.l opasmEngineSessionOrigin.l, d0
	jsr opforgeNativeCliPutHexU32
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionPcText, d1
	jsr opforgeNativeCliPutStr
	move.l opasmEngineSessionCurrentPc.l, d0
	jsr opforgeNativeCliPutHexU32
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionSourceCountText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineSourceRecordCount.l, d0
	jsr opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionStmtCountText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineStmtCount.l, d0
	jsr opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionLabelCountText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineLabelCount.l, d0
	jsr opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionImageBytesText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineImageByteCount.l, d0
	jsr opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionReadyText, d1
	jsr opforgeNativeCliPutStr
	movem.l (sp)+, d0-d2/a0-a1
	rts
	.bend  ; opforgeNativeCliEmitAssemblySessionSummary

opforgeNativeCliReportParseError	.block
	move.w NativeCliParseStatus, d0
	cmpi.w #NCLI_PARSE_QUOTED, d0
	beq.s opforgeNativeCliReportQuoted
	cmpi.w #NCLI_PARSE_UNSUPPORTED, d0
	beq.s opforgeNativeCliReportUnsupported
	cmpi.w #NCLI_PARSE_UNKNOWN_FLAG, d0
	beq.s opforgeNativeCliReportUnknown
	cmpi.w #NCLI_PARSE_MISSING_VALUE, d0
	beq.s opforgeNativeCliReportMissing
	cmpi.w #NCLI_PARSE_NO_INPUT, d0
	beq.w opforgeNativeCliReportNoInput
	cmpi.w #NCLI_PARSE_HUNK_REQUIRED, d0
	beq.w opforgeNativeCliReportHunkRequired
	cmpi.w #NCLI_PARSE_MIXED_INPUT, d0
	beq.w opforgeNativeCliReportMixedInput
	cmpi.w #NCLI_PARSE_MULTIPLE_POSITIONAL, d0
	beq.w opforgeNativeCliReportMultiplePositional
	cmpi.w #NCLI_PARSE_MODULE_PATH_CAPACITY, d0
	beq.w opforgeNativeCliReportModulePathCapacity
	move.l #UsageText, d1
	bra.w opforgeNativeCliReportText

opforgeNativeCliReportQuoted
	move.l #QuotedText, d1
	bra.w opforgeNativeCliReportText

opforgeNativeCliReportUnsupported
	move.l #UnsupportedText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliArgToken, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeSubsetHelpText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportUnknown
	move.l #UnknownFlagText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliArgToken, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportMissing
	move.l #MissingValueText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliArgToken, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportNoInput
	move.l #NoInputText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportHunkRequired
	move.l #HunkRequiredText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportMixedInput
	move.l #MixedInputText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportMultiplePositional
	move.l #MultiplePositionalText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportModulePathCapacity
	move.l #ModulePathCapacityText, d1

opforgeNativeCliReportText
	jsr opforgeNativeCliPutStr
	rts
	.bend  ; opforgeNativeCliReportParseError

	.endsection
	.endmodule

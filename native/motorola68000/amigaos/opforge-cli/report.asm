; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.report
	.cpu 68020

	.use opasm.amigaos.engine (opasmEngineGetImageByteCountV1, opasmEngineGetSessionCpuNamePtrV1)
	.use opasm.amigaos.engine (opasmEngineGetSessionPassV1, opasmEngineGetSessionOriginV1)
	.use opasm.amigaos.engine (opasmEngineGetSessionCurrentPcV1, opasmEngineGetSourceRecordCountV1)
	.use opasm.amigaos.engine (opasmEngineGetStatementCountV1, opasmEngineGetLabelCountV1)

	.use opforge.cli.constants (NCLI_PARSE_QUOTED, NCLI_PARSE_UNSUPPORTED)
	.use opforge.cli.constants (NCLI_PARSE_UNKNOWN_FLAG, NCLI_PARSE_MISSING_VALUE)
	.use opforge.cli.constants (NCLI_PARSE_NO_INPUT, NCLI_PARSE_HUNK_REQUIRED)
	.use opforge.cli.constants (NCLI_PARSE_MIXED_INPUT, NCLI_PARSE_MULTIPLE_POSITIONAL)
	.use opforge.cli.constants (NCLI_PARSE_MODULE_PATH_CAPACITY)
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
	jsr opasmEngineGetSessionCpuNamePtrV1
	move.l a0, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionPassText, d1
	jsr opforgeNativeCliPutStr
	jsr opasmEngineGetSessionPassV1
	jsr opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionOriginText, d1
	jsr opforgeNativeCliPutStr
	jsr opasmEngineGetSessionOriginV1
	jsr opforgeNativeCliPutHexU32
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionPcText, d1
	jsr opforgeNativeCliPutStr
	jsr opasmEngineGetSessionCurrentPcV1
	jsr opforgeNativeCliPutHexU32
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionSourceCountText, d1
	jsr opforgeNativeCliPutStr
	jsr opasmEngineGetSourceRecordCountV1
	jsr opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionStmtCountText, d1
	jsr opforgeNativeCliPutStr
	jsr opasmEngineGetStatementCountV1
	jsr opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionLabelCountText, d1
	jsr opforgeNativeCliPutStr
	jsr opasmEngineGetLabelCountV1
	jsr opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionImageBytesText, d1
	jsr opforgeNativeCliPutStr
	jsr opasmEngineGetImageByteCountV1
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
	beq.s quoted
	cmpi.w #NCLI_PARSE_UNSUPPORTED, d0
	beq.s unsupported
	cmpi.w #NCLI_PARSE_UNKNOWN_FLAG, d0
	beq.s unknown
	cmpi.w #NCLI_PARSE_MISSING_VALUE, d0
	beq.s missing
	cmpi.w #NCLI_PARSE_NO_INPUT, d0
	beq.w noInput
	cmpi.w #NCLI_PARSE_HUNK_REQUIRED, d0
	beq.w hunkRequired
	cmpi.w #NCLI_PARSE_MIXED_INPUT, d0
	beq.w mixedInput
	cmpi.w #NCLI_PARSE_MULTIPLE_POSITIONAL, d0
	beq.w multiplePositional
	cmpi.w #NCLI_PARSE_MODULE_PATH_CAPACITY, d0
	beq.w modulePathCapacity
	move.l #UsageText, d1
	bra.w reportText

quoted
	move.l #QuotedText, d1
	bra.w reportText

unsupported
	move.l #UnsupportedText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliArgToken, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeSubsetHelpText, d1
	bra.s reportText

unknown
	move.l #UnknownFlagText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliArgToken, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bra.s reportText

missing
	move.l #MissingValueText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliArgToken, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bra.s reportText

noInput
	move.l #NoInputText, d1
	bra.s reportText

hunkRequired
	move.l #HunkRequiredText, d1
	bra.s reportText

mixedInput
	move.l #MixedInputText, d1
	bra.s reportText

multiplePositional
	move.l #MultiplePositionalText, d1
	bra.s reportText

modulePathCapacity
	move.l #ModulePathCapacityText, d1

reportText
	jsr opforgeNativeCliPutStr
	rts
	.bend  ; opforgeNativeCliReportParseError

	.endsection
	.endmodule

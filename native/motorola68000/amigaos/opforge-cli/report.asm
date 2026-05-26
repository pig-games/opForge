; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.report
	.cpu 68020

	.use opasm.amigaos.engine

	.use opforge.cli.constants
	.use opforge.cli.state
	.use opforge.cli.dos
	.use opforge.cli.opasm_event_report
	.use opforge.cli.text_output
	.use opforge.cli.strings

	.section code, kind=code
	.pub

opforgeNativeCliEmitIncludeLineRecord	.block
	movem.l d0-d1, -(sp)
	move.l #strings.IncludeLineText, d1
	jsr dos.putStr
	move.w state.NativeCliIncludeDepth, d0
	jsr text_output.opforgeNativeCliPutDecU16
	move.l #strings.SpaceText, d1
	jsr dos.putStr
	move.l state.NativeCliSourceLineNum, d0
	jsr text_output.opforgeNativeCliPutDecU16
	move.l #strings.SpaceText, d1
	jsr dos.putStr
	move.l #state.NativeCliCurrentPath, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	movem.l (sp)+, d0-d1
	rts
	.bend  ; opforgeNativeCliEmitIncludeLineRecord

opforgeNativeCliEmitAssemblySessionSummary	.block
	movem.l d0-d2/a0-a1, -(sp)
	move.l #strings.SessionStageText, d1
	jsr dos.putStr
	move.l #strings.SessionCpuText, d1
	jsr dos.putStr
	jsr engine.opasmEngineGetSessionCpuNamePtrV1
	move.l a0, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	move.l #strings.SessionPassText, d1
	jsr dos.putStr
	jsr engine.opasmEngineGetSessionPassV1
	jsr text_output.opforgeNativeCliPutDecU16
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	move.l #strings.SessionOriginText, d1
	jsr dos.putStr
	jsr engine.opasmEngineGetSessionOriginV1
	jsr text_output.opforgeNativeCliPutHexU32
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	move.l #strings.SessionPcText, d1
	jsr dos.putStr
	jsr engine.opasmEngineGetSessionCurrentPcV1
	jsr text_output.opforgeNativeCliPutHexU32
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	move.l #strings.SessionSourceCountText, d1
	jsr dos.putStr
	jsr engine.opasmEngineGetSourceRecordCountV1
	jsr text_output.opforgeNativeCliPutDecU16
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	move.l #strings.SessionStmtCountText, d1
	jsr dos.putStr
	jsr engine.opasmEngineGetStatementCountV1
	jsr text_output.opforgeNativeCliPutDecU16
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	move.l #strings.SessionLabelCountText, d1
	jsr dos.putStr
	jsr engine.opasmEngineGetLabelCountV1
	jsr text_output.opforgeNativeCliPutDecU16
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	move.l #strings.SessionImageBytesText, d1
	jsr dos.putStr
	jsr engine.opasmEngineGetImageByteCountV1
	jsr text_output.opforgeNativeCliPutDecU16
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	move.l #strings.SessionReadyText, d1
	jsr dos.putStr
	movem.l (sp)+, d0-d2/a0-a1
	rts
	.bend  ; opforgeNativeCliEmitAssemblySessionSummary

opforgeNativeCliReportParseError	.block
	move.w state.NativeCliParseStatus, d0
	cmpi.w #constants.NCLI_PARSE_QUOTED, d0
	beq.s quoted
	cmpi.w #constants.NCLI_PARSE_UNSUPPORTED, d0
	beq.s unsupported
	cmpi.w #constants.NCLI_PARSE_UNKNOWN_FLAG, d0
	beq.s unknown
	cmpi.w #constants.NCLI_PARSE_MISSING_VALUE, d0
	beq.s missing
	cmpi.w #constants.NCLI_PARSE_NO_INPUT, d0
	beq.w noInput
	cmpi.w #constants.NCLI_PARSE_HUNK_REQUIRED, d0
	beq.w hunkRequired
	cmpi.w #constants.NCLI_PARSE_MIXED_INPUT, d0
	beq.w mixedInput
	cmpi.w #constants.NCLI_PARSE_MULTIPLE_POSITIONAL, d0
	beq.w multiplePositional
	cmpi.w #constants.NCLI_PARSE_MODULE_PATH_CAPACITY, d0
	beq.w modulePathCapacity
	move.l #strings.UsageText, d1
	bra.w reportText

quoted
	move.l #strings.QuotedText, d1
	bra.w reportText

unsupported
	move.l #strings.UnsupportedText, d1
	jsr dos.putStr
	move.l #state.NativeCliArgToken, d1
	jsr dos.putStr
	move.l #strings.NativeSubsetHelpText, d1
	bra.s reportText

unknown
	move.l #strings.UnknownFlagText, d1
	jsr dos.putStr
	move.l #state.NativeCliArgToken, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	bra.s reportText

missing
	move.l #strings.MissingValueText, d1
	jsr dos.putStr
	move.l #state.NativeCliArgToken, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	bra.s reportText

noInput
	move.l #strings.NoInputText, d1
	bra.s reportText

hunkRequired
	move.l #strings.HunkRequiredText, d1
	bra.s reportText

mixedInput
	move.l #strings.MixedInputText, d1
	bra.s reportText

multiplePositional
	move.l #strings.MultiplePositionalText, d1
	bra.s reportText

modulePathCapacity
	move.l #strings.ModulePathCapacityText, d1

reportText
	jsr dos.putStr
	rts
	.bend  ; opforgeNativeCliReportParseError

	.endsection
	.endmodule

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
	jsr text_output.opforgeNativeCliPutU16Decimal
	move.l #strings.SpaceText, d1
	jsr dos.putStr
	move.l state.NativeCliSourceLineNum, d0
	jsr text_output.opforgeNativeCliPutU16Decimal
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
	jsr text_output.opforgeNativeCliPutU16Decimal
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
	jsr text_output.opforgeNativeCliPutU16Decimal
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	move.l #strings.SessionStmtCountText, d1
	jsr dos.putStr
	jsr engine.opasmEngineGetStatementCountV1
	jsr text_output.opforgeNativeCliPutU16Decimal
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	move.l #strings.SessionLabelCountText, d1
	jsr dos.putStr
	jsr engine.opasmEngineGetLabelCountV1
	jsr text_output.opforgeNativeCliPutU16Decimal
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	move.l #strings.SessionImageBytesText, d1
	jsr dos.putStr
	jsr engine.opasmEngineGetImageByteCountV1
	jsr text_output.opforgeNativeCliPutU16Decimal
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	move.l #strings.SessionReadyText, d1
	jsr dos.putStr
	movem.l (sp)+, d0-d2/a0-a1
	rts
	.bend  ; opforgeNativeCliEmitAssemblySessionSummary

opforgeNativeCliEmitAssemblySessionStatementDump	.block
	movem.l d0-d3/a0-a1, -(sp)
	jsr engine.opasmEngineGetStatementCountV1
	tst.l d0
	beq.w done
	move.l d0, d3
	clr.l d2

loop
	move.l #strings.SessionStmtDumpText, d1
	jsr dos.putStr
	move.w d2, d0
	jsr text_output.opforgeNativeCliPutU16Decimal
	jsr text_output.opforgeNativeCliPutSpace
	move.w d2, d0
	jsr engine.opasmEngineGetStatementLineNumberV1
	jsr text_output.opforgeNativeCliPutU16Decimal
	jsr text_output.opforgeNativeCliPutSpace
	suba.l #engine.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	move.w d2, d0
	jsr engine.opasmEngineGetStatementTextMetadataV1
	bne.s noMnem
	movea.l sp, a0
	move.l engine.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(a0), d1
	jsr dos.putStr
	move.l engine.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(a0), d0
	beq.s dumpDone
	jsr text_output.opforgeNativeCliPutSpace
	movea.l sp, a0
	move.l engine.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(a0), d1
	jsr dos.putStr
	bra.s dumpDone

noMnem
	move.l #strings.SessionStmtNoMnemText, d1
	jsr dos.putStr

dumpDone
	adda.l #engine.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	addq.l #1, d2
	cmp.l d3, d2
	bcs.w loop

done
	movem.l (sp)+, d0-d3/a0-a1
	rts
	.bend  ; opforgeNativeCliEmitAssemblySessionStatementDump

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

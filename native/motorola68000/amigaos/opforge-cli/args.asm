; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.args
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.state
	.use opforge.cli.strings
	.use opforge.cli.dos
	.use opforge.cli.path
	.use opforge.cli.text_output
	.use opforge.cli.token_util

	.section code, kind=code
	.pub

; ---------------------------------------------------------------------------
; Parse the native CLI argument tail into fixed buffers and request flags.
;
; Inputs:
; - A0: DOS GetArgStr-style argument tail pointer.
;
; Outputs:
; - D0: 0 on parse completion; state.NativeCliParseStatus records OK/help/version.
; - state.* argument buffers and request flags updated from the parsed tail.
;
; Clobbers:
; - D0-D7/A0-A6/CCR
;
; CCR:
; - Reflects D0 on return.
; ---------------------------------------------------------------------------
opforgeNativeCliParseArgs	.block
	movem.l d2-d7/a2-a6, -(sp)
	movea.l a0, a3  ; A3 walks the AmigaDOS argument tail in-place
	clr.w state.NativeCliInputStyle
	clr.w state.NativeCliHunkRequested
	clr.w state.NativeCliBinRequested
	clr.w state.NativeCliOutputFormat
	clr.w state.NativeCliParseStatus
	clr.b state.NativeCliInputPath
	clr.b state.NativeCliHunkPath
	clr.b state.NativeCliBinPath
	clr.b state.NativeCliOutfileBase
	clr.b state.NativeCliCpuName
	clr.b state.NativeCliPackagePath
	move.w #1, state.NativeCliModulePathCount

parseLoop
	bsr.w opforgeNativeCliSkipWhitespace
	tst.b (a3)
	beq.w parseDone
	cmpi.b #'"', (a3)
	beq.w quoted
	lea state.NativeCliArgToken, a1
	bsr.w opforgeNativeCliCopyToken
	bne.w usage

	lea state.NativeCliArgToken, a0
	lea strings.FlagHelpLong, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w parseHelp
	lea state.NativeCliArgToken, a0
	lea strings.FlagHelpShort, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w parseHelp
	lea state.NativeCliArgToken, a0
	lea strings.FlagVersionLong, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w parseVersion
	lea state.NativeCliArgToken, a0
	lea strings.FlagVersionShort, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w parseVersion
	lea state.NativeCliArgToken, a0
	lea strings.FlagInfileShort, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w infile
	lea state.NativeCliArgToken, a0
	lea strings.FlagInfileLong, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w infile
	lea state.NativeCliArgToken, a0
	lea strings.FlagHunkLong, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w hunk
	lea state.NativeCliArgToken, a0
	lea strings.FlagBinShort, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w bin
	lea state.NativeCliArgToken, a0
	lea strings.FlagBinLong, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w bin
	lea state.NativeCliArgToken, a0
	lea strings.FlagOutfileShort, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w outfile
	lea state.NativeCliArgToken, a0
	lea strings.FlagOutfileLong, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w outfile
	lea state.NativeCliArgToken, a0
	lea strings.FlagCpuLong, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w cpu
	lea state.NativeCliArgToken, a0
	lea strings.FlagPackageLong, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w package
	lea state.NativeCliArgToken, a0
	lea strings.FlagModuleShort, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w modulePath
	lea state.NativeCliArgToken, a0
	lea strings.FlagModuleLong, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w modulePath
	bsr.w opforgeNativeCliIsUnsupportedFlag
	bne.w unsupported
	lea state.NativeCliArgToken, a0
	cmpi.b #'-', (a0)
	beq.w unknownFlag
	bra.w positionalInput

infile
	tst.w state.NativeCliInputStyle
	beq.s infileFirst
	cmpi.w #1, state.NativeCliInputStyle
	beq.w mixedInput
	bra.w usage

infileFirst
	move.w #2, state.NativeCliInputStyle
	lea state.NativeCliInputPath, a1
	bsr.w opforgeNativeCliCopyRequiredValue
	bne.w missingValue
	bra.w parseLoop

hunk
	move.w #1, state.NativeCliHunkRequested
	move.w #constants.NATIVE_OUTPUT_FORMAT_HUNK, state.NativeCliOutputFormat
	lea state.NativeCliHunkPath, a1
	bsr.w opforgeNativeCliCopyOptionalValue
	bmi.w quoted
	bra.w parseLoop

bin
	move.w #1, state.NativeCliBinRequested
	move.w #constants.NATIVE_OUTPUT_FORMAT_BIN, state.NativeCliOutputFormat
	lea state.NativeCliBinPath, a1
	bsr.w opforgeNativeCliCopyOptionalValue
	bmi.w quoted
	bra.w parseLoop

outfile
	lea state.NativeCliOutfileBase, a1
	bsr.w opforgeNativeCliCopyRequiredValue
	bne.w missingValue
	bra.w parseLoop

cpu
	lea state.NativeCliCpuName, a1
	bsr.w opforgeNativeCliCopyRequiredValue
	bne.w missingValue
	bra.w parseLoop

package
	lea state.NativeCliPackagePath, a1
	bsr.w opforgeNativeCliCopyRequiredValue
	bne.w missingValue
	bra.w parseLoop

modulePath
	lea state.NativeCliIncludeTarget, a1
	bsr.w opforgeNativeCliCopyRequiredPathValue
	cmpi.l #1, d0
	beq.w missingValue
	tst.l d0
	bne.w modulePathCapacity
	bsr.w opforgeNativeCliRecordModulePathValue
	bne.w modulePathCapacity
	bra.w parseLoop

positionalInput
	tst.w state.NativeCliInputStyle
	beq.s positionalInputFirst
	cmpi.w #2, state.NativeCliInputStyle
	beq.w mixedInput
	bra.w multiplePositional

positionalInputFirst
	move.w #1, state.NativeCliInputStyle
	lea state.NativeCliArgToken, a0
	lea state.NativeCliInputPath, a1
	jsr token_util.opforgeNativeCliCopyTokenBuffer
	bra.w parseLoop

parseDone
	tst.w state.NativeCliInputStyle
	beq.w noInput
	tst.w state.NativeCliOutputFormat
	beq.w hunkRequired
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_BIN, state.NativeCliOutputFormat
	beq.s defaultBinPath
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_HUNK, state.NativeCliOutputFormat
	beq.s defaultHunkPath
	bra.w usage

defaultBinPath
	tst.b state.NativeCliBinPath
	bne.s parseOk
	tst.b state.NativeCliOutfileBase
	beq.s parseOk
	lea state.NativeCliOutfileBase, a0
	lea state.NativeCliBinPath, a1
	jsr token_util.opforgeNativeCliCopyTokenBuffer
	bra.s parseOk

defaultHunkPath
	tst.b state.NativeCliHunkPath
	bne.s parseOk
	tst.b state.NativeCliOutfileBase
	beq.s parseOk
	lea state.NativeCliOutfileBase, a0
	lea state.NativeCliHunkPath, a1
	jsr token_util.opforgeNativeCliCopyTokenBuffer

parseOk
	bsr.w opforgeNativeCliSeedModulePathRootFromInput
	bne.w modulePathCapacity
	move.w #constants.NCLI_PARSE_OK, state.NativeCliParseStatus
	bra.w parseReturn

parseHelp
	move.w #constants.NCLI_PARSE_HELP, state.NativeCliParseStatus
	bra.w parseReturn

parseVersion
	move.w #constants.NCLI_PARSE_VERSION, state.NativeCliParseStatus
	bra.w parseReturn

usage
	move.w #constants.NCLI_PARSE_USAGE, state.NativeCliParseStatus
	bra.w parseReturn

quoted
	move.w #constants.NCLI_PARSE_QUOTED, state.NativeCliParseStatus
	bra.w parseReturn

unsupported
	move.w #constants.NCLI_PARSE_UNSUPPORTED, state.NativeCliParseStatus
	bra.w parseReturn

unknownFlag
	move.w #constants.NCLI_PARSE_UNKNOWN_FLAG, state.NativeCliParseStatus
	bra.w parseReturn

missingValue
	move.w #constants.NCLI_PARSE_MISSING_VALUE, state.NativeCliParseStatus
	bra.w parseReturn

noInput
	move.w #constants.NCLI_PARSE_NO_INPUT, state.NativeCliParseStatus
	bra.w parseReturn

hunkRequired
	move.w #constants.NCLI_PARSE_HUNK_REQUIRED, state.NativeCliParseStatus
	bra.w parseReturn

mixedInput
	move.w #constants.NCLI_PARSE_MIXED_INPUT, state.NativeCliParseStatus
	bra.w parseReturn

multiplePositional
	move.w #constants.NCLI_PARSE_MULTIPLE_POSITIONAL, state.NativeCliParseStatus
	bra.w parseReturn

modulePathCapacity
	move.w #constants.NCLI_PARSE_MODULE_PATH_CAPACITY, state.NativeCliParseStatus

parseReturn
	move.w state.NativeCliParseStatus, d0
	ext.l d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; opforgeNativeCliParseArgs

opforgeNativeCliEmitModulePathRecords	.block
	movem.l d0-d4/a0, -(sp)
	clr.w d4

emitLoop
	move.w state.NativeCliModulePathCount, d0
	cmp.w d0, d4
	bhs.s emitDone
	move.l #strings.ModPathText, d1
	jsr dos.putStr
	moveq #0, d0
	move.w d4, d0
	jsr text_output.opforgeNativeCliPutU16Decimal
	jsr text_output.opforgeNativeCliPutSpace
	moveq #0, d0
	move.w d4, d0
	lsl.l #8, d0
	lea state.NativeCliModulePathTable, a0
	adda.l d0, a0
	move.l a0, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	addq.w #1, d4
	bra.s emitLoop

emitDone
	movem.l (sp)+, d0-d4/a0
	rts
	.bend  ; opforgeNativeCliEmitModulePathRecords

	.priv

; Skip ASCII whitespace in the DOS argument tail.
; Inputs: A3 = current argument-tail pointer.
; Outputs: A3 advanced past spaces/tabs/newlines/carriage returns.
; Clobbers: CCR.
; CCR: reflects the final delimiter compare, not a status result.
opforgeNativeCliSkipWhitespace	.block
	cmpi.b #' ', (a3)
	beq.s skipOne
	cmpi.b #9, (a3)
	beq.s skipOne
	cmpi.b #10, (a3)
	beq.s skipOne
	cmpi.b #13, (a3)
	bne.s skipDone

skipOne
	addq.l #1, a3
	bra.s opforgeNativeCliSkipWhitespace

skipDone
	rts
	.bend  ; opforgeNativeCliSkipWhitespace

; Copy one unquoted CLI token from the DOS argument tail.
; Inputs: A3 = current argument-tail pointer; A1 = destination token buffer.
; Outputs: D0 = 0 on success, 1 on malformed/overflow token; A3 advanced past copied token on success; destination buffer NUL-terminated on success.
; Clobbers: D0/D6/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliCopyToken	.block
	move.l #constants.TOKEN_BUFFER_CAPACITY - 1, d6

tokenLoop
	moveq #0, d0
	move.b (a3), d0
	beq.s tokenDone
	cmpi.b #' ', d0
	beq.s tokenDone
	cmpi.b #9, d0
	beq.s tokenDone
	cmpi.b #10, d0
	beq.s tokenDone
	cmpi.b #13, d0
	beq.s tokenDone
	cmpi.b #'"', d0
	beq.s tokenFail
	tst.l d6
	beq.s tokenFail
	move.b d0, (a1)+
	addq.l #1, a3
	subq.l #1, d6
	bra.s tokenLoop

tokenDone
	clr.b (a1)
	moveq #0, d0
	rts

tokenFail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliCopyToken

; Copy one required unquoted CLI value.
; Inputs: A3 = current argument-tail pointer; A1 = destination token buffer.
; Outputs: D0 = 0 on success, 1 when the value is missing or quoted; A3 advanced past leading whitespace and the copied token on success.
; Clobbers: D0/D6/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliCopyRequiredValue	.block
	bsr.w opforgeNativeCliSkipWhitespace
	tst.b (a3)
	beq.s requiredMissing
	cmpi.b #'"', (a3)
	beq.s requiredMissing
	bsr.w opforgeNativeCliCopyToken
	rts

requiredMissing
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliCopyRequiredValue

; Copy one required module/package path value.
; Inputs: A3 = current argument-tail pointer; A1 = destination path buffer.
; Outputs: D0 = 0 on success, 1 when the value is missing/quoted, 2 when it exceeds the path buffer; A3 advanced past the copied path on success.
; Clobbers: D0/D6/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliCopyRequiredPathValue	.block
	bsr.w opforgeNativeCliSkipWhitespace
	tst.b (a3)
	beq.s pathMissing
	cmpi.b #'"', (a3)
	beq.s pathMissing
	move.l #constants.PATH_BUFFER_CAPACITY - 1, d6

pathLoop
	moveq #0, d0
	move.b (a3), d0
	beq.s pathDone
	cmpi.b #' ', d0
	beq.s pathDone
	cmpi.b #9, d0
	beq.s pathDone
	cmpi.b #10, d0
	beq.s pathDone
	cmpi.b #13, d0
	beq.s pathDone
	cmpi.b #'"', d0
	beq.s pathCapacity
	tst.l d6
	beq.s pathCapacity
	move.b d0, (a1)+
	addq.l #1, a3
	subq.l #1, d6
	bra.s pathLoop

pathDone
	clr.b (a1)
	moveq #0, d0
	rts

pathMissing
	moveq #1, d0
	rts

pathCapacity
	moveq #2, d0
	rts
	.bend  ; opforgeNativeCliCopyRequiredPathValue

; Copy one optional CLI value when the next token is a value, not another flag.
; Inputs: A3 = current argument-tail pointer; A1 = destination token buffer.
; Outputs: D0 = 0 when no value or a copied value is accepted, -1 when a quoted value would be required; destination buffer cleared when no value is consumed.
; Clobbers: D0/D6/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliCopyOptionalValue	.block
	bsr.w opforgeNativeCliSkipWhitespace
	tst.b (a3)
	beq.s optionalNone
	cmpi.b #'"', (a3)
	beq.s optionalQuoted
	cmpi.b #'-', (a3)
	beq.s optionalNone
	bsr.w opforgeNativeCliCopyToken
	rts

optionalNone
	clr.b (a1)
	moveq #0, d0
	rts

optionalQuoted
	moveq #-1, d0
	rts
	.bend  ; opforgeNativeCliCopyOptionalValue

; Check whether the current parsed flag is one of the known-but-unsupported CLI options.
; Inputs: state.NativeCliArgToken = current parsed flag token.
; Outputs: D0 = 1 when the flag is recognized as unsupported, 0 otherwise.
; Clobbers: D0/A0-A1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliIsUnsupportedFlag	.block
	lea state.NativeCliArgToken, a0
	lea strings.FlagListShort, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w unsupportedYes
	lea state.NativeCliArgToken, a0
	lea strings.FlagListLong, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w unsupportedYes
	lea state.NativeCliArgToken, a0
	lea strings.FlagHexShort, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w unsupportedYes
	lea state.NativeCliArgToken, a0
	lea strings.FlagHexLong, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w unsupportedYes
	lea state.NativeCliArgToken, a0
	lea strings.FlagSrecShort, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w unsupportedYes
	lea state.NativeCliArgToken, a0
	lea strings.FlagSrecLong, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w unsupportedYes
	lea state.NativeCliArgToken, a0
	lea strings.FlagDefineShort, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w unsupportedYes
	lea state.NativeCliArgToken, a0
	lea strings.FlagDefineLong, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w unsupportedYes
	lea state.NativeCliArgToken, a0
	lea strings.FlagIncludeShort, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w unsupportedYes
	lea state.NativeCliArgToken, a0
	lea strings.FlagIncludeLong, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.w unsupportedYes
	moveq #0, d0
	rts

unsupportedYes
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliIsUnsupportedFlag

; Seed module path slot 0 from the input file root.
; Inputs: state.NativeCliInputPath = parsed input path.
; Outputs: D0 = 0 on success, non-zero on path-copy failure; state.NativeCliModulePathTable[0] = input path root on success.
; Clobbers: D0/A0-A1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliSeedModulePathRootFromInput	.block
	lea state.NativeCliInputPath, a0
	lea state.NativeCliModulePathTable, a1
	jsr path.opforgeNativeCliCopyPathRoot
	rts
	.bend  ; opforgeNativeCliSeedModulePathRootFromInput

; Append one explicit `-M` / `--module-path` value to the module path table.
; Inputs: state.NativeCliIncludeTarget = parsed module path; state.NativeCliModulePathCount = current table length.
; Outputs: D0 = 0 on success, 1 on capacity/path-copy failure; module path table and count updated on success.
; Clobbers: D0-D1/A0-A1/CCR.
; CCR: reflects D0 on return. The epilogue restores saved registers with CCR-neutral `movem`/`rts`.
opforgeNativeCliRecordModulePathValue	.block
	movem.l d1/a0-a1, -(sp)
	moveq #0, d0
	move.w state.NativeCliModulePathCount, d0
	cmpi.w #constants.NATIVE_MODULE_PATH_CAPACITY, d0
	bhs.s recordFail
	move.l d0, d1
	lsl.l #8, d1
	lea state.NativeCliModulePathTable, a1
	adda.l d1, a1
	lea state.NativeCliIncludeTarget, a0
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.s recordFail
	move.w state.NativeCliModulePathCount, d0
	addq.w #1, d0
	move.w d0, state.NativeCliModulePathCount
	moveq #0, d0
	bra.s recordReturn

recordFail
	moveq #1, d0

recordReturn
	movem.l (sp)+, d1/a0-a1
	rts
	.bend  ; opforgeNativeCliRecordModulePathValue

	.endsection
	.endmodule

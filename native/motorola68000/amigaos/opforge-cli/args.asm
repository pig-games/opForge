; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.args
	.cpu 68020

	.use opforge.cli.constants (TOKEN_BUFFER_CAPACITY, PATH_BUFFER_CAPACITY, NATIVE_MODULE_PATH_CAPACITY)
	.use opforge.cli.constants (NATIVE_OUTPUT_FORMAT_BIN, NATIVE_OUTPUT_FORMAT_HUNK)
	.use opforge.cli.constants (NCLI_PARSE_OK, NCLI_PARSE_HELP, NCLI_PARSE_VERSION)
	.use opforge.cli.constants (NCLI_PARSE_USAGE, NCLI_PARSE_QUOTED, NCLI_PARSE_UNSUPPORTED)
	.use opforge.cli.constants (NCLI_PARSE_UNKNOWN_FLAG, NCLI_PARSE_MISSING_VALUE)
	.use opforge.cli.constants (NCLI_PARSE_NO_INPUT, NCLI_PARSE_HUNK_REQUIRED)
	.use opforge.cli.constants (NCLI_PARSE_MIXED_INPUT, NCLI_PARSE_MULTIPLE_POSITIONAL)
	.use opforge.cli.constants (NCLI_PARSE_MODULE_PATH_CAPACITY)
	.use opforge.cli.state (NativeCliInputStyle, NativeCliHunkRequested, NativeCliBinRequested)
	.use opforge.cli.state (NativeCliOutputFormat, NativeCliParseStatus, NativeCliArgToken)
	.use opforge.cli.state (NativeCliInputPath, NativeCliHunkPath, NativeCliBinPath)
	.use opforge.cli.state (NativeCliOutfileBase, NativeCliCpuName, NativeCliPackagePath)
	.use opforge.cli.state (NativeCliIncludeTarget, NativeCliModulePathCount, NativeCliModulePathTable)
	.use opforge.cli.strings (FlagHelpLong, FlagHelpShort, FlagVersionLong, FlagVersionShort)
	.use opforge.cli.strings (FlagInfileShort, FlagInfileLong, FlagHunkLong)
	.use opforge.cli.strings (FlagBinShort, FlagBinLong, FlagOutfileShort, FlagOutfileLong)
	.use opforge.cli.strings (FlagCpuLong, FlagPackageLong, FlagModuleShort, FlagModuleLong)
	.use opforge.cli.strings (FlagListShort, FlagListLong, FlagHexShort, FlagHexLong)
	.use opforge.cli.strings (FlagSrecShort, FlagSrecLong, FlagDefineShort, FlagDefineLong)
	.use opforge.cli.strings (FlagIncludeShort, FlagIncludeLong, ModPathText)
	.use opforge.cli.strings (NewlineText)
	.use opforge.cli.dos (opforgeNativeCliPutStr)
	.use opforge.cli.path (opforgeNativeCliCopyPathRoot, opforgeNativeCliCopyPathBuffer)
	.use opforge.cli.text_output (opforgeNativeCliPutDecU16, opforgeNativeCliPutSpace)
	.use opforge.cli.token_util (opforgeNativeCliTokenEquals, opforgeNativeCliCopyTokenBuffer)

	.section code, kind=code
	.pub

; Parse the native CLI argument tail into fixed buffers and request flags.
opforgeNativeCliParseArgs	.block
	movem.l d2-d7/a2-a6, -(sp)
	movea.l a0, a3  ; A3 walks the AmigaDOS argument tail in-place
	clr.w NativeCliInputStyle
	clr.w NativeCliHunkRequested
	clr.w NativeCliBinRequested
	clr.w NativeCliOutputFormat
	clr.w NativeCliParseStatus
	clr.b NativeCliInputPath
	clr.b NativeCliHunkPath
	clr.b NativeCliBinPath
	clr.b NativeCliOutfileBase
	clr.b NativeCliCpuName
	clr.b NativeCliPackagePath
	move.w #1, NativeCliModulePathCount

parseLoop
	bsr.w opforgeNativeCliSkipWhitespace
	tst.b (a3)
	beq.w parseDone
	cmpi.b #'"', (a3)
	beq.w quoted
	lea NativeCliArgToken, a1
	bsr.w opforgeNativeCliCopyToken
	tst.l d0
	bne.w usage

	lea NativeCliArgToken, a0
	lea FlagHelpLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w parseHelp
	lea NativeCliArgToken, a0
	lea FlagHelpShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w parseHelp
	lea NativeCliArgToken, a0
	lea FlagVersionLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w parseVersion
	lea NativeCliArgToken, a0
	lea FlagVersionShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w parseVersion
	lea NativeCliArgToken, a0
	lea FlagInfileShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w infile
	lea NativeCliArgToken, a0
	lea FlagInfileLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w infile
	lea NativeCliArgToken, a0
	lea FlagHunkLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w hunk
	lea NativeCliArgToken, a0
	lea FlagBinShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w bin
	lea NativeCliArgToken, a0
	lea FlagBinLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w bin
	lea NativeCliArgToken, a0
	lea FlagOutfileShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w outfile
	lea NativeCliArgToken, a0
	lea FlagOutfileLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w outfile
	lea NativeCliArgToken, a0
	lea FlagCpuLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w cpu
	lea NativeCliArgToken, a0
	lea FlagPackageLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w package
	lea NativeCliArgToken, a0
	lea FlagModuleShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w modulePath
	lea NativeCliArgToken, a0
	lea FlagModuleLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w modulePath
	bsr.w opforgeNativeCliIsUnsupportedFlag
	tst.l d0
	bne.w unsupported
	lea NativeCliArgToken, a0
	cmpi.b #'-', (a0)
	beq.w unknownFlag
	bra.w positionalInput

infile
	tst.w NativeCliInputStyle
	beq.s infileFirst
	cmpi.w #1, NativeCliInputStyle
	beq.w mixedInput
	bra.w usage

infileFirst
	move.w #2, NativeCliInputStyle
	lea NativeCliInputPath, a1
	bsr.w opforgeNativeCliCopyRequiredValue
	tst.l d0
	bne.w missingValue
	bra.w parseLoop

hunk
	move.w #1, NativeCliHunkRequested
	move.w #NATIVE_OUTPUT_FORMAT_HUNK, NativeCliOutputFormat
	lea NativeCliHunkPath, a1
	bsr.w opforgeNativeCliCopyOptionalValue
	tst.l d0
	bmi.w quoted
	bra.w parseLoop

bin
	move.w #1, NativeCliBinRequested
	move.w #NATIVE_OUTPUT_FORMAT_BIN, NativeCliOutputFormat
	lea NativeCliBinPath, a1
	bsr.w opforgeNativeCliCopyOptionalValue
	tst.l d0
	bmi.w quoted
	bra.w parseLoop

outfile
	lea NativeCliOutfileBase, a1
	bsr.w opforgeNativeCliCopyRequiredValue
	tst.l d0
	bne.w missingValue
	bra.w parseLoop

cpu
	lea NativeCliCpuName, a1
	bsr.w opforgeNativeCliCopyRequiredValue
	tst.l d0
	bne.w missingValue
	bra.w parseLoop

package
	lea NativeCliPackagePath, a1
	bsr.w opforgeNativeCliCopyRequiredValue
	tst.l d0
	bne.w missingValue
	bra.w parseLoop

modulePath
	lea NativeCliIncludeTarget, a1
	bsr.w opforgeNativeCliCopyRequiredPathValue
	cmpi.l #1, d0
	beq.w missingValue
	tst.l d0
	bne.w modulePathCapacity
	bsr.w opforgeNativeCliRecordModulePathValue
	tst.l d0
	bne.w modulePathCapacity
	bra.w parseLoop

positionalInput
	tst.w NativeCliInputStyle
	beq.s positionalInputFirst
	cmpi.w #2, NativeCliInputStyle
	beq.w mixedInput
	bra.w multiplePositional

positionalInputFirst
	move.w #1, NativeCliInputStyle
	lea NativeCliArgToken, a0
	lea NativeCliInputPath, a1
	jsr opforgeNativeCliCopyTokenBuffer
	bra.w parseLoop

parseDone
	tst.w NativeCliInputStyle
	beq.w noInput
	tst.w NativeCliOutputFormat
	beq.w hunkRequired
	cmpi.w #NATIVE_OUTPUT_FORMAT_BIN, NativeCliOutputFormat
	beq.s defaultBinPath
	cmpi.w #NATIVE_OUTPUT_FORMAT_HUNK, NativeCliOutputFormat
	beq.s defaultHunkPath
	bra.w usage

defaultBinPath
	tst.b NativeCliBinPath
	bne.s parseOk
	tst.b NativeCliOutfileBase
	beq.s parseOk
	lea NativeCliOutfileBase, a0
	lea NativeCliBinPath, a1
	jsr opforgeNativeCliCopyTokenBuffer
	bra.s parseOk

defaultHunkPath
	tst.b NativeCliHunkPath
	bne.s parseOk
	tst.b NativeCliOutfileBase
	beq.s parseOk
	lea NativeCliOutfileBase, a0
	lea NativeCliHunkPath, a1
	jsr opforgeNativeCliCopyTokenBuffer

parseOk
	bsr.w opforgeNativeCliRecordImplicitModulePathRoot
	tst.l d0
	bne.w modulePathCapacity
	move.w #NCLI_PARSE_OK, NativeCliParseStatus
	bra.w parseReturn

parseHelp
	move.w #NCLI_PARSE_HELP, NativeCliParseStatus
	bra.w parseReturn

parseVersion
	move.w #NCLI_PARSE_VERSION, NativeCliParseStatus
	bra.w parseReturn

usage
	move.w #NCLI_PARSE_USAGE, NativeCliParseStatus
	bra.w parseReturn

quoted
	move.w #NCLI_PARSE_QUOTED, NativeCliParseStatus
	bra.w parseReturn

unsupported
	move.w #NCLI_PARSE_UNSUPPORTED, NativeCliParseStatus
	bra.w parseReturn

unknownFlag
	move.w #NCLI_PARSE_UNKNOWN_FLAG, NativeCliParseStatus
	bra.w parseReturn

missingValue
	move.w #NCLI_PARSE_MISSING_VALUE, NativeCliParseStatus
	bra.w parseReturn

noInput
	move.w #NCLI_PARSE_NO_INPUT, NativeCliParseStatus
	bra.w parseReturn

hunkRequired
	move.w #NCLI_PARSE_HUNK_REQUIRED, NativeCliParseStatus
	bra.w parseReturn

mixedInput
	move.w #NCLI_PARSE_MIXED_INPUT, NativeCliParseStatus
	bra.w parseReturn

multiplePositional
	move.w #NCLI_PARSE_MULTIPLE_POSITIONAL, NativeCliParseStatus
	bra.w parseReturn

modulePathCapacity
	move.w #NCLI_PARSE_MODULE_PATH_CAPACITY, NativeCliParseStatus

parseReturn
	move.w NativeCliParseStatus, d0
	ext.l d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; opforgeNativeCliParseArgs

opforgeNativeCliEmitModulePathRecords	.block
	movem.l d0-d4/a0, -(sp)
	clr.w d4

emitLoop
	move.w NativeCliModulePathCount, d0
	cmp.w d0, d4
	bhs.s emitDone
	move.l #ModPathText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w d4, d0
	jsr opforgeNativeCliPutDecU16
	jsr opforgeNativeCliPutSpace
	moveq #0, d0
	move.w d4, d0
	lsl.l #8, d0
	lea NativeCliModulePathTable, a0
	adda.l d0, a0
	move.l a0, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	addq.w #1, d4
	bra.s emitLoop

emitDone
	movem.l (sp)+, d0-d4/a0
	rts
	.bend  ; opforgeNativeCliEmitModulePathRecords

	.priv

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

opforgeNativeCliCopyToken	.block
	move.l #TOKEN_BUFFER_CAPACITY - 1, d6

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

opforgeNativeCliCopyRequiredPathValue	.block
	bsr.w opforgeNativeCliSkipWhitespace
	tst.b (a3)
	beq.s pathMissing
	cmpi.b #'"', (a3)
	beq.s pathMissing
	move.l #PATH_BUFFER_CAPACITY - 1, d6

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

opforgeNativeCliIsUnsupportedFlag	.block
	lea NativeCliArgToken, a0
	lea FlagListShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w unsupportedYes
	lea NativeCliArgToken, a0
	lea FlagListLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w unsupportedYes
	lea NativeCliArgToken, a0
	lea FlagHexShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w unsupportedYes
	lea NativeCliArgToken, a0
	lea FlagHexLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w unsupportedYes
	lea NativeCliArgToken, a0
	lea FlagSrecShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w unsupportedYes
	lea NativeCliArgToken, a0
	lea FlagSrecLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w unsupportedYes
	lea NativeCliArgToken, a0
	lea FlagDefineShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w unsupportedYes
	lea NativeCliArgToken, a0
	lea FlagDefineLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w unsupportedYes
	lea NativeCliArgToken, a0
	lea FlagIncludeShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w unsupportedYes
	lea NativeCliArgToken, a0
	lea FlagIncludeLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w unsupportedYes
	moveq #0, d0
	rts

unsupportedYes
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliIsUnsupportedFlag

opforgeNativeCliRecordImplicitModulePathRoot	.block
	lea NativeCliInputPath, a0
	lea NativeCliModulePathTable, a1
	jsr opforgeNativeCliCopyPathRoot
	rts
	.bend  ; opforgeNativeCliRecordImplicitModulePathRoot

opforgeNativeCliRecordModulePathValue	.block
	movem.l d1/a0-a1, -(sp)
	moveq #0, d0
	move.w NativeCliModulePathCount, d0
	cmpi.w #NATIVE_MODULE_PATH_CAPACITY, d0
	bhs.s recordFail
	move.l d0, d1
	lsl.l #8, d1
	lea NativeCliModulePathTable, a1
	adda.l d1, a1
	lea NativeCliIncludeTarget, a0
	jsr opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.s recordFail
	move.w NativeCliModulePathCount, d0
	addq.w #1, d0
	move.w d0, NativeCliModulePathCount
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

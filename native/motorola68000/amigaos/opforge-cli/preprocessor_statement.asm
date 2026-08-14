; Bounded native statement-signature selection and capture binding.
; @opforge-owner: opforge.cli.preprocessor_statement
; @opforge-slice: documentation/plans/slices/native-porting-slice-statement-expansion.toml

	.module opforge.cli.preprocessor_statement
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.copy
	.use opforge.cli.state
	.use opforge.cli.line_text
	.use opforge.cli.preprocessor
	.use opforge.cli.module_use

	.section code, kind=code
	.pub

; Select and bind one stored statement invocation before ordinary tokenization.
; Inputs: state.NativeCliSourceLine contains the logical source line.
; Outputs: D0 = 0 passthrough, 1 matched frame, -1 malformed/unsupported match.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliParseStatementInvocationV1	.block
	bsr.w normalizeScalarAssignment
	tst.l d0
	bne.w malformed
	clr.w state.NativeCliPreprocessInvocationLabelLen
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	movea.l a0, a2
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.w pass
	cmpa.l a2, a0
	bne.s mnemonicReady
	cmpi.b #'.', (a0)
	beq.w pass
	cmpi.b #';', (a0)
	beq.w pass
	bsr.w captureLeadingLabel
	bne.w pass
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.w pass
mnemonicReady
	movea.l a0, a3
	move.l d0, d6
	bsr.w takeStatementMnemonic
	bne.w pass
	move.l d0, StatementMnemonicLen.l
	move.l a3, StatementMnemonicPtr.l
	move.l d6, StatementSourceRemaining.l
	clr.w StatementBestKeywordLen.l
	move.w #-1, StatementBestDefinition.l
	moveq #0, d7
keywordLoop
	cmp.w state.NativeCliPreprocessDefinitionCount, d7
	bcc.w keywordDone
	bsr.w loadStatementHeader
	bne.w keywordNext
	move.l StatementKeywordLen.l, d0
	cmp.l StatementMnemonicLen.l, d0
	bhi.w keywordNext
	movea.l StatementKeywordPtr.l, a0
	move.l StatementKeywordLen.l, d0
	movea.l StatementMnemonicPtr.l, a1
	move.l StatementKeywordLen.l, d1
	jsr module_use.opforgeNativeCliDefinitionInvocationNameMatchesV1
	tst.l d0
	beq.w keywordNext
	move.l StatementKeywordLen.l, d0
	cmp.w StatementBestKeywordLen.l, d0
	bls.w keywordNext
	move.w d0, StatementBestKeywordLen.l
	move.w d7, StatementBestDefinition.l
keywordNext
	addq.w #1, d7
	bra.w keywordLoop
keywordDone
	tst.w StatementBestDefinition.l
	bmi.w pass
	move.w #-1, StatementSelectedDefinition.l
	clr.w StatementBestLiteralScore.l
	clr.w StatementBestAtomScore.l
	clr.w StatementSelectionTied.l
	moveq #0, d7
matchLoop
	cmp.w state.NativeCliPreprocessDefinitionCount, d7
	bcc.w matchDone
	bsr.w loadStatementHeader
	bne.w matchNext
	move.l StatementKeywordLen.l, d0
	cmp.w StatementBestKeywordLen.l, d0
	bne.w matchNext
	movea.l StatementKeywordPtr.l, a0
	move.l StatementKeywordLen.l, d0
	movea.l StatementMnemonicPtr.l, a1
	move.l StatementKeywordLen.l, d1
	jsr module_use.opforgeNativeCliDefinitionInvocationNameMatchesV1
	tst.l d0
	beq.w matchNext
	bsr.w matchLoadedStatement
	bne.w matchNext
	move.w StatementMatchLiteralScore.l, d0
	cmp.w StatementBestLiteralScore.l, d0
	bhi.w select
	bcs.w matchNext
	move.w StatementMatchAtomScore.l, d0
	cmp.w StatementBestAtomScore.l, d0
	bhi.w select
	bcs.w matchNext
	tst.w StatementSelectedDefinition.l
	bmi.w select
	move.w #1, StatementSelectionTied.l
	bra.w matchNext
select
	move.w d7, StatementSelectedDefinition.l
	move.w StatementMatchLiteralScore.l, d0
	move.w d0, StatementBestLiteralScore.l
	move.w StatementMatchAtomScore.l, d0
	move.w d0, StatementBestAtomScore.l
	clr.w StatementSelectionTied.l
matchNext
	addq.w #1, d7
	bra.w matchLoop
matchDone
	tst.w StatementSelectedDefinition.l
	bmi.w malformed
	tst.w StatementSelectionTied.l
	bne.w malformed
	move.w StatementSelectedDefinition.l, d0
	jsr preprocessor.opforgeNativeCliBeginMacroInvocationFrameV1
	bne.w malformed
	move.w StatementSelectedDefinition.l, d7
	bsr.w loadStatementHeader
	bne.w clearAndFail
	bsr.w matchLoadedStatement
	bne.w clearAndFail
	moveq #1, d0
	rts
clearAndFail
	move.w #-1, state.NativeCliPreprocessInvocationDefinition
malformed
	moveq #-1, d0
	rts
pass
	clr.w state.NativeCliPreprocessInvocationLabelLen
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliParseStatementInvocationV1

; Resolve a captured statement name to its positional slot.
; Inputs: A0 = requested name bytes; D0 = requested name length.
; Outputs: D0 = zero-based slot, -1 when not found.
; Clobbers: D0-D6/A1-A2/CCR. CCR: reflects D0.
opforgeNativeCliFindStatementCaptureV1	.block
	movea.l a0, a2
	move.l d0, d6
	moveq #0, d5
loop
	cmp.w state.NativeCliPreprocessInvocationArgCount, d5
	bcc.s no
	move.l d5, d0
	add.l d0, d0
	lea StatementCaptureNameLen.l, a0
	moveq #0, d1
	move.w 0(a0, d0.l), d1
	cmp.l d6, d1
	bne.s next
	move.l d5, d0
	lsl.l #2, d0
	lea StatementCaptureNamePtr.l, a0
	movea.l 0(a0, d0.l), a1
	clr.l d1
compare
	cmp.l d6, d1
	beq.s found
	move.b 0(a1, d1.l), d3
	move.b 0(a2, d1.l), d4
	cmpi.b #'A', d3
	bcs.s requestFold
	cmpi.b #'Z', d3
	bhi.s requestFold
	addi.b #32, d3
requestFold
	cmpi.b #'A', d4
	bcs.s bytesReady
	cmpi.b #'Z', d4
	bhi.s bytesReady
	addi.b #32, d4
bytesReady
	cmp.b d4, d3
	bne.s next
	addq.l #1, d1
	bra.s compare
next
	addq.w #1, d5
	bra.s loop
found
	move.w d5, d0
	rts
no
	moveq #-1, d0
	rts
	.bend  ; opforgeNativeCliFindStatementCaptureV1

	.priv

; Rewrite `name = expr` to the already-supported `name .const expr` surface.
; Sequence/list assignments retain their existing `{...}` owner and are skipped.
; Inputs: state.NativeCliSourceLine/Len. Outputs: D0 = 0 or 1 on overflow.
; Clobbers: D0-D4/A0-A2/CCR. CCR: reflects D0.
normalizeScalarAssignment	.block
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	tst.l d0
	beq.w pass
	move.b (a0), d1
	cmpi.b #'A', d1
	bcs.s firstLower
	cmpi.b #'Z', d1
	bls.s scanName
firstLower
	cmpi.b #'a', d1
	bcs.w pass
	cmpi.b #'z', d1
	bhi.w pass
scanName
	movea.l a0, a1
	move.l d0, d2
nameLoop
	tst.l d2
	beq.w pass
	move.b (a1), d1
	cmpi.b #' ', d1
	beq.s afterName
	cmpi.b #9, d1
	beq.s afterName
	addq.l #1, a1
	subq.l #1, d2
	bra.s nameLoop
afterName
	tst.l d2
	beq.w pass
skipBeforeEquals
	move.b (a1), d1
	cmpi.b #' ', d1
	beq.s oneBeforeEquals
	cmpi.b #9, d1
	bne.s checkEquals
oneBeforeEquals
	addq.l #1, a1
	subq.l #1, d2
	beq.w pass
	bra.s skipBeforeEquals
checkEquals
	cmpi.b #'=', (a1)
	bne.w pass
	movea.l a1, a2
	addq.l #1, a1
	subq.l #1, d2
skipAfterEquals
	tst.l d2
	beq.s scalar
	move.b (a1), d1
	cmpi.b #' ', d1
	beq.s oneAfterEquals
	cmpi.b #9, d1
	bne.s checkSequence
oneAfterEquals
	addq.l #1, a1
	subq.l #1, d2
	bra.s skipAfterEquals
checkSequence
	cmpi.b #'{', (a1)
	beq.s pass
scalar
	move.l d0, d3
	addq.l #5, d3
	cmpi.l #constants.SOURCE_LINE_BUFFER_CAPACITY, d3
	bcc.s fail
	lea state.NativeCliSourceLine, a0
	adda.l d0, a0
	movea.l a0, a1
	adda.l #5, a1
	move.l a0, d4
	sub.l a2, d4
	subq.l #1, d4
shift
	tst.l d4
	beq.s writeDirective
	move.b -(a0), -(a1)
	subq.l #1, d4
	bra.s shift
writeDirective
	move.b #'.', (a2)+
	move.b #'c', (a2)+
	move.b #'o', (a2)+
	move.b #'n', (a2)+
	move.b #'s', (a2)+
	move.b #'t', (a2)+
	move.w d3, state.NativeCliSourceLineLen
	lea state.NativeCliSourceLine, a0
	clr.b 0(a0, d3.l)
pass
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; normalizeScalarAssignment

; Capture the column-one label token and retain it for first-line attachment.
; Inputs: A0/D0 = source slice. Outputs: A0/D0 advanced, D1 = status.
; Clobbers: D1-D3/A1/CCR. CCR: reflects D1.
captureLeadingLabel	.block
	lea state.NativeCliPreprocessInvocationLabel, a1
	clr.w d3
loop
	tst.l d0
	beq.s done
	move.b (a0), d1
	cmpi.b #':', d1
	beq.s colon
	cmpi.b #' ', d1
	beq.s done
	cmpi.b #9, d1
	beq.s done
	cmpi.b #'A', d1
	bcs.s lower
	cmpi.b #'Z', d1
	bls.s copy
lower
	cmpi.b #'a', d1
	bcs.s special
	cmpi.b #'z', d1
	bls.s copy
special
	cmpi.b #'_', d1
	beq.s copy
	cmpi.b #'0', d1
	bcs.s fail
	cmpi.b #'9', d1
	bhi.s fail
copy
	cmpi.w #constants.NATIVE_PREPROCESS_INVOCATION_LABEL_CAPACITY - 1, d3
	bcc.s fail
	move.b d1, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	addq.w #1, d3
	bra.s loop
colon
	addq.l #1, a0
	subq.l #1, d0
done
	tst.w d3
	beq.s fail
	clr.b (a1)
	move.w d3, state.NativeCliPreprocessInvocationLabelLen
	moveq #0, d1
	rts
fail
	moveq #1, d1
	rts
	.bend  ; captureLeadingLabel

; Measure one Rust-compatible mnemonic identifier, including dot suffixes.
; Inputs: A0/D0 = mnemonic slice. Outputs: D0 = length, D1 = status.
; Clobbers: D0-D2/CCR. CCR: reflects D1.
takeStatementMnemonic	.block
	clr.l d2
loop
	cmp.l d6, d2
	bcc.s done
	move.b 0(a0, d2.l), d1
	cmpi.b #'A', d1
	bcs.s lower
	cmpi.b #'Z', d1
	bls.s next
lower
	cmpi.b #'a', d1
	bcs.s special
	cmpi.b #'z', d1
	bls.s next
special
	cmpi.b #'_', d1
	beq.s next
	cmpi.b #'.', d1
	beq.s next
	cmpi.b #'$', d1
	beq.s next
	cmpi.b #'0', d1
	bcs.s done
	cmpi.b #'9', d1
	bhi.s done
next
	addq.l #1, d2
	bra.s loop
done
	tst.l d2
	beq.s fail
	move.l d2, d0
	moveq #0, d1
	rts
fail
	moveq #1, d1
	rts
	.bend  ; takeStatementMnemonic

; Load one statement definition's keyword/signature slices into scratch state.
; Inputs: D7.W = definition index. Outputs: D0 = status.
; Clobbers: D0-D3/A0-A2/CCR. CCR: reflects D0.
loadStatementHeader	.block
	lea state.NativeCliPreprocessDefinitionKind, a0
	cmpi.b #constants.NATIVE_PREPROCESS_DEFINITION_KIND_STATEMENT, 0(a0, d7.w)
	bne.w fail
	move.l d7, d2
	mulu #constants.NATIVE_PREPROCESS_DEFINITION_HEADER_CAPACITY, d2
	lea state.NativeCliPreprocessDefinitionHeader, a0
	adda.l d2, a0
	move.l d7, d2
	add.l d2, d2
	lea state.NativeCliPreprocessDefinitionHeaderLen, a1
	moveq #0, d0
	move.w 0(a1, d2.l), d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	cmpi.l #10, d0
	bcs.s fail
	adda.l #10, a0
	subi.l #10, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.s fail
	move.l a0, StatementKeywordPtr.l
	clr.l d1
keyword
	cmp.l d0, d1
	bcc.s keywordEnd
	move.b 0(a0, d1.l), d2
	cmpi.b #' ', d2
	beq.s keywordEnd
	cmpi.b #9, d2
	beq.s keywordEnd
	cmpi.b #';', d2
	beq.s keywordEnd
	addq.l #1, d1
	bra.s keyword
keywordEnd
	tst.l d1
	beq.s fail
	move.l d1, StatementKeywordLen.l
	adda.l d1, a0
	sub.l d1, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	move.l a0, StatementSignaturePtr.l
	move.l d0, StatementSignatureLen.l
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; loadStatementHeader

; Compare D0 bytes at A0/A1 case-insensitively.
; Outputs: D1 = status. Clobbers: D1-D3/CCR. CCR: reflects D1.
compareFoldedBytes	.block
	clr.l d2
loop
	cmp.l d0, d2
	bcc.s yes
	move.b 0(a0, d2.l), d1
	move.b 0(a1, d2.l), d3
	cmpi.b #'A', d1
	bcs.s foldInput
	cmpi.b #'Z', d1
	bhi.s foldInput
	addi.b #32, d1
foldInput
	cmpi.b #'A', d3
	bcs.s compare
	cmpi.b #'Z', d3
	bhi.s compare
	addi.b #32, d3
compare
	cmp.b d3, d1
	bne.s no
	addq.l #1, d2
	bra.s loop
yes
	moveq #0, d1
	rts
no
	moveq #1, d1
	rts
	.bend  ; compareFoldedBytes

; Match the loaded canonical signature and bind its captures positionally.
; Supports literal atoms, typed captures, and adjacent `[{capture}]` boundaries.
; Inputs: loaded header scratch plus source scratch. Outputs: D0 = status.
; Clobbers: D0-D7/A0-A3/CCR. CCR: reflects D0.
matchLoadedStatement	.block
	movea.l StatementSignaturePtr.l, a0
	move.l StatementSignatureLen.l, d6
	movea.l StatementMnemonicPtr.l, a1
	moveq #0, d0
	move.w StatementBestKeywordLen.l, d0
	adda.l d0, a1
	move.l StatementSourceRemaining.l, d5
	sub.l d0, d5
	clr.w state.NativeCliPreprocessInvocationArgCount
	clr.w state.NativeCliPreprocessInvocationFullArgsLen
	clr.w StatementMatchLiteralScore.l
	clr.w StatementMatchAtomScore.l
	bsr.w skipInputWhitespace
signatureLoop
	bsr.w skipSignatureWhitespace
	tst.l d0
	beq.s signatureReady
	bsr.w skipInputWhitespace
signatureReady
	tst.l d6
	beq.w signatureDone
	move.b (a0), d0
	cmpi.b #';', d0
	beq.w signatureDone
	cmpi.b #'"', d0
	beq.s literal
	cmpi.b #'\'', d0
	beq.s literal
	cmpi.b #'[', d0
	beq.s boundary
	moveq #0, d4
	bsr.w captureAtom
	bne.w fail
	addq.w #1, StatementMatchAtomScore.l
	bra.w signatureLoop
literal
	bsr.w matchQuotedLiteral
	bne.w fail
	addq.w #1, StatementMatchLiteralScore.l
	addq.w #1, StatementMatchAtomScore.l
	bra.w signatureLoop
boundary
	cmpi.l #2, d6
	bcs.w fail
	cmpi.b #'{', 1(a0)
	bne.w fail
	addq.l #2, a0
	subq.l #2, d6
	moveq #1, d4
	bsr.w captureAtom
	bne.w fail
	cmpi.l #2, d6
	bcs.w fail
	cmpi.b #'}', (a0)
	bne.w fail
	cmpi.b #']', 1(a0)
	bne.w fail
	addq.l #2, a0
	subq.l #2, d6
	addq.w #1, StatementMatchAtomScore.l
	bra.w signatureLoop
signatureDone
	bsr.w skipInputWhitespace
	tst.l d5
	beq.s success
	cmpi.b #';', (a1)
	bne.s fail
success
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; matchLoadedStatement

skipSignatureWhitespace	.block
	moveq #0, d0
scan
	tst.l d6
	beq.s done
	cmpi.b #' ', (a0)
	beq.s one
	cmpi.b #9, (a0)
	bne.s done
one
	moveq #1, d0
	addq.l #1, a0
	subq.l #1, d6
	bra.s scan
done
	rts
	.bend  ; skipSignatureWhitespace

skipInputWhitespace	.block
	tst.l d5
	beq.s done
	cmpi.b #' ', (a1)
	beq.s one
	cmpi.b #9, (a1)
	bne.s done
one
	addq.l #1, a1
	subq.l #1, d5
	bra.s skipInputWhitespace
done
	rts
	.bend  ; skipInputWhitespace

; Match one quoted literal atom exactly, excluding its signature quotes.
matchQuotedLiteral	.block
	move.b (a0), d2
	addq.l #1, a0
	subq.l #1, d6
loop
	tst.l d6
	beq.s fail
	move.b (a0), d0
	cmp.b d2, d0
	beq.s close
	tst.l d5
	beq.s fail
	cmp.b (a1), d0
	bne.s fail
	addq.l #1, a0
	subq.l #1, d6
	addq.l #1, a1
	subq.l #1, d5
	bra.s loop
close
	addq.l #1, a0
	subq.l #1, d6
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; matchQuotedLiteral

; Parse type:name and capture one bounded invocation token.
; D4 = 1 requires adjacency; current implementation never skips input here.
captureAtom	.block
	move.l a1, StatementInputPtr.l
	movea.l a0, a2
	clr.l d2
typeLoop
	cmp.l d6, d2
	bcc.w fail
	move.b 0(a0, d2.l), d0
	cmpi.b #':', d0
	beq.s typeDone
	cmpi.b #'A', d0
	bcs.s typeLower
	cmpi.b #'Z', d0
	bls.s typeNext
typeLower
	cmpi.b #'a', d0
	bcs.w fail
	cmpi.b #'z', d0
	bhi.w fail
typeNext
	addq.l #1, d2
	bra.s typeLoop
typeDone
	tst.l d2
	beq.w fail
	move.l d2, StatementCaptureTypeLen.l
	addq.l #1, d2
	adda.l d2, a0
	sub.l d2, d6
	movea.l a0, a3
	clr.l d3
nameLoop
	cmp.l d6, d3
	bcc.w nameDone
	move.b 0(a0, d3.l), d0
	cmpi.b #'A', d0
	bcs.w nameLower
	cmpi.b #'Z', d0
	bls.w nameNext
nameLower
	cmpi.b #'a', d0
	bcs.w nameSpecial
	cmpi.b #'z', d0
	bls.w nameNext
nameSpecial
	cmpi.b #'_', d0
	beq.w nameNext
	cmpi.b #'0', d0
	bcs.w nameDone
	cmpi.b #'9', d0
	bhi.w nameDone
nameNext
	addq.l #1, d3
	bra.w nameLoop
nameDone
	tst.l d3
	beq.w fail
	adda.l d3, a0
	sub.l d3, d6
	bsr.w validateCaptureType
	bne.w fail
	moveq #0, d0
	move.w state.NativeCliPreprocessInvocationArgCount, d0
	cmpi.w #constants.NATIVE_PREPROCESS_MACRO_ARG_CAPACITY, d0
	bcc.w fail
	move.l d0, d1
	lsl.l #2, d1
	lea StatementCaptureNamePtr.l, a2
	move.l a3, 0(a2, d1.l)
	add.l d0, d0
	lea StatementCaptureNameLen.l, a2
	move.w d3, 0(a2, d0.l)
	tst.l d5
	beq.w fail
	movea.l StatementInputPtr.l, a1
	move.b (a1), d0
	cmpi.b #'\'', d0
	beq.w quotedToken
	cmpi.b #'"', d0
	beq.w quotedToken
	cmpi.w #4, StatementCaptureKind.l
	bne.s scanToken
	moveq #1, d2
	bra.w tokenReady
scanToken
	clr.l d2
tokenLoop
	cmp.l d5, d2
	bcc.w tokenDone
	move.b 0(a1, d2.l), d0
	cmpi.b #' ', d0
	beq.w tokenDone
	cmpi.b #9, d0
	beq.w tokenDone
	cmpi.b #',', d0
	beq.w tokenDone
	cmpi.b #']', d0
	beq.w tokenDone
	cmpi.b #';', d0
	beq.w tokenDone
	addq.l #1, d2
	bra.w tokenLoop
tokenDone
	tst.l d2
	beq.w fail
tokenReady
	move.l d2, -(sp)
	bsr.w validateCapturedValue
	move.l (sp)+, d2
	tst.l d0
	bne.w fail
	bra.w store
quotedToken
	move.b d0, d3
	moveq #1, d2
quotedLoop
	cmp.l d5, d2
	bcc.w fail
	move.b 0(a1, d2.l), d0
	cmpi.b #'\\', d0
	bne.s quotedClose
	addq.l #2, d2
	cmp.l d5, d2
	bhi.w fail
	bra.s quotedLoop
quotedClose
	addq.l #1, d2
	cmp.b d3, d0
	bne.s quotedLoop
	cmp.l d5, d2
	bcc.s tokenReady
	move.b 0(a1, d2.l), d0
	cmpi.b #' ', d0
	beq.s tokenReady
	cmpi.b #9, d0
	beq.s tokenReady
	cmpi.b #',', d0
	beq.s tokenReady
	cmpi.b #']', d0
	beq.s tokenReady
	cmpi.b #';', d0
	beq.s tokenReady
	bra.w fail
store
	move.w state.NativeCliPreprocessInvocationArgCount, d0
	cmpi.w #constants.NATIVE_PREPROCESS_MACRO_ARG_CAPACITY, d0
	bcc.w fail
	cmpi.l #constants.NATIVE_PREPROCESS_INVOCATION_ARG_TEXT_CAPACITY - 1, d2
	bcc.w fail
	move.l d0, d3
	mulu #constants.NATIVE_PREPROCESS_INVOCATION_ARG_TEXT_CAPACITY, d3
	lea state.NativeCliPreprocessInvocationArgs, a2
	adda.l d3, a2
	movea.l a1, a3
	move.l d2, d0
	jsr copy.copyBytes
	clr.b (a2)
	move.w state.NativeCliPreprocessInvocationArgCount, d0
	add.w d0, d0
	lea state.NativeCliPreprocessInvocationArgLen, a2
	move.w d2, 0(a2, d0.w)
	addq.w #1, state.NativeCliPreprocessInvocationArgCount
	movea.l a3, a1
	adda.l d2, a1
	sub.l d2, d5
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; captureAtom

; Accept only Rust statement capture types and retain their value-check kind.
; Inputs: A2 = type bytes; StatementCaptureTypeLen. Outputs: D0 = status.
; Clobbers: D0-D1/CCR. CCR: reflects D0.
validateCaptureType	.block
	clr.w StatementCaptureKind.l
	move.l StatementCaptureTypeLen.l, d0
	cmpi.l #3, d0
	beq.w strType
	cmpi.l #4, d0
	bne.w fail
	move.b (a2), d0
	ori.b #32, d0
	cmpi.b #'b', d0
	beq.w byteType
	cmpi.b #'w', d0
	beq.w wordType
	cmpi.b #'l', d0
	beq.w longType
	cmpi.b #'c', d0
	beq.w charType
	bra.w fail
byteType
	move.b 1(a2), d0
	ori.b #32, d0
	cmpi.b #'y', d0
	bne.w fail
	move.b 2(a2), d0
	ori.b #32, d0
	cmpi.b #'t', d0
	bne.w fail
	move.b 3(a2), d0
	ori.b #32, d0
	cmpi.b #'e', d0
	bne.w fail
	move.w #1, StatementCaptureKind.l
	bra.w matched
wordType
	move.b 1(a2), d0
	ori.b #32, d0
	cmpi.b #'o', d0
	bne.w fail
	move.b 2(a2), d0
	ori.b #32, d0
	cmpi.b #'r', d0
	bne.w fail
	move.b 3(a2), d0
	ori.b #32, d0
	cmpi.b #'d', d0
	bne.w fail
	move.w #2, StatementCaptureKind.l
	bra.w matched
longType
	move.b 1(a2), d0
	ori.b #32, d0
	cmpi.b #'o', d0
	bne.w fail
	move.b 2(a2), d0
	ori.b #32, d0
	cmpi.b #'n', d0
	bne.w fail
	move.b 3(a2), d0
	ori.b #32, d0
	cmpi.b #'g', d0
	bne.w fail
	move.w #3, StatementCaptureKind.l
	bra.w matched
charType
	move.b 1(a2), d0
	ori.b #32, d0
	cmpi.b #'h', d0
	bne.s fail
	move.b 2(a2), d0
	ori.b #32, d0
	cmpi.b #'a', d0
	bne.s fail
	move.b 3(a2), d0
	ori.b #32, d0
	cmpi.b #'r', d0
	bne.s fail
	move.w #4, StatementCaptureKind.l
matched
	moveq #0, d0
	rts
strType
	move.b (a2), d0
	ori.b #32, d0
	cmpi.b #'s', d0
	bne.w fail
	move.b 1(a2), d0
	ori.b #32, d0
	cmpi.b #'t', d0
	bne.w fail
	move.b 2(a2), d0
	ori.b #32, d0
	cmpi.b #'r', d0
	bne.w fail
	move.w #5, StatementCaptureKind.l
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; validateCaptureType

; Enforce Rust StatementSignature token-kind and numeric-width semantics.
; Inputs: A1/D2 = captured token text. Outputs: D0 = status.
; Clobbers: D0-D4/A2/CCR. CCR: reflects D0.
validateCapturedValue	.block
	move.w StatementCaptureKind.l, d0
	cmpi.w #4, d0
	beq.w charValue
	cmpi.w #5, d0
	beq.w strValue
	move.b (a1), d1
	cmpi.b #'\'', d1
	beq.w quotedScalar
	cmpi.b #'"', d1
	beq.w quotedScalar
	cmpi.b #'-', d1
	beq.w numericValue
	cmpi.b #'$', d1
	beq.w numericValue
	cmpi.b #'%', d1
	beq.w numericValue
	cmpi.b #'0', d1
	bcs.w identifierValue
	cmpi.b #'9', d1
	bls.w numericValue
identifierValue
	; Rust accepts identifier/register tokens for all numeric capture widths.
	bra.w validateIdentifierToken
quotedScalar
	cmpi.w #3, StatementCaptureKind.l
	beq.w fail
	bsr.w countQuotedValueBytes
	bne.w fail
	cmpi.w #1, StatementCaptureKind.l
	beq.w byteString
	cmpi.l #2, d3
	bhi.w fail
	bra.w success
byteString
	cmpi.l #1, d3
	bhi.w fail
	bra.w success
charValue
	move.b (a1), d1
	cmpi.b #'\'', d1
	beq.w quotedChar
	cmpi.b #'"', d1
	beq.w quotedChar
	cmpi.l #1, d2
	bne.w fail
	cmpi.b #'0', d1
	bcs.w success
	cmpi.b #'9', d1
	bls.w fail
	bra.w validateIdentifierToken
quotedChar
	bsr.w countQuotedValueBytes
	bne.w fail
	cmpi.l #1, d3
	bne.w fail
	bra.w success
strValue
	move.b (a1), d1
	cmpi.b #'\'', d1
	beq.s validateString
	cmpi.b #'"', d1
	bne.w fail
validateString
	bsr.w countQuotedValueBytes
	bne.w fail
	bra.w success
numericValue
	bsr.w parseCapturedNumber
	bne.w fail
	move.w StatementCaptureKind.l, d0
	cmpi.w #3, d0
	beq.w success
	tst.w d4
	bne.w negativeWidth
	cmpi.w #1, d0
	beq.w positiveByte
	cmpi.l #$0000ffff, d3
	bhi.w fail
	bra.w success
positiveByte
	cmpi.l #$000000ff, d3
	bhi.w fail
	bra.w success
negativeWidth
	cmpi.w #1, d0
	beq.w negativeByte
	cmpi.l #$00008000, d3
	bhi.w fail
	bra.w success
negativeByte
	cmpi.l #$00000080, d3
	bhi.w fail
success
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; validateCapturedValue

; Count decoded bytes inside an already bounded quoted token.
; Inputs: A1/D2 = quoted token. Outputs: D3 = decoded byte count, D0 status.
countQuotedValueBytes	.block
	cmpi.l #2, d2
	bcs.s fail
	move.b (a1), d1
	cmp.b -1(a1, d2.l), d1
	bne.s fail
	moveq #1, d0
	clr.l d3
loop
	move.l d2, d4
	subq.l #1, d4
	cmp.l d4, d0
	bcc.s success
	move.b 0(a1, d0.l), d1
	cmpi.b #'\\', d1
	bne.s one
	addq.l #1, d0
	cmp.l d4, d0
	bcc.s fail
	move.b 0(a1, d0.l), d1
	cmpi.b #'x', d1
	bne.s one
	move.l d0, -(sp)
	addq.l #2, d0
	cmp.l d4, d0
	bcc.s hexFail
	move.l (sp)+, d0
	move.b 1(a1, d0.l), d1
	bsr.w isHexDigitByte
	tst.l d1
	bne.s fail
	move.b 2(a1, d0.l), d1
	bsr.w isHexDigitByte
	tst.l d1
	bne.s fail
	addq.l #3, d0
	addq.l #1, d3
	bra.s loop
hexFail
	addq.l #4, sp
	bra.s fail
one
	addq.l #1, d0
	addq.l #1, d3
	bra.s loop
success
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; countQuotedValueBytes

; Inputs: D1.B candidate. Outputs: D1 = 0 hex digit, 1 otherwise.
isHexDigitByte	.block
	cmpi.b #'0', d1
	bcs.s upper
	cmpi.b #'9', d1
	bls.s yes
upper
	cmpi.b #'A', d1
	bcs.s lower
	cmpi.b #'F', d1
	bls.s yes
lower
	cmpi.b #'a', d1
	bcs.s no
	cmpi.b #'f', d1
	bls.s yes
no
	moveq #1, d1
	rts
yes
	clr.l d1
	rts
	.bend  ; isHexDigitByte

; Parse Rust-compatible signed number spellings to magnitude D3 and sign D4.
; Inputs: A1/D2 = token. Outputs: D3 magnitude, D4 = negative, D0 status.
parseCapturedNumber	.block
	movea.l a1, a2
	move.l d2, d1
	clr.l d3
	clr.l d4
	cmpi.b #'-', (a2)
	bne.s prefix
	moveq #1, d4
	addq.l #1, a2
	subq.l #1, d1
	beq.w fail
prefix
	moveq #10, d0
	cmpi.l #2, d1
	bcs.s sigil
	cmpi.b #'0', (a2)
	bne.s sigil
	move.b 1(a2), d2
	ori.b #32, d2
	cmpi.b #'x', d2
	beq.s prefixHex
	cmpi.b #'o', d2
	beq.s prefixOctal
	cmpi.b #'b', d2
	beq.s prefixBinary
sigil
	cmpi.b #'$', (a2)
	beq.s oneHex
	cmpi.b #'%', (a2)
	beq.s oneBinary
	; Suffixes select their radix; B follows Rust's binary-else-hex rule.
	move.b -1(a2, d1.l), d2
	ori.b #32, d2
	cmpi.b #'h', d2
	beq.s suffixHex
	cmpi.b #'o', d2
	beq.s suffixOctal
	cmpi.b #'q', d2
	beq.s suffixOctal
	cmpi.b #'d', d2
	beq.s suffixDecimal
	cmpi.b #'b', d2
	bne.s digits
	bsr.w capturedDigitsAreBinary
	tst.l d2
	bne.s suffixBinary
	bra.s suffixHex
prefixHex
	moveq #16, d0
	bra.s twoPrefix
prefixOctal
	moveq #8, d0
	bra.s twoPrefix
prefixBinary
	moveq #2, d0
twoPrefix
	addq.l #2, a2
	subq.l #2, d1
	bra.s digits
oneHex
	moveq #16, d0
	bra.s onePrefix
oneBinary
	moveq #2, d0
onePrefix
	addq.l #1, a2
	subq.l #1, d1
	bra.s digits
suffixHex
	moveq #16, d0
	bra.s oneSuffix
suffixOctal
	moveq #8, d0
	bra.s oneSuffix
suffixDecimal
	moveq #10, d0
	bra.s oneSuffix
suffixBinary
	moveq #2, d0
oneSuffix
	subq.l #1, d1
digits
	tst.l d1
	beq.w fail
	clr.w StatementNumericDigitSeen.l
	clr.l d3
	clr.l d2
digitLoop
	tst.l d1
	beq.w range
	move.b (a2)+, d2
	subq.l #1, d1
	cmpi.b #'_', d2
	beq.s digitLoop
	cmpi.b #'0', d2
	bcs.s foldHex
	cmpi.b #'9', d2
	bls.s decimalDigit
foldHex
	ori.b #32, d2
	cmpi.b #'a', d2
	bcs.w fail
	cmpi.b #'f', d2
	bhi.w fail
	subi.b #'a' - 10, d2
	bra.s digitReady
decimalDigit
	subi.b #'0', d2
digitReady
	cmp.b d0, d2
	bcc.w fail
	move.w #1, StatementNumericDigitSeen.l
	; Detect unsigned 32-bit overflow before magnitude = magnitude * radix + digit.
	move.l d1, -(sp)
	cmpi.w #16, d0
	beq.s times16
	cmpi.w #10, d0
	beq.s times10
	cmpi.w #8, d0
	beq.s times8
	cmpi.l #$7fffffff, d3
	bhi.s multiplyFail
	lsl.l #1, d3
	bra.s addDigit
times8
	cmpi.l #$1fffffff, d3
	bhi.s multiplyFail
	lsl.l #3, d3
	bra.s addDigit
times16
	cmpi.l #$0fffffff, d3
	bhi.s multiplyFail
	lsl.l #4, d3
	bra.s addDigit
times10
	cmpi.l #429496729, d3
	bhi.s multiplyFail
	bcs.s times10Ready
	cmpi.b #5, d2
	bhi.s multiplyFail
times10Ready
	move.l d3, d1
	lsl.l #3, d3
	lsl.l #1, d1
	add.l d1, d3
addDigit
	moveq #0, d1
	move.b d2, d1
	add.l d1, d3
	bcs.s multiplyFail
	move.l (sp)+, d1
	bra.w digitLoop
multiplyFail
	addq.l #4, sp
	bra.w fail
range
	tst.w StatementNumericDigitSeen.l
	beq.s fail
	tst.w d4
	beq.s success
	cmpi.l #$80000000, d3
	bhi.s fail
success
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; parseCapturedNumber

; Return D2=1 when the suffix body contains only 0/1/underscore.
capturedDigitsAreBinary	.block
	movea.l a2, a0
	move.l d1, d2
	subq.l #1, d2
	beq.s no
loop
	move.b (a0)+, d3
	subq.l #1, d2
	cmpi.b #'_', d3
	beq.s next
	cmpi.b #'0', d3
	beq.s next
	cmpi.b #'1', d3
	bne.s no
next
	tst.l d2
	bne.s loop
	moveq #1, d2
	rts
no
	clr.l d2
	rts
	.bend  ; capturedDigitsAreBinary

; Accept one Rust identifier/register-shaped token and reject operators or
; multi-token expressions that the native raw scanner would otherwise absorb.
; Inputs: A1/D2 = token. Outputs: D0 = status.
validateIdentifierToken	.block
	tst.l d2
	beq.w fail
	move.b (a1), d0
	cmpi.b #'A', d0
	bcs.s firstLower
	cmpi.b #'Z', d0
	bls.s scanStart
firstLower
	cmpi.b #'a', d0
	bcs.s firstSpecial
	cmpi.b #'z', d0
	bls.s scanStart
firstSpecial
	cmpi.b #'_', d0
	beq.s scanStart
	bra.s fail
scanStart
	moveq #1, d1
scan
	cmp.l d2, d1
	bcc.s success
	move.b 0(a1, d1.l), d0
	cmpi.b #'A', d0
	bcs.s lower
	cmpi.b #'Z', d0
	bls.s next
lower
	cmpi.b #'a', d0
	bcs.s special
	cmpi.b #'z', d0
	bls.s next
special
	cmpi.b #'_', d0
	beq.s next
	cmpi.b #'.', d0
	beq.s next
	cmpi.b #'$', d0
	beq.s next
	cmpi.b #'\'', d0
	bne.s digit
	move.l d1, d0
	addq.l #1, d0
	cmp.l d2, d0
	beq.s next
	bra.s fail
digit
	cmpi.b #'0', d0
	bcs.s fail
	cmpi.b #'9', d0
	bhi.s fail
next
	addq.l #1, d1
	bra.s scan
success
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; validateIdentifierToken

	.endsection
	.section bss, kind=bss
StatementMnemonicPtr
	.res long, 1
StatementMnemonicLen
	.res long, 1
StatementSourceRemaining
	.res long, 1
StatementKeywordPtr
	.res long, 1
StatementKeywordLen
	.res long, 1
StatementSignaturePtr
	.res long, 1
StatementSignatureLen
	.res long, 1
StatementInputPtr
	.res long, 1
StatementCaptureTypeLen
	.res long, 1
StatementCaptureKind
	.res word, 1
StatementNumericDigitSeen
	.res word, 1
StatementBestKeywordLen
	.res word, 1
StatementBestDefinition
	.res word, 1
StatementSelectedDefinition
	.res word, 1
StatementBestLiteralScore
	.res word, 1
StatementBestAtomScore
	.res word, 1
StatementMatchLiteralScore
	.res word, 1
StatementMatchAtomScore
	.res word, 1
StatementSelectionTied
	.res word, 1
StatementCaptureNamePtr
	.res long, constants.NATIVE_PREPROCESS_MACRO_ARG_CAPACITY
StatementCaptureNameLen
	.res word, constants.NATIVE_PREPROCESS_MACRO_ARG_CAPACITY
	.endsection
	.endmodule

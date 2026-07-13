; Native session-local text-encoding state.

	.module opasm.amigaos.flow_text_encoding
	.cpu 68020

TEXT_ENCODING_CUSTOM_CAPACITY = 4
TEXT_ENCODING_NAME_CAPACITY = 32
TEXT_ENCODING_CHAR_MAP_BYTES = 128
TEXT_ENCODING_ESCAPE_CAPACITY = 8
TEXT_ENCODING_ESCAPE_PATTERN_CAPACITY = 16

	.section code, kind=code
	.pub

; Reset text-encoding state at an assembly-pass boundary.
; Outputs: D0 = 0.
; Clobbers: D0-D1/A0/CCR.
; CCR: reflects D0 on return.
resetStateV1	.block
	clr.w TextEncodingActiveKind.l
	clr.w TextEncodingActiveCustomIndex.l
	clr.w TextEncodingDefinitionOpen.l
	clr.w TextEncodingCustomCount.l
	lea TextEncodingCustomNames.l, a0
	moveq #(TEXT_ENCODING_CUSTOM_CAPACITY * TEXT_ENCODING_NAME_CAPACITY / 4) - 1, d1
clearNames
	clr.l (a0)+
	dbra d1, clearNames
	lea TextEncodingCustomNameLens.l, a0
	clr.l (a0)
	lea TextEncodingCustomBaseKind.l, a0
	clr.l (a0)
	lea TextEncodingCustomMapPresent.l, a0
	moveq #(TEXT_ENCODING_CUSTOM_CAPACITY * TEXT_ENCODING_CHAR_MAP_BYTES / 4) - 1, d1
clearMaps
	clr.l (a0)+
	dbra d1, clearMaps
	lea TextEncodingCustomCharMap.l, a0
	moveq #(TEXT_ENCODING_CUSTOM_CAPACITY * TEXT_ENCODING_CHAR_MAP_BYTES / 4) - 1, d1
clearMapBytes
	clr.l (a0)+
	dbra d1, clearMapBytes
	lea TextEncodingEscapeCounts.l, a0
	clr.l (a0)
	clr.l 4(a0)
	lea TextEncodingEscapePatternLens.l, a0
	moveq #(TEXT_ENCODING_CUSTOM_CAPACITY * TEXT_ENCODING_ESCAPE_CAPACITY / 4) - 1, d1
clearEscapeLens
	clr.l (a0)+
	dbra d1, clearEscapeLens
	moveq #0, d0
	rts
	.bend  ; resetStateV1

; Consume the built-in `.enc` / `.encoding` selectors before normal statement
; processing.  Source-defined tables are added by the definition path below.
; Inputs: A0/D0 = mnemonic; A1/D1 = operand text.
; Outputs: D0 = 0 on success, 1 for an invalid selector; D3 = 1 handled, 0 unhandled.
; Clobbers: D0-D6/A0-A2/CCR.
; CCR: reflects D0 on return.
routeDirectiveV1	.block
	tst.l d0
	beq.s normalizedMnemonic
	cmpi.b #'.', (a0)
	bne.s normalizedMnemonic
	addq.l #1, a0
	subq.l #1, d0
normalizedMnemonic
	movea.l a0, a2
	movea.l a1, a3
	move.l d0, d3
	move.l d1, d6
	lea EncMnemonicText, a1
	moveq #3, d2
	bsr.w textEquals
	beq.w selector
	; `.encoding` is deliberately checked separately: a successful `.enc`
	; comparison must not fall through and be rejected as the long spelling.
	movea.l a2, a0
	move.l d3, d0
	lea EncodingMnemonicText, a1
	moveq #8, d2
	bsr.w textEquals
	beq.w selector
	movea.l a2, a0
	move.l d3, d0
	lea EncodeMnemonicText, a1
	moveq #6, d2
	bsr.w textEquals
	beq.s beginDefinition
	movea.l a2, a0
	move.l d3, d0
	lea CdefMnemonicText, a1
	moveq #4, d2
	bsr.w textEquals
	beq.s defineCdef
	movea.l a2, a0
	move.l d3, d0
	lea TdefMnemonicText, a1
	moveq #4, d2
	bsr.w textEquals
	beq.s defineTdef
	movea.l a2, a0
	move.l d3, d0
	lea EndencodeMnemonicText, a1
	moveq #9, d2
	bsr.w textEquals
	beq.s endDefinition
	movea.l a2, a0
	move.l d3, d0
	lea EdefMnemonicText, a1
	moveq #4, d2
	bsr.w textEquals
	beq.s defineEdef
	bra.w unhandled

beginDefinition
	movea.l a3, a0
	move.l d6, d0
	bsr.w beginDefinitionV1
	bne.w fail
	moveq #1, d3
	moveq #0, d0
	rts

defineCdef
	movea.l a3, a0
	move.l d6, d0
	bsr.w defineCdefV1
	bne.w fail
	moveq #1, d3
	moveq #0, d0
	rts

defineTdef
	movea.l a3, a0
	move.l d6, d0
	bsr.w defineTdefV1
	bne.w fail
	moveq #1, d3
	moveq #0, d0
	rts

endDefinition
	movea.l a3, a0
	move.l d6, d0
	bsr.w endDefinitionV1
	bne.w fail
	moveq #1, d3
	moveq #0, d0
	rts

defineEdef
	movea.l a3, a0
	move.l d6, d0
	bsr.w defineEdefV1
	bne.w fail
	moveq #1, d3
	moveq #0, d0
	rts
selector
	moveq #1, d3
	movea.l a3, a0
	move.l d6, d0
	bsr.w skipWhitespace
	move.l d0, d3
	movea.l a0, a2
	lea AsciiText, a1
	moveq #5, d2
	bsr.w textEquals
	beq.s selectAscii
	movea.l a2, a0
	move.l d3, d0
	lea PetsciiText, a1
	moveq #7, d2
	bsr.w textEquals
	beq.s selectPetscii
	movea.l a2, a0
	move.l d3, d0
	bsr.w selectCustomEncodingV1
	bne.s fail
	moveq #1, d3
	moveq #0, d0
	rts

selectAscii
	move.w #0, TextEncodingActiveKind.l
	moveq #0, d0
	rts

selectPetscii
	move.w #1, TextEncodingActiveKind.l
	moveq #0, d0
	rts
unhandled
	clr.w d3
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; routeDirectiveV1

; Encode a mutable quoted-text scratch buffer with the active session table.
; Inputs: A0/D0 = scratch pointer/byte length.
; Outputs: D0 = 0 on success, 1 for a byte not representable by the active encoding;
;          D1 = encoded byte length (escape definitions can compact the buffer).
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0 on return.
encodeBytesV1	.block
	movem.l d5-d7/a2-a3, -(sp)
	movea.l a0, a2
	movea.l a0, a3
	move.l d0, d2
	clr.l d3
loop
	tst.l d2
	beq.w ok
	move.b (a2), d1
	tst.w TextEncodingActiveKind.l
	beq.s asciiByte
	cmpi.w #1, TextEncodingActiveKind.l
	beq.s petsciiByte
	bra.s customByte

asciiByte
	cmpi.b #$7f, d1
	bhi.w fail
	bra.w writeByte
petsciiByte
	cmpi.b #'A', d1
	bcs.s petsciiLower
	cmpi.b #'Z', d1
	bhi.s petsciiLower
	ori.b #$80, d1
	bra.w writeByte
petsciiLower
	cmpi.b #'a', d1
	bcs.w writeByte
	cmpi.b #'z', d1
	bhi.w writeByte
	subi.b #$20, d1
	bra.w writeByte

customByte
	cmpi.b #'{', d1
	bne.s customCharacter
	bsr.w tryEscapeV1
	tst.l d0
	beq.s customCharacter
	move.b d1, (a3)+
	adda.l d4, a2
	sub.l d4, d2
	addq.l #1, d3
	bra.w loop

customCharacter
	cmpi.b #$7f, d1
	bhi.w fail
	moveq #0, d5
	move.w TextEncodingActiveCustomIndex.l, d5
	lsl.l #7, d5
	moveq #0, d4
	move.b d1, d4
	add.l d4, d5
	lea TextEncodingCustomMapPresent.l, a1
	tst.b 0(a1, d5.l)
	beq.s customBase
	lea TextEncodingCustomCharMap.l, a1
	move.b 0(a1, d5.l), d1
	bra.w writeByte

customBase
	moveq #0, d4
	move.w TextEncodingActiveCustomIndex.l, d4
	lea TextEncodingCustomBaseKind.l, a1
	move.b 0(a1, d4.l), d4
	cmpi.b #1, d4
	beq.w asciiByte
	cmpi.b #2, d4
	beq.w petsciiByte
	bra.s fail

writeByte
	move.b d1, (a3)+
	addq.l #1, a2
	subq.l #1, d2
	addq.l #1, d3
	bra.w loop
ok
	move.l d3, d1
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0

return
	movem.l (sp)+, d5-d7/a2-a3
	rts
	.bend  ; encodeBytesV1

; Match the active custom encoding's first escape pattern at A2/D2.
; Inputs: A2/D2 = unread text pointer/byte count.
; Outputs: D0 = 1 and D1.B/D4.L = replacement/consumed length on match;
;          D0 = 0 when no pattern matches.
; Clobbers: D0-D7/A0-A3/CCR.
tryEscapeV1	.block
	movem.l d3/d5-d7/a0-a3, -(sp)
	moveq #0, d7
	move.w TextEncodingActiveCustomIndex.l, d7
	lea TextEncodingEscapeCounts.l, a1
	moveq #0, d6
	move.b 0(a1, d7.l), d6
	moveq #0, d5

entryLoop
	cmp.w d6, d5
	bhs.s noMatch
	moveq #0, d4
	move.w TextEncodingActiveCustomIndex.l, d4
	mulu.l #TEXT_ENCODING_ESCAPE_CAPACITY, d4
	add.l d5, d4
	lea TextEncodingEscapePatternLens.l, a1
	moveq #0, d3
	move.b 0(a1, d4.l), d3
	cmp.l d2, d3
	bhi.s nextEntry
	lea TextEncodingEscapePatterns.l, a1
	move.l d4, d0
	mulu.l #TEXT_ENCODING_ESCAPE_PATTERN_CAPACITY, d0
	adda.l d0, a1
	movea.l a2, a0
	move.l d3, d0

compareLoop
	tst.l d0
	beq.s matched
	move.b (a0)+, d7
	cmp.b (a1)+, d7
	bne.s nextEntry
	subq.l #1, d0
	bra.s compareLoop

matched
	lea TextEncodingEscapeValues.l, a1
	move.b 0(a1, d4.l), d1
	move.l d3, d4
	moveq #1, d0
	bra.s return

nextEntry
	addq.w #1, d5
	bra.s entryLoop

noMatch
	moveq #0, d0

return
	movem.l (sp)+, d3/d5-d7/a0-a3
	rts
	.bend  ; tryEscapeV1

; Select a source-defined encoding by normalized case-insensitive name.
; Inputs: A0/D0 = trimmed encoding name.
; Outputs: D0 = 0 on success, 1 when the name is unknown.
; Clobbers: D0-D7/A0-A2/CCR.
; CCR: reflects D0 on return.
selectCustomEncodingV1	.block
	move.l d7, -(sp)
	movea.l a0, a2
	move.l d0, d3
	moveq #0, d7

lookup
	cmp.w TextEncodingCustomCount.l, d7
	bhs.s fail
	moveq #0, d2
	lea TextEncodingCustomNameLens.l, a1
	move.b 0(a1, d7.w), d2
	lea TextEncodingCustomNames.l, a1
	move.l d7, d4
	mulu.l #TEXT_ENCODING_NAME_CAPACITY, d4
	adda.l d4, a1
	movea.l a2, a0
	move.l d3, d0
	bsr.w textEquals
	beq.s found
	addq.w #1, d7
	bra.s lookup

found
	move.w #2, TextEncodingActiveKind.l
	move.w d7, TextEncodingActiveCustomIndex.l
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	move.l (sp)+, d7
	tst.l d0
	rts
	.bend  ; selectCustomEncodingV1

; Begin a source-defined encoding with an optional built-in base table.
; Inputs: A0/D0 = `.encode` operand text.
; Outputs: D0 = 0 on success, 1 for malformed names or exhausted storage.
; Clobbers: D0-D6/A0-A3/CCR.
; CCR: reflects D0 on return.
beginDefinitionV1	.block
	move.l d7, -(sp)
	tst.w TextEncodingDefinitionOpen.l
	bne.w fail
	move.w TextEncodingActiveKind.l, d4
	move.w d4, TextEncodingPreviousKind.l
	move.w TextEncodingActiveCustomIndex.l, d4
	move.w d4, TextEncodingPreviousCustomIndex.l
	bsr.w skipWhitespace
	movea.l a0, a2
	moveq #0, d3

scanName
	tst.l d0
	beq.w nameEnd
	move.b (a0), d4
	cmpi.b #',', d4
	beq.w nameEnd
	cmpi.b #' ', d4
	beq.w nameEnd
	cmpi.b #9, d4
	beq.w nameEnd
	addq.l #1, a0
	subq.l #1, d0
	addq.l #1, d3
	bra.s scanName

nameEnd
	tst.l d3
	beq.w fail
	cmpi.l #TEXT_ENCODING_NAME_CAPACITY, d3
	bhs.w fail
	moveq #0, d6
	move.l a2, -(sp)
	bsr.w skipWhitespace
	movea.l (sp)+, a2
	tst.l d0
	beq.s ensure
	cmpi.b #',', (a0)
	bne.w fail
	addq.l #1, a0
	subq.l #1, d0
	move.l a2, -(sp)
	bsr.w skipWhitespace
	movea.l (sp)+, a2
	movea.l a0, a3
	move.l d0, d7
	lea AsciiText, a1
	moveq #5, d2
	bsr.w textEquals
	beq.s asciiBase
	movea.l a3, a0
	move.l d7, d0
	lea PetsciiText, a1
	moveq #7, d2
	bsr.w textEquals
	bne.w fail
	moveq #2, d6
	bra.s ensure

asciiBase
	moveq #1, d6

ensure
	move.l d3, -(sp)
	move.l d6, -(sp)
	move.l a2, -(sp)
	movea.l a2, a0
	move.l d3, d0
	bsr.w selectCustomEncodingV1
	movea.l (sp)+, a2
	move.l (sp)+, d6
	move.l (sp)+, d3
	beq.s open
	moveq #0, d4
	move.w TextEncodingCustomCount.l, d4
	cmpi.w #TEXT_ENCODING_CUSTOM_CAPACITY, d4
	bhs.s fail
	lea TextEncodingCustomNameLens.l, a1
	move.b d3, 0(a1, d4.w)
	lea TextEncodingCustomNames.l, a1
	move.l d4, d5
	mulu.l #TEXT_ENCODING_NAME_CAPACITY, d5
	adda.l d5, a1
	movea.l a2, a0
	move.l d3, d5

copyName
	move.b (a0)+, d1
	andi.b #$df, d1
	move.b d1, (a1)+
	subq.l #1, d5
	bne.s copyName
	move.b #0, (a1)
	lea TextEncodingCustomBaseKind.l, a1
	move.b d6, 0(a1, d4.w)
	addq.w #1, TextEncodingCustomCount.l
	move.w #2, TextEncodingActiveKind.l
	move.w d4, TextEncodingActiveCustomIndex.l

open
	move.w #1, TextEncodingDefinitionOpen.l
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	move.l (sp)+, d7
	tst.l d0
	rts
	.bend  ; beginDefinitionV1

; Close the current `.encode` definition and restore its active encoding.
; Inputs: A0/D0 = `.endencode` operand text.
; Outputs: D0 = 0 on success, 1 for operands or no open definition.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
endDefinitionV1	.block
	tst.l d0
	bne.s fail
	tst.w TextEncodingDefinitionOpen.l
	beq.s fail
	clr.w TextEncodingDefinitionOpen.l
	move.w TextEncodingPreviousKind.l, d0
	move.w d0, TextEncodingActiveKind.l
	move.w TextEncodingPreviousCustomIndex.l, d0
	move.w d0, TextEncodingActiveCustomIndex.l
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; endDefinitionV1

; Define an inclusive single-byte character range in the active encoding.
; Inputs: A0/D0 = `.cdef` operand text.
; Outputs: D0 = 0 on success, 1 for malformed input or no open encoding.
; Clobbers: D0-D6/A0-A2/CCR.
; CCR: reflects D0 on return.
defineCdefV1	.block
	move.l d7, -(sp)
	cmpi.w #2, TextEncodingActiveKind.l
	bne.w fail
	tst.w TextEncodingDefinitionOpen.l
	beq.w fail
	bsr.w skipWhitespace
	bsr.w readQuotedSourceByteV1
	bne.w fail
	move.l d2, d0
	moveq #0, d3
	move.b d1, d3
	cmpi.b #$7f, d3
	bhi.w fail
	bsr.w skipDefinitionCommaV1
	bne.w fail
	move.l d2, d0
	bsr.w readQuotedSourceByteV1
	bne.w fail
	move.l d2, d0
	moveq #0, d4
	move.b d1, d4
	cmpi.b #$7f, d4
	bhi.w fail
	cmp.l d3, d4
	bcs.w fail
	movem.l d3-d4, -(sp)
	bsr.w skipDefinitionCommaV1
	bne.s cdefValueFail
	move.l d2, d0
	bsr.w readDefinitionByteValueV1
cdefValueFail
	movem.l (sp)+, d3-d4
	bne.w fail
	move.l d2, d0
	moveq #0, d5
	move.b d1, d5
	movem.l d3-d6, -(sp)
	bsr.w skipWhitespace
	movem.l (sp)+, d3-d6
	tst.l d0
	bne.w fail
	moveq #0, d6
	move.w TextEncodingActiveCustomIndex.l, d6
	lsl.l #7, d6
	move.l d4, d0
	sub.l d3, d0
	addq.l #1, d0

mapRange
	move.l d6, d2
	add.l d3, d2
	lea TextEncodingCustomCharMap.l, a1
	move.b d5, 0(a1, d2.l)
	lea TextEncodingCustomMapPresent.l, a1
	move.b #1, 0(a1, d2.l)
	addq.l #1, d3
	addq.l #1, d5
	subq.l #1, d0
	bne.s mapRange
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	move.l (sp)+, d7
	tst.l d0
	rts
	.bend  ; defineCdefV1

; Define explicit or incrementing byte mappings for a source string.
; Inputs: A0/D0 = `.tdef` operand text.
; Outputs: D0 = 0 on success, 1 for malformed input or no open encoding.
; Clobbers: D0-D6/A0-A3/CCR.
; CCR: reflects D0 on return.
defineTdefV1	.block
	move.l d7, -(sp)
	cmpi.w #2, TextEncodingActiveKind.l
	bne.w fail
	tst.w TextEncodingDefinitionOpen.l
	beq.w fail
	bsr.w skipWhitespace
	tst.l d0
	beq.w fail
	cmpi.b #'"', (a0)
	bne.w fail
	addq.l #1, a0
	subq.l #1, d0
	movea.l a0, a2
	moveq #0, d3

scanChars
	tst.l d0
	beq.w fail
	cmpi.b #'"', (a0)
	beq.s charsEnd
	addq.l #1, a0
	subq.l #1, d0
	addq.l #1, d3
	bra.s scanChars

charsEnd
	tst.l d3
	beq.w fail
	addq.l #1, a0
	subq.l #1, d0
	move.l d3, -(sp)
	move.l a2, -(sp)
	bsr.w skipDefinitionCommaV1
	bne.s tdefFirstValueFail
	move.l d2, d0
	bsr.w readDefinitionByteValueV1
tdefFirstValueFail
	movea.l (sp)+, a2
	move.l (sp)+, d3
	tst.l d0
	bne.w fail
	move.l d2, d0
	moveq #0, d5
	moveq #0, d6
	move.w TextEncodingActiveCustomIndex.l, d6
	lsl.l #7, d6
	movem.l d3-d6/a2, -(sp)
	bsr.w skipWhitespace
	movem.l (sp)+, d3-d6/a2
	tst.l d0
	beq.s incrementingValues
	movea.l a0, a3
	move.l d0, d2
	bra.s explicitValues

incrementingValues
	bsr.w mapTdefCharacterV1
	bne.w fail
	addq.l #1, d1
	addq.l #1, d5
	cmp.l d3, d5
	blo.s incrementingValues
	moveq #0, d0
	bra.w return

explicitValues
	bsr.w mapTdefCharacterV1
	bne.w fail
	addq.l #1, d5
	cmp.l d3, d5
	beq.s explicitDone
	movea.l a3, a0
	move.l d2, d0
	move.l d3, -(sp)
	move.l d5, -(sp)
	move.l d6, -(sp)
	move.l a2, -(sp)
	bsr.w skipDefinitionCommaV1
	bne.s tdefExplicitValueFail
	move.l d2, d0
	bsr.w readDefinitionByteValueV1
tdefExplicitValueFail
	movea.l (sp)+, a2
	move.l (sp)+, d6
	move.l (sp)+, d5
	move.l (sp)+, d3
	tst.l d0
	bne.w fail
	move.l d2, d0
	move.l a2, -(sp)
	bsr.w skipWhitespace
	movea.l (sp)+, a2
	movea.l a0, a3
	move.l d0, d2
	bra.s explicitValues

explicitDone
	cmp.l d3, d5
	bne.w fail
	moveq #0, d0
	bra.w return

fail
	moveq #1, d0

return
	move.l (sp)+, d7
	tst.l d0
	rts
	.bend  ; defineTdefV1

; Store a one-byte replacement for a braced escape pattern.
; Inputs: A0/D0 = `.edef` operand text.
; Outputs: D0 = 0 on success, 1 for malformed input or exhausted storage.
; Clobbers: D0-D6/A0-A3/CCR.
; CCR: reflects D0 on return.
defineEdefV1	.block
	move.l d7, -(sp)
	cmpi.w #2, TextEncodingActiveKind.l
	bne.w fail
	tst.w TextEncodingDefinitionOpen.l
	beq.w fail
	bsr.w skipWhitespace
	tst.l d0
	beq.w fail
	cmpi.b #'"', (a0)
	bne.w fail
	addq.l #1, a0
	subq.l #1, d0
	movea.l a0, a2
	moveq #0, d3

scanPattern
	tst.l d0
	beq.w fail
	cmpi.b #'"', (a0)
	beq.s patternEnd
	addq.l #1, a0
	subq.l #1, d0
	addq.l #1, d3
	bra.s scanPattern

patternEnd
	tst.l d3
	beq.w fail
	cmpi.l #TEXT_ENCODING_ESCAPE_PATTERN_CAPACITY, d3
	bhs.w fail
	addq.l #1, a0
	subq.l #1, d0
	move.l d3, -(sp)
	move.l a2, -(sp)
	bsr.w skipDefinitionCommaV1
	bne.s edefValueFail
	move.l d2, d0
	bsr.w readDefinitionByteValueV1
edefValueFail
	movea.l (sp)+, a2
	move.l (sp)+, d3
	tst.l d0
	bne.w fail
	move.l d2, d0
	move.l a2, -(sp)
	bsr.w skipWhitespace
	movea.l (sp)+, a2
	tst.l d0
	bne.w fail
	moveq #0, d4
	move.w TextEncodingActiveCustomIndex.l, d4
	lea TextEncodingEscapeCounts.l, a1
	moveq #0, d5
	move.b 0(a1, d4.l), d5
	cmpi.b #TEXT_ENCODING_ESCAPE_CAPACITY, d5
	bhs.w fail
	move.l d4, d6
	mulu.l #TEXT_ENCODING_ESCAPE_CAPACITY, d6
	add.l d5, d6
	lea TextEncodingEscapePatternLens.l, a1
	move.b d3, 0(a1, d6.l)
	lea TextEncodingEscapeValues.l, a1
	move.b d1, 0(a1, d6.l)
	lea TextEncodingEscapePatterns.l, a1
	move.l d6, d4
	mulu.l #TEXT_ENCODING_ESCAPE_PATTERN_CAPACITY, d4
	adda.l d4, a1
	movea.l a2, a0
	move.l d3, d4

copyPattern
	move.b (a0)+, (a1)+
	subq.l #1, d4
	bne.s copyPattern
	moveq #0, d4
	move.w TextEncodingActiveCustomIndex.l, d4
	lea TextEncodingEscapeCounts.l, a1
	addq.b #1, 0(a1, d4.l)
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	move.l (sp)+, d7
	tst.l d0
	rts
	.bend  ; defineEdefV1

	.priv

; Store D1.B for character index D5 in A2 at custom-table base D6.
; Outputs: D0 = 0 on success, 1 for an unsupported source byte.
mapTdefCharacterV1	.block
	moveq #0, d4
	move.b 0(a2, d5.l), d4
	cmpi.b #$7f, d4
	bhi.s fail
	add.l d6, d4
	lea TextEncodingCustomCharMap.l, a1
	move.b d1, 0(a1, d4.l)
	lea TextEncodingCustomMapPresent.l, a1
	move.b #1, 0(a1, d4.l)
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; mapTdefCharacterV1

; Consume a one-byte quoted source character from A0/D0.
; Outputs: D0 = 0 on success, 1 on malformed input; D1.B = character.
readQuotedSourceByteV1	.block
	tst.l d0
	beq.s fail
	cmpi.b #'"', (a0)
	bne.s fail
	addq.l #1, a0
	subq.l #1, d0
	tst.l d0
	beq.s fail
	move.b (a0)+, d1
	subq.l #1, d0
	tst.l d0
	beq.s fail
	cmpi.b #'"', (a0)
	bne.s fail
	addq.l #1, a0
	subq.l #1, d0
	move.l d0, d2
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; readQuotedSourceByteV1

; Consume a comma between source-definition operands.
skipDefinitionCommaV1	.block
	bsr.w skipWhitespace
	tst.l d0
	beq.s fail
	cmpi.b #',', (a0)
	bne.s fail
	addq.l #1, a0
	subq.l #1, d0
	bsr.w skipWhitespace
	move.l d0, d2
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; skipDefinitionCommaV1

; Parse a decimal or `$`-prefixed hexadecimal byte from A0/D0.
; Outputs: D0 = 0 on success, 1 on malformed/out-of-range input; D1.B = value.
readDefinitionByteValueV1	.block
	moveq #10, d2
	moveq #0, d1
	tst.l d0
	beq.s fail
	cmpi.b #'$', (a0)
	bne.s digit
	moveq #16, d2
	addq.l #1, a0
	subq.l #1, d0
	tst.l d0
	beq.s fail

digit
	moveq #0, d3

scan
	tst.l d0
	beq.s done
	move.b (a0), d4
	cmpi.b #' ', d4
	beq.s done
	cmpi.b #9, d4
	beq.s done
	cmpi.b #',', d4
	beq.s done
	cmpi.b #'0', d4
	bcs.s fail
	cmpi.b #'9', d4
	bls.s decimal
	andi.b #$df, d4
	cmpi.b #'A', d4
	bcs.s fail
	cmpi.b #'F', d4
	bhi.s fail
	subi.b #'A' - 10, d4
	bra.s accumulate

decimal
	subi.b #'0', d4

accumulate
	cmp.b d2, d4
	bhs.s fail
	mulu.l d2, d1
	add.l d4, d1
	bcs.s fail
	cmpi.l #$000000ff, d1
	bhi.s fail
	addq.l #1, a0
	subq.l #1, d0
	addq.l #1, d3
	bra.s scan

done
	tst.l d3
	beq.s fail
	move.l d0, d2
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; readDefinitionByteValueV1

; Compare A0/D0 against A1/D2, case-insensitively.  Z set on equality.
textEquals	.block
	cmp.l d2, d0
	bne.s no
compare
	tst.l d2
	beq.s yes
	move.b (a0)+, d4
	move.b (a1)+, d5
	andi.b #$df, d4
	andi.b #$df, d5
	cmp.b d5, d4
	bne.s no
	subq.l #1, d2
	bra.s compare
yes
	moveq #0, d0
	rts
no
	moveq #1, d0
	rts
	.bend  ; textEquals

; Trim leading and trailing spaces/tabs from A0/D0.
skipWhitespace	.block
lead
	tst.l d0
	beq.s done
	cmpi.b #' ', (a0)
	beq.s skip
	cmpi.b #9, (a0)
	bne.s trailing
skip
	addq.l #1, a0
	subq.l #1, d0
	bra.s lead
trailing
	movea.l a0, a2
	adda.l d0, a2
tail
	tst.l d0
	beq.s done
	move.b -(a2), d4
	cmpi.b #' ', d4
	beq.s trim
	cmpi.b #9, d4
	bne.s done
trim
	subq.l #1, d0
	bra.s tail
done
	rts
	.bend  ; skipWhitespace

	.endsection

	.section bss, kind=bss

	.align 2

; 0 = ASCII (Rust's default); 1 = PETSCII.  Subsequent routines extend this
; compact session record with named source-defined encodings.
TextEncodingActiveKind
	.res word, 1
TextEncodingActiveCustomIndex
	.res word, 1
TextEncodingDefinitionOpen
	.res word, 1
TextEncodingPreviousKind
	.res word, 1
TextEncodingPreviousCustomIndex
	.res word, 1
TextEncodingCustomCount
	.res word, 1

; Each user encoding owns a normalized name, one 7-bit character table, and
; a presence bit so byte value zero remains representable.
TextEncodingCustomNames
	.res byte, TEXT_ENCODING_CUSTOM_CAPACITY * TEXT_ENCODING_NAME_CAPACITY
TextEncodingCustomNameLens
	.res byte, TEXT_ENCODING_CUSTOM_CAPACITY
TextEncodingCustomBaseKind
	.res byte, TEXT_ENCODING_CUSTOM_CAPACITY
TextEncodingCustomCharMap
	.res byte, TEXT_ENCODING_CUSTOM_CAPACITY * TEXT_ENCODING_CHAR_MAP_BYTES
TextEncodingCustomMapPresent
	.res byte, TEXT_ENCODING_CUSTOM_CAPACITY * TEXT_ENCODING_CHAR_MAP_BYTES
TextEncodingEscapeCounts
	.res byte, TEXT_ENCODING_CUSTOM_CAPACITY
TextEncodingEscapePatternLens
	.res byte, TEXT_ENCODING_CUSTOM_CAPACITY * TEXT_ENCODING_ESCAPE_CAPACITY
TextEncodingEscapeValues
	.res byte, TEXT_ENCODING_CUSTOM_CAPACITY * TEXT_ENCODING_ESCAPE_CAPACITY
TextEncodingEscapePatterns
	.res byte, TEXT_ENCODING_CUSTOM_CAPACITY * TEXT_ENCODING_ESCAPE_CAPACITY * TEXT_ENCODING_ESCAPE_PATTERN_CAPACITY

	.endsection

	.section data, kind=data
EncMnemonicText
	.byte "enc"
EncodingMnemonicText
	.byte "encoding"
EncodeMnemonicText
	.byte "encode"
CdefMnemonicText
	.byte "cdef"
TdefMnemonicText
	.byte "tdef"
EndencodeMnemonicText
	.byte "endencode"
EdefMnemonicText
	.byte "edef"
AsciiText
	.byte "ascii"
PetsciiText
	.byte "petscii"
	.endsection

	.endmodule

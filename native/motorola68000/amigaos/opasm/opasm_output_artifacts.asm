; Native opasm output artifact builders.

	.module opasm.amigaos.output_artifacts
	.cpu 68020

	.use opasm.amigaos.engine
	.use opasm.amigaos.flow_scopes as scopes
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	.use debug.amigaos.platform_profile as platform_profile
.endif

OPASM_OUTPUT_PRG_BUFFER_CAPACITY = 4098
OPASM_OUTPUT_HEX_BUFFER_CAPACITY = 12000
OPASM_OUTPUT_LISTING_BUFFER_CAPACITY = 24000

	.section code, kind=code
	.pub

; ---------------------------------------------------------------------------
; Build the first-run flat `.bin` artifact payload from the opasm image.
;
; Inputs:
; - opasm engine image buffer/count contain the assembled flat output.
;
; Outputs:
; - D0.L: status, 0 on success.
; - D1.L: artifact byte count.
; - A0: artifact payload pointer.
;
; Clobbers:
; - D0-D1/A0-A1/CCR
;
; CCR:
; - Reflects D0 on return.
; ---------------------------------------------------------------------------
opasmOutputBuildBinArtifactV1	.block
	jsr engine.opasmEngineGetImageBufferPtrV1
	movea.l a0, a1
	jsr engine.opasmEngineGetImageByteCountV1
	move.l d0, d1
	movea.l a1, a0
	moveq #0, d0
	rts
	.bend  ; opasmOutputBuildBinArtifactV1

; Return one validated byte range from the current engine image. This keeps
; engine-image ownership inside the artifact layer even when a higher-level
; writer is composing selected source sections.
; Inputs: D2.L = image offset; D3.L = byte count.
; Outputs: D0.L = 0 on success, 1 when offset + count exceeds the image;
;          A0 = first requested byte on success.
; Clobbers: D0-D1/A0/CCR.
opasmOutputGetImageRangeV1	.block
	move.l d2, d1
	add.l d3, d1
	bcs.s fail
	jsr engine.opasmEngineGetImageByteCountV1
	cmp.l d0, d1
	bhi.s fail
	jsr engine.opasmEngineGetImageBufferPtrV1
	adda.l d2, a0
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opasmOutputGetImageRangeV1

; Return the origin associated with the current artifact image.
; Outputs: D0.L = session origin.
opasmOutputGetSessionOriginV1	.block
	jsr engine.opasmEngineGetSessionOriginV1
	rts
	.bend  ; opasmOutputGetSessionOriginV1

; Build a Commodore PRG artifact from the current engine image.
; Inputs:
; - D2.L = load address, or -1 to use the session origin.
; Outputs:
; - D0.L = 0 on success, 1 on invalid load address.
; - A0 = opasm-owned PRG artifact buffer pointer.
; - D1.L = byte count including the two-byte load address prefix.
opasmOutputBuildPrgArtifactV1	.block
	cmpi.l #$FFFFFFFF, d2
	bne.s haveLoadAddr
	jsr engine.opasmEngineGetSessionOriginV1
	move.l d0, d2

haveLoadAddr
	cmpi.l #$0000FFFF, d2
	bhi.s fail
	lea OpasmPrgArtifactBuffer.l, a2
	move.b d2, (a2)+
	move.l d2, d0
	lsr.w #8, d0
	move.b d0, (a2)+
	jsr opasmOutputBuildBinArtifactV1
	bne.s fail
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	move.l d0, -(sp)
	move.l d1, d0
	jsr platform_profile.opforgePlatformProfileCopyRequestedV1
	move.l (sp)+, d0
	move.l a0, -(sp)
.endif
	move.l d1, d3
	beq.s doneCopy
	subq.l #1, d3

copyLoop
	move.b (a0)+, (a2)+
	dbra d3, copyLoop

doneCopy
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	move.l d0, -(sp)
	move.l a0, d0
	sub.l 4(sp), d0
	jsr platform_profile.opforgePlatformProfileCopyCompletedV1
	move.l (sp)+, d0
	lea 4(sp), sp
.endif
	addi.l #2, d1
	lea OpasmPrgArtifactBuffer.l, a0
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opasmOutputBuildPrgArtifactV1

; Build an Intel HEX artifact from the current contiguous engine image.
; Outputs:
; - D0.L = 0 on success, 1 when output exceeds 16-bit HEX address range.
; - A0 = opasm-owned HEX artifact buffer pointer.
; - D1.L = text byte count.
opasmOutputBuildHexArtifactV1	.block
	movem.l d2-d7/a2-a3, -(sp)
	jsr engine.opasmEngineGetSessionOriginV1
	cmpi.l #$0000FFFF, d0
	bhi.w fail
	move.l d0, d6
	jsr opasmOutputBuildBinArtifactV1
	bne.w fail
	movea.l a0, a3
	move.l d1, d5
	lea OpasmHexArtifactBuffer.l, a2
	tst.l d5
	beq.w eofRecord
	move.l d6, d0
	add.l d5, d0
	subq.l #1, d0
	cmpi.l #$0000FFFF, d0
	bhi.w fail

recordLoop
	move.l #255, d7
	cmp.l d7, d5
	bhs.s haveRecordLen
	move.l d5, d7

haveRecordLen
	move.b #':', (a2)+
	move.l d7, d3
	move.l d7, d0
	bsr.w opasmOutputEmitHexByte
	move.l d6, d0
	lsr.w #8, d0
	andi.l #$000000FF, d0
	add.l d0, d3
	bsr.w opasmOutputEmitHexByte
	move.l d6, d0
	andi.l #$000000FF, d0
	add.l d0, d3
	bsr.w opasmOutputEmitHexByte
	moveq #0, d0
	bsr.w opasmOutputEmitHexByte
	move.l d7, d4
	subq.l #1, d4

dataLoop
	moveq #0, d0
	move.b (a3)+, d0
	add.l d0, d3
	bsr.w opasmOutputEmitHexByte
	dbra d4, dataLoop
	move.l d3, d0
	neg.l d0
	andi.l #$000000FF, d0
	bsr.w opasmOutputEmitHexByte
	move.b #10, (a2)+
	sub.l d7, d5
	add.l d7, d6
	tst.l d5
	bne.w recordLoop

eofRecord
	lea OpasmHexEofRecord.l, a0
	moveq #11, d0

copyEof
	move.b (a0)+, (a2)+
	dbra d0, copyEof
	lea OpasmHexArtifactBuffer.l, a0
	move.l a2, d1
	move.l a0, d0
	sub.l d0, d1
	moveq #0, d0
	movem.l (sp)+, d2-d7/a2-a3
	rts

fail
	moveq #1, d0
	movem.l (sp)+, d2-d7/a2-a3
	rts
	.bend  ; opasmOutputBuildHexArtifactV1

; Build a Rust-style `.lst` artifact from every preserved source record,
; attaching statement/image data when the source line produced a statement.
; Outputs:
; - D0.L = 0 on success.
; - A0 = opasm-owned listing artifact buffer pointer.
; - D1.L = text byte count.
opasmOutputBuildListingArtifactV1	.block
	movem.l d2-d7/a2-a6, -(sp)
	lea OpasmListingArtifactBuffer.l, a2
	lea OpasmListingTitle.l, a0
	bsr.w opasmOutputAppendCString
	lea OpasmListingHeader.l, a0
	bsr.w opasmOutputAppendCString
	jsr engine.opasmEngineGetSourceRecordCountV1
	move.l d0, d6
	moveq #0, d7

lineLoop
	cmp.l d6, d7
	bhs.w footer
	move.l d7, d0
	jsr engine.opasmEngineGetSourceRecordLineNumberV1
	move.l d0, d5
	bsr.w opasmOutputFindStatementForLineV1
	move.l d0, d2
	bmi.s noLineBytes
	move.l d2, d0
	jsr engine.opasmEngineGetStatementOutputByteCountV1
	move.l d0, d5
	beq.s checkOrg
	move.l d2, d0
	jsr engine.opasmEngineGetStatementOutputAddrV1
	bsr.w opasmOutputAppendHexWord
	moveq #2, d0
	bsr.w opasmOutputAppendSpaces
	bra.s locDone

checkOrg
	move.l d2, d0
	jsr engine.opasmEngineStatementIsOrgV1
	beq.s noLineBytes
	lea OpasmListingNoLocation.l, a0
	bsr.w opasmOutputAppendCString
	moveq #2, d0
	bsr.w opasmOutputAppendSpaces
	lea OpasmListingEquPrefix.l, a0
	bsr.w opasmOutputAppendCString
	jsr engine.opasmEngineGetSessionOriginV1
	bsr.w opasmOutputAppendHexWord
	moveq #8, d4
	bra.s bytesDone

noLineBytes
	moveq #0, d5
	lea OpasmListingNoLocation.l, a0
	bsr.w opasmOutputAppendCString

locDone
	moveq #2, d0
	bsr.w opasmOutputAppendSpaces
	moveq #0, d4
	tst.l d5
	beq.s bytesDone
	move.l d2, d0
	jsr engine.opasmEngineGetStatementOutputOffsetV1
	move.l d0, d3
	jsr engine.opasmEngineGetImageBufferPtrV1
	adda.l d3, a0
	movea.l a0, a3
	move.l d5, d4
	add.l d4, d4
	add.l d5, d4
	subq.l #1, d4
	move.l d5, d3
	subq.l #1, d3

byteLoop
	moveq #0, d0
	move.b (a3)+, d0
	bsr.w opasmOutputEmitHexByte
	tst.l d3
	beq.s bytesDone
	move.b #' ', (a2)+
	subq.l #1, d3
	bra.s byteLoop

bytesDone
	move.l #23, d0
	cmp.l d0, d4
	bhs.s byteColumnDone
	sub.l d4, d0
	bsr.w opasmOutputAppendSpaces

byteColumnDone
	moveq #2, d0
	bsr.w opasmOutputAppendSpaces
	move.l d7, d0
	jsr engine.opasmEngineGetSourceRecordLineNumberV1
	bsr.w opasmOutputAppendRight4Decimal
	moveq #2, d0
	bsr.w opasmOutputAppendSpaces
	move.l d7, d0
	jsr engine.opasmEngineGetSourceRecordTextV1
	tst.l d0
	beq.s sourceDone
	move.l d0, d3

sourceLoop
	move.b (a0)+, (a2)+
	subq.l #1, d3
	bne.s sourceLoop

sourceDone
	move.b #10, (a2)+
	addq.l #1, d7
	bra.w lineLoop

footer
	lea OpasmListingLinesPrefix.l, a0
	bsr.w opasmOutputAppendCString
	move.l d6, d0
	bsr.w opasmOutputAppendU16Decimal
	lea OpasmListingCountsSuffix.l, a0
	bsr.w opasmOutputAppendCString
	jsr engine.opasmEngineGetLabelCountV1
	move.l d0, d3
	beq.w symbolNone
	lea OpasmListingSymbolHeader.l, a0
	bsr.w opasmOutputAppendCString
	moveq #0, d2

symbolLoop
	moveq #0, d4
	jsr scopes.rootModuleNameV1
	tst.l d0
	beq.s symbolRawName
	move.l d0, d6
symbolModuleLoop
	cmpi.l #15, d4
	bhs.s symbolRawName
	move.b (a0)+, (a2)+
	addq.l #1, d4
	subq.l #1, d6
	bne.s symbolModuleLoop
	cmpi.l #15, d4
	bhs.s symbolRawName
	move.b #'.', (a2)+
	addq.l #1, d4

symbolRawName
	move.l d2, d0
	jsr engine.opasmEngineGetLabelNameV1

symbolNameLoop
	tst.b (a0)
	beq.s symbolNameDone
	move.b (a0)+, (a2)+
	addq.l #1, d4
	cmpi.l #15, d4
	blo.s symbolNameLoop

symbolNameDone
	move.l #17, d0
	sub.l d4, d0
	bsr.w opasmOutputAppendSpaces
	move.l d2, d0
	jsr engine.opasmEngineGetLabelValueV1
	bsr.w opasmOutputAppendHexWord
	lea OpasmListingSymbolSuffix.l, a0
	bsr.w opasmOutputAppendCString
	addq.l #1, d2
	cmp.l d3, d2
	blo.s symbolLoop
	bra.s symbolsDone

symbolNone
	lea OpasmListingSymbolNone.l, a0
	bsr.w opasmOutputAppendCString

symbolsDone
	jsr engine.opasmEngineGetImageByteCountV1
	move.l d0, d5
	lea OpasmListingMemoryPrefix.l, a0
	bsr.w opasmOutputAppendCString
	move.l d5, d0
	bsr.w opasmOutputAppendU16Decimal
	lea OpasmListingMemorySuffix.l, a0
	bsr.w opasmOutputAppendCString
	lea OpasmListingGeneratedHeader.l, a0
	bsr.w opasmOutputAppendCString
	tst.l d5
	beq.s generatedNone
	jsr engine.opasmEngineGetImageBufferPtrV1
	movea.l a0, a3
	jsr engine.opasmEngineGetSessionOriginV1
	move.l d0, d4
	move.l d5, d3

generatedLoop
	move.l d4, d0
	bsr.w opasmOutputAppendHexWord
	moveq #4, d0
	bsr.w opasmOutputAppendSpaces
	moveq #16, d2

generatedByteLoop
	moveq #0, d0
	move.b (a3)+, d0
	bsr.w opasmOutputEmitHexByte
	subq.l #1, d3
	addq.l #1, d4
	tst.l d3
	beq.s generatedDone
	subq.l #1, d2
	beq.s generatedNextLine
	move.b #' ', (a2)+
	bra.s generatedByteLoop

generatedNextLine
	move.b #10, (a2)+
	bra.s generatedLoop

generatedDone
	move.b #10, (a2)+
	bra.s finish

generatedNone
	lea OpasmListingNoneLine.l, a0
	bsr.w opasmOutputAppendCString

finish
	lea OpasmListingArtifactBuffer.l, a0
	move.l a2, d1
	move.l a0, d0
	sub.l d0, d1
	moveq #0, d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; opasmOutputBuildListingArtifactV1

; Find the statement associated with one source line.
; Inputs: D5 = source line number.
; Outputs: D0 = statement index, or -1 when the line has no statement.
opasmOutputFindStatementForLineV1	.block
	movem.l d1-d3, -(sp)
	jsr engine.opasmEngineGetStatementCountV1
	move.l d0, d3
	moveq #0, d2

scan
	cmp.l d3, d2
	bhs.s absent
	move.l d2, d0
	jsr engine.opasmEngineGetStatementLineNumberV1
	cmp.l d5, d0
	beq.s found
	addq.l #1, d2
	bra.s scan

found
	move.l d2, d0
	movem.l (sp)+, d1-d3
	rts

absent
	moveq #-1, d0
	movem.l (sp)+, d1-d3
	rts
	.bend  ; opasmOutputFindStatementForLineV1

; Append one byte as two uppercase hexadecimal characters.
; Inputs: D0.B = byte; A2 = destination cursor.
; Outputs: A2 advanced by two bytes.
opasmOutputEmitHexByte	.block
	movem.l d0-d2/a1, -(sp)
	andi.l #$000000FF, d0
	lea OpasmHexDigits.l, a1
	move.l d0, d1
	lsr.b #4, d1
	move.b 0(a1, d1.l), (a2)+
	andi.b #$0F, d0
	move.b 0(a1, d0.l), (a2)+
	movem.l (sp)+, d0-d2/a1
	rts
	.bend  ; opasmOutputEmitHexByte

opasmOutputAppendHexWord	.block
	movem.l d0/d2, -(sp)
	move.l d0, d2
	lsr.w #8, d0
	bsr.w opasmOutputEmitHexByte
	move.l d2, d0
	bsr.w opasmOutputEmitHexByte
	movem.l (sp)+, d0/d2
	rts
	.bend  ; opasmOutputAppendHexWord

opasmOutputAppendCString	.block
	tst.b (a0)
	beq.s done

loop
	move.b (a0)+, (a2)+
	tst.b (a0)
	bne.s loop

done
	rts
	.bend  ; opasmOutputAppendCString

opasmOutputAppendSpaces	.block
	tst.l d0
	beq.s done

loop
	move.b #' ', (a2)+
	subq.l #1, d0
	bne.s loop

done
	rts
	.bend  ; opasmOutputAppendSpaces

opasmOutputAppendRight4Decimal	.block
	movem.l d0/d1, -(sp)
	move.l d0, d1
	cmpi.l #10, d1
	bhs.s maybeHundred
	moveq #3, d0
	bsr.w opasmOutputAppendSpaces
	bra.s number

maybeHundred
	cmpi.l #100, d1
	bhs.s maybeThousand
	moveq #2, d0
	bsr.w opasmOutputAppendSpaces
	bra.s number

maybeThousand
	cmpi.l #1000, d1
	bhs.s number
	moveq #1, d0
	bsr.w opasmOutputAppendSpaces

number
	move.l d1, d0
	bsr.w opasmOutputAppendU16Decimal
	movem.l (sp)+, d0/d1
	rts
	.bend  ; opasmOutputAppendRight4Decimal

opasmOutputAppendU16Decimal	.block
	movem.l d0-d5, -(sp)
	move.l d0, d1
	moveq #0, d5
	move.w #10000, d2
	bsr.s decimalDigit
	move.w #1000, d2
	bsr.s decimalDigit
	move.w #100, d2
	bsr.s decimalDigit
	move.w #10, d2
	bsr.s decimalDigit
	move.l d1, d0
	addi.b #'0', d0
	move.b d0, (a2)+
	movem.l (sp)+, d0-d5
	rts

decimalDigit
	move.l d1, d0
	divu.w d2, d0
	move.l d0, d3
	andi.l #$0000FFFF, d3
	swap d0
	andi.l #$0000FFFF, d0
	move.l d0, d1
	tst.l d3
	bne.s emit
	tst.l d5
	beq.s skip

emit
	moveq #1, d5
	move.l d3, d4
	addi.b #'0', d4
	move.b d4, (a2)+

skip
	rts
	.bend  ; opasmOutputAppendU16Decimal

	.endsection

	.section data, kind=data

OpasmHexDigits
	.byte "0123456789ABCDEF"
OpasmHexEofRecord
	.byte ":00000001FF", 10
OpasmListingTitle
	.byte "opForge Assembler v0.9.7 | full-runtime | bundled", 10, 0
OpasmListingHeader
	.byte "ADDR    BYTES                    LINE  SOURCE", 10
	.byte "------  -----------------------  ----  ------", 10, 0
OpasmListingNoLocation
	.byte "----  ", 0
OpasmListingEquPrefix
	.byte "EQU ", 0
OpasmListingLinesPrefix
	.byte 10, "Lines: ", 0
OpasmListingCountsSuffix
	.byte "  Errors: 0  Warnings: 0", 10, 10, "SYMBOL TABLE", 10, 10, 0
OpasmListingSymbolHeader
	.byte "NAME             VALUE     VIS  KIND", 10
	.byte "---------------  --------  ---  ----", 10, 0
OpasmListingSymbolNone
	.byte "(none)", 10, 0
OpasmListingSymbolSuffix
	.byte "      prv  lbl ", 10, 0
OpasmListingMemoryPrefix
	.byte 10, "Total memory is ", 0
OpasmListingMemorySuffix
	.byte " bytes", 10, 0
OpasmListingGeneratedHeader
	.byte 10, "GENERATED OUTPUT", 10, 10
	.byte "ADDR    BYTES", 10
	.byte "------  -----------------------", 10, 0
OpasmListingNoneLine
	.byte "(none)", 10, 0

	.endsection

	.section bss, kind=bss
	.align 4

OpasmPrgArtifactBuffer
	.res byte, OPASM_OUTPUT_PRG_BUFFER_CAPACITY
OpasmHexArtifactBuffer
	.res byte, OPASM_OUTPUT_HEX_BUFFER_CAPACITY
OpasmListingArtifactBuffer
	.res byte, OPASM_OUTPUT_LISTING_BUFFER_CAPACITY

	.endsection
	.endmodule

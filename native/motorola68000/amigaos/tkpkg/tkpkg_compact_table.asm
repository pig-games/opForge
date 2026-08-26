; Compact package-table lookup for package-owned fixed programs.
; @opforge-owner: tkpkg.amigaos.compact_table
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68k-fixed-opcode-package-v1.toml

	.module tkpkg.amigaos.compact_table
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.selection_service as selection

COMPACT_TABLE_VERSION_V1 = 1
COMPACT_INDEX_NONE = $FFFF
COMPACT_TABLE_MALFORMED_TEXT_LEN = 31
ZERO_SHAPE_MODE_KEY_LEN = 7
CTBL_LOCAL_MODE_PTR = 0
CTBL_LOCAL_MODE_LEN = 4
CTBL_LOCAL_FAMILY_OWNER = 6
CTBL_LOCAL_CPU_OWNER = 8
CTBL_LOCAL_DIALECT_OWNER = 10
CTBL_LOCAL_MNEMONIC_INDEX = 12
CTBL_LOCAL_MODE_INDEX = 14
CTBL_LOCAL_PROGRAM_INDEX = 16
CTBL_LOCAL_PREVIOUS_STRING_LEN = 18
CTBL_LOCAL_PROGRAM_TABLE_PTR = 20
CTBL_LOCAL_BYTES = 24

	.section data, kind=data
	.priv

CompactTableMalformedText
	.byte "OTR901: compact table malformed", 0

ZeroShapeModeKey
	.byte $69, $6D, $70, $6C, $69, $65, $64

	.endsection

	.section code, kind=code
	.pub

; Resolve a request and optional selected mode to one package-owned compact
; table program.
;
; Inputs:
; - A0: selected-instruction request control block.
; - A1/D0.W: selected mode bytes/length; zero length selects Rust's canonical
;   fixed-program mode key for a zero-operand request.
;
; Outputs:
; - D0: 0 on success/no match, 1 when CTBL is malformed or unsupported.
; - D1: program length on success, zero when no fixed program matches.
; - A1: program bytes on success, diagnostic text on failure.
;
; Clobbers:
; - D0-D7/A1-A6/CCR.
;
; CCR:
; - Reflects D0 on return.
findFixedProgramFromRequestV1	.block
	movem.l d2-d7/a2-a6, -(sp)
	lea -CTBL_LOCAL_BYTES(sp), sp
	movea.l sp, a4
	move.l a1, CTBL_LOCAL_MODE_PTR(a4)
	move.w d0, CTBL_LOCAL_MODE_LEN(a4)
	moveq #0, d0
	move.b abi.CB_INPUT_PTR(a0), d0
	moveq #0, d1
	move.b 17(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	lea 0(a0, d0.w), a5
	moveq #0, d7
	move.b abi.CB_INPUT_LEN(a0), d7
	moveq #0, d0
	move.b 19(a0), d0
	lsl.w #8, d0
	or.w d0, d7
	cmpi.w #9, d7
	bcs.w noMatch
	adda.w #4, a5
	subq.w #4, d7
	moveq #0, d2
	move.b (a5)+, d2
	moveq #0, d3
	move.b (a5)+, d3
	lsl.w #8, d3
	or.w d3, d2
	moveq #0, d3
	move.b (a5)+, d3
	moveq #0, d0
	move.b (a5)+, d0
	lsl.w #8, d0
	or.w d0, d3
	subq.w #4, d7
	or.w d3, d2
	bne.s requestHasShape
	lea ZeroShapeModeKey, a1
	move.l a1, CTBL_LOCAL_MODE_PTR(a4)
	move.w #ZERO_SHAPE_MODE_KEY_LEN, CTBL_LOCAL_MODE_LEN(a4)
	bra.s requestShapeReady

requestHasShape
	tst.w CTBL_LOCAL_MODE_LEN(a4)
	beq.w noMatch

requestShapeReady
	moveq #0, d4
	move.b (a5)+, d4
	subq.w #1, d7
	beq.w noMatch
	cmp.w d7, d4
	bhi.w noMatch
	lea buffers.CtblChunkOffsetLo, a3
	jsr selection.tkpkgServiceChunkPtrFromLocatorV1
	bne.w noMatch
	jsr selection.tkpkgServiceReadU16LeV1
	bne.w malformed
	cmpi.w #COMPACT_TABLE_VERSION_V1, d0
	bne.w malformed
	jsr selection.tkpkgServiceReadU16LeV1
	bne.w malformed
	tst.w d0
	beq.w malformed
	move.w d0, d7
	move.w #COMPACT_INDEX_NONE, CTBL_LOCAL_FAMILY_OWNER(a4)
	move.w #COMPACT_INDEX_NONE, CTBL_LOCAL_CPU_OWNER(a4)
	move.w #COMPACT_INDEX_NONE, CTBL_LOCAL_DIALECT_OWNER(a4)
	moveq #0, d5

ownerLoop
	moveq #1, d0
	jsr selection.tkpkgServiceRequireBytesV1
	bne.w malformed
	moveq #0, d6
	move.b (a2)+, d6
	bsr.w locateCompactStringV1
	bne.w malformed
	move.l a2, -(sp)
	jsr selection.tkpkgSelectedMselOwnerMatchesV1
	movea.l (sp)+, a2
	tst.b d0
	beq.s nextOwner
	tst.b d6
	beq.s selectFamilyOwner
	cmpi.b #1, d6
	beq.s selectCpuOwner
	cmpi.b #2, d6
	bne.s nextOwner
	move.w d5, CTBL_LOCAL_DIALECT_OWNER(a4)
	bra.s nextOwner

selectCpuOwner
	move.w d5, CTBL_LOCAL_CPU_OWNER(a4)
	bra.s nextOwner

selectFamilyOwner
	move.w d5, CTBL_LOCAL_FAMILY_OWNER(a4)

nextOwner
	addq.w #1, d5
	subq.w #1, d7
	bne.w ownerLoop

	jsr selection.tkpkgServiceReadU16LeV1
	bne.w malformed
	tst.w d0
	beq.w malformed
	move.w d0, d7
	clr.w CTBL_LOCAL_PREVIOUS_STRING_LEN(a4)
	move.w #COMPACT_INDEX_NONE, CTBL_LOCAL_MNEMONIC_INDEX(a4)
	move.w #COMPACT_INDEX_NONE, CTBL_LOCAL_MODE_INDEX(a4)
	moveq #0, d5

stringLoop
	jsr selection.tkpkgServiceReadU16LeV1
	bne.w malformed
	move.w d0, d6
	cmp.w CTBL_LOCAL_PREVIOUS_STRING_LEN(a4), d6
	bhi.w malformed
	bsr.w locateCompactStringV1
	bne.w malformed
	move.w d0, d2
	add.w d6, d0
	bcs.w malformed
	cmpi.w #buffers.COMPACT_STRING_SCRATCH_CAPACITY, d0
	bhi.w malformed
	move.w d0, CTBL_LOCAL_PREVIOUS_STRING_LEN(a4)
	lea buffers.CompactStringScratchBuffer, a3
	adda.w d6, a3
	move.w d2, d0
	beq.s compareString

copySuffix
	move.b (a1)+, (a3)+
	subq.w #1, d0
	bne.s copySuffix

compareString
	move.l a2, -(sp)
	move.w d4, -(sp)
	lea buffers.CompactStringScratchBuffer, a1
	movea.l a5, a2
	move.w CTBL_LOCAL_PREVIOUS_STRING_LEN(a4), d0
	move.w d4, d1
	jsr selection.tkpkgServiceStringEqAsciiCasefoldV1
	move.w (sp)+, d4
	movea.l (sp)+, a2
	tst.b d0
	beq.s compareModeString
	move.w d5, CTBL_LOCAL_MNEMONIC_INDEX(a4)

compareModeString
	tst.w CTBL_LOCAL_MODE_LEN(a4)
	beq.s nextString
	move.l a2, -(sp)
	move.w d4, -(sp)
	lea buffers.CompactStringScratchBuffer, a1
	movea.l CTBL_LOCAL_MODE_PTR(a4), a2
	move.w CTBL_LOCAL_PREVIOUS_STRING_LEN(a4), d0
	move.w CTBL_LOCAL_MODE_LEN(a4), d1
	jsr selection.tkpkgServiceStringEqAsciiCasefoldV1
	move.w (sp)+, d4
	movea.l (sp)+, a2
	tst.b d0
	beq.s nextString
	move.w d5, CTBL_LOCAL_MODE_INDEX(a4)

nextString
	addq.w #1, d5
	subq.w #1, d7
	bne.w stringLoop
	move.w CTBL_LOCAL_MNEMONIC_INDEX(a4), d0
	cmpi.w #COMPACT_INDEX_NONE, d0
	beq.w noMatch
	tst.w CTBL_LOCAL_MODE_LEN(a4)
	beq.s haveSelectedStrings
	move.w CTBL_LOCAL_MODE_INDEX(a4), d0
	cmpi.w #COMPACT_INDEX_NONE, d0
	beq.w noMatch

haveSelectedStrings

	jsr selection.tkpkgServiceReadU16LeV1
	bne.w malformed
	move.w d0, d7
	move.l a2, CTBL_LOCAL_PROGRAM_TABLE_PTR(a4)
	tst.w d7
	beq.w malformed

skipProgramLoop
	bsr.w readCompactLengthV1
	bne.w malformed
	adda.w #4, a2
	move.w d0, d6
	jsr selection.tkpkgServiceRequireBytesV1
	bne.w malformed
	adda.w d6, a2
	subq.w #1, d7
	bne.w skipProgramLoop

	bsr.w readCompactLengthV1
	bne.w malformed
	adda.w #4, a2
	moveq #0, d4
	move.w d0, d4
	move.w #COMPACT_INDEX_NONE, CTBL_LOCAL_PROGRAM_INDEX(a4)
	tst.w d4
	beq.w noMatch
	moveq #0, d0
	move.w d4, d0
	lsl.l #3, d0
	jsr selection.tkpkgServiceRequireBytesV1
	bne.w malformed
	movea.l a2, a5

	move.w CTBL_LOCAL_FAMILY_OWNER(a4), d6
	bsr.w findTableProgramForOwnerV1
	tst.l d0
	bmi.w noMatch
	beq.s checkCpuProgram
	move.w d1, CTBL_LOCAL_PROGRAM_INDEX(a4)

checkCpuProgram
	move.w CTBL_LOCAL_CPU_OWNER(a4), d6
	bsr.w findTableProgramForOwnerV1
	tst.l d0
	bmi.w noMatch
	beq.s checkDialectProgram
	move.w d1, CTBL_LOCAL_PROGRAM_INDEX(a4)

checkDialectProgram
	move.w CTBL_LOCAL_DIALECT_OWNER(a4), d6
	bsr.w findTableProgramForOwnerV1
	tst.l d0
	bmi.w noMatch
	beq.s tableSelectionReady
	move.w d1, CTBL_LOCAL_PROGRAM_INDEX(a4)

tableSelectionReady
	move.w CTBL_LOCAL_PROGRAM_INDEX(a4), d0
	cmpi.w #COMPACT_INDEX_NONE, d0
	beq.w noMatch

	movea.l CTBL_LOCAL_PROGRAM_TABLE_PTR(a4), a2
	moveq #0, d7
	move.w CTBL_LOCAL_PROGRAM_INDEX(a4), d7

programLoop
	bsr.w readCompactLengthV1
	bne.w malformed
	adda.w #4, a2
	tst.w d7
	beq.s found
	move.w d0, d6
	jsr selection.tkpkgServiceRequireBytesV1
	bne.w malformed
	adda.w d6, a2
	subq.w #1, d7
	bra.w programLoop

found
	move.w d0, d1
	movea.l a2, a1
	moveq #0, d0
	bra.s return

noMatch
	moveq #0, d0
	moveq #0, d1
	bra.s return

malformed
	lea CompactTableMalformedText, a1
	moveq #COMPACT_TABLE_MALFORMED_TEXT_LEN, d1
	moveq #1, d0

return
	lea CTBL_LOCAL_BYTES(sp), sp
	movem.l (sp)+, d2-d7/a2-a6
	tst.l d0
	rts
	.bend  ; findFixedProgramFromRequestV1

	.priv

; Find one `(owner-index, mnemonic-index)` row in the sorted compact entry
; table. The Rust package serializer emits this fixed-width table ordered by
; owner and mnemonic, so binary lookup keeps combined-family packages bounded
; without introducing family-specific runtime logic.
; Inputs: A5 = entry table; D4.W = entry count; D6.W = owner index.
; Outputs: D0 = 1 found, 0 absent, -1 duplicate key; D1.W = program index.
findTableProgramForOwnerV1	.block
	movem.l d2-d7/a0-a2, -(sp)
	cmpi.w #COMPACT_INDEX_NONE, d6
	beq.w tableKeyAbsent
	moveq #0, d2
	moveq #0, d3
	move.w d4, d3

tableBinaryLoop
	cmp.l d3, d2
	bcc.w tableKeyAbsent
	move.l d2, d5
	add.l d3, d5
	lsr.l #1, d5
	move.l d5, d7
	lsl.l #3, d7
	lea 0(a5, d7.l), a1
	moveq #0, d7
	move.b (a1), d7
	moveq #0, d0
	move.b 1(a1), d0
	lsl.w #8, d0
	or.w d0, d7
	cmp.w d6, d7
	blo.s tableKeyIsLower
	bhi.s tableKeyIsHigher
	moveq #0, d7
	move.b 2(a1), d7
	moveq #0, d0
	move.b 3(a1), d0
	lsl.w #8, d0
	or.w d0, d7
	move.w CTBL_LOCAL_MNEMONIC_INDEX(a4), d0
	cmp.w d0, d7
	blo.s tableKeyIsLower
	bhi.s tableKeyIsHigher
	tst.w CTBL_LOCAL_MODE_LEN(a4)
	beq.s tableKeyMatched
	moveq #0, d7
	move.b 4(a1), d7
	moveq #0, d0
	move.b 5(a1), d0
	lsl.w #8, d0
	or.w d0, d7
	move.w CTBL_LOCAL_MODE_INDEX(a4), d0
	cmp.w d0, d7
	blo.s tableKeyIsLower
	bhi.s tableKeyIsHigher

tableKeyMatched

	; A duplicate key is ambiguous for the zero-operand fixed-program slice.
	tst.l d5
	beq.s checkNextTableKey
	lea -8(a1), a0
	bsr.s tableKeyAtPtrMatchesV1
	bne.s tableKeyDuplicate

checkNextTableKey
	move.l d5, d7
	addq.l #1, d7
	cmp.l d4, d7
	bcc.s tableKeyUnique
	lea 8(a1), a0
	bsr.s tableKeyAtPtrMatchesV1
	bne.s tableKeyDuplicate

tableKeyUnique
	moveq #0, d1
	move.b 6(a1), d1
	moveq #0, d0
	move.b 7(a1), d0
	lsl.w #8, d0
	or.w d0, d1
	moveq #1, d0
	bra.s tableLookupReturn

tableKeyIsLower
	move.l d5, d2
	addq.l #1, d2
	bra.w tableBinaryLoop

tableKeyIsHigher
	move.l d5, d3
	bra.w tableBinaryLoop

tableKeyDuplicate
	moveq #-1, d0
	moveq #0, d1
	bra.s tableLookupReturn

tableKeyAbsent
	moveq #0, d0
	moveq #0, d1

tableLookupReturn
	movem.l (sp)+, d2-d7/a0-a2
	rts
	.bend  ; findTableProgramForOwnerV1

; Compare the compact row at A0 with the active owner/mnemonic key.
; Outputs: D0 = 1 equal, 0 different. Clobbers D0-D1/CCR.
tableKeyAtPtrMatchesV1	.block
	moveq #0, d0
	move.b (a0), d0
	moveq #0, d1
	move.b 1(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	cmp.w d6, d0
	bne.s tablePtrKeyNo
	moveq #0, d0
	move.b 2(a0), d0
	moveq #0, d1
	move.b 3(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	cmp.w CTBL_LOCAL_MNEMONIC_INDEX(a4), d0
	bne.s tablePtrKeyNo
	tst.w CTBL_LOCAL_MODE_LEN(a4)
	beq.s tablePtrKeyYes
	moveq #0, d0
	move.b 4(a0), d0
	moveq #0, d1
	move.b 5(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	cmp.w CTBL_LOCAL_MODE_INDEX(a4), d0
	bne.s tablePtrKeyNo

tablePtrKeyYes
	moveq #1, d0
	rts

tablePtrKeyNo
	moveq #0, d0
	rts
	.bend  ; tableKeyAtPtrMatchesV1

; Read a bounded u32 length whose native fixed-program path requires high zero.
; Inputs: A2/A6 = field cursor/chunk end.
; Outputs: D0.W = length and D1 = 0, or D1 = 1 on bounds/range failure.
readCompactLengthV1	.block
	moveq #4, d0
	jsr selection.tkpkgServiceRequireBytesV1
	bne.s compactLengthFail
	tst.b 2(a2)
	bne.s compactLengthFail
	tst.b 3(a2)
	bne.s compactLengthFail
	moveq #0, d0
	move.b (a2), d0
	moveq #0, d1
	move.b 1(a2), d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
	rts

compactLengthFail
	moveq #1, d1
	rts
	.bend  ; readCompactLengthV1

; Locate one bounded u32-length string and advance the compact cursor.
; Inputs: A2/A6 = string record cursor/chunk end.
; Outputs: D0.W = byte length, A1 = bytes, D1 = 0; D1 = 1 on failure.
locateCompactStringV1	.block
	bsr.s readCompactLengthV1
	bne.s compactStringFail
	move.w d0, d2
	addq.l #4, d0
	jsr selection.tkpkgServiceRequireBytesV1
	bne.s compactStringFail
	lea 4(a2), a1
	lea 4(a2), a2
	adda.w d2, a2
	move.w d2, d0
	moveq #0, d1
	rts

compactStringFail
	moveq #1, d1
	rts
	.bend  ; locateCompactStringV1

	.endsection
	.endmodule

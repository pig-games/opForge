; Lookup against immutable prepared package tables and the active owner binding.
; @opforge-owner: tkpkg.amigaos.compact_table
; @opforge-slice: documentation/plans/slices/native-porting-slice-m68k-fixed-opcode-package-v1.toml

	.module tkpkg.amigaos.compact_table
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.selection_service as selection
	.use tkpkg.amigaos.compact_prepare as prepared
	.include "telemetry_macros.i"
	.priv

NONE = $FFFF
MALFORMED_LENGTH = 31
ZERO_MODE_LENGTH = 7

Lookup	.struct
Mode	.long ?
ModeLength	.word ?
Mnemonic	.word ?
ModeIndex	.word ?
Program	.word ?
PreviousLength	.word ?
Padding	.word ?
.endstruct

FRAME_BYTES = Lookup.Padding + 2

	.section data, kind=data
	.priv

MalformedText
	.byte "OTR901: compact table malformed", 0

ZeroModeText
	.byte $69, $6D, $70, $6C, $69, $65, $64

	.endsection

	.section bss, kind=bss
	.priv
; Replaced only after a successful pipeline commit; lookups also require its flag.
ActiveOwners
	.res word, 3
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
; - D0-D1/A1/CCR; other registers preserved.
;
; CCR:
; - Reflects D0 on return.
find	.block
	movem.l d2-d7/a2-a6, -(sp)
	lea -FRAME_BYTES(sp), sp
	movea.l sp, a4
	move.l a1, Lookup.Mode(a4)
	move.w d0, Lookup.ModeLength(a4)
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
	lea ZeroModeText, a1
	move.l a1, Lookup.Mode(a4)
	move.w #ZERO_MODE_LENGTH, Lookup.ModeLength(a4)
	bra.s requestShapeReady

requestHasShape
	tst.w Lookup.ModeLength(a4)
	beq.w noMatch

requestShapeReady
	moveq #0, d4
	move.b (a5)+, d4
	subq.w #1, d7
	beq.w noMatch
	cmp.w d7, d4
	bhi.w noMatch
	.TELEMETRY_COMPACT runtime_profile.compactLookup, #1
	tst.b prepared.valid
	beq.w noMatch
	btst #1, buffers.PackageStateFlags
	beq.w noMatch
	movea.l prepared.strings, a2
	movea.l prepared.end, a6
	move.w prepared.stringCount, d7
	tst.w d7
	beq.w noMatch
	clr.w Lookup.PreviousLength(a4)
	move.w #NONE, Lookup.Mnemonic(a4)
	move.w #NONE, Lookup.ModeIndex(a4)
	moveq #0, d5

stringLoop
	jsr selection.tkpkgServiceReadU16LeV1
	bne.w malformed
	move.w d0, d6
	cmp.w Lookup.PreviousLength(a4), d6
	bhi.w malformed
	bsr.w readString
	bne.w malformed
	move.w d0, d2
	add.w d6, d0
	bcs.w malformed
	cmpi.w #buffers.COMPACT_STRING_SCRATCH_CAPACITY, d0
	bhi.w malformed
	move.w d0, Lookup.PreviousLength(a4)
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
	move.w Lookup.PreviousLength(a4), d0
	move.w d4, d1
	jsr selection.tkpkgServiceStringEqAsciiCasefoldV1
	move.w (sp)+, d4
	movea.l (sp)+, a2
	tst.b d0
	beq.s compareModeString
	move.w d5, Lookup.Mnemonic(a4)

compareModeString
	tst.w Lookup.ModeLength(a4)
	beq.s nextString
	move.l a2, -(sp)
	move.w d4, -(sp)
	lea buffers.CompactStringScratchBuffer, a1
	movea.l Lookup.Mode(a4), a2
	move.w Lookup.PreviousLength(a4), d0
	move.w Lookup.ModeLength(a4), d1
	jsr selection.tkpkgServiceStringEqAsciiCasefoldV1
	move.w (sp)+, d4
	movea.l (sp)+, a2
	tst.b d0
	beq.s nextString
	move.w d5, Lookup.ModeIndex(a4)

nextString
	addq.w #1, d5
	subq.w #1, d7
	bne.w stringLoop
	.TELEMETRY_COMPACT runtime_profile.compactStrings, d5
	move.w Lookup.Mnemonic(a4), d0
	cmpi.w #NONE, d0
	beq.w noMatch
	moveq #0, d4
	move.l prepared.rowCount, d4
	move.w #NONE, Lookup.Program(a4)
	tst.w d4
	beq.w noMatch
	movea.l prepared.rows, a5

	move.w ActiveOwners, d6
	bsr.w findOwner
	tst.l d0
	beq.s checkCpuProgram
	move.w d1, Lookup.Program(a4)

checkCpuProgram
	move.w ActiveOwners+2, d6
	bsr.w findOwner
	tst.l d0
	beq.s checkDialectProgram
	move.w d1, Lookup.Program(a4)

checkDialectProgram
	move.w ActiveOwners+4, d6
	bsr.w findOwner
	tst.l d0
	beq.s tableSelectionReady
	move.w d1, Lookup.Program(a4)

tableSelectionReady
	move.w Lookup.Program(a4), d0
	cmpi.w #NONE, d0
	beq.w noMatch

	cmp.w prepared.programCount, d0
	bcc.w malformed
	andi.l #$ffff, d0
	lsl.l #2, d0
	movea.l prepared.programs, a1
	movea.l 0(a1, d0.l), a2
	bsr.w readLength
	bne.w malformed
	move.w d0, d1
	lea 4(a2), a1
	moveq #0, d0
	bra.s return

noMatch
	moveq #0, d0
	moveq #0, d1
	bra.s return

malformed
	lea MalformedText, a1
	moveq #MALFORMED_LENGTH, d1
	moveq #1, d0

return
	lea FRAME_BYTES(sp), sp
	movem.l (sp)+, d2-d7/a2-a6
	tst.l d0
	rts
	.bend  ; find

	.pub
; Bind validated CTBL owners after pipeline commit. No allocation or failure path.
; Inputs: active pipeline in buffers. Outputs: D0=0; CCR reflects D0.
; Other registers preserved. Reloaded packages cannot use this binding until selected.
bind	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.w #NONE, ActiveOwners
	move.w #NONE, ActiveOwners+2
	move.w #NONE, ActiveOwners+4
	tst.b prepared.valid
	beq.w done
	movea.l prepared.owners, a2
	movea.l prepared.end, a6
	move.w prepared.ownerCount, d7
	beq.w done
	moveq #0, d5
loop
	moveq #0, d6
	move.b (a2)+, d6
	bsr.w readString
	jsr selection.tkpkgSelectedMselOwnerMatchesV1
	tst.b d0
	beq.s next
	move.w d6, d0
	add.w d0, d0
	lea ActiveOwners, a3
	move.w d5, 0(a3, d0.w)
next
	addq.w #1, d5
	subq.w #1, d7
	bne.s loop
done
	moveq #0, d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; bind

	.priv

; Find one `(owner, mnemonic, mode)` row in the validated compact entry
; table. The Rust package serializer emits this fixed-width table ordered by
; owner and mnemonic, so binary lookup keeps combined-family packages bounded
; without introducing family-specific runtime logic.
; Inputs: A5 = entry table; D4.W = entry count; D6.W = owner index.
; Outputs: D0 = 1 found, 0 absent; D1.W = program index.
findOwner	.block
	movem.l d2-d7/a0-a2, -(sp)
	cmpi.w #NONE, d6
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
	.TELEMETRY_COMPACT runtime_profile.compactTableRows, #1
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
	move.w Lookup.Mnemonic(a4), d0
	cmp.w d0, d7
	blo.s tableKeyIsLower
	bhi.s tableKeyIsHigher
	moveq #0, d7
	move.b 4(a1), d7
	moveq #0, d0
	move.b 5(a1), d0
	lsl.w #8, d0
	or.w d0, d7
	move.w Lookup.ModeIndex(a4), d0
	cmp.w d0, d7
	blo.s tableKeyIsLower
	bhi.s tableKeyIsHigher

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

tableKeyAbsent
	moveq #0, d0
	moveq #0, d1

tableLookupReturn
	movem.l (sp)+, d2-d7/a0-a2
	rts
	.bend  ; findOwner

; Read a bounded u32 length whose native fixed-program path requires high zero.
; Inputs: A2/A6 = field cursor/chunk end.
; Outputs: D0.W = length and D1 = 0, or D1 = 1 on bounds/range failure.
readLength	.block
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
	.bend  ; readLength

; Locate one bounded u32-length string and advance the compact cursor.
; Inputs: A2/A6 = string record cursor/chunk end.
; Outputs: D0.W = byte length, A1 = bytes, D1 = 0; D1 = 1 on failure.
readString	.block
	bsr.s readLength
	bne.s compactStringFail
	moveq #0, d2
	move.w d0, d2
	addq.l #4, d0
	jsr selection.tkpkgServiceRequireBytesV1
	bne.s compactStringFail
	lea 4(a2), a1
	lea 4(a2), a2
	adda.l d2, a2
	move.w d2, d0
	moveq #0, d1
	rts

compactStringFail
	moveq #1, d1
	rts
	.bend  ; readString

	.endsection
	.endmodule

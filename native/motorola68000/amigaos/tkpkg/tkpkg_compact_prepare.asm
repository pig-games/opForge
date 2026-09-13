; Validate CTBL once and publish its package-owned sections plus an O(1)
; program directory. Strings remain front-compressed in package storage.
;
; This native preparation boundary validates the v1 structure, owner tags,
; front-compression lengths, native u16 program lengths, row indices, strict
; row-key ordering, and exact chunk consumption. Unlike the Rust decoder it
; does not validate UTF-8 or reject duplicate owner/string/program payloads.
; @opforge-owner: tkpkg.amigaos.compact_prepare

	.module tkpkg.amigaos.compact_prepare
	.cpu 68020
	.pub
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.selection_service as selection
	.include "telemetry_macros.i"
	.priv

VERSION_V1 = 1
MEM_PUBLIC = 1

Pending	.struct
Owners	.long ?
OwnerCount	.word ?
Padding0	.word ?
Strings	.long ?
StringCount	.word ?
Padding1	.word ?
Programs	.long ?
ProgramCount	.word ?
Padding2	.word ?
Rows	.long ?
RowCount	.long ?
End	.long ?
Bytes	.long ?
PreviousStringLength	.long ?
PreviousOwner	.word ?
PreviousMnemonic	.word ?
PreviousMode	.word ?
Padding3	.word ?
.endstruct

FRAME_BYTES = Pending.Padding3 + 2

	.section data, kind=data
	.priv

MalformedText
	.byte "OTR901: compact table malformed"
MalformedTextEnd
	.byte 0

AllocFailedText
	.byte "OTR902: compact table allocation failed"
AllocFailedTextEnd
	.byte 0

; Diagnostic lengths exclude their trailing NUL, matching A1/D1 callers.
MALFORMED_TEXT_LENGTH = MalformedTextEnd - MalformedText
ALLOC_FAILED_TEXT_LENGTH = AllocFailedTextEnd - AllocFailedText

	.endsection

	.section bss, kind=bss
	.pub

Owners
	.res long, 1
OwnerCount
	.res word, 1
	.align 4
Strings
	.res long, 1
StringCount
	.res word, 1
	.align 4
Programs
	.res long, 1
ProgramCount
	.res word, 1
	.align 4
Rows
	.res long, 1
RowCount
	.res long, 1
End
	.res long, 1
Valid
	.res byte, 1
	.align 4
Bytes
	.res long, 1

	.endsection

	.section code, kind=code
	.pub

; Release the prepared program directory and invalidate all published CTBL
; state. Safe before the first prepare and after package replacement.
; Inputs: none.
; Outputs: D0 = 0.
; Clobbers: D0-D1/A1/CCR. Preserves D2-D7/A0/A2-A6.
; CCR: reflects D0 on return.
reset	.block
	movem.l a0/a6, -(sp)
	clr.b Valid
	movea.l Programs, a1
	move.l Bytes, d0
	beq.s clear
	movea.l 4.w, a6
	jsr -210(a6)
clear
	clr.l Owners
	clr.w OwnerCount
	clr.l Strings
	clr.w StringCount
	clr.l Programs
	clr.w ProgramCount
	clr.l Rows
	clr.l RowCount
	clr.l End
	clr.l Bytes
	movem.l (sp)+, a0/a6
	moveq #0, d0
	rts
	.bend  ; reset

; Validate the current package's compact table and publish prepared views.
; An absent CTBL is a successful invalid state.
; Inputs: current package storage and CTBL locator in buffers.
; Outputs: D0 = 0 success, 1 failure; A1/D1 = diagnostic on failure.
; Memory: allocates exactly programCount*4 public bytes for `programs`.
; Clobbers: D0-D1/A1/CCR. Preserves D2-D7/A0/A2-A6 including ExecBase.
; CCR: reflects D0 on return.
prepare	.block
	movem.l d2-d7/a0/a2-a6, -(sp)
	lea -FRAME_BYTES(sp), sp
	movea.l sp, a4
	clr.l Pending.Programs(a4)
	clr.l Pending.Bytes(a4)
	jsr reset
	lea buffers.CtblChunkOffsetLo, a3
	jsr selection.tkpkgServiceChunkPtrFromLocatorV1
	bne.w absent
	move.l a6, Pending.End(a4)

	bsr.w readU16
	bne.w malformed
	cmpi.w #VERSION_V1, d0
	bne.w malformed
	bsr.w readU16
	bne.w malformed
	move.w d0, Pending.OwnerCount(a4)
	move.l a2, Pending.Owners(a4)
	move.w d0, d7
	beq.s ownersDone
ownerLoop
	moveq #1, d0
	bsr.w require
	bne.w malformed
	cmpi.b #2, (a2)
	bhi.w malformed
	addq.l #1, a2
	bsr.w skipString
	bne.w malformed
	subq.w #1, d7
	bne.s ownerLoop
ownersDone

	bsr.w readU16
	bne.w malformed
	move.w d0, Pending.StringCount(a4)
	move.l a2, Pending.Strings(a4)
	clr.l Pending.PreviousStringLength(a4)
	move.w d0, d7
	beq.s stringsDone
stringLoop
	bsr.w readU16
	bne.w malformed
	moveq #0, d4
	move.w d0, d4
	cmp.l Pending.PreviousStringLength(a4), d4
	bhi.w malformed
	bsr.w readLength
	bne.w malformed
	addq.l #4, a2
	moveq #0, d6
	move.w d0, d6
	bsr.w require
	bne.w malformed
	adda.l d6, a2
	add.l d4, d6
	cmpi.l #buffers.COMPACT_STRING_SCRATCH_CAPACITY, d6
	bhi.w malformed
	move.l d6, Pending.PreviousStringLength(a4)
	subq.w #1, d7
	bne.s stringLoop
stringsDone

	bsr.w readU16
	bne.w malformed
	moveq #0, d5
	move.w d0, d5
	move.w d5, Pending.ProgramCount(a4)
	lsl.l #2, d5
	move.l d5, Pending.Bytes(a4)
	beq.s programsReady
	move.l d5, d0
	moveq #MEM_PUBLIC, d1
	movea.l 4.w, a6
	jsr -198(a6)
	tst.l d0
	beq.w allocFailed
	move.l d0, Pending.Programs(a4)
programsReady
	movea.l Pending.End(a4), a6
	movea.l Pending.Programs(a4), a5
	move.w Pending.ProgramCount(a4), d7
	beq.s programsDone
programLoop
	move.l a2, (a5)+
	bsr.w readLength
	bne.w malformed
	addq.l #4, a2
	moveq #0, d6
	move.w d0, d6
	bsr.w require
	bne.w malformed
	adda.l d6, a2
	subq.w #1, d7
	bne.s programLoop
programsDone

	bsr.w readU32
	bne.w malformed
	cmpi.l #$FFFF, d0
	bhi.w malformed
	move.l d0, Pending.RowCount(a4)
	move.l a2, Pending.Rows(a4)
	move.l a6, d1
	sub.l a2, d1
	move.l d1, d6
	andi.l #7, d6
	bne.w malformed
	lsr.l #3, d1
	cmp.l d1, d0
	bne.w malformed
	tst.l d0
	beq.w rowsDone
	move.l d0, d7
	moveq #0, d5
rowLoop
	moveq #0, d0
	move.b (a2), d0
	moveq #0, d1
	move.b 1(a2), d1
	lsl.w #8, d1
	or.w d1, d0
	cmp.w Pending.OwnerCount(a4), d0
	bhs.w malformed
	moveq #0, d1
	move.b 2(a2), d1
	moveq #0, d2
	move.b 3(a2), d2
	lsl.w #8, d2
	or.w d2, d1
	cmp.w Pending.StringCount(a4), d1
	bhs.w malformed
	moveq #0, d2
	move.b 4(a2), d2
	moveq #0, d3
	move.b 5(a2), d3
	lsl.w #8, d3
	or.w d3, d2
	cmp.w Pending.StringCount(a4), d2
	bhs.w malformed
	moveq #0, d3
	move.b 6(a2), d3
	moveq #0, d4
	move.b 7(a2), d4
	lsl.w #8, d4
	or.w d4, d3
	cmp.w Pending.ProgramCount(a4), d3
	bhs.w malformed
	tst.l d5
	beq.s rowAccepted
	cmp.w Pending.PreviousOwner(a4), d0
	blo.w malformed
	bhi.s rowAccepted
	cmp.w Pending.PreviousMnemonic(a4), d1
	blo.w malformed
	bhi.s rowAccepted
	cmp.w Pending.PreviousMode(a4), d2
	bls.w malformed
rowAccepted
	move.w d0, Pending.PreviousOwner(a4)
	move.w d1, Pending.PreviousMnemonic(a4)
	move.w d2, Pending.PreviousMode(a4)
	addq.l #8, a2
	addq.l #1, d5
	subq.l #1, d7
	bne.w rowLoop
rowsDone
	cmpa.l a6, a2
	bne.w malformed

	move.l Pending.Owners(a4), Owners
	move.w Pending.OwnerCount(a4), OwnerCount
	move.l Pending.Strings(a4), Strings
	move.w Pending.StringCount(a4), StringCount
	move.l Pending.Programs(a4), Programs
	move.w Pending.ProgramCount(a4), ProgramCount
	move.l Pending.Rows(a4), Rows
	move.l Pending.RowCount(a4), RowCount
	move.l Pending.End(a4), End
	move.l Pending.Bytes(a4), Bytes
	move.b #1, Valid
	.TELEMETRY_COMPACT runtime_profile.compactPrepare, #1
	.TELEMETRY_COMPACT_WORD runtime_profile.compactProgramRows, Pending.ProgramCount(a4)
	; Metadata bytes counts only the allocated O(1) program directory.
	.TELEMETRY_COMPACT runtime_profile.compactMetadataBytes, Pending.Bytes(a4)
	moveq #0, d0
	bra.s return

absent
	moveq #0, d0
	bra.s return

allocFailed
	lea AllocFailedText, a1
	moveq #ALLOC_FAILED_TEXT_LENGTH, d1
	moveq #1, d0
	bra.s return

malformed
	movea.l Pending.Programs(a4), a1
	move.l Pending.Bytes(a4), d0
	beq.s malformedDiagnostic
	movea.l 4.w, a6
	jsr -210(a6)
malformedDiagnostic
	lea MalformedText, a1
	moveq #MALFORMED_TEXT_LENGTH, d1
	moveq #1, d0

return
	lea FRAME_BYTES(sp), sp
	movem.l (sp)+, d2-d7/a0/a2-a6
	tst.l d0
	rts
	.bend  ; prepare

	.priv

; Require D0.L bytes at A2 within A6. Outputs D1 = 0/1.
require	.block
	move.l a6, d1
	sub.l a2, d1
	bcs.s fail
	cmp.l d1, d0
	bhi.s fail
	moveq #0, d1
	rts
fail
	moveq #1, d1
	rts
	.bend  ; require

; Read bounded little-endian u16. Advances A2 on success.
readU16	.block
	moveq #2, d0
	bsr.s require
	bne.s fail
	moveq #0, d0
	move.b (a2)+, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
fail
	rts
	.bend  ; readU16

; Read bounded little-endian u32 and advance A2.
readU32	.block
	moveq #4, d0
	bsr.s require
	bne.s fail
	moveq #0, d0
	move.b 3(a2), d0
	lsl.l #8, d0
	move.b 2(a2), d0
	lsl.l #8, d0
	move.b 1(a2), d0
	lsl.l #8, d0
	move.b (a2), d0
	addq.l #4, a2
	moveq #0, d1
fail
	rts
	.bend  ; readU32

; Read a bounded u32 length accepted by the current native u16 consumers.
; Leaves A2 at the length field and returns D0.W.
readLength	.block
	moveq #4, d0
	bsr.s require
	bne.s fail
	tst.b 2(a2)
	bne.s fail
	tst.b 3(a2)
	bne.s fail
	moveq #0, d0
	move.b (a2), d0
	moveq #0, d1
	move.b 1(a2), d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
fail
	rts
	.bend  ; readLength

; Skip one bounded native-v1 u32-length byte string.
skipString	.block
	bsr.s readLength
	bne.s fail
	addq.l #4, a2
	moveq #0, d2
	move.w d0, d2
	move.l d2, d0
	bsr.s require
	bne.s fail
	adda.l d2, a2
	moveq #0, d1
fail
	rts
	.bend  ; skipString

	.endsection
	.endmodule

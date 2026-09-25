; Bounded Hunk serialization over caller-owned section bytes and fixups.
; @opforge-owner: experimental.amigaos.binary_hunk_output
	.module experimental.amigaos.binary_hunk_output
	.cpu 68020
	.pub
Frame	.struct
Segments	.long ?
Count	.long ?
Output	.long ?
Capacity	.long ?
Used	.long ?
	.endstruct
Part	.struct
Kind	.word ?  ; 1=code, 2=data, 3=bss
Reserved	.word ?
Data	.long ?
Used	.long ?
Size	.long ?
Fixups	.long ?
FixupCount	.long ?
	.endstruct
Reloc	.struct
Target	.long ?  ; zero-based index in Frame.Segments
Offset	.long ?  ; byte offset of an initialized absolute longword
	.endstruct
SEGMENT_BYTES = Part.FixupCount+4
FIXUP_BYTES = Reloc.Offset+4
MAX_SEGMENTS = 8
HUNK_HEADER = $3f3
HUNK_CODE = $3e9
HUNK_DATA = $3ea
HUNK_BSS = $3eb
HUNK_RELOC32 = $3ec
HUNK_END = $3f2
STATUS_OK = 0
STATUS_BAD = 1
	.section code, kind=code
	.pub

; A0=Frame. D0/CCR=0 on success, 1 on failure. All other registers are
; preserved. Output=0 measures the exact byte count into Frame.Used. A
; populated Output receives the complete Hunk only if Capacity is sufficient.
; Source fixups must arrive in ascending byte-offset order; interleaved target
; sections are allowed. This is the assembly pass's natural emission order.
build	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a4
	clr.l Frame.Used(a4)
	bsr.w measure
	bne.w failed
	move.l d1, Frame.Used(a4)
	move.l Frame.Output(a4), d0
	beq.w good
	cmp.l Frame.Capacity(a4), d1
	bhi.w failed
	bsr.w emit
	bra.w done
good
	moveq #STATUS_OK, d0
	bra.w done
failed
	clr.l Frame.Used(a4)
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; build
	.priv

; Validate the complete input and measure with checked 32-bit arithmetic.
; A4=Frame. D1=total byte count on success; D0/CCR=status.
measure	.block
	move.l Frame.Count(a4), d7
	beq.w bad
	cmpi.l #MAX_SEGMENTS, d7
	bhi.w bad
	movea.l Frame.Segments(a4), a5
	move.l a5, d0
	beq.w bad
	cmpi.w #1, Part.Kind(a5)
	bne.w bad
	move.l d7, d1
	lsl.l #2, d1
	addi.l #20, d1  ; header, names terminator, range and table
	moveq #0, d6
segment
	movea.l a5, a6
	moveq #0, d2
	move.w Part.Kind(a6), d2
	beq.w bad
	cmpi.w #3, d2
	bhi.w bad
	move.l Part.Size(a6), d3
	addi.l #3, d3
	bcs.w bad
	lsr.l #2, d3
	cmpi.l #$3fffffff, d3
	bhi.w bad
	move.l Part.Used(a6), d4
	cmpi.w #3, d2
	beq.w bss
	cmp.l Part.Size(a6), d4
	bhi.w bad
	tst.l d4
	beq.w bad  ; empty initialized sections are filtered by the caller
	tst.l Part.Data(a6)
	beq.w bad
	addi.l #3, d4
	bcs.w bad
	andi.l #$fffffffc, d4
	bra.w payload
bss
	tst.l d4
	bne.w bad
	tst.l Part.Data(a6)
	bne.w bad
	moveq #0, d4
payload
	add.l d4, d1
	bcs.w bad
	addi.l #12, d1  ; kind, payload size, end marker
	bcs.w bad
	move.l Part.FixupCount(a6), d5
	beq.w next
	cmpi.w #3, d2
	beq.w bad
	move.l Part.Used(a6), d0
	lsr.l #2, d0
	cmp.l d0, d5
	bhi.w bad  ; one absolute fixup per initialized longword at most
	movea.l Part.Fixups(a6), a0
	move.l a0, d0
	beq.w bad
	moveq #0, d2
	moveq #0, d3
fixup
	move.l Reloc.Target(a0), d4
	cmp.l d7, d4
	bhs.w bad
	move.l Reloc.Offset(a0), d4
	move.l Part.Used(a6), d0
	cmpi.l #4, d0
	blo.w bad
	subq.l #4, d0
	cmp.l d0, d4
	bhi.w bad
	tst.l d2
	beq.w firstOffset
	cmp.l d3, d4
	bls.w bad
firstOffset
	move.l d4, d3
	moveq #1, d2
	adda.l #FIXUP_BYTES, a0
	subq.l #1, d5
	bne.w fixup
	move.l Part.FixupCount(a6), d4
	lsl.l #2, d4
	bcs.w bad
	add.l d4, d1
	bcs.w bad
	addi.l #8, d1  ; HUNK_RELOC32 and zero group terminator
	bcs.w bad
	moveq #0, d2
group
	move.l d2, d3
	bsr.w countTarget
	tst.l d0
	beq.w emptyGroup
	addq.l #8, d1  ; count and target index
	bcs.w bad
emptyGroup
	addq.l #1, d2
	cmp.l d7, d2
	blo.w group
next
	adda.l #SEGMENT_BYTES, a5
	addq.l #1, d6
	cmp.l d7, d6
	blo.w segment
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_BAD, d0
	rts
	.bend  ; measure

; A6=source segment, D3=target index. Return D0=count, preserving D1-D7.
countTarget	.block
	move.l d4, -(sp)
	movea.l Part.Fixups(a6), a0
	move.l Part.FixupCount(a6), d0
	moveq #0, d4
loop
	tst.l d0
	beq.w done
	cmp.l Reloc.Target(a0), d3
	bne.w skip
	addq.l #1, d4
skip
	adda.l #FIXUP_BYTES, a0
	subq.l #1, d0
	bra.w loop
done
	move.l d4, d0
	move.l (sp)+, d4
	rts
	.bend  ; countTarget

; Emit only after measure has validated all input and capacity.
; A4=Frame, D7=count. D0/CCR=status.
emit	.block
	movea.l Frame.Output(a4), a3
	movea.l Frame.Segments(a4), a5
	move.l #HUNK_HEADER, (a3)+
	clr.l (a3)+
	move.l d7, (a3)+
	clr.l (a3)+
	move.l d7, d0
	subq.l #1, d0
	move.l d0, (a3)+
	moveq #0, d6
table
	move.l Part.Size(a5), d0
	addq.l #3, d0
	lsr.l #2, d0
	move.l d0, (a3)+
	adda.l #SEGMENT_BYTES, a5
	addq.l #1, d6
	cmp.l d7, d6
	blo.w table
	movea.l Frame.Segments(a4), a5
	moveq #0, d6
segment
	movea.l a5, a6
	moveq #0, d2
	move.w Part.Kind(a6), d2
	addi.l #HUNK_CODE-1, d2
	move.l d2, (a3)+
	cmpi.w #3, Part.Kind(a6)
	beq.w bss
	move.l Part.Used(a6), d5
	bra.w size
bss
	move.l Part.Size(a6), d5
size
	move.l d5, d0
	addq.l #3, d0
	lsr.l #2, d0
	move.l d0, (a3)+
	cmpi.w #3, Part.Kind(a6)
	beq.w relocations
	movea.l Part.Data(a6), a0
copy
	tst.l d5
	beq.w pad
	move.b (a0)+, (a3)+
	subq.l #1, d5
	bra.w copy
pad
	move.l Part.Used(a6), d0
	neg.l d0
	andi.l #3, d0
padByte
	tst.l d0
	beq.w relocations
	clr.b (a3)+
	subq.l #1, d0
	bra.w padByte
relocations
	tst.l Part.FixupCount(a6)
	beq.w endSegment
	move.l #HUNK_RELOC32, (a3)+
	moveq #0, d2
group
	move.l d2, d3
	bsr.w countTarget
	tst.l d0
	beq.w nextGroup
	move.l d0, (a3)+
	move.l d2, (a3)+
	movea.l Part.Fixups(a6), a0
	move.l Part.FixupCount(a6), d5
offset
	cmp.l Reloc.Target(a0), d2
	bne.w nextOffset
	move.l Reloc.Offset(a0), (a3)+
nextOffset
	adda.l #FIXUP_BYTES, a0
	subq.l #1, d5
	bne.w offset
nextGroup
	addq.l #1, d2
	cmp.l d7, d2
	blo.w group
	clr.l (a3)+
endSegment
	move.l #HUNK_END, (a3)+
	adda.l #SEGMENT_BYTES, a5
	addq.l #1, d6
	cmp.l d7, d6
	blo.w segment
	moveq #STATUS_OK, d0
	rts
	.bend  ; emit
	.endsection
	.endmodule

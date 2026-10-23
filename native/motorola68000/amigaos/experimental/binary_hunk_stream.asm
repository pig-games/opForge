; Bounded metadata with borrowed payloads; binary_hunk_output remains the oracle.
; @opforge-owner: experimental.amigaos.binary_hunk_stream
	.module experimental.amigaos.binary_hunk_stream
	.cpu 68020
	.use experimental.amigaos.binary_hunk_output as hunk
	.pub
Frame	.struct
Source	.long ?  ; hunk.Frame, Output=0; source records stay stable until END
Buffer	.long ?  ; caller-owned, even address; overwritten by the next call
Capacity	.long ?  ; at least MIN_CAPACITY; no allocation by this module
State	.long ?
Segment	.long ?
Index	.long ?
Target	.long ?
Cursor	.long ?
Remaining	.long ?
Emitted	.long ?
Expected	.long ?
	.endstruct
FRAME_BYTES = Frame.Expected+4
MIN_CAPACITY = 64
ITEM = 0
END = 1
ERROR = 2
	.priv
STATE_HEADER = 1
STATE_SEGMENT = 2
STATE_PAYLOAD = 3
STATE_PADDING = 4
STATE_RELOCATIONS = 5
STATE_GROUP = 6
STATE_OFFSETS = 7
STATE_SEGMENT_END = 8
STATE_DONE = 9
STATE_FAILED = 10
	.section code, kind=code
	.pub
; A0=Frame. D0/CCR=0 success, ERROR failure; other registers preserved.
; Validate and measure through the unchanged serializer, then reset iteration.
; Caller owns the frame/buffer and all source records and payloads until END.
; Buffer must not alias either frame, the parts, fixups or payloads. All input
; fields and borrowed bytes remain stable until END; frame state is private.
begin	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a4
	move.l #STATE_FAILED, Frame.State(a4)
	clr.l Frame.Emitted(a4)
	clr.l Frame.Expected(a4)
	move.l Frame.Buffer(a4), d0
	beq.w bad
	btst #0, d0
	bne.w bad
	move.l Frame.Capacity(a4), d1
	cmpi.l #MIN_CAPACITY, d1
	blo.w bad
	cmpi.l #$7ffffffc, d1
	bhi.w bad
	add.l d1, d0
	bcs.w bad
	move.l Frame.Source(a4), d0
	beq.w bad
	btst #0, d0
	bne.w bad
	movea.l d0, a0
	tst.l hunk.Frame.Output(a0)
	bne.w bad
	jsr hunk.build
	tst.l d0
	bne.w bad
	movea.l Frame.Source(a4), a0
	move.l hunk.Frame.Used(a0), Frame.Expected(a4)
	move.l hunk.Frame.Segments(a0), Frame.Segment(a4)
	clr.l Frame.Index(a4)
	move.l #STATE_HEADER, Frame.State(a4)
	moveq #0, d0
	bra.w done
bad
	moveq #ERROR, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; begin

; Generator ABI: A0=Frame; D0/CCR=ITEM, END or ERROR. ITEM returns A0=data,
; D1=nonzero bytes, valid until the next call. Other registers preserved.
; Metadata is even-addressed in Buffer, payload is returned directly, and
; 0..3 padding bytes are returned separately. No payload-sized allocation/copy.
; Repeated END is allowed. Any error latches STATE_FAILED until a successful begin.
next	.block
	movem.l d2-d7/a1-a6, -(sp)
	movea.l a0, a4
	movea.l Frame.Source(a4), a5
	movea.l Frame.Segment(a4), a6
	movea.l Frame.Buffer(a4), a3
dispatch
	move.l Frame.State(a4), d0
	cmpi.l #STATE_HEADER, d0
	beq.w header
	cmpi.l #STATE_SEGMENT, d0
	beq.w segment
	cmpi.l #STATE_PAYLOAD, d0
	beq.w payload
	cmpi.l #STATE_PADDING, d0
	beq.w padding
	cmpi.l #STATE_RELOCATIONS, d0
	beq.w relocations
	cmpi.l #STATE_GROUP, d0
	beq.w group
	cmpi.l #STATE_OFFSETS, d0
	beq.w offsets
	cmpi.l #STATE_SEGMENT_END, d0
	beq.w segmentEnd
	cmpi.l #STATE_DONE, d0
	beq.w finish
	bra.w bad
header
	move.l #hunk.HUNK_HEADER, (a3)+
	clr.l (a3)+
	move.l hunk.Frame.Count(a5), d2
	move.l d2, (a3)+
	clr.l (a3)+
	subq.l #1, d2
	move.l d2, (a3)+
	movea.l a6, a1
table
	move.l hunk.Part.Size(a1), d0
	addq.l #3, d0
	lsr.l #2, d0
	move.l d0, (a3)+
	adda.l #hunk.SEGMENT_BYTES, a1
	dbra d2, table
	move.l #STATE_SEGMENT, Frame.State(a4)
	bra.w metadata
segment
	moveq #0, d0
	move.w hunk.Part.Kind(a6), d0
	addi.l #hunk.HUNK_CODE-1, d0
	move.l d0, (a3)+
	move.l #STATE_RELOCATIONS, Frame.State(a4)
	move.l hunk.Part.Size(a6), d0
	cmpi.w #3, hunk.Part.Kind(a6)
	beq.w segmentSize
	move.l hunk.Part.Data(a6), Frame.Cursor(a4)
	move.l hunk.Part.Used(a6), d0
	move.l d0, Frame.Remaining(a4)
	move.l #STATE_PAYLOAD, Frame.State(a4)
segmentSize
	addq.l #3, d0
	lsr.l #2, d0
	move.l d0, (a3)+
	bra.w metadata
payload
	movea.l Frame.Cursor(a4), a0
	move.l Frame.Remaining(a4), d1
	cmpi.l #$7ffffffc, d1
	bls.w payloadChunk
	move.l #$7ffffffc, d1  ; DOS transport takes positive signed lengths
payloadChunk
	add.l d1, Frame.Cursor(a4)
	bcs.w bad
	sub.l d1, Frame.Remaining(a4)
	bne.w yieldItem
	move.l #STATE_PADDING, Frame.State(a4)
	bra.w yieldItem
padding
	move.l #STATE_RELOCATIONS, Frame.State(a4)
	move.l hunk.Part.Used(a6), d1
	neg.l d1
	andi.l #3, d1
	beq.w dispatch
	lea Zeros, a0
	bra.w yieldItem
relocations
	tst.l hunk.Part.FixupCount(a6)
	beq.w segmentEnd
	move.l #hunk.HUNK_RELOC32, (a3)+
	clr.l Frame.Target(a4)
	move.l #STATE_GROUP, Frame.State(a4)
	bra.w metadata
group
	move.l Frame.Target(a4), d3
	cmp.l hunk.Frame.Count(a5), d3
	bhs.w groupsDone
	movea.l hunk.Part.Fixups(a6), a1
	move.l hunk.Part.FixupCount(a6), d4
	moveq #0, d0
countTarget
	cmp.l hunk.Reloc.Target(a1), d3
	bne.w countNext
	addq.l #1, d0
countNext
	adda.l #hunk.FIXUP_BYTES, a1
	subq.l #1, d4
	bne.w countTarget
	addq.l #1, Frame.Target(a4)
	tst.l d0
	beq.w group
	move.l d0, (a3)+
	move.l d3, (a3)+
	move.l hunk.Part.Fixups(a6), Frame.Cursor(a4)
	move.l hunk.Part.FixupCount(a6), Frame.Remaining(a4)
	move.l #STATE_OFFSETS, Frame.State(a4)
	bra.w metadata
offsets
	move.l Frame.Target(a4), d3
	subq.l #1, d3
	movea.l Frame.Cursor(a4), a1
	move.l Frame.Remaining(a4), d4
	move.l Frame.Capacity(a4), d5
	andi.l #$fffffffc, d5
	add.l a3, d5
offsetScan
	cmp.l hunk.Reloc.Target(a1), d3
	bne.w offsetNext
	move.l hunk.Reloc.Offset(a1), (a3)+
offsetNext
	adda.l #hunk.FIXUP_BYTES, a1
	subq.l #1, d4
	beq.w offsetsDone
	cmp.l a3, d5
	bhi.w offsetScan
	move.l a1, Frame.Cursor(a4)
	move.l d4, Frame.Remaining(a4)
	bra.w metadata
offsetsDone
	move.l #STATE_GROUP, Frame.State(a4)
	cmpa.l Frame.Buffer(a4), a3
	beq.w dispatch  ; all matches may have appeared in the previous full chunk
	bra.w metadata
groupsDone
	clr.l (a3)+
segmentEnd
	move.l #hunk.HUNK_END, (a3)+
	addq.l #1, Frame.Index(a4)
	adda.l #hunk.SEGMENT_BYTES, a6
	move.l a6, Frame.Segment(a4)
	move.l #STATE_SEGMENT, Frame.State(a4)
	move.l Frame.Index(a4), d0
	cmp.l hunk.Frame.Count(a5), d0
	blo.w metadata
	move.l #STATE_DONE, Frame.State(a4)
metadata
	movea.l Frame.Buffer(a4), a0
	move.l a3, d1
	sub.l a0, d1
yieldItem
	tst.l d1
	ble.w bad
	move.l Frame.Emitted(a4), d0
	add.l d1, d0
	bcs.w bad
	cmp.l Frame.Expected(a4), d0
	bhi.w bad
	move.l d0, Frame.Emitted(a4)
	moveq #ITEM, d0
	bra.w done
finish
	move.l Frame.Emitted(a4), d0
	cmp.l Frame.Expected(a4), d0
	bne.w bad
	moveq #END, d0
	moveq #0, d1
	bra.w done
bad
	move.l #STATE_FAILED, Frame.State(a4)
	moveq #ERROR, d0
	moveq #0, d1
done
	movem.l (sp)+, d2-d7/a1-a6
	tst.l d0
	rts
	.bend  ; next
	.endsection
	.priv
	.section data, kind=data
Zeros	.long 0
	.endsection
	.endmodule

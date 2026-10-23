; Stream raw BIN over an immutable sorted, disjoint sparse image.
; @opforge-owner: opforge.assembler.output.flat
	.module opforge.assembler.output.flat
	.cpu 68020
	.use opforge.assembler.output.records as records
	.pub
Frame	.struct
Data	.long ?
DataBytes	.long ?
Spans	.long ?
SpanBytes	.long ?
Buffer	.long ?  ; caller scratch, no ownership transferred
Capacity	.long ?  ; positive signed chunk limit, at most $7fffffff bytes
RangeSet	.word ?  ; 0 initialized envelope, 1 explicit inclusive range
Fill	.word ?  ; unsigned byte used for holes and explicit range padding
RangeStart	.long ?
RangeEnd	.long ?
Load	.long ?  ; first address; zero for an empty image
Extent	.long ?  ; low32 of last-inclusive - Load + 1
ExtentHigh	.long ?  ; zero or one: full 32-bit address space has extent1:0
Valid	.long ?
Cursor	.long ?  ; byte offset in Spans
Within	.long ?
Address	.long ?
Remaining	.long ?
RemainingHigh	.long ?
	.endstruct
FRAME_BYTES = Frame.RemainingHigh+4
ITEM = 0
END = 1
INVALID = 2
STATUS_OK = 0
STATUS_BAD = 1
	.priv
MAGIC = $464c4154
	.section code, kind=code
	.pub
; A0=even Frame. D0/CCR=status,D1=load address on success (zero on failure);
; other registers preserved. Publish Load/Extent/ExtentHigh only after complete
; preflight. Failure publishes nothing and leaves Frame/input/output bytes alone.
; Spans use records.Span12, all nonempty, sorted and disjoint. Data offsets must
; fit DataBytes; inclusive address ends may equal $ffffffff. Pointer extents must
; not wrap. Frame and Buffer must be disjoint from each other and both input
; views; the two immutable input views may overlap. No allocation or OS calls.
; next requires a successful init; inputs then remain immutable until END.
; Empty images permit NULL input pointers. An explicit ascending inclusive range
; clips initialized bytes and pads either side with Fill; otherwise only internal
; holes are filled. Range/fill validation is atomic with input preflight.
init	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a4
	move.l a0, d0
	beq.w bad
	btst #0, d0
	bne.w bad
	add.l #FRAME_BYTES, d0
	bcs.w bad
	tst.l Frame.Capacity(a4)
	ble.w bad
	cmpi.w #1, Frame.RangeSet(a4)
	bhi.w bad
	cmpi.w #255, Frame.Fill(a4)
	bhi.w bad
	movea.l Frame.Buffer(a4), a0
	move.l Frame.Capacity(a4), d0
	bsr.w span
	bne.w bad
	move.l Frame.Buffer(a4), d0
	move.l Frame.Capacity(a4), d1
	move.l a4, d2
	moveq #FRAME_BYTES, d3
	bsr.w disjoint
	bne.w bad
	movea.l Frame.Spans(a4), a0
	move.l Frame.SpanBytes(a4), d0
	bsr.w span
	bne.w bad
	move.l Frame.Spans(a4), d0
	move.l Frame.SpanBytes(a4), d1
	bsr.w inputsDisjoint
	bne.w bad
	movea.l Frame.Data(a4), a0
	move.l Frame.DataBytes(a4), d0
	bsr.w span
	bne.w bad
	move.l Frame.Data(a4), d0
	move.l Frame.DataBytes(a4), d1
	bsr.w inputsDisjoint
	bne.w bad
	moveq #0, d4  ; seen a nonempty span
	moveq #0, d6  ; minimum address
	moveq #0, d7  ; last inclusive address
	move.l Frame.SpanBytes(a4), d5
	beq.w validated
	move.l Frame.Spans(a4), d0
	btst #0, d0
	bne.w bad
	movea.l d0, a5
checkSpan
	cmpi.l #records.SPAN_BYTES, d5
	blo.w bad
	move.l records.Span.Bytes(a5), d1
	beq.w bad
	move.l Frame.DataBytes(a4), d2
	sub.l records.Span.Offset(a5), d2
	bcs.w bad
	cmp.l d2, d1
	bhi.w bad
	move.l records.Span.Address(a5), d2
	tst.l d4
	beq.w first
	cmp.l d7, d2
	bls.w bad
	bra.w addressEnd
first
	move.l d2, d6
addressEnd
	subq.l #1, d1
	add.l d1, d2
	bcs.w bad
	move.l d2, d7
	moveq #1, d4
	adda.w #records.SPAN_BYTES, a5
	subi.l #records.SPAN_BYTES, d5
	bne.w checkSpan
validated
	moveq #0, d0  ; initial span cursor
	moveq #0, d1  ; initial payload displacement
	tst.w Frame.RangeSet(a4)
	beq.w extent
	move.l Frame.RangeStart(a4), d6
	move.l Frame.RangeEnd(a4), d7
	cmp.l d6, d7
	blo.w bad
	moveq #1, d4  ; explicit ranges also render an empty initialized image
seek
	cmp.l Frame.SpanBytes(a4), d0
	bhs.w extent
	movea.l Frame.Spans(a4), a5
	adda.l d0, a5
	move.l records.Span.Address(a5), d5
	cmp.l d6, d5
	bhs.w extent
	add.l records.Span.Bytes(a5), d5
	subq.l #1, d5  ; validated inclusive end; exclusive end may wrap
	cmp.l d6, d5
	bhs.w within
	addi.l #records.SPAN_BYTES, d0
	bra.w seek
within
	move.l d6, d1
	sub.l records.Span.Address(a5), d1
extent
	moveq #0, d2
	moveq #0, d3
	tst.l d4
	beq.w publish
	move.l d7, d2
	sub.l d6, d2
	addq.l #1, d2
	bcc.w publish
	moveq #1, d3
publish
	move.l d6, Frame.Load(a4)
	move.l d2, Frame.Extent(a4)
	move.l d3, Frame.ExtentHigh(a4)
	move.l d6, Frame.Address(a4)
	move.l d2, Frame.Remaining(a4)
	move.l d3, Frame.RemainingHigh(a4)
	move.l d0, Frame.Cursor(a4)
	move.l d1, Frame.Within(a4)
	move.l #MAGIC, Frame.Valid(a4)
	move.l d6, d1
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #0, d1
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; init

; output_io Generator ABI: A0=initialized Frame/context. D0/CCR=ITEM0,END1,
; INVALID2. ITEM returns A0=Buffer,D1=positive bytes <=Capacity. END/INVALID
; preserve incoming A0/D1; all other registers always preserved. Streams exactly
; ExtentHigh:Extent bytes, with configured Fill for absent initialized bytes.
; Remaining counts terminate iteration, not a wrapping address/end sentinel.
; The final Address may wrap after emitting $ffffffff; it is never read again.
next	.block
	movem.l d1/a0, -(sp)
	movem.l d2-d7/a1-a6, -(sp)
	movea.l a0, a4
	cmpi.l #MAGIC, Frame.Valid(a4)
	bne.w bad
	move.l Frame.Remaining(a4), d0
	or.l Frame.RemainingHigh(a4), d0
	beq.w exhausted
	move.l Frame.Capacity(a4), d6
	tst.l Frame.RemainingHigh(a4)
	bne.w chunkSize
	cmp.l Frame.Remaining(a4), d6
	bls.w chunkSize
	move.l Frame.Remaining(a4), d6
chunkSize
	move.l d6, d7
	movea.l Frame.Buffer(a4), a5
more
	tst.l d7
	beq.w chunkReady
	move.l Frame.Cursor(a4), d0
	cmp.l Frame.SpanBytes(a4), d0
	bhs.w trailing
	movea.l Frame.Spans(a4), a6
	adda.l d0, a6
	tst.l Frame.Within(a4)
	bne.w payload
	move.l records.Span.Address(a6), d4
	sub.l Frame.Address(a4), d4
	bcs.w bad
	beq.w payload
	cmp.l d7, d4
	bls.w zeroSize
	move.l d7, d4
zeroSize
	move.l d4, d0
zeros
	move.b Frame.Fill+1(a4), (a5)+
	subq.l #1, d0
	bne.w zeros
	bsr.w advance
	bra.w more
trailing
	move.l d7, d4
	bra.w zeroSize
payload
	move.l records.Span.Bytes(a6), d4
	sub.l Frame.Within(a4), d4
	bls.w bad
	cmp.l d7, d4
	bls.w payloadSize
	move.l d7, d4
payloadSize
	movea.l Frame.Data(a4), a1
	adda.l records.Span.Offset(a6), a1
	adda.l Frame.Within(a4), a1
	move.l d4, d0
copy
	move.b (a1)+, (a5)+
	subq.l #1, d0
	bne.w copy
	add.l d4, Frame.Within(a4)
	move.l Frame.Within(a4), d0
	cmp.l records.Span.Bytes(a6), d0
	bne.w payloadReady
	addi.l #records.SPAN_BYTES, Frame.Cursor(a4)
	clr.l Frame.Within(a4)
payloadReady
	bsr.w advance
	bra.w more
chunkReady
	movea.l Frame.Buffer(a4), a0
	move.l d6, d1
	moveq #ITEM, d0
	movem.l (sp)+, d2-d7/a1-a6
	addq.l #8, sp  ; discard saved A0/D1 only for an emitted item
	tst.l d0
	rts
exhausted
	moveq #END, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d2-d7/a1-a6
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; next
	.priv
; A4=Frame,D4=positive emitted count,D7=chunk remaining. Only D7 and cursor
; fields change; CCR unspecified. Address wrap is permitted only at the tail.
advance	.block
	add.l d4, Frame.Address(a4)
	sub.l d4, Frame.Remaining(a4)
	bcc.w remainingReady
	subq.l #1, Frame.RemainingHigh(a4)
remainingReady
	sub.l d4, d7
	rts
	.bend  ; advance

; A0/D0=physical input span. D0/CCR=status; all other registers preserved.
span	.block
	move.l d1, -(sp)
	tst.l d0
	beq.w ready
	move.l a0, d1
	beq.w bad
	add.l d0, d1
	bcs.w bad
ready
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	move.l (sp)+, d1
	tst.l d0
	rts
	.bend  ; span

; D0/D1=input pointer/bytes,A4=Frame. D0/status; others preserved.
; Input views must be disjoint from mutable Frame and Buffer.
inputsDisjoint	.block
	movem.l d2-d3, -(sp)
	move.l a4, d2
	moveq #FRAME_BYTES, d3
	move.l d0, -(sp)
	bsr.w disjoint
	move.l d0, d2
	move.l (sp)+, d0
	tst.l d2
	bne.w bad
	move.l Frame.Buffer(a4), d2
	move.l Frame.Capacity(a4), d3
	bsr.w disjoint
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d2-d3
	tst.l d0
	rts
	.bend  ; inputsDisjoint

; D0/D1=span one,D2/D3=span two, with nonwrapping validated physical bounds.
; D0/CCR=status; other registers preserved. Zero byte spans do not overlap.
disjoint	.block
	movem.l d4-d5, -(sp)
	tst.l d1
	beq.w ready
	tst.l d3
	beq.w ready
	move.l d0, d4
	add.l d1, d4
	move.l d2, d5
	add.l d3, d5
	cmp.l d5, d0
	bhs.w ready
	cmp.l d4, d2
	blo.w bad
ready
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d4-d5
	tst.l d0
	rts
	.bend  ; disjoint
	.endsection
	.endmodule

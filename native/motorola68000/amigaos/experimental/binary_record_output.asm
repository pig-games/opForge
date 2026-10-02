; Bounded Intel HEX / Motorola S-record rendering over validated sparse spans.
; @opforge-owner: experimental.amigaos.binary_record_output
	.module experimental.amigaos.binary_record_output
	.cpu 68020
	.include "memory_telemetry.i"
	.pub
Span	.struct
Address	.long ?
Offset	.long ?
Bytes	.long ?
	.endstruct
SPAN_BYTES = 12
Frame	.struct
Data	.long ?
DataBytes	.long ?
Spans	.long ?
SpanBytes	.long ?
Format	.word ?
StartSet	.word ?
Start	.long ?
Buffer	.long ?
Capacity	.long ?
; Private cursor and one record's payload; caller allocates FRAME_BYTES.
Valid	.long ?
Cursor	.long ?
Within	.long ?
Bank	.long ?
Phase	.word ?
Width	.word ?
Payload	.res 32
	.endstruct
FRAME_BYTES = Frame.Payload+32
HEX = 4
SREC = 5
ITEM = 0
END = 1
INVALID = 2
STATUS_OK = 0
STATUS_BAD = 1
MIN_CAPACITY = 128
LINE_LIMIT = 32
MAX_LINE_BYTES = 79
MAGIC = $52454331
	.section code, kind=code
	.pub
; A0=Frame. D0/CCR=0 success, 1 invalid; other registers preserved.
; Validates all spans before enabling iteration. Input views and fields must
; remain unchanged through END; buffer must not alias the frame or input views.
; Empty images are allowed. Span addresses are sorted and nonoverlapping.
begin	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a4
	clr.l Frame.Valid(a4)
	.MEMORY_COUNTER_CLEAR RecordCount
	.MEMORY_COUNTER_CLEAR OutputBytes
	cmpi.w #HEX, Frame.Format(a4)
	beq.w formatOk
	cmpi.w #SREC, Frame.Format(a4)
	bne.w bad
formatOk
	cmpi.l #MIN_CAPACITY, Frame.Capacity(a4)
	blo.w bad
	move.l Frame.Buffer(a4), d0
	beq.w bad
	add.l Frame.Capacity(a4), d0
	bcs.w bad
	moveq #0, d6
	tst.w Frame.StartSet(a4)
	beq.w startOk
	move.l Frame.Start(a4), d6
startOk
	move.l Frame.SpanBytes(a4), d7
	beq.w validated
	move.l Frame.Spans(a4), d0
	beq.w bad
	btst #0, d0
	bne.w bad
	movea.l d0, a5
	add.l d7, d0
	bcs.w bad
	move.l Frame.Data(a4), d0
	beq.w bad
	add.l Frame.DataBytes(a4), d0
	bcs.w bad
	moveq #0, d5
checkSpan
	cmpi.l #SPAN_BYTES, d7
	blo.w bad
	move.l Span.Bytes(a5), d1
	beq.w bad
	move.l Span.Offset(a5), d2
	add.l d1, d2
	bcs.w bad
	cmp.l Frame.DataBytes(a4), d2
	bhi.w bad
	subq.l #1, d1
	move.l Span.Address(a5), d2
	tst.l d5
	beq.w first
	cmp.l d3, d2
	bls.w bad
first
	add.l d1, d2
	bcs.w bad
	move.l d2, d3
	cmp.l d6, d2
	bls.w maximum
	move.l d2, d6
maximum
	moveq #1, d5
	adda.w #SPAN_BYTES, a5
	subi.l #SPAN_BYTES, d7
	bne.w checkSpan
validated
	move.w #2, Frame.Width(a4)
	cmpi.l #$ffff, d6
	bls.w ready
	move.w #3, Frame.Width(a4)
	cmpi.l #$ffffff, d6
	bls.w ready
	move.w #4, Frame.Width(a4)
ready
	clr.l Frame.Cursor(a4)
	clr.l Frame.Within(a4)
	move.l #-1, Frame.Bank(a4)
	clr.w Frame.Phase(a4)
	move.l #MAGIC, Frame.Valid(a4)
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; begin

; A0=Frame. D0/CCR=ITEM(0), END(1), INVALID(2).
; ITEM: A0=caller buffer, D1=positive byte count of complete LF records.
; Other registers preserved; A0/D1 unchanged on END/INVALID. Fills as many
; complete records as bounded capacity permits; no allocation or OS calls.
next	.block
	movem.l d1/a0, -(sp)
	movem.l d2-d7/a1-a6, -(sp)
	movea.l a0, a4
	cmpi.l #MAGIC, Frame.Valid(a4)
	bne.w bad
	move.l Frame.Buffer(a4), d3
	move.l Frame.Capacity(a4), d5
	moveq #0, d2
more
	movea.l a4, a0
	bsr.w record
	tst.l d0
	bne.w stopped
	add.l d1, d2
	move.l d5, d0
	sub.l d2, d0
	cmpi.l #MAX_LINE_BYTES, d0
	blo.w chunk
	move.l d3, d0
	add.l d2, d0
	move.l d0, Frame.Buffer(a4)
	bra.w more
stopped
	tst.l d2
	bne.w chunk
	move.l d3, Frame.Buffer(a4)
	bra.w done
chunk
	move.l d3, Frame.Buffer(a4)
	movea.l d3, a0
	move.l d2, d1
	.MEMORY_COUNTER_ADD OutputBytes, d1
	moveq #ITEM, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d2-d7/a1-a6
	tst.l d0
	bne.w restore
	addq.l #8, sp
	tst.l d0
	rts
restore
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; next
	.priv

; Produce one bounded record. Same output ABI as next; caller reserves 79
; bytes at Frame.Buffer. Private iteration never changes inputs except cursor.
record	.block
	movem.l d2-d7/a1-a6, -(sp)
	movea.l a0, a4
	cmpi.l #MAGIC, Frame.Valid(a4)
	bne.w bad
	cmpi.w #3, Frame.Phase(a4)
	beq.w ended
	movea.l Frame.Buffer(a4), a1
	moveq #0, d5
	moveq #0, d7
	moveq #0, d4
	moveq #0, d6
	lea Frame.Payload(a4), a2
	tst.w Frame.Phase(a4)
	bne.w finish
	move.l Frame.Cursor(a4), d0
	cmp.l Frame.SpanBytes(a4), d0
	beq.w finish
	movea.l Frame.Spans(a4), a5
	adda.l d0, a5
	move.l Span.Address(a5), d6
	add.l Frame.Within(a4), d6
	cmpi.w #HEX, Frame.Format(a4)
	bne.w gather
	move.l d6, d0
	swap d0
	andi.l #$ffff, d0
	cmp.l Frame.Bank(a4), d0
	beq.w gather
	move.l Frame.Bank(a4), d2
	move.l d0, Frame.Bank(a4)
	tst.l d0
	bne.w bank
	tst.l d2
	bmi.w gather
bank
	move.b d0, 1(a2)
	lsr.w #8, d0
	move.b d0, (a2)
	moveq #2, d5
	moveq #4, d4
	moveq #0, d6
	bra.w render
; Gather neighboring address-contiguous spans, even with disjoint data offsets.
gather
	move.l d6, d3
byte
	movea.l Frame.Data(a4), a3
	adda.l Span.Offset(a5), a3
	move.l Frame.Within(a4), d0
	move.b 0(a3, d0.l), d2
	move.b d2, (a2)+
	addq.w #1, d5
	addq.l #1, d0
	move.l d0, Frame.Within(a4)
	cmp.l Span.Bytes(a5), d0
	bne.w remaining
	clr.l Frame.Within(a4)
	addi.l #SPAN_BYTES, Frame.Cursor(a4)
	adda.w #SPAN_BYTES, a5
	move.l Frame.Cursor(a4), d0
	cmp.l Frame.SpanBytes(a4), d0
	beq.w render
	cmpi.l #$ffffffff, d3
	beq.w render
	addq.l #1, d3
	cmp.l Span.Address(a5), d3
	bne.w render
	bra.w limit
remaining
	addq.l #1, d3
limit
	cmpi.w #LINE_LIMIT, d5
	beq.w render
	cmpi.w #HEX, Frame.Format(a4)
	bne.w byte
	tst.w d3
	beq.w render
	bra.w byte
finish
	cmpi.w #HEX, Frame.Format(a4)
	bne.w termination
	tst.w Frame.Phase(a4)
	bne.w eof
	move.w #1, Frame.Phase(a4)
	tst.w Frame.StartSet(a4)
	beq.w eof
	moveq #4, d5
	moveq #3, d4
	move.l Frame.Start(a4), d0
	cmpi.l #$ffff, d0
	bls.w start
	moveq #5, d4
start
	move.l d0, (a2)
	bra.w render
eof
	moveq #1, d4
	move.w #3, Frame.Phase(a4)
	bra.w render
termination
	move.w #3, Frame.Phase(a4)
	tst.w Frame.StartSet(a4)
	beq.w render
	move.l Frame.Start(a4), d6
render
	lea Frame.Payload(a4), a2
	cmpi.w #HEX, Frame.Format(a4)
	bne.w srecord
	move.b #':', (a1)+
	move.w d5, d0
	bsr.w octet
	move.w d6, d0
	lsr.w #8, d0
	bsr.w octet
	move.w d6, d0
	bsr.w octet
	move.w d4, d0
	bsr.w octet
	bra.w payload
srecord
	move.b #'S', (a1)+
	move.w Frame.Width(a4), d2
	move.w d2, d0
	addi.w #'0'-1, d0
	cmpi.w #3, Frame.Phase(a4)
	bne.w kind
	moveq #'0'+11, d0
	sub.w d2, d0
kind
	move.b d0, (a1)+
	move.w d5, d0
	add.w d2, d0
	addq.w #1, d0
	bsr.w octet
	move.l d6, d3
	moveq #4, d0
	sub.w d2, d0
	lsl.w #3, d0
	lsl.l d0, d3
address
	rol.l #8, d3
	move.l d3, d0
	bsr.w octet
	subq.w #1, d2
	bne.w address
payload
	tst.w d5
	beq.w checksum
	move.w d5, d2
bytes
	moveq #0, d0
	move.b (a2)+, d0
	bsr.w octet
	subq.w #1, d2
	bne.w bytes
checksum
	move.w d7, d0
	not.b d0
	cmpi.w #HEX, Frame.Format(a4)
	bne.w checked
	addq.b #1, d0
checked
	bsr.w octet
	move.b #10, (a1)+
	movea.l Frame.Buffer(a4), a0
	move.l a1, d1
	sub.l a0, d1
	.MEMORY_COUNTER_ADD RecordCount, #1
	moveq #ITEM, d0
	bra.w done
ended
	moveq #END, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d2-d7/a1-a6
	tst.l d0
	rts
	.bend  ; record
; D0 low byte -> uppercase pair at A1; adds byte to D7 checksum.
; Clobbers D0 only, advances A1; CCR unspecified.
octet	.block
	add.b d0, d7
	move.l d0, -(sp)
	lsr.b #4, d0
	bsr.w digit
	move.l (sp)+, d0
	andi.b #15, d0
	bsr.w digit
	rts
	.bend  ; octet
digit	.block
	cmpi.b #9, d0
	bls.w decimal
	addq.b #7, d0
decimal
	addi.b #'0', d0
	move.b d0, (a1)+
	rts
	.bend  ; digit
	.endsection

.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	.section data, kind=data
	.pub
RecordCount
	.long 0
OutputBytes
	.long 0
	.endsection
.endif
.endif
	.endmodule

; Append-only dense logical storage. Chunk allocations never move; directory rows
; can move on append, so callers retain numeric handles, never descriptor pointers.
; @opforge-owner: experimental.amigaos.binary_capture_store
	.module experimental.amigaos.binary_capture_store
	.cpu 68020
	.include "capture_telemetry.i"
	.include "hotspot_telemetry.i"
	.use experimental.amigaos.binary_memory as memory
	.pub
State	.struct
Directory	.res memory.Block.Used+4
Active	.long ?
Cached	.long ?
LogicalUsed	.long ?
Reserved	.long ?
.endstruct
Row	.struct
Base	.long ?
Storage	.res memory.Block.Used+4
.endstruct
STATE_BYTES = State.Reserved+4
ROW_BYTES = Row.Storage+memory.Block.Used+4
ROW_SHIFT = 4
CHUNK_BYTES = 32768
DEDICATED_THRESHOLD = 4096
	.section code, kind=code
; A0=zero-initialized State,A1=record,D0=positive bytes. D0/CCR=status,
; D1=dense logical handle on success (zero on failure). Others preserved.
; Complete records are contiguous. Failed allocation preserves committed records;
; directory capacity may grow. memory telemetry observes all allocations/copies.
append	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a4
	movea.l a1, a5
	move.l d0, d5
	beq.w bad
	cmpi.l #memory.LIMIT, d5
	bhi.w bad
	move.l State.LogicalUsed(a4), d6
	add.l d5, d6
	bcs.w bad
	cmpi.l #DEDICATED_THRESHOLD, d5
	bhi.w newRow
	move.l State.Active(a4), d4
	beq.w newRow
	subq.l #1, d4
	movea.l State.Directory+memory.Block.Pointer(a4), a3
	adda.l d4, a3
	move.l Row.Storage+memory.Block.Capacity(a3), d0
	sub.l Row.Storage+memory.Block.Used(a3), d0
	cmp.l d5, d0
	bhs.w copyRecord
newRow
	move.l State.Directory+memory.Block.Used(a4), d4
	move.l d4, d0
	addi.l #ROW_BYTES, d0
	lea State.Directory(a4), a0
	jsr memory.reserve
	bne.w bad
	movea.l State.Directory+memory.Block.Pointer(a4), a3
	adda.l d4, a3
	; An uncommitted row owns nothing, including after a failed allocation.
	clr.l Row.Storage+memory.Block.Pointer(a3)
	clr.l Row.Storage+memory.Block.Capacity(a3)
	clr.l Row.Storage+memory.Block.Used(a3)
	move.l #CHUNK_BYTES, d0
	cmpi.l #DEDICATED_THRESHOLD, d5
	bls.w allocate
	move.l d5, d0
allocate
	lea Row.Storage(a3), a0
	.CAPTURE_GROW_BEGIN
	jsr memory.reserveExact
	.CAPTURE_GROW_END
	bne.w bad
	move.l Row.Storage+memory.Block.Capacity(a3), d0
	add.l d0, State.Reserved(a4)
	move.l State.LogicalUsed(a4), Row.Base(a3)
	addi.l #ROW_BYTES, State.Directory+memory.Block.Used(a4)
	clr.l State.Active(a4)
	cmpi.l #DEDICATED_THRESHOLD, d5
	bhi.w copyRecord
	move.l d4, d0
	addq.l #1, d0
	move.l d0, State.Active(a4)
copyRecord
	.HOTSPOT_ENTER #HOTSPOT_CAPTURE_APPEND
	movea.l Row.Storage+memory.Block.Pointer(a3), a1
	adda.l Row.Storage+memory.Block.Used(a3), a1
	move.l d5, d2
	lsr.l #2, d2
	beq.w tail
copyLong
	move.l (a5)+, (a1)+
	subq.l #1, d2
	bne.w copyLong
tail
	move.l d5, d2
	andi.l #3, d2
	beq.w copied
copyByte
	move.b (a5)+, (a1)+
	subq.l #1, d2
	bne.w copyByte
copied
	.HOTSPOT_LEAVE #HOTSPOT_CAPTURE_APPEND
	add.l d5, Row.Storage+memory.Block.Used(a3)
	move.l State.LogicalUsed(a4), d1
	move.l d6, State.LogicalUsed(a4)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
	moveq #0, d1
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; append

; A0=State,D1=logical byte offset. D0/CCR=status; on success A1=Block,
; D1=local byte offset,D2=remaining used bytes. Failure clears all outputs.
; Preserves D3-D7/A0/A2-A6. Cached interval precedes binary search. Descriptor
; is valid until append/cleanup; returned remaining extent excludes capacity slack.
resolve	.block
	movem.l d3-d7/a0/a2-a6, -(sp)
	movea.l a0, a4
	cmp.l State.LogicalUsed(a4), d1
	bhs.w bad
	movea.l State.Directory+memory.Block.Pointer(a4), a3
	move.l State.Cached(a4), d4
	beq.w search
	subq.l #1, d4
	lea 0(a3, d4.l), a2
	move.l d1, d3
	sub.l Row.Base(a2), d3
	bcs.w search
	cmp.l Row.Storage+memory.Block.Used(a2), d3
	blo.w found
search
	moveq #0, d5
	move.l State.Directory+memory.Block.Used(a4), d6
	lsr.l #ROW_SHIFT, d6
searchLoop
	cmp.l d6, d5
	bhs.w bad
	move.l d5, d4
	add.l d6, d4
	lsr.l #1, d4
	move.l d4, d7
	lsl.l #ROW_SHIFT, d4
	lea 0(a3, d4.l), a2
	move.l d1, d3
	sub.l Row.Base(a2), d3
	bcs.w lower
	cmp.l Row.Storage+memory.Block.Used(a2), d3
	blo.w found
	move.l d7, d5
	addq.l #1, d5
	bra.w searchLoop
lower
	move.l d7, d6
	bra.w searchLoop
found
	addq.l #1, d4
	move.l d4, State.Cached(a4)
	lea Row.Storage(a2), a1
	move.l d3, d1
	move.l memory.Block.Used(a1), d2
	sub.l d1, d2
	moveq #0, d0
	bra.w done
bad
	suba.l a1, a1
	moveq #0, d1
	moveq #0, d2
	moveq #1, d0
done
	movem.l (sp)+, d3-d7/a0/a2-a6
	tst.l d0
	rts
	.bend  ; resolve

; A0=State. Free every owned chunk and directory, then zero State. Idempotent.
; Preserves all registers; CCR unspecified. No descriptor survives this call.
cleanup	.block
	movem.l d0-d2/a0-a2, -(sp)
	movea.l a0, a2
	movea.l State.Directory+memory.Block.Pointer(a2), a1
	move.l State.Directory+memory.Block.Used(a2), d2
next
	tst.l d2
	beq.w directory
	lea Row.Storage(a1), a0
	.CAPTURE_RETAINED_RELEASE memory.Block.Capacity(a0)
	jsr memory.release
	adda.l #ROW_BYTES, a1
	subi.l #ROW_BYTES, d2
	bra.w next
directory
	lea State.Directory(a2), a0
	jsr memory.release
	moveq #STATE_BYTES/4-1, d2
clear
	clr.l (a2)+
	dbra d2, clear
	movem.l (sp)+, d0-d2/a0-a2
	rts
	.bend  ; cleanup
	.endsection
	.endmodule

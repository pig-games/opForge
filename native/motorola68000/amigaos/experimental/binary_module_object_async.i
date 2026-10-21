; One object write in flight. Writer.Io starts at zero so the synchronous
; path/close/discard helpers share exactly the same file publication contract.
	.pub
PENDING = asynchronous.PENDING
; A0=zero Writer,A1=configured idle Context. D0/CCR=status; others preserved.
; Borrows its directory/DOS lifetime; owns a separate handle and packet port.
; No second directory creation and no allocation until the first beginWrite.
initWriter	.block
	movem.l d1/a0-a2, -(sp)
	move.l Context.Dos(a1), d0
	beq.w bad
	tst.l Context.OwnedDir(a1)
	beq.w bad
	tst.l Context.Dos(a0)
	bne.w bad
	move.l d0, Context.Dos(a0)
	move.l #1, Context.OwnedDir(a0)
	lea Context.Directory(a1), a1
	lea Context.Directory(a0), a2
	move.w #512/4-1, d1
copy
	move.l (a1)+, (a2)+
	dbra d1, copy
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1/a0-a2
	tst.l d0
	rts
	.bend  ; initWriter

; A0=Writer,A2=filename,A1=immutable buffer,D2=1..WRITE_LIMIT.
; D0/CCR=OK when submitted, INVALID otherwise; others preserved.
; The caller retains the entire buffer until pollWrite/waitWrite is terminal.
; Only payload writing overlaps CPU work. Open, close and rename stay synchronous.
beginWrite	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	tst.l Writer.Failure(a5)
	bne.w bad
	cmpi.l #WRITE_LIMIT, d2
	bhi.w bad
	move.l a1, d4
	move.l d2, d6
	bsr.w openStore
	bne.w bad
	lea Writer.Transfer(a5), a0
	movea.l Context.Dos(a5), a1
	move.l Context.File(a5), d1
	movea.l d4, a2
	move.l d6, d2
	jsr asynchronous.begin
	cmpi.l #PENDING, d0
	bne.w failed
	moveq #OK, d0
	bra.w done
failed
	move.l #1, Writer.Failure(a5)
	bsr.w discard
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; beginWrite

; A0=Writer. D0/CCR=PENDING/OK/INVALID; others preserved.
; Nonblocking packet collection; terminal completion closes/renames exactly once.
; Failure is sticky until cleanupWriter. A failed temporary is never published.
pollWrite	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	tst.l Writer.Failure(a5)
	bne.w bad
	tst.l Context.File(a5)
	beq.w good
	lea Writer.Transfer(a5), a0
	jsr asynchronous.poll
	cmpi.l #PENDING, d0
	beq.w done
	tst.l d0
	bne.w failed
	bsr.w finishWrite
	bra.w done
failed
	bsr.w failWrite
	bra.w done
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; pollWrite

; A0=Writer. D0/CCR=OK/INVALID; others preserved. Drain before releasing
; the buffer/handle or returning final success. No cancellation of live packets.
waitWrite	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	lea Writer.Transfer(a5), a0
	jsr asynchronous.wait
	bne.w failed
	tst.l Writer.Failure(a5)
	bne.w bad
	tst.l Context.File(a5)
	beq.w good
	bsr.w finishWrite
	bra.w done
failed
	bsr.w failWrite
	bra.w done
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; waitWrite

; A0=Writer. D0/CCR=OK/INVALID; others preserved. Drain, then dispose
; packet resources and unfinished file. Completed objects remain deliverables.
cleanupWriter	.block
	movem.l d1-d2/a0-a2, -(sp)
	movea.l a0, a2
	bsr.w waitWrite
	move.l d0, d2
	lea Writer.Transfer(a2), a0
	jsr asynchronous.release
	or.l d0, d2
	movea.l a2, a0
	bsr.w cleanup
	or.l d2, d0
	clr.l Writer.Failure(a2)
	movem.l (sp)+, d1-d2/a0-a2
	tst.l d0
	rts
	.bend  ; cleanupWriter
	.priv
; A5=Writer, acknowledged complete buffer. D0/CCR=status.
finishWrite	.block
	movea.l Context.Dos(a5), a6
	bsr.w publishStore
	beq.w done
	bsr.w failWrite
done
	rts
	.bend  ; finishWrite
; A5=Writer, no packet outstanding. D0/CCR=INVALID; unfinished ownership
; remains retryable by cleanup when Close/DeleteFile itself reports a failure.
failWrite	.block
	move.l #1, Writer.Failure(a5)
	movea.l Context.Dos(a5), a6
	bsr.w discard
	moveq #INVALID, d0
	rts
	.bend  ; failWrite

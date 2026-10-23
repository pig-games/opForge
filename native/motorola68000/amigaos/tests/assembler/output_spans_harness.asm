; Numeric interval overlay owner, not a source parser or CPU encoder.
.module main
.cpu 68020
.use opforge.assembler.output.spans as spans
.use opforge.platform.memory as memory
.priv
Case	.struct
Address	.long ?
Offset	.long ?
Bytes	.long ?
Fault	.long ?
.endstruct
CASE_BYTES = Case.Fault+4
FixtureHeader	.struct
CaseCount	.long ?
.endstruct
FIXTURE_HEADER_BYTES = FixtureHeader.CaseCount+4
Result	.struct
Status	.long ?
Used	.long ?
.endstruct
RESULT_BYTES = Result.Used+4
FAULT_USED_BOUNDS = 1
EXEC_BASE = 4
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
	.pub
	.section entry, kind=code
; No inputs. D0=0 on success or 20 on failure; CCR reflects D0.
; D2-D7/A2-A6 are preserved; other registers may be clobbered.
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	move.l #20, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l EXEC_BASE.w, a6
	jsr EXEC_OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done
	move.l d0, DosBase
	movea.l d0, a6
	move.l #OutputName, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, OutputHandle
	beq.w closeLibrary
	jsr cases
	move.l d0, ReturnCode
	movea.l DosBase, a6
	move.l OutputHandle, d1
	jsr DOS_CLOSE(a6)
	tst.l d0
	bne.w closeLibrary
	move.l #20, ReturnCode
closeLibrary
	movea.l DosBase, a1
	movea.l EXEC_BASE.w, a6
	jsr EXEC_CLOSE_LIBRARY(a6)
done
	move.l ReturnCode, d0
	tst.l d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; start
	.endsection

	.priv
	.section code, kind=code
cases	.block
	move.l #Fixture, Cursor
	moveq #FIXTURE_HEADER_BYTES, d0
	bsr.w take
	bne.w bad
	move.l FixtureHeader.CaseCount(a0), d7
next
	tst.l d7
	beq.w valid
	moveq #CASE_BYTES, d0
	bsr.w take
	bne.w bad
	move.l Case.Fault(a0), d6
	move.l Case.Address(a0), d1
	move.l Case.Offset(a0), d2
	move.l Case.Bytes(a0), d3
	move.l Owner+memory.Block.Used, d5
	cmpi.l #FAULT_USED_BOUNDS, d6
	bne.w append
	move.l Owner+memory.Block.Capacity, d0
	addq.l #1, d0
	move.l d0, Owner+memory.Block.Used
append
	lea Owner, a0
	jsr spans.append
	move.l d0, ResultData+Result.Status
	tst.l d6
	beq.w published
	; Deliberately invalid caller envelope must not alter owned numeric rows.
	move.l d5, Owner+memory.Block.Used
published
	move.l Owner+memory.Block.Used, ResultData+Result.Used
	lea ResultData, a0
	moveq #RESULT_BYTES, d0
	bsr.w writeSpan
	bne.w bad
	movea.l Owner+memory.Block.Pointer, a0
	move.l Owner+memory.Block.Used, d0
	bsr.w writeSpan
	bne.w bad
	subq.l #1, d7
	bra.w next
valid
	lea Owner, a0
	jsr memory.release
	moveq #0, d0
	rts
bad
	lea Owner, a0
	jsr memory.release
	moveq #20, d0
	rts
	.bend  ; cases
take	.block
	movem.l d1, -(sp)
	tst.l d0
	bmi.w invalid
	move.l #FixtureEnd, d1
	sub.l Cursor, d1
	cmp.l d1, d0
	bhi.w invalid
	movea.l Cursor, a0
	add.l d0, Cursor
	moveq #0, d0
	bra.w done
invalid
	moveq #1, d0
done
	tst.l d0
	movem.l (sp)+, d1
	rts
	.bend  ; take
; D0=raw bytes; consume its four-byte-padded fixture block.
padded	.block
	tst.l d0
	bmi.w invalid
	addq.l #3, d0
	andi.l #$fffffffc, d0
ready
	bsr.w take
	rts
invalid
	moveq #1, d0
	rts
	.bend  ; padded
; A0/D0=output span. D0=status/CCR; others preserved. Exact partial-write loop.
writeSpan	.block
	movem.l d1-d5/a0-a6, -(sp)
	movea.l a0, a3
	move.l d0, d5
next
	tst.l d5
	beq.w valid
	movea.l DosBase, a6
	move.l OutputHandle, d1
	move.l a3, d2
	move.l d5, d3
	jsr DOS_WRITE(a6)
	tst.l d0
	ble.w fail
	cmp.l d5, d0
	bhi.w fail
	adda.l d0, a3
	sub.l d0, d5
	bra.w next
valid
	moveq #0, d0
	bra.w done
fail
	moveq #1, d0
done
	tst.l d0
	movem.l (sp)+, d1-d5/a0-a6
	rts
	.bend  ; writeSpan
	.endsection
.section data, kind=data
DosName	.byte "dos.library", 0
OutputName	.byte "Work:output-spans-results.bin", 0
.align 4
Fixture	.incbin "output-spans-fixture.bin"
FixtureEnd
.endsection
.section bss, kind=bss
.align 4
DosBase	.res long, 1
OutputHandle	.res long, 1
ReturnCode	.res long, 1
Cursor	.res long, 1
ResultData	.res byte, RESULT_BYTES
Owner	.res byte, memory.Block.Used+4
.endsection
.output "output-spans.hunk", format=hunk, sections=entry, code, data, bss
.endmodule

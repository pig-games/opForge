; Fresh owned-capture head roles, compared with the live shared Rust A8 oracle.
; This component does not execute statement bodies or claim CLI parity.
	.module main
	.cpu 68020
	.use opforge.assembler.language.statement_heads as heads
	.use opforge.vm.tkvm.runtime as tokenizer
	.priv
CaseHeader	.struct
Count	.long ?
LexemeBytes	.long ?
	.endstruct
CASE_HEADER_BYTES = CaseHeader.LexemeBytes+4
Result	.struct
Status	.long ?
Role	.long ?
	.endstruct
RESULT_BYTES = Result.Role+4
EXEC_BASE = 4
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
	.pub
	.section entry, kind=code
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
	moveq #4, d0
	bsr.w take
	bne.w bad
	move.l (a0), d7
	moveq #4, d0
	bsr.w take
	bne.w bad
	move.l (a0), d0
	bsr.w padded
	bne.w bad
	move.l a0, Request+heads.Frame.Package
	lea NameCache, a1
	jsr heads.configure
	bne.w bad
	move.l #NameCache, Request+heads.Frame.Names
next
	tst.l d7
	beq.w exhausted
	moveq #CASE_HEADER_BYTES, d0
	bsr.w take
	bne.w bad
	move.l (a0), d6
	move.l CaseHeader.LexemeBytes(a0), d5
	move.l d6, Request+heads.Frame.Count
	mulu.w #tokenizer.TOKEN_RECORD_SIZE, d6
	move.l d6, d0
	bsr.w take
	bne.w bad
	move.l a0, Request+heads.Frame.Tokens
	move.l d5, Request+heads.Frame.LexemeBytes
	move.l d5, d0
	bsr.w padded
	bne.w bad
	move.l a0, Request+heads.Frame.Lexemes
	lea Request, a0
	jsr heads.classify
	move.l d0, ResultData+Result.Status
	move.l d1, ResultData+Result.Role
	lea ResultData, a0
	moveq #RESULT_BYTES, d0
	bsr.w writeSpan
	bne.w bad
	subq.l #1, d7
	bra.w next
exhausted
	move.l #FixtureEnd, d0
	cmp.l Cursor, d0
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #20, d0
	rts
	.bend  ; cases
; D0=nonnegative bytes. A0=bounded fixture span,D0=status/CCR; others preserved.
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
; D0=raw bytes; consume its even-byte-padded fixture block. Same outputs as take.
padded	.block
	tst.l d0
	bmi.w invalid
	btst #0, d0
	beq.w ready
	addq.l #1, d0
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
OutputName	.byte "Work:statement-heads-results.bin", 0
	.align 4
Fixture	.incbin "statement-heads-cases.bin"
FixtureEnd
	.endsection
	.section bss, kind=bss
	.align 4
DosBase	.res long, 1
OutputHandle	.res long, 1
ReturnCode	.res long, 1
Cursor	.res long, 1
Request	.res byte, heads.FRAME_BYTES
NameCache	.res byte, heads.CACHE_BYTES
ResultData	.res byte, RESULT_BYTES
	.endsection
	.output "statement-heads.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule

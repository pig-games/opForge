; Native owned argument-view transport with live Rust facet expectations.
.module main
.cpu 68020
.use experimental.amigaos.binary_statement_tail as tail
.use experimental.amigaos.binary_statement_session as session
.use experimental.amigaos.binary_statement_heads as heads
.use prvm.amigaos.statement_abi as abi
.priv
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
cases .block
 move.l #Fixture,Cursor
 moveq #8,d0
 bsr.w take
 bne.w bad
 move.l (a0),d7
 move.l 4(a0),d0
 bsr.w padded
 bne.w bad
 move.l a0,Request+tail.Frame.Package
 lea Names,a1
 jsr heads.configure
 bne.w bad
next
 tst.l d7
 beq.w valid
 moveq #16,d0
 bsr.w take
 bne.w bad
 move.l (a0),d0
 move.l d0,Source+session.Snapshot.Count
 move.l 4(a0),Source+session.Snapshot.ArenaBytes
 move.l 8(a0),Route+abi.Route.HeadToken
 move.l 12(a0),Route+abi.Route.PrefixBytes
 move.l Source+session.Snapshot.Count,d0
 mulu.w #abi.TOKEN_BYTES,d0
 bsr.w take
 bne.w bad
 move.l a0,Source+session.Snapshot.Views
 move.l Source+session.Snapshot.ArenaBytes,d0
 bsr.w padded
 bne.w bad
 move.l a0,Source+session.Snapshot.Arena
 lea Output,a0
 moveq #session.SNAPSHOT_BYTES/4-1,d0
clearOutput
 clr.l (a0)+
 dbra d0,clearOutput
 lea Request,a0
 lea TailState,a1
 jsr tail.prepare
 move.l d0,Result
 beq.w resultReady
 lea Output,a0
 moveq #session.SNAPSHOT_BYTES/4-1,d0
checkOutput
 tst.l (a0)+
 bne.w bad
 dbra d0,checkOutput
resultReady
 move.l Output+session.Snapshot.Count,Result+4
 move.l Output+session.Snapshot.ArenaBytes,Result+8
 lea Result,a0
 moveq #12,d0
 bsr.w writeSpan
 bne.w bad
 movea.l Output+session.Snapshot.Views,a0
 move.l Output+session.Snapshot.Count,d0
 mulu.w #abi.TOKEN_BYTES,d0
 bsr.w writeSpan
 bne.w bad
 movea.l Output+session.Snapshot.Arena,a0
 move.l Output+session.Snapshot.ArenaBytes,d0
 bsr.w writeSpan
 bne.w bad
 subq.l #1,d7
 bra.w next
valid
 lea TailState,a0
 jsr tail.finish
 moveq #0,d0
 rts
bad
 lea TailState,a0
 jsr tail.finish
 moveq #20,d0
 rts
 .bend
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
	addq.l #3,d0
	andi.l #$fffffffc,d0
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
.section data,kind=data
DosName .byte "dos.library",0
OutputName .byte "Work:stmt-tail-results.bin",0
.align 4
Fixture .incbin "stmt-tail-fixture.bin"
FixtureEnd
Request .long Source,Route,0,Names,Output
Route .long abi.NONE,0,0
.endsection
.section bss,kind=bss
.align 4
DosBase .res long,1
OutputHandle .res long,1
ReturnCode .res long,1
Cursor .res long,1
Names .res byte,heads.CACHE_BYTES
Source .res byte,session.SNAPSHOT_BYTES
Output .res byte,session.SNAPSHOT_BYTES
Result .res long,3
TailState .res byte,tail.STATE_BYTES
.endsection
.output "stmt-tail.hunk",format=hunk,sections=entry,code,data,bss
.endmodule

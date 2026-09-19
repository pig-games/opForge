; Dedicated bounded passive accounting, imported only by gated macros.
; @opforge-owner: debug.amigaos.memory_profile
	.module debug.amigaos.memory_profile
	.cpu 68020
Fields	.struct
Magic	.long ?
Live	.long ?
Peak	.long ?
Allocated	.long ?
Freed	.long ?
PreparedLive	.long ?
PreparedFreed	.long ?
FreeAtEntry	.long ?
LargestAtEntry	.long ?
ExecVersion	.long ?
AssemblyLive	.long ?
CleanupLive	.long ?
DosVersion	.long ?
RuntimeBytes	.long ?
RecordBytes	.long ?
SourceBytes	.long ?
Compiled	.long ?
Evaluated	.long ?
ProgramBytes	.long ?
Clocks	.res 9*4
Frequency	.long ?
Error	.long ?
Elapsed	.res 6*8
Entries	.res 6*4
Opcodes	.res 19*4
Pairs	.res 19*19*4
TokenWork	.res 7*4
TokenElapsed	.res 2*8
	.endstruct
	.section data, kind=data
	.priv
Path	.byte "Work:memory.bin", 0
TimerName	.byte "timer.device", 0
	.endsection
	.section bss, kind=bss
	.align 4
Record	.res byte, 1756
Port	.res long, 1
Request	.res long, 1
Timer	.res long, 1
Active	.res long, 1
Current	.res long, 1
Previous	.res long, 2
Stamp	.res long, 2
PreviousOpcode	.res long, 1
ScopeActive	.res long, 2
ScopeStamp	.res long, 4
	.endsection
	.section code, kind=code
	.pub
; D0=actual allocation capacity. All registers/CCR preserved; no request storage.
allocate	.block
	move.w ccr, -(sp)
	movem.l d0-d1/a0, -(sp)
	lea Record, a0
	add.l d0, Fields.Live(a0)
	add.l d0, Fields.Allocated(a0)
	move.l Fields.Live(a0), d1
	cmp.l Fields.Peak(a0), d1
	bls.w done
	move.l d1, Fields.Peak(a0)
done
	movem.l (sp)+, d0-d1/a0
	move.w (sp)+, ccr
	rts
	.bend  ; allocate
; D0=freed capacity. Same passive ABI.
release	.block
	move.w ccr, -(sp)
	move.l a0, -(sp)
	lea Record, a0
	sub.l d0, Fields.Live(a0)
	add.l d0, Fields.Freed(a0)
	movea.l (sp)+, a0
	move.w (sp)+, ccr
	rts
	.bend  ; release
; D0=0 start,1 prepared/reclaimed,2 assembled,3 cleaned. Passive ABI.
phase	.block
	move.w ccr, -(sp)
	movem.l d0-d2/a0-a2/a6, -(sp)
	lea Record, a2
	tst.l d0
	bne.w later
	move.l #$4d454d35, Fields.Magic(a2)
	movea.l 4.w, a6
	moveq #0, d0
	move.w 20(a6), d0
	move.l d0, Fields.ExecVersion(a2)
	moveq #0, d1
	jsr -216(a6)
	move.l d0, Fields.FreeAtEntry(a2)
	move.l #$20000, d1
	jsr -216(a6)
	move.l d0, Fields.LargestAtEntry(a2)
	bra.w done
later
	cmpi.l #1, d0
	bne.w assembled
	move.l Fields.Live(a2), Fields.PreparedLive(a2)
	move.l Fields.Freed(a2), Fields.PreparedFreed(a2)
	bra.w done
assembled
	cmpi.l #2, d0
	bne.w cleaned
	move.l Fields.Live(a2), Fields.AssemblyLive(a2)
	bra.w done
cleaned
	move.l Fields.Live(a2), Fields.CleanupLive(a2)
done
	movem.l (sp)+, d0-d2/a0-a2/a6
	move.w (sp)+, ccr
	rts
	.bend  ; phase
; A0=dos.library. Export only the dedicated immutable terminal record.
; All registers/CCR preserved. Missing/partial telemetry fails host validation.
save	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	move.l a0, -(sp)
	bsr.w closeTimer
	movea.l (sp)+, a6
	lea Record, a0
	moveq #0, d0
	move.w 20(a6), d0
	move.l d0, Fields.DosVersion(a0)
	move.l #Path, d1
	move.l #1006, d2
	jsr -30(a6)
	tst.l d0
	beq.w done
	move.l d0, d4
	move.l d4, d1
	move.l #Record, d2
	move.l #1756, d3
	jsr -48(a6)
	move.l d4, d1
	jsr -36(a6)
done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; save
; D0=runtime prefix bytes,D1=packed bytes,D2=source bytes. Passive ABI.
layout	.block
	move.w ccr, -(sp)
	move.l a0, -(sp)
	lea Record, a0
	move.l d0, Fields.RuntimeBytes(a0)
	move.l d1, Fields.RecordBytes(a0)
	move.l d2, Fields.SourceBytes(a0)
	movea.l (sp)+, a0
	move.w (sp)+, ccr
	rts
	.bend  ; layout
; D0=work counter 0..2, D1=amount. Passive ABI, bounded dedicated storage.
work	.block
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	cmpi.l #2, d0
	bhi.w done
	lsl.l #2, d0
	lea Record, a0
	lea Fields.Compiled(a0), a0
	adda.l d0, a0
	add.l d1, (a0)
done
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
	rts
	.bend  ; work
; A0=dos.library, D0=phase 0..2. Save DOS DateStamp (50 ticks/s), separately
; from uninstrumented START-to-DONE timing. All registers/CCR preserved.
clock	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	cmpi.l #2, d0
	bhi.w done
	move.l d0, d7
	movea.l a0, a5
	tst.l d7
	bne.w finish
	bsr.w openTimer
	bra.w stamp
finish
	cmpi.l #1, d7
	bne.w stamp
	moveq #0, d0
	bsr.w stage
	clr.l Active
stamp
	movea.l a5, a6
	move.l d7, d0
	mulu.w #12, d0
	lea Record, a0
	lea Fields.Clocks(a0), a0
	adda.l d0, a0
	move.l a0, d1
	jsr -192(a6)
done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; clock
; D0=new stage 0..5. Exclusive, non-nesting; passive ABI.
; Disabled after preparation clock 1, so assembly cannot enter these totals.
stage	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	tst.l Active
	beq.w done
	lea Record, a2
	cmpi.l #5, d0
	bhi.w invalid
	move.l d0, d7
	movea.l Timer, a6
	lea Stamp, a0
	jsr -60(a6)
	cmp.l Fields.Frequency(a2), d0
	bne.w frequency
	lea Stamp, a0
	lea Previous, a1
	move.l (a0), d2
	move.l 4(a0), d3
	sub.l 4(a1), d3
	move.l (a1), d4
	subx.l d4, d2
	bcs.w overflow
	move.l (a0), (a1)
	move.l 4(a0), 4(a1)
	move.l Current, d0
	lsl.l #3, d0
	lea Fields.Elapsed(a2), a0
	adda.l d0, a0
	add.l d3, 4(a0)
	move.l (a0), d4
	addx.l d2, d4
	bcs.w overflow
	move.l d4, (a0)
	move.l d7, Current
	lsl.l #2, d7
	lea Fields.Entries(a2), a0
	adda.l d7, a0
	addq.l #1, (a0)
	bcs.w overflow
	bra.w done
invalid
	ori.l #2, Fields.Error(a2)
	bra.w stop
frequency
	ori.l #4, Fields.Error(a2)
	bra.w stop
overflow
	ori.l #8, Fields.Error(a2)
stop
	clr.l Active
done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; stage
; D0=source-line bytes. Reset adjacency at each VM invocation. Passive ABI.
tokenBegin	.block
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	move.l #-1, PreviousOpcode
	move.l d0, d1
	moveq #0, d0
	bsr.w tokenWork
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
	rts
	.bend  ; tokenBegin
; D0=dense opcode 0..18. Count ordered pairs within an invocation only.
tokenOpcode	.block
	move.w ccr, -(sp)
	movem.l d0-d1/a0-a1, -(sp)
	lea Record, a0
	cmpi.l #18, d0
	bhi.w invalid
	move.l d0, d1
	lsl.l #2, d1
	lea Fields.Opcodes(a0), a1
	adda.l d1, a1
	addq.l #1, (a1)
	bcs.w overflow
	move.l PreviousOpcode, d1
	move.l d0, PreviousOpcode
	tst.l d1
	bmi.w done
	mulu.w #19, d1
	add.l d0, d1
	lsl.l #2, d1
	lea Fields.Pairs(a0), a1
	adda.l d1, a1
	addq.l #1, (a1)
	bcs.w overflow
	bra.w done
invalid
	move.l #-1, PreviousOpcode
	bra.w done
overflow
	ori.l #8, Fields.Error(a0)
done
	movem.l (sp)+, d0-d1/a0-a1
	move.w (sp)+, ccr
	rts
	.bend  ; tokenOpcode
; D0=work index 0..6, D1=amount. Passive ABI.
; Line bytes, committed tokens/lexemes, source reads, taken EOL/byte/class.
tokenWork	.block
	move.w ccr, -(sp)
	movem.l d0/a0-a1, -(sp)
	lea Record, a0
	cmpi.l #6, d0
	bhi.w invalid
	lsl.l #2, d0
	lea Fields.TokenWork(a0), a1
	adda.l d0, a1
	add.l d1, (a1)
	bcs.w overflow
	bra.w done
invalid
	ori.l #2, Fields.Error(a0)
	bra.w done
overflow
	ori.l #8, Fields.Error(a0)
done
	movem.l (sp)+, d0/a0-a1
	move.w (sp)+, ccr
	rts
	.bend  ; tokenWork
; D0=scope 0 scanner+commit,1 commit only. Different scopes may nest.
; Timing includes probe overhead and is separate from exclusive stage totals.
tokenScopeBegin	.block
	move.w ccr, -(sp)
	movem.l d0-d4/d7/a0-a3/a6, -(sp)
	lea Record, a2
	cmpi.l #1, d0
	bhi.w invalid
	tst.l Active
	beq.w done
	move.l d0, d7
	lsl.l #2, d0
	lea ScopeActive, a3
	adda.l d0, a3
	tst.l (a3)
	bne.w mismatch
	move.l #1, (a3)
	lsl.l #3, d7
	lea ScopeStamp, a0
	adda.l d7, a0
	movea.l Timer, a6
	jsr -60(a6)
	cmp.l Fields.Frequency(a2), d0
	bne.w frequency
	bra.w done
invalid
	ori.l #2, Fields.Error(a2)
	bra.w done
mismatch
	ori.l #32, Fields.Error(a2)
	bra.w done
frequency
	ori.l #4, Fields.Error(a2)
done
	movem.l (sp)+, d0-d4/d7/a0-a3/a6
	move.w (sp)+, ccr
	rts
	.bend  ; tokenScopeBegin
tokenScopeEnd	.block
	move.w ccr, -(sp)
	movem.l d0-d4/d7/a0-a3/a6, -(sp)
	lea Record, a2
	cmpi.l #1, d0
	bhi.w invalid
	tst.l Active
	beq.w done
	move.l d0, d7
	lsl.l #2, d0
	lea ScopeActive, a3
	adda.l d0, a3
	tst.l (a3)
	beq.w mismatch
	clr.l (a3)
	lea Stamp, a0
	movea.l Timer, a6
	jsr -60(a6)
	cmp.l Fields.Frequency(a2), d0
	bne.w frequency
	lsl.l #3, d7
	lea ScopeStamp, a1
	adda.l d7, a1
	lea Stamp, a0
	move.l (a0), d2
	move.l 4(a0), d3
	sub.l 4(a1), d3
	move.l (a1), d4
	subx.l d4, d2
	bcs.w overflow
	lea Fields.TokenElapsed(a2), a0
	adda.l d7, a0
	add.l d3, 4(a0)
	move.l (a0), d4
	addx.l d2, d4
	bcs.w overflow
	move.l d4, (a0)
	bra.w done
invalid
	ori.l #2, Fields.Error(a2)
	bra.w done
mismatch
	ori.l #32, Fields.Error(a2)
	bra.w done
frequency
	ori.l #4, Fields.Error(a2)
	bra.w done
overflow
	ori.l #8, Fields.Error(a2)
done
	movem.l (sp)+, d0-d4/d7/a0-a3/a6
	move.w (sp)+, ccr
	rts
	.bend  ; tokenScopeEnd
; D0=scope 0..1. Close only if active; passive ABI, safe at common returns.
tokenScopeClose	.block
	move.w ccr, -(sp)
	movem.l d0-d1/a0, -(sp)
	cmpi.l #1, d0
	bhi.w invalid
	move.l d0, d1
	lsl.l #2, d1
	lea ScopeActive, a0
	adda.l d1, a0
	tst.l (a0)
	beq.w done
	bsr.w tokenScopeEnd
	bra.w done
invalid
	lea Record, a0
	ori.l #2, Fields.Error(a0)
done
	movem.l (sp)+, d0-d1/a0
	move.w (sp)+, ccr
	rts
	.bend  ; tokenScopeClose
	.priv
; No I/O requests are submitted. Private helpers clobber D0-D1/A0-A2/A6/CCR.
; Exec V36 port/request vectors; timer ReadEClock is the V36 -60 vector.
openTimer	.block
	movea.l 4.w, a6
	jsr -666(a6)
	move.l d0, Port
	beq.w failed
	movea.l d0, a0
	moveq #40, d0
	jsr -654(a6)
	move.l d0, Request
	beq.w failed
	movea.l d0, a1
	lea TimerName, a0
	moveq #2, d0
	moveq #0, d1
	jsr -444(a6)
	tst.l d0
	bne.w failed
	movea.l Request, a0
	move.l 20(a0), Timer
	movea.l Timer, a6
	lea Previous, a0
	jsr -60(a6)
	lea Record, a2
	move.l d0, Fields.Frequency(a2)
	beq.w failed
	clr.l Current
	move.l #1, Active
	move.l #1, Fields.Entries(a2)
	rts
failed
	lea Record, a2
	ori.l #1, Fields.Error(a2)
	rts
	.bend  ; openTimer
closeTimer	.block
	lea Record, a2
	tst.l Active
	beq.w device
	ori.l #16, Fields.Error(a2)
	clr.l Active
device
	lea ScopeActive, a0
	move.l (a0), d0
	or.l 4(a0), d0
	beq.w scopesClosed
	ori.l #32, Fields.Error(a2)
scopesClosed
	movea.l 4.w, a6
	tst.l Timer
	beq.w freeRequest
	movea.l Request, a1
	jsr -450(a6)
	clr.l Timer
freeRequest
	tst.l Request
	beq.w freePort
	movea.l Request, a0
	jsr -660(a6)
	clr.l Request
freePort
	tst.l Port
	beq.w done
	movea.l Port, a0
	jsr -672(a6)
	clr.l Port
done
	rts
	.bend  ; closeTimer
	.endsection
	.endmodule

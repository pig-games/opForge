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
	.endstruct
	.section data, kind=data
	.priv
Path	.byte "Work:memory.bin", 0
	.endsection
	.section bss, kind=bss
	.align 4
Record	.res long, 16
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
	move.l #$4d454d32, Fields.Magic(a2)
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
	movea.l a0, a6
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
	moveq #64, d3
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
	.endsection
	.endmodule

; First preparation completion failure. Imported only under all three gates.
; @opforge-owner: debug.amigaos.binding_diagnostic
	.module debug.amigaos.binding_diagnostic
	.cpu 68020
	.use debug.amigaos.memory_profile as memory_profile
	.pub
CURRENT = 1
IMPORTS = 2
SECTIONS = 3
OUTPUTS = 4
EXPLICIT = 5
UNRESOLVED = 6
VISIBILITY = 7
REMAP = 8
IMPORT_MODULE = 9
SELECTED_MODULE = 10
IMPORT_SELECTION = 11
SELECTED_SELECTION = 12
PROXY = 13
TARGET_DECLARATION = 14
; Numeric entry bytes are copied without interpreting binding semantics.
; No preparation owner is imported by this observer.
; Word offsets into the existing owned scope/entry layout. Each diagnostic
; caller emits this descriptor under the same gates, using its actual types.
View	.struct
Count	.word ?
Base	.word ?
Current	.word ?
Entries	.word ?
Arena	.word ?
ArenaUsed	.word ?
EntryBytes	.word ?
Name	.word ?
Length	.word ?
.endstruct
Snapshot	.struct
Stage	.long ?
Index	.long ?
Related	.long ?
Base	.word ?
Count	.word ?
Current	.long ?
Entry	.res 28
NameBytes	.long ?
Name	.res 256
.endstruct
SNAPSHOT_BYTES = Snapshot.Name+256
PendingCheck	.struct
Stage	.long ?
Index	.long ?
Related	.long ?
.endstruct
	.section code, kind=code
; No inputs/results. Registers, CCR and stack preserved. Begin one completion.
clear	.block
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	lea Record, a0
	move.w #SNAPSHOT_BYTES/4-1, d0
loop
	clr.l (a0)+
	dbra d0, loop
	lea Pending, a0
	clr.l PendingCheck.Stage(a0)
	clr.l PendingCheck.Index(a0)
	clr.l PendingCheck.Related(a0)
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
	rts
	.bend  ; clear
; D0=stage,D1=entry,D2=related. Mark the check about to execute, without
; copying source data. All registers/CCR preserved; fixed twelve-byte storage.
attempt	.block
	move.w ccr, -(sp)
	move.l a0, -(sp)
	lea Record, a0
	tst.l Snapshot.Stage(a0)
	bne.w done
	lea Pending, a0
	move.l d0, PendingCheck.Stage(a0)
	move.l d1, PendingCheck.Index(a0)
	move.l d2, PendingCheck.Related(a0)
done
	movea.l (sp)+, a0
	move.w (sp)+, ccr
	rts
	.bend  ; attempt
; D0=stage,D1=canonical source ID,D2=source base. Mark the canonical target
; after a successful bind, retaining the previous origin index as Related.
; All registers/CCR/stack preserved. The completed binder owns ID validity.
attemptTarget	.block
	move.w ccr, -(sp)
	movem.l d1/a0, -(sp)
	lea Record, a0
	tst.l Snapshot.Stage(a0)
	bne.w done
	lea Pending, a0
	move.l PendingCheck.Index(a0), PendingCheck.Related(a0)
	sub.l d2, d1
	move.l d1, PendingCheck.Index(a0)
	move.l d0, PendingCheck.Stage(a0)
done
	movem.l (sp)+, d1/a0
	move.w (sp)+, ccr
	rts
	.bend  ; attemptTarget
; D0=status,A0=scope state,A1=View descriptor. Commit the pending check only
; for actual failure. First actual failure wins. Registers/CCR/stack preserved;
; no allocation or I/O. Entry/name views are bounded by count/owned arena extent.
commit	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	tst.l d0
	beq.w done
	movea.l a0, a5
	movea.l a1, a4
	lea Record, a6
	tst.l Snapshot.Stage(a6)
	bne.w done
	lea Pending, a0
	move.l PendingCheck.Stage(a0), d0
	beq.w done
	move.l d0, Snapshot.Stage(a6)
	move.l PendingCheck.Index(a0), d1
	move.l d1, Snapshot.Index(a6)
	move.l PendingCheck.Related(a0), Snapshot.Related(a6)
	moveq #0, d0
	move.w View.Base(a4), d0
	move.w 0(a5, d0.l), Snapshot.Base(a6)
	move.w View.Count(a4), d0
	moveq #0, d3
	move.w 0(a5, d0.l), d3
	move.w d3, Snapshot.Count(a6)
	move.w View.Current(a4), d0
	moveq #0, d2
	move.w 0(a5, d0.l), d2
	move.l d2, Snapshot.Current(a6)
	cmp.l d3, d1
	bhs.w done
	moveq #0, d6
	move.w View.EntryBytes(a4), d6
	beq.w done
	cmpi.l #28, d6
	bhi.w done
	move.l d1, d0
	mulu.w d6, d0
	move.l d0, d2
	moveq #0, d0
	move.w View.Entries(a4), d0
	movea.l 0(a5, d0.l), a0
	move.l a0, d0
	beq.w done
	adda.l d2, a0
	movea.l a0, a3
	lea Snapshot.Entry(a6), a2
entryLoop
	move.b (a0)+, (a2)+
	subq.l #1, d6
	bne.w entryLoop
	moveq #0, d0
	move.w View.Name(a4), d0
	move.l 0(a3, d0.l), d2
	move.w View.Length(a4), d0
	moveq #0, d7
	move.w 0(a3, d0.l), d7
	cmpi.l #255, d7
	bhi.w done
	move.l d2, d3
	add.l d7, d3
	bcs.w done
	move.w View.ArenaUsed(a4), d0
	cmp.l 0(a5, d0.l), d3
	bhi.w done
	move.w View.Arena(a4), d0
	movea.l 0(a5, d0.l), a0
	move.l a0, d0
	beq.w done
	adda.l d2, a0
	move.l d7, Snapshot.NameBytes(a6)
	lea Snapshot.Name(a6), a1
	tst.l d7
	beq.w done
nameLoop
	move.b (a0)+, (a1)+
	subq.l #1, d7
	bne.w nameLoop
done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; commit
	.pub
; A0=dos.library. Emit five numeric rows then 22 zero-padded name rows through
; the established progress ABI. Phases32..36 contain metadata;64..85 contain
; successive twelve-byte canonical-name chunks (last row contains four bytes).
; All registers, CCR and stack preserved. No output if no failure was latched.
report	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	movea.l a0, a6
	lea Record, a5
	tst.l Snapshot.Stage(a5)
	beq.w done
	moveq #32, d0
	move.l Snapshot.Stage(a5), d1
	move.l Snapshot.Index(a5), d2
	move.l Snapshot.Related(a5), d3
	movea.l a6, a0
	jsr memory_profile.progress
	moveq #33, d0
	move.l Snapshot.Base(a5), d1
	move.l Snapshot.Current(a5), d2
	move.l Snapshot.NameBytes(a5), d3
	movea.l a6, a0
	jsr memory_profile.progress
	lea Snapshot.Entry(a5), a4
	moveq #34, d7
	moveq #1, d6
metadataLoop
	move.l d7, d0
	move.l (a4)+, d1
	move.l (a4)+, d2
	move.l (a4)+, d3
	movea.l a6, a0
	jsr memory_profile.progress
	addq.l #1, d7
	dbra d6, metadataLoop
	moveq #36, d0
	move.l (a4), d1
	moveq #0, d2
	moveq #0, d3
	movea.l a6, a0
	jsr memory_profile.progress
	lea Snapshot.Name(a5), a4
	moveq #64, d7
	moveq #20, d6
nameLoop
	move.l d7, d0
	move.l (a4)+, d1
	move.l (a4)+, d2
	move.l (a4)+, d3
	movea.l a6, a0
	jsr memory_profile.progress
	addq.l #1, d7
	dbra d6, nameLoop
	moveq #85, d0
	move.l (a4), d1
	moveq #0, d2
	moveq #0, d3
	movea.l a6, a0
	jsr memory_profile.progress
done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; report
	.endsection
	.section bss, kind=bss
	.align 4
	.priv
Record	.res byte, SNAPSHOT_BYTES
Pending	.res byte, PendingCheck.Related+4
	.endsection
	.endmodule

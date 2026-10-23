; Executed global immutable owners, independent of numeric value availability.
; Ordinary-loop candidates remain unknown until a complete source sweep ends.
; @opforge-owner: opforge.assembler.language.decl_activity
	.module opforge.assembler.language.decl_activity
	.cpu 68020
	.use opforge.platform.memory as memory
	.use opforge.packages.format as pkg
	.use opforge.assembler.values.values as values
	.use opforge.assembler.language.repeat_inventory as inventory
	.use opforge.assembler.source.records as source
	.pub
ACTIVE = 1
INACTIVE = 2
UNKNOWN = 3
UNKNOWN_OFFSET = -1
OFFSET_SHIFT = 2
State	.struct
Current	.res memory.Block.Used+4
Published	.res memory.Block.Used+4
Inventory	.long ?  ; borrowed preparation identity owner, never a record pointer
Context	.long ?
Count	.long ?
Changed	.word ?
Pending	.word ?
Open	.word ?
Reserved	.word ?
.endstruct
STATE_BYTES = State.Reserved+2
	.section code, kind=code
; A0=state,A1=inventory,A2=context. D0/CCR=status; others preserved.
; Allocate two bounded numeric maps only for ordinary-loop global candidates.
; Published uses -1 unknown,0 inactive,record offset+1 active. Current is a
; per-sweep execution claim, not a value cell. Release is required on any exit.
prepare	.block
	movem.l d1-d4/a0-a4, -(sp)
	movea.l a0, a4
	move.l a1, State.Inventory(a4)
	move.l a2, State.Context(a4)
	clr.l State.Count(a4)
	clr.w State.Changed(a4)
	clr.w State.Pending(a4)
	clr.w State.Open(a4)
	movea.l pkg.Context.Owner(a2), a3
	move.l a3, d0
	bne.w ownerReady
	tst.l inventory.State.DynamicGlobals(a1)
	beq.w good
	bra.w bad
ownerReady
	clr.l values.Owner.DeclarationActivity(a3)
	tst.l inventory.State.DynamicGlobals(a1)
	beq.w good
	move.l pkg.Context.Count(a2), d4
	beq.w bad
	cmpi.l #65536, d4
	bhi.w bad
	cmp.l inventory.State.Symbols(a1), d4
	bne.w bad
	move.l d4, d0
	lsl.l #OFFSET_SHIFT, d0
	move.l d0, d3
	lea State.Current(a4), a0
	jsr memory.reserveExact
	bne.w done
	move.l d3, memory.Block.Used(a0)
	lea State.Published(a4), a0
	move.l d3, d0
	jsr memory.reserveExact
	bne.w done
	move.l d3, memory.Block.Used(a0)
	movea.l State.Published+memory.Block.Pointer(a4), a0
	movea.l State.Current+memory.Block.Pointer(a4), a1
	move.l d4, d1
clear
	move.l #UNKNOWN_OFFSET, (a0)+
	clr.l (a1)+
	subq.l #1, d1
	bne.w clear
	move.l d4, State.Count(a4)
	move.l a4, values.Owner.DeclarationActivity(a3)
good
	moveq #0, d0
	bra.w done
bad
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1-d4/a0-a4
	tst.l d0
	rts
	.bend  ; prepare

; A0=state. Begin sizing/emission observations; completed owners remain live.
; D0/CCR=status; others preserved. Independent sweeps reset execution claims.
beginPass	.block
	clr.w State.Changed(a0)
	clr.w State.Pending(a0)
	moveq #0, d0
	tst.w State.Open(a0)
	beq.w done
	moveq #values.MALFORMED, d0
done
	tst.l d0
	rts
	.bend  ; beginPass

; A0=state. D0/CCR=status; others preserved. No allocation on replay.
beginSweep	.block
	movem.l d1/a1, -(sp)
	tst.w State.Open(a0)
	bne.w bad
	move.l State.Count(a0), d1
	beq.w good
	movea.l State.Current+memory.Block.Pointer(a0), a1
clear
	clr.l (a1)+
	subq.l #1, d1
	bne.w clear
	move.w #1, State.Open(a0)
good
	moveq #0, d0
	bra.w done
bad
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1/a1
	tst.l d0
	rts
	.bend  ; beginSweep

; A0=bounded active record,A1=end,A2=context,A3=state. D0/CCR=status;
; others preserved. Call after shared branch/loop selection, before RHS work.
; Immutable execution is claimed even when its value is still unavailable.
claim	.block
	movem.l d1-d4/a0-a5, -(sp)
	movea.l a3, a5
	tst.l State.Count(a5)
	beq.w good
	move.l a1, d0
	sub.l a0, d0
	bcs.w bad
	beq.w bad
	moveq #0, d3
	move.b inventory.PackedRecord.Length(a0), d3
	addq.w #1, d3
	cmp.l d0, d3
	bhi.w bad
	move.l d3, d0
	cmpi.l #inventory.PackedRecord.Rhs, d0
	blo.w good
	cmpi.b #1, inventory.PackedRecord.NameKind(a0)
	bhi.w good
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, inventory.PackedRecord.Marker(a0)
	bne.w good
	moveq #0, d1
	move.w inventory.PackedRecord.NameId(a0), d1
	movea.l a0, a4
	movea.l a5, a0
	bsr.w tracked
	cmpi.l #values.BOUNDS, d0
	beq.w good
	tst.l d0
	bne.w done
	tst.w State.Open(a5)
	beq.w bad
	movea.l State.Inventory(a5), a0
	move.l a4, d2
	sub.l inventory.State.SourceBase(a0), d2
	bcs.w bad
	cmpi.l #memory.LIMIT, d2
	bhs.w bad
	addq.l #1, d2
	move.l d1, d3
	lsl.l #OFFSET_SHIFT, d3
	movea.l State.Current+memory.Block.Pointer(a5), a0
	tst.l 0(a0, d3.l)
	bne.w bad  ; two actual immutable executions, including one repeated record
	move.l d2, 0(a0, d3.l)
good
	moveq #0, d0
	bra.w done
bad
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1-d4/a0-a5
	tst.l d0
	rts
	.bend  ; claim

; A0=state,D0=control uncertainty (0 resolved,nonzero deferred). Publish only
; a complete source sweep. D0/CCR=status; other registers preserved.
; Inactive alternatives do not overwrite an executed same-ID declaration.
; Mapped sweeps are independent executions; Changed accumulates for the pass.
endSweep	.block
	movem.l d1-d4/a0-a4, -(sp)
	movea.l a0, a4
	move.l d0, d4
	tst.l State.Count(a4)
	beq.w good
	tst.w State.Open(a4)
	beq.w bad
	tst.l d4
	beq.w controlsReady
	move.w #1, State.Pending(a4)  ; strict replay even without a binding query
controlsReady
	movea.l State.Current+memory.Block.Pointer(a4), a2
	movea.l State.Published+memory.Block.Pointer(a4), a3
	moveq #0, d1
next
	cmp.l State.Count(a4), d1
	bhs.w complete
	movea.l a4, a0
	bsr.w tracked
	cmpi.l #values.BOUNDS, d0
	beq.w advance
	tst.l d0
	bne.w done
	move.l d1, d2
	lsl.l #OFFSET_SHIFT, d2
	move.l 0(a2, d2.l), d3
	bne.w observed
	tst.l d4
	beq.w observed
	moveq #UNKNOWN_OFFSET, d3  ; unresolved controls cannot prove nonexecution
observed
	cmp.l 0(a3, d2.l), d3
	beq.w advance
	move.l d3, 0(a3, d2.l)
	move.w #1, State.Changed(a4)
advance
	addq.l #1, d1
	bra.w next
complete
	clr.w State.Open(a4)
good
	moveq #0, d0
	bra.w done
bad
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1-d4/a0-a4
	tst.l d0
	rts
	.bend  ; endSweep

; A0=state. D0/CCR=status,D1=changed/unfinished ownership (0/1); others kept.
; Sizing must replay before allocation. Emission must reproduce stable owners.
endPass	.block
	moveq #0, d1
	move.w State.Changed(a0), d1
	or.w State.Pending(a0), d1
	moveq #0, d0
	tst.w State.Open(a0)
	beq.w done
	moveq #values.MALFORMED, d0
done
	tst.l d0
	rts
	.bend  ; endPass

; A0=values Owner,D1=canonical ID. D0/CCR=status,D2=activity; others kept.
; IDs outside the controlled inventory retain ordinary declaration semantics.
; An unknown longer declaration defers binding; only INACTIVE permits fallback.
declaration	.block
	movem.l d1/d3/a0-a2, -(sp)
	moveq #ACTIVE, d2
	cmp.l values.Owner.Count(a0), d1
	bhs.w bad
	movea.l values.Owner.DeclarationActivity(a0), a0
	move.l a0, d0
	beq.w good
	bsr.w tracked
	cmpi.l #values.BOUNDS, d0
	beq.w good
	tst.l d0
	bne.w done
	move.l d1, d3
	lsl.l #OFFSET_SHIFT, d3
	movea.l State.Current+memory.Block.Pointer(a0), a1
	tst.l 0(a1, d3.l)
	bne.w good  ; an executed owner is already known in this source sweep
	movea.l State.Published+memory.Block.Pointer(a0), a1
	move.l 0(a1, d3.l), d3
	bmi.w unobserved
	beq.w absent
	bra.w good
unobserved
	move.w #1, State.Pending(a0)
	moveq #UNKNOWN, d2
	bra.w good
absent
	moveq #INACTIVE, d2
good
	moveq #0, d0
	bra.w done
bad
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1/d3/a0-a2
	tst.l d0
	rts
	.bend  ; declaration

; A0=state. Release owned maps and detach the borrowed values-owner hook.
; All registers preserved; CCR reflects caller D0. Safe after partial prepare.
release	.block
	movem.l d1/a0-a2, -(sp)
	movea.l a0, a2
	move.l State.Context(a2), d1
	beq.w maps
	movea.l d1, a1
	movea.l pkg.Context.Owner(a1), a1
	move.l a1, d1
	beq.w maps
	cmpa.l values.Owner.DeclarationActivity(a1), a2
	bne.w maps
	clr.l values.Owner.DeclarationActivity(a1)
maps
	lea State.Current(a2), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea State.Published(a2), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	clr.l State.Inventory(a2)
	clr.l State.Context(a2)
	clr.l State.Count(a2)
	clr.w State.Open(a2)
	movem.l (sp)+, d1/a0-a2
	tst.l d0
	rts
	.bend  ; release
	.priv
; A0=state,D1=ID. D0/CCR=OK tracked,BOUNDS ordinary,MALFORMED invalid.
; Other registers kept. Preparation owns kind/lexical-loop identity permanently.
tracked	.block
	movem.l d2/a1, -(sp)
	cmp.l State.Count(a0), d1
	bhs.w bad
	movea.l State.Inventory(a0), a1
	movea.l inventory.State.Declarations+memory.Block.Pointer(a1), a1
	move.l d1, d2
	lsl.l #inventory.DECL_SHIFT, d2
	tst.w inventory.Decl.Owner(a1, d2.l)
	bne.w ordinary
	move.w inventory.Decl.Kind(a1, d2.l), d2
	andi.w #inventory.LOOP_CONTROLLED, d2
	beq.w ordinary
	moveq #0, d0
	bra.w done
ordinary
	moveq #values.BOUNDS, d0
	bra.w done
bad
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d2/a1
	tst.l d0
	rts
	.bend  ; tracked
	.endsection
	.endmodule

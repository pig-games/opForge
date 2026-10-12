; Sparse target settings and a borrowed effective view. Only Config.Bins and
; each Target.Settings.Bins own storage; Config.Effective must never be released.
; A1=Config,D0=canonical target. Return A1=settings,D0/CCR=status.
; Preserve A0 (event), A3 (frame), D6 (Config), and D7 (root-file flag).
acquireSettings	.block
	movem.l d2-d5/a0/a2, -(sp)
	tst.w d0
	beq.w foundDefault
	move.w d0, d5
	movea.l Config.Targets+memory.Block.Pointer(a1), a2
	move.l Config.TargetCount(a1), d3
search
	tst.l d3
	beq.w allocate
	cmp.w Target.Id(a2), d5
	beq.w found
	adda.w #TARGET_BYTES, a2
	subq.l #1, d3
	bra.w search
allocate
	lea Config.Targets(a1), a0
	move.l memory.Block.Used(a0), d2
	move.l d2, d0
	addi.l #TARGET_BYTES, d0
	bcs.w bad
	move.l #$7fffffff, d1
	jsr memory.reserveBounded
	bne.w bad
	movea.l memory.Block.Pointer(a0), a2
	adda.l d2, a2
	movea.l a2, a0
	move.l #TARGET_BYTES/4-1, d3
zero
	clr.l (a0)+
	dbra d3, zero
	move.w d5, Target.Id(a2)
	addi.l #TARGET_BYTES, Config.Targets+memory.Block.Used(a1)
	addq.l #1, Config.TargetCount(a1)
found
	lea Target.Settings(a2), a1
foundDefault
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d5/a0/a2
	tst.l d0
	rts
	.bend  ; acquireSettings

	.pub
; A0=Config (optional). D0/CCR=status; other registers preserved.
; Only logical root completion calls this; physical include boundaries inherit.
complete	.block
	moveq #0, d0
	cmpa.l #0, a0
	beq.w done
	tst.w Config.Mode(a0)
	beq.w done
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; complete

; A0=Config. A0=borrowed effective Settings; all other registers preserved.
; Missing target fields inherit defaults. A target BIN vector replaces defaults.
select	.block
	movem.l d0-d2/a1-a3, -(sp)
	movea.l a0, a3
	lea Config.Effective(a3), a1
	movea.l a3, a2
	move.l #SETTINGS_BYTES/4-1, d0
copy
	move.l (a2)+, (a1)+
	dbra d0, copy
	movea.l Config.Targets+memory.Block.Pointer(a3), a2
	move.l Config.TargetCount(a3), d0
search
	tst.l d0
	beq.w done
	move.w Config.SelectedTarget(a3), d1
	cmp.w Target.Id(a2), d1
	beq.w merge
	adda.w #TARGET_BYTES, a2
	subq.l #1, d0
	bra.w search
merge
	lea Target.Settings(a2), a2
	lea Config.Effective(a3), a1
	tst.w Config.NameSet(a2)
	beq.w hex
	move.w #1, Config.NameSet(a1)
	lea Config.Name(a2), a0
	lea Config.Name(a1), a1
	move.w #PATH_BYTES/4-1, d0
nameCopy
	move.l (a0)+, (a1)+
	dbra d0, nameCopy
hex
	lea Config.Effective(a3), a1
	tst.w Config.HexSet(a2)
	beq.w fill
	move.w #1, Config.HexSet(a1)
	lea Config.Hex(a2), a0
	lea Config.Hex(a1), a1
	move.w #PATH_BYTES/4-1, d0
hexCopy
	move.l (a0)+, (a1)+
	dbra d0, hexCopy
fill
	lea Config.Effective(a3), a1
	tst.w Config.FillSet(a2)
	beq.w binary
	move.w #1, Config.FillSet(a1)
	move.l Config.Fill(a2), Config.Fill(a1)
binary
	tst.l Config.BinCount(a2)
	beq.w done
	move.l Config.BinCount(a2), Config.BinCount(a1)
	move.l Config.Bins+memory.Block.Pointer(a2), Config.Bins+memory.Block.Pointer(a1)
	move.l Config.Bins+memory.Block.Capacity(a2), Config.Bins+memory.Block.Capacity(a1)
	move.l Config.Bins+memory.Block.Used(a2), Config.Bins+memory.Block.Used(a1)
done
	lea Config.Effective(a3), a0
	movem.l (sp)+, d0-d2/a1-a3
	rts
	.bend  ; select
	.priv

; CLI-owned root export identity for output artifacts.
; @opforge-owner: opforge.cli.output
; @opforge-slice: documentation/plans/slices/native-performance-listing-symbol-footer-v1.toml
; @opforge-role: implementation

	.module opforge.cli.symbol_metadata
	.cpu 68020
	.use opasm.amigaos.engine as engine
	.use opforge.cli.state

	.section code, kind=code
	.pub

; Inputs: D0 = engine label index. Outputs: D0 = root export boolean.
; Clobbers: D0/CCR. CCR: reflects D0.
; Exports retain raw names; engine labels already include their owner prefix.
labelIsPublicV1	.block
	.priv
	movem.l d1-d7/a0-a3, -(sp)
	move.l d0, d7
	jsr engine.opasmEngineGetLabelNameV1
	movea.l a0, a3
	moveq #0, d6
loop
	cmp.w state.NativeCliOrdinaryExportCount, d6
	bhs.w no
	move.l d6, d0
	add.l d0, d0
	lea state.NativeCliOrdinaryExportOwnerTable, a0
	move.w 0(a0, d0.l), d1
	cmp.w state.NativeCliRootModuleId, d1
	bne.w next
	; Match the owner prefix in place, then compare the raw export suffix.
	moveq #0, d0
	move.w d1, d0
	lsl.l #6, d0
	lea state.NativeCliModuleNameTable, a1
	adda.l d0, a1
	movea.l a3, a0
ownerCharacter
	moveq #0, d2
	move.b (a1)+, d2
	beq.s ownerEnd
	moveq #0, d3
	move.b (a0)+, d3
	cmpi.b #'A', d2
	blo.s foldLabel
	cmpi.b #'Z', d2
	bhi.s foldLabel
	ori.b #$20, d2
foldLabel
	cmpi.b #'A', d3
	blo.s compareOwner
	cmpi.b #'Z', d3
	bhi.s compareOwner
	ori.b #$20, d3
compareOwner
	cmp.b d2, d3
	bne.s next
	bra.s ownerCharacter
ownerEnd
	cmpi.b #'.', (a0)+
	bne.s next
	move.l d6, d0
	lsl.l #2, d0
	lea state.NativeCliOrdinaryExportNameOffsetTable, a1
	move.l 0(a1, d0.l), d0
	lea state.NativeCliOrdinaryExportNamePool, a1
	adda.l d0, a1
	bsr.w namesEqualFoldedV1
	tst.l d0
	bne.s yes
next
	addq.l #1, d6
	bra.w loop
yes
	moveq #1, d0
	bra.s return
no
	moveq #0, d0
return
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; labelIsPublicV1
	.pub

; Compare NUL strings A0/A1 using ASCII lowercase order.
; Outputs: D0=-1/0/1. Clobbers: D0/D2-D3/A0-A1/CCR. CCR: reflects D0.
compareFoldedNamesV1	.block
	.priv
loop
	moveq #0, d2
	moveq #0, d3
	move.b (a0)+, d2
	move.b (a1)+, d3
	cmpi.b #'A', d2
	blo.s foldRight
	cmpi.b #'Z', d2
	bhi.s foldRight
	ori.b #$20, d2
foldRight
	cmpi.b #'A', d3
	blo.s compare
	cmpi.b #'Z', d3
	bhi.s compare
	ori.b #$20, d3
compare
	cmp.b d3, d2
	blo.s less
	bhi.s greater
	tst.b d2
	bne.s loop
	moveq #0, d0
	rts
less
	moveq #-1, d0
	rts
greater
	moveq #1, d0
	rts
	.bend  ; compareFoldedNamesV1

	.priv

; Inputs: A0/A1 = NUL strings. Outputs: D0 = folded equality boolean.
; Clobbers: D0/D2-D3/A0-A1/CCR. CCR: reflects D0.
namesEqualFoldedV1	.block
	bsr.w compareFoldedNamesV1
	tst.l d0
	bne.s no
	moveq #1, d0
	rts
no
	moveq #0, d0
	rts
	.bend  ; namesEqualFoldedV1

	.endsection
	.endmodule

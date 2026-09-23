; Preparation-only source-root planning. The entry anchors search, while the
; module graph decides execution order. No path enters packed source records.
; @opforge-owner: experimental.amigaos.binary_input_plan
	.module experimental.amigaos.binary_input_plan
	.cpu 68020
	.use experimental.amigaos.binary_discovery as discovery
PATH_BYTES = 256
	.pub
Frame	.struct
Entry	.long ?  ; NUL-terminated entry path
Roots	.long ?  ; contiguous 256-byte NUL-terminated path slots
Count	.long ?
Scratch	.long ?
Callback	.long ?
Dos	.long ?
Directory	.long ?  ; caller-owned 256-byte temporary path
	.endstruct
	.section code, kind=code

; Seed the entry and search its directory before optional module roots.
; A0=Frame; D0/CCR=status. Preserves D1-D7/A0-A6.
seed	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l Frame.Entry(a5), a0
	movea.l Frame.Callback(a5), a2
	suba.l a1, a1
	jsr (a2)
	bne.w bad
	bsr.w entryDirectory
	bne.w bad
	movea.l Frame.Directory(a5), a1
	bsr.w scanRoot
	bne.w bad
	moveq #0, d7
next
	cmp.l Frame.Count(a5), d7
	bhs.w good
	move.l d7, d0
	lsl.l #8, d0
	movea.l Frame.Roots(a5), a1
	adda.l d0, a1
	bsr.w scanRoot
	bne.w bad
	addq.l #1, d7
	bra.w next
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; seed
	.priv
; A5=Frame. Derive a directory, using an empty AmigaDOS path for the current dir.
entryDirectory	.block
	movea.l Frame.Entry(a5), a0
	movea.l Frame.Directory(a5), a1
	moveq #0, d2
	moveq #-1, d3
find
	cmpi.l #PATH_BYTES, d2
	bhs.w bad
	moveq #0, d0
	move.b 0(a0, d2.l), d0
	beq.w found
	cmpi.b #'/', d0
	beq.w slash
	cmpi.b #':', d0
	bne.w advance
	move.l d2, d3
	addq.l #1, d3
	bra.w advance
slash
	move.l d2, d3
	bne.w advance
	moveq #1, d3
advance
	addq.l #1, d2
	bra.w find
found
	tst.l d2
	beq.w bad
	tst.l d3
	bpl.w copy
	clr.b (a1)
	bra.w good
copy
	move.l d3, d2
copyByte
	move.b (a0)+, (a1)+
	subq.l #1, d2
	bne.w copyByte
	clr.b (a1)
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; entryDirectory

; A1=NUL root, A5=Frame. D0/CCR=status; preserves A5/D7 loop state.
scanRoot	.block
	movea.l Frame.Scratch(a5), a0
	movea.l Frame.Callback(a5), a2
	suba.l a3, a3
	movea.l Frame.Dos(a5), a4
	jsr discovery.scan
	rts
	.bend  ; scanRoot
	.endsection
	.endmodule

; Bounded package-driven register-list mask projection.
; @opforge-owner: experimental.amigaos.binary_register_mask
	.module experimental.amigaos.binary_register_mask
	.cpu 68020
	.include "telemetry_macros.i"
	.use experimental.amigaos.binary_package as pkg
	.section code, kind=code
	.pub

; A0/A1=list bounds,A4=bounded mask projection,A5=BSP package.
; D0/CCR=status,D3=mask on success; all other registers preserved.
project	.block
	movem.l d1-d2/d4-d7/a0-a2/a4-a6, -(sp)
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_OPERAND
	cmpi.b #18, pkg.MaskProjection.Kind(a4)
	bne.w projectBad
	cmpi.w #$ffff, pkg.MaskProjection.ValueProgram(a4)
	bne.w projectBad
	cmpi.w #1, pkg.MaskProjection.Flags(a4)
	bhi.w projectBad
	move.w pkg.MaskProjection.FirstClass(a4), d0
	cmp.w pkg.MaskProjection.SecondClass(a4), d0
	beq.w projectBad
	cmpi.b #15, pkg.MaskProjection.FirstShift(a4)
	bhi.w projectBad
	cmpi.b #15, pkg.MaskProjection.SecondShift(a4)
	bhi.w projectBad
	movea.l a5, a6
	movea.l a4, a5
	bsr.w parseMask
	bne.w projectBad
	move.l d4, d3
	moveq #0, d0
	bra.w projectDone
projectBad
	moveq #1, d0
projectDone
	.TELEMETRY_SERVICE_LEAVE
	movem.l (sp)+, d1-d2/d4-d7/a0-a2/a4-a6
	tst.l d0
	rts
	.bend  ; project

; A0/A1=list,A5=validated mask projection,A6=package; D4=mask on
; success,D0/CCR=status. Internal register scratch is caller-owned.
parseMask	.block
	cmpa.l a1, a0
	bhs.w maskBad
	clr.l d6
nextItem
	bsr.w register
	bne.w maskBad
	bsr.w bitIndex
	bne.w maskBad
	move.w d3, d4
	move.w d1, d7
	cmpa.l a1, a0
	beq.w oneBit
	cmpi.b #19, (a0)
	bne.w oneBit
	addq.l #1, a0
	bsr.w register
	bne.w maskBad
	bsr.w bitIndex
	bne.w maskBad
	cmp.w d7, d1
	bne.w maskBad
	cmp.w d4, d3
	blo.w maskBad
rangeBit
	bset d4, d6
	cmp.w d3, d4
	beq.w itemEnd
	addq.w #1, d4
	bra.w rangeBit
oneBit
	bset d3, d6
itemEnd
	cmpa.l a1, a0
	beq.w reverse
	cmpi.b #22, (a0)+
	bne.w maskBad
	bra.w nextItem
reverse
	move.l d6, d4
	tst.w pkg.MaskProjection.Flags(a5)
	beq.w maskReady
	moveq #0, d4
	moveq #15, d3
reverseBit
	lsl.w #1, d4
	lsr.w #1, d6
	bcc.w zeroBit
	addq.w #1, d4
zeroBit
	dbra d3, reverseBit
maskReady
	moveq #0, d0
	rts
maskBad
	moveq #1, d0
	rts
	.bend  ; parseMask

	.priv
; A0/A1=one bounded numeric name,A6=package. Return D1=class,D2=index,
; A0 advanced by four; D0/CCR=status. Other registers preserved.
register	.block
	movem.l d3-d4/a3, -(sp)
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w invalid
	cmpi.b #1, (a0)
	bhi.w invalid
	tst.b 3(a0)
	bne.w invalid
	moveq #0, d1
	move.w 1(a0), d1
	addq.l #4, a0
	move.l pkg.Header.RegisterRows(a6), d0
	move.l pkg.Header.RegisterCount(a6), d3
	cmpi.l #$ffff, d3
	bhi.w invalid
	move.l d3, d4
	mulu.w #6, d4
	add.l d0, d4
	bcs.w invalid
	cmp.l pkg.Header.Bytes(a6), d4
	bhi.w invalid
	movea.l a6, a3
	adda.l d0, a3
find
	tst.l d3
	beq.w invalid
	cmp.w (a3), d1
	beq.w found
	addq.l #6, a3
	subq.l #1, d3
	bra.w find
found
	moveq #0, d1
	move.w 2(a3), d1
	moveq #0, d2
	move.w 4(a3), d2
	moveq #0, d0
	bra.w done
invalid
	moveq #1, d0
done
	movem.l (sp)+, d3-d4/a3
	tst.l d0
	rts
	.bend  ; register

; D1=package class,D2=opaque index,A5=validated mask projection.
; Return D3=mask bit or D0=1.
bitIndex	.block
	cmp.w pkg.MaskProjection.FirstClass(a5), d1
	beq.w first
	cmp.w pkg.MaskProjection.SecondClass(a5), d1
	bne.w invalid
	moveq #0, d3
	move.b pkg.MaskProjection.SecondShift(a5), d3
	bra.w mapped
first
	moveq #0, d3
	move.b pkg.MaskProjection.FirstShift(a5), d3
mapped
	add.l d2, d3
	cmpi.l #15, d3
	bhi.w invalid
	moveq #0, d0
	rts
invalid
	moveq #1, d0
	rts
	.bend  ; bitIndex
	.endsection
	.endmodule

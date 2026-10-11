; Session provider for decoded text bytes and canonical ExprVM string scalars.
	.module experimental.amigaos.binary_string_provider
	.cpu 68020
	.use experimental.amigaos.binary_text as text
	.use experimental.amigaos.binary_text_layout as layout
	.use experimental.amigaos.binary_memory as memory
	.use exprvm.amigaos.runtime as runtime
	.pub
	.section code, kind=code
; A0=session TextState or zero at cleanup. D0/CCR=0, others preserved.
configure	.block
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	move.l a0, d0
	beq.w install
	lea scalar, a0
install
	jsr runtime.configureStringProvider
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; configure

; A0=TextState,A1/D1=decoded bytes/length. D0=status,A1/D1=encoded bytes/
; length on success. Buffer survives until next encode/release. Others preserved.
; Retry overflow with bounded geometric growth; partial prefixes never publish.
encode	.block
	movem.l d2-d4/a0/a2-a3, -(sp)
	clr.l Output+memory.Block.Used
	move.l a0, d0
	beq.w invalid
	movea.l a0, a3
	move.l d1, d3
	move.l #256, d4
retry
	lea Output, a0
	move.l d4, d0
	jsr memory.reserve
	tst.l d0
	bne.w done
	movea.l memory.Block.Pointer(a0), a2
	move.l memory.Block.Capacity(a0), d2
	movea.l a3, a0
	move.l d3, d1
	jsr text.encode
	tst.l d0
	beq.w success
	cmpi.l #layout.STATUS_OVERFLOW, d0
	bne.w done
	cmpi.l #memory.LIMIT, d2
	bhs.w done
	move.l d2, d4
	add.l d4, d4
	bra.w retry
success
	movea.l a2, a1
	move.l d1, Output+memory.Block.Used
	bra.w done
invalid
	moveq #layout.STATUS_INVALID, d0
done
	movem.l (sp)+, d2-d4/a0/a2-a3
	tst.l d0
	rts
	.bend  ; encode

; Release session output storage. D0/CCR=status; other registers preserved.
release	.block
	movem.l a0-a1, -(sp)
	suba.l a0, a0
	suba.l a1, a1
	jsr runtime.configureStringProvider
	lea Output, a0
	jsr memory.release
	clr.l Output+memory.Block.Used
	moveq #layout.STATUS_OK, d0
	movem.l (sp)+, a0-a1
	rts
	.bend  ; release

; Callback A0/D1=decoded bytes/length,A1=TextState. D0=status,D1=scalar.
; Encoded scalars must contain one or two bytes in big-endian order.
scalar	.block
	movem.l a0-a2, -(sp)
	exg a0, a1
	jsr encode
	tst.l d0
	bne.w done
	tst.l d1
	beq.w invalid
	cmpi.l #2, d1
	bhi.w invalid
	move.l d1, d0
	moveq #0, d1
	move.b (a1)+, d1
	cmpi.l #2, d0
	bne.w ready
	lsl.w #8, d1
	move.b (a1), d1
ready
	moveq #layout.STATUS_OK, d0
	bra.w done
invalid
	moveq #layout.STATUS_INVALID, d0
done
	movem.l (sp)+, a0-a2
	tst.l d0
	rts
	.bend  ; scalar
	.endsection
	.section bss, kind=bss
Output	.res byte, memory.Block.Used+4
	.endsection
	.endmodule

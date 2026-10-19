; Complete valid source requiring whole-module fallback at the private boundary:
; automatic width crosses an alignment fragment and must not become frozen.
branchWidthChecks	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea WidthContext, a0
	move.l #Package, compiler.Context.Package(a0)
	move.l #WidthSource, compiler.Context.Source(a0)
	move.l #WidthSourceEnd-WidthSource, compiler.Context.Bytes(a0)
	lea WidthImage, a1
	jsr compiler.compile
	tst.l d0
	beq.w bad
	move.l WidthImage+memory.Block.Pointer, d0
	or.l WidthImage+memory.Block.Capacity, d0
	or.l WidthImage+memory.Block.Used, d0
	bne.w bad
	moveq #0, d7
	bra.w cleanup
bad
	moveq #1, d7
cleanup
	lea WidthImage, a0
	jsr memory.release
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; branchWidthChecks
	.endsection
	.section data, kind=data
	.priv
WidthSource	.incbin "module_branch_width_auto.asm"
WidthSourceEnd
	.endsection
	.section bss, kind=bss
	.priv
	.align 4
WidthContext	.res byte, compiler.CONTEXT_BYTES
WidthImage	.res byte, memory.Block.Used+4
	.endsection
	.section code, kind=code
	.priv

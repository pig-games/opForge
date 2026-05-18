; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.token_util
	.cpu 68020

	.use opforge.cli.constants (TOKEN_BUFFER_CAPACITY)

	.section code, kind=code
	.pub

opforgeNativeCliCopyTokenBuffer	.block
	moveq #0, d0
	move.b (a0)+, d0
	move.b d0, (a1)+
	bne.s opforgeNativeCliCopyTokenBuffer
	rts
	.bend  ; opforgeNativeCliCopyTokenBuffer

opforgeNativeCliTokenLen	.block
	movem.l d1/a0, -(sp)
	moveq #0, d0
	move.l #TOKEN_BUFFER_CAPACITY - 1, d1

loop
	tst.b (a0)+
	beq.s done
	addq.w #1, d0
	dbra d1, loop

done
	movem.l (sp)+, d1/a0
	rts
	.bend  ; opforgeNativeCliTokenLen

	.endsection
	.endmodule

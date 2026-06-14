; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.path
	.cpu 68020

	.use opforge.cli.constants

	.section code, kind=code
	.pub

opforgeNativeCliPathHasVolumePrefix	.block
	moveq #0, d0

loop
	move.b (a0)+, d1
	beq.s no
	cmpi.b #':', d1
	beq.s yes
	bra.s loop

yes
	moveq #1, d0
	rts

no
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliPathHasVolumePrefix

opforgeNativeCliCopyPathRoot	.block
	movem.l d2-d6/a2, -(sp)
	movea.l a0, a2
	clr.l d5
	clr.l d6

scan
	move.b (a2)+, d2
	beq.s copy
	addq.l #1, d5
	cmpi.b #':', d2
	beq.s mark
	cmpi.b #'/', d2
	bne.s scan

mark
	move.l d5, d6
	bra.s scan

copy
	movea.l a0, a2
	move.l #constants.PATH_BUFFER_CAPACITY - 1, d4
	tst.l d6
	beq.s done

loop
	tst.l d4
	beq.s fail
	move.b (a2)+, d3
	move.b d3, (a1)+
	subq.l #1, d6
	subq.l #1, d4
	tst.l d6
	bne.s loop

done
	clr.b (a1)
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d2-d6/a2
	rts
	.bend  ; opforgeNativeCliCopyPathRoot

opforgeNativeCliCopyPathBuffer	.block
	move.l #constants.PATH_BUFFER_CAPACITY - 1, d6

loop
	move.b (a0)+, d2
	move.b d2, (a1)+
	beq.s ok
	subq.l #1, d6
	bne.s loop
	clr.b -(a1)
	moveq #1, d0
	rts

ok
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliCopyPathBuffer

opforgeNativeCliAppendPathBuffer	.block
	move.l #constants.PATH_BUFFER_CAPACITY - 1, d6

end
	tst.b (a1)
	beq.s copy
	addq.l #1, a1
	subq.l #1, d6
	beq.s fail
	bra.s end

copy
	move.b (a0)+, d2
	move.b d2, (a1)+
	beq.s ok
	subq.l #1, d6
	bne.s copy

fail
	clr.b -(a1)
	moveq #1, d0
	rts

ok
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliAppendPathBuffer

; Append one relative path segment to a path buffer, inserting `/` when needed.
; Inputs: A0 = NUL-terminated segment; A1 = destination path buffer.
; Outputs: D0 = 0 on success, 1 on capacity failure; destination is NUL-terminated.
; Clobbers: D0/D2/D6/A0-A1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliAppendPathSegmentBuffer	.block
	move.l #constants.PATH_BUFFER_CAPACITY - 1, d6

end
	tst.b (a1)
	beq.s maybeSeparator
	addq.l #1, a1
	subq.l #1, d6
	beq.s fail
	bra.s end

maybeSeparator
	cmpi.l #constants.PATH_BUFFER_CAPACITY - 1, d6
	beq.s copy
	cmpi.b #':', -1(a1)
	beq.s copy
	cmpi.b #'/', -1(a1)
	beq.s copy
	move.b #'/', (a1)+
	subq.l #1, d6
	beq.s fail

copy
	move.b (a0)+, d2
	move.b d2, (a1)+
	beq.s ok
	subq.l #1, d6
	bne.s copy

fail
	clr.b -(a1)
	moveq #1, d0
	rts

ok
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliAppendPathSegmentBuffer

	.endsection
	.endmodule

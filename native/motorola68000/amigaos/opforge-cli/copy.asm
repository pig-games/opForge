; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.copy
	.cpu 68020

	.section code, kind=code
	.pub

; Copy D0 bytes from A1 to A2.
copyBytes	.block
	move.w d0, d2
	beq.s done

loop
	move.b (a1)+, (a2)+
	subq.w #1, d2
	bne.s loop

done
	rts
	.bend  ; copyBytes

; Copy a C string from A0 to A1 and return copied byte count, including NUL.
copyCString	.block
	moveq #0, d0

loop
	move.b (a0)+, d1
	move.b d1, (a1)+
	addq.w #1, d0
	tst.b d1
	bne.s loop
	rts
	.bend  ; copyCString

; Copy exactly D0 bytes from A0 to A1.
copyFixedString	.block
	move.w d0, d2
	beq.s done

loop
	move.b (a0)+, (a1)+
	subq.w #1, d2
	bne.s loop

done
	rts
	.bend  ; copyFixedString

; Clear D0 bytes at A0.
clearBytes	.block
	tst.l d0
	beq.s done
	moveq #0, d1

loop
	move.b d1, (a0)+
	subq.l #1, d0
	bne.s loop

done
	rts
	.bend  ; clearBytes

	.endsection
	.endmodule

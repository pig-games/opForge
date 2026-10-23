; Compare bounded prepared keyword bytes under the package invocation policy.
; @opforge-owner: opforge.vm.prvm.statement_text_match
	.module opforge.vm.prvm.statement_text_match
	.cpu 68020
	.use opforge.vm.prvm.statement_abi as abi
	.use opforge.vm.prvm.statement_inputs as inputs
	.priv
ASCII_UPPER_FIRST = 'A'
ASCII_UPPER_LAST = 'Z'
ASCII_CASE_DELTA = 'a'-'A'
	.pub
	.section code, kind=code
; A0/A1=validated byte spans,D0=shared bytes,A2=validated A9,A3=budget cell.
; D0=status/CCR,D1=equal boolean; other registers preserved. The package fold
; flag selects Rust's ASCII case-insensitive byte equality; non-ASCII stays exact.
equal	.block
	movem.l d2-d5/a0-a1, -(sp)
	move.l d0, d5
	moveq #0, d4
	move.b abi.InvocationPolicy.Flags(a2), d4
	andi.l #abi.INVOCATION_FOLD_ASCII, d4
next
	tst.l d5
	beq.w matched
	move.l a0, -(sp)
	movea.l a3, a0
	jsr inputs.tick
	movea.l (sp)+, a0
	bne.w failed
	moveq #0, d2
	moveq #0, d3
	move.b (a0)+, d2
	move.b (a1)+, d3
	tst.l d4
	beq.w compare
	cmpi.l #ASCII_UPPER_FIRST, d2
	blo.w right
	cmpi.l #ASCII_UPPER_LAST, d2
	bhi.w right
	addi.l #ASCII_CASE_DELTA, d2
right
	cmpi.l #ASCII_UPPER_FIRST, d3
	blo.w compare
	cmpi.l #ASCII_UPPER_LAST, d3
	bhi.w compare
	addi.l #ASCII_CASE_DELTA, d3
compare
	cmp.l d2, d3
	bne.w mismatch
	subq.l #1, d5
	bra.w next
matched
	moveq #1, d1
	moveq #abi.OK, d0
	bra.w done
mismatch
	moveq #abi.OK, d0
failed
	moveq #0, d1
done
	tst.l d0
	movem.l (sp)+, d2-d5/a0-a1
	rts
	.bend  ; equal
	.endsection
	.endmodule

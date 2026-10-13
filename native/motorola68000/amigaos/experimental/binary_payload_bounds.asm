; Bounded opaque payloads shared by prepared-record consumers.
; @opforge-owner: experimental.amigaos.binary_payload_bounds
.module experimental.amigaos.binary_payload_bounds
.cpu 68020
.use experimental.amigaos.binary_source as source
.pub
.section code, kind=code
; A0=LE16 string length,A1=expression end. Advance only after full validation.
; D0/CCR=status; other registers preserved except successful A0 advancement.
string	.block
	move.l d1, -(sp)
	move.l a1, d0
	sub.l a0, d0
	bcs.w bad
	cmpi.l #2, d0
	blo.w bad
	moveq #0, d1
	move.b 1(a0), d1
	lsl.w #8, d1
	move.b (a0), d1
	subq.l #2, d0
	cmp.l d1, d0
	blo.w bad
	addq.l #2, a0
	adda.l d1, a0
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	move.l (sp)+, d1
	tst.l d0
	rts
.bend
; D0=kind42..44,D1=record flags,A0=after kind,A1=record end.
; D0/CCR=status,D1=matched(0/1). A0 advances only for a validated tail.
; Nonterminal43/44 retain declaration meaning; all other registers preserved.
plan	.block
	movem.l d2-d3, -(sp)
	move.l d1, d2
	moveq #source.FLAG_PLAN, d3
	cmpi.b #source.MACRO_PLAN, d0
	beq.w required
	btst #source.LINE_PLAN_BIT, d2
	beq.w ordinary
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #source.PLAN_TOKEN_BYTES-1, d0
	bne.w ordinary
	cmpi.b #source.PLAN_TOKEN_BYTES, source.PlanToken.Bytes-1(a0)
	bne.w ordinary
	moveq #source.LINE_PLAN_FLAG, d3
required
	andi.w #source.FLAG_PLAN+source.LINE_PLAN_FLAG, d2
	cmp.w d3, d2
	bne.w bad
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #source.PLAN_TOKEN_BYTES-1, d0
	bne.w bad
	cmpi.b #source.PLAN_TOKEN_BYTES, source.PlanToken.Bytes-1(a0)
	bne.w bad
	tst.l (a0)
	beq.w bad
	movea.l a1, a0
	moveq #1, d1
	moveq #0, d0
	bra.w done
ordinary
	moveq #0, d1
	moveq #0, d0
	bra.w done
bad
	moveq #0, d1
	moveq #1, d0
done
	movem.l (sp)+, d2-d3
	tst.l d0
	rts
.bend
.endsection
.endmodule

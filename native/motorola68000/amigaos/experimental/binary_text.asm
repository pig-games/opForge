; Shared byte mapping owner. Inputs are decoded private-token values; this module
; never consults source text or package instruction encoders. Rust reference:
; crates/opforge-types/src/text_encoding.rs (7-bit builtin maps and custom definitions).
	.module experimental.amigaos.binary_text
	.cpu 68020
	.use experimental.amigaos.binary_text_layout as layout
	.use experimental.amigaos.binary_text_registry as registry
	.use experimental.amigaos.binary_memory as memory
	.section code, kind=code
	.pub
; A0=zero-initialized State,D1/D2=canonical ASCII/PETSCII name IDs.
; D0=status; all other registers saved. IDs are registered by the tokenizer.
; CCR reflects D0. The IDs must be distinct nonzero u16 values.
init	.block
	moveq #layout.STATUS_INVALID, d0
	tst.l d1
	beq.w done
	tst.l d2
	beq.w done
	cmpi.l #$ffff, d1
	bhi.w done
	cmpi.l #$ffff, d2
	bhi.w done
	cmp.w d1, d2
	beq.w done
	move.w d1, layout.State.AsciiId(a0)
	move.w d2, layout.State.PetsciiId(a0)
	bra.w resetProfile
done
	tst.l d0
	rts
	.bend  ; init

; A0=State. Reset active selection at pass/module
; entry. Registered definitions and tokenizer-owned builtin identities survive.
; D0=status, other registers preserved. CCR reflects D0.
resetProfile	.block
	clr.l layout.State.Scopes(a0)
	move.w layout.State.AsciiId(a0), layout.State.Active(a0)
	moveq #layout.STATUS_OK, d0
	rts
	.bend  ; resetProfile

; A0=State,D1=canonical numeric name ID. Builtin IDs were supplied to init.
; D0=status, other registers saved. CCR reflects D0.
select	.block
	movem.l d1-d4/a1-a3, -(sp)
	moveq #layout.STATUS_INVALID, d0
	tst.l d1
	beq.w done
	cmpi.l #$ffff, d1
	bhi.w done
	jsr registry.find
	tst.l d0
	bne.w done
	move.w d1, layout.State.Active(a0)
done
	movem.l (sp)+, d1-d4/a1-a3
	tst.l d0
	rts
	.bend  ; select

; A0=State. Release all definition storage at session end; D0=status.
; Other registers preserved, CCR reflects D0.
release	.block
	movem.l a0, -(sp)
	lea layout.State.Arena(a0), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movem.l (sp)+, a0
	clr.l layout.State.Entries(a0)
	bra.w resetProfile
	.bend  ; release

; A0=State,A1/D1=decoded input,A2/D2=output/capacity. D0=status,D1=written
; bytes on success. Output may contain a partial prefix on failure; callers must
; publish it only on success. Other registers saved; CCR reflects D0.
encode	.block
	movem.l d2-d7/a1-a6, -(sp)
	move.l d1, d6
	move.l d2, d7
	movea.l a2, a6
	moveq #0, d1
	move.w layout.State.Active(a0), d1
	jsr registry.find
	tst.l d0
	bne.w done
	move.l a3, d0
	beq.w loop
	move.l d6, d1
	jsr registry.encodeOwned
	bra.w done
loop
	tst.l d6
	beq.w complete
	moveq #0, d0
	move.b (a1)+, d0
	cmpi.w #128, d0
	bhs.w missing
	move.w layout.State.PetsciiId(a0), d1
	cmp.w layout.State.Active(a0), d1
	bne.w mapped
	cmpi.b #'A', d0
	blo.w mapped
	cmpi.b #'Z', d0
	bhi.w lower
	ori.b #$80, d0
	bra.w mapped
lower
	cmpi.b #'a', d0
	blo.w mapped
	cmpi.b #'z', d0
	bhi.w mapped
	subi.b #$20, d0
mapped
	tst.l d7
	beq.w overflow
	move.b d0, (a2)+
	subq.l #1, d7
	subq.l #1, d6
	bra.w loop
complete
	move.l a2, d1
	sub.l a6, d1
	moveq #layout.STATUS_OK, d0
	bra.w done
missing
	moveq #layout.STATUS_MISSING, d0
	bra.w done
overflow
	moveq #layout.STATUS_OVERFLOW, d0
done
	movem.l (sp)+, d2-d7/a1-a6
	tst.l d0
	rts
	.bend  ; encode
	.endsection
	.endmodule

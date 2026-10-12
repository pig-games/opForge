; Package-owned numeric statement definition/end selection over prepared facets.
; @opforge-owner: prvm.amigaos.statement_head
	.module prvm.amigaos.statement_head
	.cpu 68020
	.use prvm.amigaos.statement_abi as abi
	.include "telemetry_macros.i"
	.use prvm.amigaos.statement_inputs as inputs
	.pub
	.section code, kind=code
; A0=program,D0=bounded bytes. D0=status/CCR; other registers preserved.
; Definition/end identities must be nonzero and distinct; name bounds belong
; to the owning package container, which knows its name table's extent.
validateProgram	.block
	movem.l d1/a0, -(sp)
	cmpi.l #abi.HEAD_PROGRAM_BYTES, d0
	bne.w invalidProgram
	jsr inputs.span
	bne.w done
	cmpi.b #abi.OP_HEAD, (a0)  ; HeadProgram.Opcode (zero offset)
	bne.w invalidProgram
	cmpi.b #1, abi.HeadProgram.LabelPolicy(a0)
	bne.w invalidProgram
	cmpi.b #abi.OP_PUBLISH, abi.HeadProgram.Publish(a0)
	bne.w invalidProgram
	cmpi.b #abi.OP_END, abi.HeadProgram.End(a0)
	bne.w invalidProgram
	move.w abi.HeadProgram.Definition(a0), d1
	beq.w invalidProgram
	tst.w abi.HeadProgram.EndMarker(a0)
	beq.w invalidProgram
	cmp.w abi.HeadProgram.EndMarker(a0), d1
	beq.w invalidProgram
	moveq #abi.OK, d0
	bra.w done
invalidProgram
	moveq #abi.INVALID_PROGRAM, d0
done
	tst.l d0
	movem.l (sp)+, d1/a0
	rts
	.bend  ; validateProgram
; A0=HeadFrame,D0=available bytes. D0=status/CCR,D1=action(0/1/2),
; D2=keyword token index,D3=signature token index. Other registers preserved.
; No tail validation: end-marker indices may intentionally lie after token count.
run	.block
	.TELEMETRY_VM_ENTER runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	movem.l d4-d7/a0-a6, -(sp)
	subq.l #4, sp
	movea.l a0, a4
	move.l a0, d1
	btst #0, d1
	bne.w invalid
	cmpi.l #abi.HEAD_FRAME_BYTES, d0
	blo.w invalid
	moveq #abi.HEAD_FRAME_BYTES, d0
	jsr inputs.span
	bne.w finish
	cmpi.l #abi.HEAD_COMPLETE, abi.HeadFrame.Mode(a4)
	bhi.w invalid
	move.l abi.HeadFrame.StepBudget(a4), (sp)  ; local budget (zero offset)
	bmi.w invalid
	movea.l sp, a0
	jsr inputs.tick
	bne.w finish
	movea.l abi.HeadFrame.ProgramPtr(a4), a0
	move.l abi.HeadFrame.ProgramBytes(a4), d0
	bsr.w validateProgram
	bne.w finish
	movea.l a0, a5
	move.w abi.HeadProgram.Definition(a5), d4
	move.w abi.HeadProgram.EndMarker(a5), d5
	move.l abi.HeadFrame.TokenCount(a4), d6
	cmp.l abi.HeadFrame.NamesCount(a4), d6
	bne.w invalid
	cmpi.l #$03ffffff, d6
	bhi.w invalid
	movea.l abi.HeadFrame.TokenPtr(a4), a3
	move.l a3, d0
	btst #0, d0
	bne.w invalid
	movea.l a3, a0
	move.l d6, d0
	lsl.l #5, d0  ; abi.TOKEN_BYTES = 32
	jsr inputs.span
	bne.w finish
	movea.l abi.HeadFrame.NamesPtr(a4), a2
	move.l a2, d0
	btst #0, d0
	bne.w invalid
	movea.l a2, a0
	move.l d6, d0
	add.l d0, d0
	jsr inputs.span
	bne.w finish
	moveq #0, d7
	moveq #0, d2
	tst.l d6
	beq.w absent
	cmpi.b #abi.K_REGISTER, (a3)  ; TokenView.Kind (zero offset)
	bhi.w head
	moveq #1, d7
	addq.l #1, d2
	adda.w #abi.TOKEN_BYTES, a3
	addq.l #abi.IDENTITY_BYTES, a2
	cmp.l d6, d2
	bhs.w absent
	cmpi.b #abi.K_COLON, (a3)
	bne.w head
	addq.l #1, d2
	adda.w #abi.TOKEN_BYTES, a3
	addq.l #abi.IDENTITY_BYTES, a2
head
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	movea.l sp, a0
	jsr inputs.tick
	bne.w finish
	cmp.l d6, d2
	bhs.w absent
	cmpi.b #abi.K_DOT, (a3)
	bne.w absent
	move.l d2, d0
	addq.l #1, d0
	cmp.l d6, d0
	bhs.w absent
	cmpi.b #abi.K_REGISTER, abi.TOKEN_BYTES+abi.TokenView.Kind(a3)
	bhi.w absent
	move.w abi.IDENTITY_BYTES(a2), d0  ; following token's BE16 numeric identity
	cmp.w d4, d0
	beq.w definition
	cmp.w d5, d0
	bne.w absent
	moveq #abi.HEAD_END, d1
	bra.w selected
definition
	tst.l abi.HeadFrame.Mode(a4)
	beq.w definitionReady
	tst.l d7
	bne.w invalidToken
	move.l d2, d0
	addq.l #2, d0
	cmp.l d6, d0
	bhs.w invalidToken
	cmpi.b #abi.K_REGISTER, abi.TOKEN_BYTES*2+abi.TokenView.Kind(a3)
	bhi.w invalidToken
definitionReady
	moveq #abi.HEAD_DEFINITION, d1
selected
	addq.l #2, d2
	move.l d2, d3
	addq.l #1, d3
	moveq #abi.OK, d0
	bra.w done
absent
	moveq #abi.OK, d0
	bra.w finish
invalidToken
	moveq #abi.INVALID_TOKEN, d0
	bra.w finish
invalid
	moveq #abi.INVALID_ARGUMENT, d0
finish
	moveq #0, d1
	moveq #0, d2
	moveq #0, d3
done
	.TELEMETRY_VM_LEAVE
	addq.l #4, sp
	tst.l d0
	movem.l (sp)+, d4-d7/a0-a6
	rts
	.bend  ; run
	.endsection
	.endmodule

; Shared package-owned loop envelope; EXVM owns expression syntax.
; @opforge-owner: prvm.amigaos.packed_loop
	.module prvm.amigaos.packed_loop
	.cpu 68020
	.use prvm.amigaos.abi as abi
	.include "telemetry_macros.i"
	.pub
	.section code, kind=code
; A0/A1=writer prefix/end,A2/D1=program/bytes,D0=candidate numeric name.
; Bind the contextual grammar word only after `.for <bare variable>`.
; D0/CCR=0 matched,1 absent; other registers preserved. The writer has not
; finalized the line-length byte yet; the caller supplies the exact prefix end.
contextKeyword	.block
	movem.l d1-d3/a0-a2, -(sp)
	move.l a0, d2
	beq.w absent
	move.l a2, d2
	beq.w absent
	cmpi.l #7, d1
	bne.w absent
	cmpi.b #$99, (a2)
	bne.w absent
	cmpi.b #$83, 5(a2)
	bne.w absent
	tst.b 6(a2)
	bne.w absent
	move.l d0, d1
	move.l a1, d2
	sub.l a0, d2
	cmpi.l #13, d2
	bne.w absent
	move.b 1(a0), d3
	andi.b #$d8, d3
	bne.w absent
	cmpi.b #7, 4(a0)
	bne.w absent
	cmpi.b #1, 5(a0)
	bhi.w absent
	tst.b 8(a0)
	bne.w absent
	cmpi.b #1, 9(a0)
	bhi.w absent
	tst.b 12(a0)
	bne.w absent
	move.b 6(a0), d3
	cmp.b 1(a2), d3
	bne.w absent
	move.b 7(a0), d3
	cmp.b 2(a2), d3
	bne.w absent
	moveq #0, d2
	move.b 3(a2), d2
	lsl.w #8, d2
	move.b 4(a2), d2
	cmp.l d1, d2
	bne.w absent
	moveq #0, d0
	bra.w done
absent
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; contextKeyword

; A0=raw writer record,D0=extent,A1=program,D1=program bytes.
; D0=matched/ordinary/malformed,D1=variable identity (-1 counted),D2=operand
; offset. Preserves D3-D7/A0-A6; CCR reflects D0. No writes to inputs.
classify	.block
	movem.l d3/d7/a0-a1, -(sp)
	move.l d0, d3
	move.l a0, d0
	beq.w malformed
	move.l a1, d0
	beq.w malformed
	cmpi.l #7, d1
	bne.w malformed
	cmpi.b #$99, (a1)
	bne.w malformed
	cmpi.b #$83, 5(a1)
	bne.w malformed
	tst.b 6(a1)
	bne.w malformed
	cmpi.l #4, d3
	blo.w malformed
	cmpi.l #256, d3
	bhi.w malformed
	moveq #0, d0
	move.b (a0), d0
	addq.l #1, d0
	cmp.l d3, d0
	bne.w malformed
	move.b 1(a0), d0
	andi.b #$c0, d0
	bne.w malformed
	move.l #$99, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	move.b 1(a0), d0
	andi.b #24, d0
	bne.w ordinary
	cmpi.l #9, d3
	blo.w ordinary
	cmpi.b #7, 4(a0)
	bne.w ordinary
	cmpi.b #1, 5(a0)
	bhi.w ordinary
	move.b 6(a0), d0
	cmp.b 1(a1), d0
	bne.w ordinary
	move.b 7(a0), d0
	cmp.b 2(a1), d0
	bne.w ordinary
	tst.b 8(a0)
	bne.w malformed
	cmpi.l #9, d3
	beq.w malformed
	moveq #-1, d1
	moveq #9, d2
	cmpi.l #17, d3
	blo.w matched
	cmpi.b #1, 9(a0)
	bhi.w matched
	cmpi.b #1, 13(a0)
	bhi.w matched
	move.b 14(a0), d0
	cmp.b 3(a1), d0
	bne.w matched
	move.b 15(a0), d0
	cmp.b 4(a1), d0
	bne.w matched
	tst.b 12(a0)
	bne.w malformed
	tst.b 16(a0)
	bne.w malformed
	cmpi.l #17, d3
	beq.w malformed
	moveq #0, d1
	move.b 10(a0), d1
	lsl.w #8, d1
	move.b 11(a0), d1
	moveq #17, d2
matched
	move.l #$83, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	moveq #abi.PRVM_LOOP_MATCHED, d0
	bra.w done
ordinary
	moveq #abi.PRVM_LOOP_ORDINARY, d0
	bra.w empty
malformed
	moveq #abi.PRVM_LOOP_MALFORMED, d0
empty
	moveq #-1, d1
	moveq #0, d2
done
	movem.l (sp)+, d3/d7/a0-a1
	tst.l d0
	rts
	.bend  ; classify
	.endsection
	.endmodule

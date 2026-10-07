; Shared package-owned loop envelope; EXVM owns expression syntax.
; @opforge-owner: prvm.amigaos.packed_loop
	.module prvm.amigaos.packed_loop
	.cpu 68020
	.use prvm.amigaos.abi as abi
	.include "telemetry_macros.i"
	.priv
MAX_RECORD_BYTES = 256
NON_STATEMENT_FLAGS = $18; omitted and layout-only records
UNSUPPORTED_FLAGS = $c0; preparation-only and reserved flags
CONTEXT_UNSUPPORTED_FLAGS = NON_STATEMENT_FLAGS+UNSUPPORTED_FLAGS
	.pub
	.section code, kind=code
; Base pointers address LoopRecord.Length / LoopProgram.Opcode directly.
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
	cmpi.l #abi.LOOP_PROGRAM_BYTES, d1
	bne.w absent
	cmpi.b #abi.OP_LOOP_HEADER, (a2)
	bne.w absent
	cmpi.b #abi.OP_PUBLISH, abi.LoopProgram.Publish(a2)
	bne.w absent
	tst.b abi.LoopProgram.End(a2)
	bne.w absent
	move.l d0, d1
	move.l a1, d2
	sub.l a0, d2
	cmpi.l #abi.LoopRecord.MarkerKind, d2
	bne.w absent
	move.b abi.LoopRecord.Flags(a0), d3
	andi.b #CONTEXT_UNSUPPORTED_FLAGS, d3
	bne.w absent
	cmpi.b #abi.PRVM_TOKEN_KIND_DOT, abi.LoopRecord.HeadDot(a0)
	bne.w absent
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, abi.LoopRecord.HeadKind(a0)
	bhi.w absent
	tst.b abi.LoopRecord.HeadQualifier(a0)
	bne.w absent
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, abi.LoopRecord.VariableKind(a0)
	bhi.w absent
	tst.b abi.LoopRecord.VariableQualifier(a0)
	bne.w absent
	move.b abi.LoopRecord.HeadHigh(a0), d3
	cmp.b abi.LoopProgram.ForHigh(a2), d3
	bne.w absent
	move.b abi.LoopRecord.HeadLow(a0), d3
	cmp.b abi.LoopProgram.ForLow(a2), d3
	bne.w absent
	moveq #0, d2
	move.b abi.LoopProgram.InHigh(a2), d2
	lsl.w #8, d2
	move.b abi.LoopProgram.InLow(a2), d2
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
	cmpi.l #abi.LOOP_PROGRAM_BYTES, d1
	bne.w malformed
	cmpi.b #abi.OP_LOOP_HEADER, (a1)
	bne.w malformed
	cmpi.b #abi.OP_PUBLISH, abi.LoopProgram.Publish(a1)
	bne.w malformed
	tst.b abi.LoopProgram.End(a1)
	bne.w malformed
	cmpi.l #abi.LoopRecord.HeadDot, d3
	blo.w malformed
	cmpi.l #MAX_RECORD_BYTES, d3
	bhi.w malformed
	moveq #0, d0
	move.b (a0), d0
	addq.l #1, d0
	cmp.l d3, d0
	bne.w malformed
	move.b abi.LoopRecord.Flags(a0), d0
	andi.b #UNSUPPORTED_FLAGS, d0
	bne.w malformed
	move.l #abi.OP_LOOP_HEADER, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	move.b abi.LoopRecord.Flags(a0), d0
	andi.b #NON_STATEMENT_FLAGS, d0
	bne.w ordinary
	cmpi.l #abi.LoopRecord.VariableKind, d3
	blo.w ordinary
	cmpi.b #abi.PRVM_TOKEN_KIND_DOT, abi.LoopRecord.HeadDot(a0)
	bne.w ordinary
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, abi.LoopRecord.HeadKind(a0)
	bhi.w ordinary
	move.b abi.LoopRecord.HeadHigh(a0), d0
	cmp.b abi.LoopProgram.ForHigh(a1), d0
	bne.w ordinary
	move.b abi.LoopRecord.HeadLow(a0), d0
	cmp.b abi.LoopProgram.ForLow(a1), d0
	bne.w ordinary
	tst.b abi.LoopRecord.HeadQualifier(a0)
	bne.w malformed
	cmpi.l #abi.LoopRecord.VariableKind, d3
	beq.w malformed
	moveq #-1, d1
	moveq #abi.LoopRecord.VariableKind, d2
	cmpi.l #abi.LOOP_EXPRESSION_OFFSET, d3
	blo.w matched
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, abi.LoopRecord.VariableKind(a0)
	bhi.w matched
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, abi.LoopRecord.MarkerKind(a0)
	bhi.w matched
	move.b abi.LoopRecord.MarkerHigh(a0), d0
	cmp.b abi.LoopProgram.InHigh(a1), d0
	bne.w matched
	move.b abi.LoopRecord.MarkerLow(a0), d0
	cmp.b abi.LoopProgram.InLow(a1), d0
	bne.w matched
	tst.b abi.LoopRecord.VariableQualifier(a0)
	bne.w malformed
	tst.b abi.LoopRecord.MarkerQualifier(a0)
	bne.w malformed
	cmpi.l #abi.LOOP_EXPRESSION_OFFSET, d3
	beq.w malformed
	moveq #0, d1
	move.b abi.LoopRecord.VariableHigh(a0), d1
	lsl.w #8, d1
	move.b abi.LoopRecord.VariableLow(a0), d1
	moveq #abi.LOOP_EXPRESSION_OFFSET, d2
matched
	move.l #abi.OP_PUBLISH, d7
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

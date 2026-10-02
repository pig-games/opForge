; Shared policy-driven initial target selection over canonical TKVM records.
; @opforge-owner: prvm.amigaos.target_bootstrap
	.module prvm.amigaos.target_bootstrap
	.cpu 68020
	.use prvm.amigaos.abi as abi
	.include "telemetry_macros.i"
	.pub
	.section code, kind=code
; A0=request,D0=available bytes. D0=status,D1=result count,D2=0,D3=bytes.
; SOURCE is decoded lexeme storage; TOKEN holds native TKVM 20-byte rows.
; Result spans use lexeme offsets. Preserves D4-D7/A3-A6; CCR reflects D0.
run	.block
	movem.l d4-d7/a3-a6, -(sp)
	move.l a0, d1
	beq.w invalidFrame
	btst #0, d1
	bne.w invalidFrame
	cmpi.l #abi.PRVM_REQUEST_FRAME_SIZE, d0
	blo.w invalidFrame
	movea.l a0, a4
	cmpi.l #abi.PRVM_MAGIC_OPRP, abi.PRVM_FRAME_MAGIC(a4)
	bne.w invalidFrame
	cmpi.w #abi.PRVM_ABI_VERSION_V1, abi.PRVM_FRAME_ABI_VERSION(a4)
	bne.w invalidFrame
	cmpi.w #abi.PRVM_REQUEST_FRAME_SIZE, abi.PRVM_FRAME_FRAME_SIZE(a4)
	blo.w invalidFrame
	cmpi.w #abi.PRVM_ENTRY_KIND_TARGET_BOOTSTRAP, abi.PRVM_FRAME_ENTRY_KIND(a4)
	bne.w invalidFrame
	tst.w abi.PRVM_FRAME_CALL_MODE(a4)
	bne.w invalidFrame
	cmpi.l #abi.PRVM_PARSER_CONTRACT_VERSION_V2, abi.PRVM_FRAME_PARSER_CONTRACT_VERSION(a4)
	bne.w invalidFrame
	tst.l abi.PRVM_FRAME_FLAGS(a4)
	bne.w invalidFrame
	cmpi.l #abi.PRVM_TOKEN_RECORD_SIZE, abi.PRVM_FRAME_TOKEN_RECORD_SIZE(a4)
	bne.w invalidFrame
	tst.l abi.PRVM_FRAME_RESULT_CAPACITY(a4)
	bmi.w invalidFrame
	move.l abi.PRVM_FRAME_STEP_BUDGET(a4), d7
	bmi.w invalidFrame
	move.l abi.PRVM_FRAME_TOKEN_COUNT(a4), d6
	bmi.w invalidFrame
	move.l abi.PRVM_FRAME_SOURCE_LEN(a4), d0
	bmi.w invalidFrame
	movea.l abi.PRVM_FRAME_SOURCE_PTR(a4), a3
	tst.l d0
	beq.w sourceReady
	move.l a3, d0
	beq.w invalidFrame
sourceReady
	movea.l abi.PRVM_FRAME_TOKEN_PTR(a4), a6
	move.l abi.PRVM_FRAME_PROGRAM_PTR(a4), d0
	beq.w invalidProgram
	movea.l d0, a5
	move.l abi.PRVM_FRAME_PROGRAM_LEN(a4), d4
	bmi.w invalidProgram
	cmpi.l #4, d4
	blo.w invalidProgram
	cmpi.b #$97, (a5)+
	bne.w invalidProgram
	moveq #0, d5
	move.b (a5)+, d5
	tst.l d5
	beq.w invalidProgram
	subq.l #2, d4
validatePolicy
	cmpi.l #4, d4
	blo.w invalidProgram
	cmpi.b #1, (a5)
	blo.w invalidProgram
	cmpi.b #2, (a5)+
	bhi.w invalidProgram
	moveq #0, d0
	move.b (a5)+, d0
	beq.w invalidProgram
	subq.l #2, d4
	move.l d4, d1
	subq.l #2, d1
	cmp.l d1, d0
	bhi.w invalidProgram
	sub.l d0, d4
	move.l d0, d2
	subq.l #1, d2
validateName
	move.b (a5)+, d1
	cmpi.b #'A', d1
	blo.w invalidProgram
	cmpi.b #'Z', d1
	bls.w validNameByte
	cmpi.b #'a', d1
	blo.w invalidProgram
	cmpi.b #'z', d1
	bhi.w invalidProgram
validNameByte
	dbra d2, validateName
	subq.l #1, d5
	bne.w validatePolicy
	cmpi.l #2, d4
	bne.w invalidProgram
	cmpi.b #$83, (a5)+
	bne.w invalidProgram
	tst.b (a5)
	bne.w invalidProgram
	tst.l d6
	beq.w tokensValidated
	move.l a6, d0
	beq.w invalidFrame
	btst #0, d0
	bne.w invalidFrame
	movea.l a6, a0
	move.l d6, d4
validateTokens
	cmpi.w #40, (a0)
	bhi.w invalidFrame
	move.l 12(a0), d0
	cmp.l abi.PRVM_FRAME_SOURCE_LEN(a4), d0
	bhi.w invalidFrame
	move.l abi.PRVM_FRAME_SOURCE_LEN(a4), d1
	sub.l d0, d1
	cmp.l 16(a0), d1
	blo.w invalidFrame
	adda.w #abi.PRVM_TOKEN_RECORD_SIZE, a0
	subq.l #1, d4
	bne.w validateTokens
tokensValidated
	bsr.w tick
	bne.w finish
	tst.l d6
	beq.w empty
	cmpi.l #2, d6
	blo.w stop
	cmpi.w #abi.PRVM_TOKEN_KIND_DOT, (a6)
	bne.w stop
	cmpi.w #abi.PRVM_TOKEN_KIND_IDENTIFIER, 20(a6)
	bne.w stop
	movea.l abi.PRVM_FRAME_PROGRAM_PTR(a4), a5
	moveq #0, d4
	move.b 1(a5), d4
	addq.l #2, a5
findPolicy
	bsr.w tick
	bne.w finish
	moveq #0, d5
	move.b (a5)+, d5
	moveq #0, d2
	move.b (a5)+, d2
	cmp.l 36(a6), d2
	bne.w nextPolicy
	movea.l a3, a0
	adda.l 32(a6), a0
	movea.l a5, a1
	move.l d2, d3
compareName
	moveq #0, d0
	move.b (a0)+, d0
	moveq #0, d1
	move.b (a1)+, d1
	bsr.w foldPair
	cmp.b d1, d0
	bne.w nextPolicy
	subq.l #1, d3
	bne.w compareName
	cmpi.l #2, d5
	beq.w target
	cmpi.l #3, d6
	bne.w invalidToken
	cmpi.w #abi.PRVM_TOKEN_KIND_IDENTIFIER, 40(a6)
	bne.w invalidToken
	bsr.w validateOperand
	bne.w finish
	bra.w empty
nextPolicy
	adda.l d2, a5
	subq.l #1, d4
	bne.w findPolicy
	bra.w stop
target
	cmpi.l #3, d6
	bne.w invalidToken
	move.w 40(a6), d0
	cmpi.w #abi.PRVM_TOKEN_KIND_IDENTIFIER, d0
	beq.w targetKind
	cmpi.w #2, d0
	beq.w targetKind
	cmpi.w #3, d0
	bne.w invalidToken
targetKind
	bsr.w validateOperand
	bne.w finish
	moveq #1, d5
	bra.w emit
stop
	moveq #2, d5
	moveq #0, d2
	moveq #0, d3
emit
	bsr.w tick
	bne.w finish
	move.l abi.PRVM_FRAME_RESULT_PTR(a4), d0
	beq.w invalidFrame
	btst #0, d0
	bne.w invalidFrame
	cmpi.l #1, abi.PRVM_FRAME_RESULT_CAPACITY(a4)
	blo.w overflow
	movea.l d0, a0
	moveq #7, d0
clearResult
	clr.l (a0)+
	dbra d0, clearResult
	movea.l abi.PRVM_FRAME_RESULT_PTR(a4), a0
	move.w #abi.PRVM_RESULT_TARGET_BOOTSTRAP, (a0)
	move.w d5, 2(a0)
	move.l d2, 4(a0)
	move.l d3, 8(a0)
	moveq #1, d1
	moveq #abi.PRVM_RESULT_RECORD_SIZE, d3
	moveq #0, d0
	bra.w done
empty
	moveq #0, d0
	bra.w finish
invalidFrame
	moveq #abi.PRVM_STATUS_INVALID_ARGUMENT, d0
	bra.w finish
invalidToken
	moveq #abi.PRVM_STATUS_INVALID_TOKEN, d0
	bra.w finish
invalidProgram
	moveq #abi.PRVM_STATUS_INVALID_PROGRAM, d0
	bra.w finish
overflow
	moveq #abi.PRVM_STATUS_OUTPUT_OVERFLOW, d0
finish
	moveq #0, d1
	clr.l d3
done
	clr.l d2
	tst.l d0
	movem.l (sp)+, d4-d7/a3-a6
	rts
	.bend  ; run
	.priv
; Validate a selected operand span; D2/D3 return decoded offset/length.
; Clobbers A0/D0/D4, CCR reflects D0.
validateOperand	.block
	move.l 56(a6), d3
	beq.w invalid
	cmpi.l #255, d3
	bhi.w invalid
	move.l 52(a6), d2
	movea.l a3, a0
	adda.l d2, a0
	move.l d3, d4
loop
	tst.b (a0)+
	beq.w invalid
	subq.l #1, d4
	bne.w loop
	moveq #0, d0
	rts
invalid
	moveq #abi.PRVM_STATUS_INVALID_TOKEN, d0
	rts
	.bend  ; validateOperand
; D7=remaining request budget. D0/status and CCR; other registers preserved.
tick	.block
	tst.l d7
	beq.s exhausted
	subq.l #1, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	moveq #0, d0
	rts
exhausted
	moveq #abi.PRVM_STATUS_BUDGET_EXCEEDED, d0
	rts
	.bend  ; tick
; ASCII fold the two compared policy bytes in D0/D1. CCR unspecified.
foldPair	.block
	cmpi.b #'A', d0
	blo.s firstReady
	cmpi.b #'Z', d0
	bhi.s firstReady
	addi.b #32, d0
firstReady
	cmpi.b #'A', d1
	blo.s done
	cmpi.b #'Z', d1
	bhi.s done
	addi.b #32, d1
done
	rts
	.bend  ; foldPair
	.endsection
	.endmodule

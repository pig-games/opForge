; Focused guest harness for opasm assembly-session initialization.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent

	.module opasm.session.init.harness
	.cpu 68020

	.use opasm.amigaos.engine as engine

HARNESS_FAIL = 20

	.section entry, kind=code
	.pub

; Prove the actual session initializer with two nonzero memory patterns.
;
; Inputs:
; - None.
;
; Outputs:
; - D0: zero on success; HARNESS_FAIL on the first mismatch.
;
; Clobbers:
; - D0-D6/A0-A3/CCR.
;
; CCR:
; - Reflects D0 on return.
start	.block
	move.l #$a5a5a5a5, d0
	bsr.w verifyPattern
	bne.s return
	move.l #$5a5a5a5a, d0
	bsr.w verifyPattern

return
	rts
	.bend  ; start

	.priv

; Seed session boundaries, initialize the real engine, and check the result.
; Inputs: D0.L = repeated nonzero poison pattern.
; Outputs: D0 = zero on success; HARNESS_FAIL on mismatch.
; Clobbers: D0-D6/A0-A3/CCR.
; CCR: reflects D0 on return.
verifyPattern	.block
	move.l d0, d6

	; FlowPending is the exported word immediately before session-owned state.
	move.w d6, engine.OpasmEngineFlowPending

	; Live initialization deliberately leaves unused statement rows untouched.
	lea engine.OpasmEngineStmtLineTable.l, a0
	move.l d6, (a0)
	move.l #engine.NATIVE_STATEMENT_TABLE_CAPACITY - 1, d5
	lsl.l #2, d5
	move.l d6, 0(a0, d5.l)

	; The final six bytes exposed by the public presence-map accessor are the
	; historical under-clear boundary. Never write beyond the public capacity.
	jsr engine.opasmEngineGetImagePresentBufferPtrV1
	adda.l #engine.NATIVE_IMAGE_BUFFER_CAPACITY - 6, a0
	moveq #5, d5
seedTail
	move.b d6, (a0)+
	dbra d5, seedTail

	; initSessionV1 promises status zero and preserves its A0/D1 inputs. D2 and
	; A2 retain the expected stack and pointer values for the harness checks.
	move.l sp, d2
	lea CpuText.l, a0
	movea.l a0, a2
	move.l #$13579bdf, d1
	jsr engine.initSessionV1
	tst.l d0
	bne.w fail
	cmpa.l d2, sp
	bne.w fail
	cmpa.l a2, a0
	bne.w fail
	cmpi.l #$13579bdf, d1
	bne.w fail

	cmp.w engine.OpasmEngineFlowPending, d6
	bne.w fail

	lea engine.OpasmEngineStmtLineTable.l, a0
	move.l #engine.NATIVE_STATEMENT_TABLE_CAPACITY - 1, d5
	lsl.l #2, d5
.ifdef OPFORGE_SESSION_CLEAR_ALL_STATEMENTS
	tst.l (a0)
	bne.w fail
	tst.l 0(a0, d5.l)
	bne.w fail
.else
	cmp.l (a0), d6
	bne.w fail
	cmp.l 0(a0, d5.l), d6
	bne.w fail
.endif

	jsr engine.opasmEngineGetImagePresentBufferPtrV1
	adda.l #engine.NATIVE_IMAGE_BUFFER_CAPACITY - 6, a0
	moveq #5, d5
checkTail
	tst.b (a0)+
	bne.w fail
	dbra d5, checkTail

	jsr engine.opasmEngineGetSessionCpuNamePtrV1
	cmpi.b #'6', (a0)+
	bne.w fail
	cmpi.b #'8', (a0)+
	bne.w fail
	cmpi.b #'0', (a0)+
	bne.w fail
	cmpi.b #'2', (a0)+
	bne.w fail
	cmpi.b #'0', (a0)+
	bne.w fail
	tst.b (a0)
	bne.w fail

	moveq #0, d0
	rts

fail
	moveq #HARNESS_FAIL, d0
	rts
	.bend  ; verifyPattern

	.endsection

	.section data, kind=data
CpuText
	.byte "68020", 0
	.endsection

	.output "build/opasm_session_init_harness", format=hunk, sections=entry, code, data, bss
	.endmodule

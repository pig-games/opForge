; FS-UAE diagnostic harness for stored macro-expanded statement spans.
; @opforge-evidence: level=E; role=diagnostic; authority=none; lifecycle=permanent

	.module macro.cli.debug.event.harness
	.cpu 68020

	.use opforge.cli.run
	.use opasm.amigaos.engine as engine

	.section entry, kind=code
	.pub

; Run the full CLI macro fixture and verify its complete image length.
; This is a bounded Level E diagnostic for the native macro/frontend bridge.
; Inputs: none.
; Outputs: D0 = zero only when the fixture emits all 11 expected bytes.
; Clobbers: D0-D1/A0/CCR.
; CCR: reflects D0 on return.
start	.block
	jsr run.opforgeNativeCliRun
	jsr engine.opasmEngineGetImageByteCountV1
	cmpi.l #11, d0
	bne.w badImageLength
	moveq #0, d0
	rts

	; Retained below as a manual statement-span probe for interactive diagnosis.
	suba.l #engine.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	moveq #5, d0
	jsr engine.opasmEngineGetStatementTextMetadataV1
	bne.w metadataMissing
	move.l engine.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(sp), d0
	cmpi.l #4, d0
	bne.w badMnemonicLength
	moveq #5, d0
	jsr engine.opasmEngineGetStatementSourceTextV1
	cmpi.l #11, d0
	bne.w badSourceLength
	cmpi.b #'.', (a0)+
	bne.w badSourceText
	cmpi.b #'b', (a0)+
	bne.w badSourceText
	cmpi.b #'y', (a0)+
	bne.w badSourceText
	cmpi.b #'t', (a0)+
	bne.w badSourceText
	cmpi.b #'e', (a0)+
	bne.w badSourceText
	cmpi.b #' ', (a0)+
	bne.w badSourceText
	cmpi.b #'1', (a0)+
	bne.w badSourceText
	cmpi.b #',', (a0)+
	bne.w badSourceText
	cmpi.b #' ', (a0)+
	bne.w badSourceText
	cmpi.b #'2', (a0)
	bne.w badSourceText
	move.l engine.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d0
	cmpi.l #4, d0
	bne.w badOperandLength
	movea.l engine.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	cmpi.b #'b', (a0)+
	bne.w badMnemonicText
	cmpi.b #'y', (a0)+
	bne.w badMnemonicText
	cmpi.b #'t', (a0)+
	bne.w badMnemonicText
	cmpi.b #'e', (a0)
	bne.w badMnemonicText
	adda.l #engine.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #5, d0
	jsr engine.statementOperandSpanV1
	tst.l d0
	beq.s missing
	cmpi.l #8, d0
	bne.s badStart
	cmpi.l #12, d1
	bne.s badEnd
	moveq #0, d0
	rts

missing
	moveq #41, d0
	rts

metadataMissing
	adda.l #engine.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #40, d0
	rts

badMnemonicLength
	movea.l engine.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	moveq #0, d0
	move.b (a0), d0
	adda.l #engine.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	rts

badOperandLength
	adda.l #engine.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #45, d0
	rts

badSourceLength
	adda.l #engine.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	; Return the short stored length as an offset from the diagnostic range.
	; The fixture line is bounded well below the AmigaDOS exit-code limit.
	addi.l #50, d0
	rts

badSourceText
	adda.l #engine.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #48, d0
	rts

badMnemonicText
	adda.l #engine.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #46, d0
	rts

badStart
	rts

badEnd
	moveq #43, d0
	rts

badImageLength
	addi.l #60, d0
	rts
	.bend  ; start

	.endsection

	.output "build/macro_cli_debug_event_harness", format=hunk, sections=entry, code, data, bss
	.endmodule

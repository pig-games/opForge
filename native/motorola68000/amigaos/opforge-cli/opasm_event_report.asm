; CLI renderer for structured opasm event records.

	.module opforge.cli.opasm_event_report
	.cpu 68020

	.use opasm.amigaos.callback_abi as abi
	.use opasm.amigaos.engine as engine

	.use opforge.cli.dos
	.use opforge.cli.text_output
	.use opforge.cli.strings

	.section code, kind=code
	.pub

; Render a sequence of opasm event records through the CLI report path.
;
; Inputs:
; - A0: OPASM_EVENT_* record buffer.
; - D0: event count.
;
; Outputs:
; - D0: 0 after all records have been rendered.
;
; Clobbers: D0/CCR; D1-D2/A0-A1 are preserved.
; CCR: unspecified on return.
opforgeNativeCliRenderOpasmEventsV1	.block
	movem.l d1-d2/a0-a1, -(sp)
	movea.l a0, a1
	move.w d0, d2
	beq.s done

loop
	movea.l a1, a0
	jsr opforgeNativeCliRenderOpasmEventV1
	adda.l #abi.OPASM_EVENT_BYTES, a1
	subq.w #1, d2
	bne.s loop

done
	movem.l (sp)+, d1-d2/a0-a1
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliRenderOpasmEventsV1

; Render one opasm event record through the CLI report path.
;
; Inputs:
; - A0: OPASM_EVENT_* record.
;
; Outputs:
; - D0: 0 after the record has been rendered or ignored.
;
; Clobbers: D0/CCR; D1-D2/A0-A2 are preserved.
; CCR: unspecified on return.
opforgeNativeCliRenderOpasmEventV1	.block
	movem.l d1-d2/a0-a2, -(sp)
	movea.l a0, a2
	move.w abi.OPASM_EVENT_KIND(a2), d0
	cmpi.w #abi.OPASM_EVENT_PASS_BEGIN, d0
	beq.w passBegin
	cmpi.w #abi.OPASM_EVENT_PASS_OK, d0
	beq.w passOk
	cmpi.w #abi.OPASM_EVENT_LABEL_STORED, d0
	beq.w labelStored
	cmpi.w #abi.OPASM_EVENT_LABEL_DUPLICATE, d0
	beq.w labelDuplicate
	cmpi.w #abi.OPASM_EVENT_IMAGE_CAPACITY_EXCEEDED, d0
	beq.w imageCapacity
	cmpi.w #abi.OPASM_EVENT_SELECTOR_STATUS_OK, d0
	beq.w selectorOk
	cmpi.w #abi.OPASM_EVENT_UNKNOWN_MNEMONIC, d0
	beq.w unknownMnemonic
	cmpi.w #abi.OPASM_EVENT_UNSUPPORTED_ADDRESSING, d0
	beq.w unsupportedAddressing
	cmpi.w #abi.OPASM_EVENT_UNRESOLVED_LABEL, d0
	beq.w unresolvedLabel
	cmpi.w #abi.OPASM_EVENT_BAD_ORG, d0
	beq.w badOrg
	cmpi.w #abi.OPASM_EVENT_SERVICE_FAILURE, d0
	beq.w serviceFailure
	bra.w done

passBegin
	move.w abi.OPASM_EVENT_PASS(a2), d0
	cmpi.w #1, d0
	beq.s passOneBegin
	cmpi.w #2, d0
	beq.s passTwoBegin
	bra.w done

passOneBegin
	move.l #strings.NativePassOneText, d1
	bra.w reportText

passTwoBegin
	move.l #strings.NativePassTwoText, d1
	bra.w reportText

passOk
	move.w abi.OPASM_EVENT_PASS(a2), d0
	cmpi.w #1, d0
	beq.s passOneOk
	cmpi.w #2, d0
	beq.s passTwoOk
	bra.w done

passOneOk
	move.l #strings.NativePassOneOkText, d1
	bra.w reportText

passTwoOk
	move.l #strings.NativePassTwoOkText, d1
	bra.w reportText

labelStored
	move.l #strings.NativeLabelText, d1
	jsr dos.putStr
	bsr.w reportEventText
	jsr text_output.opforgeNativeCliPutSpace
	move.l abi.OPASM_EVENT_VALUE(a2), d0
	jsr text_output.opforgeNativeCliPutHexU32
	move.l #strings.NewlineText, d1
	bra.w reportText

labelDuplicate
	move.l #strings.NativeDuplicateLabelText, d1
	jsr dos.putErrStr
	bsr.w reportEventErrorText
	move.l #strings.NewlineText, d1
	bra.w reportErrorText

imageCapacity
	move.l #strings.NativeImageCapacityText, d1
	jsr dos.putErrStr
	move.l abi.OPASM_EVENT_VALUE(a2), d0
	beq.w done
	move.l #strings.NativeImageCapacityRequestText, d1
	jsr dos.putErrStr
	move.l abi.OPASM_EVENT_VALUE(a2), d0
	jsr text_output.opforgeNativeCliPutErrU16Decimal
	move.l #strings.NewlineText, d1
	bra.w reportErrorText

selectorOk
	move.l #strings.NativeSelectorStatusOkText, d1
	bra.w reportText

unknownMnemonic
	move.l #strings.NativeUnknownMnemonicText, d1
	jsr dos.putErrStr
	moveq #0, d0
	move.w abi.OPASM_EVENT_STMT_INDEX(a2), d0
	jsr text_output.opforgeNativeCliPutErrU16Decimal
	jsr text_output.opforgeNativeCliPutErrSpace
	suba.l #engine.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	moveq #0, d0
	move.w abi.OPASM_EVENT_STMT_INDEX(a2), d0
	jsr engine.opasmEngineGetStatementTextMetadataV1
	bne.s unknownMnemonicDone
	move.l engine.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), d1
	jsr dos.putErrStr

unknownMnemonicDone
	adda.l #engine.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	move.l #strings.NewlineText, d1
	bra.w reportErrorText

unsupportedAddressing
	move.l #strings.NativeUnsupportedAddressingText, d1
	bra.w reportErrorText

unresolvedLabel
	move.l #strings.NativeUnresolvedLabelText, d1
	bra.s reportErrorText

badOrg
	move.l #strings.NativeBadOrgText, d1
	jsr dos.putErrStr
	moveq #0, d0
	move.w abi.OPASM_EVENT_STMT_INDEX(a2), d0
	jsr text_output.opforgeNativeCliPutErrU16Decimal
	move.l #strings.NewlineText, d1
	bra.s reportErrorText

serviceFailure
	bsr.w reportEventErrorText
	move.l #strings.NewlineText, d1
	bra.s reportErrorText

reportText
	jsr dos.putStr
	bra.s done

reportErrorText
	jsr dos.putErrStr

done
	movem.l (sp)+, d1-d2/a0-a2
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliRenderOpasmEventV1

	.priv

; Render the event-owned text payload for the current record.
;
; Inputs:
; - A2: OPASM_EVENT_* record.
;
; Outputs:
; - text bytes have been emitted to stdout.
reportEventText	.block
	movem.l d0-d2/a0-a2, -(sp)
	movea.l abi.OPASM_EVENT_TEXT_PTR(a2), a0
	move.w abi.OPASM_EVENT_TEXT_LEN(a2), d2
	beq.s done
	cmpi.w #255, d2
	bls.s sizeOk
	move.w #255, d2

sizeOk
	lea EventTextBuffer, a1
	move.w d2, d0
	beq.s emit
	subq.w #1, d0

copyLoop
	move.b (a0)+, (a1)+
	dbf d0, copyLoop

emit
	clr.b (a1)
	move.l #EventTextBuffer, d1
	jsr dos.putStr

done
	movem.l (sp)+, d0-d2/a0-a2
	rts
	.bend  ; reportEventText

; Render the event-owned text payload through the CLI ErrorOutput path.
; Inputs: A2 = OPASM_EVENT_* record.
; Outputs: diagnostic text is emitted when the payload is non-empty.
; Clobbers: none; D0-D2/A0-A2 are protected by the routine body.
; CCR: unspecified on return.
reportEventErrorText	.block
	movem.l d0-d2/a0-a2, -(sp)
	movea.l abi.OPASM_EVENT_TEXT_PTR(a2), a0
	move.w abi.OPASM_EVENT_TEXT_LEN(a2), d2
	beq.s done
	cmpi.w #255, d2
	bls.s sizeOk
	move.w #255, d2

sizeOk
	lea EventTextBuffer, a1
	move.w d2, d0
	beq.s emit
	subq.w #1, d0

copyLoop
	move.b (a0)+, (a1)+
	dbf d0, copyLoop

emit
	clr.b (a1)
	move.l #EventTextBuffer, d1
	jsr dos.putErrStr

done
	movem.l (sp)+, d0-d2/a0-a2
	rts
	.bend  ; reportEventErrorText

	.endsection

	.section bss, kind=bss

EventTextBuffer
	.res byte, 256

	.endsection
	.endmodule

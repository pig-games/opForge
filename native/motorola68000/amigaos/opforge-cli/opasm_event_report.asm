; CLI renderer for structured opasm event records.

	.module opforge.cli.opasm_event_report
	.cpu 68020

	.use opasm.amigaos.callback_abi (OPASM_EVENT_KIND, OPASM_EVENT_PASS, OPASM_EVENT_TEXT_PTR)
	.use opasm.amigaos.callback_abi (OPASM_EVENT_TEXT_LEN, OPASM_EVENT_VALUE, OPASM_EVENT_BYTES)
	.use opasm.amigaos.callback_abi (OPASM_EVENT_PASS_BEGIN, OPASM_EVENT_PASS_OK)
	.use opasm.amigaos.callback_abi (OPASM_EVENT_LABEL_STORED, OPASM_EVENT_LABEL_DUPLICATE)
	.use opasm.amigaos.callback_abi (OPASM_EVENT_IMAGE_CAPACITY_EXCEEDED, OPASM_EVENT_SELECTOR_STATUS_OK)
	.use opasm.amigaos.callback_abi (OPASM_EVENT_UNKNOWN_MNEMONIC, OPASM_EVENT_UNSUPPORTED_ADDRESSING)
	.use opasm.amigaos.callback_abi (OPASM_EVENT_UNRESOLVED_LABEL, OPASM_EVENT_BAD_ORG)
	.use opasm.amigaos.callback_abi (OPASM_EVENT_SERVICE_FAILURE)

	.use opforge.cli.dos
	.use opforge.cli.text_output (opforgeNativeCliPutHexU32, opforgeNativeCliPutSpace)
	.use opforge.cli.strings (NativePassOneText, NativePassTwoText, NativePassOneOkText, NativePassTwoOkText)
	.use opforge.cli.strings (NativeLabelText, NativeDuplicateLabelText, NativeImageCapacityText)
	.use opforge.cli.strings (NativeSelectorStatusOkText, NativeUnknownMnemonicText)
	.use opforge.cli.strings (NativeUnsupportedAddressingText, NativeUnresolvedLabelText)
	.use opforge.cli.strings (NativeBadOrgText, NewlineText)

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
opforgeNativeCliRenderOpasmEventsV1	.block
	movem.l d1-d2/a0-a1, -(sp)
	movea.l a0, a1
	move.w d0, d2
	beq.s done

loop
	movea.l a1, a0
	jsr opforgeNativeCliRenderOpasmEventV1
	adda.l #OPASM_EVENT_BYTES, a1
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
opforgeNativeCliRenderOpasmEventV1	.block
	movem.l d1-d2/a0-a1, -(sp)
	movea.l a0, a1
	move.w OPASM_EVENT_KIND(a1), d0
	cmpi.w #OPASM_EVENT_PASS_BEGIN, d0
	beq.w passBegin
	cmpi.w #OPASM_EVENT_PASS_OK, d0
	beq.w passOk
	cmpi.w #OPASM_EVENT_LABEL_STORED, d0
	beq.w labelStored
	cmpi.w #OPASM_EVENT_LABEL_DUPLICATE, d0
	beq.w labelDuplicate
	cmpi.w #OPASM_EVENT_IMAGE_CAPACITY_EXCEEDED, d0
	beq.w imageCapacity
	cmpi.w #OPASM_EVENT_SELECTOR_STATUS_OK, d0
	beq.w selectorOk
	cmpi.w #OPASM_EVENT_UNKNOWN_MNEMONIC, d0
	beq.w unknownMnemonic
	cmpi.w #OPASM_EVENT_UNSUPPORTED_ADDRESSING, d0
	beq.w unsupportedAddressing
	cmpi.w #OPASM_EVENT_UNRESOLVED_LABEL, d0
	beq.w unresolvedLabel
	cmpi.w #OPASM_EVENT_BAD_ORG, d0
	beq.w badOrg
	cmpi.w #OPASM_EVENT_SERVICE_FAILURE, d0
	beq.w serviceFailure
	bra.w done

passBegin
	move.w OPASM_EVENT_PASS(a1), d0
	cmpi.w #1, d0
	beq.s passOneBegin
	cmpi.w #2, d0
	beq.s passTwoBegin
	bra.w done

passOneBegin
	move.l #NativePassOneText, d1
	bra.w reportText

passTwoBegin
	move.l #NativePassTwoText, d1
	bra.w reportText

passOk
	move.w OPASM_EVENT_PASS(a1), d0
	cmpi.w #1, d0
	beq.s passOneOk
	cmpi.w #2, d0
	beq.s passTwoOk
	bra.w done

passOneOk
	move.l #NativePassOneOkText, d1
	bra.w reportText

passTwoOk
	move.l #NativePassTwoOkText, d1
	bra.w reportText

labelStored
	move.l #NativeLabelText, d1
	jsr dos.putStr
	bsr.w reportEventText
	jsr opforgeNativeCliPutSpace
	move.l OPASM_EVENT_VALUE(a1), d0
	jsr opforgeNativeCliPutHexU32
	move.l #NewlineText, d1
	bra.s reportText

labelDuplicate
	move.l #NativeDuplicateLabelText, d1
	jsr dos.putStr
	bsr.w reportEventText
	move.l #NewlineText, d1
	bra.s reportText

imageCapacity
	move.l #NativeImageCapacityText, d1
	bra.s reportText

selectorOk
	move.l #NativeSelectorStatusOkText, d1
	bra.s reportText

unknownMnemonic
	move.l #NativeUnknownMnemonicText, d1
	bra.s reportText

unsupportedAddressing
	move.l #NativeUnsupportedAddressingText, d1
	bra.s reportText

unresolvedLabel
	move.l #NativeUnresolvedLabelText, d1
	bra.s reportText

badOrg
	move.l #NativeBadOrgText, d1
	bra.s reportText

serviceFailure
	bsr.w reportEventText
	move.l #NewlineText, d1

reportText
	jsr dos.putStr

done
	movem.l (sp)+, d1-d2/a0-a1
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliRenderOpasmEventV1

	.priv

reportEventText	.block
	movem.l d0-d2/a0-a2, -(sp)
	movea.l OPASM_EVENT_TEXT_PTR(a1), a0
	move.w OPASM_EVENT_TEXT_LEN(a1), d2
	beq.s done
	lea EventCharBuffer, a2

loop
	move.b (a0)+, (a2)
	clr.b 1(a2)
	move.l #EventCharBuffer, d1
	jsr dos.putStr
	subq.w #1, d2
	bne.s loop

done
	movem.l (sp)+, d0-d2/a0-a2
	rts
	.bend  ; reportEventText

	.endsection

	.section data, kind=data

EventCharBuffer
	.byte 0, 0

	.endsection
	.endmodule

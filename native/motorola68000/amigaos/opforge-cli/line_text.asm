; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.line_text
	.cpu 68020

	.use opforge.cli.state
	.use opforge.cli.constants
	.use opforge.cli.strings
	.use opforge.cli.module_use
	.use opforge.cli.token_util

	.section code, kind=code
	.pub

; Skip leading spaces/tabs in one source-line slice.
; Inputs: A0 = current line pointer; D0 = remaining byte count.
; Outputs: A0 advanced past leading spaces/tabs; D0 = remaining byte count after trimming.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliSkipLineWhitespace	.block
	tst.l d0
	beq.s done
	cmpi.b #' ', (a0)
	beq.s one
	cmpi.b #9, (a0)
	bne.s done

one
	addq.l #1, a0
	subq.l #1, d0
	bra.s opforgeNativeCliSkipLineWhitespace

done
	tst.l d0
	rts
	.bend  ; opforgeNativeCliSkipLineWhitespace

opforgeNativeCliLineStartsWith	.block
	cmp.l d1, d0
	bcs.s no
	movea.l a0, a2
	movea.l a1, a3
	move.l d1, d2
	beq.s boundary
	subq.l #1, d2

loop
	move.b (a2)+, d3
	move.b (a3)+, d4
	cmpi.b #'A', d3
	bcs.s compare
	cmpi.b #'Z', d3
	bhi.s compare
	addi.b #32, d3

compare
	cmp.b d4, d3
	bne.s no
	dbra d2, loop

boundary
	cmp.l d1, d0
	beq.s yes
	move.b 0(a0, d1.l), d3
	cmpi.b #' ', d3
	beq.s yes
	cmpi.b #9, d3
	beq.s yes
	cmpi.b #';', d3
	beq.s yes
	moveq #0, d0
	rts

yes
	moveq #1, d0
	rts

no
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliLineStartsWith

opforgeNativeCliCopyLineWord	.block
	move.l #constants.TOKEN_BUFFER_CAPACITY - 1, d6
	clr.l d5

loop
	tst.l d0
	beq.s done
	moveq #0, d2
	move.b (a0), d2
	cmpi.b #' ', d2
	beq.s done
	cmpi.b #9, d2
	beq.s done
	cmpi.b #';', d2
	beq.s done
	cmpi.b #'(', d2
	beq.s done
	cmpi.b #',', d2
	beq.s done
	tst.l d6
	beq.s fail
	move.b d2, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	addq.l #1, d5
	subq.l #1, d6
	bra.s loop

done
	clr.b (a1)
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliCopyLineWord

opforgeNativeCliCopyOperandText	.block
	movem.l d0-d4/a0-a1, -(sp)
	clr.w d5
	move.l #constants.TOKEN_BUFFER_CAPACITY - 1, d4

loop
	tst.l d0
	beq.s done
	moveq #0, d2
	move.b (a0), d2
	beq.s done
	cmpi.b #';', d2
	beq.s done
	cmpi.b #10, d2
	beq.s done
	cmpi.b #13, d2
	beq.s done
	tst.l d4
	beq.s done
	move.b d2, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	addq.w #1, d5
	subq.l #1, d4
	bra.s loop

done
	bsr.w trim
	clr.b (a1)
	movem.l (sp)+, d0-d4/a0-a1
	rts

trim
	tst.w d5
	beq.s trimDone
	movea.l a1, a0

trimLoop
	tst.w d5
	beq.s trimSetEnd
	subq.l #1, a0
	move.b (a0), d0
	cmpi.b #' ', d0
	beq.s trimOne
	cmpi.b #9, d0
	beq.s trimOne
	bra.s trimSetEnd

trimOne
	subq.w #1, d5
	bra.s trimLoop

trimSetEnd
	movea.l a0, a1
	addq.l #1, a1

trimDone
	rts
	.bend  ; opforgeNativeCliCopyOperandText

opforgeNativeCliCopyUseToken	.block
	move.l #constants.TOKEN_BUFFER_CAPACITY - 1, d6

loop
	tst.l d0
	beq.s done
	moveq #0, d2
	move.b (a0), d2
	cmpi.b #' ', d2
	beq.s done
	cmpi.b #9, d2
	beq.s done
	cmpi.b #';', d2
	beq.s done
	cmpi.b #'(', d2
	beq.s done
	cmpi.b #')', d2
	beq.s done
	cmpi.b #',', d2
	beq.s done
	tst.l d6
	beq.s fail
	move.b d2, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	subq.l #1, d6
	bra.s loop

done
	clr.b (a1)
	moveq #0, d1
	rts

fail
	clr.b (a1)
	moveq #1, d1
	rts
	.bend  ; opforgeNativeCliCopyUseToken

; Parse optional `as <alias>` text after one selective `use` item.
; Inputs: A0 = current line pointer; D0 = remaining byte count.
; Outputs: D0 = remaining byte count after any parsed alias text; D1 = 0 on success, 1 on malformed alias; state.NativeCliIncludeTarget = parsed alias when present.
; Clobbers: D0-D1/D6/A0-A1/CCR.
; CCR: reflects D1 on return.
opforgeNativeCliParseUseOptionalAlias	.block
	movem.l d6/a1, -(sp)
	move.l d0, d6
	lea strings.AsKeywordText, a1
	moveq #2, d1
	bsr.w opforgeNativeCliLineStartsWith
	beq.s none
	move.l d6, d0
	addq.l #2, a0
	subq.l #2, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea state.NativeCliIncludeTarget, a1
	bsr.w opforgeNativeCliCopyUseToken
	tst.l d1
	bne.s fail
	tst.b state.NativeCliIncludeTarget
	beq.s fail
	moveq #0, d1
	bra.s return

none
	move.l d6, d0
	moveq #0, d1
	bra.s return

fail
	moveq #1, d1

return
	movem.l (sp)+, d6/a1
	rts
	.bend  ; opforgeNativeCliParseUseOptionalAlias

; Parse a parenthesized selective/wildcard `use (...)` item list.
; Inputs: A0 = current line pointer at the list payload; D0 = remaining byte count; D4 = current import owner/module index.
; Outputs: D0 = remaining byte count after the closing `)` on success; D1 = 0 on success, 1 on malformed input; D7 = emitted selective-import count on success.
; Clobbers: D0-D7/A0-A1/CCR.
; CCR: reflects D1 on return.
opforgeNativeCliParseUseItems
	move.w d4, d5
	clr.w d7
	bsr.w opforgeNativeCliSkipLineWhitespace
	beq.w opforgeNativeCliParseUseItemsFail
	cmpi.b #')', (a0)
	beq.w opforgeNativeCliParseUseItemsFail
	cmpi.b #'*', (a0)
	beq.w opforgeNativeCliParseUseWildcard

opforgeNativeCliParseUseItemLoop
	lea state.NativeCliArgToken, a1
	bsr.w opforgeNativeCliCopyUseToken
	tst.l d1
	bne.w opforgeNativeCliParseUseItemsFail
	tst.b state.NativeCliArgToken
	beq.w opforgeNativeCliParseUseItemsFail
	cmpi.b #'*', state.NativeCliArgToken
	bne.s opforgeNativeCliParseUseItemNameOk
	lea state.NativeCliArgToken, a1
	tst.b 1(a1)
	beq.w opforgeNativeCliParseUseItemsFail

opforgeNativeCliParseUseItemNameOk
	clr.b state.NativeCliIncludeTarget
	bsr.w opforgeNativeCliSkipLineWhitespace
	bsr.w opforgeNativeCliParseUseOptionalAlias
	tst.l d1
	bne.w opforgeNativeCliParseUseItemsFail
	moveq #0, d3
	tst.b state.NativeCliIncludeTarget
	beq.s opforgeNativeCliParseUseItemNoAliasFlag
	moveq #1, d3

opforgeNativeCliParseUseItemNoAliasFlag
	move.l d0, -(sp)
	move.w d5, d4
	jsr module_use.opforgeNativeCliRecordImportSelect
	bne.w opforgeNativeCliParseUseItemsFailPop
	jsr module_use.opforgeNativeCliEmitImportSelectRecord
	move.l (sp)+, d0
	addq.w #1, d7
	bsr.w opforgeNativeCliSkipLineWhitespace
	beq.w opforgeNativeCliParseUseItemsFail
	cmpi.b #')', (a0)
	beq.s opforgeNativeCliParseUseItemsClose
	cmpi.b #',', (a0)
	bne.w opforgeNativeCliParseUseItemsFail
	addq.l #1, a0
	subq.l #1, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	bra.w opforgeNativeCliParseUseItemLoop

opforgeNativeCliParseUseItemsClose
	addq.l #1, a0
	subq.l #1, d0
	moveq #0, d1
	rts

opforgeNativeCliParseUseWildcard
	addq.l #1, a0
	subq.l #1, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	move.l d0, d6
	lea strings.AsKeywordText, a1
	moveq #2, d1
	bsr.w opforgeNativeCliLineStartsWith
	bne.s opforgeNativeCliParseUseItemsFail
	move.l d6, d0
	beq.s opforgeNativeCliParseUseItemsFail
	cmpi.b #')', (a0)
	bne.s opforgeNativeCliParseUseItemsFail
	addq.l #1, a0
	subq.l #1, d0
	moveq #0, d3
	move.w d5, d4
	bsr.w module_use.opforgeNativeCliEmitImportWildcardRecord
	moveq #0, d1
	rts

opforgeNativeCliParseUseItemsFailPop
	addq.l #4, sp

opforgeNativeCliParseUseItemsFail
	moveq #1, d1
	rts

	.endsection
	.endmodule

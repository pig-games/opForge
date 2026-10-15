; Experimental acquisition adapter. TKVM still runs once; this indexed spelling
; view is transient and never replaces declaration binding or packed execution.
; A5=session/A6=scratch throughout. Cached only until the next physical tokenize.
; D0/CCR=status; all other registers preserved.
ensureBinaryView	.block
	movem.l d1-d7/a0-a4, -(sp)
	movea.l a6, a4
	adda.l #BINARY_VIEW_READY, a4
	tst.l (a4)
	bne.w ready
	movea.l a6, a0
	adda.l #BINARY_VIEW_FRAME, a0
	move.l Frame.Source(a5), binary_spelling.Frame.Source(a0)
	move.l Frame.SourceBytes(a5), binary_spelling.Frame.SourceBytes(a0)
	lea TOKENS(a6), a1
	move.l a1, binary_spelling.Frame.Tokens(a0)
	move.l LINE_FRAME+writer.Frame.Count(a6), binary_spelling.Frame.TokenCount(a0)
	movea.l a6, a1
	adda.l #BINARY_VIEW, a1
	move.l a1, binary_spelling.Frame.Result(a0)
	move.l #binary_spelling.MAX_BYTES, binary_spelling.Frame.Capacity(a0)
	jsr binary_spelling.run
	bne.w done
	movea.l a6, a0
	adda.l #BINARY_VIEW_BYTES, a0
	move.l d3, (a0)
	move.l #1, (a4)
ready
	moveq #0, d0
done
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
	.bend  ; ensureBinaryView

; A0=initialized ordinary descriptor request; replace only its input view.
; Package-selected program, body mode, lexical ownership and outputs are intact.
; D0/CCR=status,D1=count,D2=error,D3=published bytes; preserves D4-D7/A0-A4.
binaryDescriptors	.block
	movem.l d4-d7/a0-a4, -(sp)
	movea.l a0, a4
	bsr.w ensureBinaryView
	bne.w done
	bsr.w binaryRequest
	moveq #parser_abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr binary_runtime.run
done
	movem.l (sp)+, d4-d7/a0-a4
	tst.l d0
	rts
	.bend  ; binaryDescriptors

; A0=request over one plan's selected raw spelling. Interpret that range in the
; already captured view. Comments are outside the token/spelling service input.
; D0/CCR=status,D1=count,D2=error,D3=published bytes; preserves D4-D7/A0-A4.
binaryFragments	.block
	movem.l d4-d7/a0-a4, -(sp)
	movea.l a0, a4
	bsr.w ensureBinaryView
	bne.w done
	move.l parser_abi.PRVM_FRAME_SOURCE_PTR(a4), d4
	sub.l Frame.Source(a5), d4
	move.l d4, d5
	add.l parser_abi.PRVM_FRAME_SOURCE_LEN(a4), d5
	bsr.w binaryRequest
	movea.l parser_abi.PRVM_FRAME_SOURCE_PTR(a4), a1
	moveq #0, d0
	move.w binary_spelling.Header.SpellingBytes(a1), d0
	cmp.l d0, d5
	bls.w endReady
	move.l d0, d5
endReady
	move.l d4, d1
	move.l d5, d2
	moveq #parser_abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr binary_runtime.runRange
done
	movem.l (sp)+, d4-d7/a0-a4
	tst.l d0
	rts
	.bend  ; binaryFragments

; A4=request, A6=scratch; view must have been built successfully.
; A0=request on return; clobbers A1/D0/CCR only.
binaryRequest	.block
	movea.l a6, a1
	adda.l #BINARY_VIEW, a1
	move.l a1, parser_abi.PRVM_FRAME_SOURCE_PTR(a4)
	movea.l a6, a1
	adda.l #BINARY_VIEW_BYTES, a1
	move.l (a1), parser_abi.PRVM_FRAME_SOURCE_LEN(a4)
	clr.l parser_abi.PRVM_FRAME_TOKEN_PTR(a4)
	clr.l parser_abi.PRVM_FRAME_TOKEN_COUNT(a4)
	movea.l a4, a0
	rts
	.bend  ; binaryRequest

; A1=selected line row. Code-only spelling and its recipe must own the same
; extent. The eager row includes comments; they are absent from the binary view.
; D0/CCR=status; other registers preserved. Updates only Row.SpellingEnd.
binaryLineExtent	.block
	movem.l d1/a0, -(sp)
	bsr.w ensureBinaryView
	bne.w done
	movea.l a6, a0
	adda.l #BINARY_VIEW, a0
	moveq #0, d1
	move.w binary_spelling.Header.SpellingBytes(a0), d1
	cmp.l macro_plans.Row.SpellingEnd(a1), d1
	bcc.w ready
	move.l d1, macro_plans.Row.SpellingEnd(a1)
ready
	moveq #0, d0
done
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; binaryLineExtent

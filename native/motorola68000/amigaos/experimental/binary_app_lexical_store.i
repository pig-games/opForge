; Lexical storage follows the capture policy; semantic completion stays global.
; These hooks only change ownership after successful shared-frontend publication.

; Configure the active fresh frontend's private backing when disk policy applies.
; D0/CCR=status; all other registers preserved.
configureLexicalStorage	.block
	movem.l a0-a2, -(sp)
	tst.l CaptureDirectory
	beq.w good
	movea.l Front+frontend.Frame.Scratch, a0
	adda.l #frontend.SCOPE_STATE+layout.LEXICAL_STATE, a0
	movea.l DosBase, a1
	movea.l CaptureDirectory, a2
	jsr lexical.configure
	bra.w done
good
	moveq #0, d0
done
	movem.l (sp)+, a0-a2
	tst.l d0
	rts
	.bend  ; configureLexicalStorage

; Seal the active frontend's newly published module range. No binding, import,
; nominal identity or placement phase runs here. Late readers use numeric copies.
; D0/CCR=status; all other registers preserved.
sealLexicalStorage	.block
	movem.l a0-a3, -(sp)
	movea.l Front+frontend.Frame.Scratch, a3
	adda.l #frontend.SCOPE_STATE, a3
	movea.l scopes.MODULE_STATE+modules.OWNERS_POINTER(a3), a1
	movea.l scopes.MODULE_STATE+modules.FLAGS_POINTER(a3), a2
	lea layout.LEXICAL_STATE(a3), a0
	moveq #modules.PUBLIC+modules.MODULE_ID, d0
	jsr lexical.seal
	movem.l (sp)+, a0-a3
	tst.l d0
	rts
	.bend  ; sealLexicalStorage

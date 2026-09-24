; Preparation-only selection of one requested module in a discovered file.
; Sibling source is scanned only for literal module boundaries. No source text
; survives preparation, and selected lines still use the normal binary frontend.

; D0=1 lower this line, 0 skip it, -1 invalid duplicate selection. Other
; registers preserved. RequestedModule=0 leaves entry/explicit-order files alone.
selectLine .block
	tst.l SelectedFileDerived
	bne.w lower
	tst.l RequestedModule
	beq.w lower
	movem.l d1-d5/a0-a3, -(sp)
	movea.l LineBuffer, a0
	move.l LineUsed, d4
spaces
	tst.l d4
	beq.w skip
	cmpi.b #' ', (a0)
	beq.w advance
	cmpi.b #9, (a0)
	bne.w directive
advance
	addq.l #1, a0
	subq.l #1, d4
	bra.w spaces
directive
	cmpi.b #'.', (a0)
	bne.w nonDirective
	addq.l #1, a0
	subq.l #1, d4
	cmpi.l #1, SelectionState
	beq.w endWord
	lea SelectedModuleKeyword, a1
	moveq #6, d3
	bsr.w matchSelectionWord
	tst.l d0
	beq.w skip
	tst.l d4
	beq.w skip
	cmpi.b #' ', (a0)
	beq.w nameSpace
	cmpi.b #9, (a0)
	bne.w skip
nameSpace
	tst.l d4
	beq.w skip
	cmpi.b #' ', (a0)
	beq.w nameAdvance
	cmpi.b #9, (a0)
	bne.w nameStart
nameAdvance
	addq.l #1, a0
	subq.l #1, d4
	bra.w nameSpace
nameStart
	movea.l a0, a3
	moveq #0, d5
nameEnd
	tst.l d4
	beq.w compareName
	move.b (a0), d0
	cmpi.b #' ', d0
	beq.w compareName
	cmpi.b #9, d0
	beq.w compareName
	cmpi.b #13, d0
	beq.w compareName
	cmpi.b #';', d0
	beq.w compareName
	addq.l #1, a0
	addq.l #1, d5
	subq.l #1, d4
	bra.w nameEnd
compareName
	cmp.l RequestedNameBytes, d5
	bne.w skip
	movea.l RequestedName, a2
compare
	moveq #0, d1
	move.b (a3)+, d1
	bsr.w foldSelectionByte
	move.b d1, d2
	moveq #0, d1
	move.b (a2)+, d1
	bsr.w foldSelectionByte
	cmp.b d1, d2
	bne.w skip
	subq.l #1, d5
	bne.w compare
	tst.l SelectionState
	bne.w invalid
	move.l #1, SelectionState
	bra.w lowerSaved
endWord
	lea SelectedEndmoduleKeyword, a1
	moveq #9, d3
	bsr.w matchSelectionWord
	tst.l d0
	beq.w ordinary
	tst.l d4
	beq.w close
	move.b (a0), d0
	cmpi.b #' ', d0
	beq.w close
	cmpi.b #9, d0
	beq.w close
	cmpi.b #13, d0
	beq.w close
	cmpi.b #';', d0
	bne.w ordinary
close
	move.l #2, SelectionState
	bra.w ordinary
nonDirective
	cmpi.l #1, SelectionState
	bne.w skip
ordinary
	moveq #1, d0
	bra.w done
skip
	moveq #0, d0
	bra.w done
invalid
	moveq #-1, d0
	bra.w done
lowerSaved
	moveq #1, d0
done
	movem.l (sp)+, d1-d5/a0-a3
	rts
lower
	moveq #1, d0
	rts
	.bend  ; selectLine

; A0=first keyword byte,D4=remaining bytes,A1=expected lowercase text,
; D3=expected length. On match D0=1 and A0/D4 advance; otherwise D0=0.
matchSelectionWord .block
	cmp.l d3, d4
	blo.w no
	move.l d3, d2
next
	moveq #0, d1
	move.b (a0)+, d1
	bsr.w foldSelectionByte
	cmp.b (a1)+, d1
	bne.w no
	subq.l #1, d2
	bne.w next
	sub.l d3, d4
	moveq #1, d0
	rts
no
	moveq #0, d0
	rts
	.bend  ; matchSelectionWord

; D1=ASCII byte; fold spelling only. Other registers preserved.
foldSelectionByte .block
	cmpi.b #'A', d1
	blo.w done
	cmpi.b #'Z', d1
	bhi.w done
	addi.b #32, d1
done
	rts
	.bend  ; foldSelectionByte

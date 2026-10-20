; Read-only proof over the current branch program's candidate metadata.
	.priv
BranchHeader	.struct
Operation	.byte ?
OpcodeInput	.byte ?
TargetInput	.byte ?
Unresolved	.byte ?
Count	.byte ?
	.endstruct
BRANCH_HEADER_BYTES = BranchHeader.Count+1
BranchCandidatePrefix	.struct
Id	.byte ?
Automatic	.byte ?
SuffixBytes	.byte ?
	.endstruct
BRANCH_CANDIDATE_PREFIX_BYTES = BranchCandidatePrefix.SuffixBytes+1
BranchCandidateBody	.struct
Width	.byte ?
Endian	.byte ?
Adjustment	.long ?
Placeholder	.long ?
ReservedCount	.byte ?
	.endstruct
BRANCH_CANDIDATE_BODY_BYTES = BranchCandidateBody.ReservedCount+1
BRANCH_SELECT = 1
BRANCH_END = $ff
BRANCH_CANDIDATE_LIMIT = 16
BRANCH_SUFFIX_LIMIT = 8
BRANCH_RESERVED_LIMIT = 8
BranchInterval	.struct
Selected	.long ?
Minimum	.long ?
Maximum	.long ?
Placeholder	.long ?
Previous	.long ?
Found	.word ?
Earlier	.word ?
Chosen	.word ?
ReservedSeen	.word ?
	.endstruct
BRANCH_INTERVAL_BYTES = BranchInterval.ReservedSeen+2
	.pub

; A1/D1=current validated branch program/bytes,D2=selected candidate ID,
; D3/D4=signed target-minus-PC bounds before the package position adjustment.
; D0/CCR=0 proves that AUTO must select this candidate throughout the interval.
; Other registers preserved; no Context or output owner is required or changed.
; Conservatively require the first candidate with any automatic-class mask:
; the original successful AUTO execution already proved its actual class.
; Later candidates cannot win while that first candidate remains valid.
; Unresolved is already validated by execution; it selects only a deferred
; placeholder candidate and cannot affect this resolved AUTO interval proof.
validateBranchInterval	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #BRANCH_INTERVAL_BYTES, sp
	move.l d2, BranchInterval.Selected(sp)
	move.l d3, BranchInterval.Minimum(sp)
	move.l d4, BranchInterval.Maximum(sp)
	clr.w BranchInterval.Found(sp)
	clr.w BranchInterval.Earlier(sp)
	cmpi.l #255, d2
	bhi.w bad
	cmp.l d4, d3
	bgt.w bad
	movea.l a1, a0
	move.l a1, d0
	add.l d1, d0
	bcs.w bad
	movea.l d0, a5
	moveq #BRANCH_HEADER_BYTES, d0
	bsr.w intervalAvailable
	bne.w bad
	cmpi.b #BRANCH_SELECT, BranchHeader.Operation(a0)
	bne.w bad
	tst.b BranchHeader.OpcodeInput(a0)
	bne.w bad
	tst.b BranchHeader.TargetInput(a0)
	bne.w bad
	moveq #0, d7
	move.b BranchHeader.Count(a0), d7
	beq.w bad
	cmpi.l #BRANCH_CANDIDATE_LIMIT, d7
	bhi.w bad
	adda.w #BRANCH_HEADER_BYTES, a0
nextCandidate
	moveq #BRANCH_CANDIDATE_PREFIX_BYTES, d0
	bsr.w intervalAvailable
	bne.w bad
	moveq #0, d6
	move.b (a0)+, d6
	moveq #0, d5
	move.b (a0)+, d5
	moveq #0, d4
	move.b (a0)+, d4
	cmpi.l #BRANCH_SUFFIX_LIMIT, d4
	bhi.w bad
	clr.w BranchInterval.Chosen(sp)
	cmp.l BranchInterval.Selected(sp), d6
	bne.w candidateReady
	tst.w BranchInterval.Found(sp)
	bne.w bad
	tst.w BranchInterval.Earlier(sp)
	bne.w bad
	tst.l d5
	beq.w bad
	move.w #1, BranchInterval.Chosen(sp)
candidateReady
	move.l d4, d0
	addi.l #BRANCH_CANDIDATE_BODY_BYTES, d0
	bsr.w intervalAvailable
	bne.w bad
	adda.l d4, a0
	moveq #0, d2
	move.b (a0)+, d2
	cmpi.l #1, d2
	beq.w widthReady
	cmpi.l #2, d2
	beq.w widthReady
	cmpi.l #4, d2
	bne.w bad
widthReady
	cmpi.b #1, (a0)+
	bhi.w bad
	bsr.w readU32
	bne.w bad
	tst.w BranchInterval.Chosen(sp)
	beq.w adjustmentReady
	sub.l d0, BranchInterval.Minimum(sp)
	bvs.w bad
	sub.l d0, BranchInterval.Maximum(sp)
	bvs.w bad
	move.l BranchInterval.Minimum(sp), d0
	bsr.w fitsSignedWidth
	bne.w bad
	move.l BranchInterval.Maximum(sp), d0
	bsr.w fitsSignedWidth
	bne.w bad
adjustmentReady
	bsr.w readU32
	bne.w bad
	move.l d0, BranchInterval.Placeholder(sp)
	bsr.w fitsSignedWidth
	bne.w bad
	moveq #0, d4
	move.b (a0)+, d4
	cmpi.l #BRANCH_RESERVED_LIMIT, d4
	bhi.w bad
	move.l d4, d0
	lsl.l #2, d0
	bsr.w intervalAvailable
	bne.w bad
	clr.w BranchInterval.ReservedSeen(sp)
nextReserved
	tst.l d4
	beq.w candidateComplete
	bsr.w readU32
	bne.w bad
	move.l d0, d3
	bsr.w fitsSignedWidth
	bne.w bad
	cmp.l BranchInterval.Placeholder(sp), d3
	beq.w bad
	tst.w BranchInterval.ReservedSeen(sp)
	beq.w reservedReady
	cmp.l BranchInterval.Previous(sp), d3
	ble.w bad
reservedReady
	move.l d3, BranchInterval.Previous(sp)
	move.w #1, BranchInterval.ReservedSeen(sp)
	tst.w BranchInterval.Chosen(sp)
	beq.w reservedNext
	cmp.l BranchInterval.Minimum(sp), d3
	blt.w reservedNext
	cmp.l BranchInterval.Maximum(sp), d3
	ble.w bad  ; an interior reserved value invalidates the whole interval
reservedNext
	subq.l #1, d4
	bra.w nextReserved
candidateComplete
	tst.w BranchInterval.Chosen(sp)
	beq.w earlierReady
	move.w #1, BranchInterval.Found(sp)
earlierReady
	tst.l d5
	beq.w advance
	move.w #1, BranchInterval.Earlier(sp)
advance
	subq.l #1, d7
	bne.w nextCandidate
	moveq #1, d0
	bsr.w intervalAvailable
	bne.w bad
	cmpi.b #BRANCH_END, (a0)+
	bne.w bad
	cmpa.l a5, a0
	bne.w bad
	tst.w BranchInterval.Found(sp)
	beq.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	adda.w #BRANCH_INTERVAL_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; validateBranchInterval
	.priv

; A0=cursor,A5=end,D0=required bytes. D0/CCR=status; D1 scratch.
; Compare remaining length instead of allowing pointer addition to wrap.
intervalAvailable	.block
	move.l a5, d1
	sub.l a0, d1
	bcs.w bad
	cmp.l d0, d1
	blo.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; intervalAvailable

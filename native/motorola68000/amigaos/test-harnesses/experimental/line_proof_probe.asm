; Disposable owned whole-line storage and ordinary-plan equivalence checks.
; @opforge-owner: tests.amigaos.line_proof_probe
	.module tests.amigaos.line_proof_probe
	.cpu 68020
	.use experimental.amigaos.binary_line_proofs as proofs
	.use experimental.amigaos.binary_macro_plans as plans
	.use experimental.amigaos.binary_memory as memory
	.use prvm.amigaos.abi as abi
SOURCE_BYTES = SpellingEnd-Spelling
RECIPE_COUNT = 3
SENTINEL = $5a39c671
	.section code, kind=code
	.pub
; D0/CCR=0 success/1 failure; all other registers preserved. Every path releases
; all three owners. No parser, production selection or external logging involved.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	bsr.w cleanup
	lea ProofArena, a0
	move.l a0, PlanFrame+plans.Frame.Arena
	move.l #Root, PlanFrame+plans.Frame.Events
	move.l #1, PlanFrame+plans.Frame.Count
	move.l #Spelling, PlanFrame+plans.Frame.Source
	move.l #SOURCE_BYTES, PlanFrame+plans.Frame.SourceBytes
	move.l #DeclarationMap, PlanFrame+plans.Frame.PackedMap
	move.l #2, PlanFrame+plans.Frame.TokenCount
	move.l #Recipes, PlanFrame+plans.Frame.RecipeEvents
	move.l #RECIPE_COUNT, PlanFrame+plans.Frame.RecipeCount
	; A valid noncanonical root must fall back before allocating anything.
	move.w #1, Root+plans.Row.Flags
	lea PlanFrame, a0
	jsr proofs.create
	cmpi.l #proofs.STATUS_UNSUPPORTED, d0
	bne.w bad
	tst.l d1
	bne.w bad
	move.l ProofArena+memory.Block.Pointer, d0
	or.l ProofArena+memory.Block.Capacity, d0
	or.l ProofArena+memory.Block.Used, d0
	bne.w bad
	clr.w Root+plans.Row.Flags
	; The shared validator rejects malformed source extents and recipe coverage.
	move.l #SOURCE_BYTES+1, Root+plans.Row.SpellingEnd
	bsr.w rejectCreate
	bne.w bad
	move.l #SOURCE_BYTES, Root+plans.Row.SpellingEnd
	move.l #1, Recipes+plans.Row.SpellingStart
	bsr.w rejectCreate
	bne.w bad
	clr.l Recipes+plans.Row.SpellingStart
	lea PlanFrame, a0
	movem.l d2-d7/a0-a6, Before
	move.l sp, SavedSp
	jsr proofs.create
	movem.l d2-d7/a0-a6, After
	bne.w bad
	move.l d1, Handle
	move.l sp, d0
	cmp.l SavedSp, d0
	bne.w bad
	moveq #12, d0
	bsr.w preserved
	bne.w bad
	; The ordinary constructor on the original declaration is the oracle.
	move.l #Expected, PlanFrame+plans.Frame.Arena
	lea PlanFrame, a0
	jsr plans.create
	bne.w bad
	move.l #ProofArena, View+proofs.ViewFrame.Arena
	move.l Handle, View+proofs.ViewFrame.Handle
	move.l #Rows, View+proofs.ViewFrame.Rows
	move.l #Staging, View+proofs.ViewFrame.Staging
	move.l #proofs.MAX_ROWS, View+proofs.ViewFrame.Capacity
	lea View, a0
	movem.l d3-d7/a0/a3-a6, Before
	move.l sp, SavedSp
	jsr proofs.view
	movem.l d3-d7/a0/a3-a6, After
	bne.w bad
	cmpi.l #RECIPE_COUNT, d1
	bne.w bad
	tst.l d2
	bne.w bad
	cmpa.l #Rows, a1
	bne.w bad
	cmpa.l View+proofs.ViewFrame.Source, a2
	bne.w bad
	move.l sp, d0
	cmp.l SavedSp, d0
	bne.w bad
	moveq #9, d0
	bsr.w preserved
	bne.w bad
	cmpi.l #SOURCE_BYTES, View+proofs.ViewFrame.SourceBytes
	bne.w bad
	cmpi.l #2, View+proofs.ViewFrame.TokenCount
	bne.w bad
	; Reconstruct an ordinary plan with the unchanged declaration binding map.
	move.l #Actual, PlanFrame+plans.Frame.Arena
	move.l View+proofs.ViewFrame.Source, PlanFrame+plans.Frame.Source
	move.l #Rows, PlanFrame+plans.Frame.RecipeEvents
	lea PlanFrame, a0
	jsr plans.create
	bne.w bad
	move.l Expected+memory.Block.Used, d7
	cmp.l Actual+memory.Block.Used, d7
	bne.w bad
	movea.l Expected+memory.Block.Pointer, a0
	movea.l Actual+memory.Block.Pointer, a1
comparePlans
	move.b (a0)+, d0
	cmp.b (a1)+, d0
	bne.w bad
	subq.l #1, d7
	bne.w comparePlans
	; Force relocation; the retained numeric handle must still resolve freshly.
	move.l ProofArena+memory.Block.Pointer, d7
	lea ProofArena, a0
	move.l #4096, d0
	jsr memory.reserve
	bne.w bad
	cmp.l ProofArena+memory.Block.Pointer, d7
	beq.w bad
	lea ProofArena, a0
	move.l Handle, d1
	lea Staging, a3
	movem.l d3-d7/a0/a2-a6, Before
	move.l sp, SavedSp
	jsr proofs.sourceView
	movem.l d3-d7/a0/a2-a6, After
	bne.w bad
	cmpi.l #SOURCE_BYTES, d1
	bne.w bad
	cmpi.l #2, d2
	bne.w bad
	move.l sp, d0
	cmp.l SavedSp, d0
	bne.w bad
	moveq #10, d0
	bsr.w preserved
	bne.w bad
	lea Spelling, a0
	moveq #SOURCE_BYTES-1, d7
compareSource
	move.b (a0)+, d0
	cmp.b (a1)+, d0
	bne.w bad
	dbra d7, compareSource
	; A short caller buffer must not receive even the first expanded row.
	move.l #RECIPE_COUNT-1, View+proofs.ViewFrame.Capacity
	bsr.w rejectView
	bne.w bad
	move.l #proofs.MAX_ROWS, View+proofs.ViewFrame.Capacity
	; Staging is disposable; absent or overlapping buffers cannot change output.
	clr.l View+proofs.ViewFrame.Staging
	bsr.w rejectView
	bne.w bad
	move.l #Rows, View+proofs.ViewFrame.Staging
	bsr.w rejectView
	bne.w bad
	move.l #Rows+plans.ROW_BYTES, View+proofs.ViewFrame.Staging
	bsr.w rejectView
	bne.w bad
	move.l #Staging, View+proofs.ViewFrame.Staging
	; Corrupt each independent bounded representation dimension in turn.
	movea.l ProofArena+memory.Block.Pointer, a4
	move.l proofs.Container.End(a4), d7
	move.l #$ffffffff, proofs.Container.End(a4)
	bsr.w rejectView
	bne.w bad
	move.l d7, proofs.Container.End(a4)
	move.w #proofs.MAX_ROWS+1, proofs.Container.RecipeCount(a4)
	bsr.w rejectView
	bne.w bad
	move.w #RECIPE_COUNT, proofs.Container.RecipeCount(a4)
	move.w #1, proofs.HEADER_BYTES+proofs.Fragment.Start(a4)
	lea ProofArena, a0
	move.l Handle, d1
	lea Staging, a3
	jsr proofs.sourceView
	cmpi.l #proofs.STATUS_INVALID, d0
	bne.w bad
	bsr.w rejectView
	bne.w bad
	clr.w proofs.HEADER_BYTES+proofs.Fragment.Start(a4)
	; Offset+1 zero and misalignment are rejected before any arena access.
	clr.l View+proofs.ViewFrame.Handle
	bsr.w rejectView
	bne.w bad
	move.l #2, View+proofs.ViewFrame.Handle
	bsr.w rejectView
	bne.w bad
	move.l Handle, View+proofs.ViewFrame.Handle
	lea View, a0
	jsr proofs.view
	bne.w bad
	bsr.w cleanup
	moveq #0, d0
	bra.w done
bad
	bsr.w cleanup
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; run
	.priv
; Invalid source must neither allocate nor publish a handle.
rejectCreate	.block
	movem.l d1/a0, -(sp)
	lea PlanFrame, a0
	jsr proofs.create
	cmpi.l #proofs.STATUS_INVALID, d0
	bne.w bad
	tst.l d1
	bne.w bad
	tst.l ProofArena+memory.Block.Used
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; rejectCreate
; Every rejected view must leave the entire rows destination and all four
; published frame outputs untouched. D0/CCR=status; other registers preserved.
rejectView	.block
	movem.l d1-d2/a0-a1, -(sp)
	lea Rows, a1
	move.w #proofs.MAX_ROWS*plans.ROW_BYTES/4-1, d2
seed
	move.l #SENTINEL, (a1)+
	dbra d2, seed
	lea View+proofs.ViewFrame.Source, a1
	moveq #3, d2
seedOutputs
	move.l #SENTINEL, (a1)+
	dbra d2, seedOutputs
	lea View, a0
	jsr proofs.view
	cmpi.l #proofs.STATUS_INVALID, d0
	bne.w bad
	lea Rows, a1
	move.w #proofs.MAX_ROWS*plans.ROW_BYTES/4-1, d2
checkRows
	cmpi.l #SENTINEL, (a1)+
	bne.w bad
	dbra d2, checkRows
	lea View+proofs.ViewFrame.Source, a1
	moveq #3, d2
checkOutputs
	cmpi.l #SENTINEL, (a1)+
	bne.w bad
	dbra d2, checkOutputs
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; rejectView
; D0=number of saved registers minus one; D0/CCR=status, others preserved.
preserved	.block
	movem.l d1-d2/a0-a1, -(sp)
	move.l d0, d2
	lea Before, a0
	lea After, a1
next
	move.l (a0)+, d1
	cmp.l (a1)+, d1
	bne.w bad
	dbra d2, next
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; preserved
; Release all owners and restore mutable fixtures, including failure paths.
cleanup	.block
	movem.l d0/a0, -(sp)
	lea ProofArena, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea Expected, a0
	jsr plans.finish
	lea Actual, a0
	jsr plans.finish
	clr.w Root+plans.Row.Flags
	move.l #SOURCE_BYTES, Root+plans.Row.SpellingEnd
	clr.l Recipes+plans.Row.SpellingStart
	movem.l (sp)+, d0/a0
	rts
	.bend  ; cleanup
	.endsection
	.section data, kind=data
Root	.word abi.PRVM_RESULT_MACRO_LINE, 0
	.long 0, 2, 0, SOURCE_BYTES, 0, 0, 0
Recipes	.word abi.PRVM_RESULT_MACRO_LITERAL, 0
	.long 0, 0, 0, NamedStart-Spelling, 0, 0, 0
	.word abi.PRVM_RESULT_MACRO_NAMED, 0
	.long 0, 0, NamedStart-Spelling, PositionStart-Spelling
	.long NamedStart-Spelling+1, PositionStart-Spelling, 0
	.word abi.PRVM_RESULT_MACRO_POSITIONAL, 0
	.long 0, 0, PositionStart-Spelling, SOURCE_BYTES, 1, 0, 0
; A leading tab and quoted marker stay byte-for-byte literal spelling.
Spelling	.byte 9, 34, 64, 34, 32
NamedStart	.byte 92, "name"
PositionStart	.byte 92, "1"
SpellingEnd
	.align 2
DeclarationMap	.word 0, 4, 8
	.endsection
	.section bss, kind=bss
ProofArena	.res byte, memory.Block.Used+4
Expected	.res byte, plans.STATE_BYTES
Actual	.res byte, plans.STATE_BYTES
PlanFrame	.res byte, plans.FRAME_BYTES
View	.res byte, proofs.FRAME_BYTES
Rows	.res byte, proofs.MAX_ROWS*plans.ROW_BYTES
Staging	.res byte, proofs.SCRATCH_BYTES
Handle	.res long, 1
Before	.res long, 13
After	.res long, 13
SavedSp	.res long, 1
	.endsection
	.endmodule

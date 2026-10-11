; Selected-branch root eligibility, with no conditional or expression evaluator.
; PRVM owns branch roles; the existing dependency DFS owns constant evaluation.
; @opforge-owner: experimental.amigaos.binary_repeat_activation
	.module experimental.amigaos.binary_repeat_activation
	.cpu 68020
	.use experimental.amigaos.binary_dependency_layout as graph
	.use experimental.amigaos.binary_repeat_inventory as inventory
	.use experimental.amigaos.binary_package as pkg
	.use experimental.amigaos.binary_source as source
	.use opasm.amigaos.binary_expression as expression
	.use prvm.amigaos.packed_branch as branch
	.use prvm.amigaos.packed_loop as loopParser
	.use prvm.amigaos.abi as abi
	.include "memory_telemetry.i"
	.pub
	.section code, kind=code
; A0=graph.Frame initialized for the live iteration; Branch is the selected
; .if/.elseif/.else control offset. Activate directly owned constants until the
; next sibling control. Nested branches remain dormant until PRVM selects them.
; D0/CCR=status,D1=root capacity bound; other registers preserved. No record or
; live value is modified. Expression offsets come from the initial local index.
select	.block
	movem.l d2-d7/a0-a6, -(sp)
	.MEMORY_COUNTER_INC Selections
	movea.l a0, a6
	.MEMORY_COUNTER_INC_ZERO GlobalSelections, graph.Frame.Owner(a6)
	movea.l graph.Frame.Context(a6), a2
	movea.l graph.Frame.Records(a6), a4
	movea.l a4, a5
	adda.l graph.Frame.RecordBytes(a6), a5
	move.l graph.Frame.Branch(a6), d0
	cmp.l graph.Frame.RecordBytes(a6), d0
	bhs.w bad
	adda.l d0, a4
	bsr.w classify
	bne.w done
	cmpi.l #branch.CASE, d1
	beq.w selected
	cmpi.l #branch.DEFAULT, d1
	beq.w selected
	cmpi.l #branch.IF, d1
	beq.w selected
	cmpi.l #branch.ELSEIF, d1
	beq.w selected
	cmpi.l #branch.ELSE, d1
	bne.w bad
selected
	adda.l d4, a4
	moveq #0, d7
	moveq #0, d2
	moveq #0, d3  ; unentered nested loops cannot contribute selected roots
next
	cmpa.l a5, a4
	bhs.w bad
	bsr.w classify
	bne.w done
	bsr.w loopDepth
	bne.w done
	cmpi.l #branch.MATCH, d1
	beq.w opening
	cmpi.l #branch.IF, d1
	beq.w opening
	cmpi.l #branch.ENDMATCH, d1
	beq.w closing
	cmpi.l #branch.ENDIF, d1
	beq.w closing
	cmpi.l #branch.CASE, d1
	beq.w sibling
	cmpi.l #branch.DEFAULT, d1
	beq.w sibling
	cmpi.l #branch.ELSEIF, d1
	beq.w sibling
	cmpi.l #branch.ELSE, d1
	beq.w sibling
	tst.l d7
	bne.w advance
	tst.l d3
	bne.w advance
	cmpi.l #inventory.PackedRecord.Rhs+3, d4
	blo.w advance
	cmpi.b #1, inventory.PackedRecord.NameKind(a4)
	bhi.w advance
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, inventory.PackedRecord.Marker(a4)
	bne.w advance
	moveq #0, d6
	move.w inventory.PackedRecord.NameId(a4), d6
	cmp.l pkg.Context.Count(a2), d6
	bhs.w bad
	movea.l graph.Frame.Declarations(a6), a0
	move.l d6, d5
	lsl.l #inventory.DECL_SHIFT, d5
	moveq #0, d0
	move.w inventory.Decl.Owner(a0, d5.l), d0
	cmp.l graph.Frame.Owner(a6), d0
	bne.w advance
	move.w inventory.Decl.Kind(a0, d5.l), d0
	andi.w #inventory.KIND_MASK+inventory.CONDITIONAL, d0
	cmpi.w #inventory.IMMUTABLE+inventory.CONDITIONAL, d0
	bne.w bad
	move.l a4, d0
	sub.l graph.Frame.Records(a6), d0
	addi.l #inventory.PackedRecord.Rhs, d0
	move.l d6, d5
	lsl.l #graph.OFFSET_SHIFT, d5
	movea.l graph.Frame.Offsets(a6), a0
	tst.l graph.Offset.Expression(a0, d5.l)
	bne.w bad  ; multiple active definitions of one immutable ID
	move.l d0, graph.Offset.Expression(a0, d5.l)
	cmpi.b #expression.VALUE_TAG, inventory.PackedRecord.Rhs(a4)
	beq.w advance  ; compound declarations remain statement-time snapshots
	cmpi.b #expression.COMPILED_TAG, inventory.PackedRecord.Rhs(a4)
	bne.w bad
	movea.l graph.Frame.States(a6), a0
	move.b #graph.PENDING, 0(a0, d6.l)
	addq.l #1, d2
	.MEMORY_COUNTER_INC Roots
	bra.w advance
opening
	addq.l #1, d7
	bra.w advance
closing
	tst.l d7
	beq.w good
	subq.l #1, d7
	bra.w advance
sibling
	tst.l d7
	beq.w good
advance
	adda.l d4, a4
	bra.w next
good
	move.l d2, d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; select
	.priv
; A4=record,A5=stream end,A2=context. D0/CCR=status,D1=role,D4=extent.
; Other registers preserved; the same package-owned classifier runs at replay.
classify	.block
	movem.l a0-a1, -(sp)
	moveq #0, d4
	move.b inventory.PackedRecord.Length(a4), d4
	addq.w #1, d4
	move.l a5, d0
	sub.l a4, d0
	cmp.l d0, d4
	bhi.w bad
	movea.l a4, a0
	move.l d4, d0
	movea.l pkg.Context.Package(a2), a1
	adda.l pkg.Header.BranchPlan(a1), a1
	jsr branch.classify
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; classify

; A4/D4=record/extent,A2=context,D3=nested unentered loop depth.
; D0/CCR=status,D3=updated; others preserved. Shared PRVM alone supplies roles.
loopDepth	.block
	movem.l d1-d2/a0-a1, -(sp)
	movea.l a4, a0
	move.l d4, d0
	movea.l pkg.Context.Package(a2), a1
	move.l pkg.Header.LoopPlanBytes(a1), d1
	adda.l pkg.Header.LoopPlan(a1), a1
	jsr loopParser.classify
	cmpi.l #abi.PRVM_LOOP_MALFORMED, d0
	beq.w bad
	cmpi.l #abi.PRVM_LOOP_ORDINARY, d0
	beq.w good
	cmpi.l #abi.PRVM_LOOP_ENDFOR, d0
	beq.w closing
	cmpi.l #abi.PRVM_LOOP_ENDWHILE, d0
	beq.w closing
	cmpi.l #inventory.DEPTH_LIMIT, d3
	bhs.w bad
	addq.l #1, d3
	bra.w good
closing
	tst.l d3
	beq.w bad
	subq.l #1, d3
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; loopDepth
	.endsection
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	.pub
	.section bss, kind=bss
Selections	.res long, 1
Roots	.res long, 1
GlobalSelections	.res long, 1
	.endsection
.endif
.endif
	.endmodule

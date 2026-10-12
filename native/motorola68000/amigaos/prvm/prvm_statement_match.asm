; Execute package-selected typed matching over immutable prepared lexical facets.
; @opforge-owner: prvm.amigaos.statement_match
	.module prvm.amigaos.statement_match
	.cpu 68020
	.use prvm.amigaos.statement_abi as abi
	.include "telemetry_macros.i"
	.use prvm.amigaos.statement_inputs as inputs
	.use prvm.amigaos.statement_request as request
	.use prvm.amigaos.statement_plan as plan
	.use prvm.amigaos.statement_policy as policies
	.use prvm.amigaos.statement_facets as facets
	.priv
Local	.struct
Frame	.long ?
Budget	.long ?
BudgetPtr	.long ?
CaptureCount	.long ?
CaptureBytes	.long ?
LiteralCursor	.long ?
LiteralEnd	.long ?
PriorEnd	.long ?
HasPrior	.long ?
	.endstruct
LOCAL_BYTES = Local.HasPrior+4
	.pub
	.section code, kind=code
; A0=program,D0=bounded bytes. D0=status/CCR; other registers preserved.
; Pure validation uses the same current adjacency/type decoder as run.
validateProgram	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a3
	move.l d0, d7
	jsr inputs.span
	bne.w done
	lea -LOCAL_BYTES-abi.MATCH_FRAME_BYTES(sp), sp
	movea.l sp, a6
	lea LOCAL_BYTES(sp), a4
	move.l a3, (a4)  ; MatchFrame.ProgramPtr (zero offset)
	move.l d7, abi.MatchFrame.ProgramBytes(a4)
	move.l #abi.POLICY_TYPE_COUNT, Local.Budget(a6)
	lea Local.Budget(a6), a0
	move.l a0, Local.BudgetPtr(a6)
	bsr.w policy
	lea LOCAL_BYTES+abi.MATCH_FRAME_BYTES(sp), sp
done
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; validateProgram
; A0=MatchFrame,D0=available bytes. D0=status/CCR,D1=matched,D2=capture count.
; Other registers preserved. Output is untouched on errors and nonmatches.
; Workspace is caller-owned scratch; token text/numeric interpretation is prepared.
run	.block
	move.l a1, -(sp)
	suba.l a1, a1
	bsr.w runBudget
	movea.l (sp)+, a1
	tst.l d0
	rts
	.bend  ; run
; Same frame/output ABI as run, A1=shared budget cell (nonnull for nested calls).
; The cell is caller-owned, validated, and disjoint from all frame spans. A null
; cell is reserved for run's wrapper and selects its validated frame budget.
runBudget	.block
	.TELEMETRY_VM_ENTER runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	movem.l d3-d7/a0-a6, -(sp)
	lea -LOCAL_BYTES(sp), sp
	movea.l sp, a6
	movea.l a0, a4
	move.l a4, (a6)  ; Local.Frame (zero offset)
	jsr request.match
	bne.w finish
	move.l abi.MatchFrame.StepBudget(a4), Local.Budget(a6)
	move.l a1, Local.BudgetPtr(a6)
	tst.l Local.BudgetPtr(a6)
	bne.w sharedBudget
	lea Local.Budget(a6), a0
	move.l a0, Local.BudgetPtr(a6)
sharedBudget
	movea.l Local.BudgetPtr(a6), a0
	jsr inputs.budget
	bne.w finish
	clr.l Local.CaptureCount(a6)
	clr.l Local.CaptureBytes(a6)
	bsr.w policy
	bne.w finish
	movea.l abi.MatchFrame.TokenPtr(a4), a0
	move.l abi.MatchFrame.TokenCount(a4), d0
	movea.l abi.MatchFrame.ArenaPtr(a4), a1
	move.l abi.MatchFrame.ArenaBytes(a4), d1
	movea.l Local.BudgetPtr(a6), a2
	jsr facets.validate
	bne.w finish
	movea.l abi.MatchFrame.PlanPtr(a4), a0
	move.l abi.MatchFrame.PlanBytes(a4), d0
	movea.l abi.MatchFrame.WorkspacePtr(a4), a1
	move.l abi.MatchFrame.WorkspaceCapacity(a4), d1
	jsr plan.validate
	bne.w finish
	move.l d1, d6
	movea.l abi.MatchFrame.PlanPtr(a4), a5
	adda.w #abi.PLAN_HEADER_BYTES, a5
	movea.l abi.MatchFrame.TokenPtr(a4), a3
	movea.l abi.MatchFrame.WorkspacePtr(a4), a2
	moveq #0, d7
	moveq #0, d5
	moveq #0, d4
	moveq #0, d3
nextRow
	tst.l d6
	beq.w complete
	bsr.w tick
	bne.w finish
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	cmpi.b #abi.ATOM_OPEN, (a5)  ; AtomRow.Kind (zero offset)
	beq.w open
	cmpi.b #abi.ATOM_CLOSE, (a5)
	beq.w close
	cmp.l abi.MatchFrame.TokenCount(a4), d7
	bhs.w mismatch
	tst.l d5
	beq.w atom
	tst.l d3
	beq.w atom
	cmp.l abi.TokenView.ColStart(a3), d4
	bne.w mismatch
atom
	cmpi.b #abi.ATOM_CAPTURE, (a5)
	bne.w literal
	bsr.w accepts
	tst.l d0
	beq.w mismatch
	move.l abi.MatchFrame.WorkspaceCapacity(a4), d0
	sub.l Local.CaptureBytes(a6), d0
	cmpi.l #abi.CAPTURE_BYTES, d0
	blo.w overflow
	move.l abi.MatchFrame.OutputCapacity(a4), d0
	sub.l Local.CaptureBytes(a6), d0
	cmpi.l #abi.CAPTURE_BYTES, d0
	blo.w overflow
	move.l abi.AtomRow.PayloadOffset(a5), (a2)  ; Capture.NameOffset (zero offset)
	move.l abi.AtomRow.PayloadBytes(a5), abi.Capture.NameBytes(a2)
	move.l d7, abi.Capture.TokenIndex(a2)
	adda.w #abi.CAPTURE_BYTES, a2
	addq.l #1, Local.CaptureCount(a6)
	addi.l #abi.CAPTURE_BYTES, Local.CaptureBytes(a6)
	move.l abi.TokenView.ColEnd(a3), d4
	moveq #1, d3
	adda.w #abi.TOKEN_BYTES, a3
	addq.l #1, d7
	bra.w advance
literal
	move.l abi.AtomRow.PayloadBytes(a5), d0
	beq.w mismatch  ; the Rust oracle never consumes zero tokens for an empty literal
	movea.l abi.MatchFrame.PlanPtr(a4), a0
	adda.l abi.AtomRow.PayloadOffset(a5), a0
	move.l a0, Local.LiteralCursor(a6)
	adda.l d0, a0
	move.l a0, Local.LiteralEnd(a6)
	clr.l Local.HasPrior(a6)
literalToken
	move.l Local.LiteralCursor(a6), d0
	cmp.l Local.LiteralEnd(a6), d0
	beq.w literalComplete
	bsr.w tick
	bne.w finish
	cmp.l abi.MatchFrame.TokenCount(a4), d7
	bhs.w mismatch
	tst.l Local.HasPrior(a6)
	beq.w literalAdjacent
	move.l Local.PriorEnd(a6), d0
	cmp.l abi.TokenView.ColStart(a3), d0
	bne.w mismatch
literalAdjacent
	movea.l Local.LiteralCursor(a6), a0
	movea.l abi.MatchFrame.ArenaPtr(a4), a1
	adda.l abi.TokenView.LiteralOffset(a3), a1
	move.l abi.TokenView.LiteralBytes(a3), d2
	move.l Local.LiteralEnd(a6), d0
	sub.l Local.LiteralCursor(a6), d0
	cmp.l d0, d2
	bhi.w mismatch
literalByte
	tst.l d2
	beq.w literalCopied
	bsr.w tick
	bne.w finish
	move.b (a0)+, d0
	cmp.b (a1)+, d0
	bne.w mismatch
	subq.l #1, d2
	bra.w literalByte
literalCopied
	move.l a0, Local.LiteralCursor(a6)
	move.l abi.TokenView.ColEnd(a3), Local.PriorEnd(a6)
	move.l #1, Local.HasPrior(a6)
	adda.w #abi.TOKEN_BYTES, a3
	addq.l #1, d7
	bra.w literalToken
literalComplete
	move.l Local.PriorEnd(a6), d4
	moveq #1, d3
	bra.w advance
open
	addq.l #1, d5
	bra.w advance
close
	subq.l #1, d5
advance
	adda.w #abi.ATOM_ROW_BYTES, a5
	subq.l #1, d6
	bra.w nextRow
complete
	cmp.l abi.MatchFrame.TokenCount(a4), d7
	bne.w mismatch
	bsr.w tick
	bne.w finish
	movea.l abi.MatchFrame.WorkspacePtr(a4), a0
	movea.l abi.MatchFrame.OutputPtr(a4), a1
	move.l Local.CaptureBytes(a6), d0
publish
	tst.l d0
	beq.w matched
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bra.w publish
matched
	moveq #1, d1
	move.l Local.CaptureCount(a6), d2
	moveq #abi.OK, d0
	bra.w done
mismatch
	moveq #abi.OK, d0
	bra.w finish
overflow
	moveq #abi.OUTPUT_OVERFLOW, d0
finish
	moveq #0, d1
	moveq #0, d2
done
	.TELEMETRY_VM_LEAVE
	lea LOCAL_BYTES(sp), sp
	tst.l d0
	movem.l (sp)+, d3-d7/a0-a6
	rts
	.bend  ; runBudget
	.priv
; Validate the package-selected current adjacency/type table; no VM version state.
; D0=status/CCR. Other registers preserved, local budget consumed.
policy	.block
	movem.l a0-a2, -(sp)
	movea.l abi.MatchFrame.ProgramPtr(a4), a0
	move.l abi.MatchFrame.ProgramBytes(a4), d0
	movea.l Local.BudgetPtr(a6), a2
	jsr policies.match
	tst.l d0
	movem.l (sp)+, a0-a2
	rts
	.bend  ; policy

; A5=capture row,A3=token. D0=acceptance boolean; all other registers preserved.
accepts	.block
	movem.l d1-d3/a0, -(sp)
	moveq #0, d0
	move.b abi.AtomRow.CaptureType(a5), d0
	subq.l #1, d0
	lsl.l #3, d0
	move.l d0, d1
	add.l d0, d0
	add.l d1, d0  ; abi.TYPE_POLICY_BYTES = 24
	movea.l abi.MatchFrame.ProgramPtr(a4), a0
	adda.w #abi.MATCH_HEADER_BYTES, a0
	adda.l d0, a0
	moveq #0, d0
	move.b (a3), d0  ; TokenView.Kind (zero offset)
	cmpi.l #abi.K_REGISTER, d0
	bls.w identifier
	cmpi.l #abi.K_NUMBER, d0
	beq.w number
	cmpi.l #abi.K_STRING, d0
	beq.w string
	bra.w reject
identifier
	cmpi.b #1, abi.TypePolicy.Identifier(a0)
	beq.w accept
	cmpi.b #2, abi.TypePolicy.Identifier(a0)
	bne.w reject
	cmpi.l #1, abi.TokenView.LiteralBytes(a3)
	beq.w accept
	bra.w reject
number
	cmpi.b #1, abi.TokenView.NumericStatus(a3)
	bne.w reject
	move.l abi.TokenView.NumericHigh(a3), d2
	move.l abi.TokenView.NumericLow(a3), d3
	cmp.l abi.TypePolicy.NumericMinHigh(a0), d2
	blt.w reject
	bgt.w minimumReady
	cmp.l abi.TypePolicy.NumericMinLow(a0), d3
	blo.w reject
minimumReady
	cmp.l abi.TypePolicy.NumericMaxHigh(a0), d2
	bgt.w reject
	blt.w accept
	cmp.l abi.TypePolicy.NumericMaxLow(a0), d3
	bhi.w reject
	bra.w accept
string
	moveq #0, d0
	move.b abi.TypePolicy.StringMax(a0), d0
	beq.w reject
	moveq #0, d1
	move.b abi.TypePolicy.StringMin(a0), d1
	move.l abi.TokenView.DecodedBytes(a3), d2
	cmp.l d1, d2
	blo.w reject
	cmpi.l #255, d0
	beq.w accept
	cmp.l d0, d2
	bhi.w reject
accept
	moveq #1, d0
	bra.w done
reject
	moveq #0, d0
done
	tst.l d0
	movem.l (sp)+, d1-d3/a0
	rts
	.bend  ; accepts

; Local budget tick preserving the current literal cursor in A0.
tick	.block
	move.l a0, -(sp)
	movea.l Local.BudgetPtr(a6), a0
	jsr inputs.tick
	movea.l (sp)+, a0
	rts
	.bend  ; tick
	.endsection
	.endmodule

; Compile package-selected signature grammar into an offset-based immutable plan.
; @opforge-owner: opforge.vm.prvm.statement_compile
	.module opforge.vm.prvm.statement_compile
	.cpu 68020
	.use opforge.vm.prvm.statement_abi as abi
	.include "telemetry_macros.i"
	.use opforge.vm.prvm.statement_inputs as inputs
	.use opforge.vm.prvm.statement_request as request
	.use opforge.vm.prvm.statement_facets as facets
	.use opforge.vm.prvm.statement_policy as policy
	.priv
Local	.struct
Budget	.long ?
TypeRows	.long ?
RenderRows	.long ?
RowCount	.long ?
RowCursor	.long ?
PayloadBase	.long ?
PayloadCursor	.long ?
Top	.long ?
NextTop	.long ?
LiteralScore	.long ?
AtomScore	.long ?
	.endstruct
LOCAL_BYTES = Local.AtomScore+4
POLICY_VALIDATION_STEPS = (abi.POLICY_TYPE_COUNT+abi.RENDER_COUNT)*256+1
	.pub
	.section code, kind=code
; A0=program,D0=bounded bytes. D0=status/CCR; other registers preserved.
; Pure current-policy validation. Counts/name lengths bound its local work budget.
validateProgram	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l #POLICY_VALIDATION_STEPS, -(sp)
	movea.l sp, a2
	jsr policy.compile
	addq.l #4, sp
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; validateProgram

; A0=CompileFrame,D0=available bytes. D0=status/CCR,D1=plan extent,D2=rows.
; Other registers preserved. Workspace stages all rows/payload before publication;
; output is untouched on every error. Inputs are prepared facets, never source.
run	.block
	.TELEMETRY_VM_ENTER runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	movem.l d3-d7/a0-a6, -(sp)
	lea -LOCAL_BYTES(sp), sp
	movea.l sp, a6
	movea.l a0, a4
	jsr request.compile
	bne.w finish
	move.l abi.CompileFrame.StepBudget(a4), (a6)  ; Local.Budget (zero offset)
	movea.l abi.CompileFrame.ProgramPtr(a4), a0
	move.l abi.CompileFrame.ProgramBytes(a4), d0
	movea.l a6, a2
	jsr policy.compile
	bne.w finish
	move.l a0, Local.TypeRows(a6)
	move.l a1, Local.RenderRows(a6)
	movea.l abi.CompileFrame.TokenPtr(a4), a0
	move.l abi.CompileFrame.TokenCount(a4), d0
	movea.l abi.CompileFrame.ArenaPtr(a4), a1
	move.l abi.CompileFrame.ArenaBytes(a4), d1
	jsr facets.validate
	bne.w finish
	movea.l abi.CompileFrame.TokenPtr(a4), a3
	movea.l abi.CompileFrame.ProgramPtr(a4), a5
	moveq #0, d7
	bsr.w canonical
	bne.w finish
	move.l abi.CompileFrame.TokenCount(a4), d0
	cmpi.l #$05555554, d0
	bhi.w overflow
	bsr.w rowBytes
	addi.l #abi.PLAN_HEADER_BYTES, d0
	cmp.l abi.CompileFrame.WorkspaceCapacity(a4), d0
	bhi.w overflow
	move.l d0, Local.PayloadBase(a6)
	movea.l abi.CompileFrame.WorkspacePtr(a4), a0
	adda.l d0, a0
	move.l a0, Local.PayloadCursor(a6)
	movea.l abi.CompileFrame.WorkspacePtr(a4), a0
	adda.w #abi.PLAN_HEADER_BYTES, a0
	move.l a0, Local.RowCursor(a6)
	clr.l Local.RowCount(a6)
	move.l #abi.NONE, Local.Top(a6)
	clr.l Local.LiteralScore(a6)
	clr.l Local.AtomScore(a6)
next
	cmp.l abi.CompileFrame.TokenCount(a4), d7
	beq.w complete
	bsr.w tick
	bne.w finish
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	moveq #0, d0
	move.b (a3), d0  ; TokenView.Kind (zero offset)
	cmpi.l #abi.NONE, Local.Top(a6)
	beq.w opening
	cmp.b abi.CompileProgram.CloseFirst(a5), d0
	beq.w closing
opening
	cmp.b abi.CompileProgram.OpenFirst(a5), d0
	bne.w atom
	move.l d7, d1
	addq.l #1, d1
	cmp.l abi.CompileFrame.TokenCount(a4), d1
	bhs.w atom
	move.b abi.TOKEN_BYTES+abi.TokenView.Kind(a3), d1
	cmp.b abi.CompileProgram.OpenSecond(a5), d1
	bne.w atom
	moveq #0, d0
	suba.l a0, a0
	move.l abi.TokenView.ColStart(a3), d1
	move.l abi.TokenView.ColEnd(a3), d2
	moveq #abi.ATOM_OPEN, d3
	moveq #0, d4
	move.l Local.Top(a6), d6
	bsr.w emit
	bne.w finish
	move.l Local.RowCount(a6), d0
	subq.l #1, d0
	move.l d0, Local.Top(a6)
	bra.w advancePair
closing
	move.l d7, d1
	addq.l #1, d1
	cmp.l abi.CompileFrame.TokenCount(a4), d1
	bhs.w invalidToken
	move.b abi.TOKEN_BYTES+abi.TokenView.Kind(a3), d1
	cmp.b abi.CompileProgram.CloseSecond(a5), d1
	bne.w invalidToken
	move.l Local.Top(a6), d0
	bsr.w rowAddress
	move.l abi.AtomRow.PairedRow(a1), Local.NextTop(a6)
	move.l abi.AtomRow.ColStart(a1), d1
	move.l abi.TOKEN_BYTES+abi.TokenView.ColEnd(a3), d2
	moveq #0, d0
	suba.l a0, a0
	moveq #abi.ATOM_CLOSE, d3
	moveq #0, d4
	move.l Local.Top(a6), d6
	bsr.w emit
	bne.w finish
	move.l Local.RowCount(a6), d0
	subq.l #1, d0
	move.l d0, abi.AtomRow.PairedRow(a1)
	move.l d2, abi.AtomRow.ColEnd(a1)
	move.l Local.NextTop(a6), Local.Top(a6)
advancePair
	adda.w #abi.TOKEN_BYTES*2, a3
	addq.l #2, d7
	bra.w next
atom
	cmp.b abi.CompileProgram.Quoted(a5), d0
	bne.w dot
	cmpi.l #abi.K_STRING, d0
	bne.w invalidToken
	bra.w literal
dot
	cmp.b abi.CompileProgram.Dot(a5), d0
	beq.w literal
	cmpi.l #abi.K_REGISTER, d0
	bhi.w invalidToken
	bsr.w captureType
	tst.l d0
	beq.w invalidToken
	move.l d0, d4
	move.l d7, d1
	addq.l #2, d1
	cmp.l abi.CompileFrame.TokenCount(a4), d1
	bhs.w invalidToken
	move.b abi.TOKEN_BYTES+abi.TokenView.Kind(a3), d1
	cmp.b abi.CompileProgram.Delimiter(a5), d1
	bne.w invalidToken
	cmpi.b #abi.K_REGISTER, abi.TOKEN_BYTES*2+abi.TokenView.Kind(a3)
	bhi.w invalidToken
	movea.l abi.CompileFrame.ArenaPtr(a4), a0
	adda.l abi.TOKEN_BYTES*2+abi.TokenView.LiteralOffset(a3), a0
	move.l abi.TOKEN_BYTES*2+abi.TokenView.LiteralBytes(a3), d0
	move.l abi.TokenView.ColStart(a3), d1
	move.l abi.TOKEN_BYTES*2+abi.TokenView.ColEnd(a3), d2
	moveq #abi.ATOM_CAPTURE, d3
	move.l #abi.NONE, d6
	bsr.w emit
	bne.w finish
	adda.w #abi.TOKEN_BYTES*3, a3
	addq.l #3, d7
	bra.w next
literal
	movea.l abi.CompileFrame.ArenaPtr(a4), a0
	adda.l abi.TokenView.LiteralOffset(a3), a0
	move.l abi.TokenView.LiteralBytes(a3), d0
	move.l abi.TokenView.ColStart(a3), d1
	move.l abi.TokenView.ColEnd(a3), d2
	moveq #abi.ATOM_LITERAL, d3
	moveq #0, d4
	move.l #abi.NONE, d6
	bsr.w emit
	bne.w finish
	adda.w #abi.TOKEN_BYTES, a3
	addq.l #1, d7
	bra.w next
complete
	cmpi.l #abi.NONE, Local.Top(a6)
	bne.w invalidToken
	bsr.w compact
	bne.w finish
	move.l Local.RowCount(a6), d2
	moveq #abi.OK, d0
	bra.w done
invalidToken
	moveq #abi.INVALID_TOKEN, d0
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
	.bend  ; run
	.priv
; Validate every known punctuation facet against the package's canonical text.
; D0=status/CCR; main token cursor/index and other registers preserved.
canonical	.block
	movem.l d1-d3/d7/a0-a3, -(sp)
	move.l abi.CompileFrame.TokenCount(a4), d7
token
	tst.l d7
	beq.w valid
	bsr.w tick
	bne.w done
	movea.l Local.RenderRows(a6), a0
	moveq #0, d0
	move.b (a3), d0  ; TokenView.Kind (zero offset)
	jsr policy.render
	tst.l d0
	beq.w advance
	cmp.l abi.TokenView.LiteralBytes(a3), d0
	bne.w invalid
	move.l d0, d2
	movea.l abi.CompileFrame.ArenaPtr(a4), a1
	adda.l abi.TokenView.LiteralOffset(a3), a1
byte
	move.b (a0)+, d0
	cmp.b (a1)+, d0
	bne.w invalid
	subq.l #1, d2
	bne.w byte
advance
	adda.w #abi.TOKEN_BYTES, a3
	subq.l #1, d7
	bra.w token
valid
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_TOKEN, d0
done
	tst.l d0
	movem.l (sp)+, d1-d3/d7/a0-a3
	rts
	.bend  ; canonical

; A3=type-name facet. D0=selected package type ID, zero if unknown.
; Other registers preserved. ASCII folding matches eq_ignore_ascii_case.
captureType	.block
	movem.l d1-d3/d5-d6/a0-a2, -(sp)
	movea.l Local.TypeRows(a6), a2
	moveq #abi.POLICY_TYPE_COUNT, d6
row
	moveq #0, d5
	move.b (a2)+, d5  ; TypeName.Id (zero offset)
	moveq #0, d3
	move.b (a2)+, d3
	cmp.l abi.TokenView.LiteralBytes(a3), d3
	bne.w advance
	movea.l a2, a0
	movea.l abi.CompileFrame.ArenaPtr(a4), a1
	adda.l abi.TokenView.LiteralOffset(a3), a1
	move.l d3, d2
byte
	moveq #0, d0
	move.b (a0)+, d0
	moveq #0, d1
	move.b (a1)+, d1
	bsr.w fold
	cmp.b d1, d0
	bne.w advance
	subq.l #1, d2
	bne.w byte
	move.l d5, d0
	bra.w done
advance
	adda.l d3, a2
	subq.l #1, d6
	bne.w row
	moveq #0, d0
done
	tst.l d0
	movem.l (sp)+, d1-d3/d5-d6/a0-a2
	rts
	.bend  ; captureType

; A0/D0=payload,D1/D2=span,D3=kind,D4=type,D6=pair. A2=emitted row.
; D0=status/CCR; other registers preserved. Workspace publication only.
emit	.block
	movem.l d1-d6/a0-a1, -(sp)
	move.l d0, d5
	move.l Local.RowCount(a6), d0
	cmp.l abi.CompileFrame.TokenCount(a4), d0
	bhs.w overflow
	move.l abi.CompileFrame.WorkspaceCapacity(a4), d0
	movea.l Local.PayloadCursor(a6), a1
	sub.l a1, d0
	add.l abi.CompileFrame.WorkspacePtr(a4), d0
	cmp.l d0, d5
	bhi.w overflow
	movea.l Local.RowCursor(a6), a2
	move.l a2, -(sp)
	moveq #abi.ATOM_ROW_BYTES/4-1, d0
clear
	clr.l (a2)+
	dbra d0, clear
	movea.l (sp)+, a2
	move.b d3, (a2)  ; AtomRow.Kind (zero offset)
	move.b d4, abi.AtomRow.CaptureType(a2)
	move.l a1, d0
	sub.l abi.CompileFrame.WorkspacePtr(a4), d0
	move.l d0, abi.AtomRow.PayloadOffset(a2)
	move.l d5, abi.AtomRow.PayloadBytes(a2)
	move.l d6, abi.AtomRow.PairedRow(a2)
	move.l d1, abi.AtomRow.ColStart(a2)
	move.l d2, abi.AtomRow.ColEnd(a2)
copy
	tst.l d5
	beq.w copied
	bsr.w tick
	bne.w done
	move.b (a0)+, (a1)+
	subq.l #1, d5
	bra.w copy
copied
	move.l a1, Local.PayloadCursor(a6)
	addi.l #abi.ATOM_ROW_BYTES, Local.RowCursor(a6)
	addq.l #1, Local.RowCount(a6)
	cmpi.l #abi.NONE, Local.Top(a6)
	bne.w valid
	cmpi.l #abi.ATOM_CLOSE, d3
	beq.w valid
	addq.l #1, Local.AtomScore(a6)
	cmpi.l #abi.ATOM_LITERAL, d3
	bne.w valid
	addq.l #1, Local.LiteralScore(a6)
valid
	moveq #abi.OK, d0
	bra.w done
overflow
	moveq #abi.OUTPUT_OVERFLOW, d0
done
	tst.l d0
	movem.l (sp)+, d1-d6/a0-a1
	rts
	.bend  ; emit

; Compact reserved rows/payload, patch offsets, then publish the complete plan.
; D0=status/CCR,D1=extent; other main registers may be clobbered after parsing.
compact	.block
	move.l Local.RowCount(a6), d0
	bsr.w rowBytes
	addi.l #abi.PLAN_HEADER_BYTES, d0
	move.l Local.PayloadBase(a6), d3
	sub.l d0, d3  ; gap removed between rows and payload
	move.l Local.PayloadCursor(a6), d1
	sub.l abi.CompileFrame.WorkspacePtr(a4), d1
	sub.l d3, d1
	cmp.l abi.CompileFrame.OutputCapacity(a4), d1
	bhi.w overflow
	movea.l abi.CompileFrame.WorkspacePtr(a4), a0
	move.l d1, (a0)  ; PlanHeader.Extent (zero offset)
	move.l Local.RowCount(a6), abi.PlanHeader.RowCount(a0)
	move.l Local.LiteralScore(a6), abi.PlanHeader.LiteralScore(a0)
	move.l Local.AtomScore(a6), abi.PlanHeader.AtomScore(a0)
	lea abi.PLAN_HEADER_BYTES(a0), a2
	move.l Local.RowCount(a6), d2
row
	tst.l d2
	beq.w payload
	bsr.w tick
	bne.w done
	sub.l d3, abi.AtomRow.PayloadOffset(a2)
	adda.w #abi.ATOM_ROW_BYTES, a2
	subq.l #1, d2
	bra.w row
payload
	movea.l abi.CompileFrame.WorkspacePtr(a4), a0
	movea.l a0, a1
	adda.l Local.PayloadBase(a6), a0
	move.l Local.RowCount(a6), d0
	bsr.w rowBytes
	adda.w #abi.PLAN_HEADER_BYTES, a1
	adda.l d0, a1
byte
	cmpa.l Local.PayloadCursor(a6), a0
	beq.w ready
	bsr.w tick
	bne.w done
	move.b (a0)+, (a1)+
	bra.w byte
ready
	bsr.w tick
	bne.w done
	movea.l abi.CompileFrame.WorkspacePtr(a4), a0
	move.l (a0), d1  ; PlanHeader.Extent (zero offset)
	movea.l abi.CompileFrame.OutputPtr(a4), a1
	move.l d1, d2
publish
	move.b (a0)+, (a1)+
	subq.l #1, d2
	bne.w publish
	moveq #abi.OK, d0
	bra.w done
overflow
	moveq #abi.OUTPUT_OVERFLOW, d0
done
	rts
	.bend  ; compact

; D0=row count/index. D0=bytes; D1 preserved; CCR unspecified.
rowBytes	.block
	move.l d1, -(sp)
	lsl.l #3, d0
	move.l d0, d1
	add.l d0, d0
	add.l d1, d0  ; abi.ATOM_ROW_BYTES = 3*8
	move.l (sp)+, d1
	rts
	.bend  ; rowBytes
; D0=checked row index. A1=workspace row; other registers preserved except D0.
rowAddress	.block
	bsr.w rowBytes
	movea.l abi.CompileFrame.WorkspacePtr(a4), a1
	adda.w #abi.PLAN_HEADER_BYTES, a1
	adda.l d0, a1
	rts
	.bend  ; rowAddress
; ASCII fold compared bytes D0/D1; other registers preserved, CCR unspecified.
fold	.block
	cmpi.b #'A', d0
	blo.s second
	cmpi.b #'Z', d0
	bhi.s second
	addi.b #32, d0
second
	cmpi.b #'A', d1
	blo.s done
	cmpi.b #'Z', d1
	bhi.s done
	addi.b #32, d1
done
	rts
	.bend  ; fold
; Budget tick preserving a current payload cursor in A0.
tick	.block
	move.l a0, -(sp)
	movea.l a6, a0  ; Local.Budget (zero offset)
	jsr inputs.tick
	movea.l (sp)+, a0
	rts
	.bend  ; tick
	.endsection
	.endmodule

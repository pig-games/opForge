; Included by binary_frontend: source binding and borrowed session wiring only.
; Shared PRVM and the statement/template owners hold grammar and expansion.
	.priv
; A5=session,A6=scratch. D0/CCR=status,D1=regular/consumed; others preserved.
; A statement inside an unexpanded macro/segment stays in that template body.
statementLine	.block
	movem.l d2/a0-a4, -(sp)
	suba.w #statement_session.FRAME_BYTES, sp
	movea.l a6, a0
	adda.l #STATEMENT_STATE, a0
	tst.w statements.State.Open(a0)
	bne.w dispatch
	tst.w statements.State.Skipping(a0)
	bne.w dispatch
	movea.l a6, a1
	adda.l #TEMPLATE_STATE, a1
	move.l a0, -(sp)
	movea.l a1, a0
	jsr templates.hasOpen
	movea.l (sp)+, a0
	bne.w ordinary
	tst.w templates.State.Skipping(a1)
	bne.w ordinary
dispatch
	movea.l sp, a0
	bsr.w statementRequest
	jsr statement_session.line
	bra.w done
ordinary
	moveq #0, d1
	moveq #0, d0
done
	adda.w #statement_session.FRAME_BYTES, sp
	tst.l d0
	movem.l (sp)+, d2/a0-a4
	rts
	.bend  ; statementLine

; A0=request,A5=session,A6=scratch. Populate borrowed source-session fields.
; Preserves all registers; binding and grammar remain with their owners.
statementRequest	.block
	movem.l d2/a0-a1/a4, -(sp)
	movea.l a0, a4
	movea.l a6, a1
	adda.l #STATEMENT_STATE, a1
	move.l a1, statement_session.Frame.State(a4)
	move.l Frame.Package(a5), statement_session.Frame.Package(a4)
	movea.l a6, a0
	adda.l #STATEMENT_HEAD_NAMES, a0
	move.l a0, statement_session.Frame.HeadNames(a4)
	movea.l a6, a0
	adda.l #TEMPLATE_STATE, a0
	move.l a0, statement_session.Frame.Templates(a4)
	lea SCOPE_STATE(a6), a0
	move.l a0, statement_session.Frame.Scope(a4)
	lea MACRO_PLANS(a6), a0
	move.l a0, statement_session.Frame.MacroPlans(a4)
	move.l LINE_FRAME+writer.Frame.Tokens(a6), statement_session.Frame.Tokens(a4)
	move.l LINE_FRAME+writer.Frame.Count(a6), statement_session.Frame.Count(a4)
	move.l LINE_FRAME+writer.Frame.Lexemes(a6), statement_session.Frame.Lexemes(a4)
	move.l LINE_FRAME+writer.Frame.LexemeBytes(a6), statement_session.Frame.LexemeBytes(a4)
	movea.l Frame.Output(a5), a0
	move.l a0, statement_session.Frame.Record(a4)
	moveq #0, d2
	move.b writer.RecordHeader.Length(a0), d2
	addq.l #1, d2
	move.l d2, statement_session.Frame.RecordBytes(a4)
	move.l Frame.Origin(a5), statement_session.Frame.Origin(a4)
	moveq #0, d2
	move.w SCOPE_STATE+scopes.MODULE_STATE+modules.State.Active(a6), d2
	move.l d2, statement_session.Frame.Module(a4)
	moveq #0, d2
	move.w SCOPE_STATE+scopes.MODULE_STATE+modules.State.Visibility(a6), d2
	move.l d2, statement_session.Frame.Visibility(a4)
	movea.l a6, a0
	adda.l #CONDITION_STATE, a0
	moveq #0, d2
	move.w conditionals.State.Active(a0), d2
	move.l d2, statement_session.Frame.Active(a4)
	moveq #0, d2
	tst.w conditionals.State.Deferred(a0)
	bne.w ownershipReady
	move.w SCOPE_STATE+layout.State.LoopDepth(a6), d2
	cmp.w Frame.SourceIteration(a5), d2
	seq d2
	andi.l #1, d2
ownershipReady
	move.l d2, statement_session.Frame.DeclarationAllowed(a4)
	move.l #statementIntern, statement_session.Frame.Intern(a4)
	move.l #statementKeywordIntern, statement_session.Frame.KeywordIntern(a4)
	move.l a6, statement_session.Frame.Context(a4)
	movem.l (sp)+, d2/a0-a1/a4
	rts
	.bend  ; statementRequest

; A5=session,A6=scratch. D0/status,D1=regular/expansion queued; others preserved.
statementInvoke	.block
	movem.l d2/a0-a4, -(sp)
	suba.w #statement_session.FRAME_BYTES+statement_execution.FRAME_BYTES, sp
	movea.l sp, a0
	bsr.w statementRequest
	lea statement_session.FRAME_BYTES(sp), a1
	move.l a0, statement_execution.Frame.Source(a1)
	movea.l a6, a0
	adda.l #STATEMENT_EXECUTION, a0
	move.l a0, statement_execution.Frame.State(a1)
	lea LINE_FRAME(a6), a0
	move.l a0, statement_execution.Frame.Writer(a1)
	movea.l a1, a0
	jsr statement_execution.run
	adda.w #statement_session.FRAME_BYTES+statement_execution.FRAME_BYTES, sp
	tst.l d0
	movem.l (sp)+, d2/a0-a4
	rts
	.bend  ; statementInvoke

; A0/name,D0/bytes,A1=session scratch. D0/CCR=status,D1=numeric source ID.
; Clobbers A0/A1; all other registers preserved. Grammar vocabulary never
; enters CPU/package binding or semantic reference tracking here.
statementIntern	.block
	movem.l d2-d7/a2-a6, -(sp)
	adda.l #SCOPE_STATE, a1
	jsr scopes.bindOrigin
	tst.l d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; statementIntern

; Same source-ID ABI as statementIntern. Statement keyword dots are opaque;
; the scope owner composes the full module name without lexical block prefixes.
statementKeywordIntern	.block
	movem.l d2-d7/a2-a6, -(sp)
	adda.l #SCOPE_STATE, a1
	jsr scopes.bindStatementKeyword
	tst.l d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; statementKeywordIntern

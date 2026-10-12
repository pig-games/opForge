; Borrowed request wiring only. A3=source Frame,A4=State,A6=locals.
; Helpers preserve other registers unless documented otherwise. prepareRequest
; clobbers D0; the other request builders preserve all registers.
prepareRequest	.block
	movem.l a0-a1, -(sp)
	lea Local.Prepare(a6), a0
	move.l a4, calls.PrepareFrame.State(a0)
	move.l session.Frame.State(a3), calls.PrepareFrame.Definitions(a0)
	move.l session.Frame.Scope(a3), calls.PrepareFrame.CallerScope(a0)
	movea.l session.Frame.Package(a3), a1
	move.l package.Header.StatementMatch(a1), d0
	add.l a1, d0
	move.l d0, calls.PrepareFrame.Program(a0)
	move.l package.Header.StatementMatchBytes(a1), calls.PrepareFrame.ProgramBytes(a0)
	move.l Local.Source+session.Snapshot.Views(a6), calls.PrepareFrame.HeadViews(a0)
	move.l Local.Source+session.Snapshot.Count(a6), calls.PrepareFrame.HeadCount(a0)
	move.l Local.Source+session.Snapshot.Arena(a6), calls.PrepareFrame.HeadArena(a0)
	move.l Local.Source+session.Snapshot.ArenaBytes(a6), calls.PrepareFrame.HeadArenaBytes(a0)
	move.l #STEP_BUDGET, calls.PrepareFrame.StepBudget(a0)
	movem.l (sp)+, a0-a1
	rts
	.bend  ; prepareRequest

tailRequest	.block
	move.l a0, -(sp)
	lea Local.Source(a6), a0
	move.l a0, Local.Tail+tail.Frame.Source(a6)
	lea State.Calls+calls.State.Route(a4), a0
	move.l a0, Local.Tail+tail.Frame.Route(a6)
	move.l session.Frame.Package(a3), Local.Tail+tail.Frame.Package(a6)
	move.l session.Frame.HeadNames(a3), Local.Tail+tail.Frame.HeadNames(a6)
	lea Local.Arguments(a6), a0
	move.l a0, Local.Tail+tail.Frame.Output(a6)
	movea.l (sp)+, a0
	rts
	.bend  ; tailRequest

selectRequest	.block
	movem.l a0-a1, -(sp)
	lea Local.Select(a6), a0
	move.l a4, calls.SelectFrame.State(a0)
	move.l Local.Arguments+session.Snapshot.Views(a6), calls.SelectFrame.Views(a0)
	move.l Local.Arguments+session.Snapshot.Count(a6), calls.SelectFrame.Count(a0)
	move.l Local.Arguments+session.Snapshot.Arena(a6), calls.SelectFrame.Arena(a0)
	move.l Local.Arguments+session.Snapshot.ArenaBytes(a6), calls.SelectFrame.ArenaBytes(a0)
	movea.l State.Work+memory.Block.Pointer(a4), a1
	move.l a1, calls.SelectFrame.Output(a0)
	move.l #SELECTION_BYTES, calls.SelectFrame.OutputCapacity(a0)
	adda.w #FALLBACK_OFFSET, a1
	move.l a1, calls.SelectFrame.Fallback(a0)
	move.l #FALLBACK_BYTES, calls.SelectFrame.FallbackCapacity(a0)
	movea.l State.Work+memory.Block.Pointer(a4), a1
	adda.w #MATCH_OFFSET, a1
	move.l a1, calls.SelectFrame.Workspace(a0)
	move.l #MATCH_BYTES, calls.SelectFrame.WorkspaceCapacity(a0)
	movem.l (sp)+, a0-a1
	rts
	.bend  ; selectRequest

; A2=selection. Populate the exclusive argument-writer request.
packRequest	.block
	movem.l d0/a0-a1, -(sp)
	lea Local.Pack(a6), a0
	lea abi.SELECTION_BYTES(a2), a1
	move.l a1, arguments.Frame.Captures(a0)
	move.l abi.Selection.CaptureCount(a2), arguments.Frame.CaptureCount(a0)
	move.l #templates.PARAM_LIMIT*abi.CAPTURE_BYTES, arguments.Frame.CaptureBytes(a0)
	move.l Local.Views(a6), arguments.Frame.Views(a0)
	move.l Local.ViewCount(a6), arguments.Frame.ViewCount(a0)
	move.l Local.ViewCount(a6), d0
	mulu.w #abi.TOKEN_BYTES, d0
	move.l d0, arguments.Frame.ViewBytes(a0)
	move.l Local.Arguments+session.Snapshot.Arena(a6), arguments.Frame.Arena(a0)
	move.l Local.Arguments+session.Snapshot.ArenaBytes(a6), arguments.Frame.ArenaBytes(a0)
	move.l Frame.Writer(a5), arguments.Frame.Writer(a0)
	movea.l State.Work+memory.Block.Pointer(a4), a1
	adda.w #TOKENS_OFFSET, a1
	move.l a1, arguments.Frame.Tokens(a0)
	move.l #TOKEN_LIMIT, arguments.Frame.TokenCapacity(a0)
	movea.l State.Work+memory.Block.Pointer(a4), a1
	adda.w #LEXEMES_OFFSET, a1
	move.l a1, arguments.Frame.Lexemes(a0)
	move.l #LEXEMES_BYTES, arguments.Frame.LexemeCapacity(a0)
	movea.l State.Work+memory.Block.Pointer(a4), a1
	adda.w #ARGUMENTS_OFFSET, a1
	move.l a1, arguments.Frame.Arguments(a0)
	move.l #ARGUMENTS_BYTES, arguments.Frame.ArgumentCapacity(a0)
	movea.l State.Work+memory.Block.Pointer(a4), a1
	adda.w #PACKED_OFFSET, a1
	move.l a1, arguments.Frame.Packed(a0)
	move.l #templates.ARG_LIMIT, arguments.Frame.PackedCapacity(a0)
	movea.l State.Work+memory.Block.Pointer(a4), a1
	adda.w #FULL_OFFSET, a1
	move.l a1, arguments.Frame.Full(a0)
	move.l #FULL_BYTES-1, arguments.Frame.FullCapacity(a0)
	movem.l (sp)+, d0/a0-a1
	rts
	.bend  ; packRequest

; A2=selection,D1=full spelling bytes. D0/CCR=status; others preserved.
; Caller labels are already numeric in the current writer record. The selected
; row supplies the private carrier name, never a new mnemonic/string lookup.
callRequest	.block
	movem.l d1-d3/a0-a3, -(sp)
	move.w d1, Local.Call+templates.ExternalCall.FullBytes(a6)
	move.l abi.Selection.TemplateId(a2), d0
	cmpi.l #$ffff, d0
	bhi.w invalid
	move.w d0, Local.Call+templates.ExternalCall.Definition(a6)
	move.l abi.Selection.CaptureCount(a2), d0
	move.w d0, Local.Call+templates.ExternalCall.ArgCount(a6)
	move.l Local.Pack+arguments.Frame.Arguments(a6), Local.Call+templates.ExternalCall.Arguments(a6)
	move.l Local.Pack+arguments.Frame.Full(a6), Local.Call+templates.ExternalCall.FullSpelling(a6)
	movea.l session.Frame.Record(a3), a0
	cmpi.l #writer.RECORD_HEADER_BYTES, session.Frame.RecordBytes(a3)
	blo.w invalid
	move.w writer.RecordHeader.Line(a0), Local.Call+templates.ExternalCall.CallLine(a6)
	clr.w Local.Call+templates.ExternalCall.LabelPresent(a6)
	clr.l Local.Call+templates.ExternalCall.Label(a6)
	tst.l Frame.InheritedLabelPresent(a5)
	beq.w lexicalLabel
	cmpi.l #1, Frame.InheritedLabelPresent(a5)
	bne.w invalid
	cmpi.l #abi.NONE, State.Calls+calls.State.Route+abi.Route.LabelToken(a4)
	bne.w invalid  ; a body label must not replace an inherited caller label
	move.w #1, Local.Call+templates.ExternalCall.LabelPresent(a6)
	move.l Frame.InheritedLabel(a5), Local.Call+templates.ExternalCall.Label(a6)
	bra.w labelReady
lexicalLabel
	cmpi.l #abi.NONE, State.Calls+calls.State.Route+abi.Route.LabelToken(a4)
	beq.w labelReady
	cmpi.l #writer.RECORD_HEADER_BYTES+writer.NAME_TOKEN_BYTES, session.Frame.RecordBytes(a3)
	blo.w invalid
	move.w #1, Local.Call+templates.ExternalCall.LabelPresent(a6)
	move.l writer.RECORD_HEADER_BYTES(a0), Local.Call+templates.ExternalCall.Label(a6)
labelReady
	move.l abi.Selection.PlanId(a2), d1
	movea.l a4, a0
	jsr calls.chosen
	bne.w done
	move.w definitions.Row.Name(a1), Local.Call+templates.ExternalCall.CallName(a6)
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
done
	tst.l d0
	movem.l (sp)+, d1-d3/a0-a3
	rts
	.bend  ; callRequest

; Separate inventory ABI v1. Fixed summary phases 3000..3014; every used
; definition has eight triples at 3100+8*index, every edge three at20000+3*index.
; References are one-based row ordinals; direct callers are zero. No addresses
; or paths enter this report. The parent report resolves opaque origins itself.
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
TEMPLATE_PROGRESS .macro phase, first, second, third
	move.l .phase, d0
	move.l .first, d1
	move.l .second, d2
	move.l .third, d3
	movea.l a4, a0
	jsr memory_profile.progress
.endmacro
	.priv
templateReport .block
	.TEMPLATE_SAVE
	movea.l a0, a4
	lea TemplateState, a6
	bsr.w templateVerify
	; Report is terminal. An active lifetime has no complete pool snapshot.
	tst.l TemplateFields.Active(a6)
	beq.w summary
	ori.l #TEMPLATE_UNFINISHED, TemplateFields.Errors(a6)
	move.l TemplateFields.Active(a6), TemplateFields.Unfinished(a6)
summary
	.TEMPLATE_PROGRESS #3000, #1, TemplateFields.Errors(a6), TemplateFields.Epoch(a6)
	.TEMPLATE_PROGRESS #3001, #TEMPLATE_STATE_BYTES, #TEMPLATE_DEFINITION_LIMIT, #TEMPLATE_EDGE_LIMIT
	.TEMPLATE_PROGRESS #3002, TemplateFields.Count(a6), TemplateFields.EdgeCount(a6), TemplateFields.Finished(a6)
	.TEMPLATE_PROGRESS #3003, TemplateFields.BindingCalls(a6), TemplateFields.BindingBytes(a6), TemplateFields.Calls(a6)
	.TEMPLATE_PROGRESS #3004, TemplateFields.NestedCalls(a6), TemplateFields.DefaultCalls(a6), TemplateFields.DefaultSlots(a6)
	.TEMPLATE_PROGRESS #3005, TemplateFields.Pools+TemplatePoolValues.InternalBytes(a6), TemplateFields.Pools+TemplatePoolValues.ExternalBytes(a6), TemplateFields.Pools+TemplatePoolValues.DefaultBytes(a6)
	.TEMPLATE_PROGRESS #3006, TemplateFields.Pools+TemplatePoolValues.TextBytes(a6), TemplateFields.Pools+TemplatePoolValues.BodyBytes(a6), TemplateFields.Pools+TemplatePoolValues.PlanBytes(a6)
	.TEMPLATE_PROGRESS #3007, TemplateFields.Payload+TemplatePoolValues.InternalBytes(a6), TemplateFields.Payload+TemplatePoolValues.ExternalBytes(a6), TemplateFields.Payload+TemplatePoolValues.DefaultBytes(a6)
	.TEMPLATE_PROGRESS #3008, TemplateFields.Payload+TemplatePoolValues.TextBytes(a6), TemplateFields.Payload+TemplatePoolValues.BodyBytes(a6), TemplateFields.Closed(a6)
	.TEMPLATE_PROGRESS #3009, TemplateFields.Residual+TemplatePoolValues.InternalBytes(a6), TemplateFields.Residual+TemplatePoolValues.ExternalBytes(a6), TemplateFields.Residual+TemplatePoolValues.DefaultBytes(a6)
	.TEMPLATE_PROGRESS #3010, TemplateFields.Residual+TemplatePoolValues.TextBytes(a6), TemplateFields.Residual+TemplatePoolValues.BodyBytes(a6), TemplateFields.Residual+TemplatePoolValues.PlanBytes(a6)
	.TEMPLATE_PROGRESS #3011, TemplateFields.OpenRows(a6), TemplateFields.Unfinished(a6), TemplateFields.UnattributedCalls(a6)
	.TEMPLATE_PROGRESS #3012, TemplateFields.UnattributedBytes(a6), TemplateFields.RowCalls(a6), TemplateFields.EdgeCalls(a6)
	.TEMPLATE_PROGRESS #3013, TemplateFields.RowBinds(a6), TemplateFields.RowBoundBytes(a6), TemplateFields.EdgeDefaults(a6)
	.TEMPLATE_PROGRESS #3014, TemplateFields.EdgeSlots(a6), TemplateFields.Active(a6), #0
	movea.l a6, a3
	adda.l #TemplateFields.Rows, a3
	move.l TemplateFields.Count(a6), d7
	move.l #TEMPLATE_DEFINITION_PHASE, d4
definition
	tst.l d7
	beq.w edges
	movea.l a3, a5
	moveq #7, d6
triple
	move.l d4, d0
	move.l (a5)+, d1
	move.l (a5)+, d2
	move.l (a5)+, d3
	movea.l a4, a0
	jsr memory_profile.progress
	addq.l #1, d4
	dbra d6, triple
	adda.w #TEMPLATE_DEFINITION_BYTES, a3
	subq.l #1, d7
	bra.w definition
edges
	movea.l a6, a3
	adda.l #TemplateFields.Edges, a3
	move.l TemplateFields.EdgeCount(a6), d7
	move.l #TEMPLATE_EDGE_PHASE, d4
edge
	tst.l d7
	beq.w done
	.TEMPLATE_PROGRESS d4, TemplateEdge.Caller(a3), TemplateEdge.Callee(a3), TemplateEdge.Origin(a3)
	addq.l #1, d4
	.TEMPLATE_PROGRESS d4, TemplateEdge.Line(a3), TemplateEdge.Calls(a3), TemplateEdge.DefaultCalls(a3)
	addq.l #1, d4
	.TEMPLATE_PROGRESS d4, TemplateEdge.DefaultSlots(a3), #0, #0
	addq.l #1, d4
	adda.w #TEMPLATE_EDGE_BYTES, a3
	subq.l #1, d7
	bra.w edge
done
	.TEMPLATE_RESTORE
	rts
	.bend  ; templateReport
.endif
.endif
.endif

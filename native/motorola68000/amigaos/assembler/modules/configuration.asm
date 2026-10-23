; Configuration-only discovery over owned unbound capture records.
; @opforge-owner: opforge.assembler.modules.configuration

	.module opforge.assembler.modules.configuration
	.cpu 68020
	.use opforge.assembler.source.capture as capture
	.use opforge.assembler.source.records as source
	.use opforge.assembler.symbols.scope_layout as layout
	.use opforge.assembler.symbols.scopes as scopes
	.use opforge.assembler.language.conditionals as conditionals
	.use opforge.assembler.modules.graph as graph
	.use opforge.assembler.symbols.imports as imports
	.use opforge.assembler.modules.bindings as modules
	.use opforge.platform.memory as memory
	.use opforge.vm.prvm.abi as abi
	.use opforge.vm.prvm.statement_abi as statement_abi
	.pub
; Private classification roles are negative longs; shared directive keys are
; positive. Exact comparisons and moveq retain them, while unsigned keyword
; interval tests exclude them at the upper bound in both discovery sweeps.
ASSIGNMENT = -1
LOOP_OPEN = -2
LOOP_CLOSE = -3
SOURCE_INCLUDE = -4
TEMPLATE_OPEN = -5
TEMPLATE_CLOSE = -6
SCHEMA_ACTIVE = 1
SCHEMA_UNAVAILABLE = 2
Frame	.struct
Session	.long ?
Arena	.long ?
Scope	.long ?
Graph	.long ?
BindCapture	.long ?  ; frontend.bindCapture ABI; the selected writer copy only
FileRole	.long ?  ; shared read-only file action head selection
LoopRole	.long ?  ; read-only package loop-head selection callback
DeclarationRole	.long ?  ; read-only package declaration-head selection callback
UpdateRole	.long ?  ; shared numeric compound-update selection callback
StatementRole	.long ?  ; shared A8 role over an owned capture.Record
Output	.long ?  ; session's caller-owned writer output
Capacity	.long ?  ; writer output capacity, including shared label normalization
Cursor	.long ?  ; local capture offset, independent of the graph's logical offset
End	.long ?
IndexModule	.long ?  ; source binding index+1, no module semantic state
Binding	.long ?  ; requested graph binding for configure
Depth	.word ?
Priority	.word ?  ; physical entry only: static incoming-use root priority
Schema	.word ?  ; zero idle; shared capture or unavailable body
TemplateDepth	.word ?  ; shared template ownership, independent of guard activity
	.endstruct
CONDITIONS = Frame.TemplateDepth+2
FRAME_BYTES = CONDITIONS+conditionals.SCRATCH_BYTES
	.section code, kind=code
; A0=Frame with configured pointers. Clear physical index/depth state without
; resetting the persistent scope or graph. D0/CCR=status; other registers kept.
begin	.block
	clr.l Frame.IndexModule(a0)
	clr.w Frame.Depth(a0)
	clr.w Frame.Schema(a0)
	clr.w Frame.TemplateDepth(a0)
	move.l a0, -(sp)
	lea CONDITIONS(a0), a0
	jsr conditionals.beginStatic
	movea.l (sp)+, a0
	tst.l d0
	rts
	.bend  ; begin

; A0=Frame,D1=globally bound file-derived module index+1. Index a synthetic
; opening boundary before capture lines. The scope is not opened semantically.
; D0/CCR=status; other registers preserved.
indexDerived	.block
	movem.l d1-d2/a0-a1, -(sp)
	movea.l a0, a1
	tst.l Frame.IndexModule(a1)
	bne.w bad
	move.l d1, d2
	beq.w bad
	movea.l Frame.Graph(a1), a0
	moveq #0, d0
	moveq #0, d1
	jsr graph.line
	bne.w bad
	move.l d2, Frame.IndexModule(a1)
	move.l d2, d1
	jsr graph.requireConfiguration
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; indexDerived

; A0=Frame. Index [Cursor,End) capture boundaries without binding body records.
; graph.Cursor advances in logical capture bytes; local Cursor addresses Arena.
; BindCapture serves module names and the entry-only static priority subset.
; Reference origins use the indexed module, with Active restored on return.
; D0/CCR=status; other registers preserved.
index	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l Frame.Scope(a6), a0
	move.w layout.MODULE_STATE+modules.State.Active(a0), -(sp)
next
	bsr.w resolveNext
	bne.w bad
	tst.l d1
	beq.w good
	movea.l a1, a5
	move.l capture.Record.Bytes(a5), d7
	movea.l a5, a0
	bsr.w classify
	bne.w bad
	move.l d1, d3
	movea.l a6, a0
	bsr.w trackTemplateDepth
	move.l d0, d2
	movea.l Frame.Scope(a6), a0
	move.w Frame.IndexModule+2(a6), layout.MODULE_STATE+modules.State.Active(a0)
	moveq #0, d4
	move.l Frame.IndexModule(a6), d5
	move.l d5, d6
	; Template headers, bodies and terminators keep their enclosing module's
	; capture range. Their guards, values and dot operands are not configuration.
	tst.l d2
	beq.w outsideTemplate
	tst.w CONDITIONS+conditionals.State.Active(a6)
	bne.w publish
	moveq #1, d4
	bra.w publish
outsideTemplate
	tst.w Frame.Priority(a6)
	bne.w controls
	tst.l d5
	bne.w boundary
controls
	tst.w Frame.Schema(a6)
	bne.w indexSchema
	tst.w Frame.Depth(a6)
	beq.w moduleControls
	move.l d3, d1
	movea.l a6, a0
	bsr.w trackDepth
	bmi.w bad
	bra.w publish
moduleControls
	cmpi.l #scopes.KEY_IF, d3
	blo.w outsideActive
	cmpi.l #scopes.KEY_ENDMATCH, d3
	bhi.w outsideActive
	bsr.w bindIndexRecord
	bne.w bad
	movea.l Frame.Output(a6), a0
	movea.l Frame.Scope(a6), a1
	lea CONDITIONS(a6), a2
	jsr conditionals.line
	bne.w bad
	moveq #1, d4
	bra.w publish
outsideActive
	tst.w CONDITIONS+conditionals.State.Uncertain(a6)
	beq.w indexCertaintyReady
	cmpi.l #ASSIGNMENT, d3
	bne.w indexCertaintyReady
	bsr.w bindIndexRecord
	bne.w bad
	bsr.w invalidateConditionalWrite
	bne.w bad
	bra.w publish
indexCertaintyReady
	tst.w CONDITIONS+conditionals.State.Active(a6)
	bne.w boundary
	moveq #1, d4
	bra.w publish
boundary
	tst.w Frame.Priority(a6)
	beq.w statement
	tst.l d5
	beq.w statement
	cmpi.l #scopes.KEY_STRUCT, d3
	beq.w indexSchemaOpen
	move.l d3, d1
	movea.l a6, a0
	bsr.w trackDepth
	bmi.w bad
	bne.w publish
	cmpi.l #ASSIGNMENT, d3
	bne.w importTarget
	tst.w Frame.Depth(a6)
	bne.w publish
	bsr.w bindIndexRecord
	bne.w bad
	movea.l Frame.Output(a6), a0
	movea.l Frame.Scope(a6), a1
	move.l Frame.Capacity(a6), d0
	jsr scopes.configurationStatement
	bne.w bad
	bsr.w installConfigurationBindings
	bne.w bad
	jsr imports.captureValue
	bra.w publish
importTarget
	cmpi.l #scopes.KEY_USE, d3
	bne.w statement
	bsr.w bindIndexRecord
	bne.w bad
	movea.l Frame.Output(a6), a0
	movea.l Frame.Scope(a6), a1
	move.l Frame.Capacity(a6), d0
	jsr scopes.configurationStatement
	bne.w bad
	adda.l d1, a0
	addq.l #5, a0
	lea scopes.bind, a2
	bsr.w installConfigurationBindings
	bne.w bad
	jsr imports.configurationTarget
	bne.w bad
	movea.l Frame.Graph(a6), a0
	movea.l Frame.Scope(a6), a1
	jsr graph.entryTarget
	bne.w bad
	bra.w publish
indexSchemaOpen
	movea.l Frame.Scope(a6), a0
	move.w Frame.IndexModule+2(a6), layout.State.Current(a0)
	move.w #SCHEMA_ACTIVE, Frame.Schema(a6)
indexSchema
	move.l d3, d1
	bsr.w captureSchema
	bra.w publish
statement
	move.l d3, d1
	cmpi.l #scopes.KEY_MODULE, d1
	beq.w module
	cmpi.l #scopes.KEY_ENDMODULE, d1
	beq.w endModule
	cmpi.l #scopes.KEY_END, d1
	bne.w publish
	; .end closes an implicit unit; explicit modules end with .endmodule.
	moveq #0, d6
	bra.w clearPriority
module
	tst.l d5
	bne.w bad
	clr.w Frame.Depth(a6)
	clr.w Frame.TemplateDepth(a6)
	tst.w Frame.Priority(a6)
	beq.w moduleName
	movea.l Frame.Scope(a6), a0
	bsr.w clearStaticValues
	bne.w bad
moduleName
	bsr.w bindRecord
	bne.w bad
	movea.l Frame.Output(a6), a0
	movea.l Frame.Scope(a6), a1
	move.l Frame.Capacity(a6), d0
	jsr scopes.configurationStatement
	bne.w bad
	moveq #0, d0
	move.b (a0), d0
	addq.w #1, d0
	sub.l d1, d0
	cmpi.l #9, d0
	bne.w bad
	adda.l d1, a0
	cmpi.b #1, 5(a0)
	bhi.w bad
	moveq #0, d6
	move.w 6(a0), d6
	movea.l Frame.Scope(a6), a0
	sub.w layout.State.Base(a0), d6
	bcs.w bad
	cmp.w layout.State.Count(a0), d6
	bhs.w bad
	addq.l #1, d6
	bra.w publish
endModule
	tst.l d5
	beq.w bad
	moveq #0, d6
clearPriority
	clr.w Frame.TemplateDepth(a6)
	tst.w Frame.Priority(a6)
	beq.w publish
	movea.l Frame.Scope(a6), a0
	bsr.w clearStaticValues
	bne.w bad
publish
	movea.l Frame.Graph(a6), a0
	move.l d7, d0
	move.l d5, d1
	move.l d6, d2
	or.l d2, d1
	beq.w outside
	move.l d5, d1
	jsr graph.line
	bne.w bad
	tst.l d5
	bne.w stored
	move.l d6, d1
	jsr graph.requireConfiguration
	bne.w bad
	bra.w stored
outside
	tst.l d4
	bne.w outsideReady
	tst.b capture.Record.Count(a5)
	beq.w outsideReady
	cmpi.l #scopes.KEY_END, d3
	bne.w outsideContent
	cmpi.b #2, capture.Record.Count(a5)
	beq.w outsideReady
outsideContent
	move.w #1, graph.GraphState.Invalid(a0)
outsideReady
	jsr graph.advanceCapture
	bne.w bad
stored
	move.l d6, Frame.IndexModule(a6)
	add.l d7, Frame.Cursor(a6)
	bra.w next
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movea.l Frame.Scope(a6), a0
	move.w (sp)+, layout.MODULE_STATE+modules.State.Active(a0)
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; index

; A0=Frame. Close an implicit indexed unit at physical EOF. Explicit units
; must already be closed; callers invoke this only for file-derived sources.
; D0/CCR=status; other registers preserved.
endDerived	.block
	movem.l d1-d2/a0-a1, -(sp)
	movea.l a0, a1
	move.l Frame.IndexModule(a1), d1
	beq.w good
	movea.l Frame.Graph(a1), a0
	moveq #0, d0
	moveq #0, d2
	jsr graph.line
	bne.w done
	clr.l Frame.IndexModule(a1)
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; endDerived

; A0=Frame. End physical indexing after any synthetic close. Reject incomplete
; explicit modules and conditionals crossing source files. D0/CCR=status;
; other registers preserved.
endIndex	.block
	tst.w Frame.Priority(a0)
	beq.w validate
	movem.l a0-a1, -(sp)
	movea.l Frame.Scope(a0), a0
	bsr.w clearStaticValues
	movem.l (sp)+, a0-a1
	bne.w bad
validate
	tst.l Frame.IndexModule(a0)
	bne.w bad
	move.l a0, -(sp)
	lea CONDITIONS(a0), a0
	jsr conditionals.endFile
	movea.l (sp)+, a0
	tst.l d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; endIndex

; A0=Frame with [Cursor,End) set to one indexed module's capture region.
; Evaluate active imports and preceding module-level constants only. Incoming
; parameters already reside in Scope. Explicit .module/.endmodule records are
; in the span; callers open/close implicit scope with ordinary file-derived APIs.
; D0/CCR=status; other registers preserved. Marks Binding configured on success.
configure	.block
	move.l a0, -(sp)
	bsr.w beginConfiguration
	bne.w done
	bsr.w configurePart
	bne.w done
	bsr.w endConfiguration
done
	movea.l (sp)+, a0
	tst.l d0
	rts
	.bend  ; configure

; A0=Frame. Start one module's configuration before its first capture region.
; D0/CCR=status; other registers preserved. Scope is already opened if implicit.
beginConfiguration	.block
	clr.w Frame.Depth(a0)
	clr.w Frame.TemplateDepth(a0)
	clr.w Frame.Schema(a0)
	clr.w Frame.Priority(a0)
	move.l a0, -(sp)
	lea CONDITIONS(a0), a0
	jsr conditionals.beginStatic
	movea.l (sp)+, a0
	tst.l d0
	rts
	.bend  ; beginConfiguration

; A0=Frame,[Cursor,End) within one owned region. Continue the same module's
; configuration; nesting and conditional state survive region boundaries.
; D0/CCR=status; other registers preserved.
configurePart	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
next
	bsr.w resolveNext
	bne.w bad
	tst.l d1
	beq.w finish
	movea.l a1, a5
	move.l capture.Record.Bytes(a5), d7
	movea.l a5, a0
	bsr.w classify
	bne.w bad
	move.l d1, d6
	movea.l a6, a0
	bsr.w trackTemplateDepth
	tst.l d0
	bne.w advance
	tst.w Frame.Schema(a6)
	beq.w ordinaryDepth
	move.l d6, d1
	bsr.w captureSchema
	bra.w advance
ordinaryDepth
	; Nested bodies cannot contribute module configuration or guard values.
	tst.w Frame.Depth(a6)
	beq.w moduleControls
	movea.l a6, a0
	move.l d6, d1
	bsr.w trackDepth
	bmi.w bad
	bra.w advance
moduleControls
	cmpi.l #scopes.KEY_IF, d6
	blo.w active
	cmpi.l #scopes.KEY_ENDMATCH, d6
	bhi.w active
	bsr.w bindRecord
	bne.w bad
	movea.l Frame.Output(a6), a0
	movea.l Frame.Scope(a6), a1
	lea CONDITIONS(a6), a2
	jsr conditionals.line
	bne.w bad
	bra.w advance
active
	tst.w CONDITIONS+conditionals.State.Uncertain(a6)
	beq.w configurationCertaintyReady
	cmpi.l #ASSIGNMENT, d6
	bne.w configurationCertaintyReady
	bsr.w bindRecord
	bne.w bad
	bsr.w invalidateConditionalWrite
	bne.w bad
	bra.w advance
configurationCertaintyReady
	tst.w CONDITIONS+conditionals.State.Active(a6)
	beq.w advance
	cmpi.l #scopes.KEY_MODULE, d6
	beq.w moduleBoundary
	cmpi.l #scopes.KEY_ENDMODULE, d6
	beq.w moduleBoundary
	cmpi.l #scopes.KEY_STRUCT, d6
	beq.w schemaOpen
	movea.l a6, a0
	move.l d6, d1
	bsr.w trackDepth
	bmi.w bad
	bne.w advance
	cmpi.l #scopes.KEY_USE, d6
	beq.w selected
	cmpi.l #scopes.KEY_END, d6
	beq.w selected
	cmpi.l #ASSIGNMENT, d6
	bne.w advance
	tst.w Frame.Depth(a6)
	bne.w advance
	bra.w selected
schemaOpen
	move.w #SCHEMA_ACTIVE, Frame.Schema(a6)
	move.l d6, d1
	bsr.w captureSchema
	bra.w advance
moduleBoundary
	clr.w Frame.Depth(a6)
	clr.w Frame.TemplateDepth(a6)
selected
	bsr.w bindRecord
	bne.w bad
	movea.l Frame.Output(a6), a0
	movea.l Frame.Scope(a6), a1
	move.l Frame.Capacity(a6), d0
	bsr.w installConfigurationBindings
	bne.w bad
	move.l Frame.Capacity(a6), d0
	jsr scopes.configurationLine
	bne.w bad
	bra.w advance
advance
	add.l d7, Frame.Cursor(a6)
	bra.w next
finish
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; configurePart

; A0=Frame. Publish completion only after all regions of its module were scanned.
; D0/CCR=status; other registers preserved.
endConfiguration	.block
	movem.l d1/a0-a1, -(sp)
	movea.l a0, a1
	lea CONDITIONS(a0), a0
	jsr conditionals.endFile
	bne.w done
	movea.l Frame.Graph(a1), a0
	move.l Frame.Binding(a1), d1
	jsr graph.configured
done
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; endConfiguration

; A6=Frame with selected bound assignment. Invalidate only mutable snapshots
; affected by an unavailable conditional choice. D0/CCR=status; others kept.
invalidateConditionalWrite	.block
	movem.l d1/a0-a1, -(sp)
	movea.l Frame.Output(a6), a0
	movea.l Frame.Scope(a6), a1
	move.l Frame.Capacity(a6), d0
	jsr scopes.configurationStatement
	bne.w done
	jsr imports.invalidateMutable
done
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; invalidateConditionalWrite

; A6=Frame. Install the current numeric occurrence plans before snapshot VM
; evaluation. D0/CCR=status; other registers preserved.
installConfigurationBindings	.block
	movem.l a0-a1, -(sp)
	movea.l Frame.Scope(a6), a0
	movea.l a0, a1
	adda.l #layout.IMPORT_STATE+imports.KNOWN_OWNER, a1
	jsr scopes.exportBindings
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; installConfigurationBindings

; A6=Frame,A5=capture record,D1=shared directive key. Best-effort schema
; capture uses the ordinary numeric binder and struct handler. A failed geometry
; is skipped through the shared end key; ordinary preparation remains decisive.
; D0/CCR=0; other registers preserved.
captureSchema	.block
	movem.l d1-d3/a0-a1, -(sp)
	move.l d1, d3
	tst.w CONDITIONS+conditionals.State.Uncertain(a6)
	beq.w certaintyReady
	move.w #SCHEMA_UNAVAILABLE, Frame.Schema(a6)
certaintyReady
	cmpi.w #SCHEMA_UNAVAILABLE, Frame.Schema(a6)
	beq.w unavailable
	bsr.w bindRecord
	bne.w abandon
	movea.l Frame.Output(a6), a0
	movea.l Frame.Scope(a6), a1
	move.l Frame.Capacity(a6), d0
	bsr.w installConfigurationBindings
	bne.w abandon
	move.l Frame.Capacity(a6), d0
	jsr scopes.configurationStruct
	beq.w available
abandon
	movea.l Frame.Scope(a6), a0
	jsr scopes.abandonConfigurationStruct
	move.w #SCHEMA_UNAVAILABLE, Frame.Schema(a6)
unavailable
	cmpi.l #scopes.KEY_ENDSTRUCT, d3
	bne.w done
available
	cmpi.l #scopes.KEY_ENDSTRUCT, d3
	bne.w done
	clr.w Frame.Schema(a6)
	tst.w Frame.Priority(a6)
	beq.w done
	movea.l Frame.Scope(a6), a0
	clr.w layout.State.Current(a0)
done
	moveq #0, d0
	movem.l (sp)+, d1-d3/a0-a1
	rts
	.bend  ; captureSchema

; A0=configuration frame,A1=validated capture record. Read-only shared role.
; D0/CCR=status,D1=role. Other registers preserved.
captureRole	.block
	movem.l a0/a6, -(sp)
	movea.l a0, a6
	movea.l a1, a0
	bsr.w classify
	movem.l (sp)+, a0/a6
	tst.l d0
	rts
	.bend  ; captureRole

; A0=validated capture.Record. D0=0,D1=shared directive key/ASSIGNMENT/zero;
; all other registers preserved. Read TKVM structural rows only; identifier
; spelling classification uses shared scope roles and package declaration identity.
classify	.block
	movem.l d2-d5/a0-a4, -(sp)
	movea.l a0, a4
	movea.l Frame.StatementRole(a6), a3
	move.l a3, d2
	beq.w ordinaryHead
	movea.l Frame.Session(a6), a0
	movea.l a4, a1
	jsr (a3)
	tst.l d0
	bne.w invalidRole
	cmpi.l #statement_abi.HEAD_DEFINITION, d1
	beq.w statementOpen
	cmpi.l #statement_abi.HEAD_END, d1
	beq.w statementClose
ordinaryHead
	moveq #0, d5
	move.b capture.Record.Count(a4), d5
	moveq #0, d1
	cmpi.l #2, d5
	blo.w done
	lea capture.HEADER_BYTES(a4), a2
	cmpi.w #1, source.Token.Kind(a2)
	bhi.w directive
	movea.l Frame.UpdateRole(a6), a3
	move.l a3, d2
	beq.w ordinaryAssignment
	movea.l Frame.Session(a6), a0
	movea.l a4, a1
	jsr (a3)
	tst.l d0
	bne.w assignmentRecord
ordinaryAssignment
	cmpi.w #34, 20+source.Token.Kind(a2)
	beq.w assignmentRecord
	adda.w #20, a2
	subq.w #1, d5
	cmpi.w #5, source.Token.Kind(a2)
	bne.w directive
	adda.w #20, a2
	subq.w #1, d5
directive
	cmpi.l #2, d5
	blo.w done
	cmpi.w #7, source.Token.Kind(a2)
	bne.w done
	adda.w #20, a2
	cmpi.w #1, source.Token.Kind(a2)
	bhi.w done
	moveq #0, d0
	move.b capture.Record.Count(a4), d0
	mulu.w #20, d0
	lea capture.HEADER_BYTES(a4), a0
	adda.l d0, a0
	adda.l source.Token.Offset(a2), a0
	move.l source.Token.Length(a2), d0
	move.l a0, -(sp)
	move.l d0, -(sp)
	jsr scopes.classifySpelling
	move.l d0, d1
	move.l (sp)+, d0
	movea.l (sp)+, a1
	tst.l d1
	bne.w done
	movem.l d0/a1, -(sp)
	movea.l Frame.LoopRole(a6), a3
	move.l a3, d2
	beq.w noLoopRole
	movea.l Frame.Session(a6), a0
	jsr (a3)
	cmpi.l #abi.PRVM_LOOP_ORDINARY, d0
	beq.w noLoopRole
	moveq #LOOP_CLOSE, d1
	cmpi.l #abi.PRVM_LOOP_ENDFOR, d0
	beq.w loopSelected
	cmpi.l #abi.PRVM_LOOP_ENDWHILE, d0
	beq.w loopSelected
	moveq #LOOP_OPEN, d1
loopSelected
	movem.l (sp)+, d0/a1
	bra.w done
noLoopRole
	movem.l (sp)+, d0/a1
	movea.l Frame.FileRole(a6), a3
	move.l a3, d2
	beq.w declarationRole
	movem.l d0/a1, -(sp)
	movea.l Frame.Session(a6), a0
	jsr (a3)
	move.l d0, d2
	movem.l (sp)+, d0/a1
	tst.l d2
	bmi.w invalidRole
	cmpi.l #abi.FILE_ACTION_SOURCE, d2
	bne.w declarationRole
	moveq #SOURCE_INCLUDE, d1
	bra.w done
declarationRole
	movea.l Frame.DeclarationRole(a6), a3
	move.l a3, d2
	beq.w done
	movea.l Frame.Session(a6), a0
	jsr (a3)
	tst.l d0
	beq.w done
	moveq #ASSIGNMENT, d1
	bra.w done
assignmentRecord
	moveq #ASSIGNMENT, d1
	bra.w done
statementOpen
	moveq #TEMPLATE_OPEN, d1
	bra.w done
statementClose
	moveq #TEMPLATE_CLOSE, d1
done
	moveq #0, d0
	bra.w return
invalidRole
	moveq #0, d1
	moveq #1, d0
return
	movem.l (sp)+, d2-d5/a0-a4
	tst.l d0
	rts
	.bend  ; classify
	.priv
; A0=Frame,D1=shared A8/scope role. D0/CCR=1 for a template-owned record,
; including opening and closing markers, otherwise 0. Other registers kept.
; Track ownership before activity checks: inactive alternatives cannot expose
; template guards or operand names to module configuration. A closing marker
; remains owned even when its update reduces TemplateDepth to zero. The bounded
; counter retains its saturation and zero-clamped close policy.
trackTemplateDepth	.block
	moveq #0, d0
	tst.w Frame.TemplateDepth(a0)
	beq.w classifyRole
	moveq #1, d0
classifyRole
	cmpi.l #TEMPLATE_OPEN, d1
	beq.w open
	cmpi.l #scopes.KEY_MACRO, d1
	beq.w open
	cmpi.l #scopes.KEY_SEGMENT, d1
	beq.w open
	cmpi.l #TEMPLATE_CLOSE, d1
	beq.w close
	cmpi.l #scopes.KEY_ENDMACRO, d1
	beq.w close
	cmpi.l #scopes.KEY_ENDSEGMENT, d1
	bne.w done
close
	tst.w Frame.TemplateDepth(a0)
	beq.w owned
	subq.w #1, Frame.TemplateDepth(a0)
	bra.w owned
open
	cmpi.w #$ffff, Frame.TemplateDepth(a0)
	beq.w owned
	addq.w #1, Frame.TemplateDepth(a0)
owned
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; trackTemplateDepth

; A0=Frame,D1=shared directive key. D0=1 structural record consumed,
; 0 ordinary record, -1 depth overflow; CCR reflects D0. Other registers kept.
trackDepth	.block
	cmpi.l #LOOP_OPEN, d1
	beq.w openNested
	cmpi.l #LOOP_CLOSE, d1
	beq.w closeNested
	cmpi.l #scopes.KEY_BLOCK, d1
	beq.w openNested
	cmpi.l #scopes.KEY_NAMESPACE, d1
	beq.w openNested
	cmpi.l #scopes.KEY_STRUCT, d1
	beq.w openNested
	cmpi.l #scopes.KEY_ENDBLOCK, d1
	beq.w closeNested
	cmpi.l #scopes.KEY_ENDNAMESPACE, d1
	beq.w closeNested
	cmpi.l #scopes.KEY_ENDSTRUCT, d1
	beq.w closeNested
	moveq #0, d0
	rts
openNested
	addq.w #1, Frame.Depth(a0)
	beq.w bad
	moveq #1, d0
	rts
closeNested
	tst.w Frame.Depth(a0)
	beq.w consumed
	subq.w #1, Frame.Depth(a0)
consumed
	moveq #1, d0
	rts
bad
	moveq #-1, d0
	rts
	.bend  ; trackDepth

; A0=config scope. Priority-pass snapshots reset per entry module and EOF.
; D0/CCR=status; other registers preserved. Parameters have independent ownership.
clearStaticValues	.block
	jsr imports.clearKnown
	rts
	.bend  ; clearStaticValues

; A6=Frame. Bind static priority records under their indexed module identity,
; without modules.open, declarations or persistent changes to Current.
; D0/CCR=status; other registers preserved by bindCapture.
bindIndexRecord	.block
	movem.l a0-a1, -(sp)
	movea.l Frame.Scope(a6), a1
	move.w layout.State.Current(a1), -(sp)
	move.w Frame.IndexModule+2(a6), layout.State.Current(a1)
	bsr.w bindRecord
	movea.l Frame.Scope(a6), a1
	move.w (sp)+, layout.State.Current(a1)
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; bindIndexRecord

; A6=Frame. Validate the next local handle; D0/CCR=status,D1=one if present,
; A1=immutable capture record when present. Other registers preserved.
resolveNext	.block
	move.l Frame.Cursor(a6), d1
	cmp.l Frame.End(a6), d1
	beq.w end
	bhi.w bad
	addq.l #1, d1
	movea.l Frame.Arena(a6), a0
	jsr capture.resolve
	bne.w bad
	move.l Frame.End(a6), d0
	sub.l Frame.Cursor(a6), d0
	cmp.l capture.Record.Bytes(a1), d0
	blo.w bad
	moveq #1, d1
	moveq #0, d0
	rts
end
	moveq #0, d1
	moveq #0, d0
	rts
bad
	moveq #0, d1
	moveq #1, d0
	rts
	.bend  ; resolveNext

; A6=Frame. Materialize only this selected record through the ordinary writer.
; D0/CCR=status; other registers preserved by the frontend callback.
bindRecord	.block
	movea.l Frame.Session(a6), a0
	movea.l Frame.Arena(a6), a1
	move.l Frame.Cursor(a6), d1
	addq.l #1, d1
	movea.l Frame.BindCapture(a6), a2
	jsr (a2)
	rts
	.bend  ; bindRecord
	.endsection
	.endmodule

; Configuration-only discovery over owned unbound capture records.
; @opforge-owner: experimental.amigaos.binary_configuration

	.module experimental.amigaos.binary_configuration
	.cpu 68020
	.use experimental.amigaos.binary_capture as capture
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_scopes as scopes
	.use experimental.amigaos.binary_conditionals as conditionals
	.use experimental.amigaos.binary_graph as graph
	.use experimental.amigaos.binary_imports as imports
	.use experimental.amigaos.binary_memory as memory
	.pub
ASSIGNMENT = 30
Frame	.struct
Session	.long ?
Arena	.long ?
Scope	.long ?
Graph	.long ?
BindCapture	.long ?  ; frontend.bindCapture ABI; the selected writer copy only
DeclarationRole	.long ?  ; read-only package declaration-head selection callback
Output	.long ?  ; session's caller-owned writer output
Capacity	.long ?  ; writer output capacity, including shared label normalization
Cursor	.long ?  ; local capture offset, independent of the graph's logical offset
End	.long ?
IndexModule	.long ?  ; source binding index+1, no module semantic state
Binding	.long ?  ; requested graph binding for configure
Depth	.word ?
Priority	.word ?  ; physical entry only: static incoming-use root priority
Reserved	.word ?
	.endstruct
CONDITIONS = Frame.Reserved+2
FRAME_BYTES = CONDITIONS+conditionals.SCRATCH_BYTES
	.section code, kind=code
; A0=Frame with configured pointers. Clear physical index/depth state without
; resetting the persistent scope or graph. D0/CCR=status; other registers kept.
begin	.block
	clr.l Frame.IndexModule(a0)
	clr.w Frame.Depth(a0)
	clr.w Frame.Reserved(a0)
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
; D0/CCR=status; other registers preserved.
index	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
next
	bsr.w resolveNext
	bne.w bad
	tst.l d1
	beq.w good
	movea.l a1, a5
	move.l capture.Record.Bytes(a5), d7
	movea.l a5, a0
	bsr.w classify
	move.l d1, d3
	moveq #0, d4
	move.l Frame.IndexModule(a6), d5
	move.l d5, d6
	tst.w Frame.Priority(a6)
	bne.w controls
	tst.l d5
	bne.w boundary
controls
	cmpi.l #scopes.KEY_IF, d3
	blo.w outsideActive
	cmpi.l #scopes.KEY_IFNDEF, d3
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
	tst.w CONDITIONS+conditionals.State.Active(a6)
	bne.w boundary
	moveq #1, d4
	bra.w publish
boundary
	tst.w Frame.Priority(a6)
	beq.w statement
	tst.l d5
	beq.w statement
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
	jsr imports.captureScalar
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
	jsr imports.configurationTarget
	bne.w bad
	movea.l Frame.Graph(a6), a0
	movea.l Frame.Scope(a6), a1
	jsr graph.entryTarget
	bne.w bad
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
	tst.w Frame.Priority(a6)
	beq.w moduleName
	movea.l Frame.Scope(a6), a0
	bsr.w clearStaticValues
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
	tst.w Frame.Priority(a6)
	beq.w publish
	movea.l Frame.Scope(a6), a0
	bsr.w clearStaticValues
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
	tst.l capture.Record.Count(a5)
	beq.w outsideReady
	cmpi.l #scopes.KEY_END, d3
	bne.w outsideContent
	cmpi.l #2, capture.Record.Count(a5)
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
	move.l d1, d6
	cmpi.l #scopes.KEY_IF, d6
	blo.w active
	cmpi.l #scopes.KEY_IFNDEF, d6
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
	tst.w CONDITIONS+conditionals.State.Active(a6)
	beq.w advance
	cmpi.l #scopes.KEY_MODULE, d6
	beq.w moduleBoundary
	cmpi.l #scopes.KEY_ENDMODULE, d6
	beq.w moduleBoundary
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
moduleBoundary
	clr.w Frame.Depth(a6)
selected
	bsr.w bindRecord
	bne.w bad
	movea.l Frame.Output(a6), a0
	movea.l Frame.Scope(a6), a1
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

; A0=validated capture.Record. D0=0,D1=shared directive key/ASSIGNMENT/zero;
; all other registers preserved. Read TKVM structural rows only; identifier
; spelling classification uses shared scope roles and package declaration identity.
classify	.block
	movem.l d2-d5/a0-a4, -(sp)
	movea.l a0, a4
	move.l capture.Record.Count(a4), d5
	moveq #0, d1
	cmpi.l #2, d5
	blo.w done
	lea capture.HEADER_BYTES(a4), a2
	cmpi.w #1, source.Token.Kind(a2)
	bhi.w directive
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
	move.l capture.Record.Count(a4), d0
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
done
	moveq #0, d0
	movem.l (sp)+, d2-d5/a0-a4
	rts
	.bend  ; classify
	.priv
; A0=Frame,D1=shared directive key. D0=1 structural record consumed,
; 0 ordinary record, -1 depth overflow; CCR reflects D0. Other registers kept.
trackDepth	.block
	cmpi.l #scopes.KEY_BLOCK, d1
	beq.w openNested
	cmpi.l #scopes.KEY_NAMESPACE, d1
	beq.w openNested
	cmpi.l #scopes.KEY_MACRO, d1
	beq.w openNested
	cmpi.l #scopes.KEY_SEGMENT, d1
	beq.w openNested
	cmpi.l #scopes.KEY_STRUCT, d1
	beq.w openNested
	cmpi.l #scopes.KEY_ENDBLOCK, d1
	beq.w closeNested
	cmpi.l #scopes.KEY_ENDNAMESPACE, d1
	beq.w closeNested
	cmpi.l #scopes.KEY_ENDMACRO, d1
	beq.w closeNested
	cmpi.l #scopes.KEY_ENDSEGMENT, d1
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

; A0=config scope. The priority pass owns no declarations or incoming params.
; Its known values are transient and reset for each entry module and at EOF.
; Preserve all registers; CCR unspecified. Empty known-value storage is safe.
clearStaticValues	.block
	movem.l d0/a0-a1, -(sp)
	lea layout.IMPORT_STATE+imports.KNOWN_DEFINED(a0), a0
	move.l memory.Block.Used(a0), d0
	beq.w done
	movea.l memory.Block.Pointer(a0), a1
clear
	clr.b (a1)+
	subq.l #1, d0
	bne.w clear
done
	movem.l (sp)+, d0/a0-a1
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

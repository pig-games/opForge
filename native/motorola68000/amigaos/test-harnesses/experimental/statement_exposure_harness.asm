; Direct source-facet/exposure metadata ABI controls. Rust source probes supply
; visible-key expectations; import descriptors are acquired numeric inputs here.
.module main
.cpu 68020
.use experimental.amigaos.binary_scopes as scopes
.use experimental.amigaos.binary_imports as imports
.use experimental.amigaos.binary_modules as modules
.use experimental.amigaos.binary_scope_layout as layout
.use experimental.amigaos.binary_binding_records as records
.use experimental.amigaos.binary_memory as memory
.use experimental.amigaos.binary_graph as graph
.priv
EXEC_BASE = 4
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
	.pub
	.section entry, kind=code
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	move.l #20, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l EXEC_BASE.w, a6
	jsr EXEC_OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done
	move.l d0, DosBase
	movea.l d0, a6
	move.l #OutputName, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, OutputHandle
	beq.w closeLibrary
	jsr cases
	move.l d0, ReturnCode
	movea.l DosBase, a6
	move.l OutputHandle, d1
	jsr DOS_CLOSE(a6)
	tst.l d0
	bne.w closeLibrary
	move.l #20, ReturnCode
closeLibrary
	movea.l DosBase, a1
	movea.l EXEC_BASE.w, a6
	jsr EXEC_CLOSE_LIBRARY(a6)
done
	move.l ReturnCode, d0
	tst.l d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; start
	.endsection
	.priv
	.section code, kind=code
; Component ownership boundaries: force both row owners to relocate, reject
; a wrapping word-link count, then build NEXT above signed-word displacement.
; Returns D0/CCR=status, preserving other registers and existing fixture outputs.
edgeStorage	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea ScopeState+layout.IMPORT_STATE, a6
	movea.l a6, a0
	moveq #1, d0
	jsr imports.reserveItems
	bne.w bad
	movea.l imports.ITEMS_POINTER(a6), a1
	move.w #$1234, (a1)
	move.l #imports.ITEM_BYTES, imports.ITEMS+memory.Block.Used(a6)
	movea.l a1, a2
	move.l #257, d0
	jsr imports.reserveItems
	bne.w bad
	movea.l imports.ITEMS_POINTER(a6), a1
	cmpa.l a2, a1
	beq.w bad
	cmpi.w #$1234, (a1)
	bne.w bad
	move.l imports.ITEMS+memory.Block.Capacity(a6), d3
	movea.l a1, a2
	move.l #65536, d0
	jsr imports.reserveItems
	beq.w bad
	cmpa.l imports.ITEMS_POINTER(a6), a2
	bne.w bad
	cmp.l imports.ITEMS+memory.Block.Capacity(a6), d3
	bne.w bad
	cmpi.l #imports.ITEM_BYTES, imports.ITEMS+memory.Block.Used(a6)
	bne.w bad
	moveq #1, d0
	jsr imports.reserveSelections
	bne.w bad
	movea.l imports.SELECTIONS_POINTER(a6), a1
	move.w #$2345, (a1)
	move.l #imports.SELECTION_BYTES, imports.SELECTIONS+memory.Block.Used(a6)
	movea.l a1, a2
	move.l #257, d0
	jsr imports.reserveSelections
	bne.w bad
	movea.l imports.SELECTIONS_POINTER(a6), a1
	cmpa.l a2, a1
	beq.w bad
	cmpi.w #$2345, (a1)
	bne.w bad
	movea.l a6, a0
	jsr imports.begin
	beq.w bad  ; live descriptors must not be cleared
	cmpa.l imports.SELECTIONS_POINTER(a6), a1
	bne.w bad
	; Many edges, one independently bounded graph node. Each edge requests the
	; absent binding2; order must reverse every link before reporting that miss.
	move.l #32769, d0
	jsr imports.reserveItems
	bne.w bad
	movea.l imports.ITEMS_POINTER(a6), a1
	moveq #0, d2
fill
	move.w #1, imports.Item.Target(a1)
	clr.w imports.Item.Qualifier(a1)
	clr.w imports.Item.Selected(a1)
	clr.w imports.Item.Unqualified(a1)
	move.w d2, imports.Item.Next(a1)
	clr.w imports.Item.Flags(a1)
	adda.w #imports.ITEM_BYTES, a1
	addq.l #1, d2
	cmpi.l #32769, d2
	blo.w fill
	move.w d2, imports.COUNT(a6)
	mulu.w #imports.ITEM_BYTES, d2
	move.l d2, imports.ITEMS+memory.Block.Used(a6)
	movea.l imports.HEADS_POINTER(a6), a1
	move.w #32769, (a1)
	lea EdgeGraph, a0
	jsr graph.begin
	bne.w bad
	moveq #4, d0
	moveq #0, d1
	moveq #1, d2
	jsr graph.line
	bne.w bad
	moveq #4, d0
	moveq #1, d1
	moveq #0, d2
	jsr graph.line
	bne.w bad
	lea ScopeState, a1
	lea EdgeSpan, a2
	moveq #graph.SPAN_BYTES, d0
	jsr graph.order
	cmpi.l #2, d0
	bne.w bad
	cmpi.w #2, d1
	bne.w bad
	movea.l EdgeGraph+graph.NEXT_POINTER, a1
	cmpi.w #2, (a1)
	bne.w bad
	move.l #32768*2, d0
	tst.w 0(a1, d0.l)
	bne.w bad  ; final source-order link has no successor
	lea EdgeGraph, a0
	jsr graph.begin
	beq.w bad  ; NEXT remains owned across retry/error
	jsr graph.release
	tst.l EdgeGraph+graph.NEXT_POINTER
	bne.w bad
	; Reuse the grown row owners for the original exposure fixtures.
	movea.l imports.HEADS_POINTER(a6), a1
	clr.w (a1)
	clr.w imports.COUNT(a6)
	clr.w imports.SELECTED_COUNT(a6)
	move.l #imports.ITEM_BYTES*2, imports.ITEMS+memory.Block.Used(a6)
	move.l #imports.SELECTION_BYTES, imports.SELECTIONS+memory.Block.Used(a6)
	moveq #0, d0
	bra.w done
bad
	lea EdgeGraph, a0
	jsr graph.release
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; edgeStorage

cases .block
 lea ScopeState,a0
 moveq #100,d0
 moveq #0,d1
 jsr scopes.begin
 bne.w bad
	; Scenario rows borrow owned import storage; reserve before copying fixtures.
	lea ScopeState+layout.IMPORT_STATE, a0
	moveq #2, d0
	jsr imports.reserveItems
	bne.w bad
	moveq #1, d0
	jsr imports.reserveSelections
 bne.w bad
 lea Names,a2
 moveq #3,d7
bindNames
 movea.l (a2)+,a0
 move.l (a2)+,d0
 lea ScopeState,a1
 jsr scopes.bindOrigin
 bne.w bad
 dbra d7,bindNames
	bsr edgeStorage
	bne.w bad
 ; Source module is entry0; declaration is module-flat even in lexical block2.
 move.w #1,ScopeState+layout.MODULE_STATE+modules.State.Active
 move.w #1,ScopeState+layout.MODULE_STATE+modules.State.Visibility
 move.w #2,ScopeState+layout.State.Current
 lea Keyword,a0
 moveq #4,d0
 lea ScopeState,a1
 jsr scopes.bindStatementKeyword
 bne.w bad
 cmpi.w #104,d1
 bne.w bad
 lea NameToken,a0
 lea ScopeState,a1
 moveq #1,d0
 jsr scopes.declareStatement
 bne.w bad
 ; Same identity may also own a dot-template facet.
 lea NameToken,a0
 lea ScopeState,a1
 jsr scopes.declareTemplate
 bne.w bad
 ; Private overload cannot clear aggregate public existence.
 clr.w ScopeState+layout.MODULE_STATE+modules.State.Visibility
 lea NameToken,a0
 lea ScopeState,a1
 moveq #1,d0
 jsr scopes.declareStatement
 bne.w bad
 movea.l ScopeState+layout.ENTRIES_POINTER,a0
 lea 4*records.ENTRY_BYTES(a0),a0
 moveq #0,d0
 move.w records.Entry.TemplateFlags(a0),d0
 move.l d0,Summary
 moveq #0,d0
 move.w records.Entry.StatementModule(a0),d0
 move.l d0,Summary+4
 lea Summary,a0
 moveq #8,d0
 bsr writeSpan
 bne.w bad
 ; Switch caller module to app (entry1), retaining a different lexical block.
 move.w #2,ScopeState+layout.MODULE_STATE+modules.State.Active
 move.w #3,ScopeState+layout.State.Current
	move.l #imports.ITEM_BYTES*2, ScopeState+layout.IMPORT_STATE+imports.ITEMS+memory.Block.Used
	move.l #imports.SELECTION_BYTES, ScopeState+layout.IMPORT_STATE+imports.SELECTIONS+memory.Block.Used
 lea Scenarios,a2
 moveq #7,d7
scenario
 ; Scenario: active module, visibility, count, two Item rows, one Selection.
 move.w (a2)+,d0
 move.w d0,ScopeState+layout.MODULE_STATE+modules.State.Active
 moveq #0,d0
 move.w (a2)+,d0
 move.l d0,Frame+imports.TemplateExposureFrame.Visibility
 move.w (a2)+,d0
 move.w d0,ScopeState+layout.IMPORT_STATE+imports.COUNT
	movea.l ScopeState+layout.IMPORT_STATE+imports.ITEMS_POINTER, a0
 moveq #imports.ITEM_BYTES/2*2-1,d0
items
 move.w (a2)+,(a0)+
 dbra d0,items
 move.w #1,ScopeState+layout.IMPORT_STATE+imports.SELECTED_COUNT
	movea.l ScopeState+layout.IMPORT_STATE+imports.SELECTIONS_POINTER, a0
 moveq #imports.SELECTION_BYTES/2-1,d0
selection
 move.w (a2)+,(a0)+
 dbra d0,selection
 movea.l ScopeState+layout.IMPORT_STATE+imports.HEADS_POINTER,a0
 clr.w 2(a0) ; app source index1's import head
 tst.w ScopeState+layout.IMPORT_STATE+imports.COUNT
 beq.w headReady
 move.w #1,2(a0)
headReady
 lea Cursor,a0
 moveq #imports.EXPOSURE_CURSOR_BYTES/4-1,d0
clearCursor
 clr.l (a0)+
 dbra d0,clearCursor
 clr.l Frame+imports.TemplateExposureFrame.KeyUsed
next
 lea Frame,a0
 jsr scopes.templateExposure
 bne.w bad
 tst.l d1
 beq.w exhausted
 move.l Row+imports.ExposureRow.KeyBytes,Summary
 move.l Row+imports.ExposureRow.ImportOrder,Summary+4
 lea Summary,a0
 moveq #8,d0
 bsr writeSpan
 bne.w bad
 lea KeyArena,a0
 adda.l Row+imports.ExposureRow.KeyOffset,a0
 move.l Row+imports.ExposureRow.KeyBytes,d0
 bsr writeSpan
 bne.w bad
 bra.w next
exhausted
 dbra d7,scenario
 lea ScopeState,a0
 jsr scopes.release
 moveq #0,d0
 rts
bad
 lea ScopeState,a0
 jsr scopes.release
 moveq #20,d0
 rts
 .bend
writeSpan	.block
	movem.l d1-d5/a0-a6, -(sp)
	movea.l a0, a3
	move.l d0, d5
next
	tst.l d5
	beq.w valid
	movea.l DosBase, a6
	move.l OutputHandle, d1
	move.l a3, d2
	move.l d5, d3
	jsr DOS_WRITE(a6)
	tst.l d0
	ble.w fail
	cmp.l d5, d0
	bhi.w fail
	adda.l d0, a3
	sub.l d0, d5
	bra.w next
valid
	moveq #0, d0
	bra.w done
fail
	moveq #1, d0
done
	tst.l d0
	movem.l (sp)+, d1-d5/a0-a6
	rts
	.bend  ; writeSpan
 .endsection
.section data,kind=data
DosName .byte "dos.library",0
OutputName .byte "Work:statement-exposure-results.bin",0
ModuleName .byte "foo.bar"
CallerName .byte "app"
AliasName .byte "bar"
SelectedAlias .byte "X"
Keyword .byte "PUSH"
.align 4
Names .long ModuleName,7,CallerName,3,AliasName,3,SelectedAlias,1
NameToken .byte 0,0,104,0
; Row descriptors deliberately use owner fields, not another source parser.
Scenarios
; default qualified: only full module key
.word 2,1,1, 0,2,0,0,0,0, 0,0,0,0,0,0, 5,4,0
; explicit same leaf alias: full + bar key
.word 2,1,1, 0,2,0,0,0,imports.EXPLICIT_QUALIFIER, 0,0,0,0,0,0, 5,4,0
; wildcard
.word 2,1,1, 0,0,0,2,0,0, 0,0,0,0,0,0, 5,4,0
; selected X, unqualified
.word 2,1,1, 0,2,1,1,0,0, 0,0,0,0,0,0, 5,4,0
; selected original keyword, explicit bar qualifier
.word 2,1,1, 0,2,1,0,0,imports.EXPLICIT_QUALIFIER, 0,0,0,0,0,0, 5,0,0
; local private overload ignores unrelated lexical Current
.word 1,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0, 5,4,0
; private overload imported nowhere (aggregate public is insufficient)
.word 2,0,1, 0,0,0,2,0,0, 0,0,0,0,0,0, 5,4,0
; two import ordinal descriptors; replacement belongs to A9 grouping owner
.word 2,1,2, 0,0,0,2,2,0, 0,0,0,2,0,0, 5,4,0
.align 4
Frame .long ScopeState,104,Keyword,4,Cursor,KeyArena,256,0,Row,imports.EXPOSURE_ROW_BYTES,1
.endsection
.section bss,kind=bss
.align 4
DosBase .res long,1
OutputHandle .res long,1
ReturnCode .res long,1
Summary .res long,2
Cursor .res byte,imports.EXPOSURE_CURSOR_BYTES
Row .res byte,imports.EXPOSURE_ROW_BYTES
KeyArena .res byte,256
ScopeState .res byte,scopes.SCRATCH_BYTES
EdgeGraph	.res byte, graph.SCRATCH_BYTES
EdgeSpan	.res byte, graph.SPAN_BYTES
.endsection
.output "statement-exposure.hunk",format=hunk,sections=entry,code,data,bss
.endmodule

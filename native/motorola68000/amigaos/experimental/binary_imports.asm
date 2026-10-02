; Preparation-only module aliases and origin-specific qualified reference IDs.
; @opforge-owner: experimental.amigaos.binary_imports
	.module experimental.amigaos.binary_imports
	.cpu 68020
	.include "telemetry_macros.i"
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_modules as modules
	.use experimental.amigaos.binary_section_prepare as sections
	.use opasm.amigaos.binary_expression as expression
	.use experimental.amigaos.binary_package as pkg
	.use exprvm.amigaos.runtime as runtime
	.pub
Item	.struct
Target	.word ?
Qualifier	.word ?
Selected	.word ?
Unqualified	.word ?
Next	.word ?
.endstruct
Selection	.struct
Name	.word ?
Alias	.word ?
Next	.word ?
.endstruct
ITEM_BYTES = Item.Next+2
SELECTION_BYTES = Selection.Next+2
COUNT = 0
LIST_LIMIT = 512
HEADS = 2
HEADS_POINTER = HEADS+memory.Block.Pointer
ITEMS = HEADS+memory.Block.Used+4
SELECTED_COUNT = ITEMS+LIST_LIMIT*ITEM_BYTES
SELECTIONS = SELECTED_COUNT+2
PROXIES = SELECTIONS+LIST_LIMIT*SELECTION_BYTES
PARAM_COUNT = PROXIES+256*2
PARAMS = PARAM_COUNT+2
PARAM_BYTES = pkg.PARAMETER_BYTES
KNOWN_VALUES = PARAMS+LIST_LIMIT*PARAM_BYTES
KNOWN_VALUES_POINTER = KNOWN_VALUES+memory.Block.Pointer
KNOWN_DEFINED = KNOWN_VALUES+memory.Block.Used+4
KNOWN_DEFINED_POINTER = KNOWN_DEFINED+memory.Block.Pointer
EXPRESSION_SCRATCH = KNOWN_DEFINED+memory.Block.Used+4
SCRATCH_BYTES = EXPRESSION_SCRATCH+256
PROXY = 8
	.section code, kind=code

; A0=import scratch. D0/CCR=status; other registers preserved.
begin	.block
	movem.l d1/a0, -(sp)
	move.w #SCRATCH_BYTES/2-1, d1
clear
	clr.w (a0)+
	dbra d1, clear
	movem.l (sp)+, d1/a0
	moveq #0, d0
	rts
	.bend  ; begin

; A0=import state,D0=minimum identity slots. D0/CCR=status; others kept.
reserve	.block
	cmpi.l #65535, d0
	bhi.w bad
	movem.l d1-d2/a0-a1, -(sp)
	movea.l a0, a1
	move.l d0, d2
	move.l d0, d1
	add.l d1, d1
	lea HEADS(a1), a0
	move.l d1, d0
	jsr memory.reserve
	bne.w done
	cmp.l memory.Block.Used(a0), d1
	bls.w extent1
	move.l d1, memory.Block.Used(a0)
extent1
	lsl.l #2, d1
	lea KNOWN_VALUES(a1), a0
	move.l d1, d0
	jsr memory.reserve
	bne.w done
	cmp.l memory.Block.Used(a0), d1
	bls.w extent2
	move.l d1, memory.Block.Used(a0)
extent2
	lea KNOWN_DEFINED(a1), a0
	move.l d2, d0
	jsr memory.reserve
	bne.w done
	cmp.l memory.Block.Used(a0), d2
	bls.w extent3
	move.l d2, memory.Block.Used(a0)
extent3
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; reserve

; A0=import state. Release every owned per-identity block; registers kept.
release	.block
	move.l a0, -(sp)
	lea HEADS(a0), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movea.l (sp), a0
	lea KNOWN_VALUES(a0), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movea.l (sp), a0
	lea KNOWN_DEFINED(a0), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movea.l (sp)+, a0
	rts
	.bend  ; release

; A0=normalized writer record,A1=scope state. Retain module/global-scope
; assignments whose values are known at this source position. Global scope
; is the zero module/current identity; nested lexical scopes remain excluded.
; The existing
; expression VM handles arithmetic; labels and forward values remain unknown.
; D0/CCR=status; other registers preserved.
captureConstant	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a1, a6
	; Known scalar values remain keyed by their lexical source IDs. Keeping
	; local assignments here also serves first-pass unit/conditional consumers.
	moveq #0, d0
	move.b (a0), d0
	addq.w #1, d0
	cmpi.w #10, d0
	blo.w constantOk
	cmpi.b #1, 4(a0)
	bhi.w constantOk
	cmpi.b #34, 8(a0)
	bne.w constantOk
	movea.l a0, a1
	adda.l d0, a1
	moveq #0, d7
	move.w 5(a0), d7
	lea 9(a0), a0
	bsr.w evaluateRange
	bne.w constantUnknown
	move.l d7, d0
	sub.w layout.State.Base(a6), d0
	bcs.w constantOk
	andi.l #$ffff, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w constantOk
	lea layout.IMPORT_STATE(a6), a0
	move.l d0, d4
	lsl.l #3, d4
	movea.l KNOWN_VALUES_POINTER(a0), a1
	move.l d1, runtime.Value.Low(a1, d4.l)
	move.l d2, runtime.Value.High(a1, d4.l)
	movea.l KNOWN_DEFINED_POINTER(a0), a1
	move.b #1, 0(a1, d0.l)
	bra.w constantOk
constantUnknown
	move.l d7, d0
	sub.w layout.State.Base(a6), d0
	bcs.w constantOk
	andi.l #$ffff, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w constantOk
	lea layout.IMPORT_STATE(a6), a0
	move.l d0, d2
	lsl.l #3, d2
	movea.l KNOWN_VALUES_POINTER(a0), a1
	adda.l d2, a1
	clr.l 0(a1)
	clr.l 4(a1)
	suba.l d2, a1
	movea.l KNOWN_DEFINED_POINTER(a0), a1
	adda.l d0, a1
	clr.b 0(a1)
	suba.l d0, a1
constantOk
	moveq #0, d0
constantDone
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; captureConstant

; A0=first .use target token,A1=scope,A2=ordinary binder callback.
; Resolve only its shared canonical module identity; no suffix, parameters or
; outgoing edge is executed. D0/CCR=status,D1=source module index+1;
; other registers preserved. The same primitive serves normal imports.line.
configurationTarget	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a1, a6
	movea.l a2, a5
	bsr.w boundModuleTarget
	bne.w done
	sub.w layout.State.Base(a6), d1
	bcs.w bad
	andi.l #$ffff, d1
	addq.l #1, d1
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; configurationTarget
	.priv
; A0=target token,A5=binder,A6=scope. Shared lexical target extraction and
; global binding. D0/CCR=status,D1=canonical source ID; caller supplies bounds.
boundModuleTarget	.block
	bsr.w tokenName
	bne.w done
	bsr.w globalBind
done
	tst.l d0
	rts
	.bend  ; boundModuleTarget
	.pub

; A0=.use token,A1=scope state,A2=binder callback,A3=section state,
; A4=record end.
; One or more selected names and an optional module alias; references stay separate.
; D0/CCR=status; other registers preserved.
line	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l a3, -(sp)
	movea.l a1, a6
	movea.l a2, a5
	moveq #0, d6
	move.w layout.MODULE_STATE+modules.State.Active(a6), d6
	beq.w bad
	cmp.w layout.State.Current(a6), d6
	bne.w bad
	addq.l #5, a0
	movea.l a0, a3
	move.l a4, d0
	sub.l a3, d0
	cmpi.l #4, d0
	blo.w bad
	bsr.w boundModuleTarget
	bne.w bad
	move.l d1, d7
	sub.w layout.State.Base(a6), d7
	andi.l #$ffff, d7
	addq.l #4, a3
	movea.l a3, a0
	movea.l a6, a1
	movea.l (sp), a2
	move.l d7, d0
	jsr sections.importMap
	bne.w bad
	movea.l a3, a0
	bsr.w parameters
	bne.w bad
	moveq #0, d4  ; selected-name list head, zero means no selection
	moveq #0, d3  ; direct unqualified access
	cmpa.l a4, a3
	beq.w defaultQualifier
	cmpi.b #14, (a3)
	bne.w afterSelection
	addq.l #1, a3
	cmpa.l a4, a3
	bhs.w bad
	cmpi.b #20, (a3)
	bne.w selectedName
	addq.l #1, a3
	cmpa.l a4, a3
	bhs.w bad
	cmpi.b #15, (a3)
	bne.w bad
	addq.l #1, a3
	moveq #2, d3  ; wildcard direct availability
	bra.w afterSelection
selectedName
	move.l a4, d0
	sub.l a3, d0
	cmpi.l #4, d0
	blo.w bad
	tst.b 3(a3)
	bne.w bad  ; this bounded form selects unqualified names
	movea.l a3, a0
	bsr.w tokenName
	bne.w bad
	movea.l a0, a2
	move.l d0, d3
	move.l d7, d0
	bsr.w entryName
	add.l d0, d3
	addq.l #1, d3
	cmpi.l #layout.NAME_BYTES-1, d3
	bhi.w bad
	lea layout.BUFFER(a6), a1
	move.l d0, d1
copyTarget
	move.b (a0)+, (a1)+
	subq.l #1, d1
	bne.w copyTarget
	move.b #'.', (a1)+
	movea.l a2, a0
	move.l d3, d1
	sub.l d0, d1
	subq.l #1, d1
copySelected
	move.b (a0)+, (a1)+
	subq.l #1, d1
	bne.w copySelected
	lea layout.BUFFER(a6), a0
	move.l d3, d0
	bsr.w globalBind
	bne.w bad
	sub.w layout.State.Base(a6), d1
	andi.l #$ffff, d1
	addq.w #1, d1
	move.w d1, d3  ; original selected target
	moveq #0, d5  ; exposed alias, zero keeps the original leaf
	addq.l #4, a3
	cmpa.l a4, a3
	bhs.w bad
	cmpi.b #1, (a3)
	bhi.w findSelection
	tst.b 3(a3)
	bne.w bad
	movea.l a3, a0
	bsr.w tokenName
	bne.w bad
	cmpi.l #2, d0
	bne.w bad
	move.w (a0), d0
	ori.w #$2020, d0
	cmpi.w #$6173, d0  ; as
	bne.w bad
	addq.l #4, a3
	move.l a4, d0
	sub.l a3, d0
	cmpi.l #4, d0
	blo.w bad
	tst.b 3(a3)
	bne.w bad
	movea.l a3, a0
	bsr.w tokenName
	bne.w bad
	moveq #0, d5
	move.w 1(a3), d5
	sub.w layout.State.Base(a6), d5
	andi.l #$ffff, d5
	addq.w #1, d5
	addq.l #4, a3
findSelection
	lea layout.IMPORT_STATE(a6), a2
	move.l d4, d2
seenName
	tst.w d2
	beq.w appendName
	move.l d2, d0
	subq.w #1, d0
	mulu.w #SELECTION_BYTES, d0
	lea SELECTIONS(a2), a0
	adda.l d0, a0
	cmp.w Selection.Name(a0), d3
	bne.w nextSeenName
	cmp.w Selection.Alias(a0), d5
	beq.w nextNameToken
nextSeenName
	moveq #0, d2
	move.w Selection.Next(a0), d2
	bra.w seenName
appendName
	moveq #0, d2
	move.w SELECTED_COUNT(a2), d2
	cmpi.w #LIST_LIMIT, d2
	bhs.w bad
	move.l d2, d0
	mulu.w #SELECTION_BYTES, d0
	lea SELECTIONS(a2), a0
	adda.l d0, a0
	move.w d3, Selection.Name(a0)
	move.w d5, Selection.Alias(a0)
	move.w d4, Selection.Next(a0)
	move.l d2, d4
	addq.w #1, d4
	move.w d4, SELECTED_COUNT(a2)
nextNameToken
	cmpa.l a4, a3
	bhs.w bad
	cmpi.b #4, (a3)
	beq.w anotherName
	cmpi.b #15, (a3)
	bne.w bad
	addq.l #1, a3
	moveq #1, d3
	bra.w afterSelection
anotherName
	addq.l #1, a3
	bra.w selectedName
afterSelection
	cmpa.l a4, a3
	beq.w defaultQualifier
	cmpi.w #2, d3
	beq.w bad  ; wildcard aliases are not supported
	move.l d4, d2
	lea layout.IMPORT_STATE(a6), a2
qualifiedSelection
	tst.w d2
	beq.w parseQualifier
	move.l d2, d0
	subq.w #1, d0
	mulu.w #SELECTION_BYTES, d0
	lea SELECTIONS(a2), a0
	adda.l d0, a0
	tst.w Selection.Alias(a0)
	bne.w bad  ; per-item aliases expose direct names only
	moveq #0, d2
	move.w Selection.Next(a0), d2
	bra.w qualifiedSelection
parseQualifier
	move.l a4, d0
	sub.l a3, d0
	cmpi.l #8, d0
	bne.w bad
	tst.b 3(a3)
	bne.w bad
	movea.l a3, a0
	bsr.w tokenName
	bne.w bad
	cmpi.l #2, d0
	bne.w bad
	move.w (a0), d0
	ori.w #$2020, d0
	cmpi.w #$6173, d0  ; as
	bne.w bad
	lea 4(a3), a0
	tst.b 3(a0)
	bne.w bad
	bsr.w tokenName
	bne.w bad
	moveq #0, d3
	bra.w qualifier
defaultQualifier
	cmpi.w #2, d3
	beq.w noQualifier
	tst.w d4
	bne.w noQualifier
	move.l d7, d0
	bsr.w entryLeaf
	bra.w qualifier
noQualifier
	moveq #0, d5
	bra.w collisionStart
qualifier
	movea.l a6, a1
	jsr (a5)
	tst.l d0
	bne.w bad
	sub.w layout.State.Base(a6), d1
	andi.l #$ffff, d1
	move.l d1, d5
collisionStart
	lea layout.IMPORT_STATE(a6), a4
	move.l d6, d0
	subq.w #1, d0
	andi.l #$ffff, d0
	add.l d0, d0
	movea.l HEADS_POINTER(a4), a3
	adda.l d0, a3
	moveq #0, d2
	move.w (a3), d2
collision
	tst.w d2
	beq.w append
	subq.w #1, d2
	mulu.w #ITEM_BYTES, d2
	lea ITEMS(a4), a0
	adda.l d2, a0
	tst.w d5
	beq.w nextCollision
	cmp.w Item.Qualifier(a0), d5
	beq.w bad
nextCollision
	moveq #0, d2
	move.w Item.Next(a0), d2
	bra.w collision
append
	moveq #0, d0
	move.w COUNT(a4), d0
	cmpi.w #LIST_LIMIT, d0
	bhs.w bad
	move.l d0, d1
	mulu.w #ITEM_BYTES, d1
	lea ITEMS(a4), a0
	adda.l d1, a0
	move.w d7, Item.Target(a0)
	move.w d5, Item.Qualifier(a0)
	move.w d4, Item.Selected(a0)
	move.w d3, Item.Unqualified(a0)
	move.w (a3), Item.Next(a0)
	addq.w #1, d0
	move.w d0, (a3)
	move.w d0, COUNT(a4)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	addq.l #4, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; line

; A0=name token,A1=scope state. Imported references get a proxy keyed by
; (module, original ID), sharing name bytes. D0/CCR=status; others kept.
reference	.block
	moveq #records.NAMESPACE_VALUE, d0
	bra.w referenceNamespace
	.bend  ; reference

; A0=name token,A1=scope state. Resolve invocation spelling independently
; of a numeric reference with the same spelling and origin.
; D0/CCR=status; other registers preserved.
referenceTemplate	.block
	moveq #records.NAMESPACE_TEMPLATE, d0
	bra.w referenceNamespace
	.bend  ; referenceTemplate

	.priv
; D0=namespace,A0=token,A1=scope state. The namespace joins the existing
; module, spelling and lexical-origin proxy key. Other registers preserved.
referenceNamespace	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.w d0, -(sp)
	movea.l a1, a6
	movea.l a0, a5
	moveq #0, d4
	moveq #0, d7
	move.w layout.MODULE_STATE+modules.State.Active(a6), d7
	beq.w ok
	tst.b 3(a0)
	bne.w referenceProxy
	moveq #0, d0
	move.w 1(a0), d0
	sub.w layout.State.Base(a6), d0
	bcs.w ok  ; package-owned names have no import selection
	andi.l #$ffff, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	move.w (sp), d0
	bsr.w selectedTarget
	bne.w bad
	tst.w d4
	beq.w ok
	cmpi.w #$ffff, d4
	bne.w referenceProxy
	; Wildcard availability must not capture an existing lexical declaration
	; (for example the consumer's own struct size). Named selections retain
	; their explicit precedence. Test the requested namespace independently.
	moveq #0, d0
	move.w 1(a5), d0
	sub.w layout.State.Base(a6), d0
	andi.l #$ffff, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	tst.w (sp)
	bne.w wildcardTemplateDeclaration
	btst #0, records.Entry.Flags+1(a3)
	bne.w ok
	bra.w referenceProxy
wildcardTemplateDeclaration
	btst #0, records.Entry.TemplateFlags+1(a3)
	bne.w ok
referenceProxy
	moveq #0, d6
	move.w 1(a0), d6
	sub.w layout.State.Base(a6), d6
	bcs.w ok
	andi.l #$ffff, d6
	cmp.w layout.State.Count(a6), d6
	bhs.w bad
	move.l d6, d0
	eor.w d7, d0
	andi.w #255, d0
	andi.l #$ffff, d0
	add.l d0, d0
	lea layout.IMPORT_STATE(a6), a4
	lea PROXIES(a4), a4
	adda.l d0, a4
	moveq #0, d2
	move.w (a4), d2
find
	tst.w d2
	beq.w allocate
	subq.w #1, d2
	move.l d2, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	moveq #records.NAMESPACE_VALUE, d0
	btst #records.TEMPLATE_PROXY_BIT, records.Entry.Flags+1(a3)
	beq.w namespaceReady
	moveq #records.NAMESPACE_TEMPLATE, d0
namespaceReady
	cmp.w (sp), d0
	bne.w next
	cmp.w records.Entry.Owner(a3), d7
	bne.w next
	move.w layout.State.Current(a6), d0
	cmp.w records.Entry.Padding(a3), d0
	bne.w next  ; the same spelling in distinct lexical scopes may shadow
	move.w records.Entry.ScopeKind(a3), d0
	subq.w #1, d0
	cmp.w d6, d0
	beq.w found
next
	moveq #0, d2
	move.w records.Entry.Next(a3), d2
	bra.w find
allocate
	moveq #0, d2
	move.w layout.State.Count(a6), d2
	move.l d2, d0
	addq.l #1, d0
	movea.l layout.State.ReserveRoutine(a6), a0
	move.l a0, d1
	beq.w bad
	movea.l a0, a1
	movea.l a6, a0
	jsr (a1)
	tst.l d0
	bne.w bad
	move.l d2, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	move.l d6, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a0
	adda.l d0, a0
	move.l records.Entry.Name(a0), records.Entry.Name(a3)
	move.w records.Entry.Length(a0), records.Entry.Length(a3)
	move.w d7, records.Entry.Owner(a3)
	move.w d4, records.Entry.Leaf(a3)  ; selected target index+1, or zero
	move.w #PROXY, records.Entry.Flags(a3)
	tst.w (sp)
	beq.w proxyNamespaceReady
	ori.w #records.TEMPLATE_PROXY, records.Entry.Flags(a3)
proxyNamespaceReady
	move.w d6, records.Entry.ScopeKind(a3)
	addq.w #1, records.Entry.ScopeKind(a3)
	move.w layout.State.Current(a6), records.Entry.Padding(a3)
	clr.w records.Entry.MemberBase(a3)
	clr.w records.Entry.TemplateModule(a3)
	clr.w records.Entry.TemplateFlags(a3)
	move.w (a4), records.Entry.Next(a3)
	move.w d2, d0
	addq.w #1, d0
	move.w d0, (a4)
	move.w d0, layout.State.Count(a6)
	mulu.w #records.ENTRY_BYTES, d0
	move.l d0, layout.ENTRIES+memory.Block.Used(a6)
	move.w layout.State.Base(a6), d0
	add.w d2, d0
	move.w d0, records.Entry.Target(a3)
	; Templates have no struct-member interpretation or numeric value remap.
	tst.w (sp)
	bne.w found
	; Struct definitions must exist at the reference site. Import and absolute
	; targets may still be forward references, so only gate the member fallback.
	movea.l layout.ARENA_POINTER(a6), a2
	adda.l records.Entry.Name(a3), a2
	moveq #0, d6
	move.w records.Entry.Length(a3), d6
	moveq #0, d5
memberDot
	cmp.w d6, d5
	bhs.w found
	cmpi.b #'.', 0(a2, d5.l)
	beq.w memberAvailable
	addq.w #1, d5
	bra.w memberDot
memberAvailable
	moveq #1, d0
	bsr.w resolveStructMember
	bne.w found
	move.w d2, records.Entry.MemberBase(a3)
found
	move.w records.Entry.Target(a3), 1(a5)
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	addq.l #2, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; referenceNamespace

; A5=unqualified name token,A6=scope state,D7=active module,D0=namespace.
; Templates retain selected original names; per-item aliases expose values.
; D4=selected
; target index+1 (zero when not imported), D0/CCR=status. Other registers kept.
selectedTarget	.block
	movem.l d1-d3/d5-d7/a0-a4, -(sp)
	move.w d0, -(sp)
	lea layout.IMPORT_STATE(a6), a4
	move.l d7, d0
	subq.w #1, d0
	andi.l #$ffff, d0
	add.l d0, d0
	movea.l HEADS_POINTER(a4), a0
	moveq #0, d7
	move.w 0(a0, d0.l), d7
	beq.w ok
	movea.l a5, a0
	bsr.w tokenName
	bne.w bad
	movea.l a0, a2
	move.l d0, d5
scan
	tst.w d7
	beq.w ok
	move.l d7, d0
	subq.w #1, d0
	mulu.w #ITEM_BYTES, d0
	lea ITEMS(a4), a3
	adda.l d0, a3
	tst.w Item.Unqualified(a3)
	beq.w next
	cmpi.w #2, Item.Unqualified(a3)
	bne.w namedSelection
	cmpi.w #$ffff, d4
	beq.w next
	tst.w d4
	bne.w next
	move.w #$ffff, d4
	bra.w next
namedSelection
	moveq #0, d6
	move.w Item.Selected(a3), d6
selected
	tst.w d6
	beq.w next
	move.l d6, d0
	subq.w #1, d0
	mulu.w #SELECTION_BYTES, d0
	lea SELECTIONS(a4), a1
	adda.l d0, a1
	moveq #0, d2
	move.w Selection.Next(a1), d2
	moveq #0, d3
	move.w Selection.Name(a1), d3
	tst.w (sp)
	bne.w originalLeaf
	moveq #0, d0
	move.w Selection.Alias(a1), d0
	beq.w originalLeaf
	move.l d0, d1
	bra.w compareLeaf
originalLeaf
	move.l d3, d1
compareLeaf
	move.l d1, d0
	subq.w #1, d0
	bsr.w entryLeaf
	cmp.w d5, d0
	bne.w nextSelected
	bsr.w prefixEqual
	bne.w nextSelected
	tst.w d4
	beq.w takeSelected
	cmpi.w #$ffff, d4
	bne.w bad  ; two direct imports claim the same name
takeSelected
	move.w d3, d4
nextSelected
	move.l d2, d6
	bra.w selected
next
	moveq #0, d7
	move.w Item.Next(a3), d7
	bra.w scan
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	addq.l #2, sp
	movem.l (sp)+, d1-d3/d5-d7/a0-a4
	tst.l d0
	rts
	.bend  ; selectedTarget

; A0=scope state,A1=binder callback. Validate forward modules and resolve every
; proxy after all imports are known. D0/CCR=status; other registers preserved.
	.pub
finish	.block
	movem.l d1-d7/a0-a6, -(sp)
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_STATE
	movea.l a0, a6
	movea.l a1, a5
	lea layout.IMPORT_STATE(a6), a4
	tst.w layout.MODULE_STATE+modules.State.Selection(a6)
	bne.w selectedModules
	moveq #0, d7
modulesLoop
	cmp.w COUNT(a4), d7
	bhs.w proxies
	move.l d7, d0
	mulu.w #ITEM_BYTES, d0
	lea ITEMS(a4), a0
	adda.l d0, a0
	moveq #0, d0
	move.w Item.Target(a0), d0
	andi.l #$ffff, d0
	add.l d0, d0
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a6), a0
	adda.l d0, a0
	btst #3, 1(a0)
	suba.l d0, a0
	beq.w bad
	move.l d7, d0
	mulu.w #ITEM_BYTES, d0
	lea ITEMS(a4), a3
	adda.l d0, a3
	bsr.w validateSelected
	bne.w bad
	addq.w #1, d7
	bra.w modulesLoop
selectedModules
	moveq #0, d6
selectedModule
	cmp.w layout.State.Count(a6), d6
	bhs.w proxies
	move.l d6, d0
	andi.l #$ffff, d0
	add.l d0, d0
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a6), a0
	adda.l d0, a0
	btst #4, 1(a0)
	suba.l d0, a0
	beq.w nextSelectedModule
	movea.l HEADS_POINTER(a4), a0
	moveq #0, d7
	move.w 0(a0, d0.l), d7
selectedItem
	tst.w d7
	beq.w nextSelectedModule
	move.l d7, d0
	subq.w #1, d0
	mulu.w #ITEM_BYTES, d0
	lea ITEMS(a4), a3
	adda.l d0, a3
	moveq #0, d0
	move.w Item.Target(a3), d0
	andi.l #$ffff, d0
	add.l d0, d0
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a6), a0
	adda.l d0, a0
	btst #3, 1(a0)
	suba.l d0, a0
	beq.w bad
	bsr.w validateSelected
	bne.w bad
	moveq #0, d7
	move.w Item.Next(a3), d7
	bra.w selectedItem
nextSelectedModule
	addq.w #1, d6
	bra.w selectedModule
proxies
	moveq #0, d7
loop
	cmp.w layout.State.Count(a6), d7
	bhs.w ok
	move.l d7, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	btst #3, records.Entry.Flags+1(a3)
	beq.w next
	btst #records.TEMPLATE_PROXY_BIT, records.Entry.Flags+1(a3)
	bne.w next  ; invocation proxies were checked when their bodies expanded
	tst.w layout.MODULE_STATE+modules.State.Selection(a6)
	beq.w resolveProxy
	moveq #0, d0
	move.w records.Entry.Owner(a3), d0
	beq.w next
	subq.w #1, d0
	andi.l #$ffff, d0
	add.l d0, d0
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a6), a0
	adda.l d0, a0
	btst #4, 1(a0)
	suba.l d0, a0
	beq.w next
resolveProxy
	bsr.w resolve
	bne.w bad
	move.l d7, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	move.w d1, records.Entry.Target(a3)
	ori.w #1, records.Entry.Flags(a3)  ; validated declaration target, not a new value
	move.w #1, layout.State.Changed(a6)
next
	addq.w #1, d7
	bra.w loop
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	.TELEMETRY_SERVICE_LEAVE
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; finish

; A0=mutable four-byte call-name token,A1=scope state,A2=scope binder.
; Resolve one template call through the same selected, wildcard and qualified
; import proxies as ordinary references. Only declared template targets are
; returned; cross-module targets require a matching .use and public visibility.
; D0/CCR=status,D1=canonical target ID; other registers preserved.
resolveTemplate	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a3
	movea.l a1, a6
	movea.l a2, a5
	jsr referenceTemplate
	bne.w templateBad
	moveq #0, d1
	move.w 1(a3), d1
	move.l d1, d0
	sub.w layout.State.Base(a6), d0
	bcs.w templateBad
	andi.l #$ffff, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w templateBad
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	btst #3, records.Entry.Flags+1(a3)
	beq.w templateTarget
	lea layout.IMPORT_STATE(a6), a4
	jsr resolve
	bne.w templateBad
templateTarget
	move.l d1, d0
	sub.w layout.State.Base(a6), d0
	bcs.w templateBad
	andi.l #$ffff, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w templateBad
	move.l d0, d6
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	btst #0, records.Entry.TemplateFlags+1(a3)
	beq.w templateBad
	moveq #0, d7
	move.w records.Entry.TemplateModule(a3), d7
	beq.w templateOk  ; unscoped global definitions remain visible
	cmp.w layout.MODULE_STATE+modules.State.Active(a6), d7
	beq.w templateOk
	movea.l a3, a1
	lea layout.MODULE_STATE(a6), a0
	jsr modules.checkTemplate
	bne.w templateBad
	moveq #0, d0
	move.w layout.MODULE_STATE+modules.State.Active(a6), d0
	beq.w templateBad
	subq.w #1, d0
	andi.l #$ffff, d0
	add.l d0, d0
	lea layout.IMPORT_STATE(a6), a4
	movea.l HEADS_POINTER(a4), a0
	moveq #0, d6
	move.w 0(a0, d0.l), d6
templateImport
	tst.w d6
	beq.w templateBad
	move.l d6, d0
	subq.w #1, d0
	mulu.w #ITEM_BYTES, d0
	lea ITEMS(a4), a0
	adda.l d0, a0
	moveq #0, d6
	move.w Item.Next(a0), d6
	moveq #0, d0
	move.w Item.Target(a0), d0
	addq.w #1, d0
	cmp.w d7, d0
	bne.w templateImport
	moveq #0, d5
	move.w Item.Selected(a0), d5
	tst.w d5
	beq.w templateOk  ; unselected or wildcard import
	move.l d1, d2
	sub.w layout.State.Base(a6), d2
	andi.l #$ffff, d2
	addq.w #1, d2
templateSelection
	tst.w d5
	beq.w templateImport
	move.l d5, d0
	subq.w #1, d0
	mulu.w #SELECTION_BYTES, d0
	lea SELECTIONS(a4), a1
	adda.l d0, a1
	cmp.w Selection.Name(a1), d2
	beq.w templateOk
	moveq #0, d5
	move.w Selection.Next(a1), d5
	bra.w templateSelection
templateOk
	moveq #0, d0
	bra.w templateDone
templateBad
	moveq #1, d0
templateDone
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; resolveTemplate

; A0=scope state,D0=call ID. Read-only named-selection hint, with no proxy
; allocation. D0/CCR=status,D1=definition ID; other registers preserved.
; Wildcard imports have no unique target and must use ordinary leaf candidates.
templateAliasTarget	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	moveq #0, d7
	move.w layout.MODULE_STATE+modules.State.Active(a6), d7
	beq.w missing
	subq.l #4, sp
	clr.b (sp)
	move.w d0, 1(sp)
	clr.b 3(sp)
	movea.l sp, a5
	moveq #0, d4
	moveq #records.NAMESPACE_TEMPLATE, d0
	bsr.w selectedTarget
	addq.l #4, sp
	bne.w missing
	tst.w d4
	beq.w missing
	cmpi.w #$ffff, d4
	beq.w missing
	moveq #0, d1
	move.w d4, d1
	subq.w #1, d1
	cmp.w layout.State.Count(a6), d1
	bhs.w missing
	add.w layout.State.Base(a6), d1
	moveq #0, d0
	bra.w done
missing
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; templateAliasTarget

; A0=scope state,D0=call ID,D1=definition ID. Reuse selectedTarget's
; read-only alias lookup before template call resolution may allocate a proxy.
; D0/CCR=zero when a selected rename exposes this definition.
templateAliasCandidate	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.w d1, d6
	sub.w layout.State.Base(a6), d6
	bcs.w aliasMissing
	andi.l #$ffff, d6
	cmp.w layout.State.Count(a6), d6
	bhs.w aliasMissing
	addq.w #1, d6
	moveq #0, d7
	move.w layout.MODULE_STATE+modules.State.Active(a6), d7
	beq.w aliasMissing
	subq.l #4, sp
	clr.b (sp)
	move.w d0, 1(sp)
	clr.b 3(sp)
	movea.l sp, a5
	moveq #0, d4
	moveq #records.NAMESPACE_TEMPLATE, d0
	bsr.w selectedTarget
	addq.l #4, sp
	bne.w aliasMissing
	cmp.w d6, d4
	bne.w aliasMissing
	moveq #0, d0
	bra.w aliasDone
aliasMissing
	moveq #1, d0
aliasDone
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; templateAliasCandidate
	.priv

; A3=import item,A6=scope state. Selected names must be declared and public
; even when no reached code references them. D0/CCR=status; others preserved.
validateSelected	.block
	movem.l d1-d2/a0-a2, -(sp)
	moveq #0, d2
	move.w Item.Selected(a3), d2
selected
	tst.w d2
	beq.w ok
	lea layout.IMPORT_STATE(a6), a2
	move.l d2, d0
	subq.w #1, d0
	cmp.w SELECTED_COUNT(a2), d0
	bhs.w bad
	mulu.w #SELECTION_BYTES, d0
	lea SELECTIONS(a2), a1
	adda.l d0, a1
	move.w Selection.Next(a1), d2
	moveq #0, d0
	move.w Selection.Name(a1), d0
	subq.w #1, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	move.l d0, d1
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a0
	adda.l d0, a0
	btst #0, records.Entry.Flags+1(a0)
	beq.w selectedTemplate
	andi.l #$ffff, d1
	add.l d1, d1
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a6), a1
	adda.l d1, a1
	btst #0, 1(a1)
	suba.l d1, a1
	beq.w bad
	bra.w selected
selectedTemplate
	btst #0, records.Entry.TemplateFlags+1(a0)
	beq.w bad
	btst #1, records.Entry.TemplateFlags+1(a0)
	beq.w bad
	bra.w selected
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a2
	tst.l d0
	rts
	.bend  ; validateSelected

; A3=proxy,A4=import state,A5=binder,A6=scope state. D1=canonical numeric ID,
; D0/CCR=status; other registers preserved. Alias precedence precedes exact names.
resolve	.block
	movem.l d2-d7/a0-a4, -(sp)
	move.w records.Entry.Flags(a3), -(sp)  ; retain namespace across moving bindings
	tst.w records.Entry.Leaf(a3)
	beq.w resolveQualified
	cmpi.w #$ffff, records.Entry.Leaf(a3)
	beq.w resolveWildcard
	bra.w resolveSelected
resolveQualified
	moveq #0, d0
	move.l records.Entry.Name(a3), d0
	movea.l layout.ARENA_POINTER(a6), a2
	adda.l d0, a2
	moveq #0, d6
	move.w records.Entry.Length(a3), d6
	moveq #0, d5
prefix
	cmp.w d6, d5
	bhs.w bad
	adda.l d5, a2
	cmpi.b #'.', 0(a2)
	suba.l d5, a2
	beq.w imports
	addq.w #1, d5
	bra.w prefix
imports
	moveq #0, d0
	move.w records.Entry.Owner(a3), d0
	subq.w #1, d0
	andi.l #$ffff, d0
	add.l d0, d0
	movea.l HEADS_POINTER(a4), a0
	moveq #0, d7
	move.w 0(a0, d0.l), d7
	move.l d7, d4
aliasLoop
	tst.w d7
	beq.w fullPaths
	move.l d7, d0
	subq.w #1, d0
	mulu.w #ITEM_BYTES, d0
	lea ITEMS(a4), a1
	adda.l d0, a1
	moveq #0, d0
	move.w Item.Qualifier(a1), d0
	move.l a1, -(sp)
	bsr.w entryLeaf
	movea.l (sp)+, a1
	cmp.w d5, d0
	bne.w aliasNext
	bsr.w prefixEqual
	bne.w aliasNext
	moveq #0, d0
	move.w Item.Target(a1), d0
	bsr.w entryName
	move.l d6, d1
	sub.w d5, d1
	add.w d0, d1
	cmpi.w #layout.NAME_BYTES-1, d1
	bhi.w bad
	lea layout.BUFFER(a6), a1
	move.l d0, d2
copyModule
	move.b (a0)+, (a1)+
	subq.w #1, d2
	bne.w copyModule
	movea.l a2, a0
	adda.l d5, a0
	move.l d6, d2
	sub.w d5, d2
copySuffix
	move.b (a0)+, (a1)+
	subq.w #1, d2
	bne.w copySuffix
	lea layout.BUFFER(a6), a0
	move.l d1, d0
	bra.w bind
aliasNext
	moveq #0, d7
	move.w Item.Next(a1), d7
	bra.w aliasLoop
fullPaths
	move.l d4, d7
	moveq #0, d3
fullLoop
	tst.w d7
	beq.w exact
	move.l d7, d0
	subq.w #1, d0
	mulu.w #ITEM_BYTES, d0
	lea ITEMS(a4), a1
	adda.l d0, a1
	moveq #0, d0
	move.w Item.Target(a1), d0
	move.l a1, -(sp)
	bsr.w entryName
	movea.l (sp)+, a1
	cmp.w d6, d0
	bhs.w fullNext
	adda.l d0, a2
	cmpi.b #'.', 0(a2)
	suba.l d0, a2
	bne.w fullNext
	bsr.w prefixEqual
	bne.w fullNext
	addq.w #1, d3
	cmpi.w #1, d3
	bhi.w bad
fullNext
	moveq #0, d7
	move.w Item.Next(a1), d7
	bra.w fullLoop
exact
	move.w (sp), d0
	btst #records.TEMPLATE_PROXY_BIT, d0
	beq.w numericExact
	movea.l a2, a0
	move.l d6, d0
	bra.w bind
numericExact
	moveq #0, d0
	bsr.w resolveStructMember
	bra.w done
bind
	bsr.w globalBind
	bne.w bad
	moveq #0, d0
	move.w d1, d0
	sub.w layout.State.Base(a6), d0
	andi.l #$ffff, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a0
	adda.l d0, a0
	move.w (sp), d0
	bsr.w targetDeclared
	bne.w bad
	moveq #0, d0
	bra.w done
resolveWildcard
	bsr.w wildcardTarget
	bra.w done
resolveSelected
	moveq #0, d0
	move.w records.Entry.ScopeKind(a3), d0
	subq.w #1, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	move.l d0, d2
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a0
	adda.l d0, a0
	move.w (sp), d0
	bsr.w targetDeclared
	beq.w selectedBound
	moveq #0, d0
	move.w records.Entry.Leaf(a3), d0
	subq.w #1, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	move.l d0, d2
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a0
	adda.l d0, a0
	move.w (sp), d0
	bsr.w targetDeclared
	bne.w bad
selectedBound
	move.w (sp), d0
	btst #records.TEMPLATE_PROXY_BIT, d0
	beq.w selectedValue
	move.l d2, d1
	add.w layout.State.Base(a6), d1
	bra.w selectedReady
selectedValue
	move.w records.Entry.Target(a0), d1
selectedReady
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	addq.l #2, sp
	movem.l (sp)+, d2-d7/a0-a4
	tst.l d0
	rts
	.bend  ; resolve

; A0=canonical entry,D0=proxy flags. Test declaration in the selected
; namespace without consulting or rewriting numeric Target. Other regs kept.
targetDeclared	.block
	btst #records.TEMPLATE_PROXY_BIT, d0
	beq.w value
	btst #0, records.Entry.TemplateFlags+1(a0)
	bra.w checked
value
	btst #0, records.Entry.Flags+1(a0)
checked
	beq.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; targetDeclared

; A0=canonical entry,D0=proxy flags,D2=canonical source index,A6=scope.
; Test namespace-specific export visibility. D0/CCR=status; others kept.
targetPublic	.block
	movem.l d1/a1, -(sp)
	btst #records.TEMPLATE_PROXY_BIT, d0
	beq.w value
	btst #1, records.Entry.TemplateFlags+1(a0)
	bra.w checked
value
	move.l d2, d1
	add.l d1, d1
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a6), a1
	adda.l d1, a1
	btst #0, 1(a1)
checked
	beq.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a1
	tst.l d0
	rts
	.bend  ; targetPublic

; A0=first suffix token,A4=end,A5=binder,A6=scope,D7=target module index.
; Capture scalar parameters by numeric ID. On success A4 ends before
; the with clause, so the existing selection/alias parser sees its own suffix.
; Other registers preserved; D0/CCR=status.
parameters	.block
	movem.l d1-d7/a0-a3/a5, -(sp)
	movea.l a0, a3
	moveq #0, d4
scanParameterSuffix
	cmpa.l a4, a3
	beq.w scanFinished
	bhi.w parametersBad
	cmpi.b #14, (a3)
	bne.w scanClose
	addq.w #1, d4
	bra.w scanAdvance
scanClose
	cmpi.b #15, (a3)
	bne.w scanWith
	tst.w d4
	beq.w parametersBad
	subq.w #1, d4
	bra.w scanAdvance
scanWith
	tst.w d4
	bne.w scanAdvance
	cmpi.b #1, (a3)
	bhi.w scanAdvance
	movea.l a3, a0
	bsr.w tokenName
	bne.w parametersBad
	cmpi.l #4, d0
	bne.w scanAdvance
	move.l (a0), d0
	ori.l #$20202020, d0
	cmpi.l #$77697468, d0  ; with
	bne.w scanAdvance
	move.l a3, d4  ; retain the suffix end before consuming the clause
	addq.l #4, a3
	cmpa.l a4, a3
	bhs.w parametersBad
	cmpi.b #14, (a3)+
	bne.w parametersBad
parameterItem
	cmpa.l a4, a3
	bhs.w parametersBad
	move.l a3, d6  ; name token address survives the binder
	cmpi.b #1, (a3)
	bhi.w parametersBad
	tst.b 3(a3)
	bne.w parametersBad
	movea.l a3, a0
	bsr.w tokenName
	bne.w parametersBad
	movea.l a0, a2
	move.l d0, d3
	move.l d7, d0
	bsr.w entryName
	add.l d0, d3
	addq.l #1, d3
	cmpi.l #layout.NAME_BYTES-1, d3
	bhi.w parametersBad
	lea layout.BUFFER(a6), a3
	move.l d0, d1
copyParameterModule
	move.b (a0)+, (a3)+
	subq.l #1, d1
	bne.w copyParameterModule
	move.b #'.', (a3)+
	move.l d3, d1
	sub.l d0, d1
	subq.l #1, d1
copyParameterLeaf
	move.b (a2)+, (a3)+
	subq.l #1, d1
	bne.w copyParameterLeaf
	lea layout.BUFFER(a6), a0
	move.l d3, d0
	bsr.w globalBind
	bne.w parametersBad
	move.l d1, d5  ; canonical ID
	sub.w layout.State.Base(a6), d1
	bcs.w parametersBad
	andi.l #$ffff, d1
	cmp.w layout.State.Count(a6), d1
	bhs.w parametersBad
	mulu.w #records.ENTRY_BYTES, d1
	movea.l layout.ENTRIES_POINTER(a6), a0
	adda.l d1, a0
	moveq #0, d3
	btst #0, records.Entry.Flags+1(a0)
	beq.w newParameter
	moveq #1, d3
	bra.w parameterValue
newParameter
	ori.w #1, records.Entry.Flags(a0)
	move.w d7, d0
	addq.w #1, d0
	move.w d0, records.Entry.Owner(a0)
	move.l d5, d0
	sub.w layout.State.Base(a6), d0
	andi.l #$ffff, d0
	add.l d0, d0
	movea.l layout.MODULE_STATE+modules.OWNERS_POINTER(a6), a0
	move.w d7, 0(a0, d0.l)
	adda.l d0, a0
	addq.w #1, 0(a0)
	suba.l d0, a0
	; The module-private flag is zero, independent of importer visibility.
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a6), a0
	adda.l d0, a0
	andi.w #$fffe, 0(a0)
	suba.l d0, a0
parameterValue
	movea.l d6, a3
	addq.l #4, a3  ; parameter name
	cmpa.l a4, a3
	bhs.w parametersBad
	cmpi.b #34, (a3)+
	bne.w parametersBad
	move.l a3, -(sp)
	moveq #0, d2
parameterExpression
	cmpa.l a4, a3
	bhs.w expressionBad
	cmpi.b #14, (a3)
	bne.w expressionClose
	addq.w #1, d2
	bra.w expressionAdvance
expressionClose
	cmpi.b #15, (a3)
	bne.w expressionComma
	tst.w d2
	beq.w expressionReady
	subq.w #1, d2
	bra.w expressionAdvance
expressionComma
	cmpi.b #4, (a3)
	bne.w expressionAdvance
	tst.w d2
	beq.w expressionReady
expressionAdvance
	movea.l a3, a0
	bsr.w nextParameterToken
	bne.w expressionBad
	movea.l a0, a3
	bra.w parameterExpression
expressionReady
	movea.l (sp)+, a0
	movea.l a3, a1
	bsr.w evaluateRange
	bne.w parametersBad
	move.l d1, d6
	bra.w expressionStored
expressionBad
	addq.l #4, sp
	bra.w parametersBad
expressionStored
	bsr.w storeParameter
	bne.w parametersBad
	cmpa.l a4, a3
	bhs.w parametersBad
	cmpi.b #4, (a3)
	bne.w parameterClose
	addq.l #1, a3
	bra.w parameterItem
parameterClose
	cmpi.b #15, (a3)+
	bne.w parametersBad
	cmpa.l a4, a3
	bne.w parametersBad
	movea.l d4, a4
	bra.w parametersOk
scanAdvance
	movea.l a3, a0
	bsr.w nextParameterToken
	bne.w parametersBad
	movea.l a0, a3
	bra.w scanParameterSuffix
scanFinished
	tst.w d4
	bne.w parametersBad
parametersOk
	moveq #0, d0
	bra.w parametersDone
parametersBad
	moveq #1, d0
parametersDone
	movem.l (sp)+, d1-d7/a0-a3/a5
	tst.l d0
	rts
	.bend  ; parameters

; A6=scope,D5=canonical ID,D6=low,D2=high,D3=already declared.
; Store or compare one scalar parameter and publish its known value. The same
; conflict rule serves parsed imports and configuration transfer. D0/CCR=status;
; other registers preserved.
storeParameter	.block
	movem.l d1/a0-a1, -(sp)
	lea layout.IMPORT_STATE(a6), a0
	moveq #0, d0
	move.w PARAM_COUNT(a0), d0
	tst.w d3
	beq.w appendParameter
	lea PARAMS(a0), a1
	move.l d0, d1
existingParameter
	tst.l d1
	beq.w bad
	cmp.w pkg.Parameter.Id(a1), d5
	bne.w nextParameter
	cmp.l pkg.Parameter.Low(a1), d6
	bne.w bad
	cmp.l pkg.Parameter.High(a1), d2
	bne.w bad
	bra.w parameterStored
nextParameter
	adda.w #PARAM_BYTES, a1
	subq.l #1, d1
	bra.w existingParameter
appendParameter
	cmpi.w #LIST_LIMIT, d0
	bhs.w bad
	mulu.w #PARAM_BYTES, d0
	lea PARAMS(a0), a1
	adda.l d0, a1
	move.w d5, pkg.Parameter.Id(a1)
	clr.w pkg.Parameter.Reserved(a1)
	move.l d6, pkg.Parameter.Low(a1)
	move.l d2, pkg.Parameter.High(a1)
	addq.w #1, PARAM_COUNT(a0)
parameterStored
	move.l d5, d0
	sub.w layout.State.Base(a6), d0
	bcs.w bad
	andi.l #$ffff, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	move.l d0, d1
	lsl.l #3, d1
	movea.l KNOWN_VALUES_POINTER(a0), a1
	move.l d6, runtime.Value.Low(a1, d1.l)
	move.l d2, runtime.Value.High(a1, d1.l)
	movea.l KNOWN_DEFINED_POINTER(a0), a1
	move.b #1, 0(a1, d0.l)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; storeParameter

	.pub
; A0=destination scope,D0=canonical parameter ID,D1=module source index+1,
; D2=low,D3=high. Seed a private scalar parameter before dependency body replay.
; Identities must already be bound in this scope. D0/CCR=status; other registers
; preserved. A conflict leaves this caller-owned preparation state releasable.
seedParameter	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l d0, d5
	move.l d2, d6
	move.l d3, d2
	move.l d1, d7
	beq.w bad
	cmp.w layout.State.Count(a6), d7
	bhi.w bad
	move.l d5, d0
	sub.w layout.State.Base(a6), d0
	bcs.w bad
	andi.l #$ffff, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	move.l d0, d4
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a1
	adda.l d0, a1
	moveq #0, d3
	btst #0, records.Entry.Flags+1(a1)
	beq.w new
	moveq #1, d3
	cmp.w records.Entry.Owner(a1), d7
	bne.w bad
	bra.w store
new
	ori.w #1, records.Entry.Flags(a1)
	move.w d7, records.Entry.Owner(a1)
	add.l d4, d4
	movea.l layout.MODULE_STATE+modules.OWNERS_POINTER(a6), a1
	move.w d7, 0(a1, d4.l)
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a6), a1
	andi.w #$fffe, 0(a1, d4.l)
store
	bsr.w storeParameter
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; seedParameter

	.pub
; A0..A1=complete numeric token range,A2=scope state. Resolve a known
; module-scope value from inside a block before evaluating a first-pass
; conditional. Only numeric
; IDs are changed, and the control record is discarded after this call.
; D1=known i64 low/D2=high on success, D0/CCR=status; other registers preserved.
evaluateScoped	.block
	movem.l d3-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a4
	movea.l a2, a6
	move.l a0, -(sp)
scanScoped
	cmpa.l a4, a5
	beq.w scopedReady
	bhi.w scopedBad
	cmpi.b #1, (a5)
	bhi.w scopedNext
	tst.b 3(a5)
	bne.w scopedNext
	moveq #0, d0
	move.w 1(a5), d0
	sub.w layout.State.Base(a6), d0
	bcs.w scopedNext
	andi.l #$ffff, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w scopedNext
	lea layout.IMPORT_STATE(a6), a0
	movea.l KNOWN_DEFINED_POINTER(a0), a0
	adda.l d0, a0
	tst.b 0(a0)
	suba.l d0, a0
	bne.w scopedNext
	move.l d0, d1
	mulu.w #records.ENTRY_BYTES, d1
	movea.l layout.ENTRIES_POINTER(a6), a3
	adda.l d1, a3
	btst #0, records.Entry.Flags+1(a3)
	bne.w scopedNext  ; a local declaration shadows the module value
	bsr.w entryLeafBytes
	move.l d0, d6
	movea.l a0, a2
	moveq #0, d7
	move.w layout.MODULE_STATE+modules.State.Active(a6), d7
	tst.w d7
	beq.w scopedNext
	moveq #0, d4
candidate
	cmp.w layout.State.Count(a6), d4
	bhs.w scopedNext
	lea layout.IMPORT_STATE(a6), a0
	movea.l KNOWN_DEFINED_POINTER(a0), a0
	adda.l d4, a0
	tst.b 0(a0)
	suba.l d4, a0
	beq.w candidateNext
	move.l d4, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a3
	adda.l d0, a3
	cmp.w records.Entry.Owner(a3), d7
	bne.w candidateNext
	bsr.w entryLeafBytes
	cmp.w d6, d0
	bne.w candidateNext
	movea.l a0, a1
	movea.l a2, a0
	move.l d6, d3
compareLeaf
	moveq #0, d1
	move.b (a0)+, d1
	bsr.w fold
	move.b d1, d2
	move.b (a1)+, d1
	bsr.w fold
	cmp.b d2, d1
	bne.w candidateNext
	subq.w #1, d3
	bne.w compareLeaf
	move.w records.Entry.Target(a3), 1(a5)
	bra.w scopedNext
candidateNext
	addq.w #1, d4
	bra.w candidate
scopedNext
	movea.l a5, a0
	bsr.w nextParameterToken
	bne.w scopedBad
	movea.l a0, a5
	bra.w scanScoped
scopedReady
	movea.l (sp)+, a0
	movea.l a4, a1
	bsr.w evaluateRange
	bra.w scopedDone
scopedBad
	addq.l #4, sp
	moveq #1, d0
scopedDone
	movem.l (sp)+, d3-d7/a0-a6
	tst.l d0
	rts
	.bend  ; evaluateScoped
	.priv

; A3=entry,A6=scope state. Return A0=last dotted component, D0=its bytes.
; Explicit qualified bindings have Leaf=0, so scan the stored name itself.
entryLeafBytes	.block
	move.l a2, -(sp)
	movea.l layout.ARENA_POINTER(a6), a0
	moveq #0, d0
	move.l records.Entry.Name(a3), d0
	adda.l d0, a0
	moveq #0, d0
	move.w records.Entry.Length(a3), d0
	movea.l a0, a1
	adda.l d0, a1
	movea.l a1, a2
leafBack
	cmpa.l a0, a1
	beq.w leafReady
	cmpi.b #'.', -(a1)
	bne.w leafBack
	lea 1(a1), a0
leafReady
	move.l a2, d0
	sub.l a0, d0
	movea.l (sp)+, a2
	rts
	.bend  ; entryLeafBytes

; A0..A1=complete numeric token range,A6=scope state. D1=known i64 low/D2=high on
; success, D0/CCR=status. The biased VM pointers are used only after every
; symbol ID has been checked against the bounded local-name arrays.
; Resolve already-declared import proxies in an owned token copy: neither source
; tokens nor proxy values are cached, so later mutable updates remain visible.
	.pub
evaluateRange	.block
	movem.l d3-d7/a0-a6, -(sp)
	subq.l #4, sp
	clr.l (sp)
	move.l a1, d0
	sub.l a0, d0
	beq.w rangeBad
	bcs.w rangeBad
	move.l d0, d1
	addq.l #1, d0
	andi.l #$fffffffe, d0
	suba.l d0, sp
	move.l d0, (sp)
	lea 4(sp), a2
	move.l d1, d0
	movea.l a2, a1
copyExpressionToken
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copyExpressionToken
	movea.l a2, a5
	movea.l a1, a4
	lea layout.IMPORT_STATE(a6), a3
validateExpressionToken
	cmpa.l a4, a5
	beq.w compileExpression
	bhi.w rangeBad
	moveq #0, d0
	move.b (a5), d0
	cmpi.b #6, d0
	beq.w rangeBad  ; current address is unavailable at an import site
	cmpi.b #1, d0
	bhi.w nextExpressionToken
	move.l a4, d0
	sub.l a5, d0
	cmpi.l #4, d0
	blo.w rangeBad
	cmpi.b #1, 3(a5)
	bhi.w rangeBad
	moveq #0, d0
	move.w 1(a5), d0
	sub.w layout.State.Base(a6), d0
	bcs.w rangeBad
	andi.l #$ffff, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w rangeBad
	move.l d0, d3
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a0
	adda.l d0, a0
	btst #3, records.Entry.Flags+1(a0)
	bne.w registeredExpressionSymbol
	; Struct extents precede scopes.line's normal reference walk. Use the same
	; import registration on our copy, including lexical origin and selections.
	movea.l a5, a0
	movea.l a6, a1
	bsr.w reference
	bne.w rangeBad
	moveq #0, d0
	move.w 1(a5), d0
	sub.w layout.State.Base(a6), d0
	bcs.w rangeBad
	andi.l #$ffff, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w rangeBad
	move.l d0, d3
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a0
	adda.l d0, a0
registeredExpressionSymbol
	ori.w #2, records.Entry.Flags(a0)  ; same REFERENCED bit as scopes.line
	clr.b 3(a5)
	move.l d3, d0
	lea layout.MODULE_STATE(a6), a0
	jsr modules.reference
	move.l d3, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a0
	adda.l d0, a0
	btst #3, records.Entry.Flags+1(a0)
	beq.w knownExpressionSymbol
	movem.l a2-a5, -(sp)
	movea.l a0, a3
	lea layout.IMPORT_STATE(a6), a4
	lea findDeclared, a5  ; lookup only: forward declarations cannot be allocated
	bsr.w resolve
	movem.l (sp)+, a2-a5
	bne.w rangeBad
	moveq #0, d0
	move.w d1, d0
	sub.w layout.State.Base(a6), d0
	bcs.w rangeBad
	andi.l #$ffff, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w rangeBad
	move.l d0, d4
	move.l d0, d1
	move.l d3, d0
	lea layout.MODULE_STATE(a6), a0
	jsr modules.check
	bne.w rangeBad
	move.l d4, d3
	move.l d3, d0
	add.w layout.State.Base(a6), d0
	move.w d0, 1(a5)
knownExpressionSymbol
	move.l d3, d0
	movea.l KNOWN_DEFINED_POINTER(a3), a0
	adda.l d0, a0
	tst.b 0(a0)
	suba.l d0, a0
	beq.w rangeBad
nextExpressionToken
	movea.l a5, a0
	bsr.w nextParameterToken
	bne.w rangeBad
	movea.l a0, a5
	bra.w validateExpressionToken
compileExpression
	movea.l a4, a1
	movea.l a2, a0
	lea EXPRESSION_SCRATCH(a3), a5
	movea.l a5, a3
	lea 256(a5), a4
	jsr expression.compile
	bne.w rangeBad
	cmpa.l a1, a0
	bne.w rangeBad
	suba.w #expression.FRAME_BYTES, sp
	movea.l sp, a2
	lea layout.IMPORT_STATE(a6), a4
	movea.l KNOWN_VALUES_POINTER(a4), a0
	moveq #0, d0
	move.w layout.State.Base(a6), d0
	move.l d0, d3
	lsl.l #3, d3
	suba.l d3, a0
	move.l a0, expression.Frame.Values(a2)
	movea.l KNOWN_DEFINED_POINTER(a4), a0
	suba.l d0, a0
	move.l a0, expression.Frame.Defined(a2)
	moveq #0, d1
	move.w layout.State.Count(a6), d1
	add.l d1, d0
	move.l d0, expression.Frame.Count(a2)
	clr.l expression.Frame.Pc(a2)
	movea.l a5, a0
	movea.l a3, a1
	jsr expression.evaluate
	tst.l d0
	bne.w evaluatedBad
	tst.l d2
	bne.w evaluatedBad
	cmpa.l a1, a0
	bne.w evaluatedBad
	move.l expression.Frame.High(a2), d2
	adda.w #expression.FRAME_BYTES, sp
	moveq #0, d0
	bra.w rangeDone
evaluatedBad
	adda.w #expression.FRAME_BYTES, sp
rangeBad
	moveq #1, d0
rangeDone
	move.l (sp), d3
	adda.l d3, sp
	addq.l #4, sp
	movem.l (sp)+, d3-d7/a0-a6
	tst.l d0
	rts
	.bend  ; evaluateRange
	.priv

; A0=token,A4=end. Advance one packed source token without inspecting its
; expression meaning. D0/CCR=status; D1 scratch.
nextParameterToken	.block
	cmpa.l a4, a0
	bhs.w badToken
	moveq #0, d1
	move.b (a0), d1
	cmpi.w #1, d1
	bls.w nameToken
	cmpi.w #2, d1
	beq.w numberToken
	cmpi.w #4, d1
	blo.w badToken
	cmpi.w #39, d1
	bhi.w badToken
	addq.l #1, a0
	bra.w tokenBounds
nameToken
	addq.l #4, a0
	bra.w tokenBounds
numberToken
	addq.l #5, a0
tokenBounds
	cmpa.l a4, a0
	bhi.w badToken
	moveq #0, d0
	rts
badToken
	moveq #1, d0
	rts
	.bend  ; nextParameterToken

; A3=wildcard proxy,A4=import state,A5=binder,A6=scope state.
; Find one public declaration among wildcard imports of this module.
; The import itself never makes a named block live; remapped references do.
wildcardTarget	.block
	movem.l d2-d7/a0-a3, -(sp)
	move.w records.Entry.Flags(a3), -(sp)
	suba.l #layout.NAME_BYTES, sp  ; retain leaf across bindings that relocate names
	moveq #0, d0
	move.w records.Entry.ScopeKind(a3), d0
	subq.w #1, d0
	bsr.w entryLeaf
	move.l d0, d6
	cmpi.l #layout.NAME_BYTES-1, d6
	bhi.w wildcardBad
	movea.l sp, a1
	tst.l d0
	beq.w wildcardBad
copyStableLeaf
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copyStableLeaf
	movea.l sp, a2
	moveq #0, d0
	move.w records.Entry.Owner(a3), d0
	subq.w #1, d0
	andi.l #$ffff, d0
	add.l d0, d0
	movea.l HEADS_POINTER(a4), a0
	moveq #0, d7
	move.w 0(a0, d0.l), d7
	moveq #0, d5
wildcardItem
	tst.w d7
	beq.w wildcardDone
	move.l d7, d0
	subq.w #1, d0
	mulu.w #ITEM_BYTES, d0
	lea ITEMS(a4), a3
	adda.l d0, a3
	moveq #0, d7
	move.w Item.Next(a3), d7
	cmpi.w #2, Item.Unqualified(a3)
	bne.w wildcardItem
	moveq #0, d0
	move.w Item.Target(a3), d0
	bsr.w entryName
	move.l d0, d4
	add.w d6, d4
	addq.w #1, d4
	cmpi.w #layout.NAME_BYTES-1, d4
	bhi.w wildcardBad
	lea layout.BUFFER(a6), a1
copyWildcardModule
	move.b (a0)+, (a1)+
	subq.w #1, d0
	bne.w copyWildcardModule
	move.b #'.', (a1)+
	movea.l a2, a0
	move.l d6, d0
copyWildcardLeaf
	move.b (a0)+, (a1)+
	subq.w #1, d0
	bne.w copyWildcardLeaf
	lea layout.BUFFER(a6), a0
	move.l d4, d0
	bsr.w globalBind
	bne.w wildcardBad
	move.l d1, d2
	sub.w layout.State.Base(a6), d2
	bcs.w wildcardItem
	andi.l #$ffff, d2
	cmp.w layout.State.Count(a6), d2
	bhs.w wildcardBad
	move.l d2, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a0
	adda.l d0, a0
	move.w layout.NAME_BYTES(sp), d0
	bsr.w targetDeclared
	bne.w wildcardItem
	move.w layout.NAME_BYTES(sp), d0
	bsr.w targetPublic
	bne.w wildcardItem
	tst.w d5
	beq.w wildcardMatch
	cmp.w d1, d5
	beq.w wildcardItem
	bra.w wildcardBad
wildcardMatch
	move.w d1, d5
	bra.w wildcardItem
wildcardDone
	tst.w d5
	beq.w wildcardBad
	move.w d5, d1
	moveq #0, d0
	bra.w wildcardExit
wildcardBad
	moveq #1, d0
wildcardExit
	adda.l #layout.NAME_BYTES, sp
	addq.l #2, sp
	movem.l (sp)+, d2-d7/a0-a3
	tst.l d0
	rts
	.bend  ; wildcardTarget

; A0/D0=bytes to compare with A2 prefix. D0/CCR=status, others preserved.
prefixEqual	.block
	movem.l d1-d3/a0-a1, -(sp)
	movea.l a2, a1
	move.l d0, d3
loop
	moveq #0, d1
	move.b (a0)+, d1
	bsr.w fold
	move.w d1, d2
	move.b (a1)+, d1
	bsr.w fold
	cmp.b d1, d2
	bne.w bad
	subq.w #1, d3
	bne.w loop
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a1
	tst.l d0
	rts
	.bend  ; prefixEqual

; A0=raw source-name token. Return A0/D0 lexical bytes (drop scope prefix for
; unqualified spellings). D0 is length on success, CCR Z explicitly signals success.
; D1/A1 scratch; A6=scope state.
tokenName	.block
	cmpi.b #1, (a0)
	bhi.w bad
	moveq #0, d1
	move.b 3(a0), d1
	cmpi.w #1, d1
	bhi.w bad
	move.l d1, -(sp)
	moveq #0, d0
	move.w 1(a0), d0
	sub.w layout.State.Base(a6), d0
	bcs.w popBad
	andi.l #$ffff, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w popBad
	tst.l (sp)+
	beq.w leaf
	bsr.w entryName
	bra.w ok
leaf
	bsr.w entryLeaf
ok
	cmp.l d0, d0
	rts
popBad
	addq.l #4, sp
bad
	moveq #1, d0
	rts
	.bend  ; tokenName

; D0=entry index, A6=scope state. A0/D0=full bytes; D1/A1 scratch.
entryName	.block
	andi.l #$ffff, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a1
	adda.l d0, a1
	movea.l layout.ARENA_POINTER(a6), a0
	moveq #0, d0
	move.l records.Entry.Name(a1), d0
	adda.l d0, a0
	moveq #0, d0
	move.w records.Entry.Length(a1), d0
	rts
	.bend  ; entryName

; Same as entryName, but derive the final component from the stored full name.
; Entry.Leaf describes first-intern context, which may have been a qualified
; declaration before this bare alias token. D1/A1 scratch.
entryLeaf	.block
	bsr.w entryName
	movea.l a0, a1
	move.l d0, d1
scan
	cmpi.b #'.', (a1)+
	bne.w next
	movea.l a1, a0
	move.l d1, d0
	subq.w #1, d0
next
	subq.w #1, d1
	bne.w scan
	rts
	.bend  ; entryLeaf

; A2/D6=raw dotted spelling,D5=first dot,A3=origin proxy,A6=scope.
; Exact names retain priority. Otherwise a bare struct base is looked up from
; the captured lexical scope, checking its definition-time struct identity.
; Dotted bases remain absolute/imported, matching Rust member-base resolution.
; D0=1 when recording availability, 0 at completion. D0=status,D1=target,
; D2=struct identity+1 (zero for exact symbols); other registers preserved.
; No identities are allocated.
resolveStructMember	.block
	movem.l d3-d7/a0-a5, -(sp)
	move.l d0, d2
	movea.l a2, a4
	moveq #0, d7
	move.w records.Entry.Padding(a3), d7
	movea.l a4, a0
	move.l d6, d0
	bsr.w findDeclared
	beq.w exactFound
	tst.l d2
	bne.w available
	tst.w records.Entry.MemberBase(a3)
	beq.w bad
available
	move.l d5, d0
	addq.l #1, d0
singleField
	cmp.l d6, d0
	bhs.w ancestor
	cmpi.b #'.', 0(a4, d0.l)
	beq.w bad
	addq.l #1, d0
	bra.w singleField
ancestor
	tst.w d7
	beq.w globalBase
	move.l d7, d0
	subq.w #1, d0
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a6), a0
	adda.l d0, a0
	moveq #0, d7
	move.w records.Entry.Owner(a0), d7
	moveq #0, d4
	move.w records.Entry.Length(a0), d4
	move.l records.Entry.Name(a0), d0
	movea.l layout.ARENA_POINTER(a6), a0
	adda.l d0, a0
	lea layout.BUFFER(a6), a1
	move.l d4, d0
copyPrefix
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copyPrefix
	move.b #'.', (a1)+
	addq.l #1, d4
	bra.w base
globalBase
	moveq #0, d4
	lea layout.BUFFER(a6), a1
base
	move.l d4, d0
	add.l d6, d0
	cmpi.l #layout.NAME_BYTES-1, d0
	bhi.w bad
	movea.l a4, a0
	move.l d5, d0
copyBase
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copyBase
	add.l d5, d4
	lea layout.BUFFER(a6), a0
	move.l d4, d0
	bsr.w findDeclared
	beq.w member
	cmp.l d5, d4
	beq.w bad  ; the global bare base was the final candidate
	bra.w ancestor
member
	btst #5, records.Entry.Flags+1(a0)
	beq.w bad  ; a nearer nonstruct declaration shadows outer struct types
	moveq #0, d0
	move.w records.Entry.Target(a0), d0
	sub.w layout.State.Base(a6), d0
	addq.w #1, d0
	tst.l d2
	bne.w captured
	cmp.w records.Entry.MemberBase(a3), d0
	bne.w bad  ; a struct declared after the reference cannot replace its base
captured
	move.l d0, d2
	lea layout.BUFFER(a6), a1
	adda.l d4, a1
	movea.l a4, a0
	adda.l d5, a0
	move.l d6, d0
	sub.l d5, d0
copyField
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copyField
	add.l d6, d4
	sub.l d5, d4
	lea layout.BUFFER(a6), a0
	move.l d4, d0
	bsr.w findDeclared
	bra.w done  ; a missing member must not fall back to an outer struct
exactFound
	moveq #0, d2
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d3-d7/a0-a5
	tst.l d0
	rts
	.bend  ; resolveStructMember

; A0/D0=name bytes,A6=scope. D0=status,D1=target,A0=declared entry.
; Preserve remaining registers; use the scope binder's folded hash chains.
; Proxies have separate import chains and are never inserted in these buckets.
findDeclared	.block
	movem.l d2-d5/a1-a3, -(sp)
	movea.l a0, a2
	move.l d0, d5
	moveq #0, d4
hash
	moveq #0, d1
	move.b (a0)+, d1
	bsr.w fold
	move.l d4, d2
	lsl.l #5, d4
	add.l d2, d4
	add.l d1, d4
	subq.l #1, d0
	bne.w hash
	andi.l #255, d4
	add.w d4, d4
	move.l d4, d0
	lea layout.BUCKETS(a6), a0
	moveq #0, d4
	move.w 0(a0, d0.w), d4
candidate
	tst.w d4
	beq.w bad
	subq.w #1, d4
	mulu.w #records.ENTRY_BYTES, d4
	movea.l layout.ENTRIES_POINTER(a6), a3
	adda.l d4, a3
	btst #0, records.Entry.Flags+1(a3)
	beq.w next
	cmp.w records.Entry.Length(a3), d5
	bne.w next
	movea.l layout.ARENA_POINTER(a6), a0
	adda.l records.Entry.Name(a3), a0
	movea.l a2, a1
	move.l d5, d3
compare
	moveq #0, d1
	move.b (a0)+, d1
	bsr.w fold
	move.l d1, d2
	move.b (a1)+, d1
	bsr.w fold
	cmp.b d2, d1
	bne.w next
	subq.l #1, d3
	bne.w compare
	movea.l a3, a0
	moveq #0, d1
	move.w records.Entry.Target(a3), d1
	moveq #0, d0
	bra.w done
next
	moveq #0, d4
	move.w records.Entry.Next(a3), d4
	bra.w candidate
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d5/a1-a3
	tst.l d0
	rts
	.bend  ; findDeclared

; A0/D0=qualified name,A5=binder,A6=scope state. D0/status,D1/ID,D2scratch;
; A0/A1 scratch. No process pointer is stored in preparation metadata.
globalBind	.block
	move.w layout.State.Current(a6), -(sp)
	clr.w layout.State.Current(a6)
	movea.l a6, a1
	jsr (a5)
	move.w (sp)+, layout.State.Current(a6)
	tst.l d0
	rts
	.bend  ; globalBind

; D1=ASCII byte; other registers preserved.
fold	.block
	cmpi.b #'A', d1
	blo.w done
	cmpi.b #'Z', d1
	bhi.w done
	addi.b #32, d1
done
	rts
	.bend  ; fold
	.endsection
	.endmodule

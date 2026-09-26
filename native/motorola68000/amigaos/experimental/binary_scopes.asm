; Preparation-only lexical scopes. Packed records retain numeric identities only.
; @opforge-owner: experimental.amigaos.binary_scopes
	.module experimental.amigaos.binary_scopes
	.cpu 68020
	.include "telemetry_macros.i"
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_modules as modules
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_imports as imports
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_section_prepare as sections
	.use experimental.amigaos.binary_structs as structs
	.pub
LIMIT = layout.LIMIT
ARENA_BYTES = layout.ARENA_BYTES
ENTRY_BYTES = records.ENTRY_BYTES
ENTRIES = layout.ENTRIES
BUCKETS = layout.BUCKETS
ARENA = layout.ARENA
BUFFER = layout.BUFFER
MODULE_STATE = layout.MODULE_STATE
IMPORT_STATE = layout.IMPORT_STATE
DECLARED = 1
REFERENCED = 2
EXPLICIT = 4
TEMPLATE = 16
KIND_BLOCK = 1
KIND_NAMESPACE = 2
KEY_BLOCK = 1
KEY_ENDBLOCK = 2
KEY_END = 3
KEY_NAMESPACE = 4
KEY_ENDNAMESPACE = 5
KEY_MODULE = 6
KEY_ENDMODULE = 7
KEY_PUB = 8
KEY_PRIV = 9
KEY_USE = 10
KEY_SECTION = 11
KEY_ENDSECTION = 12
KEY_REGION = 13
KEY_PLACE = 14
KEY_SEGMENT = 15
KEY_ENDSEGMENT = 16
KEY_MACRO = 17
KEY_ENDMACRO = 18
KEY_OUTPUT = 19
KEY_STRUCT = structs.KEY_STRUCT
KEY_ENDSTRUCT = structs.KEY_ENDSTRUCT
KEY_DB = structs.KEY_DB
KEY_DW = structs.KEY_DW
SECTION_STATE = IMPORT_STATE+imports.SCRATCH_BYTES
STRUCT_STATE = SECTION_STATE+sections.SCRATCH_BYTES
SCRATCH_BYTES = STRUCT_STATE+structs.SCRATCH_BYTES
	.section code, kind=code

; A0=caller-owned SCRATCH_BYTES, D0=first source ID, D1=.end ID. D0/CCR=status;
; other registers preserved. All stored names and links are offsets or indices.
begin	.block
	movem.l d1/a0-a1, -(sp)
	movea.l a0, a1
	cmpi.l #65536-LIMIT, d0
	bhi.w bad
	move.w d0, layout.State.Base(a0)
	move.w d1, layout.State.EndDirective(a0)
	clr.w layout.State.Changed(a0)
	clr.w layout.State.FileContent(a0)
	clr.w layout.State.Count(a0)
	clr.w layout.State.Current(a0)
	clr.w layout.State.Ended(a0)
	clr.w layout.State.ArenaUsed(a0)
	clr.w layout.State.FirstBound(a0)
	clr.w layout.State.FirstExplicit(a0)
	lea BUCKETS(a0), a0
	move.w #255, d1
clear
	clr.w (a0)+
	dbra d1, clear
	lea MODULE_STATE(a1), a0
	moveq #0, d0
	move.w layout.State.Base(a1), d0
	jsr modules.begin
	lea IMPORT_STATE(a1), a0
	jsr imports.begin
	movea.l a1, a0
	adda.l #SECTION_STATE, a0
	jsr sections.begin
	movea.l a1, a0
	adda.l #STRUCT_STATE, a0
	jsr structs.begin
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; begin

; A0=state. End one source without discarding shared definitions/imports.
; D0=nonzero requires explicit modules for nonempty files. D0/CCR=status;
; other registers preserved. Reject unfinished lexical/module scopes.
endFile	.block
	tst.l d0
	beq.w scopes
	tst.w layout.State.FileContent(a0)
	beq.w scopes
	tst.w MODULE_STATE+modules.State.Explicit(a0)
	beq.w bad
scopes
	move.l a1, -(sp)
	movea.l a0, a1
	adda.l #STRUCT_STATE, a1
	tst.w structs.State.Active(a1)
	movea.l (sp)+, a1
	bne.w bad
	tst.w layout.State.Current(a0)
	bne.w bad
	move.l a1, -(sp)
	movea.l a0, a1
	adda.l #SECTION_STATE, a1
	tst.w sections.State.Active(a1)
	movea.l (sp)+, a1
	bne.w bad
	clr.w layout.State.Ended(a0)
	clr.w layout.State.FileContent(a0)
	clr.w MODULE_STATE+modules.State.Explicit(a0)
	clr.w MODULE_STATE+modules.State.Outside(a0)
	clr.w MODULE_STATE+modules.State.Visibility(a0)
	clr.w MODULE_STATE+modules.State.FileDerived(a0)
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; endFile

; A0=scope state,A1=basename,D0=name bytes. Open its file-derived module
; through the normal numeric module binder. D1=module index+1 on success;
; D0/CCR=status; other registers preserved.
beginFileDerived	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	tst.w layout.State.Current(a6)
	bne.w fileDerivedBad
	movea.l a5, a0
	movea.l a6, a1
	jsr bind
	bne.w fileDerivedBad
	move.l d1, d0
	sub.w layout.State.Base(a6), d0
	lea ENTRIES(a6), a1
	lea ARENA(a6), a2
	movea.l a6, a3
	lea bind, a4
	lea MODULE_STATE(a6), a0
	jsr modules.open
	bne.w fileDerivedBad
	move.w modules.State.Active(a0), layout.State.Current(a6)
	move.w #1, modules.State.FileDerived(a0)
	moveq #0, d1
	move.w modules.State.Active(a0), d1
	moveq #0, d0
	bra.w fileDerivedDone
fileDerivedBad
	moveq #1, d0
fileDerivedDone
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; beginFileDerived

; A0=scope state. Close a file-derived module still active at physical EOF.
; D0/CCR=status; other registers preserved.
endFileDerived	.block
	movem.l d1/a0-a1, -(sp)
	movea.l a0, a1
	tst.w MODULE_STATE+modules.State.FileDerived(a1)
	beq.w fileDerivedClosed
	moveq #0, d0
	move.w layout.State.Current(a1), d0
	lea MODULE_STATE(a1), a0
	jsr modules.close
	bne.w fileDerivedEnd
	clr.w layout.State.Current(a1)
	clr.w MODULE_STATE+modules.State.FileDerived(a1)
fileDerivedClosed
	moveq #0, d0
fileDerivedEnd
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; endFileDerived

; A0=state. Returns D0=next unused source ID, other registers preserved.
count	.block
	move.l d1, -(sp)
	moveq #0, d0
	move.w layout.State.Base(a0), d0
	moveq #0, d1
	move.w layout.State.Count(a0), d1
	add.l d1, d0
	move.l (sp)+, d1
	rts
	.bend  ; count

; A0=state. Reset source-name metadata for the next writer record; no clobbers.
startLine	.block
	clr.w layout.State.FirstBound(a0)
	clr.w layout.State.FirstExplicit(a0)
	rts
	.bend  ; startLine

; Writer callback: A0/D0=lexical bytes, A1=state; D0/CCR=status,
; D1=numeric ID, D2=preparation-only explicit-name marker. Preserves D3-D7/A2-A6.
; Package lookup precedes this callback; only source names arrive here.
bind	.block
	movem.l d3-d7/a2-a6, -(sp)
	movea.l a1, a6
	moveq #0, d5
	moveq #0, d3
	move.w layout.State.Current(a6), d3
	bsr.w compose
	bne.w bad
	tst.w layout.State.FirstBound(a6)
	bne.w find
	move.w #1, layout.State.FirstBound(a6)
	move.w d5, layout.State.FirstExplicit(a6)
find
	bsr.w lookup
	beq.w found
	cmpi.w #LIMIT, layout.State.Count(a6)
	bhs.w bad
	moveq #0, d0
	move.w layout.State.ArenaUsed(a6), d0
	add.l d6, d0
	cmpi.l #ARENA_BYTES, d0
	bhi.w bad
	moveq #0, d1
	move.w layout.State.Count(a6), d1
	move.l d1, d2
	lsl.l #4, d2
	lea ENTRIES(a6), a3
	adda.l d2, a3
	move.w layout.State.ArenaUsed(a6), records.Entry.Name(a3)
	move.w d6, records.Entry.Length(a3)
	move.w d3, records.Entry.Owner(a3)
	move.w d7, records.Entry.Leaf(a3)
	clr.w records.Entry.Flags(a3)
	clr.w records.Entry.ScopeKind(a3)
	move.w layout.State.Base(a6), d2
	add.w d1, d2
	move.w d2, records.Entry.Target(a3)
	lea BUCKETS(a6), a4
	add.w d4, d4
	move.w 0(a4, d4.w), records.Entry.Next(a3)
	addq.w #1, d1
	move.w d1, 0(a4, d4.w)
	addq.w #1, layout.State.Count(a6)
	lea ARENA(a6), a1
	moveq #0, d1
	move.w layout.State.ArenaUsed(a6), d1
	adda.l d1, a1
	move.w d0, layout.State.ArenaUsed(a6)
	movea.l a2, a0
	move.l d6, d0
copy
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copy
found
	tst.w d5
	beq.w bound
	ori.w #EXPLICIT, records.Entry.Flags(a3)
bound
	moveq #0, d1
	move.w records.Entry.Target(a3), d1
	move.l d5, d2
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d3-d7/a2-a6
	tst.l d0
	rts
	.bend  ; bind

; A0=writer record, A1=state, D0=record buffer capacity. Normalize labels,
; mark declarations/references and consume scope
; directives before expression preparation. Named blocks lower to entry labels;
; close directives lower to empty records. D0/CCR=status, other registers kept.
line	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a1, a6
	movea.l a0, a5
	move.l d0, d4
	tst.w layout.State.Ended(a6)
	bne.w empty
	bsr.w normalizeLabel
	bne.w bad
	bsr.w authorizeLine
	bne.w bad
	movea.l a5, a0
	movea.l a6, a1
	movea.l a6, a2
	adda.l #STRUCT_STATE, a2
	lea keyword, a3
	move.l d4, d0
	jsr structs.line
	bne.w bad
	moveq #0, d6
	move.b (a5), d6
	addq.w #1, d6
	movea.l a5, a4
	adda.w d6, a4
	lea 4(a5), a0
	moveq #-1, d7
	cmpi.w #9, d6
	blo.w ok
	cmpi.b #1, (a0)
	bhi.w statement
	cmpi.b #34, 4(a0)
	beq.w declaration
	cmpi.b #5, 4(a0)
	beq.w declaration
	bra.w statement
declaration
	bsr.w declare
	bne.w bad
	moveq #0, d7
	move.w 1(a0), d7
	cmpi.b #34, 4(a0)
	beq.w assignment
	addq.l #5, a0
	bra.w statement
assignment
	addq.l #5, a0
	bra.w references
statement
	cmpa.l a4, a0
	beq.w ok
	cmpi.b #7, (a0)
	beq.w directive
	; The instruction name is package-owned, not a source-symbol reference.
	cmpi.b #1, (a0)
	bhi.w bad
	addq.l #4, a0
	bra.w references
directive
	move.l a4, d0
	sub.l a0, d0
	cmpi.l #5, d0
	blo.w bad
	cmpi.b #7, (a0)
	bne.w bad
	cmpi.b #1, 1(a0)
	bhi.w bad
	tst.b 4(a0)
	bne.w bad
	moveq #0, d0
	move.w 2(a0), d0
	bsr.w keyword
	cmpi.l #KEY_USE, d0
	beq.w importing
	cmpi.l #KEY_MODULE, d0
	beq.w module
	cmpi.l #KEY_ENDMODULE, d0
	beq.w endModule
	cmpi.l #KEY_PUB, d0
	beq.w public
	cmpi.l #KEY_PRIV, d0
	beq.w private
	cmpi.l #KEY_BLOCK, d0
	beq.w block
	cmpi.l #KEY_ENDBLOCK, d0
	beq.w endBlock
	cmpi.l #KEY_NAMESPACE, d0
	beq.w namespace
	cmpi.l #KEY_ENDNAMESPACE, d0
	beq.w endNamespace
	cmpi.l #KEY_END, d0
	beq.w end
	cmpi.l #KEY_SECTION, d0
	beq.w sectionControl
	cmpi.l #KEY_ENDSECTION, d0
	beq.w sectionControl
	cmpi.l #KEY_REGION, d0
	beq.w sectionControl
	cmpi.l #KEY_PLACE, d0
	beq.w sectionControl
	cmpi.l #KEY_OUTPUT, d0
	beq.w outputControl
	; Other directives retain their existing generic preparation/assembly route.
	addq.l #5, a0
	bra.w references
sectionControl
	movea.l a5, a0
	movea.l a6, a1
	movea.l a6, a2
	adda.l #SECTION_STATE, a2
	subi.l #KEY_SECTION-1, d0
	jsr sections.line
	bne.w bad
	bra.w ok
outputControl
	movea.l a5, a0
	movea.l a6, a1
	movea.l a6, a2
	adda.l #SECTION_STATE, a2
	jsr sections.output
	bne.w bad
	bra.w ok
importing
	tst.l d7
	bpl.w bad
	movea.l a6, a1
	lea bind, a2
	movea.l a6, a3
	adda.l #SECTION_STATE, a3
	jsr imports.line
	bne.w bad
	bra.w empty
module
	bsr.w openModule
	bne.w bad
	bra.w retainedLabel
endModule
	addq.l #5, a0
	cmpa.l a4, a0
	bne.w bad
	lea MODULE_STATE(a6), a0
	moveq #0, d0
	move.w layout.State.Current(a6), d0
	jsr modules.close
	bne.w bad
	clr.w layout.State.Current(a6)
	bra.w retainedLabel
public
	moveq #1, d1
	bra.w visibility
private
	moveq #0, d1
visibility
	addq.l #5, a0
	cmpa.l a4, a0
	bne.w bad
	lea MODULE_STATE(a6), a0
	move.w d1, modules.State.Visibility(a0)
retainedLabel
	tst.l d7
	bmi.w empty
	move.b #8, (a5)
	move.b #5, 8(a5)
	bra.w ok

block
	moveq #KIND_BLOCK, d2
	bsr.w openScope
	bne.w done
	ori.b #source.FLAG_BLOCK_OPEN, 1(a5)
	bra.w done
namespace
	moveq #KIND_NAMESPACE, d2
opening
	bsr.w openScope
	bra.w done
endBlock
	ori.b #source.FLAG_BLOCK_CLOSE, 1(a5)
	moveq #KIND_BLOCK, d2
	bra.w closing
endNamespace
	moveq #KIND_NAMESPACE, d2
closing
	bsr.w closeScope
	bne.w bad
	bra.w empty
end
	addq.l #5, a0
	cmpa.l a4, a0
	bne.w bad
	tst.w layout.State.Current(a6)
	beq.w endSyntax
	lea MODULE_STATE(a6), a0
	tst.w modules.State.FileDerived(a0)
	beq.w bad
	moveq #0, d0
	move.w layout.State.Current(a6), d0
	jsr modules.close
	bne.w bad
	clr.w layout.State.Current(a6)
	clr.w modules.State.FileDerived(a0)
endSyntax
	move.w #1, layout.State.Ended(a6)
	bra.w retainedLabel  ; EOF continues with the next explicit source
references
	cmpa.l a4, a0
	beq.w ok
	bhi.w bad
	moveq #0, d0
	move.b (a0), d0
	cmpi.b #1, d0
	bls.w reference
	cmpi.b #2, d0
	beq.w literal
	cmpi.b #3, d0
	beq.w string
	bra.w punctuation
literal
	addq.l #5, a0
	bra.w references
string
	move.l a4, d1
	sub.l a0, d1
	cmpi.l #2, d1
	blo.w bad
	moveq #0, d0
	move.b 1(a0), d0
	addq.l #2, a0
	subq.l #2, d1
	cmp.l d0, d1
	blo.w bad
	adda.l d0, a0
	bra.w references
punctuation
	addq.l #1, a0
	bra.w references
reference
	movea.l a6, a1
	jsr imports.reference
	bne.w bad
	moveq #0, d0
	move.w 1(a0), d0
	sub.w layout.State.Base(a6), d0
	bcs.w packageName
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	lsl.l #4, d0
	lea ENTRIES(a6), a3
	adda.l d0, a3
	btst #4, records.Entry.Flags+1(a3)
	bne.w bad  ; template names are callable, not numeric values
	ori.w #REFERENCED, records.Entry.Flags(a3)
	clr.b 3(a0)
	lsr.l #4, d0
	move.l a0, -(sp)
	lea MODULE_STATE(a6), a0
	jsr modules.reference
	movea.l (sp)+, a0
packageName
	addq.l #4, a0
	bra.w references
empty
	move.b #3, (a5)
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; line

; A0=packed records, D0=bytes, A1=state. Complete binding then patch IDs in
; records and compact expressions. D0/CCR=status; other registers preserved.
; No source string survives this preparation boundary.
finish	.block
	movem.l d1-d7/a0-a6, -(sp)
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_STATE
	movea.l a1, a6
	tst.w layout.State.Current(a6)
	bne.w bad
	movea.l a0, a5
	move.l d0, -(sp)
	movea.l a6, a0
	lea bind, a1
	jsr imports.finish
	bne.w failSaved
	movea.l a6, a0
	adda.l #SECTION_STATE, a0
	jsr sections.finish
	bne.w failSaved
	moveq #0, d7
resolve
	cmp.w layout.State.Count(a6), d7
	bhs.w rewrite
	move.l d7, d0
	lsl.l #4, d0
	lea ENTRIES(a6), a4
	adda.l d0, a4
	move.w records.Entry.Flags(a4), d0
	andi.w #DECLARED+REFERENCED, d0
	beq.w next
	tst.w MODULE_STATE+modules.State.Selection(a6)
	beq.w selectedReference
	move.l d7, d1
	add.w d1, d1
	lea MODULE_STATE+modules.FLAGS(a6), a0
	btst #1, 1(a0, d1.w)
	beq.w selectedReference  ; a declaration may serve a selected module
	btst #2, 1(a0, d1.w)
	bne.w selectedReference  ; mixed origins need normal validation
	lea MODULE_STATE+modules.ORIGINS(a6), a0
	moveq #0, d0
	move.w 0(a0, d1.w), d0
	beq.w selectedReference
	subq.w #1, d0
	add.w d0, d0
	lea MODULE_STATE+modules.FLAGS(a6), a0
	btst #4, 1(a0, d0.w)
	beq.w next
selectedReference
	btst #0, records.Entry.Flags+1(a4)
	bne.w access
	btst #2, records.Entry.Flags+1(a4)
	bne.w failSaved
	moveq #0, d3
	move.w records.Entry.Owner(a4), d3
parent
	tst.w d3
	beq.w failSaved
	move.l d3, d0
	subq.w #1, d0
	lsl.l #4, d0
	lea ENTRIES(a6), a3
	adda.l d0, a3
	moveq #0, d3
	move.w records.Entry.Owner(a3), d3
	lea ARENA(a6), a0
	moveq #0, d0
	move.w records.Entry.Name(a4), d0
	add.w records.Entry.Leaf(a4), d0
	adda.l d0, a0
	moveq #0, d0
	move.w records.Entry.Length(a4), d0
	sub.w records.Entry.Leaf(a4), d0
	movem.l d7/a4, -(sp)
	bsr.w compose
	bne.w lookupFailed
	bsr.w lookup
lookupFailed
	movem.l (sp)+, d7/a4
	tst.l d0
	bne.w parent
	btst #0, records.Entry.Flags+1(a3)
	beq.w parent
	move.w records.Entry.Target(a3), records.Entry.Target(a4)
	move.w #1, layout.State.Changed(a6)
access
	btst #1, records.Entry.Flags+1(a4)
	beq.w checkAccess
	moveq #0, d0
	move.w records.Entry.Target(a4), d0
	sub.w layout.State.Base(a6), d0
	bcs.w checkAccess
	cmp.w layout.State.Count(a6), d0
	bhs.w failSaved
	lsl.l #4, d0
	lea ENTRIES(a6), a3
	adda.l d0, a3
	btst #4, records.Entry.Flags+1(a3)
	bne.w failSaved
checkAccess
	move.l d7, d0
	moveq #0, d1
	move.w records.Entry.Target(a4), d1
	sub.w layout.State.Base(a6), d1
	lea MODULE_STATE(a6), a0
	jsr modules.check
	bne.w failSaved
next
	addq.w #1, d7
	bra.w resolve
rewrite
	move.l (sp)+, d0
	tst.w layout.State.Changed(a6)
	beq.w unchanged
	movea.l a5, a0
	lea ENTRIES(a6), a1
	moveq #0, d1
	move.w layout.State.Base(a6), d1
	moveq #0, d2
	move.w layout.State.Count(a6), d2
	jsr records.remap
	bra.w done
unchanged
	moveq #0, d0
	bra.w done
failSaved
	addq.l #4, sp
bad
	moveq #1, d0
done
	.TELEMETRY_SERVICE_LEAVE
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; finish
	.priv

; Classify the whole statement before declaring an optional label. Module
; boundary directives and .end may carry labels without becoming outside content.
; A0=normalized record,A6=state. D0/CCR=status; other registers preserved.
authorizeLine	.block
	movem.l d1-d2/a0-a1, -(sp)
	moveq #0, d1
	move.b (a0), d1
	addq.w #1, d1
	cmpi.w #4, d1
	beq.w ok
	move.w #1, layout.State.FileContent(a6)
	movea.l a0, a1
	adda.w d1, a1
	addq.l #4, a0
	cmpi.w #9, d1
	blo.w content
	cmpi.b #1, (a0)
	bhi.w directive
	cmpi.b #5, 4(a0)
	bne.w content
	addq.l #5, a0
directive
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #5, d0
	blo.w content
	cmpi.b #7, (a0)
	bne.w content
	tst.b 4(a0)
	bne.w content
	moveq #0, d0
	move.w 2(a0), d0
	bsr.w keyword
	cmpi.l #KEY_MODULE, d0
	beq.w ok
	cmpi.l #KEY_ENDMODULE, d0
	beq.w ok
	cmpi.l #KEY_END, d0
	beq.w ok
content
	lea MODULE_STATE(a6), a0
	jsr modules.content
	bra.w done
ok
	moveq #0, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; authorizeLine

; A0=directive,A4=end,A6=scope state. Validate the single dotted module name,
; then delegate identity, prefix construction and ownership to modules.
; D0/CCR=status; other registers preserved.
openModule	.block
	movem.l d1-d3/a0-a4, -(sp)
	tst.w layout.State.Current(a6)
	bne.w bad
	addq.l #5, a0
	move.l a4, d0
	sub.l a0, d0
	cmpi.l #4, d0
	bne.w bad
	cmpi.b #1, (a0)
	bhi.w bad
	moveq #0, d0
	move.w 1(a0), d0
	sub.w layout.State.Base(a6), d0
	bcs.w bad
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	lea ENTRIES(a6), a1
	lea ARENA(a6), a2
	movea.l a6, a3
	lea bind, a4
	lea MODULE_STATE(a6), a0
	jsr modules.open
	bne.w bad
	move.w modules.State.Active(a0), layout.State.Current(a6)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a4
	tst.l d0
	rts
	.bend  ; openModule

; Canonical column-one Identifier/Register prefixes are labels regardless of
; instruction spelling. A0=record,D0=capacity,A6=state. D0/CCR=status;
; D1-D3/A1-A2 scratch. Normalize in place only after proving one spare byte.
; Assignment syntax keeps its existing indentation-independent behavior.
normalizeLabel	.block
	moveq #0, d1
	move.b (a0), d1
	addq.w #1, d1
	cmpi.w #4, d1
	blo.w bad
	cmp.l d0, d1
	bhi.w bad
	cmpi.w #4, d1
	beq.w ok
	cmpi.b #1, 4(a0)
	bhi.w ok
	cmpi.w #8, d1
	blo.w bad
	cmpi.w #9, d1
	blo.w indentation
	cmpi.b #34, 8(a0)
	beq.w ok
	cmpi.b #5, 8(a0)
	bne.w indentation
	tst.b 1(a0)
	bne.w bad
	bra.w ok
indentation
	tst.b 1(a0)
	bne.w instruction
	moveq #0, d2
	move.w 5(a0), d2
	cmp.w layout.State.Base(a6), d2
	blo.w bad  ; package-reserved label names remain outside the native subset
	cmpi.w #256, d1
	bhs.w bad
	cmp.l d0, d1
	bhs.w bad
	movea.l a0, a1
	adda.l d1, a1
	movea.l a1, a2
	addq.l #1, a2
	move.l d1, d2
	subq.l #8, d2
	beq.w colon
shift
	move.b -(a1), -(a2)
	subq.l #1, d2
	bne.w shift
colon
	move.b #5, 8(a0)
	addq.b #1, (a0)
	bra.w ok
instruction
	; An indented identifier followed by a dot is not an entry label for a
	; scope directive. Leave ordinary operand parsing to existing preparation.
	cmpi.w #9, d1
	blo.w ok
	cmpi.b #7, 8(a0)
	beq.w bad
ok
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; normalizeLabel

; Shared scope-open boundary. A0=directive,A4=end,A5=record,A6=state,
; D7=optional parent-scope label ID (-1 absent), D2=kind. D0/CCR=status;
; D1/D3/A0/A3 scratch. Namespace identity is independent of DECLARED: an
; operand opener creates no value, and can reopen a path or share a symbol name.
openScope	.block
	addq.l #5, a0
	move.l d7, d3
	cmpa.l a4, a0
	beq.w labelScope
	cmpi.w #KIND_NAMESPACE, d2
	bne.w bad
	move.l a4, d0
	sub.l a0, d0
	cmpi.l #4, d0
	bne.w bad
	cmpi.b #1, (a0)
	bhi.w bad
	tst.b 3(a0)
	bne.w bad  ; dotted namespace declarations remain outside this increment
	moveq #0, d3
	move.w 1(a0), d3
	bra.w label
labelScope
	tst.l d3
	bmi.w bad  ; anonymous scopes are unsupported
label
	tst.l d7
	bmi.w noLabel
	tst.w layout.State.FirstExplicit(a6)
	bne.w bad
	lea 4(a5), a0
	cmpi.b #5, 4(a0)
	beq.w labelReady
	bsr.w declare
	bne.w bad
labelReady
	move.b #8, (a5)
	move.b #5, 8(a5)
	bra.w enter
noLabel
	move.b #3, (a5)
enter
	move.l d3, d0
	sub.w layout.State.Base(a6), d0
	bcs.w bad
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	move.l d0, d1
	lsl.l #4, d1
	lea ENTRIES(a6), a3
	adda.l d1, a3
	; Only opening a path owns its parent metadata. A later qualified value
	; declaration must not change an existing namespace's lexical parent.
	move.w layout.State.Current(a6), records.Entry.Owner(a3)
	lea MODULE_STATE(a6), a0
	move.w modules.State.Visibility(a0), d1
	lsl.w #8, d1
	or.w d2, d1
	move.w d1, records.Entry.ScopeKind(a3)
	addq.w #1, d0
	move.w d0, layout.State.Current(a6)
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; openScope

; Same line/state inputs as openScope, D2=expected kind. D0/CCR=status;
; A0/A3 scratch. Kind lives in the otherwise unused entry word; no extra table.
closeScope	.block
	tst.l d7
	bpl.w bad
	addq.l #5, a0
	cmpa.l a4, a0
	bne.w bad
	moveq #0, d0
	move.w layout.State.Current(a6), d0
	beq.w bad
	subq.w #1, d0
	lsl.l #4, d0
	lea ENTRIES(a6), a3
	adda.l d0, a3
	move.w records.Entry.ScopeKind(a3), d0
	andi.w #$ff, d0
	cmp.w d0, d2
	bne.w bad
	move.w records.Entry.ScopeKind(a3), d0
	lsr.w #8, d0
	move.l a0, -(sp)
	lea MODULE_STATE(a6), a0
	move.w d0, modules.State.Visibility(a0)
	movea.l (sp)+, a0
	move.w records.Entry.Owner(a3), layout.State.Current(a6)
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; closeScope

; A0=name token, A6=state. D0/status, D1/A3 scratch; other registers kept.
declare	.block
	cmpi.b #1, 3(a0)
	bhi.w bad
	moveq #0, d0
	move.w 1(a0), d0
	sub.w layout.State.Base(a6), d0
	bcs.w bad
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	lsl.l #4, d0
	lea ENTRIES(a6), a3
	adda.l d0, a3
	btst #0, records.Entry.Flags+1(a3)
	bne.w bad
	clr.b 3(a0)
	ori.w #DECLARED, records.Entry.Flags(a3)
	lsr.l #4, d0
	move.l a0, -(sp)
	lea MODULE_STATE(a6), a0
	jsr modules.claim
	movea.l (sp)+, a0
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; declare

; A0/D0=name, D3=scope index+1. Returns folded lookup input A2/D6, leaf
; offset D7 and D5=explicitly qualified. Clobbers D0-D2/A0-A1/A3; keeps D3/D4.
compose	.block
	movea.l a0, a2
	move.l d0, d6
	moveq #0, d7
	moveq #0, d5
	tst.l d0
	beq.w bad
	cmpi.l #layout.NAME_BYTES-1, d0
	bhi.w bad
scan
	cmpi.b #'.', (a0)+
	beq.w qualified
	subq.l #1, d0
	bne.w scan
	tst.w d3
	beq.w ready
	move.l d3, d0
	subq.w #1, d0
	lsl.l #4, d0
	lea ENTRIES(a6), a3
	adda.l d0, a3
	moveq #0, d7
	move.w records.Entry.Length(a3), d7
	addq.w #1, d7
	move.l d6, d0
	add.l d7, d0
	cmpi.l #layout.NAME_BYTES-1, d0
	bhi.w bad
	lea ARENA(a6), a0
	moveq #0, d1
	move.w records.Entry.Name(a3), d1
	adda.l d1, a0
	lea BUFFER(a6), a1
	move.l d7, d1
	subq.w #1, d1
prefix
	move.b (a0)+, (a1)+
	subq.w #1, d1
	bne.w prefix
	move.b #'.', (a1)+
	movea.l a2, a0
	move.l d6, d1
leaf
	move.b (a0)+, (a1)+
	subq.w #1, d1
	bne.w leaf
	lea BUFFER(a6), a2
	add.l d7, d6
	bra.w ready
qualified
	moveq #1, d5
ready
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; compose

; A2/D6=lookup bytes. D0/CCR=found status, A3=entry; D4=bucket (even on miss).
; Clobbers D0-D2/A0-A1/A3; preserves other registers.
lookup	.block
	movea.l a2, a0
	move.l d6, d0
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
	move.l d4, d0
	add.w d0, d0
	lea BUCKETS(a6), a0
	moveq #0, d2
	move.w 0(a0, d0.w), d2
chain
	tst.w d2
	beq.w missing
	subq.w #1, d2
	lsl.l #4, d2
	lea ENTRIES(a6), a3
	adda.l d2, a3
	cmp.w records.Entry.Length(a3), d6
	bne.w next
	lea ARENA(a6), a1
	moveq #0, d0
	move.w records.Entry.Name(a3), d0
	adda.l d0, a1
	movea.l a2, a0
	move.l d6, d0
compare
	moveq #0, d1
	move.b (a0)+, d1
	bsr.w fold
	move.w d1, d2
	move.b (a1)+, d1
	bsr.w fold
	cmp.b d1, d2
	bne.w next
	subq.l #1, d0
	bne.w compare
	moveq #0, d0
	rts
next
	moveq #0, d2
	move.w records.Entry.Next(a3), d2
	bra.w chain
missing
	moveq #1, d0
	rts
	.bend  ; lookup

; D1=ASCII byte, fold uppercase letters only. Other registers preserved.
fold	.block
	cmpi.b #'A', d1
	blo.w done
	cmpi.b #'Z', d1
	bhi.w done
	addi.b #32, d1
done
	rts
	.bend  ; fold

; D0=numeric directive ID. Returns D0=KEY_* code, zero for other directives.
; A6=state; preserves other registers. Package .end is supplied by caller.
keyword	.block
	movem.l d1-d4/a0-a3, -(sp)
	cmp.w layout.State.EndDirective(a6), d0
	beq.w end
	sub.w layout.State.Base(a6), d0
	bcs.w none
	cmp.w layout.State.Count(a6), d0
	bhs.w none
	lsl.l #4, d0
	lea ENTRIES(a6), a3
	adda.l d0, a3
	lea ARENA(a6), a0
	moveq #0, d0
	move.w records.Entry.Name(a3), d0
	adda.l d0, a0
	moveq #0, d4
	move.w records.Entry.Length(a3), d4
	movea.l a0, a1
	move.l d4, d0
leafScan
	cmpi.b #'.', (a1)+
	bne.w leafNext
	movea.l a1, a0
	move.l d0, d4
	subq.w #1, d4
leafNext
	subq.w #1, d0
	bne.w leafScan
	lea Words, a2
word
	moveq #0, d3
	move.b (a2)+, d3
	beq.w none
	moveq #0, d2
	move.b (a2)+, d2
	cmp.w d4, d2
	bne.w skip
	movea.l a0, a1
	move.l d2, d0
character
	moveq #0, d1
	move.b (a1)+, d1
	bsr.w fold
	cmp.b (a2)+, d1
	bne.w mismatch
	subq.w #1, d0
	bne.w character
	move.l d3, d0
	bra.w done
mismatch
	subq.w #1, d0
	adda.w d0, a2
	bra.w next
skip
	adda.w d2, a2
next
	bra.w word
end
	moveq #KEY_END, d0
	bra.w done
none
	moveq #0, d0
done
	movem.l (sp)+, d1-d4/a0-a3
	rts
	.bend  ; keyword

	.pub
; A0=scope state,D0=numeric directive ID. D0=KEY_* or zero; other
; registers preserved. Classification reads only the bound numeric name.
classifyDirective	.block
	move.l a6, -(sp)
	movea.l a0, a6
	bsr.w keyword
	movea.l (sp)+, a6
	tst.l d0
	rts
	.bend  ; classifyDirective
; A0=scope state,D0=definition-time source ID,D1=original qualifier.
; D0/CCR=status,D1=call-scope ID. Qualified names retain their identity;
; unqualified source names bind from their lexical leaf in the active scope.
; Other registers preserved. Package IDs are returned unchanged.
rebindLocal	.block
	movem.l d2-d5/a0-a3/a6, -(sp)
	move.l a1, -(sp)
	movea.l a0, a6
	tst.w d1
	bne.w unchanged
	move.l d0, d5
	sub.w layout.State.Base(a6), d5
	bcs.w unchanged
	cmp.w layout.State.Count(a6), d5
	bhs.w badRebind
	lsl.l #4, d5
	lea ENTRIES(a6), a3
	adda.l d5, a3
	moveq #0, d2
	move.w records.Entry.Leaf(a3), d2
	moveq #0, d0
	move.w records.Entry.Length(a3), d0
	sub.w d2, d0
	beq.w badRebind
	lea ARENA(a6), a0
	moveq #0, d3
	move.w records.Entry.Name(a3), d3
	add.l d2, d3
	adda.l d3, a0
	movea.l a6, a1
	bsr.w bind
	bra.w rebindDone
unchanged
	move.l d0, d1
	moveq #0, d0
	bra.w rebindDone
badRebind
	moveq #1, d0
rebindDone
	movea.l (sp)+, a1
	movem.l (sp)+, d2-d5/a0-a3/a6
	tst.l d0
	rts
	.bend  ; rebindLocal

; A0=scope state,D0=call ID,D1=definition ID. A read-only candidate check
; prevents ordinary dot directives from allocating import proxies. Selected
; per-item aliases may have a different leaf than their definition.
; D0/CCR=zero on a matching leaf or selected alias; other registers preserved.
templateCandidate	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.w d0, d4
	move.w d1, d5
	bsr.w templateLeafEqual
	beq.w candidateFound
	moveq #0, d0
	move.w d4, d0
	moveq #0, d1
	move.w d5, d1
	movea.l a6, a0
	jsr imports.templateAliasCandidate
	beq.w candidateFound
	bra.w candidateMissing
candidateFound
	moveq #0, d0
	bra.w candidateDone
candidateMissing
	moveq #1, d0
candidateDone
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; templateCandidate

; A0=scope state,D0/D1=numeric name IDs. Compare folded leaf bytes.
; D0/CCR=zero on equal leaves; other registers preserved.
templateLeafEqual	.block
	movem.l d1-d5/a0-a3/a6, -(sp)
	movea.l a0, a6
	sub.w layout.State.Base(a6), d0
	bcs.w leafMissing
	cmp.w layout.State.Count(a6), d0
	bhs.w leafMissing
	sub.w layout.State.Base(a6), d1
	bcs.w leafMissing
	cmp.w layout.State.Count(a6), d1
	bhs.w leafMissing
	lsl.l #4, d0
	lsl.l #4, d1
	lea ENTRIES(a6), a2
	adda.l d0, a2
	lea ENTRIES(a6), a3
	adda.l d1, a3
	moveq #0, d2
	move.w records.Entry.Length(a2), d2
	moveq #0, d3
	move.w records.Entry.Length(a3), d3
	lea ARENA(a6), a0
	moveq #0, d0
	move.w records.Entry.Name(a2), d0
	adda.l d0, a0
	lea ARENA(a6), a1
	moveq #0, d0
	move.w records.Entry.Name(a3), d0
	adda.l d0, a1
	; Entry.Leaf is the first binding context, not necessarily the final
	; component of an explicitly qualified import alias.
	moveq #0, d4
	moveq #0, d5
scanCallLeaf
	cmp.l d2, d4
	bhs.w callLeafReady
	cmpi.b #'.', 0(a0, d4.w)
	bne.w nextCallLeaf
	move.l d4, d5
	addq.l #1, d5
nextCallLeaf
	addq.l #1, d4
	bra.w scanCallLeaf
callLeafReady
	adda.l d5, a0
	sub.l d5, d2
	moveq #0, d4
	moveq #0, d5
scanDefinitionLeaf
	cmp.l d3, d4
	bhs.w definitionLeafReady
	cmpi.b #'.', 0(a1, d4.w)
	bne.w nextDefinitionLeaf
	move.l d4, d5
	addq.l #1, d5
nextDefinitionLeaf
	addq.l #1, d4
	bra.w scanDefinitionLeaf
definitionLeafReady
	adda.l d5, a1
	sub.l d5, d3
	cmp.l d3, d2
	bne.w leafMissing
	tst.l d2
	beq.w leafMissing
leafCompare
	moveq #0, d1
	move.b (a0)+, d1
	bsr.w fold
	move.w d1, d4
	moveq #0, d1
	move.b (a1)+, d1
	bsr.w fold
	cmp.b d1, d4
	bne.w leafMissing
	subq.w #1, d2
	bne.w leafCompare
	moveq #0, d0
	bra.w leafDone
leafMissing
	moveq #1, d0
leafDone
	movem.l (sp)+, d1-d5/a0-a3/a6
	tst.l d0
	rts
	.bend  ; templateLeafEqual

; A0=scope state,D0=call name ID,D1=definition name ID.
; D0/CCR=zero when visible, D1=ancestor distance (nearest is zero).
; Only an unqualified call may search lexical ancestors; exact IDs also match.
; Other registers preserved.
templateDistance	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	cmp.w d1, d0
	beq.w exact
	move.w d0, d6
	sub.w layout.State.Base(a6), d6
	bcs.w missing
	cmp.w layout.State.Count(a6), d6
	bhs.w missing
	move.w d1, d7
	sub.w layout.State.Base(a6), d7
	bcs.w missing
	cmp.w layout.State.Count(a6), d7
	bhs.w missing
	lsl.l #4, d6
	lsl.l #4, d7
	lea ENTRIES(a6), a4
	adda.l d6, a4
	lea ENTRIES(a6), a5
	adda.l d7, a5
	tst.w records.Entry.Leaf(a4)
	beq.w missing  ; an explicitly qualified call needs an exact ID
	moveq #0, d2
	move.w records.Entry.Length(a4), d2
	sub.w records.Entry.Leaf(a4), d2
	moveq #0, d3
	move.w records.Entry.Length(a5), d3
	sub.w records.Entry.Leaf(a5), d3
	cmp.w d3, d2
	bne.w missing
	lea ARENA(a6), a2
	moveq #0, d0
	move.w records.Entry.Name(a4), d0
	add.w records.Entry.Leaf(a4), d0
	adda.l d0, a2
	lea ARENA(a6), a3
	moveq #0, d0
	move.w records.Entry.Name(a5), d0
	add.w records.Entry.Leaf(a5), d0
	adda.l d0, a3
compareTemplateLeaf
	moveq #0, d1
	move.b (a2)+, d1
	bsr.w fold
	move.w d1, d6
	moveq #0, d1
	move.b (a3)+, d1
	bsr.w fold
	cmp.b d1, d6
	bne.w missing
	subq.w #1, d2
	bne.w compareTemplateLeaf
	moveq #0, d4
	move.w records.Entry.Owner(a4), d4
	moveq #0, d5
	move.w records.Entry.Owner(a5), d5
	moveq #0, d1
templateAncestor
	cmp.w d5, d4
	beq.w foundTemplate
	tst.w d4
	beq.w missing
	move.w d4, d0
	subq.w #1, d0
	lsl.l #4, d0
	lea ENTRIES(a6), a0
	adda.l d0, a0
	move.w records.Entry.Owner(a0), d4
	addq.w #1, d1
	bra.w templateAncestor
exact
	move.w d1, d0
	moveq #0, d1
	sub.w layout.State.Base(a6), d0
	bcs.w missing
	cmp.w layout.State.Count(a6), d0
	bhs.w missing
	add.w d0, d0
	lea MODULE_STATE+modules.OWNERS(a6), a0
	move.w 0(a0, d0.w), d0
	beq.w foundTemplate  ; global template
	cmp.w MODULE_STATE+modules.State.Active(a6), d0
	bne.w missing
foundTemplate
	moveq #0, d0
	bra.w templateDone
missing
	moveq #1, d0
	moveq #-1, d1
templateDone
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; templateDistance

; A0=template name token,A1=scope state. Make a captured .macro/.segment
; visible to the ordinary numeric declaration and module import machinery.
; The definition record itself remains in binary_templates.
; D0/CCR=status; other registers preserved.
declareTemplate	.block
	movem.l d1/a0-a1/a3/a6, -(sp)
	movea.l a1, a6
	bsr.w declare
	bne.w templateDeclared
	btst #1, records.Entry.Flags+1(a3)
	bne.w templateReferenced
	ori.w #TEMPLATE, records.Entry.Flags(a3)
	bra.w templateDeclared
templateReferenced
	moveq #1, d0
templateDeclared
	movem.l (sp)+, d1/a0-a1/a3/a6
	tst.l d0
	rts
	.bend  ; declareTemplate

; A0=four-byte call-name token,A1=scope state. Resolve an imported template
; without changing the caller's record. D0/CCR=status,D1=definition ID.
; Other registers preserved.
resolveTemplate	.block
	movem.l d2/a0-a2, -(sp)
	subq.l #4, sp
	move.l (a0), (sp)
	movea.l sp, a0
	lea bind, a2
	jsr imports.resolveTemplate
	addq.l #4, sp
	movem.l (sp)+, d2/a0-a2
	tst.l d0
	rts
	.bend  ; resolveTemplate
	.priv
Words
	.byte KEY_BLOCK, 5, "block"
	.byte KEY_ENDBLOCK, 8, "endblock"
	.byte KEY_ENDBLOCK, 4, "bend"
	.byte KEY_NAMESPACE, 9, "namespace"
	.byte KEY_ENDNAMESPACE, 12, "endnamespace"
	.byte KEY_ENDNAMESPACE, 4, "endn"
	.byte KEY_MODULE, 6, "module"
	.byte KEY_ENDMODULE, 9, "endmodule"
	.byte KEY_PUB, 3, "pub"
	.byte KEY_PRIV, 4, "priv"
	.byte KEY_USE, 3, "use"
	.byte KEY_SECTION, 7, "section"
	.byte KEY_ENDSECTION, 10, "endsection"
	.byte KEY_REGION, 6, "region"
	.byte KEY_PLACE, 5, "place"
	.byte KEY_OUTPUT, 6, "output"
	.byte KEY_SEGMENT, 7, "segment"
	.byte KEY_ENDSEGMENT, 10, "endsegment"
	.byte KEY_ENDSEGMENT, 4, "ends"
	.byte KEY_MACRO, 5, "macro"
	.byte KEY_ENDMACRO, 8, "endmacro"
	.byte KEY_ENDMACRO, 4, "endm"
	.byte KEY_STRUCT, 6, "struct"
	.byte KEY_ENDSTRUCT, 9, "endstruct"
	.byte KEY_DB, 2, "db"
	.byte KEY_DW, 2, "dw"
	.byte 0
	.align 2  ; the next module shares this instruction section
	.endsection
	.endmodule

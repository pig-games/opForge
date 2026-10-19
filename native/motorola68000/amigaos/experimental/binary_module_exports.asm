; Capture an owned module-local semantic interface before lexical retirement.
; @opforge-owner: experimental.amigaos.binary_module_exports
	.module experimental.amigaos.binary_module_exports
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_frontend as frontend
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_scopes as scopes
	.use experimental.amigaos.binary_lexical_store as lexical
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_modules as modules
	.use experimental.amigaos.binary_package as pkg
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_module_interface as interface
	.use exprvm.amigaos.runtime as exprvm
	.pub
State	.struct
Exports	.res memory.Block.Used+4
Members	.res memory.Block.Used+4
Names	.res memory.Block.Used+4
Bits	.res memory.Block.Used+4
Base	.long ?
Count	.long ?
ExportCount	.long ?
MemberCount	.long ?
Ready	.long ?
Consumed	.long ?
	.endstruct
STATE_BYTES = State.Consumed+4
OK = 0
INVALID = 1
	.priv
PrepareLocal	.struct
Entry	.res records.ENTRY_BYTES
Target	.res records.ENTRY_BYTES
	.endstruct
PREPARE_BYTES = PrepareLocal.Target+records.ENTRY_BYTES
	.section code, kind=code
	.pub
; A0=zero State,A1=completed frontend.Frame with live lexical names.
; D0/CCR=status; other registers preserved. Captures public value declarations
; and all referenced bare member keys. Rows retain local source IDs and owned
; Names-relative offsets; Export.Kind temporarily holds the runtime target ID.
; No frontend, scope, lexical-page or name pointer is retained. On failure State
; owns its partial buffers; release before reuse. Templates/statements and
; deferred/mutable declarations fail closed rather than disappearing silently.
; Publication currently admits scalar/list/range/schema values, not concrete
; address/struct instances whose nominal schema closure is not yet exported.
prepare	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #PREPARE_BYTES, sp
	movea.l sp, a4
	movea.l a0, a5
	moveq #STATE_BYTES/4-1, d0
fresh
	tst.l (a0)+
	bne.w bad
	dbra d0, fresh
	movea.l frontend.Frame.Scratch(a1), a6
	move.l a6, d0
	beq.w bad
	adda.l #frontend.SCOPE_STATE, a6
	tst.w layout.State.Current(a6)
	bne.w bad
	tst.w layout.State.LoopDepth(a6)
	bne.w bad
	tst.w layout.State.Deferred(a6)
	bne.w bad
	moveq #0, d5
	move.w layout.State.Base(a6), d5
	move.l d5, State.Base(a5)
	moveq #0, d6
	move.w layout.State.Count(a6), d6
	move.l d6, State.Count(a5)
	move.l d5, d0
	add.l d6, d0
	cmpi.l #65536, d0
	bhi.w bad
	move.l d6, d0
	addq.l #7, d0
	lsr.l #3, d0
	beq.w empty
	lea State.Bits(a5), a0
	jsr memory.reserveExact
	bne.w bad
	; reserveExact returns status, so compute and clear the bit extent explicitly.
	move.l d6, d1
	addq.l #7, d1
	lsr.l #3, d1
	move.l d1, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
clearBits
	clr.b (a1)+
	subq.l #1, d1
	bne.w clearBits
	moveq #0, d7
entry
	cmp.l d6, d7
	bhs.w members
	lea layout.LEXICAL_STATE(a6), a0
	movea.l a4, a1
	move.l d7, d0
	jsr lexical.readEntry
	bne.w bad
	; Member keys need not themselves be value declarations.
	moveq #0, d0
	move.w records.Entry.MemberId(a4), d0
	cmpi.l #$ffff, d0
	beq.w visibility
	sub.l d5, d0
	bcs.w bad
	cmp.l d6, d0
	bhs.w bad
	move.l d0, d1
	lsr.l #3, d0
	movea.l State.Bits+memory.Block.Pointer(a5), a0
	adda.l d0, a0
	bset d1, (a0)
visibility
	move.w records.Entry.TemplateFlags(a4), d0
	andi.w #records.TEMPLATE_DECLARED+records.TEMPLATE_PUBLIC, d0
	cmpi.w #records.TEMPLATE_DECLARED+records.TEMPLATE_PUBLIC, d0
	beq.w bad
	move.w records.Entry.TemplateFlags(a4), d0
	andi.w #records.STATEMENT_DECLARED+records.STATEMENT_PUBLIC, d0
	cmpi.w #records.STATEMENT_DECLARED+records.STATEMENT_PUBLIC, d0
	beq.w bad
	move.l d7, d0
	add.l d0, d0
	addq.l #2, d0
	cmp.l layout.MODULE_STATE+modules.FLAGS+memory.Block.Used(a6), d0
	bhi.w bad
	subq.l #2, d0
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a6), a0
	move.l a0, d1
	beq.w bad
	move.w 0(a0, d0.l), d0
	move.w d0, d1
	andi.w #modules.MODULE_ID, d1
	bne.w next  ; module identities are not exported values
	andi.w #modules.PUBLIC, d0
	beq.w next
	btst #records.ENCODING_NAME_HIGH_BIT, records.Entry.Flags(a4)
	bne.w next
	move.w records.Entry.Flags(a4), d0
	andi.w #scopes.DECLARED, d0
	beq.w next
	move.w records.Entry.Flags(a4), d0
	andi.w #records.MUTABLE+records.CONDITIONAL_VALUE+records.IMPORT_PROXY+records.BINDING_PROXY, d0
	bne.w bad
	moveq #0, d0
	move.w records.Entry.Target(a4), d0
	sub.l d5, d0
	bcs.w bad
	cmp.l d6, d0
	bhs.w bad
	lea layout.LEXICAL_STATE(a6), a0
	lea PrepareLocal.Target(a4), a1
	jsr lexical.readEntry
	bne.w bad
	move.w PrepareLocal.Target+records.Entry.Flags(a4), d0
	andi.w #scopes.DECLARED, d0
	beq.w bad
	move.w PrepareLocal.Target+records.Entry.Flags(a4), d0
	andi.w #records.MUTABLE+records.CONDITIONAL_VALUE+records.IMPORT_PROXY+records.BINDING_PROXY, d0
	bne.w bad
	bsr.w appendName
	bne.w bad
	move.l d1, d3
	lea State.Exports(a5), a0
	move.l memory.Block.Used(a0), d0
	addi.l #interface.EXPORT_BYTES, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a2
	adda.l memory.Block.Used(a0), a2
	addi.l #interface.EXPORT_BYTES, memory.Block.Used(a0)
	move.l d7, d0
	add.l d5, d0
	move.l d0, interface.Export.Id(a2)
	move.l d3, interface.Export.Name(a2)
	moveq #0, d0
	move.w records.Entry.Length(a4), d0
	move.l d0, interface.Export.NameBytes(a2)
	moveq #0, d0
	move.w records.Entry.Target(a4), d0
	move.l d0, interface.Export.Kind(a2)
	clr.l interface.Export.Low(a2)
	clr.l interface.Export.High(a2)
	clr.l interface.Export.Provenance(a2)
	addq.l #1, State.ExportCount(a5)
next
	addq.l #1, d7
	bra.w entry
members
	moveq #0, d7
member
	cmp.l d6, d7
	bhs.w empty
	move.l d7, d0
	lsr.l #3, d0
	movea.l State.Bits+memory.Block.Pointer(a5), a0
	adda.l d0, a0
	btst d7, (a0)
	beq.w nextMember
	lea layout.LEXICAL_STATE(a6), a0
	movea.l a4, a1
	move.l d7, d0
	jsr lexical.readEntry
	bne.w bad
	bsr.w appendName
	bne.w bad
	bsr.w bareName
	bne.w bad
	move.l d1, d3
	lea State.Members(a5), a0
	move.l memory.Block.Used(a0), d0
	addi.l #interface.MEMBER_BYTES, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a2
	adda.l memory.Block.Used(a0), a2
	addi.l #interface.MEMBER_BYTES, memory.Block.Used(a0)
	move.l d7, d0
	add.l d5, d0
	move.l d0, interface.Member.Id(a2)
	move.l d3, interface.Member.Name(a2)
	moveq #0, d0
	move.w records.Entry.Length(a4), d0
	move.l d0, interface.Member.NameBytes(a2)
	addq.l #1, State.MemberCount(a5)
nextMember
	addq.l #1, d7
	bra.w member
empty
	move.l #1, State.Ready(a5)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	adda.w #PREPARE_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; prepare

; A0=prepared State,A1=assembled pkg.Context,A2=empty destination Block.
; D0/CCR=status; other registers preserved. One publish attempt per prepare:
; names materialize only after final runtime cells are checked. interface.build
; copies names/compound arena into Destination; no borrowed runtime pointer
; survives success. On failure release the consumed snapshot and destination.
publish	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #interface.FRAME_BYTES, sp
	movea.l sp, a3
	movea.l a0, a6
	movea.l a1, a5
	cmpi.l #1, State.Ready(a6)
	bne.w bad
	tst.l State.Consumed(a6)
	bne.w bad
	move.l #1, State.Consumed(a6)
	move.l a2, interface.Frame.Destination(a3)
	move.l pkg.Context.Owner(a5), interface.Frame.Owner(a3)
	move.l State.Exports+memory.Block.Pointer(a6), interface.Frame.Exports(a3)
	move.l State.ExportCount(a6), d7
	move.l d7, interface.Frame.ExportCount(a3)
	movea.l interface.Frame.Exports(a3), a4
export
	tst.l d7
	beq.w memberNames
	move.l interface.Export.Kind(a4), d1
	cmp.l pkg.Context.Count(a5), d1
	bhs.w bad
	movea.l pkg.Context.Defined(a5), a0
	move.l a0, d0
	beq.w bad
	tst.b 0(a0, d1.l)
	beq.w bad
	movea.l pkg.Context.Owner(a5), a0
	move.l a0, d0
	beq.w bad
	jsr values.getKind
	bne.w bad
	; Concrete nominal values also require their schema dependency closure.
	; Until that is exported, reject instead of retaining an orphan nominal ID.
	cmpi.l #values.ADDRESS_RECORD, d2
	beq.w bad
	cmpi.l #values.STRUCT_LITERAL, d2
	beq.w bad
	move.l d2, interface.Export.Kind(a4)
	move.l d1, d0
	lsl.l #3, d0
	movea.l pkg.Context.Values(a5), a0
	move.l a0, d3
	beq.w bad
	move.l exprvm.Value.Low(a0, d0.l), interface.Export.Low(a4)
	move.l exprvm.Value.High(a0, d0.l), interface.Export.High(a4)
	clr.l interface.Export.Provenance(a4)
	tst.l d2
	bne.w compound
	movea.l pkg.Context.SectionIds(a5), a0
	move.l a0, d0
	beq.w bad
	moveq #0, d0
	move.b 0(a0, d1.l), d0
	cmpi.l #8, d0
	bhi.w bad
	move.l d0, interface.Export.Provenance(a4)
	bra.w exportName
compound
	tst.l interface.Export.High(a4)
	bne.w bad
exportName
	move.l interface.Export.Name(a4), d0
	move.l interface.Export.NameBytes(a4), d1
	bsr.w namePointer
	bne.w bad
	move.l d1, interface.Export.Name(a4)
	adda.w #interface.EXPORT_BYTES, a4
	subq.l #1, d7
	bra.w export
memberNames
	move.l State.Members+memory.Block.Pointer(a6), interface.Frame.Members(a3)
	move.l State.MemberCount(a6), d7
	move.l d7, interface.Frame.MemberCount(a3)
	movea.l interface.Frame.Members(a3), a4
member
	tst.l d7
	beq.w build
	move.l interface.Member.Name(a4), d0
	move.l interface.Member.NameBytes(a4), d1
	bsr.w namePointer
	bne.w bad
	move.l d1, interface.Member.Name(a4)
	adda.w #interface.MEMBER_BYTES, a4
	subq.l #1, d7
	bra.w member
build
	movea.l a3, a0
	jsr interface.build
	bra.w done
bad
	moveq #INVALID, d0
done
	adda.w #interface.FRAME_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; publish

; A0=State. Release partial preparation, consumed publication or successful
; temporary inputs. Destination belongs to the caller. D0/CCR=0; others kept.
release	.block
	movem.l d1/a0-a1, -(sp)
	movea.l a0, a1
	lea State.Exports(a1), a0
	jsr memory.release
	lea State.Members(a1), a0
	jsr memory.release
	lea State.Names(a1), a0
	jsr memory.release
	lea State.Bits(a1), a0
	jsr memory.release
	movea.l a1, a0
	moveq #STATE_BYTES/4-1, d1
clear
	clr.l (a0)+
	dbra d1, clear
	moveq #OK, d0
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; release
	.priv

; A5=State,A6=scope,A4=Entry copy. D0=status,D1=owned name offset.
; Preserve other registers. The copied terminator is spare capacity, excluded
; from Used. No row pointer crosses Names growth or a lexical page lookup.
appendName	.block
	movem.l d2-d3/a0-a2, -(sp)
	moveq #0, d2
	move.w records.Entry.Length(a4), d2
	beq.w bad
	cmpi.l #interface.NAME_LIMIT, d2
	bhi.w bad
	lea State.Names(a5), a0
	move.l memory.Block.Used(a0), d3
	move.l d3, d0
	add.l d2, d0
	bcs.w bad
	addq.l #1, d0
	bcs.w bad
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l d3, a1
	lea layout.LEXICAL_STATE(a6), a0
	move.l records.Entry.Name(a4), d0
	move.l d2, d1
	jsr lexical.copyName
	bne.w bad
	add.l d2, State.Names+memory.Block.Used(a5)
	move.l d3, d1
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d2-d3/a0-a2
	tst.l d0
	rts
	.bend  ; appendName

; A5=State,A4=bare-key Entry,D1=owned name offset. No scope/name lookup.
; D0/CCR=status; other registers preserved.
bareName	.block
	movem.l d1-d2/a0, -(sp)
	movea.l State.Names+memory.Block.Pointer(a5), a0
	adda.l d1, a0
	moveq #0, d2
	move.w records.Entry.Length(a4), d2
next
	cmpi.b #'.', (a0)+
	beq.w bad
	subq.l #1, d2
	bne.w next
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d2/a0
	tst.l d0
	rts
	.bend  ; bareName

; A6=State,D0=name offset,D1=length. D0=status,D1=borrowed name pointer.
; Preserve other registers; used only after Names has stopped growing.
namePointer	.block
	movem.l d2, -(sp)
	move.l d0, d2
	add.l d1, d2
	bcs.w bad
	cmp.l State.Names+memory.Block.Used(a6), d2
	bhi.w bad
	move.l State.Names+memory.Block.Pointer(a6), d1
	beq.w bad
	add.l d0, d1
	bcs.w bad
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d2
	tst.l d0
	rts
	.bend  ; namePointer
	.endsection
	.endmodule

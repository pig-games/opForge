; Dynamic text definitions. All links are arena offsets plus one (zero is empty).
; A single arena bounds combined entries, scopes, patterns and replacements.
; Names are normalized tokenizer identities, never looked up as source strings.
	.module experimental.amigaos.binary_text_registry
	.cpu 68020
	.use experimental.amigaos.binary_text_layout as layout
	.use experimental.amigaos.binary_memory as memory
	.section code, kind=code
	.pub
; A0=State,D1=u16 identity. D0=status,A3=owned Entry or zero for builtin.
; Clobbers D4/A3. CCR reflects D0. Newest replacement definition wins.
find	.block
	move.l layout.State.Entries(a0), d4
loop
	tst.l d4
	beq.w builtin
	movea.l layout.State.Arena+memory.Block.Pointer(a0), a3
	subq.l #1, d4
	adda.l d4, a3
	cmp.w layout.Entry.Id(a3), d1
	beq.w good
	move.l layout.Entry.Next(a3), d4
	bra.w loop
builtin
	suba.l a3, a3
	cmp.w layout.State.AsciiId(a0), d1
	beq.w good
	cmp.w layout.State.PetsciiId(a0), d1
	beq.w good
	moveq #layout.STATUS_INVALID, d0
	rts
good
	moveq #layout.STATUS_OK, d0
	rts
	.bend  ; find

; A0=State,D1=name ID,D2=base ID or zero to retain/create an empty definition.
; D0=status, all other registers saved. CCR reflects D0.
; Successful begin selects the definition; end restores prior active selection.
begin	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l d1, d6
	move.l d2, d7
	tst.l d6
	beq.w invalid
	cmpi.l #$ffff, d6
	bhi.w invalid
	cmpi.l #$ffff, d7
	bhi.w invalid
	tst.l d7
	beq.w retain
	move.l d7, d1
	bsr.w find
	tst.l d0
	bne.w done
	; Retain the base as an offset across possible allocation relocation.
	moveq #0, d5
	move.l a3, d0
	beq.w create
	move.l a3, d5
	sub.l layout.State.Arena+memory.Block.Pointer(a0), d5
	addq.l #1, d5
	bra.w create
retain
	move.l d6, d1
	bsr.w find
	tst.l d0
	bne.w empty
	move.l a3, d0
	bne.w push
	move.l d6, d7
empty
	moveq #0, d5
create
	move.l #layout.ENTRY_BYTES, d0
	bsr.w allocateRecord
	tst.l d0
	bne.w done
	move.l d4, -(sp)
	move.l layout.State.Entries(a6), layout.Entry.Next(a3)
	move.w d6, layout.Entry.Id(a3)
	clr.l layout.Entry.Escapes(a3)
	lea layout.Entry.Map(a3), a2
	moveq #0, d3
map
	move.w #layout.MISSING, d0
	tst.l d7
	beq.w store
	tst.l d5
	beq.w builtinMap
	movea.l layout.State.Arena+memory.Block.Pointer(a6), a1
	move.l d5, d0
	subq.l #1, d0
	adda.l d0, a1
	move.w d3, d0
	add.w d0, d0
	move.w layout.Entry.Map(a1, d0.w), d0
	bra.w store
builtinMap
	move.w #layout.MISSING, d0
	cmpi.w #128, d3
	bhs.w store
	move.w d3, d0
	cmp.w layout.State.PetsciiId(a6), d7
	bne.w store
	cmpi.w #'A', d3
	blo.w store
	cmpi.w #'Z', d3
	bhi.w lower
	ori.w #$80, d0
	bra.w store
lower
	cmpi.w #'a', d3
	blo.w store
	cmpi.w #'z', d3
	bhi.w store
	subi.w #$20, d0
store
	move.w d0, (a2)+
	addq.w #1, d3
	cmpi.w #layout.CHARACTERS, d3
	blo.w map
	move.l (sp)+, layout.State.Entries(a6)
	; Clone each live escape with the same raw replacement bytes.
	tst.l d5
	beq.w push
	movea.l layout.State.Arena+memory.Block.Pointer(a6), a1
	subq.l #1, d5
	adda.l d5, a1
	move.l layout.Entry.Escapes(a1), d5
clone
	tst.l d5
	beq.w push
	movea.l layout.State.Arena+memory.Block.Pointer(a6), a1
	move.l d5, d0
	subq.l #1, d0
	adda.l d0, a1
	move.l layout.Escape.Next(a1), d5
	move.l layout.Escape.PatternLength(a1), d1
	move.l layout.Escape.ReplacementLength(a1), d2
	lea layout.Escape.Payload(a1), a1
	movea.l a1, a2
	adda.l d1, a2
	movea.l a6, a0
	movea.l layout.State.Arena+memory.Block.Pointer(a0), a3
	move.l layout.State.Entries(a0), d0
	subq.l #1, d0
	adda.l d0, a3
	bsr.w escapeActive
	tst.l d0
	bne.w done
	bra.w clone
push
	movea.l a6, a0
	move.l #layout.SCOPE_BYTES, d0
	bsr.w allocateRecord
	tst.l d0
	bne.w done
	move.l layout.State.Scopes(a0), layout.Scope.Next(a3)
	move.w layout.State.Active(a0), layout.Scope.Previous(a3)
	move.w d6, layout.Scope.Definition(a3)
	move.l d4, layout.State.Scopes(a0)
	move.w d6, layout.State.Active(a0)
	moveq #layout.STATUS_OK, d0
	bra.w done
invalid
	moveq #layout.STATUS_INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; begin

; A0=State. D0=status, other registers saved; CCR reflects D0.
endDefinition	.block
	movem.l d1/a1, -(sp)
	move.l layout.State.Scopes(a0), d1
	beq.w invalid
	movea.l layout.State.Arena+memory.Block.Pointer(a0), a1
	subq.l #1, d1
	adda.l d1, a1
	move.w layout.Scope.Previous(a1), layout.State.Active(a0)
	move.l layout.Scope.Next(a1), layout.State.Scopes(a0)
	moveq #layout.STATUS_OK, d0
	bra.w done
invalid
	moveq #layout.STATUS_INVALID, d0
done
	movem.l (sp)+, d1/a1
	tst.l d0
	rts
	.bend  ; endDefinition

; A0=State,D1=source start,D2=end,D3=first mapped byte. D0=status.
; Other registers saved; CCR reflects D0. Duplicates reject as in Rust.
; The shared directive executor also uses singleton ranges for explicit .tdef
; byte lists. A failed multi-byte definition can retain its accepted prefix,
; matching the Rust registry's incremental validation.
defineRange	.block
	movem.l d1-d5/a1-a3, -(sp)
	cmpi.l #255, d1
	bhi.w invalid
	cmpi.l #255, d2
	bhi.w invalid
	cmp.l d1, d2
	blo.w invalid
	cmpi.l #255, d3
	bhi.w invalid
	move.l d1, d5
	bsr.w definition
	tst.l d0
	bne.w done
loop
	cmpi.l #255, d3
	bhi.w invalid
	move.w d5, d4
	add.w d4, d4
	cmpi.w #layout.MISSING, layout.Entry.Map(a3, d4.w)
	bne.w duplicate
	move.w d3, layout.Entry.Map(a3, d4.w)
	addq.l #1, d3
	addq.l #1, d5
	cmp.l d2, d5
	bls.w loop
	moveq #layout.STATUS_OK, d0
	bra.w done
duplicate
	moveq #layout.STATUS_DUPLICATE, d0
	bra.w done
invalid
	moveq #layout.STATUS_INVALID, d0
done
	movem.l (sp)+, d1-d5/a1-a3
	tst.l d0
	rts
	.bend  ; defineRange

; A0=State,A1/D1=nonempty decoded pattern,A2/D2=raw replacement (may be empty).
; D0=status, all other registers saved; CCR reflects D0. Latest same pattern wins.
defineEscape	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l d1, d7
	bsr.w definition
	move.l d7, d1
	tst.l d0
	bne.w done
	bsr.w escapeActive
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; defineEscape

; A0=State,A3=owned Entry,A1/D1=input,A2/D2=output/capacity.
; D0=status,D1=written on success; other registers preserved; CCR reflects D0.
; Failure can leave an unpublished output prefix. Longest escape wins.
encodeOwned	.block
	movem.l d2-d7/a1-a6, -(sp)
	move.l d1, d6
	move.l d2, d7
	movea.l a2, a6
loop
	tst.l d6
	beq.w complete
	move.l layout.Entry.Escapes(a3), d3
	moveq #0, d4
	suba.l a5, a5
scan
	tst.l d3
	beq.w matched
	movea.l layout.State.Arena+memory.Block.Pointer(a0), a4
	subq.l #1, d3
	adda.l d3, a4
	move.l layout.Escape.PatternLength(a4), d2
	cmp.l d6, d2
	bhi.w next
	cmp.l d4, d2
	bls.w next
	moveq #0, d0
compare
	move.b 0(a1, d0.l), d1
	cmp.b layout.Escape.Payload(a4, d0.l), d1
	bne.w next
	addq.l #1, d0
	cmp.l d2, d0
	blo.w compare
	move.l d2, d4
	movea.l a4, a5
next
	move.l layout.Escape.Next(a4), d3
	bra.w scan
matched
	tst.l d4
	beq.w character
	move.l layout.Escape.ReplacementLength(a5), d5
	cmp.l d7, d5
	bhi.w overflow
	sub.l d5, d7
	adda.l d4, a1
	sub.l d4, d6
	lea layout.Escape.Payload(a5), a5
	adda.l d4, a5
copy
	tst.l d5
	beq.w loop
	move.b (a5)+, (a2)+
	subq.l #1, d5
	bra.w copy
character
	moveq #0, d0
	move.b (a1)+, d0
	add.w d0, d0
	move.w layout.Entry.Map(a3, d0.w), d0
	cmpi.w #layout.MISSING, d0
	beq.w missing
	tst.l d7
	beq.w overflow
	move.b d0, (a2)+
	subq.l #1, d7
	subq.l #1, d6
	bra.w loop
complete
	move.l a2, d1
	sub.l a6, d1
	moveq #layout.STATUS_OK, d0
	bra.w done
missing
	moveq #layout.STATUS_MISSING, d0
	bra.w done
overflow
	moveq #layout.STATUS_OVERFLOW, d0
done
	movem.l (sp)+, d2-d7/a1-a6
	tst.l d0
	rts
	.bend  ; encodeOwned

	.priv
; Resolve innermost definition, independent of current .enc selection.
; A0=State,D0=status,A3=Entry; clobbers D1/D4/A3. CCR reflects D0.
definition	.block
	move.l layout.State.Scopes(a0), d4
	beq.w invalid
	movea.l layout.State.Arena+memory.Block.Pointer(a0), a3
	subq.l #1, d4
	adda.l d4, a3
	moveq #0, d1
	move.w layout.Scope.Definition(a3), d1
	bra.w find
invalid
	moveq #layout.STATUS_INVALID, d0
	rts
	.bend  ; definition

; Append D0 bytes to the single bounded arena. A0=State; D0=status,
; A3=record,D4=offset+1. Preserves other registers; CCR reflects D0.
allocateRecord	.block
	movem.l d1-d3/a0-a2, -(sp)
	move.l layout.State.Arena+memory.Block.Used(a0), d4
	move.l d4, d2
	add.l d0, d2
	bcs.w overflow
	addq.l #1, d2
	bcs.w overflow
	andi.l #$fffffffe, d2
	movea.l a0, a2
	lea layout.State.Arena(a0), a0
	move.l d2, d0
	move.l #layout.BUDGET, d1
	jsr memory.reserveBounded
	tst.l d0
	bne.w overflow
	move.l d2, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a3
	adda.l d4, a3
	addq.l #1, d4
	moveq #layout.STATUS_OK, d0
	bra.w done
overflow
	moveq #layout.STATUS_OVERFLOW, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; allocateRecord
; A3=Entry,A0=State,A1/D1=pattern,A2/D2=replacement. D0=status.
; Preserves other registers; CCR reflects D0. Relocates arena input pointers.
escapeActive	.block
	movem.l d1-d7/a0-a6, -(sp)
	tst.l d1
	beq.w invalid
	move.l d1, d3
	add.l d2, d3
	bcs.w overflow
	add.l #layout.ESCAPE_HEADER, d3
	bcs.w overflow
	movea.l layout.State.Arena+memory.Block.Pointer(a0), a5
	move.l layout.State.Arena+memory.Block.Used(a0), d7
	lea layout.Entry.Escapes(a3), a4
	move.l (a4), d5
search
	tst.l d5
	beq.w allocate
	movea.l a5, a6
	move.l d5, d0
	subq.l #1, d0
	adda.l d0, a6
	cmp.l layout.Escape.PatternLength(a6), d1
	bne.w next
	moveq #0, d0
compare
	move.b 0(a1, d0.l), d6
	cmp.b layout.Escape.Payload(a6, d0.l), d6
	bne.w next
	addq.l #1, d0
	cmp.l d1, d0
	blo.w compare
	move.l layout.Escape.Next(a6), d5
	bra.w allocate
next
	lea layout.Escape.Next(a6), a4
	move.l (a4), d5
	bra.w search
allocate
	move.l a4, d6
	sub.l a5, d6
	move.l d3, d0
	bsr.w allocateRecord
	tst.l d0
	bne.w done
	; A1/A2 may refer to live clone payloads in the relocated arena.
	movea.l layout.State.Arena+memory.Block.Pointer(a0), a4
	move.l a4, d3
	sub.l a5, d3
	move.l a1, d0
	sub.l a5, d0
	cmp.l d7, d0
	bhs.w replacementPointer
	adda.l d3, a1
replacementPointer
	move.l a2, d0
	sub.l a5, d0
	cmp.l d7, d0
	bhs.w pointersReady
	adda.l d3, a2
pointersReady
	adda.l d6, a4
	move.l d4, (a4)
	move.l d5, layout.Escape.Next(a3)
	move.l d1, layout.Escape.PatternLength(a3)
	move.l d2, layout.Escape.ReplacementLength(a3)
	lea layout.Escape.Payload(a3), a3
copyPattern
	move.b (a1)+, (a3)+
	subq.l #1, d1
	bne.w copyPattern
copyReplacement
	tst.l d2
	beq.w complete
	move.b (a2)+, (a3)+
	subq.l #1, d2
	bra.w copyReplacement
complete
	moveq #layout.STATUS_OK, d0
	bra.w done
invalid
	moveq #layout.STATUS_INVALID, d0
	bra.w done
overflow
	moveq #layout.STATUS_OVERFLOW, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; escapeActive
	.endsection
	.endmodule

; Bounded numeric bindings for package semantic programs.
; @opforge-owner: tkpkg.amigaos.semantic_bindings

	.module tkpkg.amigaos.semantic_bindings
	.cpu 68020

CAPACITY = 64

Entry	.struct
NameId	.word ?
Version	.word ?
Length	.word ?
Padding	.word ?
Program	.long ?
.endstruct

ENTRY_BYTES = Entry.Program + 4

	.section bss, kind=bss
	.priv

Count
	.res word, 1
	.align 4
Entries
	.res byte, CAPACITY * ENTRY_BYTES

	.endsection

	.section code, kind=code
	.pub

; Invalidate bindings after package replacement or active-pipeline selection.
; Outputs: D0=0. Preserves all other registers.
reset	.block
	clr.w Count
	moveq #0, d0
	rts
	.bend  ; reset

; Find a program descriptor bound to one CMSE string id.
; Inputs: D0.W=name id. Outputs: D0=0 hit, 1 miss; D1.W=version,
; D2.W=length, A1=program on hit. Preserves other registers.
find	.block
	movem.l d3-d4/a0, -(sp)
	move.w d0, d3
	move.w Count, d4
	lea Entries, a0
loop
	tst.w d4
	beq.s miss
	cmp.w Entry.NameId(a0), d3
	beq.s hit
	adda.w #ENTRY_BYTES, a0
	subq.w #1, d4
	bra.s loop
hit
	move.w Entry.Version(a0), d1
	move.w Entry.Length(a0), d2
	movea.l Entry.Program(a0), a1
	movem.l (sp)+, d3-d4/a0
	moveq #0, d0
	rts
miss
	movem.l (sp)+, d3-d4/a0
	moveq #1, d0
	rts
	.bend  ; find

; Retain one validated descriptor. A full cache remains a correct uncached path.
; Inputs: D0.W=name id, D1.W=version, D2.W=length, A1=program.
; Outputs: D0=0. Preserves all other registers.
store	.block
	movem.l d3-d5/a0, -(sp)
	move.w Count, d3
	cmpi.w #CAPACITY, d3
	bhs.s done
	andi.l #$ffff, d3
	move.l d3, d4
	lsl.l #2, d4
	move.l d4, d5
	add.l d4, d4
	add.l d5, d4
	lea Entries, a0
	adda.l d4, a0
	move.w d0, Entry.NameId(a0)
	move.w d1, Entry.Version(a0)
	move.w d2, Entry.Length(a0)
	clr.w Entry.Padding(a0)
	move.l a1, Entry.Program(a0)
	addq.w #1, Count
done
	movem.l (sp)+, d3-d5/a0
	moveq #0, d0
	rts
	.bend  ; store

	.endsection
	.endmodule

; Preparation-only index of explicit module declarations in discovered files.
; Names and file ordinals are stored as offsets and values, never pointers.
; @opforge-owner: experimental.amigaos.binary_declarations
	.module experimental.amigaos.binary_declarations
	.cpu 68020
	.pub
LIMIT = 512
ARENA_BYTES = 16384
Row	.struct
File	.word ?
Name	.word ?
Length	.word ?
FileDerived	.word ?
.endstruct
ROW_BYTES = Row.FileDerived+2
State	.struct
Count	.word ?
Used	.word ?
.endstruct
ROWS = State.Used+2
ARENA = ROWS+LIMIT*ROW_BYTES
EXPLICIT_FILES = ARENA+ARENA_BYTES
SCRATCH_BYTES = EXPLICIT_FILES+LIMIT*2
	.section code, kind=code

; A0=caller-owned SCRATCH_BYTES. Reset the bounded index. D0/CCR=status.
begin	.block
	movem.l d1/a0, -(sp)
	clr.w State.Count(a0)
	clr.w State.Used(a0)
	lea EXPLICIT_FILES(a0), a0
	move.w #LIMIT-1, d1
clear
	clr.w (a0)+
	dbra d1, clear
	movem.l (sp)+, d1/a0
	moveq #0, d0
	rts
	.bend  ; begin

; A0=index,A1=source line,D0=line bytes,D1=one-based candidate file ordinal.
; Record a leading .module name, ignoring all other source lines. This scanner
; only selects files; the normal frontend validates any file actually loaded.
; D0/CCR=status; other registers preserved.
line	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	move.l d0, d6
	move.l d1, d7
space
	tst.l d6
	beq.w skip
	cmpi.b #' ', (a5)
	beq.w advance
	cmpi.b #9, (a5)
	bne.w directive
advance
	addq.l #1, a5
	subq.l #1, d6
	bra.w space
directive
	cmpi.l #8, d6
	blo.w skip
	cmpi.b #'.', (a5)+
	bne.w skip
	lea Keyword, a4
	moveq #5, d5
keywordLoop
	moveq #0, d0
	move.b (a5)+, d0
	bsr.w fold
	cmp.b (a4)+, d0
	bne.w skip
	dbra d5, keywordLoop
	subi.l #7, d6
	move.b (a5), d0
	cmpi.b #' ', d0
	beq.w nameSpace
	cmpi.b #9, d0
	bne.w skip
nameSpace
	tst.l d6
	beq.w skip
	cmpi.b #' ', (a5)
	beq.w nextSpace
	cmpi.b #9, (a5)
	bne.w nameStart
nextSpace
	addq.l #1, a5
	subq.l #1, d6
	bra.w nameSpace
nameStart
	movea.l a5, a4
	moveq #0, d5
nameEnd
	tst.l d6
	beq.w append
	move.b (a5), d0
	cmpi.b #' ', d0
	beq.w append
	cmpi.b #9, d0
	beq.w append
	cmpi.b #13, d0
	beq.w append
	cmpi.b #';', d0
	beq.w append
	addq.l #1, a5
	addq.l #1, d5
	subq.l #1, d6
	cmpi.l #255, d5
	bhi.w skip
	bra.w nameEnd
append
	tst.l d5
	beq.w skip
	moveq #0, d3
	bsr.w appendRow
	bra.w done
skip
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; line

; A0=index,A1=basename,D0=bytes,D1=one-based candidate ordinal.
; Add a fallback that find ignores if the file declares any explicit module.
; D0/CCR=status; other registers preserved.
fileDerived	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a4
	move.l d0, d5
	move.l d1, d7
	moveq #1, d3
	bsr.w appendRow
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; fileDerived
	.priv

; A4=name,D5=length,D7=file ordinal,D3=basename fallback flag,A6=index.
; D0/CCR=status; scratch registers. Names are folded into the index arena.
appendRow	.block
	tst.l d5
	beq.w appendBad
	cmpi.l #255, d5
	bhi.w appendBad
	tst.l d7
	beq.w appendBad
	cmpi.l #LIMIT, d7
	bhi.w appendBad
	moveq #0, d0
	move.w State.Count(a6), d0
	cmpi.w #LIMIT, d0
	bhs.w appendBad
	moveq #0, d1
	move.w State.Used(a6), d1
	add.l d5, d1
	cmpi.l #ARENA_BYTES, d1
	bhi.w appendBad
	mulu.w #ROW_BYTES, d0
	lea ROWS(a6), a3
	adda.l d0, a3
	move.w d7, Row.File(a3)
	move.w State.Used(a6), Row.Name(a3)
	move.w d5, Row.Length(a3)
	move.w d3, Row.FileDerived(a3)
	tst.w d3
	bne.w copyStart
	move.l d7, d0
	subq.w #1, d0
	add.w d0, d0
	lea EXPLICIT_FILES(a6), a1
	move.w #1, 0(a1, d0.w)
copyStart
	lea ARENA(a6), a2
	moveq #0, d0
	move.w State.Used(a6), d0
	adda.l d0, a2
copy
	moveq #0, d0
	move.b (a4)+, d0
	bsr.w fold
	move.b d0, (a2)+
	subq.l #1, d5
	bne.w copy
	move.w d1, State.Used(a6)
	addq.w #1, State.Count(a6)
	moveq #0, d0
	rts
appendBad
	moveq #1, d0
	rts
	.bend  ; appendRow
	.pub

; A0=index,A1=module name,D0=name bytes. Return D0=0,D1=file ordinal,
; D2=one for basename fallback or zero for explicit declaration;
; for exactly one declaring file; D0=1 for absent or ambiguous declarations.
; Repeated declarations within one file are left for full preparation to reject.
; Other registers except D2 preserved; CCR reflects D0.
find	.block
	movem.l d3-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	move.l d0, d6
	moveq #0, d7
	moveq #0, d5
	moveq #0, d2
next
	cmp.w State.Count(a6), d7
	bhs.w finish
	move.l d7, d0
	mulu.w #ROW_BYTES, d0
	lea ROWS(a6), a4
	adda.l d0, a4
	tst.w Row.FileDerived(a4)
	beq.w compareLength
	moveq #0, d0
	move.w Row.File(a4), d0
	subq.w #1, d0
	add.w d0, d0
	lea EXPLICIT_FILES(a6), a1
	tst.w 0(a1, d0.w)
	bne.w advance
compareLength
	cmp.w Row.Length(a4), d6
	bne.w advance
	lea ARENA(a6), a3
	moveq #0, d0
	move.w Row.Name(a4), d0
	adda.l d0, a3
	movea.l a5, a2
	move.l d6, d4
compare
	moveq #0, d0
	move.b (a2)+, d0
	bsr.w fold
	cmp.b (a3)+, d0
	bne.w advance
	subq.l #1, d4
	bne.w compare
	moveq #0, d0
	move.w Row.File(a4), d0
	tst.l d5
	beq.w first
	cmp.l d5, d0
	bne.w bad
	bra.w advance
first
	move.l d0, d5
	move.w Row.FileDerived(a4), d2
advance
	addq.l #1, d7
	bra.w next
finish
	tst.l d5
	beq.w bad
	move.l d5, d1
	moveq #0, d0
	bra.w done
bad
	moveq #0, d1
	moveq #0, d2
	moveq #1, d0
done
	movem.l (sp)+, d3-d7/a0-a6
	tst.l d0
	rts
	.bend  ; find

; A0=index,D0=one-based file ordinal. D0=one if explicitly declared.
; Other registers preserved.
explicitFile	.block
	movem.l d1/a0, -(sp)
	subq.w #1, d0
	add.w d0, d0
	moveq #0, d1
	lea EXPLICIT_FILES(a0), a0
	move.w 0(a0, d0.w), d1
	move.l d1, d0
	movem.l (sp)+, d1/a0
	rts
	.bend  ; explicitFile
	.priv

; D0=ASCII byte. Fold only the spelling, not an opcode or package identity.
fold	.block
	cmpi.b #'A', d0
	blo.w done
	cmpi.b #'Z', d0
	bhi.w done
	addi.b #32, d0
done
	rts
	.bend  ; fold
	.endsection
	.section data, kind=data
Keyword	.byte "module"
	.endsection
	.endmodule

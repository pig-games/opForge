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
.endstruct
ROW_BYTES = Row.Length+2
State	.struct
Count	.word ?
Used	.word ?
.endstruct
ROWS = State.Used+2
ARENA = ROWS+LIMIT*ROW_BYTES
SCRATCH_BYTES = ARENA+ARENA_BYTES
	.section code, kind=code

; A0=caller-owned SCRATCH_BYTES. Reset the bounded index. D0/CCR=status.
begin	.block
	clr.w State.Count(a0)
	clr.w State.Used(a0)
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
	moveq #0, d0
	move.w State.Count(a6), d0
	cmpi.w #LIMIT, d0
	bhs.w bad
	moveq #0, d1
	move.w State.Used(a6), d1
	add.l d5, d1
	cmpi.l #ARENA_BYTES, d1
	bhi.w bad
	mulu.w #ROW_BYTES, d0
	lea ROWS(a6), a3
	adda.l d0, a3
	move.w d7, Row.File(a3)
	move.w State.Used(a6), Row.Name(a3)
	move.w d5, Row.Length(a3)
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

; A0=index,A1=module name,D0=name bytes. Return D0=0,D1=file ordinal
; for exactly one declaring file; D0=1 for absent or ambiguous declarations.
; Repeated declarations within one file are left for full preparation to reject.
; Other registers preserved; CCR reflects D0.
find	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	move.l d0, d6
	moveq #0, d7
	moveq #0, d5
next
	cmp.w State.Count(a6), d7
	bhs.w finish
	move.l d7, d0
	mulu.w #ROW_BYTES, d0
	lea ROWS(a6), a4
	adda.l d0, a4
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
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; find
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

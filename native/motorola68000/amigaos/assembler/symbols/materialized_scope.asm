; Private prepared-source boundaries for already selected scoped iterations.
; @opforge-owner: opforge.assembler.symbols.materialized_scope
	.module opforge.assembler.symbols.materialized_scope
	.cpu 68020
	.pub
TOKEN = $86
COLLECTION_OPEN = 1
ITERATION_OPEN = 2
ITERATION_CLOSE = 3
COLLECTION_CLOSE = 4
ANONYMOUS = $ffff
MAX_ITERATIONS = 65536
Record	.struct
Length	.byte ?
Flags	.byte ?
Line	.word ?
Kind	.byte ?
Action	.byte ?
Label	.word ?
Index	.long ?
.endstruct
RECORD_BYTES = Record.Index+4
	.section code, kind=code
; A0=destination,D0=action,D1=label,D2=index,D3=source line.
; D0/CCR=status. Other registers preserved; no retained interior pointers.
emit	.block
	cmpi.l #COLLECTION_OPEN, d0
	blo.w bad
	cmpi.l #COLLECTION_CLOSE, d0
	bhi.w bad
	cmpi.l #ANONYMOUS, d1
	bhi.w bad
	cmpi.l #COLLECTION_OPEN, d0
	beq.w emptyIndex
	cmpi.l #COLLECTION_CLOSE, d0
	beq.w countIndex
	cmpi.l #MAX_ITERATIONS, d2
	bhs.w bad
	bra.w fields
emptyIndex
	tst.l d2
	bne.w bad
	bra.w fields
countIndex
	cmpi.l #MAX_ITERATIONS, d2
	bhi.w bad
fields
	move.b #RECORD_BYTES-1, Record.Length(a0)
	clr.b Record.Flags(a0)
	move.w d3, Record.Line(a0)
	move.b #TOKEN, Record.Kind(a0)
	move.b d0, Record.Action(a0)
	move.w d1, Record.Label(a0)
	move.l d2, Record.Index(a0)
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; emit
; A0=record,A1=bounded end. D0/CCR=status. Other registers preserved.
validate	.block
	move.l d1, -(sp)
	move.l a1, d1
	sub.l a0, d1
	cmpi.l #RECORD_BYTES, d1
	blo.w bad
	cmpi.b #RECORD_BYTES-1, Record.Length(a0)
	bne.w bad
	tst.b Record.Flags(a0)
	bne.w bad
	cmpi.b #TOKEN, Record.Kind(a0)
	bne.w bad
	cmpi.b #COLLECTION_OPEN, Record.Action(a0)
	blo.w bad
	cmpi.b #COLLECTION_CLOSE, Record.Action(a0)
	bhi.w bad
	cmpi.b #COLLECTION_OPEN, Record.Action(a0)
	beq.w emptyIndex
	cmpi.b #COLLECTION_CLOSE, Record.Action(a0)
	beq.w countIndex
	cmpi.l #MAX_ITERATIONS, Record.Index(a0)
	bhs.w bad
	bra.w good
emptyIndex
	tst.l Record.Index(a0)
	bne.w bad
	bra.w good
countIndex
	cmpi.l #MAX_ITERATIONS, Record.Index(a0)
	bhi.w bad
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	move.l (sp)+, d1
	tst.l d0
	rts
	.bend  ; validate
	.endsection
	.endmodule

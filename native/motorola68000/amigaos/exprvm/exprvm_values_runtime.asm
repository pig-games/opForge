; Canonical typed expressions with immutable session-owned list descriptors.
; @opforge-owner: exprvm.amigaos.values_runtime
	.module exprvm.amigaos.values_runtime
	.cpu 68020
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_memory as memory
	.use exprvm.amigaos.runtime as runtime
	.include "telemetry_macros.i"
	.pub
STATUS_OK = 0
STATUS_MALFORMED = 1
STATUS_STORAGE = 2
STATUS_DEPTH = 3
MAX_DEPTH = 128
STEP_LIMIT = 65536
LEN_ID = 1
Frame	.struct
Values	.long ?
Defined	.long ?
Count	.long ?
Pc	.long ?
High	.long ?
Owner	.long ?
.endstruct
Cell	.struct
Low	.long ?
High	.long ?
Kind	.long ?
.endstruct
CELL_BYTES = 12
State	.struct
Unresolved	.long ?
Symbols	.long ?
Steps	.long ?
Operator	.long ?
Count	.long ?
ArenaStart	.long ?
.endstruct
STACK = State.ArenaStart+4
PACKED = STACK+MAX_DEPTH*CELL_BYTES
TEMP = PACKED+MAX_DEPTH*values.ELEMENT_BYTES
WORK_BYTES = TEMP+22
	.section code, kind=code

; A0/D0=canonical program/bytes,A2=Frame (24 bytes). D0/CCR=status;
; D1=low,D2=unresolved,D3=kind (0 scalar/1 list),D4=symbol presence.
; Frame.High receives scalar high (zero for lists). A0=after End on success.
; Preserves D5-D7/A1-A6. Scratch lives in Owner.Work, reserved once at entry;
; list construction grows only Owner.Arena. No program/version selection state.
evaluate	.block
	movem.l d5-d7/a1-a6, -(sp)
	.TELEMETRY_VM_ENTER runtime_profile.OPFORGE_RUNTIME_VM_EXPRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_EXPRESSION_EVALUATOR
	movea.l a0, a4
	suba.l a3, a3
	movea.l a2, a6
	move.l a4, d1
	add.l d0, d1
	bcs.w malformed
	movea.l d1, a5
	movea.l Frame.Owner(a6), a0
	move.l a0, d1
	beq.w malformed
	lea values.Owner.Work(a0), a0
	move.l #WORK_BYTES, d0
	jsr memory.reserve
	tst.l d0
	bne.w storage
	movea.l memory.Block.Pointer(a0), a3
	move.l #WORK_BYTES, memory.Block.Used(a0)
	clr.l State.Unresolved(a3)
	clr.l State.Symbols(a3)
	clr.l State.Steps(a3)
	movea.l Frame.Owner(a6), a0
	move.l values.Owner.Arena+memory.Block.Used(a0), State.ArenaStart(a3)
	clr.l Frame.High(a6)
	moveq #0, d7
next
	addq.l #1, State.Steps(a3)
	cmpi.l #STEP_LIMIT, State.Steps(a3)
	bhi.w malformed
	bsr.w byte
	bne.w malformed
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_EXPRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_EXPRESSION_EVALUATOR
	tst.l d1
	beq.w finish
	cmpi.b #$10, d1
	beq.w literal
	cmpi.b #$11, d1
	beq.w pushPc
	cmpi.b #$12, d1
	beq.w symbol
	cmpi.b #$20, d1
	beq.w unary
	cmpi.b #$21, d1
	beq.w binary
	cmpi.b #$51, d1
	beq.w list
	cmpi.b #$61, d1
	beq.w index
	cmpi.b #$62, d1
	beq.w builtin
	cmpi.b #$70, d1
	beq.w scalar
	; BuildRange ($52) and all other unsupported shapes fail explicitly.
	bra.w malformed
literal
	bsr.w push
	bne.w depth
	bsr.w longLe
	bne.w malformed
	move.l d1, Cell.Low(a1)
	bsr.w longLe
	bne.w malformed
	move.l d1, Cell.High(a1)
	bra.w next
pushPc
	bsr.w push
	bne.w depth
	move.l Frame.Pc(a6), Cell.Low(a1)
	clr.l Cell.High(a1)
	bra.w next
symbol
	bsr.w wordLe
	bne.w malformed
	cmp.l Frame.Count(a6), d1
	bhs.w malformed
	move.l d1, d6
	moveq #1, d0
	move.l d0, State.Symbols(a3)
	bsr.w push
	bne.w depth
	movea.l Frame.Defined(a6), a2
	move.l a2, d0
	beq.w malformed
	tst.b (a2, d6.l)
	bne.w kind
	moveq #1, d0
	move.l d0, State.Unresolved(a3)
kind
	movea.l Frame.Owner(a6), a0
	move.l d6, d1
	jsr values.getKind
	tst.l d0
	bne.w malformed
	cmpi.l #values.LIST, d2
	bhi.w malformed
	move.l d2, Cell.Kind(a1)
	tst.l d2
	bne.w symbolCell
	movea.l Frame.Defined(a6), a2
	tst.b (a2, d6.l)
	beq.w unresolved
symbolCell
	movea.l Frame.Values(a6), a2
	move.l a2, d0
	beq.w malformed
	lsl.l #3, d6
	move.l 0(a2, d6.l), Cell.Low(a1)
	move.l 4(a2, d6.l), Cell.High(a1)
	tst.l Cell.Kind(a1)
	beq.w next
	tst.l Cell.High(a1)
	bne.w malformed
	; Defined list aliases must name a complete immutable descriptor.
	move.l Cell.Low(a1), d1
	jsr values.listLength
	tst.l d0
	bne.w malformed
	bra.w next
unresolved
	clr.l Cell.Low(a1)
	clr.l Cell.High(a1)
	bra.w next
scalar
	bsr.w top
	bne.w malformed
	tst.l Cell.Kind(a1)
	bne.w malformed
	bra.w next
unary
	moveq #1, d6
	bra.w arithmetic
binary
	moveq #2, d6
arithmetic
	bsr.w byte
	bne.w malformed
	move.l d1, State.Operator(a3)
	cmp.l d6, d7
	blo.w malformed
	bsr.w top
	tst.l Cell.Kind(a1)
	bne.w malformed
	cmpi.l #2, d6
	bne.w prepare
	tst.l Cell.Kind-CELL_BYTES(a1)
	bne.w malformed
prepare
	lea TEMP(a3), a2
	cmpi.l #2, d6
	bne.w first
	suba.l #CELL_BYTES, a1
first
	bsr.w emitLiteral
	cmpi.l #2, d6
	bne.w emitUnary
	adda.l #CELL_BYTES, a1
	bsr.w emitLiteral
	move.b #$21, (a2)+
	bra.w emitOperator
emitUnary
	move.b #$20, (a2)+
emitOperator
	move.l State.Operator(a3), d1
	move.b d1, (a2)+
	clr.b (a2)+
	move.l a2, d0
	lea TEMP(a3), a0
	sub.l a0, d0
	; The scalar runtime has its own small stack and preserves our cursor,
	; depth and scratch pointer. Frame and arithmetic arity remain live here.
	movem.l d6/a6, -(sp)
	suba.l a2, a2
	suba.l a6, a6
	moveq #0, d1
	moveq #0, d2
	jsr runtime.evalNumeric64
	movem.l (sp)+, d6/a6
	tst.l d0
	bne.w malformed
	move.l d3, d5
	jsr runtime.exprvmGetLastResultHighV1
	tst.l d0
	bne.w malformed
	cmpi.l #2, d6
	bne.w arithmeticResult
	subq.l #1, d7
arithmeticResult
	move.l d1, d2
	bsr.w top
	move.l d5, Cell.Low(a1)
	move.l d2, Cell.High(a1)
	bra.w next
list
	bsr.w wordLe
	bne.w malformed
	cmp.l d7, d1
	bhi.w malformed
	move.l d1, State.Count(a3)
	move.l d7, d6
	sub.l d1, d6
	move.l d6, d0
	mulu.w #CELL_BYTES, d0
	lea STACK(a3), a1
	adda.l d0, a1
	lea PACKED(a3), a2
	move.l d1, d5
	tst.l d5
	beq.w append
pack
	tst.l Cell.Kind(a1)
	bne.w malformed
	move.l Cell.Low(a1), (a2)+
	move.l Cell.High(a1), (a2)+
	adda.l #CELL_BYTES, a1
	subq.l #1, d5
	bne.w pack
append
	movea.l Frame.Owner(a6), a0
	lea PACKED(a3), a1
	move.l State.Count(a3), d0
	move.l d0, d1
	lsl.l #3, d1
	jsr values.appendList
	tst.l d0
	bne.w storage
	move.l d6, d7
	bsr.w push
	bne.w depth
	move.l d2, Cell.Low(a1)
	clr.l Cell.High(a1)
	move.l #values.LIST, Cell.Kind(a1)
	bra.w next
index
	cmpi.l #2, d7
	blo.w malformed
	bsr.w top
	tst.l Cell.Kind(a1)
	bne.w malformed
	cmpi.l #values.LIST, Cell.Kind-CELL_BYTES(a1)
	bne.w malformed
	move.l Cell.Low(a1), d2
	move.l Cell.High(a1), d3
	move.l Cell.Low-CELL_BYTES(a1), d1
	movea.l Frame.Owner(a6), a0
	jsr values.listGet
	tst.l d0
	bne.w malformed
	subq.l #1, d7
	move.l d1, d5
	bsr.w top
	move.l d5, Cell.Low(a1)
	move.l d2, Cell.High(a1)
	clr.l Cell.Kind(a1)
	bra.w next
builtin
	bsr.w byte
	bne.w malformed
	cmpi.l #LEN_ID, d1
	bne.w malformed
	bsr.w wordLe
	bne.w malformed
	cmpi.l #1, d1
	bne.w malformed
	bsr.w top
	bne.w malformed
	cmpi.l #values.LIST, Cell.Kind(a1)
	bne.w malformed
	move.l Cell.Low(a1), d1
	movea.l Frame.Owner(a6), a0
	jsr values.listLength
	tst.l d0
	bne.w malformed
	move.l d1, Cell.Low(a1)
	clr.l Cell.High(a1)
	clr.l Cell.Kind(a1)
	bra.w next
finish
	cmpa.l a5, a4
	bne.w malformed
	cmpi.l #1, d7
	bne.w malformed
	bsr.w top
	move.l Cell.Low(a1), d1
	move.l Cell.High(a1), Frame.High(a6)
	move.l Cell.Kind(a1), d3
	tst.l d3
	bne.w retained
	bsr.w discard
	bra.w result
retained
	clr.l Frame.High(a6)
result
	move.l State.Unresolved(a3), d2
	move.l State.Symbols(a3), d4
	movea.l a4, a0
	moveq #STATUS_OK, d0
	bra.w done
malformed
	moveq #STATUS_MALFORMED, d0
	bra.w failed
storage
	moveq #STATUS_STORAGE, d0
	bra.w failed
depth
	moveq #STATUS_DEPTH, d0
failed
	; Entry failures have not initialized scratch and cannot have appended.
	cmpa.l #0, a3
	beq.w clearResult
	bsr.w discard
clearResult
	moveq #0, d1
	moveq #0, d2
	moveq #0, d3
	moveq #0, d4
done
	.TELEMETRY_VM_LEAVE
	movem.l (sp)+, d5-d7/a1-a6
	tst.l d0
	rts
	.bend  ; evaluate

	.priv
; Rewind only transient descriptor bytes. Arena allocation may have moved;
; its current pointer/capacity and all existing descriptors remain intact.
; A3=scratch,A6=Frame. Preserves all data registers and CCR; clobbers A0.
discard	.block
	move.w ccr, -(sp)
	movea.l Frame.Owner(a6), a0
	move.l State.ArenaStart(a3), values.Owner.Arena+memory.Block.Used(a0)
	move.w (sp)+, ccr
	rts
	.bend  ; discard

; D7=depth,A3=scratch. D0/CCR=status,A1=top. Others preserved.
top	.block
	tst.l d7
	beq.w bad
	move.l d7, d0
	subq.l #1, d0
	mulu.w #CELL_BYTES, d0
	lea STACK(a3), a1
	adda.l d0, a1
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_MALFORMED, d0
	rts
	.bend  ; top

; Reserve one scalar cell. D0/CCR=status,A1=new cell,D7=new depth.
push	.block
	cmpi.l #MAX_DEPTH, d7
	bhs.w bad
	addq.l #1, d7
	bsr.w top
	clr.l Cell.Kind(a1)
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_DEPTH, d0
	rts
	.bend  ; push

; A4/A5=cursor/end. D0/CCR=status,D1=unsigned byte,A4 advances on success.
byte	.block
	cmpa.l a5, a4
	bhs.w bad
	moveq #0, d1
	move.b (a4)+, d1
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_MALFORMED, d0
	rts
	.bend  ; byte

; D0/CCR=status,D1=unsigned little-endian word. Preserves D2.
wordLe	.block
	move.l d2, -(sp)
	bsr.w byte
	bne.w done
	move.l d1, d2
	bsr.w byte
	bne.w done
	lsl.l #8, d1
	or.l d2, d1
done
	movem.l (sp)+, d2
	tst.l d0
	rts
	.bend  ; wordLe

; D0/CCR=status,D1=little-endian long. Preserves D2.
longLe	.block
	move.l d2, -(sp)
	bsr.w wordLe
	bne.w done
	move.l d1, d2
	bsr.w wordLe
	bne.w done
	swap d1
	or.l d2, d1
done
	movem.l (sp)+, d2
	tst.l d0
	rts
	.bend  ; longLe

; A1=scalar cell,A2=output. Emits canonical literal; advances A2 by nine.
; Clobbers D0/D1; preserves A1.
emitLiteral	.block
	move.b #$10, (a2)+
	move.l Cell.Low(a1), d1
	bsr.w emitLong
	move.l Cell.High(a1), d1
	bsr.w emitLong
	rts
	.bend  ; emitLiteral

; D1=long,A2=output. Clobbers D1; writes four little-endian bytes.
emitLong	.block
	move.b d1, (a2)+
	lsr.l #8, d1
	move.b d1, (a2)+
	lsr.l #8, d1
	move.b d1, (a2)+
	lsr.l #8, d1
	move.b d1, (a2)+
	rts
	.bend  ; emitLong
	.endsection
	.endmodule

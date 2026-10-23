; Canonical string payloads remain opaque while adjacent symbols are remapped.
; @opforge-owner: opforge.tests.assembler.remap_typed_string
.module opforge.tests.assembler.remap_typed_string
.cpu 68020
.use opforge.assembler.symbols.binding_records as records
.use opforge.assembler.prepare.expression as expression
.use opforge.vm.exprvm.runtime as runtime
.use opforge.vm.exprvm.values_runtime as value_runtime
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
	jsr cases
	tst.l d0
	bne.w done
	lea DosName, a1
	moveq #36, d0
	movea.l EXEC_BASE.w, a6
	jsr EXEC_OPEN_LIBRARY(a6)
	move.l d0, DosBase
	beq.w done
	movea.l d0, a6
	move.l #OutputName, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, OutputHandle
	beq.w closeLibrary
	move.l d0, d1
	move.l #Result, d2
	moveq #1, d3
	jsr DOS_WRITE(a6)
	cmpi.l #1, d0
	bne.w closeFile
	clr.l ReturnCode
closeFile
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
cases	.block
	move.w #$abcd, Entries+records.Entry.Target
	lea Positive, a0
	move.l #PositiveEnd-Positive, d0
	bsr.w remap
	bne.w bad
	cmpi.l #$12008000, PositiveProgram+3
	bne.w bad
	cmpi.b #runtime.EXPRVM_OPCODE_BUILD_STRUCT, PositiveProgram+7
	bne.w bad
	cmpi.b #$cd, PositiveProgram+13
	bne.w bad
	cmpi.b #$ab, PositiveProgram+14
	bne.w bad
	lea Empty, a0
	move.l #EmptyEnd-Empty, d0
	bsr.w remap
	bne.w bad
	cmpi.b #$cd, EmptyProgram+8
	bne.w bad
	cmpi.b #$ab, EmptyProgram+9
	bne.w bad
	; Each malformed candidate has a bounded enclosing capsule and record.
	lea Short, a0
	move.l #ShortEnd-Short, d0
	bsr.w remap
	beq.w bad
	lea Long, a0
	move.l #LongEnd-Long, d0
	bsr.w remap
	beq.w bad
	lea ConsumedEnd, a0
	move.l #ConsumedEndEnd-ConsumedEnd, d0
	bsr.w remap
	beq.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
.bend  ; cases
; A0/D0=record and extent. One high unsigned source ID maps to another.
remap	.block
	lea Entries, a1
	move.l #$8000, d1
	moveq #1, d2
	lea readFixtureEntry, a2
	jsr records.remap
	rts
.bend  ; remap
; A0=one-row fixture owner,A1=owned Entry destination,D0=index.
; D0/CCR=status; all other registers preserved. Reject non-fixture identities.
readFixtureEntry	.block
	tst.l d0
	bne.w bad
	movem.l d1/a0-a1, -(sp)
	moveq #records.ENTRY_BYTES/4-1, d1
copy
	move.l (a0)+, (a1)+
	dbra d1, copy
	movem.l (sp)+, d1/a0-a1
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; readFixtureEntry
.endsection
.section data, kind=data
DosName	.byte "dos.library", 0
OutputName	.byte "Work:remap-typed-string.bin", 0
Result	.byte $5a
Positive
	.byte PositiveEnd-Positive-1, 0
	.word 1
	.byte expression.VALUE_TAG, PositiveProgramEnd-PositiveProgram
PositiveProgram
	.byte runtime.EXPRVM_OPCODE_PUSH_STRING_LITERAL, 5, 0
	.byte runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 0, $80, runtime.EXPRVM_OPCODE_END, runtime.EXPRVM_OPCODE_BUILD_STRUCT
	.byte runtime.EXPRVM_OPCODE_CALL_BUILTIN, value_runtime.LEN_ID, 0, 1
	.byte runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 0, $80
	.byte runtime.EXPRVM_OPCODE_APPLY_BINARY, runtime.EXPRVM_BINARY_ADD, runtime.EXPRVM_OPCODE_END
PositiveProgramEnd
PositiveEnd
Empty
	.byte EmptyEnd-Empty-1, 0
	.word 2
	.byte expression.VALUE_TAG, EmptyProgramEnd-EmptyProgram
EmptyProgram
	.byte runtime.EXPRVM_OPCODE_PUSH_STRING_LITERAL, 0, 0
	.byte runtime.EXPRVM_OPCODE_CALL_BUILTIN, value_runtime.LEN_ID, 0, 1
	.byte runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 0, $80
	.byte runtime.EXPRVM_OPCODE_APPLY_BINARY, runtime.EXPRVM_BINARY_ADD, runtime.EXPRVM_OPCODE_END
EmptyProgramEnd
EmptyEnd
Short
	.byte ShortEnd-Short-1, 0
	.word 3
	.byte expression.VALUE_TAG, 1, runtime.EXPRVM_OPCODE_PUSH_STRING_LITERAL
ShortEnd
Long
	.byte LongEnd-Long-1, 0
	.word 4
	.byte expression.VALUE_TAG, 4, runtime.EXPRVM_OPCODE_PUSH_STRING_LITERAL, $ff, $ff, runtime.EXPRVM_OPCODE_END
LongEnd
ConsumedEnd
	.byte ConsumedEndEnd-ConsumedEnd-1, 0
	.word 5
	.byte expression.VALUE_TAG, 5, runtime.EXPRVM_OPCODE_PUSH_STRING_LITERAL, 2, 0, $42, runtime.EXPRVM_OPCODE_END
ConsumedEndEnd
.endsection
.section bss, kind=bss
.align 4
Entries	.res byte, records.ENTRY_BYTES
DosBase	.res long, 1
OutputHandle	.res long, 1
ReturnCode	.res long, 1
.endsection
.output "remap-typed-string.hunk", format=hunk, sections=entry, code, data, bss
.endmodule

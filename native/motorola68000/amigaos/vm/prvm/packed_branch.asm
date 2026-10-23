; Package-owned conditional selection over packed numeric heads.
; @opforge-owner: opforge.vm.prvm.packed_branch
	.module opforge.vm.prvm.packed_branch
	.cpu 68020
	.use opforge.vm.prvm.abi as abi
	.pub
IF = 1
ELSEIF = 2
ELSE = 3
ENDIF = 4
MATCH = 5
CASE = 6
DEFAULT = 7
ENDMATCH = 8
HEAD_COUNT = 8
COMMA_TOKEN = abi.PRVM_TOKEN_KIND_COMMA
OPCODE = $9a
NON_STATEMENT_FLAGS = $18; omitted and layout-only packed records
NAME_TOKEN_BYTES = 4
LABEL_TOKEN_BYTES = NAME_TOKEN_BYTES+1
Program	.struct
Opcode	.byte ?
Count	.byte ?
Rows	.res HEAD_COUNT*4
Publish	.byte ?
End	.byte ?
	.endstruct
PROGRAM_BYTES = Program.End+1
Row	.struct
Head	.word ?
Role	.byte ?
Expression	.byte ?
	.endstruct
ROW_BYTES = Row.Expression+1
Record	.struct
Length	.byte ?
Flags	.byte ?
Line	.word ?
Dot	.byte ?
Kind	.byte ?
Head	.word ?
Qualifier	.byte ?
Operand	.byte ?
	.endstruct
	.section code, kind=code
; A0=bounded writer/compiled record,A1=validated program,D0=record extent.
; D0/CCR=0 valid,1 malformed; D1=role (zero ordinary). Others preserved.
; Expression syntax is EXVM-owned; the plan selects only its required extent.
classify	.block
	movem.l d2-d3, -(sp)
	bsr.w classifyDetailed
	movem.l (sp)+, d2-d3
	rts
	.bend  ; classify

; Same ABI; preparation-only branch operands belong to the preprocessor kind.
select	.block
	movem.l d2-d3, -(sp)
	bsr.w selectDetailed
	movem.l (sp)+, d2-d3
	rts
	.bend  ; select

; Same inputs as classify. D2=operand offset from original record start,
; D3=label prefix bytes. Both zero for ordinary/malformed. Others preserved.
; D0/CCR=status and D1=role as above.
classifyDetailed	.block
	movem.l d4-d6/a0-a2, -(sp)
	moveq #0, d5
	bra.w classifyMode
	.bend  ; classifyDetailed

; Same detailed ABI; preparation-only operand policy as select.
selectDetailed	.block
	movem.l d4-d6/a0-a2, -(sp)
	moveq #1, d5
	bra.w classifyMode
	.bend  ; selectDetailed
	.priv
classifyMode	.block
	move.l d0, d4
	moveq #0, d1
	moveq #0, d6
	cmpi.l #Record.Dot, d4
	blo.w bad
	cmpi.l #256, d4
	bhi.w bad
	moveq #0, d0
	move.b Record.Length(a0), d0
	addq.w #1, d0
	cmp.l d4, d0
	bne.w bad
	move.b Record.Flags(a0), d0
	andi.b #NON_STATEMENT_FLAGS, d0
	bne.w done
	cmpi.l #Record.Operand, d4
	blo.w done
	cmpi.b #abi.PRVM_TOKEN_KIND_DOT, Record.Dot(a0)
	beq.w bareHead
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, Record.Dot(a0)
	bhi.w done
	tst.b Record.Dot+NAME_TOKEN_BYTES-1(a0)
	bne.w done
	subi.l #NAME_TOKEN_BYTES, d4
	cmpi.l #Record.Operand, d4
	blo.w done
	adda.w #NAME_TOKEN_BYTES, a0
	moveq #NAME_TOKEN_BYTES, d6
	cmpi.b #abi.PRVM_TOKEN_KIND_COLON, Record.Dot(a0)
	bne.w labelHead
	subq.l #1, d4
	cmpi.l #Record.Operand, d4
	blo.w done
	addq.l #1, a0
	moveq #LABEL_TOKEN_BYTES, d6
labelHead
	cmpi.b #abi.PRVM_TOKEN_KIND_DOT, Record.Dot(a0)
	bne.w done
bareHead
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, Record.Kind(a0)
	bhi.w done
	lea Program.Rows(a1), a2
	moveq #HEAD_COUNT-1, d3
rows
	move.w Record.Head(a0), d2
	cmp.w Row.Head(a2), d2
	beq.w selected
	adda.w #ROW_BYTES, a2
	dbra d3, rows
	bra.w done
selected
	tst.b Record.Qualifier(a0)
	bne.w bad
	move.b Row.Role(a2), d1
	tst.l d5
	bne.w done
	tst.b Row.Expression(a2)
	beq.w noOperand
	cmpi.l #Record.Operand, d4
	beq.w bad
	bra.w done
noOperand
	cmpi.l #Record.Operand, d4
	bne.w bad
done
	moveq #0, d0
	bra.w finish
bad
	moveq #1, d0
	moveq #0, d1
finish
	moveq #0, d2
	moveq #0, d3
	tst.l d1
	beq.w restore
	moveq #Record.Operand, d2
	add.l d6, d2
	move.l d6, d3
restore
	movem.l (sp)+, d4-d6/a0-a2
	tst.l d0
	rts
	.bend  ; classifyMode
	.endsection
	.endmodule

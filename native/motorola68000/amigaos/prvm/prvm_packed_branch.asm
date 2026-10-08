; Package-owned conditional selection over packed numeric heads.
; @opforge-owner: prvm.amigaos.packed_branch
	.module prvm.amigaos.packed_branch
	.cpu 68020
	.use prvm.amigaos.abi as abi
	.pub
IF = 1
ELSEIF = 2
ELSE = 3
ENDIF = 4
OPCODE = $9a
NON_STATEMENT_FLAGS = $18  ; omitted and layout-only packed records
Program	.struct
Opcode	.byte ?
Count	.byte ?
Rows	.res 16
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
	movem.l d2-d5/a0-a2, -(sp)
	moveq #0, d5
	bra.w classifyMode
	.bend  ; classify

; Same ABI; select the shared head/role before preparation-only preprocessor
; branch operand policy. The enclosing preprocessor kind owns those operands.
select	.block
	movem.l d2-d5/a0-a2, -(sp)
	moveq #1, d5
	bra.w classifyMode
	.bend  ; select
	.priv
classifyMode	.block
	move.l d0, d4
	moveq #0, d1
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
	bne.w done
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, Record.Kind(a0)
	bhi.w done
	lea Program.Rows(a1), a2
	moveq #3, d3
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
	movem.l (sp)+, d2-d5/a0-a2
	tst.l d0
	rts
	.bend  ; classifyMode
	.endsection
	.endmodule

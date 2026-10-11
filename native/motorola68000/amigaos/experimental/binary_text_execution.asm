; Assembly-owned execution of validated numeric text spans. No source-name lookup.
; @opforge-owner: experimental.amigaos.binary_text_execution
	.module experimental.amigaos.binary_text_execution
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_text as text
	.use experimental.amigaos.binary_text_registry as registry
	.use experimental.amigaos.binary_string_provider as strings
	.use opasm.amigaos.binary_expression as expr
	.use prvm.amigaos.packed_text as grammar
	.use prvm.amigaos.abi as abi
	.use prvm.amigaos.macro_runtime as vm
	.pub
TEXT = grammar.TEXT
RESULT_BYTES = (grammar.MAX_OPERANDS+1)*abi.PRVM_RESULT_RECORD_SIZE
Frame	.struct
State	.long ?
Context	.long ?
Record	.long ?
Bytes	.long ?
Emit	.long ?  ; A0=bytes,D0=count,A2=Context -> D0=status; preserves others
Action	.long ?
Count	.long ?
Request	.res abi.PRVM_REQUEST_FRAME_SIZE
Results	.res RESULT_BYTES
Replacement	.res 256
	.endstruct
FRAME_BYTES = Frame.Replacement+256
	.section code, kind=code
; A0=Frame. Stage the grammar and execute state effects before output filtering.
; D0=status; Frame.Action=zero for no match, otherwise grammar action.
; Preserves other registers; CCR reflects D0. Caller executes emit only if selected.
prepare	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	clr.l Frame.Action(a5)
	movea.l Frame.Context(a5), a6
	movea.l package.Context.Package(a6), a2
	lea Frame.Request(a5), a0
	movea.l a0, a1
	moveq #abi.PRVM_REQUEST_FRAME_SIZE/4-1, d0
clear
	clr.l (a1)+
	dbra d0, clear
	move.l #abi.PRVM_MAGIC_OPRP, abi.PRVM_FRAME_MAGIC(a0)
	move.w #abi.PRVM_REQUEST_FRAME_SIZE, abi.PRVM_FRAME_FRAME_SIZE(a0)
	move.w #abi.PRVM_ENTRY_KIND_PACKED_TEXT, abi.PRVM_FRAME_ENTRY_KIND(a0)
	move.l Frame.Record(a5), abi.PRVM_FRAME_SOURCE_PTR(a0)
	move.l Frame.Bytes(a5), abi.PRVM_FRAME_SOURCE_LEN(a0)
	move.l package.Header.TextPlan(a2), d0
	add.l a2, d0
	move.l d0, abi.PRVM_FRAME_PROGRAM_PTR(a0)
	move.l package.Header.TextPlanBytes(a2), abi.PRVM_FRAME_PROGRAM_LEN(a0)
	lea Frame.Results(a5), a1
	move.l a1, abi.PRVM_FRAME_RESULT_PTR(a0)
	move.l #RESULT_BYTES, abi.PRVM_FRAME_RESULT_CAPACITY(a0)
	move.l #65536, abi.PRVM_FRAME_STEP_BUDGET(a0)
	move.l #abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr vm.run
	bne.w done
	tst.l d1
	beq.w done
	moveq #0, d7
	move.w Frame.Results+abi.PRVM_TEXT_ACTION(a5), d7
	move.l d7, Frame.Action(a5)
	move.l Frame.Results+abi.PRVM_TEXT_OPERAND_COUNT(a5), Frame.Count(a5)
	cmpi.l #grammar.TEXT, d7
	bcc.w ok
	cmpi.l #grammar.SELECT, d7
	beq.w select
	cmpi.l #grammar.BEGIN, d7
	beq.w begin
	movea.l Frame.State(a5), a0
	cmpi.l #grammar.END, d7
	beq.w end
	cmpi.l #grammar.RANGE, d7
	beq.w range
	cmpi.l #grammar.TABLE, d7
	beq.w table
	cmpi.l #grammar.ESCAPE, d7
	beq.w escape
	bra.w bad
select
	moveq #0, d7
	bsr.w operand
	bsr.w name
	bne.w done
	movea.l Frame.State(a5), a0
	jsr text.select
	bra.w done
begin
	moveq #0, d7
	bsr.w operand
	bsr.w name
	bne.w done
	move.l d1, d6
	moveq #0, d2
	cmpi.l #1, Frame.Count(a5)
	beq.w beginDefinition
	moveq #1, d7
	bsr.w operand
	bsr.w name
	bne.w done
	move.l d1, d2
beginDefinition
	move.l d6, d1
	movea.l Frame.State(a5), a0
	jsr registry.begin
	bra.w done
end
	jsr registry.endDefinition
	bra.w done
range
	moveq #0, d7
	bsr.w sourceByte
	bne.w done
	move.l d1, d4
	moveq #1, d7
	bsr.w sourceByte
	bne.w done
	move.l d1, d5
	moveq #2, d7
	bsr.w scalarByte
	bne.w done
	move.l d1, d3
	move.l d4, d1
	move.l d5, d2
	movea.l Frame.State(a5), a0
	jsr registry.defineRange
	bra.w done
table
	moveq #0, d7
	bsr.w operand
	bsr.w rawString
	bne.w done
	movea.l a1, a4
	move.l d1, d6
	tst.l d6
	beq.w bad
	moveq #1, d7
	cmpi.l #2, Frame.Count(a5)
	bne.w explicitTable
	bsr.w scalarByte
	bne.w done
	move.l d1, d5
incrementTable
	tst.l d6
	beq.w ok
	moveq #0, d1
	move.b (a4)+, d1
	move.l d1, d2
	move.l d5, d3
	movea.l Frame.State(a5), a0
	jsr registry.defineRange
	bne.w done
	addq.l #1, d5
	subq.l #1, d6
	bra.w incrementTable
explicitTable
	move.l Frame.Count(a5), d0
	subq.l #1, d0
	cmp.l d6, d0
	bne.w bad
explicitLoop
	bsr.w scalarByte
	bne.w done
	move.l d1, d3
	moveq #0, d1
	move.b (a4)+, d1
	move.l d1, d2
	movea.l Frame.State(a5), a0
	jsr registry.defineRange
	bne.w done
	addq.l #1, d7
	subq.l #1, d6
	bne.w explicitLoop
	bra.w ok
escape
	moveq #0, d7
	bsr.w operand
	bsr.w rawString
	bne.w done
	movea.l a1, a4
	move.l d1, d6
	tst.l d6
	beq.w bad
	moveq #0, d5
	moveq #1, d7
replacementLoop
	cmp.l Frame.Count(a5), d7
	bcc.w defineEscape
	bsr.w operand
	cmpi.w #grammar.STRING, abi.PRVM_TEXT_OPERAND_TYPE(a3)
	bne.w replacementScalar
	bsr.w rawString
	bne.w done
	move.l d5, d0
	add.l d1, d0
	cmpi.l #256, d0
	bhi.w bad
	lea Frame.Replacement(a5), a2
	adda.l d5, a2
	add.l d1, d5
replacementCopy
	tst.l d1
	beq.w replacementNext
	move.b (a1)+, (a2)+
	subq.l #1, d1
	bra.w replacementCopy
replacementScalar
	bsr.w scalarByte
	bne.w done
	cmpi.l #256, d5
	bcc.w bad
	lea Frame.Replacement(a5), a2
	move.b d1, 0(a2, d5.l)
	addq.l #1, d5
replacementNext
	addq.l #1, d7
	bra.w replacementLoop
defineEscape
	movea.l Frame.State(a5), a0
	movea.l a4, a1
	move.l d6, d1
	lea Frame.Replacement(a5), a2
	move.l d5, d2
	jsr registry.defineEscape
	bra.w done
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; prepare

; A0=prepared Frame. Emit only selected .text/.null/.ptext operands.
; D0=status; preserves other registers; CCR reflects D0.
emit	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l Frame.Context(a5), a6
	moveq #0, d7
loop
	bsr.w operand
	bsr.w rawString
	bne.w done
	movea.l Frame.State(a5), a0
	jsr strings.encode
	bne.w done
	move.l d1, d6
	movea.l a1, a4
	cmpi.l #grammar.NULL, Frame.Action(a5)
	bne.w nullChecked
	; .null reserves zero for its terminator. Validate the entire encoded
	; payload before publishing any bytes, including custom escape mappings.
	move.l d6, d0
	movea.l a4, a1
checkNull
	tst.l d0
	beq.w nullChecked
	tst.b (a1)+
	beq.w bad
	subq.l #1, d0
	bra.w checkNull
nullChecked
	cmpi.l #grammar.TEXT, Frame.Action(a5)
	beq.w payload
framed
	cmpi.l #grammar.PTEXT, Frame.Action(a5)
	bne.w payload
	cmpi.l #255, d6
	bhi.w bad
	move.b d6, Frame.Replacement(a5)
	lea Frame.Replacement(a5), a0
	moveq #1, d0
	bsr.w publish
	bne.w done
payload
	movea.l a4, a0
	move.l d6, d0
	bsr.w publish
	bne.w done
	cmpi.l #grammar.NULL, Frame.Action(a5)
	bne.w next
	clr.b Frame.Replacement(a5)
	lea Frame.Replacement(a5), a0
	moveq #1, d0
	bsr.w publish
	bne.w done
next
	addq.l #1, d7
	cmp.l Frame.Count(a5), d7
	blo.w loop
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; emit
	.priv
; A5=Frame,D7=operand slot. A0/A1=bounded token span,A3=result row.
operand	.block
	move.l d7, d0
	addq.l #1, d0
	lsl.l #5, d0
	lea Frame.Results(a5), a3
	adda.l d0, a3
	movea.l Frame.Record(a5), a0
	adda.l abi.PRVM_TEXT_OPERAND_OFFSET(a3), a0
	movea.l a0, a1
	adda.l abi.PRVM_TEXT_OPERAND_BYTES(a3), a1
	rts
	.bend  ; operand
; Read a grammar-validated numeric encoding identity. D1=id,D0=status.
name	.block
	moveq #0, d1
	move.b abi.PackedName.Id(a0), d1
	lsl.w #8, d1
	move.b abi.PackedName.Id+1(a0), d1
	moveq #0, d0
	rts
	.bend  ; name
; Read a grammar-validated standalone decoded string. A1=bytes,D1=length.
rawString	.block
	cmpi.w #grammar.STRING, abi.PRVM_TEXT_OPERAND_TYPE(a3)
	bne.w bad
	moveq #0, d1
	move.b abi.PackedString.Length(a0), d1
	lea abi.PRVM_PACKED_STRING_HEADER_BYTES(a0), a1
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; rawString
; Source characters are decoded bytes, independent of selected mapping.
sourceByte	.block
	bsr.w operand
	cmpi.w #grammar.STRING, abi.PRVM_TEXT_OPERAND_TYPE(a3)
	bne.w scalarByte
	bsr.w rawString
	bne.w bad
	cmpi.l #1, d1
	bne.w bad
	moveq #0, d1
	move.b (a1), d1
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; sourceByte
; Evaluate strict scalar byte through the authoritative expression VM.
scalarByte	.block
	bsr.w operand
	movea.l a6, a2
	jsr expr.evaluate
	bne.w bad
	tst.l d2
	bne.w bad
	tst.l package.Context.High(a6)
	bne.w bad
	cmpa.l a1, a0
	bne.w bad
	cmpi.l #255, d1
	bhi.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; scalarByte
; Publish through assembly's shared byte emitter; A0/D0=span.
publish	.block
	movea.l Frame.Context(a5), a2
	movea.l Frame.Emit(a5), a1
	jsr (a1)
	tst.l d0
	rts
	.bend  ; publish
	.endsection
	.endmodule

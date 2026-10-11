; Shared PRVM text grammar component proof; does not qualify source integration.
.module main
.cpu 68020
.use prvm.amigaos.packed_text as text
.use prvm.amigaos.abi as abi
.use experimental.amigaos.binary_output_io as io
.use opforge.cli.constants as os
ROWS = 4
CASE_BYTES = ROWS*abi.PRVM_RESULT_RECORD_SIZE
RESULT_BYTES = CASE_BYTES*3
.section entry, kind=code
.pub
start	.block
	movem.l d1-d7/a0-a6, -(sp)
	jsr exercise
	tst.l d0
	bne.w failed
	jsr writeResults
	tst.l d0
	bne.w failed
	moveq #0, d0
	bra.w done
failed
	moveq #20, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
.bend  ; start
.endsection
.section code, kind=code
.priv
exercise	.block
	lea Frame, a0
	move.l #abi.PRVM_MAGIC_OPRP, abi.PRVM_FRAME_MAGIC(a0)
	move.w #abi.PRVM_REQUEST_FRAME_SIZE, abi.PRVM_FRAME_FRAME_SIZE(a0)
	move.w #abi.PRVM_ENTRY_KIND_PACKED_TEXT, abi.PRVM_FRAME_ENTRY_KIND(a0)
	move.l #Source, abi.PRVM_FRAME_SOURCE_PTR(a0)
	move.l #SourceEnd-Source, abi.PRVM_FRAME_SOURCE_LEN(a0)
	move.l #Program, abi.PRVM_FRAME_PROGRAM_PTR(a0)
	move.l #ProgramEnd-Program, abi.PRVM_FRAME_PROGRAM_LEN(a0)
	move.l #CASE_BYTES, abi.PRVM_FRAME_RESULT_CAPACITY(a0)
	move.l #100, abi.PRVM_FRAME_STEP_BUDGET(a0)
	clr.l d7
positive
	lea Frame, a0
	lea ActiveFlags, a1
	move.b 0(a1, d7.l), Source+abi.PackedRecord.Flags
	move.l d7, d0
	mulu.w #CASE_BYTES, d0
	lea Results, a1
	adda.l d0, a1
	move.l a1, abi.PRVM_FRAME_RESULT_PTR(a0)
	move.l #abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr text.run
	tst.l d0
	bne.w failed
	cmpi.l #ROWS, d1
	bne.w failed
	cmpi.l #CASE_BYTES, d3
	bne.w failed
	addq.l #1, d7
	cmpi.l #3, d7
	blo.w positive
	; Insufficient output capacity must publish nothing.
	lea Frame, a0
	move.l #Guard, abi.PRVM_FRAME_RESULT_PTR(a0)
	move.l #CASE_BYTES-1, abi.PRVM_FRAME_RESULT_CAPACITY(a0)
	move.l #$12345678, Guard
	move.l #abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr text.run
	cmpi.l #abi.PRVM_STATUS_OUTPUT_OVERFLOW, d0
	bne.w failed
	tst.l d1
	bne.w failed
	tst.l d3
	bne.w failed
	cmpi.l #$12345678, Guard
	bne.w failed
	; Budget exhaustion is also atomic.
	lea Frame, a0
	clr.l abi.PRVM_FRAME_STEP_BUDGET(a0)
	move.l #abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr text.run
	cmpi.l #abi.PRVM_STATUS_BUDGET_EXCEEDED, d0
	bne.w failed
	cmpi.l #$12345678, Guard
	bne.w failed
	; Truncated payload and reserved flags cannot modify caller output.
	lea Frame, a0
	move.l #100, abi.PRVM_FRAME_STEP_BUDGET(a0)
	move.l #CASE_BYTES, abi.PRVM_FRAME_RESULT_CAPACITY(a0)
	move.b #255, Pattern+abi.PackedString.Length
	move.l #abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr text.run
	cmpi.l #abi.PRVM_STATUS_INVALID_TOKEN, d0
	bne.w failed
	cmpi.l #$12345678, Guard
	bne.w failed
	move.b #3, Pattern+abi.PackedString.Length
	move.b #255-abi.PRVM_PACKED_FLAG_ALLOWED, Source+abi.PackedRecord.Flags
	lea Frame, a0
	move.l #abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr text.run
	cmpi.l #abi.PRVM_STATUS_INVALID_ARGUMENT, d0
	bne.w failed
	cmpi.l #$12345678, Guard
	bne.w failed
	; Omitted and layout records require no result capacity.
	lea Frame, a0
	clr.l abi.PRVM_FRAME_RESULT_CAPACITY(a0)
	clr.l d7
inactive
	lea InactiveFlags, a1
	move.b 0(a1, d7.l), Source+abi.PackedRecord.Flags
	lea Frame, a0
	move.l #abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr text.run
	tst.l d0
	bne.w failed
	tst.l d1
	bne.w failed
	addq.l #1, d7
	cmpi.l #2, d7
	blo.w inactive
	; Shared descriptor selects both encoding names at the tokenizer boundary.
	lea PrefixFirst, a0
	move.l #PrefixFirstEnd-PrefixFirst, d0
	lea Program, a1
	move.l #ProgramEnd-Program, d1
	jsr text.nameRole
	tst.l d0
	bne.w failed
	cmpi.l #1, d1
	bne.w failed
	lea PrefixBase, a0
	move.l #PrefixBaseEnd-PrefixBase, d0
	lea Program, a1
	move.l #ProgramEnd-Program, d1
	jsr text.nameRole
	tst.l d0
	bne.w failed
	cmpi.l #2, d1
	bne.w failed
	clr.l d7
labelRoles
	lea LabelFlags, a1
	moveq #0, d1
	move.b 0(a1, d7.l), d1
	lea PrefixLabel, a0
	move.b d1, abi.PackedRecord.Flags(a0)
	move.l #PrefixLabelEnd-PrefixLabel, d0
	moveq #1, d2
	bsr.w checkRole
	bne.w failed
	lea LabelFlags, a1
	move.b 0(a1, d7.l), d1
	ori.b #abi.PRVM_PACKED_FLAG_INDENT, d1
	lea PrefixColon, a0
	move.b d1, abi.PackedRecord.Flags(a0)
	move.l #PrefixColonEnd-PrefixColon, d0
	moveq #1, d2
	bsr.w checkRole
	bne.w failed
	addq.l #1, d7
	cmpi.l #LabelFlagsEnd-LabelFlags, d7
	blo.w labelRoles
	lea PrefixLabel, a0
	move.b #abi.PRVM_PACKED_FLAG_INDENT, abi.PackedRecord.Flags(a0)
	move.l #PrefixLabelEnd-PrefixLabel, d0
	moveq #0, d2
	bsr.w checkRole
	bne.w failed
	moveq #0, d0
	rts
failed
	moveq #20, d0
	rts
.bend  ; exercise
; A0/D0=prefix,D2=expected name slot. D0/status and CCR; D2/D7 preserved.
checkRole	.block
	lea Program, a1
	move.l #ProgramEnd-Program, d1
	jsr text.nameRole
	tst.l d0
	bne.w done
	cmp.l d2, d1
	beq.w done
	moveq #20, d0
done
	tst.l d0
	rts
.bend  ; checkRole
writeResults	.block
	lea DosName, a1
	moveq #36, d0
	movea.l os.SYS_BASE.w, a6
	jsr os.OPEN_LIBRARY(a6)
	tst.l d0
	beq.w failed
	movea.l d0, a5
	movea.l a5, a6
	lea FileFrame, a0
	move.l #OutputName, io.Frame.Path(a0)
	move.l #Results, io.Frame.Data(a0)
	move.l #RESULT_BYTES, io.Frame.Bytes(a0)
	jsr io.write
	move.l d0, d7
	movea.l a5, a1
	movea.l os.SYS_BASE.w, a6
	jsr os.CLOSE_LIBRARY(a6)
	move.l d7, d0
	tst.l d0
	rts
failed
	moveq #20, d0
	rts
.bend  ; writeResults
.endsection
.section data, kind=data
; Numeric fixture IDs use the package descriptor's canonical directive order.
Program	.byte $a5, 10
	.word 1
	.byte 1
	.word 2
	.byte 1
	.word 3
	.byte 2
	.word 4
	.byte 3
	.word 5
	.byte 4
	.word 6
	.byte 5
	.word 7
	.byte 6
	.word 8
	.byte 7
	.word 9
	.byte 8
	.word 10
	.byte 9, $83, 0
ProgramEnd
Source	.byte SourceEnd-Source-1, abi.PRVM_PACKED_FLAG_INDENT
	.word 1
	.byte abi.PRVM_TOKEN_KIND_DOT, abi.PRVM_TOKEN_KIND_IDENTIFIER
	.word 7
	.byte 0
Pattern	.byte abi.PRVM_TOKEN_KIND_STRING, 3, abi.PRVM_TOKEN_KIND_COMMA, abi.PRVM_TOKEN_KIND_OPEN_BRACKET, abi.PRVM_TOKEN_KIND_CLOSE_PAREN
	.byte abi.PRVM_TOKEN_KIND_COMMA, abi.PRVM_TOKEN_KIND_STRING, 0, abi.PRVM_TOKEN_KIND_COMMA, abi.PRVM_TOKEN_KIND_NUMBER
	.long 9
SourceEnd
PrefixFirst	.byte 0, abi.PRVM_PACKED_FLAG_INDENT
	.word 1
	.byte abi.PRVM_TOKEN_KIND_DOT, abi.PRVM_TOKEN_KIND_IDENTIFIER
	.word 1
	.byte 0
PrefixFirstEnd
PrefixBase	.byte 0, abi.PRVM_PACKED_FLAG_INDENT
	.word 1
	.byte abi.PRVM_TOKEN_KIND_DOT, abi.PRVM_TOKEN_KIND_IDENTIFIER
	.word 3
	.byte 0, abi.PRVM_TOKEN_KIND_IDENTIFIER
	.word 20
	.byte 0, abi.PRVM_TOKEN_KIND_COMMA
PrefixBaseEnd
PrefixLabel	.byte 0, 0
	.word 1
	.byte abi.PRVM_TOKEN_KIND_IDENTIFIER
	.word 40
	.byte 0, abi.PRVM_TOKEN_KIND_DOT, abi.PRVM_TOKEN_KIND_IDENTIFIER
	.word 3
	.byte 0
PrefixLabelEnd
PrefixColon	.byte 0, abi.PRVM_PACKED_FLAG_INDENT
	.word 1
	.byte abi.PRVM_TOKEN_KIND_IDENTIFIER
	.word 40
	.byte 0, abi.PRVM_TOKEN_KIND_COLON, abi.PRVM_TOKEN_KIND_DOT, abi.PRVM_TOKEN_KIND_IDENTIFIER
	.word 3
	.byte 0
PrefixColonEnd
LabelFlags	.byte 0, abi.PRVM_PACKED_FLAG_PLAN, abi.PRVM_PACKED_FLAG_MUTABLE_SNAPSHOT, abi.PRVM_PACKED_FLAG_PLAN+abi.PRVM_PACKED_FLAG_MUTABLE_SNAPSHOT
LabelFlagsEnd
ActiveFlags	.byte abi.PRVM_PACKED_FLAG_INDENT, abi.PRVM_PACKED_FLAG_INDENT+abi.PRVM_PACKED_FLAG_MUTABLE_SNAPSHOT, abi.PRVM_PACKED_FLAG_INDENT+abi.PRVM_PACKED_FLAG_PLAN
InactiveFlags	.byte abi.PRVM_PACKED_FLAG_OMIT, abi.PRVM_PACKED_FLAG_LAYOUT
DosName	.byte "dos.library", 0
OutputName	.byte "Work:build/text-grammar.bin", 0
.endsection
.section bss, kind=bss
.align 4
Frame	.res byte, abi.PRVM_REQUEST_FRAME_SIZE
Results	.res byte, RESULT_BYTES
Guard	.res long, 1
FileFrame	.res byte, io.FRAME_BYTES
.endsection
.output "text-grammar.hunk", format=hunk, sections=entry, code, data, bss
.endmodule
.end

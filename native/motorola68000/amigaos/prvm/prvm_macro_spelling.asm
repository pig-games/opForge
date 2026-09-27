; VM-owned temporary literal-fragment lexing and macro spelling boundaries.
; Temporary lexical records never replace already expanded packed execution tokens.
	.module prvm.amigaos.macro_spelling
	.cpu 68020
	.use prvm.amigaos.abi as abi
	.use prvm.amigaos.macro_runtime as macro_runtime
	.use tkvm.amigaos.runtime as tokenizer
	.pub
Frame	.struct
TokenProgram	.long ?
TokenBytes	.long ?
MacroProgram	.long ?
MacroBytes	.long ?
Source	.long ?
SourceBytes	.long ?
Scratch	.long ?
ScratchBytes	.long ?
Result	.long ?
ResultBytes	.long ?
	.endstruct
FRAME_BYTES = Frame.ResultBytes+4
SOURCE_CAPACITY = 256
TOKEN_OFFSET = SOURCE_CAPACITY
TOKEN_CAPACITY = 64
LEXEME_OFFSET = TOKEN_OFFSET+TOKEN_CAPACITY*20
LEXEME_CAPACITY = 5888
SCRATCH_BYTES = LEXEME_OFFSET+LEXEME_CAPACITY
	.section code, kind=code
; A0 Frame. D0 status/D1 count/D2 offset/D3 bytes; preserves D4-D7/A3-A6.
; Caller supplies aligned scratch of SCRATCH_BYTES. Prefix is VM-owned grammar.
run	.block
	movem.l d4-d7/a3-a6, -(sp)
	move.l a0, d0
	beq.w invalid
	movea.l a0, a4
	move.l Frame.SourceBytes(a4), d4
	bmi.w invalid
	cmpi.l #SOURCE_CAPACITY-3, d4
	bhi.w invalid
	move.l Frame.ScratchBytes(a4), d0
	cmpi.l #SCRATCH_BYTES, d0
	bcs.w invalid
	move.l Frame.Scratch(a4), d0
	beq.w invalid
	andi.l #3, d0
	bne.w invalid
	tst.l d4
	beq.w sourceReady
	tst.l Frame.Source(a4)
	beq.w invalid
sourceReady
	movea.l Frame.Scratch(a4), a0
	move.b #'.', (a0)+
	move.b #'x', (a0)+
	move.b #' ', (a0)+
	movea.l Frame.Source(a4), a1
	move.l d4, d0
	beq.w copied
copy
	move.b (a1)+, (a0)+
	subq.l #1, d0
	bne.w copy
copied
	; TKVM owns lexical recognition. Preserve our service state around its ABI.
	movem.l d4-d7/a3-a6, -(sp)
	movea.l Frame.Scratch(a4), a0
	lea TOKEN_OFFSET(a0), a1
	lea LEXEME_OFFSET(a0), a2
	movea.l Frame.TokenProgram(a4), a3
	move.l d4, d0
	addq.l #3, d0
	moveq #TOKEN_CAPACITY, d1
	move.l #LEXEME_CAPACITY, d2
	move.l Frame.TokenBytes(a4), d3
	jsr tokenizer.tkvmRun68000
	movem.l (sp)+, d4-d7/a3-a6
	tst.l d0
	bne.w failed
	move.l d1, d5
	suba.l #abi.PRVM_REQUEST_FRAME_SIZE, sp
	movea.l sp, a3
	movea.l a3, a0
	moveq #abi.PRVM_REQUEST_FRAME_SIZE/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	move.l #abi.PRVM_MAGIC_OPRP, abi.PRVM_FRAME_MAGIC(a3)
	move.w #abi.PRVM_ABI_VERSION_V1, abi.PRVM_FRAME_ABI_VERSION(a3)
	move.w #abi.PRVM_REQUEST_FRAME_SIZE, abi.PRVM_FRAME_FRAME_SIZE(a3)
	move.w #abi.PRVM_ENTRY_KIND_MACRO_DESCRIPTORS, abi.PRVM_FRAME_ENTRY_KIND(a3)
	move.w #20, abi.PRVM_FRAME_TOKEN_RECORD_SIZE(a3)
	move.l #abi.PRVM_PARSER_CONTRACT_VERSION_V2, abi.PRVM_FRAME_PARSER_CONTRACT_VERSION(a3)
	move.l #65536, abi.PRVM_FRAME_STEP_BUDGET(a3)
	move.l Frame.Scratch(a4), d0
	move.l d0, abi.PRVM_FRAME_SOURCE_PTR(a3)
	add.l #TOKEN_OFFSET, d0
	move.l d0, abi.PRVM_FRAME_TOKEN_PTR(a3)
	move.l d5, abi.PRVM_FRAME_TOKEN_COUNT(a3)
	move.l d4, d0
	addq.l #3, d0
	move.l d0, abi.PRVM_FRAME_SOURCE_LEN(a3)
	move.l Frame.MacroProgram(a4), abi.PRVM_FRAME_PROGRAM_PTR(a3)
	move.l Frame.MacroBytes(a4), abi.PRVM_FRAME_PROGRAM_LEN(a3)
	move.l Frame.Result(a4), abi.PRVM_FRAME_RESULT_PTR(a3)
	move.l Frame.ResultBytes(a4), abi.PRVM_FRAME_RESULT_CAPACITY(a3)
	movea.l a3, a0
	move.l #abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr macro_runtime.run
	adda.l #abi.PRVM_REQUEST_FRAME_SIZE, sp
	tst.l d0
	bne.w failed
	; Remove the internal prefix only from VM-selected spelling offsets.
	movea.l Frame.Result(a4), a0
	move.l d1, d4
adjust
	subi.l #3, 12(a0)
	subi.l #3, 16(a0)
	adda.l #32, a0
	subq.l #1, d4
	bne.w adjust
	bra.w done
invalid
	moveq #4, d0
	clr.l d2
failed
	clr.l d1
	clr.l d3
	cmpi.l #3, d2
	bcs.w offsetZero
	subq.l #3, d2
	bra.w done
offsetZero
	clr.l d2
done
	movem.l (sp)+, d4-d7/a3-a6
	tst.l d0
	rts
	.bend  ; run
	.endsection
	.endmodule

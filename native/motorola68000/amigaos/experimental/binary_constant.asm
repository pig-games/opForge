; Apply the shared PRVM declaration plan to the canonical immutable assignment.
; Only numeric spans are consumed; scope and expression owners retain semantics.
	.module experimental.amigaos.binary_constant
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use prvm.amigaos.abi as abi
	.use prvm.amigaos.macro_runtime as parser
	.pub
Frame	.struct
Request	.res abi.PRVM_REQUEST_FRAME_SIZE
Result	.res abi.PRVM_RESULT_RECORD_SIZE
.endstruct
FRAME_BYTES = Frame.Result+abi.PRVM_RESULT_RECORD_SIZE
	.section code, kind=code
; A0=writer record,A1=validated package. D0/CCR=status; other registers kept.
; Shrinks a matched declaration in place, retaining its source line and label.
; Run after template capture/expansion or configuration record selection, before
; scope/conditional consumers. No lexical recipe or PackedMap is reused afterward.
normalize	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a6
	suba.w #FRAME_BYTES, sp
	movea.l sp, a3
	movea.l sp, a0
	moveq #FRAME_BYTES/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	move.l #abi.PRVM_MAGIC_OPRP, abi.PRVM_FRAME_MAGIC(a3)
	move.w #abi.PRVM_ABI_VERSION_V1, abi.PRVM_FRAME_ABI_VERSION(a3)
	move.w #abi.PRVM_REQUEST_FRAME_SIZE, abi.PRVM_FRAME_FRAME_SIZE(a3)
	move.w #abi.PRVM_ENTRY_KIND_PACKED_CONSTANT, abi.PRVM_FRAME_ENTRY_KIND(a3)
	move.l a5, abi.PRVM_FRAME_SOURCE_PTR(a3)
	moveq #0, d6
	move.b (a5), d6
	addq.l #1, d6
	move.l d6, abi.PRVM_FRAME_SOURCE_LEN(a3)
	move.l package.Header.ConstantPlan(a6), d0
	lea 0(a6, d0.l), a0
	move.l a0, abi.PRVM_FRAME_PROGRAM_PTR(a3)
	move.l package.Header.ConstantPlanBytes(a6), abi.PRVM_FRAME_PROGRAM_LEN(a3)
	lea Frame.Result(a3), a0
	move.l a0, abi.PRVM_FRAME_RESULT_PTR(a3)
	move.l #abi.PRVM_RESULT_RECORD_SIZE, abi.PRVM_FRAME_RESULT_CAPACITY(a3)
	move.l #3, abi.PRVM_FRAME_STEP_BUDGET(a3)
	move.l #abi.PRVM_PARSER_CONTRACT_VERSION_V2, abi.PRVM_FRAME_PARSER_CONTRACT_VERSION(a3)
	movea.l a3, a0
	moveq #abi.PRVM_REQUEST_FRAME_SIZE, d0
	move.l a3, -(sp)
	jsr parser.run
	movea.l (sp)+, a3
	bne.w release
	tst.l d1
	beq.w good
	cmpi.l #1, d1
	bne.w bad
	cmpi.w #abi.PRVM_RESULT_PACKED_CONSTANT, Frame.Result(a3)
	bne.w bad
	move.l Frame.Result+abi.PRVM_CONSTANT_VALUE_OFFSET(a3), d0
	cmpi.l #13, d0
	blo.w bad
	cmp.l d6, d0
	bhi.w bad
	move.l Frame.Result+abi.PRVM_CONSTANT_VALUE_BYTES(a3), d1
	move.l d0, d2
	add.l d1, d2
	bcs.w bad
	cmp.l d6, d2
	bne.w bad
	lea 0(a5, d0.l), a0
	lea 8(a5), a1
	move.b #34, (a1)+
copy
	tst.l d1
	beq.w copied
	move.b (a0)+, (a1)+
	subq.l #1, d1
	bra.w copy
copied
	move.l a1, d0
	sub.l a5, d0
	subq.l #1, d0
	move.b d0, (a5)
good
	moveq #0, d0
	bra.w release
bad
	moveq #1, d0
release
	adda.w #FRAME_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; normalize
	.endsection
	.endmodule

; Ordered unit availability over the shared VM's packed data operand plan.
; @opforge-owner: experimental.amigaos.binary_data_prepare
	.module experimental.amigaos.binary_data_prepare
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_imports as imports
	.use prvm.amigaos.abi as abi
	.use prvm.amigaos.macro_runtime as parser
	.pub
Frame	.struct
Request	.res abi.PRVM_REQUEST_FRAME_SIZE
Result	.res 32
Name	.res 4
.endstruct
FRAME_BYTES = Frame.Name+4
	.section code, kind=code
; A0=writer record,A1=package,A2=scope state. D0/CCR=status; others kept.
; Grammar stays in PRVM. Binding checks source IDs at this preparation point,
; before whole-source constant resolution can make a forward unit look known.
check	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a6
	movea.l a2, a4
	moveq #0, d6
	move.b (a5), d6
	addq.l #1, d6
	cmpi.l #9, d6
	blo.w good
	lea 4(a5), a0
	cmpi.b #1, (a0)
	bhi.w head
	cmpi.l #14, d6
	blo.w good
	cmpi.b #5, 4(a0)
	bne.w good
	addq.l #5, a0
head
	cmpi.b #7, (a0)
	bne.w good
	cmpi.b #1, 1(a0)
	bhi.w good
	move.w 2(a0), d0
	cmp.w package.Header.EmitDirective(a6), d0
	bne.w good
	suba.w #FRAME_BYTES, sp
	movea.l sp, a3
	movea.l sp, a0
	moveq #abi.PRVM_REQUEST_FRAME_SIZE/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	move.l #abi.PRVM_MAGIC_OPRP, abi.PRVM_FRAME_MAGIC(a3)
	move.w #abi.PRVM_ABI_VERSION_V1, abi.PRVM_FRAME_ABI_VERSION(a3)
	move.w #abi.PRVM_REQUEST_FRAME_SIZE, abi.PRVM_FRAME_FRAME_SIZE(a3)
	move.w #abi.PRVM_ENTRY_KIND_PACKED_DATA, abi.PRVM_FRAME_ENTRY_KIND(a3)
	move.l a5, abi.PRVM_FRAME_SOURCE_PTR(a3)
	move.l d6, abi.PRVM_FRAME_SOURCE_LEN(a3)
	move.l package.Header.DataPlan(a6), d0
	lea 0(a6, d0.l), a0
	move.l a0, abi.PRVM_FRAME_PROGRAM_PTR(a3)
	move.l package.Header.DataPlanBytes(a6), abi.PRVM_FRAME_PROGRAM_LEN(a3)
	lea Frame.Result(a3), a0
	move.l a0, abi.PRVM_FRAME_RESULT_PTR(a3)
	move.l #32, abi.PRVM_FRAME_RESULT_CAPACITY(a3)
	move.l #1024, abi.PRVM_FRAME_STEP_BUDGET(a3)
	move.l #abi.PRVM_PARSER_CONTRACT_VERSION_V2, abi.PRVM_FRAME_PARSER_CONTRACT_VERSION(a3)
	movea.l a3, a0
	moveq #abi.PRVM_REQUEST_FRAME_SIZE, d0
	move.l a3, -(sp)
	jsr parser.run
	movea.l (sp)+, a3
	bne.w failed
	cmpi.l #1, d1
	bne.w failed
	tst.l Frame.Result+abi.PRVM_DATA_FIXED_WIDTH(a3)
	bne.w checked
	move.l Frame.Result+abi.PRVM_DATA_UNIT_OFFSET(a3), d0
	lea 0(a5, d0.l), a5
	move.l Frame.Result+abi.PRVM_DATA_UNIT_BYTES(a3), d0
	movea.l a5, a6
	adda.l d0, a6
unitToken
	cmpa.l a6, a5
	beq.w checked
	bhi.w failed
	moveq #0, d0
	move.b (a5), d0
	cmpi.b #1, d0
	bls.w name
	cmpi.b #2, d0
	beq.w literal
	addq.l #1, a5
	bra.w unitToken
literal
	addq.l #5, a5
	bra.w unitToken
name
	move.l (a5), Frame.Name(a3)
	lea Frame.Name(a3), a0
	lea 4(a0), a1
	movea.l a4, a2
	jsr imports.evaluateScoped
	beq.w nextName
	moveq #0, d0
	move.w 1(a5), d0
	sub.w layout.State.Base(a4), d0
	bcs.w failed
	cmp.w layout.State.Count(a4), d0
	bhs.w failed
	mulu.w #records.ENTRY_BYTES, d0
	movea.l layout.ENTRIES_POINTER(a4), a0
	adda.l d0, a0
	btst #records.LABEL_VALUE_BIT, records.Entry.Flags+1(a0)
	beq.w failed
nextName
	addq.l #4, a5
	bra.w unitToken
checked
	moveq #0, d0
	bra.w release
failed
	moveq #1, d0
release
	adda.w #FRAME_BYTES, sp
	bra.w done
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend
	.endsection
	.endmodule

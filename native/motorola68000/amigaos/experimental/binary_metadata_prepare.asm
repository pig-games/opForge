; Apply shared PRVM metadata events during preparation; no source parsing.
; @opforge-owner: experimental.amigaos.binary_metadata_prepare
	.module experimental.amigaos.binary_metadata_prepare
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_modules as modules
	.use prvm.amigaos.abi as abi
	.use prvm.amigaos.macro_runtime as parser
	.pub
PATH_BYTES = 256
Config	.struct
NameSet	.word ?
HexSet	.word ?
Root	.word ?
Reserved	.word ?
Name	.res PATH_BYTES
Hex	.res PATH_BYTES
	.endstruct
CONFIG_BYTES = Config.Hex+PATH_BYTES
	.priv
Frame	.struct
Request	.res abi.PRVM_REQUEST_FRAME_SIZE
Result	.res abi.PRVM_RESULT_RECORD_SIZE
	.endstruct
FRAME_BYTES = Frame.Result+abi.PRVM_RESULT_RECORD_SIZE
	.section code, kind=code
	.pub
; A0=record,A1=package,A2=scope,A3=Config (optional),D1=root file boolean.
; D0/CCR=status,D1=handled; other registers kept. Copies output names only;
; descriptions are accepted without retaining unused display bytes. Metadata
; in imported/nested scopes is rejected, including already-disabled .end scope.
check	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a6
	movea.l a2, a4
	move.l d1, d7
	move.l a3, d6
	beq.w parse
	tst.l d7
	beq.w parse
	tst.w Config.Root(a3)
	bne.w parse
	move.w layout.MODULE_STATE+modules.State.Active(a4), Config.Root(a3)
parse
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
	move.w #abi.PRVM_ENTRY_KIND_PACKED_METADATA, abi.PRVM_FRAME_ENTRY_KIND(a3)
	move.l a5, abi.PRVM_FRAME_SOURCE_PTR(a3)
	moveq #0, d0
	move.b (a5), d0
	addq.l #1, d0
	move.l d0, abi.PRVM_FRAME_SOURCE_LEN(a3)
	move.l package.Header.MetadataPlan(a6), d0
	lea 0(a6, d0.l), a0
	move.l a0, abi.PRVM_FRAME_PROGRAM_PTR(a3)
	move.l package.Header.MetadataPlanBytes(a6), abi.PRVM_FRAME_PROGRAM_LEN(a3)
	lea Frame.Result(a3), a0
	move.l a0, abi.PRVM_FRAME_RESULT_PTR(a3)
	move.l #abi.PRVM_RESULT_RECORD_SIZE, abi.PRVM_FRAME_RESULT_CAPACITY(a3)
	move.l #1024, abi.PRVM_FRAME_STEP_BUDGET(a3)
	move.l #abi.PRVM_PARSER_CONTRACT_VERSION_V2, abi.PRVM_FRAME_PARSER_CONTRACT_VERSION(a3)
	movea.l a3, a0
	moveq #abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr parser.run
	bne.w bad
	tst.l d1
	beq.w done
	cmpi.l #1, d1
	bne.w bad
	tst.l d6
	beq.w bad  ; a standalone frontend must opt in to output configuration
	tst.l d7
	beq.w bad
	move.w layout.State.Current(a4), d0
	cmp.w layout.MODULE_STATE+modules.State.Active(a4), d0
	bne.w bad
	tst.w layout.State.Ended(a4)
	bne.w bad
	tst.w d0
	beq.w bad
	movea.l d6, a1
	cmp.w Config.Root(a1), d0
	bne.w bad
scopeReady
	lea Frame.Result(a3), a0
	cmpi.w #abi.PRVM_METADATA_DESCRIPTIVE, abi.PRVM_METADATA_ROLE(a0)
	beq.w consumed
	cmpi.w #abi.PRVM_METADATA_OUTPUT, abi.PRVM_METADATA_ROLE(a0)
	bne.w bad
	movea.l d6, a1
	move.l abi.PRVM_METADATA_KEY(a0), d0
	cmpi.l #abi.PRVM_METADATA_NAME, d0
	beq.w name
	cmpi.l #abi.PRVM_METADATA_HEX, d0
	bne.w bad  ; binary range/fill policy is a separate checkpoint
	lea Config.HexSet(a1), a4
	lea Config.Hex(a1), a1
	bra.w value
name
	lea Config.NameSet(a1), a4
	lea Config.Name(a1), a1
value
	move.l abi.PRVM_METADATA_VALUE_BYTES(a0), d0
	cmpi.l #PATH_BYTES-1, d0
	bhi.w bad
	move.l abi.PRVM_METADATA_VALUE_OFFSET(a0), d2
	lea 0(a5, d2.l), a2
	move.l d0, d3
	movea.l a2, a0
	tst.l d0
	beq.w terminate
validate
	tst.b (a0)+
	beq.w bad  ; AmigaDOS paths cannot contain an embedded NUL
	subq.l #1, d0
	bne.w validate
	move.l d3, d0
copy
	move.b (a2)+, (a1)+
	subq.l #1, d0
	bne.w copy
terminate
	clr.b (a1)
	move.w #1, (a4)
consumed
	move.b #3, (a5)
	clr.b 1(a5)
	moveq #0, d0
	moveq #1, d1
	bra.w done
bad
	moveq #1, d0
	moveq #0, d1
done
	adda.w #FRAME_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; check
	.endsection
	.endmodule

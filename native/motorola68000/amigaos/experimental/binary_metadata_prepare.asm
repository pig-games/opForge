; Apply shared PRVM metadata events during preparation; no source parsing.
; @opforge-owner: experimental.amigaos.binary_metadata_prepare
	.module experimental.amigaos.binary_metadata_prepare
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_modules as modules
	.use experimental.amigaos.binary_memory as memory
	.use prvm.amigaos.abi as abi
	.use prvm.amigaos.macro_runtime as parser
	.pub
PATH_BYTES = 256
Bin	.struct
RangeSet	.word ?
Reserved	.word ?
Start	.long ?
End	.long ?
Name	.res PATH_BYTES
RangeText	.res 10
	.endstruct
BIN_BYTES = Bin.RangeText+10
Config	.struct
NameSet	.word ?
HexSet	.word ?
Root	.word ?
FillSet	.word ?
Fill	.long ?
BinCount	.long ?
Bins	.res memory.Block.Used+4
Name	.res PATH_BYTES
Hex	.res PATH_BYTES
	.endstruct
CONFIG_BYTES = Config.Hex+PATH_BYTES
	.priv
Frame	.struct
Request	.res abi.PRVM_REQUEST_FRAME_SIZE
Result	.res abi.PRVM_RESULT_RECORD_SIZE
StagedBin	.res BIN_BYTES
	.endstruct
FRAME_BYTES = Frame.StagedBin+BIN_BYTES
	.section code, kind=code
	.pub
; A0=fresh Config. Zero-initialize without releasing storage. Registers kept.
clear	.block
	movem.l d0/a0, -(sp)
	move.l #CONFIG_BYTES/4-1, d0
zero
	clr.l (a0)+
	dbra d0, zero
	movem.l (sp)+, d0/a0
	rts
	.bend  ; clear

; A0=initialized Config. Free owned BIN storage and reset all fields.
; Registers kept. Safe for zero-initialized and already released configurations.
release	.block
	move.l a0, -(sp)
	lea Config.Bins(a0), a0
	jsr memory.release
	movea.l (sp)+, a0
	bra.w clear
	.bend  ; release

; A0=record,A1=package,A2=scope,A3=Config (optional),D1=root file boolean.
; D0/CCR=status,D1=handled; other registers kept. Owns output names and BIN specs;
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
	cmpi.l #abi.PRVM_METADATA_BIN, d0
	beq.w binary
	cmpi.l #abi.PRVM_METADATA_FILL, d0
	beq.w fill
	cmpi.l #abi.PRVM_METADATA_HEX, d0
	bne.w bad
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
	bra.w consumed
fill
	move.l abi.PRVM_METADATA_FILL_VALUE(a0), d0
	cmpi.l #255, d0
	bhi.w bad
	move.l d0, Config.Fill(a1)
	move.w #1, Config.FillSet(a1)
	bra.w consumed
binary
	lea Frame.StagedBin(a3), a4
	movea.l a4, a2
	move.l #BIN_BYTES/2-1, d0
zeroBin
	clr.w (a2)+
	dbra d0, zeroBin
	move.l abi.PRVM_METADATA_VALUE_BYTES(a0), d3
	cmpi.l #PATH_BYTES-1, d3
	bhi.w bad
	move.l abi.PRVM_METADATA_VALUE_OFFSET(a0), d2
	lea 0(a5, d2.l), a2
	lea Bin.Name(a4), a1
	tst.l d3
	beq.w binNameReady
binName
	move.b (a2)+, d0
	beq.w bad
	move.b d0, (a1)+
	subq.l #1, d3
	bne.w binName
binNameReady
	moveq #0, d3
	move.w abi.PRVM_METADATA_RANGE_TEXT_BYTES(a0), d3
	beq.w appendBin
	cmpi.w #4, d3
	blo.w bad
	cmpi.w #8, d3
	bhi.w bad
	move.w #1, Bin.RangeSet(a4)
	move.l abi.PRVM_METADATA_RANGE_START(a0), Bin.Start(a4)
	move.l abi.PRVM_METADATA_RANGE_END(a0), Bin.End(a4)
	moveq #0, d2
	move.w abi.PRVM_METADATA_RANGE_TEXT_OFFSET(a0), d2
	lea 0(a5, d2.l), a2
	lea Bin.RangeText(a4), a1
rangeText
	move.b (a2)+, (a1)+
	subq.l #1, d3
	bne.w rangeText
appendBin
	movea.l d6, a1
	lea Config.Bins(a1), a0
	move.l memory.Block.Used(a0), d2
	move.l d2, d0
	addi.l #BIN_BYTES, d0
	bcs.w bad
	move.l #$7fffffff, d1
	jsr memory.reserveBounded
	bne.w bad
	movea.l memory.Block.Pointer(a0), a2
	adda.l d2, a2
	move.l #BIN_BYTES/2-1, d0
publishBin
	move.w (a4)+, (a2)+
	dbra d0, publishBin
	addi.l #BIN_BYTES, memory.Block.Used(a0)
	addq.l #1, Config.BinCount(a1)

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

; Two bounded dependent recipe observations; no evaluation or scheduling.
; @opforge-owner: experimental.amigaos.binary_fragment_sites
	.module experimental.amigaos.binary_fragment_sites
	.cpu 68020
	.use experimental.amigaos.binary_assembly as assembly
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_source as source
	.use opasm.amigaos.binary_expression as expression
	.use prvm.amigaos.packed_branch as branch
	.use prvm.amigaos.abi as abi
	.use prvm.amigaos.macro_runtime as parser
	.pub
LONG_REFERENCE = 1
FORWARD_GUARD = 2
NO_SYMBOL = $ffffffff
STEP_BUDGET = 1024
Site	.struct
Role	.long ?
Record	.long ?
Operand	.long ?
Bytes	.long ?
Symbol	.long ?
	.endstruct
SITE_BYTES = Site.Symbol+4
SITE_COUNT = 2
TABLE_BYTES = SITE_COUNT*SITE_BYTES
	.priv
Scratch	.struct
Request	.res abi.PRVM_REQUEST_FRAME_SIZE
Result	.res abi.PRVM_RESULT_RECORD_SIZE
LongSelector	.res branch.PROGRAM_BYTES
	.endstruct
SCRATCH_BYTES = Scratch.LongSelector+branch.PROGRAM_BYTES
	.section code, kind=code
	.pub
; A0=TABLE_BYTES writable rows,A1=assembly.Frame with validated prepared records.
; D0/CCR=0,D1=observed count; other registers preserved. Zero-role rows are empty.
; Capture first long data capsule and first IF/ELSEIF capsule. Single-symbol
; recipes carry an ID; every other recipe is opaque with NO_SYMBOL. This is a
; deliberately incomplete use index. No symbol values or declaration state change.
observe	.block
	movem.l d2-d7/a0-a6, -(sp)
	suba.w #SCRATCH_BYTES, sp
	movea.l sp, a6
	movea.l a0, a5
	moveq #TABLE_BYTES/4-1, d0
clearTable
	clr.l (a0)+
	dbra d0, clearTable
	movea.l a1, a4
	movea.l assembly.Frame.Context(a4), a2
	movea.l package.Context.Package(a2), a3
	; Reuse the bounded numeric-head reader for the ordinary .long shape.
	; Clone its validated selector layout, then make every head the same long
	; identity. The first row remains IF (an expression-bearing head), so only
	; that row can match; no control is executed or branch semantics published.
	move.l package.Header.BranchPlan(a3), d0
	lea 0(a3, d0.l), a0
	lea Scratch.LongSelector(a6), a1
	moveq #branch.PROGRAM_BYTES-1, d0
copySelector
	move.b (a0)+, (a1)+
	dbra d0, copySelector
	lea Scratch.LongSelector+branch.Program.Rows(a6), a0
	moveq #branch.HEAD_COUNT-1, d0
selectorHeads
	move.w package.Header.LongDirective(a3), branch.Row.Head(a0)
	adda.w #branch.ROW_BYTES, a0
	dbra d0, selectorHeads
	move.l assembly.Frame.RecordBytes(a4), d7
	movea.l assembly.Frame.Records(a4), a4
	moveq #0, d6
	moveq #0, d5
record
	cmp.l d7, d6
	beq.w done
	moveq #0, d4
	move.b source.RecordHeader.Length(a4), d4
	addq.l #1, d4
	move.b source.RecordHeader.Flags(a4), d0
	andi.b #source.FLAG_OMIT+source.FLAG_LAYOUT, d0
	bne.w next
	tst.l SITE_BYTES+Site.Role(a5)
	bne.w data
	movea.l a4, a0
	move.l package.Header.BranchPlan(a3), d0
	lea 0(a3, d0.l), a1
	move.l d4, d0
	jsr branch.classifyDetailed
	bne.w data
	cmpi.l #branch.IF, d1
	beq.w guard
	cmpi.l #branch.ELSEIF, d1
	bne.w data
guard
	move.l d2, d3
	lea 0(a4, d3.l), a0
	movea.l a4, a1
	adda.l d4, a1
	jsr expression.singleSymbol
	bne.w data
	lea SITE_BYTES(a5), a0
	move.l #FORWARD_GUARD, Site.Role(a0)
	bsr.w save
	addq.l #1, d5
data
	tst.l Site.Role(a5)
	bne.w next
	movea.l a4, a0
	lea Scratch.LongSelector(a6), a1
	move.l d4, d0
	jsr branch.classifyDetailed
	bne.w emitFallback
	cmpi.l #branch.IF, d1
	bne.w emitFallback
	move.l d2, d3
	lea 0(a4, d3.l), a0
	movea.l a4, a1
	adda.l d4, a1
	jsr expression.singleSymbol
	bne.w emitFallback
	bra.w longSite
emitFallback
	lea Scratch.Request(a6), a0
	moveq #abi.PRVM_REQUEST_FRAME_SIZE/4-1, d0
clearRequest
	clr.l (a0)+
	dbra d0, clearRequest
	move.l #abi.PRVM_MAGIC_OPRP, abi.PRVM_FRAME_MAGIC(a6)
	move.w #abi.PRVM_ABI_VERSION_V1, abi.PRVM_FRAME_ABI_VERSION(a6)
	move.w #abi.PRVM_REQUEST_FRAME_SIZE, abi.PRVM_FRAME_FRAME_SIZE(a6)
	move.w #abi.PRVM_ENTRY_KIND_PACKED_DATA, abi.PRVM_FRAME_ENTRY_KIND(a6)
	move.l a4, abi.PRVM_FRAME_SOURCE_PTR(a6)
	move.l d4, abi.PRVM_FRAME_SOURCE_LEN(a6)
	move.l package.Header.DataPlan(a3), d0
	lea 0(a3, d0.l), a0
	move.l a0, abi.PRVM_FRAME_PROGRAM_PTR(a6)
	move.l package.Header.DataPlanBytes(a3), abi.PRVM_FRAME_PROGRAM_LEN(a6)
	lea Scratch.Result(a6), a0
	move.l a0, abi.PRVM_FRAME_RESULT_PTR(a6)
	move.l #abi.PRVM_RESULT_RECORD_SIZE, abi.PRVM_FRAME_RESULT_CAPACITY(a6)
	move.l #STEP_BUDGET, abi.PRVM_FRAME_STEP_BUDGET(a6)
	move.l #abi.PRVM_PARSER_CONTRACT_VERSION_V2, abi.PRVM_FRAME_PARSER_CONTRACT_VERSION(a6)
	movea.l a6, a0
	moveq #abi.PRVM_REQUEST_FRAME_SIZE, d0
	movem.l d4-d7/a2-a6, -(sp)
	jsr parser.run
	movem.l (sp)+, d4-d7/a2-a6
	bne.w next
	cmpi.l #1, d1
	bne.w next
	cmpi.l #4, Scratch.Result+abi.PRVM_DATA_FIXED_WIDTH(a6)
	bne.w next
	move.l Scratch.Result+abi.PRVM_DATA_VALUES_OFFSET(a6), d3
	cmp.l d4, d3
	bhs.w next
	move.l Scratch.Result+abi.PRVM_DATA_VALUES_BYTES(a6), d0
	move.l d4, d1
	sub.l d3, d1
	cmp.l d1, d0
	bhi.w next
	lea 0(a4, d3.l), a0
	movea.l a0, a1
	adda.l d0, a1
	jsr expression.singleSymbol
	bne.w next
longSite
	movea.l a5, a0
	move.l #LONG_REFERENCE, Site.Role(a0)
	bsr.w save
	addq.l #1, d5
next
	add.l d4, d6
	adda.l d4, a4
	bra.w record
done
	move.l d5, d1
	adda.w #SCRATCH_BYTES, sp
	moveq #0, d0
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; observe
	.priv
; A0=row,D6=record offset,D3=operand offset,D1=extent,D2=symbol metadata.
; No view pointers are retained. Other registers and CCR unspecified.
save	.block
	move.l d6, Site.Record(a0)
	move.l d6, d0
	add.l d3, d0
	move.l d0, Site.Operand(a0)
	move.l d1, Site.Bytes(a0)
	move.l d2, Site.Symbol(a0)
	rts
	.bend  ; save
	.endsection
	.endmodule

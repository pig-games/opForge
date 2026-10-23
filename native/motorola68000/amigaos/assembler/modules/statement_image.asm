; Closed statement signatures, separate from their ordinary template bodies.
; @opforge-owner: opforge.assembler.modules.statement_image
	.module opforge.assembler.modules.statement_image
	.cpu 68020
	.use opforge.platform.memory as memory
	.use opforge.assembler.language.statements as statements
	.use opforge.assembler.language.templates as templates
	.use opforge.vm.prvm.statement_abi as abi
	.use opforge.vm.prvm.statement_plan as plans
	.pub
Header	.struct
Magic	.long ?
Bytes	.long ?  ; complete even image, including at most one zero padding byte
Row	.long ?
Plan	.long ?
PlanBytes	.long ?
Keyword	.long ?
KeywordBytes	.long ?
	.endstruct
HEADER_BYTES = Header.KeywordBytes+4
MAGIC = $4f464d4b; OFMK: current closed statement metadata contract
CaptureFrame	.struct
State	.long ?
Row	.long ?
Destination	.long ?
	.endstruct
CAPTURE_FRAME_BYTES = CaptureFrame.Destination+4
InstallFrame	.struct
State	.long ?
Image	.long ?
Bytes	.long ?
Name	.long ?
Module	.long ?
Template	.long ?  ; available external carrier from templates.seedClosed
	.endstruct
INSTALL_FRAME_BYTES = InstallFrame.Template+4
OK = 0
INVALID = 1
	.priv
CaptureLocal	.struct
Image	.res memory.Block.Used+4
Plan	.long ?
Keyword	.long ?
Bytes	.long ?
	.endstruct
CAPTURE_BYTES = CaptureLocal.Bytes+4
ValidationLocal	.struct
Work	.res memory.Block.Used+4
Budget	.long ?
	.endstruct
VALIDATION_BYTES = ValidationLocal.Budget+4
	.section code, kind=code
	.pub
; A0=CaptureFrame with an idle source and a borrowed closed source Row.
; Copy exact plan/keyword extents and canonical zero padding into an even owned
; offset-only image. No lexical,
; template, plan-pool or source pointer survives. D0/CCR=status; others kept.
; Success transfers Destination; failure leaves it empty and releases scratch.
capture	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #CAPTURE_BYTES, sp
	movea.l sp, a6
	clr.l CaptureLocal.Image+memory.Block.Pointer(a6)
	clr.l CaptureLocal.Image+memory.Block.Capacity(a6)
	clr.l CaptureLocal.Image+memory.Block.Used(a6)
	movea.l a0, a5
	movea.l CaptureFrame.Destination(a5), a1
	move.l a1, d0
	beq.w bad
	move.l memory.Block.Pointer(a1), d0
	or.l memory.Block.Capacity(a1), d0
	or.l memory.Block.Used(a1), d0
	bne.w bad
	movea.l CaptureFrame.State(a5), a3
	move.l a3, d0
	beq.w bad
	tst.w statements.State.Open(a3)
	bne.w bad
	tst.w statements.State.Skipping(a3)
	bne.w bad
	movea.l CaptureFrame.Row(a5), a4
	move.l a4, d0
	beq.w bad
	btst #0, d0
	bne.w bad
	move.l statements.ROWS+memory.Block.Pointer(a3), d1
	beq.w bad
	sub.l d1, d0
	bcs.w bad
	moveq #0, d1
	move.w statements.State.Count(a3), d1
	mulu.w #statements.ROW_BYTES, d1
	cmp.l statements.ROWS+memory.Block.Used(a3), d1
	bhi.w bad
	addi.l #statements.ROW_BYTES, d0
	bcs.w bad
	cmp.l d1, d0
	bhi.w bad
	move.l statements.Row.PlanOffset(a4), d0
	move.l statements.Row.PlanBytes(a4), d1
	add.l d0, d1
	bcs.w bad
	cmp.l statements.PLANS+memory.Block.Used(a3), d1
	bhi.w bad
	move.l statements.PLANS+memory.Block.Pointer(a3), d1
	beq.w bad
	add.l d1, d0
	bcs.w bad
	move.l d0, CaptureLocal.Plan(a6)
	move.l statements.Row.KeywordOffset(a4), d0
	move.l statements.Row.KeywordBytes(a4), d1
	add.l d0, d1
	bcs.w bad
	cmp.l statements.KEYWORDS+memory.Block.Used(a3), d1
	bhi.w bad
	move.l statements.KEYWORDS+memory.Block.Pointer(a3), d1
	beq.w bad
	add.l d1, d0
	bcs.w bad
	move.l d0, CaptureLocal.Keyword(a6)
	move.l #HEADER_BYTES+statements.ROW_BYTES, d0
	add.l statements.Row.PlanBytes(a4), d0
	bcs.w bad
	add.l statements.Row.KeywordBytes(a4), d0
	bcs.w bad
	addq.l #1, d0
	bcs.w bad
	andi.l #$fffffffe, d0
	cmpi.l #memory.LIMIT, d0
	bhi.w bad
	move.l d0, CaptureLocal.Bytes(a6)
	lea CaptureLocal.Image(a6), a0
	jsr memory.reserveExact
	bne.w bad
	move.l CaptureLocal.Bytes(a6), memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a3
	; Zero the possible final padding byte before the exact keyword copy.
	movea.l a3, a1
	adda.l CaptureLocal.Bytes(a6), a1
	clr.b -(a1)
	move.l #MAGIC, Header.Magic(a3)
	move.l CaptureLocal.Bytes(a6), Header.Bytes(a3)
	move.l #HEADER_BYTES, Header.Row(a3)
	move.l #HEADER_BYTES+statements.ROW_BYTES, Header.Plan(a3)
	move.l statements.Row.PlanBytes(a4), Header.PlanBytes(a3)
	move.l #HEADER_BYTES+statements.ROW_BYTES, d0
	add.l statements.Row.PlanBytes(a4), d0
	move.l d0, Header.Keyword(a3)
	move.l statements.Row.KeywordBytes(a4), Header.KeywordBytes(a3)
	movea.l a4, a0
	lea HEADER_BYTES(a3), a1
	moveq #statements.ROW_BYTES, d0
	bsr.w copy
	clr.l HEADER_BYTES+statements.Row.PlanOffset(a3)
	clr.l HEADER_BYTES+statements.Row.KeywordOffset(a3)
	movea.l CaptureLocal.Plan(a6), a0
	movea.l a3, a1
	adda.l Header.Plan(a3), a1
	move.l Header.PlanBytes(a3), d0
	bsr.w copy
	movea.l CaptureLocal.Keyword(a6), a0
	movea.l a3, a1
	adda.l Header.Keyword(a3), a1
	move.l Header.KeywordBytes(a3), d0
	bsr.w copy
	movea.l a3, a0
	move.l CaptureLocal.Bytes(a6), d0
	bsr.w validate
	bne.w bad
	movea.l CaptureFrame.Destination(a5), a1
	move.l CaptureLocal.Image+memory.Block.Pointer(a6), memory.Block.Pointer(a1)
	move.l CaptureLocal.Image+memory.Block.Capacity(a6), memory.Block.Capacity(a1)
	move.l CaptureLocal.Image+memory.Block.Used(a6), memory.Block.Used(a1)
	clr.l CaptureLocal.Image+memory.Block.Pointer(a6)
	clr.l CaptureLocal.Image+memory.Block.Capacity(a6)
	clr.l CaptureLocal.Image+memory.Block.Used(a6)
	moveq #OK, d7
	bra.w finish
bad
	moveq #INVALID, d7
finish
	lea CaptureLocal.Image(a6), a0
	jsr memory.release
	move.l d7, d0
	adda.w #CAPTURE_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; capture

; A0=InstallFrame. Validate before touching destination, then use the existing
; statements.seed transaction and overload order. Scope is the destination
; module, matching ordinary imported statement exposure. Caller already owns
; the available external template and published independent STATEMENT facets.
; D0/CCR=status,D1=new closed handle (zero on failure); other registers kept.
install	.block
	movem.l d2-d7/a0-a6, -(sp)
	suba.w #statements.SEED_FRAME_BYTES, sp
	movea.l a0, a5
	movea.l InstallFrame.Image(a5), a4
	movea.l a4, a0
	move.l InstallFrame.Bytes(a5), d0
	bsr.w validate
	bne.w bad
	movea.l sp, a0
	move.l InstallFrame.State(a5), statements.SeedFrame.State(a0)
	movea.l a4, a1
	adda.l Header.Row(a4), a1
	move.l a1, statements.SeedFrame.Row(a0)
	movea.l a4, a1
	adda.l Header.Plan(a4), a1
	move.l a1, statements.SeedFrame.Plan(a0)
	move.l Header.PlanBytes(a4), statements.SeedFrame.PlanBytes(a0)
	movea.l a4, a1
	adda.l Header.Keyword(a4), a1
	move.l a1, statements.SeedFrame.Keyword(a0)
	move.l Header.KeywordBytes(a4), statements.SeedFrame.KeywordBytes(a0)
	move.l InstallFrame.Name(a5), statements.SeedFrame.Name(a0)
	move.l InstallFrame.Module(a5), statements.SeedFrame.Scope(a0)
	move.l InstallFrame.Module(a5), statements.SeedFrame.Module(a0)
	move.l InstallFrame.Template(a5), statements.SeedFrame.Template(a0)
	jsr statements.seed
	bra.w done
bad
	moveq #0, d1
	moveq #INVALID, d0
done
	adda.w #statements.SEED_FRAME_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; install

; A0=image,D0=readable bytes. D0/CCR=status; all other registers preserved.
; PRVM validates its own immutable signature rows/payload/grammar indices.
; These plans have no lexical, member, nominal, default or body offsets.
validate	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #VALIDATION_BYTES, sp
	movea.l sp, a6
	clr.l ValidationLocal.Work+memory.Block.Pointer(a6)
	clr.l ValidationLocal.Work+memory.Block.Capacity(a6)
	clr.l ValidationLocal.Work+memory.Block.Used(a6)
	movea.l a0, a5
	move.l d0, d6
	cmpi.l #HEADER_BYTES+statements.ROW_BYTES+abi.PLAN_HEADER_BYTES, d6
	blo.w bad
	cmpi.l #memory.LIMIT, d6
	bhi.w bad
	move.l a5, d0
	beq.w bad
	btst #0, d0
	bne.w bad
	add.l d6, d0
	bcs.w bad
	cmpi.l #MAGIC, Header.Magic(a5)
	bne.w bad
	cmp.l Header.Bytes(a5), d6
	bne.w bad
	cmpi.l #HEADER_BYTES, Header.Row(a5)
	bne.w bad
	cmpi.l #HEADER_BYTES+statements.ROW_BYTES, Header.Plan(a5)
	bne.w bad
	move.l Header.PlanBytes(a5), d0
	cmpi.l #abi.PLAN_HEADER_BYTES, d0
	blo.w bad
	add.l Header.Plan(a5), d0
	bcs.w bad
	cmp.l Header.Keyword(a5), d0
	bne.w bad
	move.l Header.KeywordBytes(a5), d1
	beq.w bad
	add.l d1, d0
	bcs.w bad
	move.l d0, d2
	addq.l #1, d2
	bcs.w bad
	andi.l #$fffffffe, d2
	cmp.l d6, d2
	bne.w bad
	cmp.l d6, d0
	beq.w rowReady
	movea.l a5, a0
	adda.l d0, a0
	tst.b (a0)
	bne.w bad
rowReady
	lea HEADER_BYTES(a5), a4
	move.l Header.PlanBytes(a5), d0
	cmp.l statements.Row.PlanBytes(a4), d0
	bne.w bad
	move.l Header.KeywordBytes(a5), d0
	cmp.l statements.Row.KeywordBytes(a4), d0
	bne.w bad
	tst.l statements.Row.PlanOffset(a4)
	bne.w bad
	tst.l statements.Row.KeywordOffset(a4)
	bne.w bad
	tst.w statements.Row.Reserved(a4)
	bne.w bad
	cmpi.w #1, statements.Row.Visibility(a4)
	bhi.w bad
	cmpi.w #templates.PARAM_LIMIT, statements.Row.CaptureCount(a4)
	bhi.w bad
	tst.w statements.Row.Template(a4)
	beq.w bad
	movea.l a5, a3
	adda.l Header.Plan(a5), a3
	move.l abi.PlanHeader.RowCount(a3), d0
	cmpi.l #memory.LIMIT/abi.ATOM_ROW_BYTES, d0
	bhi.w bad
	move.l d0, d2
	mulu.w #abi.ATOM_ROW_BYTES, d2
	addi.l #abi.PLAN_HEADER_BYTES, d2
	cmp.l Header.PlanBytes(a5), d2
	bhi.w bad
	move.l d0, ValidationLocal.Budget(a6)
	addq.l #1, ValidationLocal.Budget(a6)
	lsl.l #2, d0
	lea ValidationLocal.Work(a6), a0
	jsr memory.reserveExact
	bne.w bad
	movea.l a3, a0
	move.l Header.PlanBytes(a5), d0
	movea.l ValidationLocal.Work+memory.Block.Pointer(a6), a1
	move.l ValidationLocal.Work+memory.Block.Capacity(a6), d1
	lea ValidationLocal.Budget(a6), a2
	jsr plans.validate
	bne.w bad
	moveq #OK, d7
	bra.w finish
bad
	moveq #INVALID, d7
finish
	lea ValidationLocal.Work(a6), a0
	jsr memory.release
	move.l d7, d0
	adda.w #VALIDATION_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; validate
	.priv
; A0/A1=disjoint readable/writable bytes,D0=count; registers preserved.
copy	.block
	movem.l d0/a0-a1, -(sp)
next
	tst.l d0
	beq.w done
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bra.w next
done
	movem.l (sp)+, d0/a0-a1
	rts
	.bend  ; copy
	.endsection
	.endmodule

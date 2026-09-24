; Two bounded assembly passes over binary source, with no source-text interface.
; @opforge-owner: experimental.amigaos.binary_assembly

	.module experimental.amigaos.binary_assembly
	.cpu 68020
	.use experimental.amigaos.binary_package as pkg
	.use opasm.amigaos.binary_expression as expr
	.use experimental.amigaos.binary_encoding as encoding
	.use experimental.amigaos.binary_dependencies as dependencies
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_sections as sections
	.pub

Frame	.struct
Records	.long ?
RecordBytes	.long ?
Context	.long ?
Output	.long ?
Capacity	.long ?
Used	.long ?
Line	.word ?
Reserved	.word ?
Allocate	.long ?
RecordOffset	.long ?
.endstruct

	.section bss, kind=bss
	.priv
Active
	.res long, 1
DataBytes
	.res byte, 4
SectionState
	.res byte, sections.SCRATCH_BYTES
	.endsection
	.section code, kind=code
	.pub

; A0=Frame. Context values/defined arrays cover Count entries. The caller owns
; all buffers. Returns D0=0 only for two complete passes; Used=output bytes.
; Allocate callback: A0=Frame, D0=pass-one output size; returns D0/CCR status,
; preserves other registers, supplies Frame.Output/Capacity before pass two.
; Other registers preserved; CCR reflects D0. No text/dictionary pointer enters
; this module. Variable-size convergence and discontiguous origins are unsupported.
assemble	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	move.l a0, Active
	clr.l Frame.Used(a5)
	move.l #-1, Frame.RecordOffset(a5)
	movea.l Frame.Context(a5), a6
	move.l pkg.Context.Count(a6), d0
	beq.w fail
	cmpi.l #65536, d0
	bhi.w fail
	movea.l pkg.Context.Values(a6), a0
	movea.l pkg.Context.Defined(a6), a1
clearSymbols
	clr.l (a0)+
	clr.b (a1)+
	subq.l #1, d0
	bne.w clearSymbols
	movea.l Frame.Records(a5), a0
	move.l Frame.RecordBytes(a5), d0
	movea.l a6, a2
	jsr dependencies.resolve
	tst.l d0
	bne.w fail
	movea.l Frame.Records(a5), a0
	move.l Frame.RecordBytes(a5), d0
	lea SectionState, a1
	movea.l pkg.Context.Package(a6), a2
	move.l pkg.Header.MaxAddress(a2), d1
	jsr sections.scan
	bne.w fail
	moveq #1, d7
pass
	move.w d7, pkg.Context.Pass(a6)
	clr.l pkg.Context.Pc(a6)
	clr.l Frame.Used(a5)
	lea SectionState, a0
	jsr sections.beginPass
	movea.l Frame.Records(a5), a4
	move.l a4, d0
	add.l Frame.RecordBytes(a5), d0
	bcs.w fail
	movea.l d0, a3
line
	move.l a4, d0
	sub.l Frame.Records(a5), d0
	move.l d0, Frame.RecordOffset(a5)
	cmpa.l a3, a4
	beq.w passDone
	bhi.w fail
	moveq #0, d6
	move.b (a4), d6
	addq.w #1, d6
	cmpi.w #4, d6
	blo.w fail
	move.l a3, d0
	sub.l a4, d0
	cmp.l d0, d6
	bhi.w fail
	move.w 2(a4), Frame.Line(a5)
	moveq #0, d0
	move.b 1(a4), d0
	cmpi.b #source.FLAG_ALLOWED, d0
	bhi.w fail
	btst #4, d0
	bne.w layoutControl
	btst #3, d0
	bne.w omitted
	andi.w #source.FLAG_INDENT, d0
	lea 4(a4), a0
	movea.l a4, a1
	adda.l d6, a1
	movea.l a6, a2
	bsr.w statement
	cmpi.l #2, d0
	beq.w passDone
	tst.l d0
	bne.w fail
	bra.w omitted
layoutControl
	lea SectionState, a0
	movea.l a6, a1
	movea.l a4, a2
	jsr sections.control
	bne.w fail
omitted
	adda.l d6, a4
	bra.w line
passDone
	lea SectionState, a0
	jsr sections.finishPass
	bne.w fail
	cmpi.w #1, d7
	bne.w nextPass
	movea.l Frame.Allocate(a5), a1
	movea.l a5, a0
	move.l Frame.Used(a5), d0
	jsr (a1)
	tst.l d0
	bne.w fail
nextPass
	addq.w #1, d7
	cmpi.w #3, d7
	blo.w pass
	moveq #0, d0
	bra.w done
fail
	clr.l Frame.Used(a5)
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; assemble

	.priv

; A0/A1=bounded token range, A2=Context, D0=indent flag. D0 status, 2=.end.
statement	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l d0, d7
	movea.l pkg.Context.Package(a2), a3
	cmpa.l a1, a0
	beq.w ok
	lea SectionState, a4
	cmpi.w #6, sections.State.Active(a4)
	beq.w bad  ; mapped concrete body ordering is not supported yet
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #5, d0
	blo.w dispatch
	cmpi.b #1, (a0)
	bhi.w dispatch
	cmpi.b #34, 4(a0)
	beq.w constant
	cmpi.b #5, 4(a0)
	bne.w dispatch
	lea SectionState, a4
	tst.w sections.State.Mode(a4)
	beq.w labelSectionReady
	tst.w sections.State.Active(a4)
	beq.w bad
labelSectionReady
	tst.b 3(a0)
	bne.w bad
	moveq #0, d0
	move.w 1(a0), d0
	cmp.w pkg.Header.NameCount(a3), d0
	blo.w bad  ; package spellings are reserved in this bounded experiment
	cmp.l pkg.Context.Count(a2), d0
	bhs.w bad
	movea.l pkg.Context.Defined(a2), a4
	movea.l pkg.Context.Values(a2), a5
	move.l d0, d1
	lsl.l #2, d1
	cmpi.w #1, pkg.Context.Pass(a2)
	bne.w existingLabel
	tst.b 0(a4, d0.l)
	bne.w bad
	move.b #1, 0(a4, d0.l)
	move.l pkg.Context.Pc(a2), 0(a5, d1.l)
	bra.w labelReady
existingLabel
	move.l pkg.Context.Pc(a2), d2
	cmp.l 0(a5, d1.l), d2
	bne.w bad  ; fail closed if this subset needs another layout iteration
labelReady
	addq.l #5, a0
	moveq #1, d7  ; an explicit label permits an instruction on the same line
dispatch
	cmpa.l a1, a0
	beq.w ok
	cmpi.b #7, (a0)
	beq.w directive
	tst.l d7
	beq.w bad
	bsr.w name
	bne.w bad
	; Name returns the numeric identity and qualifier without source reconstruction.
	jsr encoding.encode
	tst.l d0
	bne.w bad
	movea.l a1, a0
	move.l d1, d0
	bsr.w emit
	bra.w done
constant
	; Absolute constants were resolved once before layout. Remaining constants
	; retain definition-site PC/label semantics and must resolve in source order.
	bsr.w name
	bne.w bad
	tst.l d1
	bne.w bad
	cmp.w pkg.Header.NameCount(a3), d0
	blo.w bad
	cmp.l pkg.Context.Count(a2), d0
	bhs.w bad
	move.l d0, d4
	move.l d0, d5
	lsl.l #2, d5
	addq.l #1, a0
	movea.l pkg.Context.Defined(a2), a4
	cmpi.b #dependencies.ABSOLUTE, 0(a4, d4.l)
	beq.w ok  ; dependency preparation validated and evaluated this definition
	movea.l a2, a6
	jsr expr.evaluate
	movea.l a6, a2
	tst.l d0
	bne.w bad
	tst.l d2
	bne.w bad
	cmpa.l a1, a0
	bne.w bad
	movea.l pkg.Context.Defined(a2), a4
	movea.l pkg.Context.Values(a2), a5
	cmpi.w #1, pkg.Context.Pass(a2)
	bne.w existingConstant
	tst.b 0(a4, d4.l)
	bne.w bad
	move.l d1, 0(a5, d5.l)
	move.b #1, 0(a4, d4.l)
	bra.w ok
existingConstant
	cmp.l 0(a5, d5.l), d1
	bne.w bad
	bra.w ok
directive
	addq.l #1, a0
	bsr.w name
	bne.w bad
	tst.l d1
	bne.w bad
	cmp.w pkg.Header.CpuDirective(a3), d0
	beq.w cpu
	cmp.w pkg.Header.OrgDirective(a3), d0
	beq.w origin
	cmp.w pkg.Header.EndDirective(a3), d0
	beq.w end
	moveq #1, d6
	cmp.w pkg.Header.ByteDirective(a3), d0
	beq.w data
	moveq #2, d6
	cmp.w pkg.Header.WordDirective(a3), d0
	beq.w data
	moveq #4, d6
	cmp.w pkg.Header.LongDirective(a3), d0
	beq.w data
	bra.w bad
cpu
	bsr.w name
	bne.w bad
	tst.l d1
	bne.w bad
	cmp.w pkg.Header.CpuName(a3), d0
	bne.w bad  ; the capsule declares this experiment's single pipeline
	cmpa.l a1, a0
	bne.w bad
	bra.w ok
origin
	lea SectionState, a4
	tst.w sections.State.Mode(a4)
	bne.w bad
	movea.l a2, a6
	jsr expr.evaluate
	movea.l a6, a2
	tst.l d0
	bne.w bad
	tst.l d2
	bne.w bad
	tst.l d1
	bmi.w bad
	cmpa.l a1, a0
	bne.w bad
	cmp.l pkg.Header.MaxAddress(a3), d1
	bhi.w bad
	movea.l Active, a4
	tst.l Frame.Used(a4)
	beq.w setOrigin
	cmp.l pkg.Context.Pc(a2), d1
	bne.w bad
setOrigin
	move.l d1, pkg.Context.Pc(a2)
	bra.w ok
end
	cmpa.l a1, a0
	bne.w bad
	moveq #2, d0
	bra.w done
data
	movea.l a2, a6
	jsr expr.evaluate
	movea.l a6, a2
	tst.l d0
	bne.w bad
	tst.l d2
	beq.w dataValue
	cmpi.w #1, pkg.Context.Pass(a2)
	bne.w bad
dataValue
	cmpi.w #4, d6
	beq.w dataRangeOk
	tst.l d1
	bmi.w bad
	cmpi.w #1, d6
	bne.w wordRange
	cmpi.l #255, d1
	bhi.w bad
	bra.w dataRangeOk
wordRange
	cmpi.l #65535, d1
	bhi.w bad
dataRangeOk
	movea.l a0, a5
	lea DataBytes, a4
	move.l d6, d5
	tst.w pkg.Header.LittleEndian(a3)
	beq.w bigEndian
littleLoop
	move.b d1, (a4)+
	lsr.l #8, d1
	subq.w #1, d5
	bne.w littleLoop
	bra.w dataReady
bigEndian
	move.l d6, d4
	subq.l #1, d4
	lsl.l #3, d4
bigLoop
	move.l d1, d3
	lsr.l d4, d3
	move.b d3, (a4)+
	subi.l #8, d4
	subq.w #1, d5
	bne.w bigLoop
dataReady
	lea DataBytes, a0
	move.l d6, d0
	bsr.w emit
	tst.l d0
	bne.w bad
	movea.l a5, a0
	cmpa.l a1, a0
	beq.w ok
	cmpi.b #4, (a0)+
	bne.w bad
	bra.w data
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; statement

; Consume one bounded identifier token. D0=u16 ID, D1=u8 qualifier, A0 advances.
; CCR signals failure; on failure D0=-1. Other registers preserved.
name	.block
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w fail
	cmpi.b #1, (a0)
	bhi.w fail
	moveq #0, d0
	move.w 1(a0), d0
	moveq #0, d1
	move.b 3(a0), d1
	addq.l #4, a0
	cmp.l d0, d0  ; explicit successful CCR, independent of the numeric ID
	rts
fail
	moveq #-1, d0
	rts
	.bend  ; name

; A0=bytes,D0=count,A2=Context. Updates PC and Used; only pass two copies output.
; D0=status; other registers preserved. CCR reflects D0.
emit	.block
	movem.l d1-d4/a0-a3, -(sp)
	movea.l a0, a3
	move.l d0, d4
	lea SectionState, a0
	movea.l a2, a1
	jsr sections.checkEmit
	bne.w fail
	movea.l a3, a0
	move.l d4, d0
	movea.l Active, a3
	move.l Frame.Used(a3), d1
	move.l d1, d2
	add.l d0, d2
	bcs.w fail
	cmpi.w #1, pkg.Context.Pass(a2)
	beq.w capacityReady
	cmp.l Frame.Capacity(a3), d2
	bhi.w fail
capacityReady
	move.l pkg.Context.Pc(a2), d3
	add.l d0, d3
	bcs.w fail
	movea.l pkg.Context.Package(a2), a1
	move.l d3, d4
	tst.l d0
	beq.w checked
	subq.l #1, d4
	cmp.l pkg.Header.MaxAddress(a1), d4
	bhi.w fail
checked
	move.l d3, pkg.Context.Pc(a2)
	move.l d2, Frame.Used(a3)
	cmpi.w #1, pkg.Context.Pass(a2)
	beq.w ok
	movea.l Frame.Output(a3), a1
	adda.l d1, a1
	tst.l d0
	beq.w ok
copy
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copy
ok
	moveq #0, d0
	bra.w done
fail
	moveq #1, d0
done
	movem.l (sp)+, d1-d4/a0-a3
	tst.l d0
	rts
	.bend  ; emit

	.endsection
	.endmodule

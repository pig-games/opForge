; Shared relative-data patches: no package program, instruction grammar or I/O.
	.priv
DeltaCase	.struct
Program	.long ?
Span	.long ?
Target	.long ?
Base	.long ?
Bias	.long ?
Expected	.long ?  ; expected wire bytes, left aligned for a word
	.endstruct
DELTA_CASE_BYTES = DeltaCase.Expected+4
DELTA_CASE_COUNT = 14
DELTA_BASE_SYMBOL = 1
FAIL_DATA_PATCH = 39
FAIL_DATA_LAYOUT = 40
FAIL_SCALAR_LAYOUT = 41
	.section code, kind=code
; Scratch output only: the live Rust artifact remains the ordinary 44 bytes.
deltaChecks	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l #FAIL_DATA_PATCH, ReturnCode
	lea Link, a0
	move.l #1, image.LinkFrame.BlockId(a0)
	move.l #FIRST_BYTES, image.LinkFrame.Capacity(a0)
	move.l #DeltaOutput, image.LinkFrame.Output(a0)
	move.l #2, ResolveId
	movea.l image.LinkFrame.Image(a0), a2
	adda.l image.Header.Relocs(a2), a2
	move.l #2, image.Reloc.Target(a2)
	lea DeltaCases, a3
	moveq #DELTA_CASE_COUNT-1, d7
next
	move.l DeltaCase.Program(a3), image.Reloc.Program(a2)
	move.l DeltaCase.Span(a3), image.Reloc.Span(a2)
	move.l DeltaCase.Target(a3), Target
	move.l DeltaCase.Base(a3), image.LinkFrame.Base(a0)
	move.l DeltaCase.Bias(a3), image.Reloc.Arguments(a2)
	jsr image.link
	bne.w bad
	move.l DeltaCase.Expected(a3), d0
	cmpi.l #4, image.Reloc.Span(a2)
	beq.w longValue
	cmpi.l #1, image.Reloc.Span(a2)
	beq.w byteValue
	swap d0
	cmp.w DeltaOutput+2, d0
	bne.w bad
	bra.w valueReady
byteValue
	lsr.l #8, d0
	lsr.l #8, d0
	lsr.l #8, d0
	cmp.b DeltaOutput+2, d0
	bne.w bad
	bra.w valueReady
longValue
	cmp.l DeltaOutput+2, d0
	bne.w bad
valueReady
	adda.w #DELTA_CASE_BYTES, a3
	dbra d7, next
	; Fixed signed fields reject out-of-range values before output mutation.
	move.l #Guard, image.LinkFrame.Output(a0)
	move.l #image.PROGRAM_SIGNED_DELTA_BE, image.Reloc.Program(a2)
	move.l #$1000, image.LinkFrame.Base(a0)
	move.l #$1080, Target
	move.l #1, image.Reloc.Span(a2)
	bsr.w rejectLink
	bne.w bad
	move.l #$9000, Target
	move.l #2, image.Reloc.Span(a2)
	bsr.w rejectLink
	bne.w bad
	move.l #$80001000, Target
	move.l #4, image.Reloc.Span(a2)
	bsr.w rejectLink
	bne.w bad
	; Shared data truncates word/long values; byte data warnings are unsupported.
	move.l #image.PROGRAM_DATA_DELTA_BE, image.Reloc.Program(a2)
	move.l #$0ff8, Target
	move.l #1, image.Reloc.Span(a2)
	bsr.w rejectLink
	bne.w bad
	move.l #3, image.Reloc.Span(a2)
	bsr.w rejectLink
	bne.w bad
	move.l #4, image.Reloc.Span(a2)
	move.l #$80000001, image.Reloc.Target(a2)
	bsr.w rejectLink
	bne.w bad
	bsr.w deltaLayoutChecks
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	; Restore the shared image before the registry ownership tests use it.
	move.l #image.PROGRAM_ABSOLUTE, image.Reloc.Program(a2)
	move.l #4, image.Reloc.Span(a2)
	move.l #17, image.Reloc.Target(a2)
	clr.l image.Reloc.Arguments(a2)
	move.l #17, ResolveId
	move.l #$2000, Target
	clr.l image.LinkFrame.Base(a0)
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; deltaChecks

; Two static rows model equal PCs before/after a zero-padding alignment. Only
; pending/relocation arrays are owned allocations; the fixture rows stay borrowed.
deltaLayoutChecks	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l #FAIL_DATA_LAYOUT, ReturnCode
	lea DeltaState, a0
	move.l #2, layout.State.Count(a0)
	move.l #2, layout.State.Names(a0)
	move.l #DeltaRows, layout.State.Geometry+memory.Block.Pointer(a0)
	move.l #DeltaInputs, layout.State.Inputs+memory.Block.Pointer(a0)
	move.l #DeltaLabels, layout.State.Labels+memory.Block.Pointer(a0)
	move.l #DeltaOwners, layout.State.Owners+memory.Block.Pointer(a0)
	lea DeltaFrame, a1
	move.l #DeltaContext, assembly.Frame.Context(a1)
	move.l #DeltaPayload, assembly.Frame.Output(a1)
	move.l #8, assembly.Frame.Used(a1)
	move.l #4, assembly.Frame.RecordOffset(a1)
	lea DeltaContext, a3
	move.l #DeltaValues, package.Context.Values(a3)
	move.w #2, package.Context.Pass(a3)
	move.w #1, package.Context.CurrentSection(a3)
	lea DeltaDescriptor, a2
	jsr layout.dataDelta
	bne.w bad
	jsr layout.finish
	bne.w bad
	cmpi.l #image.RELOC_BYTES, DeltaState+layout.State.Relocs+memory.Block.Used
	bne.w bad
	movea.l DeltaState+layout.State.Relocs+memory.Block.Pointer, a3
	cmpi.l #image.PROGRAM_DATA_DELTA_BE, image.Reloc.Program(a3)
	bne.w bad
	cmpi.l #1, image.Reloc.Block(a3)
	bne.w bad
	cmpi.l #2, image.Reloc.Target(a3)
	bne.w bad
	cmpi.l #2, image.Reloc.Offset(a3)
	bne.w bad
	cmpi.l #2, image.Reloc.Arguments(a3)
	bne.w bad
	clr.l DeltaState+layout.State.Pending+memory.Block.Used
	clr.l DeltaState+layout.State.Relocs+memory.Block.Used
	; A base at the same original PC but before the alignment cannot cancel.
	move.l #4, DeltaLabels+4*DELTA_BASE_SYMBOL
	move.l #104, DeltaValues+exprvm.VALUE_BYTES*DELTA_BASE_SYMBOL+exprvm.Value.Low
	move.l #104, layout.Delta.BaseValue(a2)
	move.l #104, layout.Delta.FieldPc(a2)
	move.l #8, assembly.Frame.RecordOffset(a1)
	jsr layout.dataDelta
	bne.w bad
	jsr layout.finish
	tst.l d0
	beq.w bad
	tst.l DeltaState+layout.State.Relocs+memory.Block.Used
	bne.w bad
	clr.l DeltaState+layout.State.Pending+memory.Block.Used
	move.l #1, layout.Delta.Span(a2)
	jsr layout.dataDelta
	tst.l d0
	beq.w bad
	move.l #2, layout.Delta.Span(a2)
	move.l #2, layout.Delta.LittleEndian(a2)
	jsr layout.dataDelta
	tst.l d0
	beq.w bad
	clr.l layout.Delta.LittleEndian(a2)
	move.l #1, layout.Delta.BaseSection(a2)
	jsr layout.dataDelta
	tst.l d0
	beq.w bad
	clr.l layout.Delta.BaseSection(a2)
	move.l #1, layout.Delta.TargetSection(a2)
	jsr layout.dataDelta
	tst.l d0
	beq.w bad
	tst.l DeltaState+layout.State.Pending+memory.Block.Used
	bne.w bad
	bsr.w scalarLayoutChecks
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	move.l d0, -(sp)
	lea DeltaState+layout.State.Pending, a0
	jsr memory.release
	lea DeltaState+layout.State.Relocs, a0
	jsr memory.release
	move.l (sp)+, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; deltaLayoutChecks

; A proof-only scalar pair emits no recipe, deduplicates repeated evaluations,
; and rejects an equal-PC anchor moved to the next alignment fragment.
scalarLayoutChecks	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l #FAIL_SCALAR_LAYOUT, ReturnCode
	lea DeltaState, a0
	lea DeltaFrame, a1
	lea DeltaDescriptor, a2
	move.l #4, assembly.Frame.RecordOffset(a1)
	clr.l layout.Delta.TargetSection(a2)
	move.l #100, layout.Delta.BaseValue(a2)
	move.l #102, layout.Delta.FieldPc(a2)
	move.l #4, DeltaLabels
	clr.l DeltaLabels+4*DELTA_BASE_SYMBOL
	move.l #100, DeltaValues+exprvm.VALUE_BYTES*DELTA_BASE_SYMBOL+exprvm.Value.Low
	jsr layout.scalarDifference
	bne.w bad
	move.l DeltaState+layout.State.Pending+memory.Block.Used, d7
	jsr layout.scalarDifference
	bne.w bad
	cmp.l DeltaState+layout.State.Pending+memory.Block.Used, d7
	bne.w bad
	jsr layout.finish
	bne.w bad
	tst.l DeltaState+layout.State.Relocs+memory.Block.Used
	bne.w bad
	clr.l DeltaState+layout.State.Pending+memory.Block.Used
	move.l #8, DeltaLabels
	jsr layout.scalarDifference
	bne.w bad
	jsr layout.finish
	tst.l d0
	beq.w bad
	tst.l DeltaState+layout.State.Relocs+memory.Block.Used
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; scalarLayoutChecks
	.endsection
	.section data, kind=data
	.align 4
DeltaCases
	.long image.PROGRAM_DATA_DELTA_BE, 2, $101c, $1000, 2, $00200000
	.long image.PROGRAM_DATA_DELTA_LE, 2, $101c, $1000, 2, $20000000
	.long image.PROGRAM_DATA_DELTA_BE, 2, $0ffa, $1000, 2, $fffe0000
	.long image.PROGRAM_DATA_DELTA_LE, 2, $0ffa, $1000, 2, $feff0000
	.long image.PROGRAM_DATA_DELTA_BE, 4, $0ff8, $1000, 2, $fffffffc
	.long image.PROGRAM_DATA_DELTA_LE, 4, $0ff8, $1000, 2, $fcffffff
	.long image.PROGRAM_DATA_DELTA_BE, 2, $201c, $2000, 2, $00200000
	.long image.PROGRAM_SIGNED_DELTA_BE, 1, $0ffa, $1000, 2, $fe000000
	.long image.PROGRAM_SIGNED_DELTA_LE, 1, $0ffa, $1000, 2, $fe000000
	.long image.PROGRAM_SIGNED_DELTA_BE, 2, $0ffa, $1000, 2, $fffe0000
	.long image.PROGRAM_SIGNED_DELTA_LE, 2, $0ffa, $1000, 2, $feff0000
	.long image.PROGRAM_SIGNED_DELTA_BE, 4, $0ff8, $1000, 2, $fffffffc
	.long image.PROGRAM_SIGNED_DELTA_LE, 4, $0ff8, $1000, 2, $fcffffff
	.long image.PROGRAM_SIGNED_DELTA_BE, 2, $101c, $1000, 2, $00200000
DeltaRows
	.long 0, 100, 104, 0, 4, 0, 8
	.long 0, 104, 108, 4, 4, 8, 16
DeltaInputs
	.long 1, 0, 4, 0, 0, image.KIND_DATA, 1, 4
	.long 2, 0, 4, 0, 0, image.KIND_DATA, 8, 4
DeltaLabels	.long 8, 0
DeltaOwners	.word 0, 0
DeltaValues	.long 104, 0, 100, 0
DeltaDescriptor
	.long 0, 0, 104, 1, 0, 100, 102, 2, 0
DeltaPayload	.long 0, 0
	.endsection
	.section bss, kind=bss
	.align 4
DeltaState	.res byte, layout.STATE_BYTES
DeltaFrame	.res byte, assembly.FRAME_BYTES
DeltaContext	.res byte, package.CONTEXT_BYTES
DeltaOutput	.res byte, FIRST_BYTES
	.endsection

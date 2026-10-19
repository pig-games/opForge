; Native shared-section compiler and moved immutable recipe replay proof.
; @opforge-owner: tests.amigaos.module_shared_probe
	.module tests.amigaos.module_shared_probe
	.cpu 68020
	.use experimental.amigaos.binary_module_compile as compiler
	.use experimental.amigaos.binary_module_request as request
	.use experimental.amigaos.binary_module_image as image
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_encoding as encoding
	.include "module_shared_fixture.i"
	.include "memory_telemetry.i"
	.priv
EXEC_BASE=4
EXEC_OPEN_LIBRARY=-552
EXEC_CLOSE_LIBRARY=-414
DOS_OPEN=-30
DOS_CLOSE=-36
DOS_WRITE=-48
MODE_NEWFILE=1006
Metric	.struct
Magic	.long ?
Compiles	.long ?
Relocations	.long ?
Fragments	.long ?
RetiredPointer	.long ?
	.endstruct
METRIC_BYTES=Metric.RetiredPointer+4
GUARD_VALUE=$a5a5a5a5
REJECTION_COUNT=9
.ifdef OPFORGE_MODULE_SHARED_REJECTIONS
OUTPUT_BYTES=REJECTION_COUNT
.else
OUTPUT_BYTES=RESULT_BYTES
.endif
RejectedInput	.struct
Source	.long ?
Bytes	.long ?
.endstruct
REJECTED_INPUT_BYTES=RejectedInput.Bytes+4
	.section entry, kind=code
	.pub
; Native entry; preserves callee-saved registers. Zero requires completed files.
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	move.l #20, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l EXEC_BASE.w, a6
	jsr EXEC_OPEN_LIBRARY(a6)
	move.l d0, DosBase
	beq.w cleanup
	.MEMORY_PHASE #0
	.MEMORY_CLOCK DosBase, #0
.ifdef OPFORGE_MODULE_SHARED_REJECTIONS
	jsr rejectedSources
	.MEMORY_PHASE #1
	.MEMORY_CLOCK DosBase, #1
.else
	jsr cases
.endif
	bne.w closeLibrary
	.MEMORY_CLOCK DosBase, #2
	movea.l DosBase, a6
	move.l #OutputName, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, Handle
	beq.w closeLibrary
	move.l d0, d1
	move.l #Result, d2
	move.l #OUTPUT_BYTES, d3
	jsr DOS_WRITE(a6)
	cmpi.l #OUTPUT_BYTES, d0
	bne.w closeFile
	clr.l ReturnCode
closeFile
	move.l Handle, d1
	jsr DOS_CLOSE(a6)
	tst.l d0
	beq.w metricsFailed
	tst.l ReturnCode
	bne.w closeLibrary
	move.l #MetricsName, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, Handle
	beq.w metricsFailed
	move.l d0, d1
	move.l #Metrics, d2
	move.l #METRIC_BYTES, d3
	jsr DOS_WRITE(a6)
	cmpi.l #METRIC_BYTES, d0
	beq.w closeMetrics
	move.l #20, ReturnCode
closeMetrics
	move.l Handle, d1
	jsr DOS_CLOSE(a6)
	tst.l d0
	bne.w closeLibrary
metricsFailed
	move.l #20, ReturnCode
closeLibrary
	jsr releaseOwners
	.MEMORY_PHASE #3
	.MEMORY_SAVE DosBase
	movea.l DosBase, a1
	movea.l EXEC_BASE.w, a6
	jsr EXEC_CLOSE_LIBRARY(a6)
cleanup
	jsr releaseOwners
	move.l ReturnCode, d0
	movem.l (sp)+, d2-d7/a2-a6
	tst.l d0
	rts
	.bend  ; start
	.endsection
	.priv
	.section code, kind=code
cases	.block
	move.l #21, ReturnCode
	lea Request, a0
	lea CompileContext, a1
	lea compileOnce, a2
	moveq #1, d1
	jsr request.acquire
	bne.w bad
	moveq #2, d1
	lea CompileContext, a1
	lea compileOnce, a2
	jsr request.acquire
	bne.w bad
	cmpi.l #1, CompileCount
	bne.w bad
	.MEMORY_PHASE #1
	.MEMORY_CLOCK DosBase, #1
	movea.l Request+request.State.Image+memory.Block.Pointer, a0
	cmpi.l #3, image.Header.BlockCount(a0)
	bne.w bad
	move.l image.Header.RelocCount(a0), Metrics+Metric.Relocations
	beq.w bad
	move.l #22, ReturnCode
	bsr.w moveImage
	bne.w bad
	move.l Moved+memory.Block.Pointer, Link+image.LinkFrame.Image
	move.l Moved+memory.Block.Used, Link+image.LinkFrame.ImageBytes
	move.l #23, ReturnCode
	bsr.w rejectedRecipes
	bne.w bad
	move.l #24, ReturnCode
	lea Result, a3
	lea Selections, a4
	moveq #2, d7
next
	lea Link, a0
	move.l (a4)+, image.LinkFrame.BlockId(a0)
	move.l (a4)+, image.LinkFrame.Base(a0)
	move.l a3, image.LinkFrame.Output(a0)
	move.l (a4)+, image.LinkFrame.Capacity(a0)
	jsr image.link
	bne.w bad
	move.l image.LinkFrame.Used(a0), d0
	cmp.l image.LinkFrame.Capacity(a0), d0
	bne.w bad
	adda.l d0, a3
	dbra d7, next
	move.l CompileCount, Metrics+Metric.Compiles
	move.l #3, Metrics+Metric.Fragments
	bsr.w releaseOwners
	move.l Request+request.State.Image+memory.Block.Pointer, Metrics+Metric.RetiredPointer
	bne.w bad
	tst.l Moved+memory.Block.Pointer
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; cases
.ifdef OPFORGE_MODULE_SHARED_REJECTIONS
; Ordinary Rust assembly accepts each expression. Independent fragment storage
; must reject layout observations for which it has no saved evaluation recipe.
rejectedSources	.block
	move.l #31, ReturnCode
	lea RejectedInputs, a3
	lea Result, a4
	moveq #REJECTION_COUNT-1, d7
next
	lea CompileContext, a0
	move.l RejectedInput.Source(a3), compiler.Context.Source(a0)
	move.l RejectedInput.Bytes(a3), compiler.Context.Bytes(a0)
	lea RejectedOwner, a1
	jsr compiler.compile
	tst.l d0
	beq.w bad
	move.l memory.Block.Pointer(a1), d0
	or.l memory.Block.Capacity(a1), d0
	or.l memory.Block.Used(a1), d0
	bne.w bad
	move.b #1, (a4)+
	adda.w #REJECTED_INPUT_BYTES, a3
	dbra d7, next
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; rejectedSources
.endif
; Retire original storage, source and private request before any linked replay.
moveImage	.block
	lea Moved, a0
	move.l Request+request.State.Image+memory.Block.Used, d0
	jsr memory.reserveExact
	bne.w done
	move.l Request+request.State.Image+memory.Block.Used, d1
	move.l d1, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	movea.l Request+request.State.Image+memory.Block.Pointer, a0
copy
	move.b (a0), (a1)+
	move.b #$a5, (a0)+
	subq.l #1, d1
	bne.w copy
	lea Source, a0
	move.l #SourceEnd-Source, d1
poison
	move.b #$cc, (a0)+
	subq.l #1, d1
	bne.w poison
	clr.l CompileContext+compiler.Context.Source
	clr.l CompileContext+compiler.Context.Bytes
	lea Request, a0
	jsr request.release
done
	tst.l d0
	rts
	.bend  ; moveImage
; Failure checks preflight against an all-guard block-sized destination.
rejectedRecipes	.block
	lea Link, a0
	move.l #Guard, image.LinkFrame.Output(a0)
	move.l #FIRST_BYTES, image.LinkFrame.Capacity(a0)
	move.l #1, image.LinkFrame.BlockId(a0)
	move.l #$1000, image.LinkFrame.Base(a0)
	clr.l image.LinkFrame.Patch(a0)
	bsr.w reject
	bne.w bad
	move.l #patch, image.LinkFrame.Patch(a0)
	move.l #1, FarTarget
	bsr.w reject
	bne.w bad
	clr.l FarTarget
	; Locate an opaque recipe without interpreting any instruction/program ID.
	movea.l Moved+memory.Block.Pointer, a2
	move.l image.Header.RelocCount(a2), d6
	adda.l image.Header.Relocs(a2), a2
scan
	cmpi.l #image.PROGRAM_ABSOLUTE, image.Reloc.Program(a2)
	bne.w found
	adda.l #image.RELOC_BYTES, a2
	subq.l #1, d6
	bne.w scan
	bra.w bad
found
	move.l image.Reloc.Block(a2), image.LinkFrame.BlockId(a0)
	move.l image.Reloc.Program(a2), d5
	move.l #$fffffffe, image.Reloc.Program(a2)
	bsr.w reject
	move.l d5, image.Reloc.Program(a2)
	tst.l d0
	bne.w bad
	move.l image.Reloc.Span(a2), d5
	clr.l image.Reloc.Span(a2)
	bsr.w reject
	move.l d5, image.Reloc.Span(a2)
	tst.l d0
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; rejectedRecipes
reject	.block
	lea Guard, a1
	move.l #RESULT_BYTES, d0
fill
	move.b #$a5, (a1)+
	subq.l #1, d0
	bne.w fill
	jsr image.link
	tst.l d0
	beq.w bad
	tst.l image.LinkFrame.Used(a0)
	bne.w bad
	lea Guard, a1
	move.l #RESULT_BYTES, d1
check
	cmpi.b #$a5, (a1)+
	bne.w bad
	subq.l #1, d1
	bne.w check
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; reject
; Pure opaque fragment-ID resolver; assigned bases differ from original layout.
; D1=id -> D0=status,D1=address; preserves all other registers.
resolve	.block
	cmpi.l #1, d1
	blo.w bad
	cmpi.l #3, d1
	bhi.w bad
	subq.l #1, d1
	lsl.l #2, d1
	move.l a1, -(sp)
	lea Bases, a1
	adda.l d1, a1
	move.l (a1), d1
	movea.l (sp)+, a1
	tst.l FarTarget
	beq.w good
	move.l #$100000, d1
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; resolve
; Forward the saved opaque recipe to its owning package VM; no opcode logic.
patch	.block
	move.l a0, -(sp)
	movea.l compiler.Context.Package(a0), a0
	jsr encoding.patchPosition
	movea.l (sp)+, a0
	tst.l d0
	rts
	.bend  ; patch
compileOnce	.block
	addq.l #1, CompileCount
	jsr compiler.compile
	rts
	.bend  ; compileOnce
releaseOwners	.block
.ifdef OPFORGE_MODULE_SHARED_REJECTIONS
	lea RejectedOwner, a0
	jsr memory.release
.endif
	lea Request, a0
	jsr request.release
	lea Moved, a0
	jsr memory.release
	rts
	.bend  ; releaseOwners
	.endsection
	.section data, kind=data
	.align 4
CompileContext
	.long Package, Source, SourceEnd-Source, 0, 0, 0, 0, 0, 0
Link
	.long 0, 0, 1, $1000, Guard, FIRST_BYTES, 0, resolve, CompileContext, patch
Bases
	.long $1000, $1400, $1800
Selections
	.long 3, $1800, THIRD_BYTES
	.long 1, $1000, FIRST_BYTES
	.long 2, $1400, SECOND_BYTES
Metrics
	.long $4d534d31, 0, 0, 0, 0
.ifdef OPFORGE_MODULE_SHARED_REJECTIONS
RejectedInputs
	.long CancelledData, CancelledDataEnd-CancelledData
	.long CancelledInstruction, CancelledInstructionEnd-CancelledInstruction
	.long CancelledTyped, CancelledTypedEnd-CancelledTyped
	.long CancelledAlias, CancelledAliasEnd-CancelledAlias
	.long CancelledMixed, CancelledMixedEnd-CancelledMixed
	.long CancelledComparison, CancelledComparisonEnd-CancelledComparison
	.long CancelledConditional, CancelledConditionalEnd-CancelledConditional
	.long CancelledPc, CancelledPcEnd-CancelledPc
	.long NumericBranch, NumericBranchEnd-NumericBranch
CancelledData
	.incbin "module_shared_bad_data.txt"
CancelledDataEnd
CancelledInstruction
	.incbin "module_shared_bad_instruction.txt"
CancelledInstructionEnd
CancelledTyped
	.incbin "module_shared_bad_typed.txt"
CancelledTypedEnd
CancelledAlias
	.incbin "module_shared_bad_alias.txt"
CancelledAliasEnd
CancelledMixed
	.incbin "module_shared_bad_mixed.txt"
CancelledMixedEnd
CancelledComparison
	.incbin "module_shared_bad_comparison.txt"
CancelledComparisonEnd
CancelledConditional
	.incbin "module_shared_bad_conditional.txt"
CancelledConditionalEnd
CancelledPc
	.incbin "module_shared_bad_pc.txt"
CancelledPcEnd
NumericBranch
	.incbin "module_shared_bad_numeric_branch.txt"
NumericBranchEnd
	.align 4
.endif
FarTarget	.long 0
Package
	.incbin "module_shared_package.bin"
	.align 4
Source
	.incbin "module_shared_source.txt"
SourceEnd
DosName	.byte "dos.library", 0
OutputName	.byte "output.bin", 0
MetricsName	.byte "metrics.bin", 0
	.endsection
	.section bss, kind=bss
	.align 4
Request	.res byte, request.STATE_BYTES
Moved	.res byte, memory.Block.Used+4
.ifdef OPFORGE_MODULE_SHARED_REJECTIONS
RejectedOwner	.res byte, memory.Block.Used+4
.endif
CompileCount	.res long, 1
Guard	.res byte, RESULT_BYTES
Result	.res byte, RESULT_BYTES
DosBase	.res long, 1
Handle	.res long, 1
ReturnCode	.res long, 1
	.endsection
	.output "module-shared-probe.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule

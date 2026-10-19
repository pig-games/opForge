; Actual native frontend/package-VM source compilation and immutable reuse.
; @opforge-owner: tests.amigaos.module_compile_probe
	.module tests.amigaos.module_compile_probe
	.cpu 68020
	.use experimental.amigaos.binary_module_compile as compiler
	.use experimental.amigaos.binary_module_request as request
	.use experimental.amigaos.binary_module_image as image
	.use experimental.amigaos.binary_memory as memory
	.include "module_compile_fixture.i"
	.include "memory_telemetry.i"
	.priv
EXEC_BASE = 4
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
Metric	.struct
Magic	.long ?
Compiles	.long ?
Reserved	.long ?
Output	.long ?
Image	.long ?
Blocks	.long ?
RetiredPointer	.long ?
	.endstruct
METRIC_BYTES = Metric.RetiredPointer+4
	.section entry, kind=code
	.pub
; D0=0 only after byte checks, owned-image retirement and complete file close.
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
	.ifdef OPFORGE_MODULE_COMPILE_REJECTIONS
	jsr rejectionCases
	bne.w closeLibrary
	.endif
	jsr cases
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
	move.l #RESULT_BYTES, d3
	jsr DOS_WRITE(a6)
	cmpi.l #RESULT_BYTES, d0
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
	.MEMORY_PHASE #3
	.MEMORY_SAVE DosBase
	movea.l DosBase, a1
	movea.l EXEC_BASE.w, a6
	jsr EXEC_CLOSE_LIBRARY(a6)
cleanup
	lea Request, a0
	jsr request.release
	move.l ReturnCode, d0
	movem.l (sp)+, d2-d7/a2-a6
	tst.l d0
	rts
	.bend  ; start
	.endsection
	.priv
	.section code, kind=code
cases	.block
	; Ordinal1 is a real leaf request; the compiler publishes all three leaves.
	lea Request, a0
	lea CompileContext, a1
	lea compileOnce, a2
	moveq #1, d1
	jsr request.acquire
	bne.w bad
	cmpi.l #FIRST_BYTES, d2
	bne.w bad
	cmpi.l #request.READY, Request+request.State.Phase
	bne.w bad
	cmpi.l #1, CompileCount
	bne.w bad
	.MEMORY_PHASE #1
	.MEMORY_CLOCK DosBase, #1
	movea.l Request+request.State.Image+memory.Block.Pointer, a0
	cmpi.l #3, image.Header.BlockCount(a0)
	bne.w bad
	movea.l a0, a1
	adda.l image.Header.Blocks(a0), a1
	move.l image.Block.Id(a1), FirstId
	move.l image.BLOCK_BYTES+image.Block.Id(a1), SecondId
	move.l 2*image.BLOCK_BYTES+image.Block.Id(a1), d1
	jsr image.findValidated
	bne.w bad
	cmpi.l #1, d1
	bne.w bad
	cmpi.b #$99, (a1)
	bne.w bad
	move.l FirstId, d1
	lea Result, a3
	move.l #FIRST_BYTES, d3
	bsr.w selectCopy
	bne.w bad
	move.l SecondId, d1
	move.l #SECOND_BYTES, d3
	bsr.w selectCopy
	bne.w bad
	move.l FirstId, d1
	move.l #FIRST_BYTES, d3
	bsr.w selectCopy
	bne.w bad
	move.l SecondId, d1
	move.l #SECOND_BYTES, d3
	bsr.w selectCopy
	bne.w bad
	cmpi.l #1, CompileCount
	bne.w bad
	.MEMORY_PHASE #2
	move.l CompileCount, Metrics+Metric.Compiles
	clr.l Metrics+Metric.Reserved
	movea.l Request+request.State.Image+memory.Block.Pointer, a0
	move.l image.Header.Bytes(a0), d0
	move.l d0, Metrics+Metric.Image
	sub.l image.Header.Payload(a0), d0
	move.l d0, Metrics+Metric.Output
	move.l image.Header.BlockCount(a0), Metrics+Metric.Blocks
	lea Request, a0
	jsr request.release
	bne.w bad
	jsr request.release
	bne.w bad
	move.l request.State.Image+memory.Block.Pointer(a0), Metrics+Metric.RetiredPointer
	tst.l Metrics+Metric.RetiredPointer
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; cases
	.ifdef OPFORGE_MODULE_COMPILE_REJECTIONS
; Separate correctness image: cross-block absolute aliases and relative branches
; fail without retaining an owner. These checks are excluded from timing images.
rejectionCases	.block
	lea BadAbsoluteContext, a1
	bsr.w rejectSource
	bne.w done
	lea BadRelativeContext, a1
	bsr.w rejectSource
done
	rts
	.bend  ; rejectionCases
rejectSource	.block
	lea NegativeRequest, a0
	lea compiler.compile, a2
	moveq #1, d1
	jsr request.acquire
	cmpi.l #request.STATUS_FAILED, d0
	bne.w bad
	cmpi.l #request.FAILED, request.State.Phase(a0)
	bne.w bad
	tst.l request.State.Image+memory.Block.Pointer(a0)
	bne.w bad
	tst.l request.State.Image+memory.Block.Capacity(a0)
	bne.w bad
	tst.l request.State.Image+memory.Block.Used(a0)
	bne.w bad
	jsr request.release
	rts
bad
	jsr request.release
	moveq #1, d0
	rts
	.bend  ; rejectSource
	.endif
; Request callback retains no private assembly/source borrows after return.
compileOnce	.block
	addq.l #1, CompileCount
	jmp compiler.compile
	.bend  ; compileOnce
; D1=native block ID,D3=expected extent,A3=append destination.
selectCopy	.block
	lea Request, a0
	lea CompileContext, a1
	lea compileOnce, a2
	jsr request.acquire
	bne.w bad
	cmp.l d3, d2
	bne.w bad
longs
	cmpi.l #4, d2
	blo.w tail
	move.l (a2)+, (a3)+
	subq.l #4, d2
	bra.w longs
tail
	tst.l d2
	beq.w done
	move.b (a2)+, (a3)+
	subq.l #1, d2
	bra.w tail
done
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; selectCopy
	.endsection
	.section data, kind=data
	.align 4
CompileContext
	.long Package, Source, SourceEnd-Source
	.ifdef OPFORGE_MODULE_COMPILE_REJECTIONS
BadAbsoluteContext
	.long Package, BadAbsolute, BadAbsoluteEnd-BadAbsolute
BadRelativeContext
	.long Package, BadRelative, BadRelativeEnd-BadRelative
BadAbsolute
	.incbin "module_compile_bad_absolute.asm"
BadAbsoluteEnd
BadRelative
	.incbin "module_compile_bad_relative.asm"
BadRelativeEnd
	.align 4
	.endif
Metrics
	.long $4d434d31, 0, 0, 0, 0, 0, 0
Package
	.incbin "module_compile_package.bin"
	.align 4
Source
	.incbin "module_compile_source.asm"
SourceEnd
DosName	.byte "dos.library", 0
OutputName	.byte "Work:output.bin", 0
MetricsName	.byte "Work:metrics.bin", 0
	.endsection
	.section bss, kind=bss
	.align 4
Request	.res byte, request.STATE_BYTES
	.ifdef OPFORGE_MODULE_COMPILE_REJECTIONS
NegativeRequest	.res byte, request.STATE_BYTES
	.endif
CompileCount	.res long, 1
FirstId	.res long, 1
SecondId	.res long, 1
Result	.res byte, RESULT_BYTES
DosBase	.res long, 1
Handle	.res long, 1
ReturnCode	.res long, 1
	.endsection
	.output "module-compile-probe.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule

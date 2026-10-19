; Actual native frontend/package-VM source compilation and immutable reuse.
; @opforge-owner: tests.amigaos.module_compile_probe
	.module tests.amigaos.module_compile_probe
	.cpu 68020
	.use experimental.amigaos.binary_module_compile as compiler
	.use experimental.amigaos.binary_module_request as request
	.use experimental.amigaos.binary_module_image as image
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_module_interface as semantics
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_struct_values as structs
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
FAIL_PUBLICATION = 21
FAIL_SELECTION = 22
FAIL_MOVE = 23
FAIL_SEMANTICS = 24
FAIL_VIEW = 25
FAIL_ANSWER = 26
FAIL_FRAME = 27
FAIL_MEMBER = 28
FAIL_LIST = 29
Metric	.struct
Magic	.long ?
Compiles	.long ?
Semantics	.long ?
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
	move.l #FAIL_SELECTION, ReturnCode
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
	movea.l Request+request.State.Image+memory.Block.Pointer, a0
	move.l image.Header.Bytes(a0), d0
	move.l d0, Metrics+Metric.Image
	move.l image.Header.SemanticsBytes(a0), Metrics+Metric.Semantics
	move.l #FIRST_BYTES+SECOND_BYTES+1, Metrics+Metric.Output
	move.l image.Header.BlockCount(a0), Metrics+Metric.Blocks
	move.l #FAIL_MOVE, ReturnCode
	bsr.w moveImage
	bne.w bad
	move.l #FAIL_SEMANTICS, ReturnCode
	bsr.w consumeSemantics
	bne.w bad
	bsr.w releaseOwners
	bne.w bad
	move.l Request+request.State.Image+memory.Block.Pointer, Metrics+Metric.RetiredPointer
	tst.l Metrics+Metric.RetiredPointer
	bne.w bad
	tst.l Moved+memory.Block.Pointer
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; cases
; Relocate every owned byte, then invalidate all original source/image borrows.
moveImage	.block
	move.l Request+request.State.Image+memory.Block.Used, d0
	lea Moved, a0
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
	bne.w done
	jsr request.release
done
	tst.l d0
	rts
	.bend  ; moveImage

; Only the moved immutable image and copied numeric IDs supply this consumer.
consumeSemantics	.block
	move.l #FAIL_VIEW, ReturnCode
	movea.l Moved+memory.Block.Pointer, a0
	move.l Moved+memory.Block.Used, d0
	subq.l #1, d0
	jsr image.validate
	tst.l d0
	beq.w bad
	move.l Moved+memory.Block.Used, d0
	jsr image.semanticView
	bne.w bad
	move.l a1, SemanticBase
	move.l d1, SemanticBytes
	movea.l a1, a0
	move.l d1, d0
	jsr semantics.validate
	bne.w bad
	movea.l SemanticBase, a6
	movea.l a6, a0
	adda.l semantics.Header.Arena(a6), a0
	move.l a0, Borrowed+values.Owner.Arena+memory.Block.Pointer
	move.l semantics.Header.ArenaBytes(a6), Borrowed+values.Owner.Arena+memory.Block.Capacity
	move.l semantics.Header.ArenaBytes(a6), Borrowed+values.Owner.Arena+memory.Block.Used
	move.l #FAIL_ANSWER, ReturnCode
	lea AnswerName, a5
	moveq #AnswerNameEnd-AnswerName, d5
	bsr.w exportName
	bne.w bad
	cmpi.l #values.SCALAR, semantics.Export.Kind(a1)
	bne.w bad
	tst.l semantics.Export.High(a1)
	bne.w bad
	tst.l semantics.Export.Provenance(a1)
	bne.w bad
	move.l semantics.Export.Low(a1), d0
	move.b d0, Result+CODE_RESULT_BYTES
	move.l #FAIL_FRAME, ReturnCode
	lea FrameName, a5
	moveq #FrameNameEnd-FrameName, d5
	bsr.w exportName
	bne.w bad
	cmpi.l #values.STRUCT_TYPE, semantics.Export.Kind(a1)
	bne.w bad
	bsr.w corruptSemanticRow
	bne.w bad
	move.l semantics.Export.Low(a1), FrameOffset
	move.l FrameOffset, d1
	lea Borrowed, a0
	jsr values.view
	bne.w bad
	cmpi.l #5, values.StructRecord.Size(a1)
	bne.w bad
	move.l #FAIL_MEMBER, ReturnCode
	move.l semantics.Header.Members(a6), d4
	move.l semantics.Header.MemberCount(a6), d7
	moveq #semantics.MEMBER_BYTES, d6
	lea ValueName, a5
	moveq #ValueNameEnd-ValueName, d5
	bsr.w nameId
	bne.w bad
	movea.l a6, a0
	move.l SemanticBytes, d0
	jsr semantics.findMember
	bne.w bad
	move.l semantics.Member.Id(a1), d1
	move.l FrameOffset, d0
	lea Borrowed, a0
	jsr structs.member
	bne.w bad
	move.b d2, Result+CODE_RESULT_BYTES+1
	move.l #FAIL_LIST, ReturnCode
	lea ChoicesName, a5
	moveq #ChoicesNameEnd-ChoicesName, d5
	bsr.w exportName
	bne.w bad
	move.l semantics.Export.Low(a1), d1
	lea Borrowed, a0
	moveq #1, d2
	moveq #0, d3
	jsr values.compoundGet
	bne.w bad
	move.b d1, Result+CODE_RESULT_BYTES+2
	movea.l a6, a0
	move.l SemanticBytes, d0
	moveq #-1, d1
	jsr semantics.findExport
	cmpi.l #semantics.NOT_FOUND, d0
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; consumeSemantics

; Deliberate malformed copies must reject, and each field is restored before
; the next operation. No validated-table assumption survives a mutation.
corruptSemanticRow	.block
	movem.l d1-d7/a0-a4, -(sp)
	movea.l a1, a4
	move.l semantics.Export.Kind(a4), d6
	move.l #values.TYPED_LIST, semantics.Export.Kind(a4)
	movea.l a6, a0
	move.l SemanticBytes, d0
	jsr semantics.validate
	move.l d0, d7
	move.l d6, semantics.Export.Kind(a4)
	cmpi.l #semantics.INVALID, d7
	bne.w bad
	move.l semantics.Export.Name(a4), d6
	move.l SemanticBytes, semantics.Export.Name(a4)
	move.l SemanticBytes, d0
	jsr semantics.validate
	move.l d0, d7
	move.l d6, semantics.Export.Name(a4)
	cmpi.l #semantics.INVALID, d7
	bne.w bad
	move.l SemanticBytes, d0
	jsr semantics.validate
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
	.bend  ; corruptSemanticRow

; Name resolution occurs once per consumer key; numeric shared APIs follow it.
; A5=name,D5=length,A6=validated owned semantic base. A1=export on success.
exportName	.block
	move.l semantics.Header.Exports(a6), d4
	move.l semantics.Header.ExportCount(a6), d7
	moveq #semantics.EXPORT_BYTES, d6
	bsr.w nameId
	bne.w done
	movea.l a6, a0
	move.l SemanticBytes, d0
	jsr semantics.findExport
done
	tst.l d0
	rts
	.bend  ; exportName

; Validated export/member tables share Id/Name/NameBytes. D1=numeric ID.
nameId	.block
	movea.l a6, a4
	adda.l d4, a4
next
	tst.l d7
	beq.w bad
	cmp.l semantics.Export.NameBytes(a4), d5
	bne.w advance
	movea.l a6, a0
	adda.l semantics.Export.Name(a4), a0
	movea.l a5, a1
	move.l d5, d0
compare
	move.b (a0)+, d1
	bsr.w foldName
	move.b d1, d2
	move.b (a1)+, d1
	bsr.w foldName
	cmp.b d1, d2
	bne.w advance
	subq.l #1, d0
	bne.w compare
	move.l semantics.Export.Id(a4), d1
	moveq #0, d0
	rts
advance
	adda.l d6, a4
	subq.l #1, d7
	bra.w next
bad
	moveq #1, d0
	rts
	.bend  ; nameId

; Source spelling is preserved; assembler identifiers compare ASCII case folded.
foldName	.block
	cmpi.b #'A', d1
	blo.w done
	cmpi.b #'Z', d1
	bhi.w done
	addi.b #32, d1
done
	rts
	.bend  ; foldName

; Borrowed is a view into Moved, never an independent owner to release.
releaseOwners	.block
	clr.l SemanticBase
	clr.l SemanticBytes
	clr.l FrameOffset
	lea Borrowed, a0
	clr.l values.Owner.Arena+memory.Block.Pointer(a0)
	clr.l values.Owner.Arena+memory.Block.Capacity(a0)
	clr.l values.Owner.Arena+memory.Block.Used(a0)
	lea Moved, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea Request, a0
	jsr request.release
	rts
	.bend  ; releaseOwners
	.ifdef OPFORGE_MODULE_COMPILE_REJECTIONS
; Separate correctness image: cross-block absolute aliases and relative branches
; and unsupported nominal/template exports fail without retaining an owner.
; These checks are excluded from timing images.
rejectionCases	.block
	lea BadAbsoluteContext, a1
	bsr.w rejectSource
	bne.w done
	lea BadRelativeContext, a1
	bsr.w rejectSource
	bne.w done
	lea BadNominalContext, a1
	bsr.w rejectSource
	bne.w done
	lea BadTemplateContext, a1
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
	jsr compiler.compile
	move.l #FAIL_PUBLICATION, ReturnCode
	tst.l d0
	beq.w done
	move.l d0, ReturnCode
done
	tst.l d0
	rts
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
BadNominalContext
	.long Package, BadNominal, BadNominalEnd-BadNominal
BadTemplateContext
	.long Package, BadTemplate, BadTemplateEnd-BadTemplate
BadNominal
	.incbin "module_compile_bad_nominal.asm"
BadNominalEnd
BadTemplate
	.incbin "module_compile_bad_template.asm"
BadTemplateEnd
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
AnswerName	.byte "leaf.Answer"
AnswerNameEnd
FrameName	.byte "leaf.Frame"
FrameNameEnd
ChoicesName	.byte "leaf.Choices"
ChoicesNameEnd
ValueName	.byte "Value"
ValueNameEnd
DosName	.byte "dos.library", 0
OutputName	.byte "output.bin", 0
MetricsName	.byte "metrics.bin", 0
	.endsection
	.section bss, kind=bss
	.align 4
Request	.res byte, request.STATE_BYTES
Moved	.res byte, memory.Block.Used+4
Borrowed	.res byte, values.OWNER_BYTES
SemanticBase	.res long, 1
SemanticBytes	.res long, 1
FrameOffset	.res long, 1
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

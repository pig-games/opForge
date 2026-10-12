; Application diagnostics share retained numeric record provenance.
reportFailure	.block
	.BINDING_DIAGNOSTIC_REPORT DosBase
	.MEMORY_PREPARATION_FAILURE DosBase, InAssembly, frontend.PreparationStage, Front+frontend.Frame.Used, Front+frontend.Frame.NameCount, #PROGRESS_PREPARATION_FAILURE
	.MEMORY_PREPARATION_RECORD DosBase, InAssembly, frontend.PreparationStage, Front+frontend.Frame.Output, Front+frontend.Frame.Capacity, frontend.PreparationInput, #PROGRESS_PREPARATION_FAILURE
	.MEMORY_PREPARATION_FAILURE DosBase, InAssembly, statement_execution.FailureStep, #0, #0, #PROGRESS_STATEMENT_FAILURE
	tst.l InAssembly
	beq.w located
	bsr.w locateFailure
	.MEMORY_PROGRESS DosBase, #PROGRESS_ASSEMBLY_FAILURE, assembly.FailureStage, SourceOrdinal, SourceLine
	.MEMORY_PROGRESS DosBase, #PROGRESS_ASSEMBLY_NAME, assembly.FailureName, #0, #0
	.MEMORY_ASSEMBLY_RECORD DosBase, #PROGRESS_ASSEMBLY_RECORD_HEAD, Work, assembly.Frame.Records, assembly.Frame.RecordOffset, assembly.Frame.RecordBytes
	.MEMORY_PROGRESS_BLOCK DosBase, #PROGRESS_ASSEMBLY_POSITION, assembly.Position, AssemblyPosition.Pass, AssemblyPosition.Sweep, AssemblyPosition.Section
	.MEMORY_PROGRESS_BLOCK DosBase, #PROGRESS_ASSEMBLY_RECORDS, Work, assembly.Frame.RecordOffset, assembly.Frame.RecordBytes, assembly.Frame.Used
	.MEMORY_PROGRESS_BLOCK DosBase, #PROGRESS_ASSEMBLY_SECTIONS, assembly.Position, AssemblyPosition.Mode, AssemblyPosition.Count, AssemblyPosition.Pass
	.MEMORY_PROGRESS_BLOCK DosBase, #PROGRESS_ASSEMBLY_SELECTION, encoding.Selection, SelectionPosition.Priority, SelectionPosition.Recipe, SelectionPosition.Projection
located
	move.l SourceOrdinal, d0
	lea FailureFile, a0
	bsr.w hexField
	move.l SourceLine, d0
	lea FailureLine, a0
	bsr.w hexField
	movea.l DosBase, a6
	jsr DOS_OUTPUT(a6)
	move.l d0, d1
	beq.w done
	move.l d1, d4
	move.l #FailureMessage, d2
	move.l #FailureMessageEnd, d3
	sub.l d2, d3
	jsr DOS_WRITE(a6)
	tst.l PackageStatus
	beq.w afterPackage
	move.l #PackageFailure, d2
	move.l #PackageFailureEnd-PackageFailure, d3
	move.l d4, d1
	jsr DOS_WRITE(a6)
afterPackage
	tst.l InAssembly
	beq.w afterLayout
	lea Work, a0
	cmpi.w #assembly.FAILURE_WHILE_LAYOUT, assembly.Frame.Failure(a0)
	beq.w showWhileLayout
	cmpi.w #assembly.FAILURE_MUTABLE_LAYOUT, assembly.Frame.Failure(a0)
	bne.w afterLayout
	move.l #MutableLayoutFailure, d2
	move.l #MutableLayoutFailureEnd-MutableLayoutFailure, d3
	bra.w writeLayoutFailure
showWhileLayout
	move.l #WhileLayoutFailure, d2
	move.l #WhileLayoutFailureEnd-WhileLayoutFailure, d3
writeLayoutFailure
	move.l d4, d1
	jsr DOS_WRITE(a6)
afterLayout
	tst.l SourceOrdinal
	bne.w sourcePath
	; Preparation state is stale after the assembly owner starts.
	tst.l InAssembly
	bne.w done
	tst.w PrepStep
	beq.w done
	moveq #0, d0
	move.w PrepStep, d0
	lea FailurePrepStepValue, a0
	bsr.w hexField
	move.l d4, d1
	move.l #FailurePrepStep, d2
	move.l #FailurePrepStepEnd, d3
	sub.l d2, d3
	jsr DOS_WRITE(a6)
	bra.w done
sourcePath
	bsr.w sourceOriginPath
	bne.w done
	move.l d3, d5
	move.l a0, d6
	move.l d4, d1
	move.l #FailurePath, d2
	moveq #8, d3
	jsr DOS_WRITE(a6)
	move.l d4, d1
	move.l d6, d2
	move.l d5, d3
	jsr DOS_WRITE(a6)
	move.l d4, d1
	move.l #FailureNewline, d2
	moveq #1, d3
	jsr DOS_WRITE(a6)
	; The capture line buffer is no longer the source of an assembly record.
	tst.l InAssembly
	bne.w done
	move.l LineUsed, d5
	beq.w done
	cmpi.l #LINE_BYTES, d5
	bhi.w done
	move.l LineBuffer, d6
	beq.w done
	move.l d4, d1
	move.l #FailureSourceLine, d2
	moveq #6, d3
	jsr DOS_WRITE(a6)
	move.l d4, d1
	move.l d6, d2
	move.l d5, d3
	jsr DOS_WRITE(a6)
	move.l d4, d1
	move.l #FailureNewline, d2
	moveq #1, d3
	jsr DOS_WRITE(a6)
done
	rts
	.bend  ; reportFailure

; Resolve the diagnostic identity without borrowing capture/discovery views.
; Retained origin slots contain owned bytes; zero-filled unused slots permit
; physical candidate fallback before retention. A0=path,D3=bounded length,
; D0/CCR=status; other registers preserved. Missing/invalid paths are omitted.
sourceOriginPath	.block
	movem.l d1-d2/a1, -(sp)
	move.l SourceOrdinal, d1
	beq.w bad
	cmpi.l #ORIGIN_LIMIT, d1
	bhi.w candidate
	lsl.l #8, d1
	move.l d1, d2
	addi.l #PATH_BYTES, d2
	lea OriginPaths, a1
	cmp.l memory.Block.Used(a1), d2
	bhi.w candidate
	cmp.l memory.Block.Capacity(a1), d2
	bhi.w candidate
	move.l memory.Block.Pointer(a1), d0
	beq.w candidate
	add.l d1, d0
	bcs.w bad
	movea.l d0, a0
	tst.b (a0)
	bne.w measure
candidate
	tst.l DiscoverMode
	beq.w working
	move.l SourceOrdinal, d1
	cmp.l CandidateCount, d1
	bhi.w bad
	cmpi.l #DISCOVERY_LIMIT, d1
	bhi.w bad
	lsl.l #8, d1
	move.l d1, d2
	addi.l #discovery.SCRATCH_BYTES, d2
	lea DiscoveryBlock, a1
	cmp.l memory.Block.Capacity(a1), d2
	bhi.w bad
	move.l memory.Block.Pointer(a1), d0
	beq.w bad
	subi.l #PATH_BYTES, d2
	add.l d2, d0
	bcs.w bad
	movea.l d0, a0
	bra.w measure
working
	lea SourcePath, a0
measure
	moveq #0, d3
length
	tst.b 0(a0, d3.w)
	beq.w ready
	addq.w #1, d3
	cmpi.w #PATH_BYTES, d3
	blo.w length
bad
	moveq #1, d0
	bra.w done
ready
	tst.l d3
	beq.w bad
	moveq #0, d0
done
	movem.l (sp)+, d1-d2/a1
	tst.l d0
	rts
	.bend  ; sourceOriginPath

; Locate the failing record in numeric provenance only; no source strings survive.
; A missing location (e.g. failure before dispatch) reports file/line zero.
locateFailure	.block
	clr.l SourceOrdinal
	clr.l SourceLine
	lea Work, a0
	move.l assembly.Frame.RecordOffset(a0), d4
	lea Records, a0
	move.l d4, d0
	addq.l #4, d0
	bcs.w done
	cmp.l memory.Block.Used(a0), d0
	bhi.w done
	movea.l memory.Block.Pointer(a0), a2
	adda.l d4, a2
	move.l OriginCount, d3
	lea OriginSpans, a0
	movea.l memory.Block.Pointer(a0), a0
loop
	tst.l d3
	beq.w done
	cmp.l Span.Start(a0), d4
	blo.w next
	cmp.l Span.End(a0), d4
	bhs.w next
	move.l Span.File(a0), SourceOrdinal
	moveq #0, d0
	move.w 2(a2), d0
	move.l d0, SourceLine
	rts
next
	adda.w #SPAN_BYTES, a0
	subq.l #1, d3
	bra.w loop
done
	rts
	.bend  ; locateFailure

; Render a fixed-width hexadecimal diagnostic field. D0=value,A0=8 byte field.
; Clobbers D0-D2/A0/CCR; normal failure formatting, not instrumentation.
hexField	.block
	moveq #7, d2
loop
	rol.l #4, d0
	move.l d0, d1
	andi.l #15, d1
	cmpi.b #9, d1
	bls.w digit
	addq.b #7, d1
digit
	addi.b #'0', d1
	move.b d1, (a0)+
	dbra d2, loop
	rts
	.bend  ; hexField

; Assembly warning callback; Work.RecordOffset identifies the emitted record.
; A0=assembly.Frame. D0/CCR=0 success, 1 diagnostic IO failure.
; Other registers and the application's current source identity are preserved.
reportByteWarning	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l SourceOrdinal, -(sp)
	move.l SourceLine, -(sp)
	bsr.w locateFailure
	move.l SourceOrdinal, d0
	lea WarningFile, a0
	bsr.w hexField
	move.l SourceLine, d0
	lea WarningLine, a0
	bsr.w hexField
	movea.l DosBase, a6
	jsr DOS_OUTPUT(a6)
	move.l d0, d4
	beq.w bad
	move.l #WarningMessage, d2
	move.l #WarningMessageEnd-WarningMessage, d3
	bsr.w writeDiagnostic
	bne.w done
	bsr.w sourceOriginPath
	bne.w good
	move.l a0, d6
	move.l d3, d5
	move.l #FailurePath, d2
	moveq #8, d3
	bsr.w writeDiagnostic
	bne.w done
	move.l d6, d2
	move.l d5, d3
	bsr.w writeDiagnostic
	bne.w done
	move.l #FailureNewline, d2
	moveq #1, d3
	bsr.w writeDiagnostic
	bra.w done
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	move.l (sp)+, d1
	move.l d1, SourceLine
	move.l (sp)+, d1
	move.l d1, SourceOrdinal
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; reportByteWarning

; D4=output handle,D2=bytes,D3=length,A6=DOS. D0/CCR=0 iff fully written.
; DOS_WRITE retains D2/D3; the callback owns their scratch values.
writeDiagnostic	.block
	move.l d4, d1
	jsr DOS_WRITE(a6)
	cmp.l d3, d0
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; writeDiagnostic

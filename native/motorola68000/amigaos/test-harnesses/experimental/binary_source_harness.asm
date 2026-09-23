; Streaming native binary-source contract; input identity belongs to the caller.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent
	.module main
	.cpu 68020
	.use experimental.amigaos.binary_frontend as frontend
	.use experimental.amigaos.binary_assembly as assembly
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_discovery as discovery
	.use experimental.amigaos.binary_declarations as declarations
	.use experimental.amigaos.binary_graph as graph
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_binding_records as records
	.include "memory_telemetry.i"
HEADER_BYTES = 76
IO_BYTES = 4096
INCLUDE_DEPTH = 8
LINE_BYTES = 4096
RECORD_BYTES = 256
DISCOVERY_LIMIT = 128
PATH_BYTES = 256
Span	.struct
Start	.long ?
End	.long ?
File	.long ?
	.endstruct
SPAN_BYTES = Span.File+4
IO_SCRATCH_BYTES = IO_BYTES*(INCLUDE_DEPTH+1)+LINE_BYTES+RECORD_BYTES
IncludeFrame	.struct
Handle	.long ?
Buffer	.long ?
Cursor	.long ?
End	.long ?
Line	.long ?
Origin	.long ?
Path	.byte ?
	.endstruct
INCLUDE_FRAME_BYTES = IncludeFrame.Path+PATH_BYTES
	.section entry, kind=code
	.pub
; AmigaDOS entry. D0=0 only after complete preparation, assembly and output.
; Preserves other registers. Every success/failure path frees owned allocations.
start	.block
	movem.l d1-d7/a0-a6, -(sp)
	.MEMORY_PHASE #0
	move.l #20, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l 4.w, a6
	jsr -552(a6)
	tst.l d0
	beq.w cleanup
	move.l d0, DosBase
	.MEMORY_CLOCK DosBase, #0
	.MEMORY_STAGE #1
	bsr.w prepare
	bne.w failed
	.MEMORY_CLOCK DosBase, #1
	.MEMORY_PHASE #1
	bsr.w run
	bne.w failed
	.MEMORY_CLOCK DosBase, #2
	.MEMORY_PHASE #2
	bsr.w writeOutput
	bne.w failed
	clr.l ReturnCode
	bra.w cleanup
failed
	bsr.w reportFailure
cleanup
	bsr.w closeIncludes
	bsr.w closeSource
	bsr.w closeInput
	tst.l FrontStarted
	beq.w freeBlocks
	lea Front, a0
	jsr frontend.finish
freeBlocks
	lea PackageBlock, a0
	jsr memory.release
	lea RuntimeBlock, a0
	jsr memory.release
	lea PrepBlock, a0
	jsr memory.release
	lea Records, a0
	jsr memory.release
	lea Symbols, a0
	jsr memory.release
	lea Output, a0
	jsr memory.release
	lea FileSpans, a0
	jsr memory.release
	lea OriginSpans, a0
	jsr memory.release
	lea RootPaths, a0
	jsr memory.release
	lea DiscoveryBlock, a0
	jsr memory.release
	lea DeclarationBlock, a0
	jsr memory.release
	lea GraphBlock, a0
	jsr memory.release
	lea GraphSpans, a0
	jsr memory.release
	lea OrderedRecords, a0
	jsr memory.release
	lea OrderedFiles, a0
	jsr memory.release
	.MEMORY_PHASE #3
	move.l DosBase, d0
	beq.w done
	.MEMORY_SAVE DosBase
	movea.l d0, a1
	movea.l 4.w, a6
	jsr -414(a6)
done
	move.l ReturnCode, d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; start
	.priv
reportFailure	.block
	tst.l InAssembly
	beq.w located
	bsr.w locateFailure
located
	move.l SourceOrdinal, d0
	lea FailureFile, a0
	bsr.w hexField
	move.l SourceLine, d0
	lea FailureLine, a0
	bsr.w hexField
	movea.l DosBase, a6
	jsr -60(a6)
	move.l d0, d1
	beq.w done
	move.l #FailureMessage, d2
	move.l #FailureMessageEnd, d3
	sub.l d2, d3
	jsr -48(a6)
done
	rts
	.bend  ; reportFailure

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

; Read exactly D3 bytes into D2 from the open InputHandle. D0/CCR=status.
; Preserves all other registers. No seek, whole-source buffer or byte-at-a-time I/O.
readExact	.block
	movem.l d1-d4/a0-a1/a6, -(sp)
	movea.l DosBase, a6
loop
	tst.l d3
	beq.w good
	move.l d3, d4
	move.l InputHandle, d1
	jsr -42(a6)
	tst.l d0
	ble.w bad
	cmp.l d4, d0
	bhi.w bad
	add.l d0, d2
	sub.l d0, d3
	bra.w loop
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d4/a0-a1/a6
	tst.l d0
	rts
	.bend  ; readExact

prepare	.block
	movea.l DosBase, a6
	move.l #InputPath, d1
	move.l #1005, d2
	jsr -30(a6)
	tst.l d0
	beq.w bad
	move.l d0, InputHandle
	move.l #Header, d2
	moveq #HEADER_BYTES, d3
	bsr.w readExact
	bne.w closeBad
	lea Header, a4
	cmpi.l #$42535033, package.Header.Magic(a4)
	bne.w closeBad
	move.l package.Header.Bytes(a4), d0
	cmpi.l #HEADER_BYTES, d0
	blo.w closeBad
	lea PackageBlock, a0
	jsr memory.reserve
	bne.w closeBad
	lea PackageBlock, a0
	movea.l memory.Block.Pointer(a0), a1
	lea Header, a0
	moveq #HEADER_BYTES, d0
	bsr.w copy
	lea PackageBlock, a0
	movea.l memory.Block.Pointer(a0), a4
	move.l a4, d2
	addi.l #HEADER_BYTES, d2
	move.l package.Header.Bytes(a4), d3
	subi.l #HEADER_BYTES, d3
	bsr.w readExact
	bne.w closeBad
	move.l package.Header.RuntimeBytes(a4), d0
	cmpi.l #HEADER_BYTES, d0
	blo.w closeBad
	cmp.l package.Header.Bytes(a4), d0
	bhi.w closeBad
	btst #0, d0
	bne.w closeBad
	cmp.l package.Header.Dictionary(a4), d0
	bhi.w closeBad
	cmp.l package.Header.Tokenizer(a4), d0
	bhi.w closeBad
	movea.l a4, a0
	jsr frontend.scratchSize
	bne.w closeBad
	move.l d1, d0
	addi.l #IO_SCRATCH_BYTES, d0
	bcs.w closeBad
	lea PrepBlock, a0
	jsr memory.reserve
	bne.w closeBad
	lea PrepBlock, a0
	movea.l memory.Block.Pointer(a0), a1
	lea Front, a0
	move.l a4, frontend.Frame.Package(a0)
	move.l a1, frontend.Frame.Scratch(a0)
	adda.l d1, a1
	move.l a1, IoBuffer
	move.l a1, IoCursor
	move.l a1, IoEnd
	adda.l #IO_BYTES*(INCLUDE_DEPTH+1), a1
	move.l a1, LineBuffer
	adda.l #LINE_BYTES, a1
	move.l a1, frontend.Frame.Output(a0)
	move.l #RECORD_BYTES, frontend.Frame.Capacity(a0)
	move.l #1, FrontStarted
	jsr frontend.begin
	bne.w closeBad
	.MEMORY_STAGE #0
	move.l #ManifestWord, d2
	moveq #2, d3
	bsr.w readExact
	bne.w closeBad
	moveq #0, d0
	move.w ManifestWord, d0
	move.l d0, d1
	andi.l #$8000, d1
	move.l d1, GraphMode
	move.l d0, d1
	andi.l #$4000, d1
	move.l d1, DiscoverMode
	andi.l #$3fff, d0
	beq.w closeBad
	tst.l DiscoverMode
	beq.w sourceCountReady
	tst.l GraphMode
	beq.w closeBad
	cmpi.l #2, d0
	blo.w closeBad
	move.l d0, SearchPathCount
	move.l #discovery.SCRATCH_BYTES+DISCOVERY_LIMIT*PATH_BYTES, d0
	lea DiscoveryBlock, a0
	jsr memory.reserve
	bne.w closeBad
	movea.l memory.Block.Pointer(a0), a1
	move.l a1, DiscoveryScratch
	adda.l #discovery.SCRATCH_BYTES, a1
	move.l a1, DiscoveryPaths
	bsr.w readManifestPath
	bne.w closeBad
	lea SourcePath, a0
	bsr.w appendCandidate
	bne.w closeBad
	subq.l #1, SearchPathCount
searchRoot
	bsr.w readManifestPath
	bne.w closeBad
	movea.l DiscoveryScratch, a0
	lea SourcePath, a1
	lea appendCandidate, a2
	suba.l a3, a3
	movea.l DosBase, a4
	jsr discovery.scan
	bne.w closeBad
	subq.l #1, SearchPathCount
	bne.w searchRoot
	move.l #ManifestWord, d2
	moveq #2, d3
	bsr.w readExact
	bne.w closeBad
	moveq #0, d0
	move.w ManifestWord, d0
	cmpi.l #16, d0
	bhi.w closeBad
	move.l d0, RootCount
	move.l d0, IncludeRootsRemaining
	lsl.l #8, d0
	lea RootPaths, a0
	jsr memory.reserve
	bne.w closeBad
	move.l RootCount, d0
	lsl.l #8, d0
	move.l d0, memory.Block.Used(a0)
includeRoot
	tst.l IncludeRootsRemaining
	beq.w rootsDone
	bsr.w readManifestPath
	bne.w closeBad
	bsr.w saveRoot
	bne.w closeBad
	subq.l #1, IncludeRootsRemaining
	bra.w includeRoot
rootsDone
	movea.l DosBase, a6
	move.l InputHandle, d1
	move.l #ManifestWord, d2
	moveq #1, d3
	jsr -42(a6)
	tst.l d0
	bne.w closeBad
	move.l CandidateCount, d0
sourceCountReady
	move.l d0, SourceCount
	clr.l SpanCount
	tst.l DiscoverMode
	beq.w spanCapacityReady
	move.l #graph.MAX_SPANS, d0
spanCapacityReady
	mulu.w #SPAN_BYTES, d0
	lea FileSpans, a0
	jsr memory.reserve
	bne.w closeBad
	tst.l GraphMode
	beq.w manifestReady
	move.l #frontend.GRAPH_BYTES, d0
	lea GraphBlock, a0
	jsr memory.reserve
	bne.w closeBad
	movea.l memory.Block.Pointer(a0), a1
	lea Front, a0
	jsr frontend.beginGraph
	bne.w closeBad
	move.l #frontend.GRAPH_SPAN_BYTES, d0
	lea GraphSpans, a0
	jsr memory.reserve
	bne.w closeBad
manifestReady
	tst.l DiscoverMode
	beq.w filesReady
	move.l #declarations.SCRATCH_BYTES, d0
	lea DeclarationBlock, a0
	jsr memory.reserve
	bne.w closeBad
	movea.l memory.Block.Pointer(a0), a0
	jsr declarations.begin
	bsr.w indexCandidates
	bne.w closeBad
filesReady
	move.l #1, SourceOrdinal
	move.l SourceCount, NextOrigin
nextFile
	tst.l DiscoverMode
	beq.w ordinalReady
	lea GraphBlock, a0
	movea.l memory.Block.Pointer(a0), a0
	move.l SourceOrdinal, graph.GraphState.SourceIndex(a0)
ordinalReady
	clr.l SourceLine
	bsr.w openSource
	bne.w closeBad
	tst.l DiscoverMode
	beq.w spanAllowed
	move.l SpanCount, d0
	cmpi.l #graph.MAX_SPANS, d0
	bhs.w closeBad
spanAllowed
	move.l SourceOrdinal, OriginId
	bsr.w fileSpan
	lea Records, a1
	move.l memory.Block.Used(a1), Span.Start(a0)
	move.l SourceOrdinal, Span.File(a0)
	clr.l LineUsed
	move.l #1, SourceLine
sourceLoop
	bsr.w readByte
	cmpi.l #-1, d0
	beq.w sourceDone
	tst.l d0
	bmi.w closeBad
	addq.l #1, SourceBytes
	cmpi.b #10, d0
	beq.w lineReady
	move.l LineUsed, d1
	cmpi.l #LINE_BYTES, d1
	bhs.w closeBad
	movea.l LineBuffer, a0
	move.b d0, 0(a0, d1.l)
	addq.l #1, LineUsed
	bra.w sourceLoop
lineReady
	bsr.w lowerLine
	bne.w closeBad
	bra.w sourceLoop
sourceDone
	tst.l LineUsed
	beq.w sourceReady
	bsr.w lowerLine
	bne.w closeBad
	bra.w sourceLoop
sourceReady
	tst.l IncludeDepthNow
	beq.w sourceEnd
	bsr.w popInclude
	bne.w closeBad
	bra.w sourceLoop
sourceEnd
	tst.l RequestedModule
	beq.w selectionComplete
	cmpi.l #2, SelectionState
	bne.w closeBad
selectionComplete
fileDone
	bsr.w closeSource
	bne.w closeBad
	move.l SourceCount, d0
	subq.l #1, d0
	lea Front, a0
	jsr frontend.endFile
	bne.w closeBad
	bsr.w fileSpan
	lea Records, a1
	move.l memory.Block.Used(a1), Span.End(a0)
	addq.l #1, SpanCount
	lea FileSpans, a0
	addi.l #SPAN_BYTES, memory.Block.Used(a0)
	tst.l DiscoverMode
	beq.w sequential
	bsr.w resolveGraph
	beq.w prepared
	cmpi.l #2, d0
	beq.w nextFile
	bra.w closeBad
sequential
	addq.l #1, SourceOrdinal
	move.l SourceCount, d0
	cmp.l SourceOrdinal, d0
	bhs.w nextFile
	movea.l DosBase, a6
	move.l InputHandle, d1
	move.l #ManifestWord, d2
	moveq #1, d3
	jsr -42(a6)
	tst.l d0
	bne.w closeBad  ; trailing manifest bytes and read errors are invalid
prepared
	move.l SourceCount, SourceOrdinal
	.MEMORY_STAGE #5
	bsr.w closeInput
	bne.w bad
	tst.l DiscoverMode
	beq.w pathsCleared
	lea DiscoveryBlock, a0
	jsr memory.release
	clr.l DiscoveryScratch
	clr.l DiscoveryPaths
pathsCleared
	lea RootPaths, a0
	jsr memory.release
	tst.l GraphMode
	beq.w orderReady
	tst.l OrderedCount
	bne.w orderReady
	lea GraphSpans, a0
	movea.l memory.Block.Pointer(a0), a1
	move.l memory.Block.Capacity(a0), d0
	lea Front, a0
	jsr frontend.orderGraph
	bne.w completionBad
	move.l d1, OrderedCount
orderReady
	lea Records, a0
	movea.l memory.Block.Pointer(a0), a1
	move.l memory.Block.Used(a0), d0
	lea Front, a0
	jsr frontend.complete
	bne.w completionBad
	move.l frontend.Frame.NameCount(a0), NameCount
	tst.l GraphMode
	beq.w selected
	bsr.w materializeOrder
	bne.w completionBad
selected
	lea Records, a0
	movea.l memory.Block.Pointer(a0), a1
	move.l memory.Block.Used(a0), d0
	lea Front, a0
	jsr frontend.indexBlocks
	bne.w completionBad
	bsr.w clearIncludeText
	lea Front, a0
	jsr frontend.finish
	clr.l FrontStarted
	lea GraphBlock, a0
	jsr memory.release
	lea GraphSpans, a0
	jsr memory.release
	lea DeclarationBlock, a0
	jsr memory.release
	lea PrepBlock, a0
	jsr memory.release
	lea FileSpans, a0
	jsr memory.release
	clr.l IoBuffer
	clr.l IoCursor
	clr.l IoEnd
	clr.l LineBuffer
; Copy relocatable execution prefix while the old allocation is still live.
; No lexical storage or source buffer remains when either assembly pass starts.
	lea PackageBlock, a0
	movea.l memory.Block.Pointer(a0), a4
	move.l package.Header.RuntimeBytes(a4), d0
	lea RuntimeBlock, a0
	jsr memory.reserve
	bne.w bad
	lea RuntimeBlock, a0
	movea.l memory.Block.Pointer(a0), a1
	movea.l a4, a0
	move.l package.Header.RuntimeBytes(a4), d0
	bsr.w copy
	lea RuntimeBlock, a0
	movea.l memory.Block.Pointer(a0), a4
	move.l package.Header.RuntimeBytes(a4), package.Header.Bytes(a4)
	clr.l package.Header.Dictionary(a4)
	clr.l package.Header.DictionaryCount(a4)
	clr.l package.Header.Tokenizer(a4)
	clr.l package.Header.TokenizerBytes(a4)
	lea PackageBlock, a0
	jsr memory.release
	lea Records, a0
	.MEMORY_LAYOUT package.Header.RuntimeBytes(a4), memory.Block.Used(a0), SourceBytes
	lea Front, a0
	clr.l frontend.Frame.Package(a0)
	clr.l frontend.Frame.Source(a0)
	clr.l frontend.Frame.SourceBytes(a0)
	clr.l frontend.Frame.Scratch(a0)
	clr.l frontend.Frame.Output(a0)
	moveq #0, d0
	rts
completionBad
	clr.l SourceOrdinal
	clr.l SourceLine
	bra.w bad
closeBad
	bsr.w closeIncludes
	bsr.w closeSource
	bsr.w closeInput
bad
	moveq #1, d0
	rts
	.bend  ; prepare

; Consume one explicit manifest path and open that actual guest file.
; The manifest remains open independently. Each source has fresh buffered I/O.
openSource	.block
	tst.l DiscoverMode
	beq.w manifestSource
	move.l SourceOrdinal, d0
	subq.l #1, d0
	lsl.l #8, d0
	movea.l DiscoveryPaths, a0
	adda.l d0, a0
	lea SourcePath, a1
	move.w #PATH_BYTES/4-1, d0
copyDiscovered
	move.l (a0)+, (a1)+
	dbra d0, copyDiscovered
	bra.w openPath
manifestSource
	bsr.w readManifestPath
	bne.w bad
openPath
	movea.l DosBase, a6
	move.l #SourcePath, d1
	move.l #1005, d2
	jsr -30(a6)
	tst.l d0
	beq.w bad
	move.l d0, SourceHandle
	move.l IoBuffer, IoCursor
	move.l IoBuffer, IoEnd
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; openSource

; Read and validate a manifest path without opening it. D0/CCR=status.
readManifestPath	.block
	move.l #ManifestWord, d2
	moveq #2, d3
	bsr.w readExact
	bne.w bad
	moveq #0, d4
	move.w ManifestWord, d4
	beq.w bad
	cmpi.l #255, d4
	bhi.w bad
	move.l #SourcePath, d2
	move.l d4, d3
	bsr.w readExact
	bne.w bad
	lea SourcePath, a0
	move.l d4, d0
check
	move.b (a0)+, d1
	cmpi.b #32, d1
	blo.w bad
	cmpi.b #126, d1
	bhi.w bad
	subq.l #1, d0
	bne.w check
	clr.b (a0)
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; readManifestPath

; A0=temporary discovered path, A1=unused callback context. Record each
; physical file once in the preparation-only path list. D0/CCR=status;
; preserves D3-D7/A2-A6 as required by discovery.scan.
appendCandidate	.block
	movem.l d1-d3/a0-a3, -(sp)
	movea.l a0, a3
	moveq #0, d3
findPath
	cmp.l CandidateCount, d3
	bhs.w addPath
	move.l d3, d0
	lsl.l #8, d0
	movea.l DiscoveryPaths, a1
	adda.l d0, a1
	movea.l a3, a0
comparePath
	move.b (a0)+, d1
	cmp.b (a1)+, d1
	bne.w nextPath
	tst.b d1
	bne.w comparePath
	moveq #0, d0
	bra.w done
nextPath
	addq.l #1, d3
	bra.w findPath
addPath
	cmpi.l #DISCOVERY_LIMIT, d3
	bhs.w bad
	move.l d3, d0
	lsl.l #8, d0
	movea.l DiscoveryPaths, a1
	adda.l d0, a1
	move.w #PATH_BYTES-1, d2
	movea.l a3, a0
copyPath
	move.b (a0)+, (a1)+
	beq.w added
	dbra d2, copyPath
	bra.w bad
added
	addq.l #1, CandidateCount
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a3
	tst.l d0
	rts
	.bend  ; appendCandidate

closeSource	.block
	move.l SourceHandle, d1
	beq.w good
	clr.l SourceHandle
	movea.l DosBase, a6
	jsr -36(a6)
	tst.l d0
	beq.w bad
	lea SourcePath, a0
	moveq #63, d0
clearPath
	clr.l (a0)+
	dbra d0, clearPath
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; closeSource

; A0=current numeric span, each {start offset,end offset,file ordinal};
; clobbers D0/A0. SpanCount identifies this preparation load.
fileSpan	.block
	move.l SpanCount, d0
	mulu.w #SPAN_BYTES, d0
	lea FileSpans, a0
	movea.l memory.Block.Pointer(a0), a0
	adda.l d0, a0
	rts
	.bend  ; fileSpan

closeInput	.block
	movea.l DosBase, a6
	move.l InputHandle, d1
	beq.w good
	clr.l InputHandle
	jsr -36(a6)
	tst.l d0
	beq.w bad
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; closeInput

; D0=unsigned byte, -1 EOF, -2 read failure. Other registers preserved.
readByte	.block
	movem.l d1-d3/a0-a1/a6, -(sp)
	movea.l IoCursor, a0
	cmpa.l IoEnd, a0
	bne.w available
	movea.l DosBase, a6
	move.l SourceHandle, d1
	move.l IoBuffer, d2
	move.l #IO_BYTES, d3
	jsr -42(a6)
	tst.l d0
	bmi.w bad
	beq.w eof
	cmpi.l #IO_BYTES, d0
	bhi.w bad
	movea.l IoBuffer, a0
	move.l a0, IoCursor
	add.l a0, d0
	move.l d0, IoEnd
available
	moveq #0, d0
	move.b (a0)+, d0
	move.l a0, IoCursor
	bra.w done
eof
	moveq #-1, d0
	bra.w done
bad
	moveq #-2, d0
done
	movem.l (sp)+, d1-d3/a0-a1/a6
	rts
	.bend  ; readByte

lowerLine	.block
	bsr.w handleInclude
	tst.l d0
	bmi.w bad
	bne.w included
	bsr.w selectLine
	tst.l d0
	bmi.w bad
	beq.w skipped
	lea Front, a0
	move.l LineBuffer, frontend.Frame.Source(a0)
	move.l LineUsed, d0
	beq.w trimmed
	movea.l LineBuffer, a1
	cmpi.b #13, -1(a1, d0.l)
	bne.w trimmed
	subq.l #1, d0
trimmed
	move.l d0, frontend.Frame.SourceBytes(a0)
	move.l SourceLine, d0
	jsr frontend.setLine
	bne.w bad
	lea Front, a0
	jsr frontend.line
	bne.w bad
	lea Records, a0
	move.l memory.Block.Used(a0), d0
	move.l d0, LineOffset
	lea Front, a1
	add.l frontend.Frame.Used(a1), d0
	bcs.w bad
	jsr memory.reserve
	bne.w bad
	lea Records, a1
	movea.l memory.Block.Pointer(a1), a2
	adda.l memory.Block.Used(a1), a2
	lea Front, a0
	move.l frontend.Frame.Used(a0), d0
	add.l d0, memory.Block.Used(a1)
	movea.l frontend.Frame.Output(a0), a0
	movea.l a2, a1
	bsr.w copy
	bsr.w appendOrigin
	bne.w bad
	clr.l LineUsed
	addq.l #1, SourceLine
	moveq #0, d0
	rts
included
	clr.l LineUsed
	moveq #0, d0
	rts
skipped
	clr.l LineUsed
	addq.l #1, SourceLine
	moveq #0, d0
	rts
bad
	move.l OriginId, SourceOrdinal
	moveq #1, d0
	rts
	.bend  ; lowerLine

run	.block
	move.l #1, InAssembly
	move.l NameCount, d0
	beq.w bad
	cmpi.l #65536, d0
	bhi.w bad
	move.l d0, d1
	lsl.l #2, d0
	add.l d1, d0
	lea Symbols, a0
	jsr memory.reserve
	bne.w bad
	lea Context, a0
	lea Symbols, a1
	movea.l memory.Block.Pointer(a1), a2
	move.l a2, package.Context.Values(a0)
	move.l NameCount, d0
	lsl.l #2, d0
	adda.l d0, a2
	lea Context, a0
	move.l a2, package.Context.Defined(a0)
	move.l NameCount, package.Context.Count(a0)
	lea RuntimeBlock, a1
	move.l memory.Block.Pointer(a1), package.Context.Package(a0)
	lea Work, a0
	lea Records, a1
	move.l memory.Block.Pointer(a1), assembly.Frame.Records(a0)
	move.l memory.Block.Used(a1), assembly.Frame.RecordBytes(a0)
	move.l #Context, assembly.Frame.Context(a0)
	move.l #allocateOutput, assembly.Frame.Allocate(a0)
	jsr assembly.assemble
	rts
bad
	moveq #1, d0
	rts
	.bend  ; run

; Assembly sizing-pass callback. D0=needed bytes,A0=Frame; other registers preserved.
allocateOutput	.block
	movem.l d1-d2/a0-a2, -(sp)
	movea.l a0, a2
	lea Output, a0
	jsr memory.reserve
	bne.w done
	move.l memory.Block.Pointer(a0), assembly.Frame.Output(a2)
	move.l memory.Block.Capacity(a0), assembly.Frame.Capacity(a2)
done
	movem.l (sp)+, d1-d2/a0-a2
	tst.l d0
	rts
	.bend  ; allocateOutput

writeOutput	.block
	movea.l DosBase, a6
	move.l #OutputPath, d1
	move.l #1006, d2
	jsr -30(a6)
	tst.l d0
	beq.w bad
	move.l d0, d4
	moveq #0, d5
	lea Work, a5
loop
	move.l assembly.Frame.Used(a5), d3
	sub.l d5, d3
	beq.w complete
	lea Output, a0
	move.l memory.Block.Pointer(a0), d2
	add.l d5, d2
	move.l d4, d1
	jsr -48(a6)
	tst.l d0
	ble.w closeBad
	add.l d0, d5
	cmp.l assembly.Frame.Used(a5), d5
	bhi.w closeBad
	bra.w loop
complete
	move.l d4, d1
	jsr -36(a6)
	tst.l d0
	beq.w bad
	moveq #0, d0
	rts
closeBad
	move.l d4, d1
	jsr -36(a6)
bad
	moveq #1, d0
	rts
	.bend  ; writeOutput
; D0 bytes A0->A1, distinct allocations. Clobbers D0/A0/A1/CCR.
copy	.block
	tst.l d0
	beq.w done
loop
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w loop
done
	rts
	.bend  ; copy
	.include "binary_source_graph_records.i"
	.include "binary_source_discovery_index.i"
	.include "binary_source_selection.i"
	.include "binary_source_includes.i"
	.endsection
	.section data, kind=data
DosName	.byte "dos.library", 0
SelectedModuleKeyword	.byte "module"
SelectedEndmoduleKeyword	.byte "endmodule"
InputPath	.byte "Work:input.bin", 0
OutputPath	.byte "Work:output.bin", 0
FailureMessage	.byte "binary source: unsupported or invalid input [file "
FailureFile	.byte "00000000"
	.byte ", line "
FailureLine	.byte "00000000", "]", 10
FailureMessageEnd
	.endsection
	.section bss, kind=bss
	.align 4
DosBase	.res long, 1
ReturnCode	.res long, 1
InputHandle	.res long, 1
SourceHandle	.res long, 1
SourceCount	.res long, 1
SpanCount	.res long, 1
GraphMode	.res long, 1
DiscoverMode	.res long, 1
SearchPathCount	.res long, 1
CandidateCount	.res long, 1
DiscoveryBlock	.res byte, memory.Block.Used+4
DiscoveryScratch	.res long, 1
DiscoveryPaths	.res long, 1
DeclarationBlock	.res byte, memory.Block.Used+4
IndexOverflow	.res long, 1
RequestedModule	.res long, 1
RequestedName	.res long, 1
RequestedNameBytes	.res long, 1
SelectionState	.res long, 1
OrderedCount	.res long, 1
GraphBlock	.res byte, memory.Block.Used+4
GraphSpans	.res byte, memory.Block.Used+4
OrderedRecords	.res byte, memory.Block.Used+4
OrderedFiles	.res byte, memory.Block.Used+4
OriginSpans	.res byte, memory.Block.Used+4
OriginCount	.res long, 1
RootPaths	.res byte, memory.Block.Used+4
RootCount	.res long, 1
IncludeRootsRemaining	.res long, 1
NextOrigin	.res long, 1
OriginId	.res long, 1
LineOffset	.res long, 1
IncludeDepthNow	.res long, 1
IncludeStack	.res byte, INCLUDE_FRAME_BYTES*INCLUDE_DEPTH
IncludePath	.res byte, PATH_BYTES
IncludeName	.res byte, PATH_BYTES
IncludeHandle	.res long, 1
SourceOrdinal	.res long, 1
SourceLine	.res long, 1
InAssembly	.res long, 1
FileSpans	.res byte, memory.Block.Used+4
ManifestWord	.res word, 1
SourcePath	.res byte, 256
	.align 4
FrontStarted	.res long, 1
IoBuffer	.res long, 1
IoCursor	.res long, 1
IoEnd	.res long, 1
LineBuffer	.res long, 1
LineUsed	.res long, 1
SourceBytes	.res long, 1
NameCount	.res long, 1
Header	.res byte, HEADER_BYTES
Front	.res byte, frontend.Frame.GraphBefore+4
Work	.res byte, assembly.Frame.RecordOffset+4
Context	.res byte, package.Context.Reserved+2
PackageBlock	.res byte, memory.Block.Used+4
RuntimeBlock	.res byte, memory.Block.Used+4
PrepBlock	.res byte, memory.Block.Used+4
Records	.res byte, memory.Block.Used+4
Symbols	.res byte, memory.Block.Used+4
Output	.res byte, memory.Block.Used+4
	.endsection
	.output "build/binary_source_harness", format=hunk, sections=entry, code, data, bss
	.endmodule

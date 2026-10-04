; Streaming native binary-source contract; input identity belongs to the caller.
; @opforge-owner: experimental.amigaos.binary_app
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent
	.module experimental.amigaos.binary_app
	.cpu 68020
	.use experimental.amigaos.binary_frontend as frontend
	.use experimental.amigaos.binary_capture as capture
	.use experimental.amigaos.binary_configuration as configuration
	.use experimental.amigaos.binary_scopes as scopes
	.use experimental.amigaos.binary_assembly as assembly
	.use experimental.amigaos.binary_sections as sections
	.use experimental.amigaos.binary_hunk_output as hunk
	.use experimental.amigaos.binary_output_plan as output_plan
	.use experimental.amigaos.binary_output_io as output_io
	.use experimental.amigaos.binary_record_output as record_output
	.use experimental.amigaos.binary_output_spans as output_spans
	.use experimental.amigaos.binary_metadata_prepare as metadata
	.use experimental.amigaos.binary_metadata_output as metadata_output
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_package_loader as loader
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_discovery as discovery
	.use experimental.amigaos.binary_input_plan as inputs
	.use experimental.amigaos.binary_line_input as line_input
	.use experimental.amigaos.binary_declarations as declarations
	.use experimental.amigaos.binary_graph as graph
	.use experimental.amigaos.binary_ordered_records as ordered
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_file_data as filedata
	.use prvm.amigaos.abi as parser_abi
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	.use experimental.amigaos.binary_block_index as blocks
	.use experimental.amigaos.binary_encoding as encoding
.endif
.endif
.endif
	.use experimental.amigaos.binary_binding_records as records
	.include "memory_telemetry.i"
	.include "binding_telemetry.i"
HEADER_BYTES = package.HEADER_BYTES
IO_BYTES = 4096
INCLUDE_DEPTH = 8
LINE_BYTES = 4096
RECORD_BYTES = 256
RECORD_LIMIT = 2*memory.LIMIT
DISCOVERY_LIMIT = 128
PATH_BYTES = 256
DOS_OUTPUT = -60
DOS_WRITE = -48
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_PUT_STR = -948
STEP_ORDER = 1
STEP_BIND = 2
PROGRESS_SOURCE_BEGIN = 1
PROGRESS_SOURCE_END = 2
PROGRESS_ORDER = 10
PROGRESS_BIND = 11
PROGRESS_MATERIALIZE = 12
PROGRESS_INDEX = 13
PROGRESS_PARAMETERS = 14
PROGRESS_PREPARED = 15
PROGRESS_ASSEMBLE = 20
PROGRESS_ASSEMBLED = 21
PROGRESS_OUTPUT = 22
PROGRESS_BLOCK_WORK = 23
PROGRESS_ASSEMBLY_FAILURE = 24
PROGRESS_ASSEMBLY_NAME = 25
PROGRESS_ASSEMBLY_POSITION = 26
PROGRESS_ASSEMBLY_RECORDS = 27
PROGRESS_ASSEMBLY_SECTIONS = 28
PROGRESS_ASSEMBLY_SELECTION = 29
PROGRESS_PACKAGE = 30
PROGRESS_RECORD_OUTPUT = 31
PROGRESS_ASSEMBLY_WORK = 300
STEP_MATERIALIZE = 3
STEP_INDEX = 4
STEP_SELECT = 5
STEP_PARAMETERS = 6
Span	.struct
Start	.long ?
End	.long ?
File	.long ?
	.endstruct
SPAN_BYTES = graph.SPAN_BYTES
CaptureRegion	.struct
Base	.long ?
End	.long ?
File	.long ?
Derived	.long ?
Arena	.byte ?
	.endstruct
CAPTURE_REGION_BYTES = CaptureRegion.Arena+memory.Block.Used+4
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
	.pub
Frame	.struct
PackagePath	.long ?
SourcePath	.long ?
OutputPath	.long ?
Mode	.word ?  ; zero: manifest harness; one: single source; two: discovery
OutputKind	.word ?  ; zero: harness auto; one: flat binary; two: Hunk; three: source-only
ModuleRoots	.long ?
ModuleCount	.long ?
IncludeRoots	.long ?
IncludeCount	.long ?
Catalog	.long ?
CatalogBytes	.long ?
Cpu	.long ?
Dialect	.long ?
PackageRoot	.long ?
StartSet	.word ?
Start	.long ?
OutputBase	.long ?  ; Shell input basename, independent of source selection
OutputDefault	.word ?  ; CLI output filename was omitted
Reserved	.word ?
LowerLine	.long ?  ; temporary capture proof callback; zero uses frontend.line
	.endstruct
FRAME_BYTES = Frame.LowerLine+4
	.section code, kind=code
	.pub
; Assemble one package and source selection. A0=Frame, D0=Shell return code.
; Preserves D1-D7/A0-A6. Every path frees owned allocations.
execute	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l Frame.PackagePath(a0), InputName
	move.l Frame.OutputPath(a0), OutputName
	move.w Frame.Mode(a0), CliMode
	move.l Frame.LowerLine(a0), LowerLineRoutine
	move.w Frame.OutputKind(a0), CliOutputKind
	move.w Frame.StartSet(a0), OutputStartSet
	move.l Frame.Start(a0), OutputStart
	lea MetadataOutput, a1
	move.l Frame.OutputBase(a0), metadata_output.Frame.Base(a1)
	move.w Frame.OutputDefault(a0), metadata_output.Frame.Default(a1)
	move.w Frame.OutputKind(a0), metadata_output.Frame.CliKind(a1)
	move.l Frame.OutputPath(a0), metadata_output.Frame.CliPath(a1)
	move.l #MetadataConfig, metadata_output.Frame.Config(a1)
	clr.w metadata_output.Frame.HexSet(a1)
	lea MetadataConfig, a1
	clr.w metadata.Config.NameSet(a1)
	clr.w metadata.Config.HexSet(a1)
	clr.w metadata.Config.Root(a1)
	.MEMORY_COUNTER_CLEAR output_spans.Events
	move.l Frame.ModuleRoots(a0), CliModuleRoots
	move.l Frame.ModuleCount(a0), CliModuleCount
	move.l Frame.IncludeRoots(a0), CliIncludeRoots
	move.l Frame.IncludeCount(a0), CliIncludeCount
	lea PackageSource, a1
	move.l Frame.Catalog(a0), loader.Frame.Catalog(a1)
	move.l Frame.CatalogBytes(a0), loader.Frame.CatalogBytes(a1)
	move.l Frame.Cpu(a0), loader.Frame.Cpu(a1)
	move.l Frame.Dialect(a0), loader.Frame.Dialect(a1)
	move.l Frame.PackageRoot(a0), loader.Frame.Root(a1)
	clr.l PackageStatus
	tst.l InputName
	bne.w packageConfigured
	tst.l loader.Frame.Cpu(a1)
	beq.w invalidConfig
packageConfigured
	cmpi.w #3, CliOutputKind
	beq.w outputConfigured
	tst.l OutputName
	beq.w invalidConfig
outputConfigured
	tst.w CliMode
	beq.w configured
	cmpi.l #8, CliModuleCount
	bhi.w invalidConfig
	cmpi.l #16, CliIncludeCount
	bhi.w invalidConfig
	tst.l CliModuleCount
	beq.w includesReady
	tst.l CliModuleRoots
	beq.w invalidConfig
includesReady
	tst.l CliIncludeCount
	beq.w entryReady
	tst.l CliIncludeRoots
	beq.w invalidConfig
entryReady
	movea.l Frame.SourcePath(a0), a1
	move.l a1, d0
	beq.w invalidConfig
	lea SourcePath, a0
	move.w #PATH_BYTES, d0
copyPath
	move.b (a1)+, (a0)+
	beq.w configured
	subq.w #1, d0
	bne.w copyPath
	bra.w invalidConfig
configured
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
	clr.w PrepStep
	bsr.w prepare
	bne.w failed
	tst.w CliMode
	beq.w outputsReady
	lea MetadataOutput, a0
	jsr metadata_output.resolve
	bne.w failed
	lea MetadataOutput, a0
	lea metadata_output.Frame.ResolvedCli(a0), a1
	move.l a1, OutputName
	tst.w OutputStartSet
	beq.w outputsReady
	cmpi.w #record_output.HEX, CliOutputKind
	beq.w outputsReady
	cmpi.w #record_output.SREC, CliOutputKind
	beq.w outputsReady
	tst.w metadata_output.Frame.HexSet(a0)
	beq.w missingRecordOutput
outputsReady
	.MEMORY_CLOCK DosBase, #1
	.MEMORY_PHASE #1
	bsr.w run
	bne.w failed
	.MEMORY_PROGRESS_RECORDS DosBase, #PROGRESS_ASSEMBLED, #0, #0, Records, memory.Block.Used
	.MEMORY_CLOCK DosBase, #2
	.MEMORY_PHASE #2
	.MEMORY_PROGRESS_RECORDS DosBase, #PROGRESS_OUTPUT, #0, #0, Records, memory.Block.Used
	bsr.w writeOutputs
	bne.w outputFailed
	clr.l ReturnCode
	bra.w cleanup
outputFailed
	movea.l DosBase, a6
	move.l #OutputFailure, d1
	jsr DOS_PUT_STR(a6)
	bra.w cleanup
missingRecordOutput
	movea.l DosBase, a6
	move.l #GoFailure, d1
	jsr DOS_PUT_STR(a6)
	bra.w cleanup
failed
	bsr.w reportFailure
	bra.w cleanup
invalidConfig
	move.l #20, ReturnCode
cleanup
	bsr.w closeIncludes
	bsr.w closeSource
	bsr.w closeInput
	tst.l FrontStarted
	beq.w freeBlocks
	lea Front, a0
	jsr frontend.finish
freeBlocks
	bsr.w releaseScheduledFrontend
	bsr.w releaseCapturedSources
	lea PackageSource, a0
	jsr loader.release
	lea RuntimeBlock, a0
	jsr memory.release
	lea PrepBlock, a0
	jsr memory.release
	lea Records, a0
	jsr memory.release
	lea Symbols, a0
	jsr memory.release
	lea Parameters, a0
	jsr memory.release
	lea Output, a0
	jsr memory.release
	lea EmissionSpans, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea TextBuffer, a0
	jsr memory.release
	lea HunkBlock, a0
	jsr memory.release
	lea RelocBlock, a0
	jsr memory.release
	lea HunkRelocs, a0
	jsr memory.release
	lea FileSpans, a0
	jsr memory.release
	lea OriginSpans, a0
	jsr memory.release
	lea OriginPaths, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
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
	.bend  ; execute
	.priv
reportFailure	.block
	.BINDING_DIAGNOSTIC_REPORT DosBase
	tst.l InAssembly
	beq.w located
	bsr.w locateFailure
	.MEMORY_PROGRESS DosBase, #PROGRESS_ASSEMBLY_FAILURE, assembly.FailureStage, SourceOrdinal, SourceLine
	.MEMORY_PROGRESS DosBase, #PROGRESS_ASSEMBLY_NAME, assembly.FailureName, #0, #0
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
	cmpi.w #assembly.FAILURE_MUTABLE_LAYOUT, assembly.Frame.Failure(a0)
	bne.w afterLayout
	move.l #MutableLayoutFailure, d2
	move.l #MutableLayoutFailureEnd-MutableLayoutFailure, d3
	move.l d4, d1
	jsr DOS_WRITE(a6)
afterLayout
	tst.l SourceOrdinal
	bne.w sourcePath
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
	lea PackageSource, a0
	move.l DosBase, loader.Frame.DosBase(a0)
	move.l InputName, loader.Frame.Path(a0)
	clr.w loader.Frame.AllowTrailing(a0)
	tst.w CliMode
	bne.w load
	move.w #1, loader.Frame.AllowTrailing(a0)
load
	jsr loader.acquire
	move.l d0, PackageStatus
	bne.w bad
	move.l loader.Frame.Handle(a0), InputHandle
	clr.l loader.Frame.Handle(a0)  ; manifest handle transfers to this caller
	movea.l loader.Frame.Data(a0), a4
	.MEMORY_PROGRESS_BLOCK DosBase, #PROGRESS_PACKAGE, PackageSource, loader.Frame.Storage, loader.Frame.Bytes, loader.Frame.ReadCalls
	movea.l a4, a0
	jsr frontend.scratchSize
	bne.w closeBad
	move.l d1, d0
	addi.l #IO_SCRATCH_BYTES, d0
	bcs.w closeBad
	lea PrepBlock, a0
	jsr memory.reserveExact
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
	move.l #IO_BYTES, IoCapacity
	adda.l #IO_BYTES*(INCLUDE_DEPTH+1), a1
	move.l a1, LineBuffer
	adda.l #LINE_BYTES, a1
	move.l a1, frontend.Frame.Output(a0)
	move.l #RECORD_BYTES, frontend.Frame.Capacity(a0)
	move.l #includeBinary, frontend.Frame.FileInclude(a0)
	move.l #MetadataConfig, frontend.Frame.Metadata(a0)
	move.l #1, FrontStarted
	jsr frontend.begin
	bne.w closeBad
	.MEMORY_STAGE #0
	cmpi.w #2, CliMode
	beq.w cliDiscovery
	tst.w CliMode
	beq.w readManifest
	moveq #1, d0
	move.l #1, GraphMode
	clr.l DiscoverMode
	bra.w sourceCountReady
cliDiscovery
	.MEMORY_STAGE #6
	bsr.w prepareCliInputs
	.MEMORY_STAGE #0
	bne.w closeBad
	bra.w rootsDone
readManifest
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
	jsr memory.reserveExact
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
	.MEMORY_STAGE #6
	jsr discovery.scan
	.MEMORY_STAGE #0
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
	jsr memory.reserveExact
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
	tst.w CliMode
	bne.w candidateCountReady
	movea.l DosBase, a6
	move.l InputHandle, d1
	move.l #ManifestWord, d2
	moveq #1, d3
	jsr -42(a6)
	tst.l d0
	bne.w closeBad
candidateCountReady
	move.l CandidateCount, d0
sourceCountReady
	move.l d0, SourceCount
	clr.l SpanCount
	clr.l SelectedFileDerived
	tst.l DiscoverMode
	beq.w spanCapacityReady
	move.l #graph.MAX_SPANS, d0
spanCapacityReady
	mulu.w #SPAN_BYTES, d0
	lea FileSpans, a0
	jsr memory.reserveExact
	bne.w closeBad
	tst.l GraphMode
	beq.w manifestReady
	move.l #frontend.GRAPH_BYTES, d0
	lea GraphBlock, a0
	jsr memory.reserveExact
	bne.w closeBad
	movea.l memory.Block.Pointer(a0), a1
	lea Front, a0
	jsr frontend.beginGraph
	bne.w closeBad
	move.l #frontend.GRAPH_SPAN_BYTES, d0
	lea GraphSpans, a0
	jsr memory.reserveExact
	bne.w closeBad
manifestReady
	tst.l DiscoverMode
	beq.w filesReady
	move.l #declarations.SCRATCH_BYTES, d0
	lea DeclarationBlock, a0
	jsr memory.reserveExact
	bne.w closeBad
	movea.l memory.Block.Pointer(a0), a0
	jsr declarations.begin
	.MEMORY_STAGE #6
	bsr.w indexCandidates
	.MEMORY_STAGE #0
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
	lea Front, a0
	clr.w frontend.Frame.RootFile(a0)
	cmpi.l #1, SourceOrdinal
	bne.w metadataFileReady
	move.w #1, frontend.Frame.RootFile(a0)
metadataFileReady
	clr.l SourceLine
	bsr.w openSource
	bne.w closeBad
	tst.l DiscoverMode
	beq.w fileDerivedReady
	cmpi.l #1, SourceOrdinal
	bne.w fileDerivedSelection
	lea DeclarationBlock, a0
	movea.l memory.Block.Pointer(a0), a0
	moveq #1, d0
	jsr declarations.explicitFile
	tst.l d0
	bne.w fileDerivedReady
	move.l #1, SelectedFileDerived
fileDerivedSelection
	tst.l SelectedFileDerived
	beq.w fileDerivedReady
	bsr.w pathStem
	beq.w closeBad
	lea Front, a0
	tst.l GraphMode
	bne.w fileDerivedReady
	jsr frontend.beginFileDerived
	bne.w closeBad
fileDerivedReady
	tst.l DiscoverMode
	beq.w spanAllowed
	move.l SpanCount, d0
	cmpi.l #graph.MAX_SPANS, d0
	bhs.w closeBad
spanAllowed
	move.l SourceOrdinal, OriginId
	bsr.w retainOriginPath
	bne.w closeBad
	tst.l GraphMode
	beq.w streamedFile
	bsr.w captureFileBegin
	bne.w closeBad
	bra.w fileRecordsReady
streamedFile
	bsr.w fileSpan
	lea Records, a1
	move.l memory.Block.Used(a1), Span.Start(a0)
	move.l SourceOrdinal, Span.File(a0)
fileRecordsReady
	clr.l LineUsed
	move.l #1, SourceLine
	.MEMORY_PROGRESS_RECORDS DosBase, #PROGRESS_SOURCE_BEGIN, SourceOrdinal, SourceLine, Records, memory.Block.Used
sourceLoop
	.MEMORY_INPUT_BEGIN SourceBytes
	lea Stream, a0
	movea.l LineBuffer, a1
	move.l #LINE_BYTES, d0
	jsr line_input.next
	add.l d2, SourceBytes
	move.l d1, LineUsed
	.MEMORY_INPUT_END SourceBytes
	cmpi.l #line_input.EOF, d0
	beq.w sourceDone
	tst.l d0
	bne.w closeBad
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
	.MEMORY_PROGRESS_RECORDS DosBase, #PROGRESS_SOURCE_END, SourceOrdinal, SourceLine, Records, memory.Block.Used
	tst.l RequestedModule
	beq.w selectionComplete
	tst.l SelectedFileDerived
	bne.w selectionComplete
	cmpi.l #2, SelectionState
	bne.w closeBad
selectionComplete
fileDone
	bsr.w closeSource
	bne.w closeBad
	tst.l GraphMode
	beq.w streamedFileEnd
	bsr.w captureFileEnd
	bne.w closeBad
	bra.w sourceScheduling
streamedFileEnd
	lea Front, a0
	jsr frontend.endFileDerived
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
sourceScheduling
	tst.l DiscoverMode
	beq.w sequential
	.MEMORY_STAGE #6
	bsr.w resolveGraph
	.MEMORY_STAGE #0
	beq.w prepared
	cmpi.l #2, d0
	beq.w nextFile
	bra.w closeBad
sequential
	addq.l #1, SourceOrdinal
	move.l SourceCount, d0
	cmp.l SourceOrdinal, d0
	bhs.w nextFile
	tst.w CliMode
	bne.w prepared
	movea.l DosBase, a6
	move.l InputHandle, d1
	move.l #ManifestWord, d2
	moveq #1, d3
	jsr -42(a6)
	tst.l d0
	bne.w closeBad  ; trailing manifest bytes and read errors are invalid
prepared
	tst.l GraphMode
	beq.w sourcesPrepared
	bsr.w prepareCapturedSources
	bne.w closeBad
sourcesPrepared
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
	cmpi.w #1, CliMode
	bne.w graphReady
	lea GraphBlock, a0
	movea.l memory.Block.Pointer(a0), a0
	tst.w graph.GraphState.Count(a0)
	bne.w graphReady
	clr.l GraphMode  ; preserve the ordinary single-source path
graphReady
	tst.l GraphMode
	beq.w orderReady
	tst.l OrderedCount
	bne.w orderReady
	move.w #STEP_ORDER, PrepStep
	lea GraphSpans, a0
	movea.l memory.Block.Pointer(a0), a1
	move.l memory.Block.Capacity(a0), d0
	lea Front, a0
	jsr frontend.orderGraph
	bne.w completionBad
	move.l d1, OrderedCount
orderReady
	move.w #STEP_BIND, PrepStep
	.MEMORY_PROGRESS_RECORDS DosBase, #PROGRESS_ORDER, SourceOrdinal, SourceLine, Records, memory.Block.Used
	lea Records, a0
	movea.l memory.Block.Pointer(a0), a1
	move.l memory.Block.Used(a0), d0
	lea Front, a0
	jsr frontend.complete
	bne.w completionBad
	.MEMORY_PROGRESS_RECORDS DosBase, #PROGRESS_BIND, SourceOrdinal, SourceLine, Records, memory.Block.Used
	move.l frontend.Frame.NameCount(a0), NameCount
	tst.l GraphMode
	beq.w selected
	move.w #STEP_MATERIALIZE, PrepStep
	lea OrderFrame, a0
	move.l OrderedCount, ordered.Frame.OrderCount(a0)
	move.l SourceCount, ordered.Frame.SourceCount(a0)
	move.l #FileSpans, ordered.Frame.SourceSpans(a0)
	move.l SpanCount, ordered.Frame.SourceSpanCount(a0)
	move.l #GraphSpans, ordered.Frame.Spans(a0)
	move.l #Records, ordered.Frame.Records(a0)
	move.l #OriginSpans, ordered.Frame.Origins(a0)
	move.l OriginCount, ordered.Frame.OriginCount(a0)
	move.l #OrderedRecords, ordered.Frame.OrderedRecords(a0)
	move.l #OrderedFiles, ordered.Frame.OrderedOrigins(a0)
	move.l #RECORD_LIMIT, ordered.Frame.RecordLimit(a0)
	jsr ordered.materialize
	bne.w completionBad
	move.l ordered.Frame.OriginCount(a0), OriginCount
selected
	move.w #STEP_INDEX, PrepStep
	.MEMORY_PROGRESS_RECORDS DosBase, #PROGRESS_MATERIALIZE, SourceOrdinal, SourceLine, Records, memory.Block.Used
	lea Records, a0
	movea.l memory.Block.Pointer(a0), a1
	move.l memory.Block.Used(a0), d0
	lea Front, a0
	jsr frontend.indexBlocks
	bne.w completionBad
	tst.l d1
	beq.w blocksSelected
	tst.l GraphMode
	beq.w blocksSelected
	move.w #STEP_SELECT, PrepStep
	lea Records, a0
	movea.l memory.Block.Pointer(a0), a1
	move.l memory.Block.Used(a0), d0
	lea Front, a0
	jsr frontend.selectBlocks
	bne.w completionBad
	.MEMORY_PROGRESS DosBase, #PROGRESS_BLOCK_WORK, blocks.QueueAdds, blocks.Scanned, blocks.Marks
blocksSelected
	move.w #STEP_PARAMETERS, PrepStep
	.MEMORY_PROGRESS_RECORDS DosBase, #PROGRESS_INDEX, SourceOrdinal, SourceLine, Records, memory.Block.Used
	lea Front, a0
	jsr frontend.parameterBytes
	move.l d0, ParameterBytes
	beq.w parametersSaved
	lea Parameters, a0
	jsr memory.reserve
	bne.w completionBad
	movea.l memory.Block.Pointer(a0), a1
	lea Front, a0
	move.l ParameterBytes, d0
	jsr frontend.copyParameters
	bne.w completionBad
parametersSaved
	.MEMORY_PROGRESS_RECORDS DosBase, #PROGRESS_PARAMETERS, SourceOrdinal, SourceLine, Records, memory.Block.Used
	bsr.w clearIncludeText
	lea Front, a0
	jsr frontend.finish
	clr.l FrontStarted
	bsr.w releaseScheduledFrontend
	; Keep bounded origin filenames for diagnostics through assembly and output.
	; They never participate in packed execution; final cleanup releases them.
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
	lea PackageSource, a0
	movea.l loader.Frame.Data(a0), a4
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
	lea PackageSource, a0
	jsr loader.release
	lea Records, a0
	.MEMORY_LAYOUT package.Header.RuntimeBytes(a4), memory.Block.Used(a0), SourceBytes
	.MEMORY_PROGRESS_RECORDS DosBase, #PROGRESS_PREPARED, #0, #0, Records, memory.Block.Used
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
	; execute reports the still-live source path, then performs all cleanup.
bad
	; Retain partial packed-record and source counts in debug telemetry on a
	; preparation failure, before execute releases the owned buffers.
	.MEMORY_LAYOUT #0, Records+memory.Block.Used, SourceBytes
	moveq #1, d0
	rts
	.bend  ; prepare

; Configure the CLI's preparation-only discovery and include search storage.
; D0/CCR=status. The caller owns all blocks and releases them on either path.
prepareCliInputs	.block
	move.l #1, GraphMode
	move.l #1, DiscoverMode
	move.l #discovery.SCRATCH_BYTES+DISCOVERY_LIMIT*PATH_BYTES, d0
	lea DiscoveryBlock, a0
	jsr memory.reserveExact
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	move.l a1, DiscoveryScratch
	adda.l #discovery.SCRATCH_BYTES, a1
	move.l a1, DiscoveryPaths
	lea InputPlan, a0
	move.l #SourcePath, inputs.Frame.Entry(a0)
	move.l CliModuleRoots, inputs.Frame.Roots(a0)
	move.l CliModuleCount, inputs.Frame.Count(a0)
	move.l DiscoveryScratch, inputs.Frame.Scratch(a0)
	move.l #appendCandidate, inputs.Frame.Callback(a0)
	move.l DosBase, inputs.Frame.Dos(a0)
	move.l #IncludePath, inputs.Frame.Directory(a0)
	jsr inputs.seed
	bne.w bad
	move.l CliIncludeCount, RootCount
	move.l RootCount, d0
	lsl.l #8, d0
	lea RootPaths, a0
	jsr memory.reserveExact
	bne.w bad
	move.l RootCount, d0
	lsl.l #8, d0
	move.l d0, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	movea.l CliIncludeRoots, a0
	bsr.w copy
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; prepareCliInputs

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
	tst.w CliMode
	bne.w openPath
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
	; Retain the preparation path for errors from file finalization. The next
	; open replaces it; binary execution uses numeric provenance only.
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
	.MEMORY_INPUT_READ
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
	tst.l GraphMode
	beq.w streamPhysicalLine
	bsr.w capturePhysicalLine
	bne.w bad
	bra.w lowered
streamPhysicalLine
	lea Front, a0
	move.l OriginId, frontend.Frame.Origin(a0)
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
	movea.l LowerLineRoutine, a1
	move.l a1, d0
	beq.w streamedLine
	jsr (a1)
	bra.w lineReady
streamedLine
	jsr frontend.line
lineReady
	bne.w bad
	.MEMORY_DETAIL_BEGIN #3
	bsr.w appendPrepared
	.MEMORY_DETAIL_END #3
	bne.w bad
expanded
	lea Front, a0
	jsr frontend.nextExpansion
	bne.w bad
	move.l frontend.Frame.Used(a0), d0
	beq.w lowered
	.MEMORY_DETAIL_BEGIN #3
	bsr.w appendPrepared
	.MEMORY_DETAIL_END #3
	bne.w bad
	bra.w expanded
lowered
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

; Append one prepared numeric record. A segment invocation can call this more
; than once for one physical source line; every record keeps that line's origin.
appendPrepared	.block
	lea Records, a0
	move.l memory.Block.Used(a0), d0
	move.l d0, LineOffset
	lea Front, a1
	add.l frontend.Frame.Used(a1), d0
	bcs.w bad
	move.l #RECORD_LIMIT, d1
	jsr memory.reserveBounded
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
	rts
bad
	moveq #1, d0
	rts
	.bend  ; appendPrepared

run	.block
	move.l #1, InAssembly
	.MEMORY_PROGRESS_RECORDS DosBase, #PROGRESS_ASSEMBLE, #0, #0, Records, memory.Block.Used
	lea Work, a0
	clr.w assembly.Frame.Failure(a0)
	move.l #-1, assembly.Frame.RecordOffset(a0)
	move.l NameCount, d0
	beq.w bad
	cmpi.l #65536, d0
	bhi.w bad
	move.l d0, d1
	lsl.l #3, d0
	add.l d1, d0
	add.l d1, d0  ; one section identity byte per numeric symbol
	lea Symbols, a0
	jsr memory.reserve
	bne.w bad
	lea Context, a0
	lea Symbols, a1
	movea.l memory.Block.Pointer(a1), a2
	move.l a2, package.Context.Values(a0)
	move.l NameCount, d0
	lsl.l #3, d0
	adda.l d0, a2
	lea Context, a0
	move.l a2, package.Context.Defined(a0)
	adda.l NameCount, a2
	move.l a2, package.Context.SectionIds(a0)
	move.l NameCount, package.Context.Count(a0)
	lea Parameters, a1
	move.l memory.Block.Pointer(a1), package.Context.Parameters(a0)
	move.l ParameterBytes, d0
	divu.w #package.PARAMETER_BYTES, d0
	andi.l #$ffff, d0
	move.l d0, package.Context.ParameterCount(a0)
	lea RuntimeBlock, a1
	move.l memory.Block.Pointer(a1), package.Context.Package(a0)
	lea Work, a0
	lea Records, a1
	move.l memory.Block.Pointer(a1), assembly.Frame.Records(a0)
	move.l memory.Block.Used(a1), assembly.Frame.RecordBytes(a0)
	move.l #Context, assembly.Frame.Context(a0)
	move.l #allocateOutput, assembly.Frame.Allocate(a0)
	move.l #appendReloc, assembly.Frame.AddReloc(a0)
	clr.l assembly.Frame.Emitted(a0)
	cmpi.w #record_output.HEX, CliOutputKind
	beq.w captureSpans
	cmpi.w #record_output.SREC, CliOutputKind
	beq.w captureSpans
	lea MetadataConfig, a1
	tst.w metadata.Config.HexSet(a1)
	beq.w assemble
captureSpans
	move.l #appendEmission, assembly.Frame.Emitted(a0)
assemble
	jsr assembly.assemble
	.MEMORY_PROGRESS DosBase, #PROGRESS_ASSEMBLY_WORK, assembly.TraversalPasses, assembly.TraversalRecords, #0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; run

; A0=assembly.Frame,D1=address,D2=buffer offset,D3=initialized bytes.
; Capture only numeric final-pass spans for requested addressed output.
; D0/CCR=status; other registers preserved. No allocation for ordinary bin/Hunk.
appendEmission	.block
	movem.l a0-a1, -(sp)
	movea.l assembly.Frame.Sections(a0), a1
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a1)
	beq.w relocatable
	lea EmissionSpans, a0
	jsr output_spans.append
	bra.w done
relocatable
	moveq #0, d0
done
	movem.l (sp)+, a0-a1
	rts
	.bend  ; appendEmission

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

; A0=assembly.Frame,D0=source slot,D1=target slot,D2=section offset.
; Append an offset-based relocation while pass two emits in section order.
appendReloc	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l d0, d5
	move.l d1, d6
	move.l d2, d7
	lea RelocBlock, a0
	move.l memory.Block.Used(a0), d0
	add.l #assembly.OUTPUT_RELOC_BYTES, d0
	bcs.w bad
	jsr memory.reserve
	bne.w bad
	lea RelocBlock, a0
	movea.l memory.Block.Pointer(a0), a1
	adda.l memory.Block.Used(a0), a1
	move.l d5, assembly.OutputReloc.Source(a1)
	move.l d6, assembly.OutputReloc.Target(a1)
	move.l d7, assembly.OutputReloc.Offset(a1)
	addi.l #assembly.OUTPUT_RELOC_BYTES, memory.Block.Used(a0)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; appendReloc

; Execute requested artifacts after one completed assembly. The harness retains
; its explicit capture path; Shell invocations additionally honor source paths.
writeOutputs	.block
	tst.w CliOutputKind
	beq.w explicit
	cmpi.w #3, CliOutputKind
	beq.w sources
explicit
	cmpi.w #record_output.HEX, CliOutputKind
	beq.w recordArtifact
	cmpi.w #record_output.SREC, CliOutputKind
	beq.w recordArtifact
	move.w CliOutputKind, d0
	move.w d0, SelectedOutputKind
	bsr.w selectOutput
	bne.w bad
	move.l OutputName, SelectedOutputPath
	clr.l PrefixBytes
	bsr.w writeSelected
	bne.w bad
	tst.w CliOutputKind
	beq.w complete
	bra.w sources
recordArtifact
	move.w CliOutputKind, d0
	move.w d0, SelectedOutputKind
	move.l OutputName, SelectedOutputPath
	bsr.w writeRecords
	bne.w bad
sources
	tst.w CliMode
	beq.w sourceDescriptors
	lea MetadataOutput, a0
	tst.w metadata_output.Frame.HexSet(a0)
	beq.w sourceDescriptors
	lea metadata_output.Frame.ResolvedHex(a0), a1
	move.l a1, SelectedOutputPath
	move.w #record_output.HEX, SelectedOutputKind
	bsr.w writeRecords
	bne.w bad
sourceDescriptors
	lea OutputCursor, a0
	lea Records, a1
	move.l memory.Block.Pointer(a1), output_plan.Cursor.Records(a0)
	move.l memory.Block.Used(a1), output_plan.Cursor.Bytes(a0)
	clr.l output_plan.Cursor.Offset(a0)
next
	lea OutputCursor, a0
	jsr output_plan.next
	cmpi.l #output_plan.END, d0
	beq.w complete
	tst.l d0
	bne.w bad
	move.w d1, SelectedOutputKind
	lea OutputPath, a3
	move.l a3, SelectedOutputPath
	move.l d3, d4
copyPath
	move.b (a2)+, (a3)+
	subq.w #1, d4
	bne.w copyPath
	clr.b (a3)
	clr.l PrefixBytes
	cmpi.w #output_plan.HUNK, d1
	beq.w hunkArtifact
	move.l d2, d0
	lea Work, a5
	move.l assembly.Frame.Used(a5), d1
	movea.l assembly.Frame.Sections(a5), a0
	jsr output_plan.range
	bne.w bad
	lea Output, a0
	add.l memory.Block.Pointer(a0), d1
	bcs.w bad
	move.l d1, WritePointer
	move.l d2, WriteBytes
	cmpi.w #output_plan.PRG, SelectedOutputKind.l
	bne.w write
	cmpi.l #$ffff, d3
	bhi.w bad
	lea Prefix, a0
	move.b d3, (a0)+
	lsr.w #8, d3
	move.b d3, (a0)
	move.l #2, PrefixBytes
	bra.w write
hunkArtifact
	bsr.w selectOutput
	bne.w bad
write
	bsr.w writeSelected
	bne.w bad
	bra.w next
complete
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; writeOutputs

; Transport only. All format selection and buffers are complete before IO.
writeSelected	.block
	lea OutputWrite, a0
	move.l SelectedOutputPath, output_io.Frame.Path(a0)
	move.l WritePointer, output_io.Frame.Data(a0)
	move.l WriteBytes, output_io.Frame.Bytes(a0)
	move.l #Prefix, output_io.Frame.Prefix(a0)
	move.l PrefixBytes, output_io.Frame.PrefixBytes(a0)
	clr.l output_io.Frame.Generator(a0)
	clr.l output_io.Frame.Context(a0)
	movea.l DosBase, a6
	jsr output_io.write
	rts
	.bend  ; writeSelected

; Format sparse initialized spans through a fixed 4 KiB transport buffer.
; D0/CCR=status; caller owns one assembly result and all requested artifacts.
writeRecords	.block
	lea Work, a0
	movea.l assembly.Frame.Sections(a0), a1
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a1)
	beq.w bad
	lea TextBuffer, a0
	move.l #4096, d0
	jsr memory.reserveExact
	bne.w bad
	lea RecordFrame, a1
	move.l memory.Block.Pointer(a0), record_output.Frame.Buffer(a1)
	move.l #4096, record_output.Frame.Capacity(a1)
	lea Output, a0
	move.l memory.Block.Pointer(a0), record_output.Frame.Data(a1)
	lea Work, a0
	move.l assembly.Frame.Used(a0), record_output.Frame.DataBytes(a1)
	lea EmissionSpans, a0
	move.l memory.Block.Pointer(a0), record_output.Frame.Spans(a1)
	move.l memory.Block.Used(a0), record_output.Frame.SpanBytes(a1)
	move.w SelectedOutputKind, record_output.Frame.Format(a1)
	move.w OutputStartSet, record_output.Frame.StartSet(a1)
	move.l OutputStart, record_output.Frame.Start(a1)
	movea.l a1, a0
	jsr record_output.begin
	bne.w bad
	lea OutputWrite, a0
	move.l SelectedOutputPath, output_io.Frame.Path(a0)
	clr.l output_io.Frame.PrefixBytes(a0)
	move.l #record_output.next, output_io.Frame.Generator(a0)
	move.l #RecordFrame, output_io.Frame.Context(a0)
	movea.l DosBase, a6
	jsr output_io.write
	.MEMORY_PROGRESS DosBase, #PROGRESS_RECORD_OUTPUT, output_spans.Events, record_output.RecordCount, record_output.OutputBytes
	rts
bad
	moveq #1, d0
	rts
	.bend  ; writeRecords

; Build selected native Hunk sections from numeric assembly metadata.
; The flat path keeps its existing write buffer. D0/CCR=status.
selectOutput	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea Work, a5
	move.l assembly.Frame.Used(a5), WriteBytes
	lea Output, a0
	move.l memory.Block.Pointer(a0), WritePointer
	movea.l assembly.Frame.Sections(a5), a6
	cmpi.w #1, SelectedOutputKind
	beq.w requireFlat
	cmpi.w #2, SelectedOutputKind
	bne.w sourceFormat
	cmpi.w #5, sections.State.Mode(a6)
	bne.w badSelect
	bra.w sourceFormat
requireFlat
	cmpi.w #5, sections.State.Mode(a6)
	beq.w badSelect
sourceFormat
	cmpi.w #5, sections.State.Mode(a6)
	bne.w selected
	lea HunkParts, a4
	lea HunkFrame, a1
	clr.l hunk.Frame.Count(a1)
	lea SlotToPart, a1
	moveq #7, d0
clearPartMap
	move.b #$ff, (a1)+
	dbra d0, clearPartMap
	moveq #0, d7
	move.w sections.State.OrderCount(a6), d6
	lea sections.ORDER(a6), a3
part
	tst.w d6
	beq.w serialize
	moveq #0, d0
	move.b (a3)+, d0
	move.l d0, d4
	mulu.w #sections.HUNK_SLOT_BYTES, d0
	lea sections.HUNK_SLOTS(a6), a2
	adda.l d0, a2
	moveq #0, d1
	move.w sections.HunkSlot.Kind(a2), d1
	move.l sections.HunkSlot.Used(a2), d2
	cmpi.w #3, d1
	beq.w keepPart
	tst.l d2
	beq.w nextPart
keepPart
	lea SlotToPart, a1
	move.b d7, 0(a1, d4.w)
	lea PartSlots, a1
	move.b d4, 0(a1, d7.w)
	move.w d1, hunk.Part.Kind(a4)
	clr.w hunk.Part.Reserved(a4)
	clr.l hunk.Part.Data(a4)
	move.l d2, hunk.Part.Used(a4)
	move.l sections.HunkSlot.Allocation(a2), hunk.Part.Size(a4)
	clr.l hunk.Part.Fixups(a4)
	clr.l hunk.Part.FixupCount(a4)
	cmpi.w #3, d1
	beq.w partReady
	move.l sections.HunkSlot.Start(a2), d0
	add.l d2, d0
	bcs.w badSelect
	cmp.l assembly.Frame.Used(a5), d0
	bhi.w badSelect
	move.l memory.Block.Pointer(a0), d0
	add.l sections.HunkSlot.Start(a2), d0
	bcs.w badSelect
	move.l d0, hunk.Part.Data(a4)
partReady
	adda.w #hunk.SEGMENT_BYTES, a4
	addq.l #1, d7
nextPart
	subq.w #1, d6
	bra.w part
serialize
	bsr.w collectRelocs
	bne.w badSelect
	lea HunkFrame, a0
	move.l #HunkParts, hunk.Frame.Segments(a0)
	move.l d7, hunk.Frame.Count(a0)
	clr.l hunk.Frame.Output(a0)
	clr.l hunk.Frame.Capacity(a0)
	jsr hunk.build
	bne.w badSelect
	move.l hunk.Frame.Used(a0), d0
	lea HunkBlock, a0
	jsr memory.reserve
	bne.w badSelect
	movea.l a0, a1
	lea HunkFrame, a0
	move.l memory.Block.Pointer(a1), hunk.Frame.Output(a0)
	move.l memory.Block.Capacity(a1), hunk.Frame.Capacity(a0)
	jsr hunk.build
	bne.w badSelect
	move.l hunk.Frame.Output(a0), WritePointer
	move.l hunk.Frame.Used(a0), WriteBytes
selected
	moveq #0, d0
	bra.w selectDone
badSelect
	moveq #1, d0
selectDone
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; selectOutput

; Convert slot-number relocation records to emitted Hunk indices. Output parts
; stay in selected order; each part's fixups are contiguous and source-ordered.
collectRelocs	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea RelocBlock, a0
	move.l memory.Block.Used(a0), d0
	cmpi.l #786420, d0
	bhi.w bad
	divu.w #assembly.OUTPUT_RELOC_BYTES, d0
	move.l d0, d1
	swap d1
	tst.w d1
	bne.w bad
	moveq #0, d1
	move.w d0, d1
	mulu.w #hunk.FIXUP_BYTES, d1
	move.l d1, d0
	lea HunkRelocs, a0
	jsr memory.reserve
	bne.w bad
	clr.l memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a5
	move.l d7, d6
	moveq #0, d4
part
	cmp.l d6, d4
	bhs.w doneParts
	move.l d4, d0
	mulu.w #hunk.SEGMENT_BYTES, d0
	lea HunkParts, a4
	adda.l d0, a4
	clr.l hunk.Part.FixupCount(a4)
	move.l a5, hunk.Part.Fixups(a4)
	lea PartSlots, a0
	moveq #0, d3
	move.b 0(a0, d4.w), d3
	lea RelocBlock, a0
	movea.l memory.Block.Pointer(a0), a6
	move.l memory.Block.Used(a0), d5
record
	tst.l d5
	beq.w nextPart
	cmpi.l #assembly.OUTPUT_RELOC_BYTES, d5
	blo.w bad
	cmp.l assembly.OutputReloc.Source(a6), d3
	bne.w skipRecord
	move.l assembly.OutputReloc.Target(a6), d0
	cmpi.l #8, d0
	bhs.w bad
	lea SlotToPart, a0
	moveq #0, d1
	move.b 0(a0, d0.w), d1
	cmpi.w #$ff, d1
	beq.w bad
	move.l d1, hunk.Reloc.Target(a5)
	move.l assembly.OutputReloc.Offset(a6), hunk.Reloc.Offset(a5)
	addq.l #1, hunk.Part.FixupCount(a4)
	adda.w #hunk.FIXUP_BYTES, a5
skipRecord
	adda.w #assembly.OUTPUT_RELOC_BYTES, a6
	subi.l #assembly.OUTPUT_RELOC_BYTES, d5
	bra.w record
nextPart
	addq.l #1, d4
	bra.w part
doneParts
	lea HunkRelocs, a0
	move.l a5, d0
	sub.l memory.Block.Pointer(a0), d0
	move.l d0, memory.Block.Used(a0)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; collectRelocs
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
	.include "binary_source_schedule.i"
	.include "binary_source_discovery_idx.i"
	.include "binary_source_selection.i"
	.include "binary_source_includes.i"
	.include "binary_source_assets.i"
	.endsection
	.section data, kind=data
GoFailure	.byte "compact CLI: --go requires Hex or S-record output (CLI or metadata)", 10, 0
DosName	.byte "dos.library", 0
OutputFailure	.byte "compact CLI: output failed", 10, 0
PackageFailure	.byte "package: missing, invalid or incompatible runtime package", 10
PackageFailureEnd
SelectedModuleKeyword	.byte "module"
SelectedEndmoduleKeyword	.byte "endmodule"
FailureMessage	.byte "binary source: unsupported or invalid input [file "
FailureFile	.byte "00000000"
	.byte ", line "
FailureLine	.byte "00000000", "]", 10
FailureMessageEnd
MutableLayoutFailure	.byte "mutable declarations are not supported in mapped outputs until source-order traversal is implemented", 10
MutableLayoutFailureEnd
FailurePrepStep	.byte "preparation step: "
FailurePrepStepValue	.byte "00000000", 10
FailurePrepStepEnd
FailurePath	.byte "source: "
FailureSourceLine	.byte "line: "
FailureNewline	.byte 10
	.endsection
	.section bss, kind=bss
	.align 4
; Match line_input.State; real labels retain Hunk relocation information.
; The include stack saves Handle/Buffer/Cursor/End by their existing names.
Stream
IoCursor	.res long, 1
IoEnd	.res long, 1
IoBuffer	.res long, 1
IoCapacity	.res long, 1
SourceHandle	.res long, 1
DosBase	.res long, 1
InputName	.res long, 1
OutputName	.res long, 1
CliMode	.res word, 1
CliOutputKind	.res word, 1
LowerLineRoutine	.res long, 1
	.align 4
CliModuleRoots	.res long, 1
CliModuleCount	.res long, 1
CliIncludeRoots	.res long, 1
CliIncludeCount	.res long, 1
InputPlan	.res byte, inputs.Frame.Directory+4
ReturnCode	.res long, 1
InputHandle	.res long, 1
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
RequestedNameOffset	.res long, 1
RequestedNameBytes	.res long, 1
SelectionState	.res long, 1
SelectedFileDerived	.res long, 1
OrderedCount	.res long, 1
GraphBlock	.res byte, memory.Block.Used+4
GraphSpans	.res byte, memory.Block.Used+4
CaptureRegions	.res byte, memory.Block.Used+4
CaptureRegionCount	.res long, 1
CaptureRegionCurrent	.res long, 1
CaptureBytes	.res long, 1
CaptureDerived	.res long, 1
CaptureLine	.res byte, memory.Block.Used+4
CaptureRequest	.res byte, capture.FRAME_BYTES
ConfigScan	.res byte, configuration.FRAME_BYTES
CaptureOrder	.res byte, memory.Block.Used+4
CaptureOrderCount	.res long, 1
CaptureOrderIndex	.res long, 1
ScheduleCursor	.res long, 1
ScheduleEnd	.res long, 1
ScheduleFile	.res long, 1
ScheduleDerived	.res long, 1
ConfigFront	.res byte, frontend.FRAME_BYTES
ConfigFrontStarted	.res long, 1
SemanticScratch	.res byte, memory.Block.Used+4
OrderFrame	.res byte, ordered.FRAME_BYTES
OrderedRecords	.res byte, memory.Block.Used+4
OrderedFiles	.res byte, memory.Block.Used+4
OriginSpans	.res byte, memory.Block.Used+4
OriginCount	.res long, 1
RootPaths	.res byte, memory.Block.Used+4
RootCount	.res long, 1
IncludeRootsRemaining	.res long, 1
NextOrigin	.res long, 1
OriginId	.res long, 1
OriginPaths	.res byte, memory.Block.Used+4
LineOffset	.res long, 1
IncludeDepthNow	.res long, 1
IncludeStack	.res byte, INCLUDE_FRAME_BYTES*INCLUDE_DEPTH
IncludePath	.res byte, PATH_BYTES
IncludeName	.res byte, PATH_BYTES
AllowedPath	.res byte, PATH_BYTES
IncludeHandle	.res long, 1
SourceOrdinal	.res long, 1
SourceLine	.res long, 1
InAssembly	.res long, 1
PrepStep	.res word, 1
FileSpans	.res byte, memory.Block.Used+4
ManifestWord	.res word, 1
SourcePath	.res byte, 256
	.align 4
FrontStarted	.res long, 1
LineBuffer	.res long, 1
LineUsed	.res long, 1
SourceBytes	.res long, 1
NameCount	.res long, 1
Front	.res byte, frontend.FRAME_BYTES
Work	.res byte, assembly.FRAME_BYTES
Context	.res byte, package.CONTEXT_BYTES
PackageSource	.res byte, loader.FRAME_BYTES
PackageStatus	.res long, 1
RuntimeBlock	.res byte, memory.Block.Used+4
PrepBlock	.res byte, memory.Block.Used+4
Records	.res byte, memory.Block.Used+4
Symbols	.res byte, memory.Block.Used+4
Parameters	.res byte, memory.Block.Used+4
ParameterBytes	.res long, 1
Output	.res byte, memory.Block.Used+4
HunkBlock	.res byte, memory.Block.Used+4
RelocBlock	.res byte, memory.Block.Used+4
HunkRelocs	.res byte, memory.Block.Used+4
HunkFrame	.res byte, hunk.Frame.Used+4
HunkParts	.res byte, hunk.MAX_SEGMENTS*hunk.SEGMENT_BYTES
SlotToPart	.res byte, 8
PartSlots	.res byte, 8
EmissionSpans	.res byte, memory.Block.Used+4
TextBuffer	.res byte, memory.Block.Used+4
RecordFrame	.res byte, record_output.FRAME_BYTES
OutputStartSet	.res word, 1
OutputStart	.res long, 1
MetadataConfig	.res byte, metadata.CONFIG_BYTES
MetadataOutput	.res byte, metadata_output.FRAME_BYTES
	.align 4
OutputCursor	.res byte, output_plan.CURSOR_BYTES
OutputWrite	.res byte, output_io.FRAME_BYTES
OutputPath	.res byte, PATH_BYTES
SelectedOutputPath	.res long, 1
SelectedOutputKind	.res word, 1
Prefix	.res byte, 2
PrefixBytes	.res long, 1
WritePointer	.res long, 1
WriteBytes	.res long, 1
	.endsection
	.endmodule

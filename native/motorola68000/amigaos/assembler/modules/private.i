; Sequential private compilation before retained semantic publication.
PRIVATE_NUMBER_COUNT = 17
PROGRESS_PRIVATE_COMPILE = 454
PROGRESS_PRIVATE_FAILURE = 455
PROGRESS_PRIVATE_POSITION = 456
PROGRESS_PRIVATE_SECTIONS = 457
PROGRESS_PRIVATE_SELECTION = 458
; Existing progress ABI: F=captured file ordinal,L=returned status,R=zero.
; Snapshot D0/D7 before MEMORY_PROGRESS loads its own argument registers.
; Execution failure adds only retained scalar snapshots: 455 F=file,L=stage,
; R=name; 456 F=pass,L=sweep,R=mode; 457 F=mode,L=section,R=count;
; 458 F=priority,L=recipe,R=projection. No released record pointers are read.
; The three existing gates keep both call and preservation absent in release.
PRIVATE_RESULT	.macro phase, status
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	move.l d4, -(sp)
	move.l .status, d4
	.MEMORY_PROGRESS DosBase, .phase, PublishedCapture+Span.File, d4, #0
	cmpi.l #moduleCompile.INVALID, d4
	bne.w executionFailure
	; The existing preparation stage is scalar evidence and survives retirement.
	.MEMORY_PROGRESS DosBase, #PROGRESS_PRIVATE_FAILURE, PublishedCapture+Span.File, frontend.PreparationStage, #0
	.BINDING_DIAGNOSTIC_REPORT DosBase
	bra.w done
executionFailure
	cmpi.l #moduleCompile.EXECUTION_INVALID, d4
	bne.w done
	.MEMORY_PROGRESS DosBase, #PROGRESS_PRIVATE_FAILURE, PublishedCapture+Span.File, assembly.FailureStage, assembly.FailureName
	.MEMORY_PROGRESS_BLOCK DosBase, #PROGRESS_PRIVATE_POSITION, assembly.Position, AssemblyPosition.Pass, AssemblyPosition.Sweep, AssemblyPosition.Mode
	.MEMORY_PROGRESS_BLOCK DosBase, #PROGRESS_PRIVATE_SECTIONS, assembly.Position, AssemblyPosition.Mode, AssemblyPosition.Section, AssemblyPosition.Count
	.MEMORY_PROGRESS_BLOCK DosBase, #PROGRESS_PRIVATE_SELECTION, encoding.Selection, SelectionPosition.Priority, SelectionPosition.Recipe, SelectionPosition.Projection
done
	move.l (sp)+, d4
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
PrivateSection	.struct
Name	.res PATH_BYTES
Bytes	.long ?
Align	.long ?
Kind	.long ?
.endstruct
PRIVATE_SECTION_BYTES = PrivateSection.Kind+4
PrivateLinks	.struct
Refs	.res memory.Block.Used+4
Public	.res memory.Block.Used+4
.endstruct
PRIVATE_LINK_BYTES = PrivateLinks.Public+memory.Block.Used+4
; Active semantic Front at a file boundary. D0=status,D1=handled (0 fallback).
; Tentative work owns separate scopes/records. Only a successful image can
; enter retained module ownership; unsupported inputs replay normally.
tryPrivateModule	.block
	movem.l d2-d7/a0-a6, -(sp)
	moveq #0, d1
	tst.w ReferenceModules
	bne.w fallback
	bsr.w privateCheckText
	bne.w fallback
	bsr.w privateReleaseAttempt
	movea.l Front+frontend.Frame.Scratch, a0
	lea frontend.SCOPE_STATE(a0), a0
	move.l a0, PrivateGlobalScope
	bsr.w beginStoredInterfaces
	bne.w failed
	tst.l PrivateRequestMode
	beq.w configurationOwner
	move.l a0, PrivateConfigScope
	bsr.w privateSelectRequest
	bne.w fallback
	bra.w requestReady
configurationOwner
	movea.l ConfigFront+frontend.Frame.Scratch, a0
	move.l a0, d0
	beq.w fallback
	adda.l #frontend.SCOPE_STATE, a0
	move.l a0, PrivateConfigScope
	bsr.w privateFindCapture
	bne.w fallback
requestReady
	clr.w PrivateReused
	lea ModuleProgress, a0
	lea PrivateModuleName, a1
	move.l PrivateModuleBytes, d0
	clr.b 0(a1, d0.l)  ; request-table names are length-delimited
	moveq #progress.PROCESSING, d0
	jsr progress.begin
	bsr.w incrementalTry
	bne.w failed
	tst.l d1
	beq.w compileFresh
	move.w #1, PrivateReused
	bra.w cached
compileFresh
	move.l Front+frontend.Frame.Package, PrivateCompile+moduleCompile.Context.Package
	move.l #privateInitialize, PrivateCompile+moduleCompile.Context.Initialize
	move.l #privateReplay, PrivateCompile+moduleCompile.Context.Replay
	move.l #privateSeed, PrivateCompile+moduleCompile.Context.Seed
	move.l #privateExternal, PrivateCompile+moduleCompile.Context.Resolve
	move.l #privateInventory, PrivateCompile+moduleCompile.Context.Inventory
	move.l #PrivateFacets, PrivateCompile+moduleCompile.Context.Facets
	move.l #privateCloseTemplates, PrivateCompile+moduleCompile.Context.CloseTemplates
	move.l #privateCloseValues, PrivateCompile+moduleCompile.Context.CloseValues
	move.l #PrivateObjectDependencies, PrivateCompile+moduleCompile.Context.Dependencies
	move.l #PrivateObjectImports, PrivateCompile+moduleCompile.Context.Imports
	move.l #privateSealInterfaces, PrivateCompile+moduleCompile.Context.Seal
	lea PrivateCompile, a0
	lea PrivateImage, a1
	jsr moduleCompile.compile
	move.l d0, d7
	.PRIVATE_RESULT #PROGRESS_PRIVATE_COMPILE, d7
	lea Front, a0
	jsr frontend.activate
	bne.w failed
	jsr source_control.begin
	bne.w failed
	tst.l d7
	bne.w fallback
	bsr.w privateEligibleImage
	bne.w fallback
	bsr.w incrementalSave
	bne.w failed
cached
	bsr.w privatePublish
	bne.w failed
	bsr.w incrementalPublished
	bne.w failed
	addq.l #1, PrivatePublishedCount
	moveq #1, d1
	moveq #0, d0
	bra.w release
fallback
	moveq #0, d1
	moveq #0, d0
	bra.w release
failed
	moveq #0, d1
	moveq #1, d0
release
	bsr.w finishModuleProgress
	move.l d0, d7
	bsr.w privateReleaseAttempt
	bsr.w incrementalReleaseAttempt
	move.l d7, d0
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; tryPrivateModule

; Reference modules may register encodings before a later private candidate.
; Inspect only newly retained records; a registry effect keeps later modules
; on ordinary replay because private text registries do not inherit definitions.
privateCheckText	.block
	movem.l d1-d2/a0-a1, -(sp)
	tst.l PrivateTextUnsafe
	bne.w bad
	move.l Records+memory.Block.Used, d2
	move.l d2, d0
	sub.l PrivateTextChecked, d0
	bcs.w hazard
	beq.w good
	tst.l CaptureDirectory
	bne.w stored
	movea.l Records+memory.Block.Pointer, a0
	adda.l PrivateTextChecked, a0
	movea.l Front+frontend.Frame.Package, a1
	jsr moduleAdmission.textSafe
	bne.w hazard
	move.l d2, PrivateTextChecked
	bra.w good
stored
	bsr.w privateCheckStoredText
	bne.w hazard
	move.l d2, PrivateTextChecked
good
	moveq #0, d0
	bra.w done
hazard
	move.l #1, PrivateTextUnsafe
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; privateCheckText

; Disk prepared ownership has no contiguous Records payload. Inspect only the
; appended suffix through bounded read leases; never leave a page borrowed.
; D0/CCR=status; other registers preserved.
privateCheckStoredText	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea PreparedView, a0
	lea PreparedStore, a1
	jsr views.initStore
	bne.w done
	move.l PrivateTextChecked, d4
next
	cmp.l Records+memory.Block.Used, d4
	bhs.w good
	move.l d4, d1
	lea PreparedView, a0
	jsr views.seek
	bne.w failed
	move.l d2, d5
	movea.l a1, a0
	move.l d2, d0
	movea.l Front+frontend.Frame.Package, a1
	jsr moduleAdmission.textSafe
	bne.w failed
	add.l d5, d4
	bra.w next
good
	moveq #0, d7
	bra.w finish
failed
	moveq #1, d7
finish
	lea PreparedView, a0
	jsr views.finish
	or.l d7, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; privateCheckStoredText

; Identify the configuration node by its exact captured extent. Copy only the
; owning public module name; source spellings never escape this attempt.
privateFindCapture	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l GraphBlock+memory.Block.Pointer, a0
	moveq #0, d7
	move.w graph.GraphState.Count(a0), d7
	lea graph.NODES(a0), a1
next
	tst.l d7
	beq.w bad
	move.l graph.Node.Start(a1), d0
	cmp.l PublishedCapture+Span.Start, d0
	bne.w advance
	move.l graph.Node.End(a1), d0
	cmp.l PublishedCapture+Span.End, d0
	bne.w advance
	moveq #0, d0
	move.w graph.Node.File(a1), d0
	cmp.l PublishedCapture+Span.File, d0
	bne.w advance
	moveq #0, d0
	move.w graph.Node.Binding(a1), d0
	beq.w bad
	move.l d0, PrivateConfigModule
	subq.l #1, d0
	movea.l PrivateConfigScope, a0
	lea layout.LEXICAL_STATE(a0), a0
	lea PrivateEntry, a1
	jsr lexical.readEntry
	bne.w bad
	moveq #0, d1
	move.w records.Entry.Length(a1), d1
	move.l d1, PrivateModuleBytes
	move.l records.Entry.Name(a1), d0
	lea PrivateModuleName, a1
	jsr lexical.copyName
	bra.w done
advance
	adda.w #graph.NODE_BYTES, a1
	subq.l #1, d7
	bra.w next
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; privateFindCapture

; Compiler initializer: A0=private frontend,A1=compiler Context. Read only
; direct dependencies from configuration; unrelated public templates do not
; make otherwise independent modules ineligible.
privateInitialize	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l a0, PrivateFrontend
	movea.l frontend.Frame.Scratch(a0), a6
	adda.l #frontend.SCOPE_STATE, a6
	tst.l CaptureDirectory
	beq.w storageReady
	lea layout.LEXICAL_STATE(a6), a0
	movea.l DosBase, a1
	movea.l CaptureDirectory, a2
	jsr lexical.configureScratch
	bne.w bad
storageReady
	lea StoredPrivate, a0
	movea.l PrivateFrontend, a1
	lea PrivateRegistry, a2
	jsr moduleProvider.begin
	bne.w bad
	move.l #storedInterfaceAddress, StoredPrivate+moduleProvider.State.Address
	move.l #StoredPrivate, StoredPrivate+moduleProvider.State.AddressContext
	; No interface lease survives between compilers. Reuse the idle root's
	; reader window instead of allocating a second cache during preparation.
	tst.l StoredRoot+moduleProvider.State.Reader+moduleInterfaceStore.State.Window+memory.Block.Pointer
	beq.w readerReady
	bsr.w storedInterfaceSwapCache
readerReady
	movea.l PrivateConfigScope, a0
	movea.l a6, a1
	jsr scopes.seedConfiguration
	bne.w bad
	tst.l PrivateRequestMode
	beq.w configurationImports
	movea.l PrivateRequestCurrent, a0
	move.l PrivateRequest.Start(a0), d7
	move.l PrivateRequest.End(a0), PrivateRequestEnd
	bra.w next
configurationImports
	movea.l PrivateConfigScope, a5
	move.l PrivateConfigModule, d0
	subq.l #1, d0
	add.l d0, d0
	movea.l scopes.IMPORT_STATE+imports.HEADS_POINTER(a5), a0
	moveq #0, d7
	move.w 0(a0, d0.l), d7
next
	tst.l PrivateRequestMode
	beq.w importItem
	cmp.l PrivateRequestEnd, d7
	bhs.w good
	movea.l PrivateRequestRows+memory.Block.Pointer, a0
	adda.l d7, a0
	move.l PrivateRequestName.Bytes(a0), PrivateDependencyBytes
	move.l PrivateRequestName.Name(a0), d0
	movea.l PrivateRequestNames+memory.Block.Pointer, a0
	adda.l d0, a0
	lea PrivateDependencyName, a1
	move.l PrivateDependencyBytes, d0
	bsr.w copy
	move.l d7, d6
	addi.l #PRIVATE_REQUEST_NAME_BYTES, d6
	bra.w dependencyReady
importItem
	tst.l d7
	beq.w good
	cmp.w scopes.IMPORT_STATE+imports.COUNT(a5), d7
	bhi.w bad
	move.l d7, d0
	subq.l #1, d0
	mulu.w #imports.ITEM_BYTES, d0
	movea.l scopes.IMPORT_STATE+imports.ITEMS_POINTER(a5), a0
	adda.l d0, a0
	moveq #0, d6
	move.w imports.Item.Next(a0), d6
	moveq #0, d0
	move.w imports.Item.Target(a0), d0
	movea.l a5, a0
	adda.l #layout.LEXICAL_STATE, a0
	lea PrivateEntry, a1
	jsr lexical.readEntry
	bne.w bad
	moveq #0, d1
	move.w records.Entry.Length(a1), d1
	move.l d1, PrivateDependencyBytes
	move.l records.Entry.Name(a1), d0
	lea PrivateDependencyName, a1
	jsr lexical.copyName
	bne.w bad
dependencyReady
	bsr.w privateFindGlobalModule
	bne.w bad
	move.l d1, d5
	lea PrivateDependencies, a0
	; Keep every declared edge for publication ordering, including repeated
	; targets under distinct aliases. Each stored provider is registered once.
	move.l memory.Block.Used(a0), d4
	move.l d4, d0
	addq.l #4, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	move.l d5, 0(a1, d4.l)
	addq.l #4, memory.Block.Used(a0)
	move.l d4, d0  ; compare only earlier entries, not this new edge
deduplicate
	tst.l d0
	beq.w seed
	cmp.l (a1)+, d5
	beq.w advance
	subq.l #4, d0
	bra.w deduplicate
seed
	move.l d5, d1
	bsr.w storedInterfaceModuleId
	bne.w bad
	move.l d1, d3
	movea.l a6, a0
	lea PrivateDependencyName, a1
	move.l PrivateDependencyBytes, d0
	bsr.w storedInterfaceModule
	bne.w bad
	lea StoredPrivate, a0
	move.l d3, d2
	jsr moduleProvider.add
	bne.w bad
advance
	move.l d6, d7
	bra.w next
good
	moveq #0, d0
	bra.w done
bad
	moveq #moduleCompile.REPLAY_UNSUPPORTED, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; privateInitialize

; D1=retained module index+1 matching PrivateDependencyName, no insertion.
privateFindGlobalModule	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l PrivateGlobalScope, a6
	moveq #0, d7
	move.w layout.State.Count(a6), d7
	moveq #0, d6
next
	cmp.l d7, d6
	bhs.w bad
	move.l d6, d0
	add.l d0, d0
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a6), a0
	move.w 0(a0, d0.l), d0
	andi.w #modules.MODULE_ID, d0
	beq.w advance
	lea layout.LEXICAL_STATE(a6), a0
	lea PrivateEntry, a1
	move.l d6, d0
	jsr lexical.readEntry
	bne.w bad
	moveq #0, d1
	move.w records.Entry.Length(a1), d1
	cmp.l PrivateDependencyBytes, d1
	bne.w advance
	move.l records.Entry.Name(a1), d0
	lea PrivateCompareName, a1
	jsr lexical.copyName
	bne.w bad
	lea PrivateDependencyName, a0
	lea PrivateCompareName, a1
	move.l PrivateDependencyBytes, d2
compare
	move.b (a0)+, d0
	bsr.w privateFold
	move.b d0, d3
	move.b (a1)+, d0
	bsr.w privateFold
	cmp.b d3, d0
	bne.w advance
	subq.l #1, d2
	bne.w compare
	move.l d6, d1
	addq.l #1, d1
	moveq #0, d0
	bra.w done
advance
	addq.l #1, d6
	bra.w next
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; privateFindGlobalModule
privateFold	.block
	cmpi.b #'A', d0
	blo.w done
	cmpi.b #'Z', d0
	bhi.w done
	addi.b #32, d0
done
	rts
	.bend  ; privateFold
; Compiler replay callback swaps only owned frame/block descriptors; every
; app cursor/provenance counter is restored before returning to the producer.
; Activate each moved Frame before callbacks borrow its scratch back-pointers.
; File actions reject before acquisition, so fallback has no external effects.
privateReplay	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l a0, PrivateBorrowedFront
	move.l a1, PrivateBorrowedRecords
	movea.l GraphBlock+memory.Block.Pointer, a0
	move.l graph.GraphState.SourceIndex(a0), PrivateSavedGraphIndex
	lea PrivateNumberPointers, a0
	lea PrivateSavedNumbers, a1
	moveq #PRIVATE_NUMBER_COUNT-1, d0
saveNumbers
	movea.l (a0)+, a2
	move.l (a2), (a1)+
	dbra d0, saveNumbers
	lea Front, a0
	movea.l PrivateBorrowedFront, a1
	move.l #frontend.FRAME_BYTES, d0
	bsr.w privateSwap
	; Provider template demand follows the descriptor while scope borrows stay
	; attached to their original scratch owner.
	move.l #Front, StoredPrivate+moduleProvider.State.Frontend
	move.l PrivateBorrowedFront, StoredRoot+moduleProvider.State.Frontend
	lea Records, a0
	movea.l PrivateBorrowedRecords, a1
	bsr.w swapModuleGraphs
	lea FileSpans, a0
	lea PrivateFileSpans, a1
	bsr.w swapModuleGraphs
	lea OriginSpans, a0
	lea PrivateOrigins, a1
	bsr.w swapModuleGraphs
	clr.l SpanCount
	clr.l OriginCount
	lea Front, a0
	jsr frontend.activate
	bne.w replayFailed
	move.l #privateRejectFile, frontend.Frame.FileInclude(a0)
	move.l #sourceCursor, frontend.Frame.SourceCursor(a0)
	move.l #sourcePeek, frontend.Frame.SourcePeek(a0)
	move.l #source_control.line, frontend.Frame.SourceControl(a0)
	jsr source_control.begin
	bne.w replayFailed
	move.l #1, PrivateReplayActive
	lea PublishedCapture, a0
	bsr.w replayCapturedSpan
	move.l d0, d7
	bra.w restoreOwners
replayFailed
	move.l d0, d7
restoreOwners
	clr.l PrivateReplayActive
	lea Front, a0
	movea.l PrivateBorrowedFront, a1
	move.l #frontend.FRAME_BYTES, d0
	bsr.w privateSwap
	move.l PrivateBorrowedFront, StoredPrivate+moduleProvider.State.Frontend
	move.l #Front, StoredRoot+moduleProvider.State.Frontend
	lea Records, a0
	movea.l PrivateBorrowedRecords, a1
	bsr.w swapModuleGraphs
	lea FileSpans, a0
	lea PrivateFileSpans, a1
	bsr.w swapModuleGraphs
	lea OriginSpans, a0
	lea PrivateOrigins, a1
	bsr.w swapModuleGraphs
	lea PrivateNumberPointers, a0
	lea PrivateSavedNumbers, a1
	moveq #PRIVATE_NUMBER_COUNT-1, d0
restoreNumbers
	movea.l (a0)+, a2
	move.l (a1)+, (a2)
	dbra d0, restoreNumbers
	movea.l GraphBlock+memory.Block.Pointer, a0
	move.l PrivateSavedGraphIndex, graph.GraphState.SourceIndex(a0)
	; Both descriptors returned to their original addresses. Restore the global
	; owner's back-pointers first, then leave compiler/private grammar active.
	; A cleanup activation failure must not replace the original replay error.
	lea Front, a0
	jsr frontend.activate
	tst.l d7
	bne.w globalActivated
	move.l d0, d7
globalActivated
	movea.l PrivateBorrowedFront, a0
	jsr frontend.activate
	tst.l d7
	bne.w privateActivated
	move.l d0, d7
privateActivated
	tst.l d7
	beq.w good
	moveq #moduleCompile.REPLAY_UNSUPPORTED, d0
	bra.w done
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; privateReplay
privateRejectFile	.block
	moveq #1, d0
	rts
	.bend  ; privateRejectFile

; Nonoverlapping source Block A0 appends bytes to destination Block A1.
privateAppendBlock	.block
	movem.l d1-d3/a0-a3, -(sp)
	movea.l a0, a3
	movea.l a1, a2
	move.l memory.Block.Used(a0), d2
	move.l memory.Block.Used(a1), d3
	move.l d3, d0
	add.l d2, d0
	bcs.w bad
	movea.l a1, a0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a3), a0
	movea.l memory.Block.Pointer(a2), a1
	adda.l d3, a1
	move.l d2, d0
	bsr.w copy
	add.l d2, memory.Block.Used(a2)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a3
	tst.l d0
	rts
	.bend  ; privateAppendBlock
privateSwap	.block
	movem.l d0-d3/a0-a1, -(sp)
	lsr.l #2, d0
	subq.l #1, d0
next
	move.l (a0), d1
	move.l (a1), d2
	move.l d2, (a0)+
	move.l d1, (a1)+
	dbra d0, next
	movem.l (sp)+, d0-d3/a0-a1
	rts
	.bend  ; privateSwap

	.include "requests.i"
	.include "private_seed.i"
	.include "private_pub.i"

; A fixed external recipe needs an existing stable-width proof. Until package
; metadata exposes that proof here, accept only the fixed BE32 absolute-field
; recipe. Opaque external position recipes use normal module replay.
privateEligibleImage	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l PrivateImage+memory.Block.Pointer, a6
	; A validated interface-only image owns no fragments. It still publishes
	; semantics and a stored object; no payload carrier or final read is needed.
	lea PrivateSeen, a0
	move.l PrivateOwnerCount, d0
	jsr memory.reserveExact
	bne.w bad
	move.l PrivateOwnerCount, d0
	move.l d0, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	bsr.w privateClear
	movea.l a6, a0
	adda.l moduleImage.Header.Blocks(a6), a0
	move.l moduleImage.Header.BlockCount(a6), d7
	moveq #-1, d5
owner
	tst.l d7
	beq.w relocations
	move.l moduleImage.Block.Owner(a0), d0
	cmp.l PrivateOwnerCount, d0
	bhs.w bad
	cmp.l d5, d0
	beq.w ownerNext
	move.l d0, d5
	tst.l d0
	beq.w ownerNext
	movea.l PrivateSeen+memory.Block.Pointer, a1
	tst.b 0(a1, d0.l)
	bne.w bad
	move.b #1, 0(a1, d0.l)
ownerNext
	adda.w #moduleImage.BLOCK_BYTES, a0
	subq.l #1, d7
	bra.w owner
relocations
	move.l moduleImage.Header.RelocCount(a6), d7
	movea.l a6, a0
	adda.l moduleImage.Header.Relocs(a6), a0
next
	tst.l d7
	beq.w good
	tst.l moduleImage.Reloc.Target(a0)
	bpl.w advance
	tst.l moduleImage.Reloc.Addend(a0)
	bmi.w bad
	cmpi.l #moduleImage.PROGRAM_ABSOLUTE, moduleImage.Reloc.Program(a0)
	bne.w bad
advance
	adda.w #moduleImage.RELOC_BYTES, a0
	subq.l #1, d7
	bra.w next
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; privateEligibleImage

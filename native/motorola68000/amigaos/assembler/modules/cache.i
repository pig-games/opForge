; Persistent module cache orchestration. OFMI remains the linker image; an OFIC
; companion owns timestamp dependencies and pointer-free publication metadata.
; Metadata is invalidated before replacing an image, and published last. A miss
; always compiles through the maintained private path. One writer per directory.
IncrementalHeader	.struct
Magic	.long ?
Bytes	.long ?
Generation	.long ?
KeyBytes	.long ?
SourceCount	.long ?
DependencyCount	.long ?
PublicationBytes	.long ?
ImageBytes	.long ?
ImageInfo	.res 16
.endstruct
INCREMENTAL_HEADER_BYTES = IncrementalHeader.ImageInfo+16
INCREMENTAL_MAGIC = $4f494331; OIC1 cache/container integrity, not a VM version
IncrementalDependency	.struct
Module	.long ?  ; current build registry ordinal, validated by filename/key
Generation	.long ?
Name	.res moduleObjectStore.NAME_BYTES
.endstruct
INCREMENTAL_DEP_BYTES = IncrementalDependency.Name+moduleObjectStore.NAME_BYTES
IncrementalGeneration	.struct
Number	.long ?
Changed	.long ?  ; nonzero propagates rebuild even within one clock tick
.endstruct
INCREMENTAL_GENERATION_BYTES = IncrementalGeneration.Changed+4
CACHE_DOS_LOCK = -84
CACHE_DOS_UNLOCK = -90
CACHE_DOS_OLD_FILE = 1005
CACHE_TOOL_BUFFER_BYTES = 4096
INCREMENTAL_BLOCK_BYTES = memory.Block.Used+4
CACHE_DOS_OPEN = -30
CACHE_DOS_CLOSE = -36
CACHE_DOS_READ = -42
CACHE_DOS_OUTPUT = -60
CACHE_DOS_WRITE = -48
CACHE_DOS_DELETE = -72
CACHE_DOS_NAME_FROM_LOCK = -402
CACHE_DOS_PROGRAM_NAME = -576
CACHE_DOS_PROGRAM_DIR = -600

	.include "cache_key.i"
	.include "cache_pub.i"
	.include "cache_sources.i"
	.include "cache_store.i"

; Initialize only for real CLI requests, never harness/reference execution.
; Tool/package identities distinguish code and embedded package compatibility.
; Source freshness is exclusively the recorded date/size tuple.
incrementalBegin	.block
	movem.l d1-d7/a0-a6, -(sp)
	clr.l IncrementalEnabled
	clr.l SharedModuleStore
	clr.l IncrementalDigestPackage
	clr.l IncrementalHits
	clr.l IncrementalMisses
	tst.w ReferenceModules
	bne.w good
	tst.w CliMode
	beq.w good
	movea.l IncrementalDirectory, a0
	move.l a0, d0
	beq.w good
	tst.b (a0)
	beq.w good
	movea.l DosBase, a2
	jsr incrementalStore.ensureDirectory
	bne.w bad
	movea.l DosBase, a6
	move.l #IncrementalEmpty, d1
	moveq #-2, d2
	jsr CACHE_DOS_LOCK(a6)
	move.l d0, d4
	beq.w bad
	move.l d4, d1
	move.l #IncrementalWorkingPath, d2
	move.l #PATH_BYTES, d3
	jsr CACHE_DOS_NAME_FROM_LOCK(a6)
	move.l d0, d5
	move.l d4, d1
	jsr CACHE_DOS_UNLOCK(a6)
	tst.l d5
	beq.w bad
	move.l #IncrementalToolName, d1
	move.l #PATH_BYTES, d2
	jsr CACHE_DOS_PROGRAM_NAME(a6)
	tst.l d0
	beq.w good  ; no reliable tool identity: safely compile without reuse
	jsr CACHE_DOS_PROGRAM_DIR(a6)
	move.l d0, d1
	beq.w good
	move.l #IncrementalToolPath, d2
	move.l #PATH_BYTES, d3
	jsr CACHE_DOS_NAME_FROM_LOCK(a6)
	tst.l d0
	beq.w good
	lea IncrementalToolName, a0
	lea IncrementalToolPath, a1
	; ProgramName may contain the invocation path. ProgramDir owns the executable;
	; append only its leaf, independent of the current source directory.
	movea.l a0, a2
name
	move.b (a0)+, d0
	beq.w leaf
	cmpi.b #'/', d0
	beq.w separator
	cmpi.b #':', d0
	bne.w name
separator
	movea.l a0, a2
	bra.w name
leaf
	movea.l a2, a0
	bsr.w incrementalJoin
	bne.w good
	bsr.w incrementalToolIdentity
	bne.w good
	move.l #1, IncrementalEnabled
good
	moveq #0, d0
	bra.w done
bad
	movea.l DosBase, a6
	move.l #IncrementalUnavailable, d1
	jsr DOS_PUT_STR(a6)
	moveq #0, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; incrementalBegin

; Called before compiling the next module. D0=status,D1=hit. Invalid/stale
; files are misses; after import hydration begins, failures are real errors.
incrementalTry	.block
	movem.l d2-d7/a0-a6, -(sp)
	clr.l IncrementalHit
	clr.l IncrementalSaved
	move.l #1, IncrementalNextGeneration
	tst.l IncrementalEnabled
	beq.w miss
	bsr.w incrementalPaths
	bne.w miss
	bsr.w incrementalBuildKey
	bne.w miss
	; Key contains exact configuration and tool/package compatibility identities.
	lea IncrementalToolInfo, a0
	moveq #8, d0
	bsr.w incrementalKeyAppend
	bne.w miss
	movea.l Front+frontend.Frame.Package, a0
	cmpa.l IncrementalDigestPackage, a0
	beq.w digestReady
	move.l a0, IncrementalDigestPackage
	move.l package.Header.Bytes(a0), d0
	bsr.w incrementalDigest
	move.l d1, IncrementalPackageDigest
	move.l d2, IncrementalPackageDigest+4
digestReady
	lea IncrementalPackageDigest, a0
	moveq #8, d0
	bsr.w incrementalKeyAppend
	bne.w miss
	move.l IncrementalKey+memory.Block.Used, d0
	neg.l d0
	andi.l #3, d0
	lea IncrementalZero, a0
	bsr.w incrementalKeyAppend
	bne.w miss
	lea IncrementalMetaPath, a0
	lea IncrementalEntry, a1
	movea.l DosBase, a2
	jsr incrementalStore.load
	bne.w miss
	movea.l IncrementalEntry+memory.Block.Pointer, a6
	cmpi.l #INCREMENTAL_HEADER_BYTES, IncrementalEntry+memory.Block.Used
	blo.w miss
	cmpi.l #INCREMENTAL_MAGIC, IncrementalHeader.Magic(a6)
	bne.w miss
	move.l IncrementalHeader.Generation(a6), d0
	beq.w miss
	addq.l #1, d0
	beq.w miss
	move.l d0, IncrementalNextGeneration
	tst.w IncrementalRebuild
	bne.w miss
	move.l IncrementalHeader.Bytes(a6), d0
	cmp.l IncrementalEntry+memory.Block.Used, d0
	bne.w miss
	move.l IncrementalHeader.KeyBytes(a6), d6
	cmp.l IncrementalKey+memory.Block.Used, d6
	bne.w miss
	move.l IncrementalHeader.SourceCount(a6), d7
	beq.w miss
	cmpi.l #memory.LIMIT/INCREMENTAL_SOURCE_BYTES, d7
	bhi.w miss
	move.l d7, d5
	mulu.l #INCREMENTAL_SOURCE_BYTES, d5
	move.l IncrementalHeader.DependencyCount(a6), d4
	cmpi.l #memory.LIMIT/INCREMENTAL_DEP_BYTES, d4
	bhi.w miss
	mulu.l #INCREMENTAL_DEP_BYTES, d4
	move.l #INCREMENTAL_HEADER_BYTES, d0
	add.l d6, d0
	bcs.w miss
	add.l d5, d0
	bcs.w miss
	add.l d4, d0
	bcs.w miss
	add.l IncrementalHeader.PublicationBytes(a6), d0
	bcs.w miss
	cmp.l IncrementalEntry+memory.Block.Used, d0
	bne.w miss
	lea INCREMENTAL_HEADER_BYTES(a6), a0
	movea.l IncrementalKey+memory.Block.Pointer, a1
	move.l d6, d0
	bsr.w incrementalEqual
	bne.w miss
	lea INCREMENTAL_HEADER_BYTES(a6), a0
	adda.l d6, a0
	bsr.w incrementalCurrentSources
	bne.w miss
	bsr.w incrementalCheckSources
	bne.w miss
	adda.l d5, a0
	move.l IncrementalHeader.DependencyCount(a6), d7
	bsr.w incrementalCheckDependencies
	bne.w miss
	adda.l d4, a0
	move.l a0, IncrementalPubPointer
	lea IncrementalImagePath, a0
	lea IncrementalCurrentInfo, a1
	movea.l DosBase, a2
	jsr incrementalStore.stamp
	bne.w miss
	lea IncrementalHeader.ImageInfo(a6), a0
	lea IncrementalCurrentInfo, a1
	moveq #16, d0
	bsr.w incrementalEqual
	bne.w miss
	lea IncrementalImagePath, a0
	lea PrivateImage, a1
	movea.l DosBase, a2
	jsr incrementalStore.load
	bne.w releaseMiss
	move.l PrivateImage+memory.Block.Used, d0
	cmp.l IncrementalHeader.ImageBytes(a6), d0
	bne.w releaseMiss
	movea.l PrivateImage+memory.Block.Pointer, a0
	jsr moduleImage.validate
	bne.w releaseMiss
	; All freshness checks precede publication. Restore creates fresh root IDs.
	movea.l IncrementalPubPointer, a0
	move.l IncrementalHeader.PublicationBytes(a6), d0
	bsr.w incrementalRestorePublication
	cmpi.l #2, d0
	beq.w releaseMiss
	tst.l d0
	bne.w fatal
	bsr.w privateEligibleImage
	bne.w fatal
	move.l IncrementalHeader.Generation(a6), IncrementalNextGeneration
	move.l #1, IncrementalHit
	addq.l #1, IncrementalHits
	moveq #1, d1
	moveq #0, d0
	bra.w done
releaseMiss
	lea PrivateImage, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
miss
	moveq #0, d1
	moveq #0, d0
	bra.w done
fatal
	moveq #0, d1
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; incrementalTry

; Current dependencies must have the same names/generations and must not have
; been rebuilt earlier in this run. The latter protects coarse timestamp ticks.
; A0=rows,D7=count; D0/CCR=status; other registers preserved.
incrementalCheckDependencies	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a4
next
	tst.l d7
	beq.w good
	move.l IncrementalDependency.Module(a4), d6
	cmpi.l #65535, d6
	bhi.w bad
	mulu.l #INCREMENTAL_GENERATION_BYTES, d6
	move.l d6, d0
	addi.l #INCREMENTAL_GENERATION_BYTES, d0
	bcs.w bad
	cmp.l IncrementalGenerations+memory.Block.Used, d0
	bhi.w bad
	movea.l IncrementalGenerations+memory.Block.Pointer, a0
	adda.l d6, a0
	tst.l IncrementalGeneration.Changed(a0)
	bne.w bad
	move.l IncrementalGeneration.Number(a0), d0
	cmp.l IncrementalDependency.Generation(a4), d0
	bne.w bad
	lea PrivateRegistry, a0
	lea IncrementalDependency.Name(a4), a1
	moveq #0, d2
length
	tst.b 0(a1, d2.l)
	beq.w named
	addq.l #1, d2
	cmpi.l #moduleObjectStore.NAME_BYTES, d2
	bhs.w bad
	bra.w length
named
	jsr moduleRegistry.findObject
	bne.w bad
	cmp.l IncrementalDependency.Module(a4), d1
	bne.w bad
	adda.w #INCREMENTAL_DEP_BYTES, a4
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
	.bend  ; incrementalCheckDependencies

; Construct bounded cache filenames using the same module identity as OFMI's
; dependency directory. Full module name/configuration in the key disambiguates
; truncated filename collisions; discovery-order changes safely cause misses.
incrementalPaths	.block
	movem.l d1, -(sp)
	move.l PrivateRegistry+moduleRegistry.State.Modules+memory.Block.Used, d1
	moveq #moduleRegistry.MODULE_BYTES, d0
	divu.l d0, d1
	bsr.w incrementalModulePaths
	movem.l (sp)+, d1
	tst.l d0
	rts
	.bend ; incrementalPaths

; D1=current module ordinal, before or after registry admission. Other registers
; preserved; D0/CCR=status. Also used to invalidate a disabled shared cache safely.
incrementalModulePaths	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea PrivateModuleName, a1
	move.l PrivateModuleBytes, d2
	lea IncrementalFileName, a2
	jsr moduleObjectStore.makeName
	bne.w done
	movea.l IncrementalDirectory, a0
	lea IncrementalImagePath, a1
	move.l #PATH_BYTES, d0
	bsr.w copy
	lea IncrementalFileName, a0
	lea IncrementalImagePath, a1
	bsr.w incrementalJoin
	bne.w done
	lea IncrementalImagePath, a0
	lea IncrementalMetaPath, a1
	move.l #PATH_BYTES*2, d0
	bsr.w copy
	; Generated names always end in .ofm; companion replaces only the last byte.
	lea IncrementalMetaPath, a0
scan
	tst.b (a0)+
	bne.w scan
	move.b #'i', -2(a0)
	moveq #0, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; incrementalModulePaths

; A0=NUL leaf,A1=NUL path with capacity PATH_BYTES. Appends slash as needed.
; D0/CCR=status; other registers preserved. No allocation or OS side effects.
incrementalJoin	.block
	movem.l d1-d3/a0-a2, -(sp)
	movea.l a1, a2
	moveq #0, d2
end
	cmpi.l #PATH_BYTES-2, d2
	bhs.w bad
	move.b (a2)+, d0
	beq.w append
	addq.l #1, d2
	bra.w end
append
	subq.l #1, a2
	tst.l d2
	beq.w text
	cmpi.b #':', -1(a2)
	beq.w text
	cmpi.b #'/', -1(a2)
	beq.w text
	move.b #'/', (a2)+
	addq.l #1, d2
text
	cmpi.l #PATH_BYTES-1, d2
	bhs.w bad
	move.b (a0)+, (a2)+
	beq.w good
	addq.l #1, d2
	bra.w text
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; incrementalJoin

; Package/config container identity, never a source freshness test.
; A0/D0=bytes; D1/D2=independent 32-bit fingerprints, other registers kept.
incrementalDigest	.block
	move.l #$811c9dc5, d1
	move.l #$9e3779b9, d2
	bra.w incrementalDigestBytes
	.bend  ; incrementalDigest
incrementalDigestBytes	.block
	movem.l d0/d3/a0, -(sp)
next
	tst.l d0
	beq.w done
	moveq #0, d3
	move.b (a0)+, d3
	eor.l d3, d1
	mulu.l #16777619, d1
	eor.l d3, d2
	ror.l #5, d2
	add.l d1, d2
	subq.l #1, d0
	bra.w next
done
	movem.l (sp)+, d0/d3/a0
	rts
	.bend  ; incrementalDigestBytes

	.include "cache_tool.i"
	.include "cache_save.i"
	.endsection
	.section bss, kind=bss
IncrementalDirectory	.res byte, 4
IncrementalRebuild	.res byte, 2
	.align 4
IncrementalEnabled	.res byte, 4
IncrementalRootPath	.res byte, PATH_BYTES
IncrementalWorkingPath	.res byte, PATH_BYTES
IncrementalToolName	.res byte, PATH_BYTES
IncrementalToolPath	.res byte, PATH_BYTES
IncrementalToolInfo	.res byte, 8
IncrementalPackageDigest	.res byte, 8
IncrementalDigestPackage	.res byte, 4
IncrementalZero	.res byte, 4
IncrementalCurrentInfo	.res byte, 16
IncrementalImagePath	.res byte, PATH_BYTES*2
IncrementalMetaPath	.res byte, PATH_BYTES*2
IncrementalFileName	.res byte, moduleObjectStore.NAME_BYTES
IncrementalPubPointer	.res byte, 4
IncrementalHit	.res byte, 4
IncrementalSaved	.res byte, 4
IncrementalNextGeneration	.res byte, 4
IncrementalHits	.res byte, 4
IncrementalMisses	.res byte, 4
IncrementalSources	.res byte, memory.Block.Used+4
IncrementalGenerations	.res byte, memory.Block.Used+4
IncrementalEntry	.res byte, memory.Block.Used+4
	.endsection
	.section code, kind=code

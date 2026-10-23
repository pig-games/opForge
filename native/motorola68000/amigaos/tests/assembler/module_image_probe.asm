; Standalone component proof: generated instruction bytes remain opaque here.
; @opforge-owner: opforge.tests.assembler.module_image_probe
	.module opforge.tests.assembler.module_image_probe
	.cpu 68020
	.use opforge.assembler.modules.image as image
	.use opforge.assembler.modules.interface_store as storedInterface
	.use opforge.assembler.modules.runtime as storedRuntime
	.use opforge.assembler.modules.assembly_interface as interface
	.use opforge.assembler.modules.records as moduleRecords
	.use opforge.assembler.layout.sections as sectionState
	.use opforge.assembler.modules.layout as layout
	.use opforge.assembler.layout.assembly as assembly
	.use opforge.packages.format as package
	.use opforge.vm.exprvm.runtime as exprvm
	.use opforge.assembler.modules.semantic_exports as semantics
	.use opforge.platform.memory as memory
	.use opforge.assembler.modules.request as request
	.use opforge.assembler.modules.registry as registry
	.use opforge.assembler.modules.object_store as objects
	.include "module_image_payload.i"
	.include "module_registry_mode.i"
	.priv
EXEC_BASE = 4
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
IMAGE_BYTES = image.HEADER_BYTES+3*image.BLOCK_BYTES+image.RELOC_BYTES+PAYLOAD_BYTES
GUARD_VALUE = $a5a5a5a5
FAIL_ACQUISITION = 21
FAIL_IMAGE = 22
FAIL_REJECTION = 23
FAIL_LINK = 24
FAIL_REQUEST = 25
FAIL_PARTIAL_OWNER = 26
FAIL_PARTIAL_RETRY = 27
FAIL_PARTIAL_RELEASE = 28
FAIL_RECURSIVE_OWNER = 29
FAIL_RECURSIVE_RETRY = 30
FAIL_RECURSIVE_RELEASE = 31
FAIL_READY_RELEASE = 32
FAIL_PARTIAL_STATUS = 33
FAIL_OWNER_PHASE = 34
FAIL_OWNER_POINTER = 35
FAIL_OWNER_CAPACITY = 36
FAIL_OWNER_USED = 37
FAIL_REGISTRY = 38
	.section entry, kind=code
	.pub
; Native Hunk entry; D0=0 only after all checks and a complete file write/close.
; Other callee-saved registers preserved. Every owned allocation is released.
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	move.l #20, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l EXEC_BASE.w, a6
	jsr EXEC_OPEN_LIBRARY(a6)
	move.l d0, DosBase
	beq.w cleanup
	jsr cases
	tst.l d0
	bne.w closeLibrary
	movea.l DosBase, a6
	move.l #OutputName, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, OutputHandle
	beq.w closeLibrary
	move.l d0, d1
	move.l #Result, d2
	moveq #44, d3
	jsr DOS_WRITE(a6)
	cmpi.l #44, d0
	bne.w closeFile
	clr.l ReturnCode
closeFile
	move.l OutputHandle, d1
	jsr DOS_CLOSE(a6)
	tst.l d0
	bne.w closeLibrary
	move.l #20, ReturnCode
closeLibrary
	jsr releaseRuntimeRelocation
	lea Registry, a0
	jsr registry.release
	lea RegistryOwner, a0
	jsr memory.release
	movea.l DosBase, a1
	movea.l EXEC_BASE.w, a6
	jsr EXEC_CLOSE_LIBRARY(a6)
cleanup
	lea FailedRequest, a0
	jsr request.release
	lea ReentrantRequest, a0
	jsr request.release
	lea Request, a0
	jsr request.release
	lea SourceOwner, a0
	jsr memory.release
	lea OriginalOwner, a0
	jsr memory.release
	lea MovedOwner, a0
	jsr memory.release
	move.l ReturnCode, d0
	movem.l (sp)+, d2-d7/a2-a6
	tst.l d0
	rts
	.bend  ; start
	.endsection
	.priv
	.section code, kind=code
; The current OFMA wire retains only exact object identity and local entities.
; Check serialization plus malformed dependency/import rejection independently
; of frontend ownership and final placement.
interfaceChecks	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l #FAIL_IMAGE, ReturnCode
	lea InterfaceBuild, a0
	jsr interface.build
	bne.w bad
	lea InterfaceBuffer, a0
	move.l InterfaceBuild+interface.Frame.Used, d0
	jsr interface.validate
	bne.w bad
	move.l interface.Header.Imports(a0), d1
	movea.l a0, a2
	adda.l d1, a2
	cmpi.l #7, interface.Import.Entity(a2)
	bne.w bad
	move.l #1, interface.Import.Dependency(a2)
	move.l InterfaceBuild+interface.Frame.Used, d0
	jsr interface.validate
	tst.l d0
	beq.w bad
	clr.l interface.Import.Dependency(a2)
	move.l InterfaceBuild+interface.Frame.Used, d0
	jsr interface.validate
	bne.w bad
	; The writer rejects an import outside its dependency directory.
	move.l #1, ProbeImport+interface.Import.Dependency
	lea InterfaceBuild, a0
	jsr interface.build
	tst.l d0
	beq.w bad
	clr.l ProbeImport+interface.Import.Dependency
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; interfaceChecks

	.include "module_runtime_reloc_checks.i"
; The first requested block compiles and owns the image exactly once.
cases	.block
	bsr.w headerChecks
	bne.w bad
	bsr.w interfaceChecks
	bne.w bad
	bsr.w runtimeRelocationChecks
	bne.w bad
	move.l #FAIL_ACQUISITION, ReturnCode
	lea Request, a0
	lea CompileCount, a1
	lea compileImage, a2
	moveq #1, d1
	jsr request.acquire
	bne.w bad
	cmpi.l #1, CompileCount
	bne.w bad
	cmpi.l #FIRST_BYTES, d2
	bne.w bad
	moveq #2, d1
	lea CompileCount, a1
	lea compileImage, a2
	jsr request.acquire
	bne.w bad
	cmpi.l #1, CompileCount
	bne.w bad
	cmpi.l #SECOND_BYTES, d2
	bne.w bad
	movea.l Request+request.State.Image+memory.Block.Pointer, a0
	move.l a0, Link+image.LinkFrame.Image
	bra.w selected
bad
	moveq #1, d0
	rts
	.bend  ; cases
; Callback publishes one independently owned complete image, with no borrowed
; source or original-image allocation remaining. Preserve the callback ABI.
compileImage	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l #FAIL_IMAGE, ReturnCode
	movea.l a1, a5
	addq.l #1, CompileCount
	lea SourceOwner, a0
	move.l #PAYLOAD_BYTES, d0
	jsr memory.reserveExact
	bne.w bad
	move.l #PAYLOAD_BYTES, SourceOwner+memory.Block.Used
	lea Source, a1
	movea.l SourceOwner+memory.Block.Pointer, a2
	move.l #PAYLOAD_BYTES, d1
	bsr.w copy
	move.l SourceOwner+memory.Block.Pointer, Inputs+image.Input.Data
	move.l SourceOwner+memory.Block.Pointer, d0
	addi.l #FIRST_BYTES, d0
	move.l d0, Inputs+image.INPUT_BYTES+image.Input.Data
	addi.l #SECOND_BYTES, d0
	move.l d0, Inputs+2*image.INPUT_BYTES+image.Input.Data
	; Measure without an output allocation, then check capacity transactionality.
	lea Build, a0
	jsr image.build
	bne.w bad
	cmpi.l #IMAGE_BYTES, Build+image.Frame.Used
	bne.w bad
	move.l #Guard, Build+image.Frame.Output
	move.l #IMAGE_BYTES-1, Build+image.Frame.Capacity
	clr.l Build+image.Frame.Used
	jsr image.build
	bsr.w buildFailed
	bne.w bad
	move.l #1, Inputs+image.INPUT_BYTES+image.Input.Id
	jsr image.build
	bsr.w buildFailed
	bne.w bad
	move.l #2, Inputs+image.INPUT_BYTES+image.Input.Id
	lea OriginalOwner, a0
	move.l #IMAGE_BYTES, d0
	jsr memory.reserveExact
	bne.w bad
	move.l OriginalOwner+memory.Block.Pointer, Build+image.Frame.Output
	move.l #IMAGE_BYTES, Build+image.Frame.Capacity
	lea Build, a0
	jsr image.build
	bne.w bad
	move.l #IMAGE_BYTES, OriginalOwner+memory.Block.Used
	lea MovedOwner, a0
	move.l #IMAGE_BYTES, d0
	jsr memory.reserveExact
	bne.w bad
	move.l #IMAGE_BYTES, MovedOwner+memory.Block.Used
	movea.l OriginalOwner+memory.Block.Pointer, a1
	movea.l MovedOwner+memory.Block.Pointer, a2
	move.l #IMAGE_BYTES, d1
	bsr.w copy
	; Poison before release: the moved image must own every byte it needs.
	lea SourceOwner, a0
	bsr.w poisonRelease
	lea OriginalOwner, a0
	bsr.w poisonRelease
	movea.l MovedOwner+memory.Block.Pointer, a0
	move.l a0, Link+image.LinkFrame.Image
	move.l #IMAGE_BYTES, d0
	jsr image.validate
	bne.w bad
	move.l #IMAGE_BYTES-1, d0
	jsr image.validate
	tst.l d0
	beq.w bad
	; Directory corruption must be rejected, then restore the intact image.
	movea.l MovedOwner+memory.Block.Pointer, a2
	move.l image.HEADER_BYTES+image.BLOCK_BYTES+image.Block.Id(a2), d7
	move.l #1, image.HEADER_BYTES+image.BLOCK_BYTES+image.Block.Id(a2)
	movea.l a2, a0
	move.l #IMAGE_BYTES, d0
	jsr image.validate
	tst.l d0
	beq.w bad
	move.l d7, image.HEADER_BYTES+image.BLOCK_BYTES+image.Block.Id(a2)
	move.l image.HEADER_BYTES+image.Block.Offset(a2), d7
	move.l #IMAGE_BYTES, image.HEADER_BYTES+image.Block.Offset(a2)
	move.l #IMAGE_BYTES, d0
	jsr image.validate
	tst.l d0
	beq.w bad
	move.l d7, image.HEADER_BYTES+image.Block.Offset(a2)
	; Transfer ownership to the request boundary only after successful checks.
	move.l MovedOwner+memory.Block.Pointer, memory.Block.Pointer(a5)
	move.l MovedOwner+memory.Block.Capacity, memory.Block.Capacity(a5)
	move.l MovedOwner+memory.Block.Used, memory.Block.Used(a5)
	clr.l MovedOwner+memory.Block.Pointer
	clr.l MovedOwner+memory.Block.Capacity
	clr.l MovedOwner+memory.Block.Used
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; compileImage
selected	.block
	move.l #FAIL_IMAGE, ReturnCode
	; Find distinct fragments after the complete image has changed allocation.
	move.l #IMAGE_BYTES, d0
	moveq #1, d1
	jsr image.findValidated
	bne.w bad
	cmpi.l #FIRST_BYTES, d1
	bne.w bad
	move.l #IMAGE_BYTES, d0
	moveq #2, d1
	jsr image.findValidated
	bne.w bad
	cmpi.l #SECOND_BYTES, d1
	bne.w bad
	move.l #IMAGE_BYTES, d0
	moveq #99, d1
	jsr image.findValidated
	tst.l d0
	beq.w bad
	; Images without semantic exports have no readable semantic view.
	move.l #IMAGE_BYTES, d0
	jsr image.semanticView
	cmpi.l #semantics.NOT_FOUND, d0
	bne.w bad
	; Link failure checks always use a guarded output and zero publication count.
	move.l #FAIL_REJECTION, ReturnCode
	lea Link, a0
	move.l #Guard, image.LinkFrame.Output(a0)
	move.l #FIRST_BYTES-1, image.LinkFrame.Capacity(a0)
	bsr.w rejectLink
	bne.w bad
	move.l #FIRST_BYTES, image.LinkFrame.Capacity(a0)
	move.l #99, image.LinkFrame.BlockId(a0)
	bsr.w rejectLink
	bne.w bad
	move.l #1, image.LinkFrame.BlockId(a0)
	move.l #1, ResolveFails
	bsr.w rejectLink
	bne.w bad
	clr.l ResolveFails
	move.l #$fffffffc, image.LinkFrame.Base(a0)
	bsr.w rejectLink
	bne.w bad
	clr.l image.LinkFrame.Base(a0)
	move.l #$fffffffc, Target
	bsr.w rejectLink
	bne.w bad
	; Opaque programs require a callback and exact bounded output in preflight.
	move.l #$1000, Target
	movea.l image.LinkFrame.Image(a0), a2
	adda.l image.Header.Relocs(a2), a2
	move.l #7, image.Reloc.Program(a2)
	move.l #9, image.Reloc.Arguments(a2)
	bsr.w rejectLink
	bne.w bad
	move.l #patch, image.LinkFrame.Patch(a0)
	move.l #3, PatchBytes
	bsr.w rejectLink
	bne.w bad
	move.l #4, PatchBytes
	move.l #Result, image.LinkFrame.Output(a0)
	jsr image.link
	bne.w bad
	cmpi.l #$1004, Result+2
	bne.w bad
	move.l #image.PROGRAM_ABSOLUTE, image.Reloc.Program(a2)
	clr.l image.Reloc.Arguments(a2)
	clr.l image.LinkFrame.Patch(a0)
	; The same immutable block links twice with distinct resolver addresses.
	move.l #FAIL_LINK, ReturnCode
	move.l #$1000, Target
	move.l #Result, image.LinkFrame.Output(a0)
	jsr image.link
	bne.w bad
	cmpi.l #FIRST_BYTES, Link+image.LinkFrame.Used
	bne.w bad
	cmpi.l #$1004, Result+2
	bne.w bad
	move.l #$2000, Target
	move.l #Result+FIRST_BYTES, image.LinkFrame.Output(a0)
	jsr image.link
	bne.w bad
	cmpi.l #$2004, Result+FIRST_BYTES+2
	bne.w bad
	move.l #2, image.LinkFrame.BlockId(a0)
	move.l #Result+2*FIRST_BYTES, image.LinkFrame.Output(a0)
	move.l #SECOND_BYTES, image.LinkFrame.Capacity(a0)
	jsr image.link
	bne.w bad
	cmpi.l #SECOND_BYTES, Link+image.LinkFrame.Used
	bne.w bad
	bsr.w deltaChecks
	bne.w bad
	bsr.w registryCases
	bne.w bad
	bsr.w requestFailures
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; selected
; Pure naming coverage: leaf-first qualification, long-leaf tail, unsafe bytes,
; and unique ID suffixes despite identical case-folded/truncated spelling.
nameCases	.block
	movem.l d1-d2/a0-a3, -(sp)
	lea QualifiedName, a1
	move.l #QUALIFIED_NAME_BYTES, d2
	moveq #0, d1
	lea QualifiedFile, a3
	bsr.w nameCase
	bne.w bad
	lea ShortName, a1
	moveq #7, d2
	moveq #1, d1
	lea ShortFile, a3
	bsr.w nameCase
	bne.w bad
	lea LongLeaf, a1
	moveq #33, d2
	moveq #2, d1
	lea LongFile, a3
	bsr.w nameCase
	bne.w bad
	lea UnsafeName, a1
	moveq #12, d2
	moveq #10, d1
	lea UnsafeFile, a3
	bsr.w nameCase
	bne.w bad
	lea CaseName, a1
	move.l #QUALIFIED_NAME_BYTES, d2
	moveq #1, d1
	lea CaseFile, a3
	bsr.w nameCase
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a3
	tst.l d0
	rts
	.bend  ; nameCases
; A1/D2=source,A3=expected,D1=ID. Compare complete NUL-terminated component.
nameCase	.block
	lea NameBuffer, a2
	jsr objects.makeName
	bne.w done
	movea.l a2, a0
	movea.l a3, a1
compare
	move.b (a0)+, d0
	cmp.b (a1)+, d0
	bne.w bad
	tst.b d0
	bne.w compare
	tst.b NameBuffer+objects.NAME_BYTES-1
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; nameCase

; Both storage modes link A/B/A after ownership transfer. B uses a local
; fragment target; A uses a numeric export installed from another module.
registryCases	.block
	move.l #FAIL_REGISTRY, ReturnCode
	bsr.w nameCases
	bne.w bad
	.if DISK_REGISTRY
	lea Registry, a0
	movea.l DosBase, a1
	lea WorkName, a2
	jsr registry.configure
	bne.w bad
	.endif
	moveq #0, d6
addModule
	lea RegistryOwner, a0
	move.l #IMAGE_BYTES, d0
	jsr memory.reserveExact
	bne.w bad
	move.l #IMAGE_BYTES, RegistryOwner+memory.Block.Used
	movea.l Request+request.State.Image+memory.Block.Pointer, a1
	movea.l RegistryOwner+memory.Block.Pointer, a2
	move.l #IMAGE_BYTES, d1
	bsr.w copy
	tst.l d6
	beq.w addReady
	movea.l RegistryOwner+memory.Block.Pointer, a2
	adda.l image.Header.Relocs(a2), a2
	move.l #2, image.Reloc.Target(a2)
addReady
	lea Registry, a0
	lea RegistryOwner, a1
	lea QualifiedName, a2
	move.l #QUALIFIED_NAME_BYTES, d2
	jsr registry.add
	bne.w bad
	cmp.l d6, d1
	bne.w bad
	tst.l RegistryOwner+memory.Block.Pointer
	bne.w bad
	tst.l RegistryOwner+memory.Block.Capacity
	bne.w bad
	tst.l RegistryOwner+memory.Block.Used
	bne.w bad
	; add has no IO. Full pending ownership supports construction until store;
	; disk locate already returns only the retained linker metadata.
	jsr registry.pending
	bne.w bad
	movea.l a1, a0
	move.l #IMAGE_BYTES, d0
	jsr image.validate
	bne.w bad
	lea Registry, a0
	move.l d6, d1
	moveq #1, d2
	bsr.w registryMetadata
	bne.w bad
	tst.l Registry+registry.State.Cache+memory.Block.Pointer
	bne.w bad
	tst.l Registry+registry.State.Cached
	bne.w bad
	.if DISK_REGISTRY
	cmp.l registry.DiskWrites, d6
	bne.w bad
	tst.l registry.DiskReads
	bne.w bad
	; Unstored payloads cannot enter the final emission reader.
	jsr registry.acquireEmit
	cmpi.l #registry.INVALID, d0
	bne.w bad
	.endif
	jsr registry.store
	bne.w bad
	jsr registry.store  ; repeated publication is idempotent
	bne.w bad
	moveq #1, d2
	bsr.w registryMetadata
	bne.w bad
	.if DISK_REGISTRY
	move.l d6, d0
	addq.l #1, d0
	cmp.l registry.DiskWrites, d0
	bne.w bad
	tst.l registry.DiskReads
	bne.w bad
	jsr registry.pending
	cmpi.l #registry.INVALID, d0
	bne.w bad
	movea.l Registry+registry.State.Modules+memory.Block.Pointer, a4
	move.l d6, d0
	mulu.l #registry.MODULE_BYTES, d0
	adda.l d0, a4
	tst.l registry.Module.Image+memory.Block.Pointer(a4)
	bne.w bad
	tst.l registry.Module.Image+memory.Block.Capacity(a4)
	bne.w bad
	tst.l registry.Module.Image+memory.Block.Used(a4)
	bne.w bad
	.else
	jsr registry.pending  ; resident store retains the original owner
	bne.w bad
	.endif
	tst.l d6
	bne.w added
	; Keep A as a completed deliverable; rolling back B must remove only B.
	lea RegistryMark, a1
	jsr registry.checkpoint
added
	addq.l #1, d6
	cmpi.l #2, d6
	bne.w addModule
	; Retire the original payload after both registry transfers. Reloads must
	; read independent module storage; request directory remains valid for its
	; later missing-selection checks.
	movea.l Request+request.State.Image+memory.Block.Pointer, a1
	adda.l image.Header.Payload(a1), a1
	move.l #PAYLOAD_BYTES, d1
poisonPayload
	move.b #$dd, (a1)+
	subq.l #1, d1
	bne.w poisonPayload
	; Three independent final-link requests A/B/A exercise eviction. Normal
	; Hunk CLI emission instead visits every selected module's fragments together.
	moveq #0, d6
linkModule
	lea Registry, a0
	move.l d6, d1
	andi.l #1, d1
	move.l d1, ActiveModule
	moveq #1, d2
	bsr.w registryMetadata
	bne.w bad
	jsr registry.acquireEmit
	bne.w bad
	move.l a1, RegistryLink+image.LinkFrame.Image
	jsr registry.acquireEmit  ; a same-module borrow must not reread payload
	bne.w bad
	.if DISK_REGISTRY
	move.l ActiveModule, d0
	addq.l #1, d0
	cmp.l Registry+registry.State.Cached, d0
	bne.w bad
	cmpi.l #IMAGE_BYTES, Registry+registry.State.Cache+memory.Block.Used
	bne.w bad
	move.l d6, d0
	addq.l #1, d0
	cmp.l registry.DiskReads, d0
	bne.w bad
	.endif
	lea Result+20, a1
	move.l d6, d0
	lsl.l #3, d0
	adda.l d0, a1
	move.l a1, RegistryLink+image.LinkFrame.Output
	lea RegistryLink, a0
	jsr image.linkValidated
	bne.w bad
	.if DISK_REGISTRY
	; Local resolution uses locate; it cannot invalidate or reload emission.
	move.l d6, d0
	addq.l #1, d0
	cmp.l registry.DiskReads, d0
	bne.w bad
	.endif
	addq.l #1, d6
	cmpi.l #3, d6
	bne.w linkModule
	lea Registry, a0
	lea RegistryMark, a1
	jsr registry.rollback
	bne.w bad
	cmpi.l #registry.MODULE_BYTES, Registry+registry.State.Modules+memory.Block.Used
	bne.w bad
	cmpi.l #3*registry.PLACEMENT_BYTES, Registry+registry.State.Placements+memory.Block.Used
	bne.w bad
	tst.l Registry+registry.State.Addresses+memory.Block.Used
	bne.w bad
	tst.l Registry+registry.State.Cache+memory.Block.Pointer
	bne.w bad
	tst.l Registry+registry.State.Cached
	bne.w bad
	moveq #0, d1
	moveq #1, d2
	bsr.w registryMetadata
	bne.w bad
	moveq #1, d1
	jsr registry.locate
	cmpi.l #registry.INVALID, d0
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; registryCases
; A0=Registry,D1=module,D2=first fragment. Check metadata without payload IO.
; D0/CCR=status; other registers preserved.
registryMetadata	.block
	movem.l d1-d5/a0-a4, -(sp)
	move.l registry.DiskReads, d5
	jsr registry.locate
	bne.w bad
	cmpi.l #IMAGE_BYTES, image.Header.Bytes(a1)
	bne.w bad
	cmpi.l #3, image.Header.BlockCount(a1)
	bne.w bad
	cmp.l image.Block.Id(a2), d2
	bne.w bad
	cmpi.l #FIRST_BYTES, image.Block.Bytes(a2)
	bne.w bad
	movea.l Registry+registry.State.Modules+memory.Block.Pointer, a4
	move.l d1, d0
	mulu.l #registry.MODULE_BYTES, d0
	adda.l d0, a4
	.if DISK_REGISTRY
	cmpa.l registry.Module.Metadata+memory.Block.Pointer(a4), a1
	bne.w bad
	move.l image.Header.Payload(a1), d0
	cmp.l registry.Module.Metadata+memory.Block.Used(a4), d0
	bne.w bad
	.else
	cmpa.l registry.Module.Image+memory.Block.Pointer(a4), a1
	bne.w bad
	.endif
	cmp.l registry.DiskReads, d5
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d5/a0-a4
	tst.l d0
	rts
	.bend  ; registryMetadata
; Mimic runtime's two resolution paths: global cells need no image lookup,
; while local fragment lookup keeps the active module's borrowed image live.
registryResolve	.block
	movem.l d2/a0-a3, -(sp)
	cmpi.l #17, d1
	beq.w external
	cmpi.l #2, d1
	bne.w bad
	move.l d1, d2
	move.l ActiveModule, d1
	lea Registry, a0
	jsr registry.locate
	bne.w bad
	move.l #$3000, d1
	bra.w good
external
	move.l #$1000, d1
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2/a0-a3
	tst.l d0
	rts
	.bend  ; registryResolve
	; Missing selections preserve a published image; compiler failures and
; ignored recursive rejection never publish a partial allocation.
requestFailures	.block
	move.l #FAIL_REQUEST, ReturnCode
	lea Request, a0
	lea CompileCount, a1
	lea compileImage, a2
	moveq #99, d1
	jsr request.acquire
	cmpi.l #request.STATUS_MISSING, d0
	bne.w bad
	cmpi.l #1, CompileCount
	bne.w bad
	cmpi.l #request.READY, Request+request.State.Phase
	bne.w bad
	move.l #FAIL_PARTIAL_OWNER, ReturnCode
	lea FailedRequest, a0
	lea FailureCount, a1
	lea failCompile, a2
	moveq #1, d1
	move.l #FAIL_PARTIAL_STATUS, ReturnCode
	jsr request.acquire
	cmpi.l #request.STATUS_FAILED, d0
	bne.w bad
	bsr.w checkFailed
	bne.w bad
	move.l #FAIL_PARTIAL_RETRY, ReturnCode
	lea FailureCount, a1
	lea failCompile, a2
	moveq #1, d1
	jsr request.acquire
	cmpi.l #request.STATUS_FAILED, d0
	bne.w bad
	cmpi.l #1, FailureCount
	bne.w bad
	bsr.w checkFailed
	bne.w bad
	move.l #FAIL_PARTIAL_RELEASE, ReturnCode
	bsr.w releaseTwice
	bne.w bad
	move.l #FAIL_RECURSIVE_OWNER, ReturnCode
	lea ReentrantRequest, a0
	movea.l a0, a1
	lea reenterCompile, a2
	moveq #1, d1
	jsr request.acquire
	cmpi.l #request.STATUS_FAILED, d0
	bne.w bad
	cmpi.l #request.STATUS_BUSY, InnerStatus
	bne.w bad
	cmpi.l #1, ReentrantCount
	bne.w bad
	bsr.w checkFailed
	bne.w bad
	move.l #FAIL_RECURSIVE_RETRY, ReturnCode
	movea.l a0, a1
	lea reenterCompile, a2
	moveq #1, d1
	jsr request.acquire
	cmpi.l #request.STATUS_FAILED, d0
	bne.w bad
	cmpi.l #1, ReentrantCount
	bne.w bad
	bsr.w checkFailed
	bne.w bad
	move.l #FAIL_RECURSIVE_RELEASE, ReturnCode
	bsr.w releaseTwice
	bne.w bad
	move.l #FAIL_READY_RELEASE, ReturnCode
	lea Request, a0
	bsr.w releaseTwice
	rts
bad
	moveq #1, d0
	rts
	.bend  ; requestFailures
; Deliberately abandon a partial owned allocation to the failure boundary.
failCompile	.block
	movem.l d1/a0, -(sp)
	addq.l #1, FailureCount
	movea.l a1, a0
	moveq #32, d0
	jsr memory.reserveExact
	bne.w done
	move.l #7, memory.Block.Used(a0)
done
	movem.l (sp)+, d1/a0
	moveq #1, d0
	rts
	.bend  ; failCompile
; Ignore the nested failure to prove that the outer request detects it.
reenterCompile	.block
	movem.l d1-d7/a0-a6, -(sp)
	addq.l #1, ReentrantCount
	movea.l a0, a4
	movea.l a1, a0
	moveq #32, d0
	jsr memory.reserveExact
	bne.w done
	move.l #7, memory.Block.Used(a0)
	move.l memory.Block.Pointer(a0), d5
	movea.l a4, a0
	movea.l a4, a1
	lea reenterCompile, a2
	moveq #1, d1
	jsr request.acquire
	move.l d0, InnerStatus
	cmpi.l #request.STATUS_BUSY, d0
	bne.w bad
	moveq #1, d6
checkOwned
	cmpi.l #request.BUILDING, request.State.Phase(a4)
	bne.w bad
	tst.l request.State.Failure(a4)
	beq.w bad
	tst.l d5
	beq.w bad
	cmp.l request.State.Image+memory.Block.Pointer(a4), d5
	bne.w bad
	cmpi.l #32, request.State.Image+memory.Block.Capacity(a4)
	bne.w bad
	cmpi.l #7, request.State.Image+memory.Block.Used(a4)
	bne.w bad
	movea.l a4, a0
	jsr request.release
	cmpi.l #request.STATUS_BUSY, d0
	bne.w bad
	dbra d6, checkOwned
	; The second rejected release also leaves the outer callback's owner live.
	cmpi.l #request.BUILDING, request.State.Phase(a4)
	bne.w bad
	cmp.l request.State.Image+memory.Block.Pointer(a4), d5
	bne.w bad
	cmpi.l #32, request.State.Image+memory.Block.Capacity(a4)
	bne.w bad
	cmpi.l #7, request.State.Image+memory.Block.Used(a4)
	bne.w bad
	bra.w done
bad
	move.l #request.STATUS_FAILED, InnerStatus
done
	movem.l (sp)+, d1-d7/a0-a6
	moveq #0, d0
	rts
	.bend  ; reenterCompile
; A0=request; no partial descriptor may escape a failed acquisition.
checkFailed	.block
	move.l #FAIL_OWNER_PHASE, ReturnCode
	cmpi.l #request.FAILED, request.State.Phase(a0)
	bne.w bad
	move.l #FAIL_OWNER_POINTER, ReturnCode
	tst.l request.State.Image+memory.Block.Pointer(a0)
	bne.w bad
	move.l #FAIL_OWNER_CAPACITY, ReturnCode
	tst.l request.State.Image+memory.Block.Capacity(a0)
	bne.w bad
	move.l #FAIL_OWNER_USED, ReturnCode
	tst.l request.State.Image+memory.Block.Used(a0)
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; checkFailed
releaseTwice	.block
	jsr request.release
	bne.w bad
	jsr request.release
	bne.w bad
	tst.l request.State.Phase(a0)
	bne.w bad
	tst.l request.State.Image+memory.Block.Pointer(a0)
	bne.w bad
	tst.l request.State.Image+memory.Block.Capacity(a0)
	bne.w bad
	tst.l request.State.Image+memory.Block.Used(a0)
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; releaseTwice
; Failure must publish no extent and touch no output byte.
buildFailed	.block
	tst.l d0
	beq.w bad
	tst.l Build+image.Frame.Used
	bne.w bad
	cmpi.l #GUARD_VALUE, Guard
	bne.w bad
	cmpi.l #GUARD_VALUE, Guard+4
	bne.w bad
	lea Build, a0
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; buildFailed
rejectLink	.block
	clr.l image.LinkFrame.Used(a0)
	jsr image.link
	tst.l d0
	beq.w bad
	tst.l image.LinkFrame.Used(a0)
	bne.w bad
	cmpi.l #GUARD_VALUE, Guard
	bne.w bad
	cmpi.l #GUARD_VALUE, Guard+4
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; rejectLink
; Pure resolver: opaque numeric import17 maps to the current target address.
resolve	.block
	moveq #1, d0
	cmp.l ResolveId, d1
	bne.w done
	tst.l ResolveFails
	bne.w done
	move.l Target, d1
	moveq #0, d0
done
	tst.l d0
	rts
	.bend  ; resolve
; Deliberately opaque program/arguments; writes only the supplied four bytes.
patch	.block
	cmpi.l #4, d0
	bne.w bad
	cmpi.l #7, d1
	bne.w bad
	cmpi.l #9, d2
	bne.w bad
	cmpi.l #2, d3
	bne.w bad
	move.l d4, (a1)
	move.l PatchBytes, d1
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; patch
copy	.block
	move.b (a1)+, (a2)+
	subq.l #1, d1
	bne.w copy
	rts
	.bend  ; copy
poisonRelease	.block
	movea.l memory.Block.Pointer(a0), a1
	move.l memory.Block.Used(a0), d1
next
	move.b #$dd, (a1)+
	subq.l #1, d1
	bne.w next
	jsr memory.release
	rts
	.bend  ; poisonRelease
	.endsection
	.section data, kind=data
	.align 4
EmptyValues
	.long semantics.MAGIC, semantics.HEADER_BYTES, 0, semantics.HEADER_BYTES
	.long 0, semantics.HEADER_BYTES, semantics.HEADER_BYTES, 0, semantics.HEADER_BYTES, 0
ProbeDependency
	.long ProbeObjectName, ProbeObjectNameEnd-ProbeObjectName
ProbeImport
	.long 0, 7
InterfaceBuild
	.long EmptyValues, semantics.HEADER_BYTES, 0, 0, InterfaceBuffer, 256, 0
	.long ProbeDependency, 1, ProbeImport, 1
ProbeObjectName	.byte "provider-0000.ofm"
ProbeObjectNameEnd
	.align 4
Inputs
	; Id,Data,Bytes,Owner,Section,Kind,Alignment,Extent (image.Input).
	.long 1, 0, FIRST_BYTES, 1, 0, image.KIND_CODE, 1, FIRST_BYTES
	.long 2, 0, SECOND_BYTES, 2, 0, image.KIND_CODE, 1, SECOND_BYTES
	.long 3, 0, 1, 3, 0, image.KIND_CODE, 1, 1
Fixup
	.long 1, 2, 17, 4, 4, image.PROGRAM_ABSOLUTE, 0
Build
	.long Inputs, 3, Fixup, 1, 0, 0, 0, 0, 0
Link
	.long 0, IMAGE_BYTES, 1, 0, 0, FIRST_BYTES, 0, resolve, 0, 0
RegistryLink
	.long 0, IMAGE_BYTES, 1, 0, 0, FIRST_BYTES, 0, registryResolve, 0, 0
QualifiedName	.byte "opforge.assembler.prepare.frontend"
QualifiedNameEnd
QUALIFIED_NAME_BYTES = QualifiedNameEnd-QualifiedName
QualifiedFile	.byte "frontend.opforge.asse-0000.ofm", 0
CaseName	.byte "OPFORGE.ASSEMBLER.PREPARE.FRONTEND"
CaseFile	.byte "FRONTEND.OPFORGE.ASSE-0001.ofm", 0
ShortName	.byte "foo.bar"
ShortFile	.byte "bar.foo-0001.ofm", 0
LongLeaf	.byte "parent.abcdefghijklmnopqrstuvwxyz"
LongFile	.byte "fghijklmnopqrstuvwxyz-0002.ofm", 0
UnsafeName	.byte "parent.a:b/c"
UnsafeFile	.byte "a_b_c.parent-000A.ofm", 0
WorkName	.byte "Work:objects", 0
	.align 4
Guard	.long GUARD_VALUE, GUARD_VALUE
Target	.long $1000
ResolveId	.long 17
ResolveFails	.long 0
PatchBytes	.long 4
DosName	.byte "dos.library", 0
OutputName	.byte "Work:output.bin", 0
Source
	.incbin "module_image_payload.bin"
	.endsection
	.section bss, kind=bss
	.align 4
InterfaceBuffer	.res byte, 256
	.align 4
SourceOwner	.res byte, memory.Block.Used+4
OriginalOwner	.res byte, memory.Block.Used+4
MovedOwner	.res byte, memory.Block.Used+4
Result	.res byte, 44
NameBuffer	.res byte, objects.NAME_BYTES
Registry	.res byte, registry.STATE_BYTES
RegistryOwner	.res byte, memory.Block.Used+4
RegistryMark	.res byte, registry.CHECKPOINT_BYTES
ActiveModule	.res long, 1
DosBase	.res long, 1
OutputHandle	.res long, 1
ReturnCode	.res long, 1
CompileCount	.res long, 1
Request	.res byte, request.STATE_BYTES
FailedRequest	.res byte, request.STATE_BYTES
ReentrantRequest	.res byte, request.STATE_BYTES
FailureCount	.res long, 1
ReentrantCount	.res long, 1
InnerStatus	.res long, 1
	.endsection
	.include "module_data_delta_checks.i"
	.include "module_iface_header_checks.i"
	.output "module-image-probe.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule

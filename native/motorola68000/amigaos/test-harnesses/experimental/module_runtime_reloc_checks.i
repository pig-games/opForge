; Exercise the runtime's real imported-address and relocation callbacks. The
; import ordinal, source section and field offset deliberately differ from
; the callback's zero status, so argument/status aliasing cannot pass unnoticed.
runtimeRelocationChecks	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l #FAIL_LINK, ReturnCode
	.if DISK_REGISTRY
	lea RuntimeRegistry, a0
	movea.l DosBase, a1
	lea WorkName, a2
	jsr registry.configure
	bne.w bad
	.endif
	lea RuntimeInterfaceBuild, a0
	jsr interface.build
	bne.w bad
	move.l RuntimeInterfaceBuild+interface.Frame.Used, RuntimeProviderBuild+image.Frame.SemanticsBytes
	lea RuntimeProviderBuild, a0
	bsr.w runtimeBuildObject
	bne.w bad
	lea RuntimeRegistry, a0
	lea RuntimeOwner, a1
	lea RuntimeProviderName, a2
	moveq #8, d2
	jsr registry.add
	bne.w bad
	tst.l d1
	bne.w bad
	.if DISK_REGISTRY
	jsr registry.store
	.else
	bsr.w runtimeResidentName
	.endif
	bne.w bad
	moveq #1, d2
	jsr registry.locate
	bne.w bad
	move.l #$1010, registry.Placement.Base(a3)
	move.l #1, registry.Placement.Section(a3)
	move.l #1, registry.Placement.Known(a3)
	move.l #EmptyValues, RuntimeInterfaceBuild+interface.Frame.Values
	move.l #semantics.HEADER_BYTES, RuntimeInterfaceBuild+interface.Frame.ValueBytes
	move.l #ProbeDependency, RuntimeInterfaceBuild+interface.Frame.Dependencies
	move.l #1, RuntimeInterfaceBuild+interface.Frame.DependencyCount
	move.l #RuntimeImports, RuntimeInterfaceBuild+interface.Frame.Imports
	move.l #2, RuntimeInterfaceBuild+interface.Frame.ImportCount
	lea RuntimeInterfaceBuild, a0
	jsr interface.build
	bne.w bad
	move.l RuntimeInterfaceBuild+interface.Frame.Used, RuntimeConsumerBuild+image.Frame.SemanticsBytes
	lea RuntimeConsumerBuild, a0
	bsr.w runtimeBuildObject
	bne.w bad
	lea RuntimeRegistry, a0
	lea RuntimeOwner, a1
	lea RuntimeConsumerName, a2
	moveq #8, d2
	jsr registry.add
	bne.w bad
	cmpi.l #1, d1
	bne.w bad
	.if DISK_REGISTRY
	jsr registry.store
	.else
	bsr.w runtimeResidentName
	.endif
	bne.w bad
	lea RuntimeRecords, a0
	moveq #1, d1
	moveq #1, d2
	moveq #1, d3
	jsr moduleRecords.appendFragment
	bne.w bad
	lea RuntimeRegistry, a0
	jsr storedRuntime.begin
	lea runtimeImportIdentity, a0
	lea RuntimeCallbackCount, a1
	jsr storedRuntime.setImportResolver
	move.l #RuntimeContext, RuntimeFrame+assembly.Frame.Context
	move.l #RuntimeSections, RuntimeFrame+assembly.Frame.Sections
	move.l #runtimeRelocation, RuntimeFrame+assembly.Frame.AddReloc
	move.l #$2004, RuntimeContext+package.Context.Pc
	move.w #2, RuntimeContext+package.Context.Pass
	move.l #RuntimeOrigins, RuntimeContext+package.Context.SectionBases
	move.w #sectionState.HUNK_MODE, RuntimeSections+sectionState.State.Mode
	move.w #2, RuntimeSections+sectionState.State.HunkCurrent
	lea RuntimeFrame, a0
	movea.l RuntimeRecords+memory.Block.Pointer, a1
	moveq #storedRuntime.EMIT, d0
	jsr storedRuntime.fragment
	bne.w bad
	cmpi.l #1, RuntimeCallbackCount
	bne.w bad
	cmpi.l #1, RuntimeRelocationCount
	bne.w bad
	cmpi.l #8, d2
	bne.w bad
	cmpi.l #$10, 4(a1)
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	move.l d0, -(sp)
	bsr.w releaseRuntimeRelocation
	move.l (sp)+, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; runtimeRelocationChecks

; Assign the same checked catalogue identity to resident pending images,
; without configuring disk ownership or persisting a file.
; A0=Registry,A2=qualified name,D1=module,D2=name bytes; others preserved.
runtimeResidentName	.block
	movem.l a1, -(sp)
	movea.l a2, a1
	jsr registry.name
	movem.l (sp)+, a1
	tst.l d0
	rts
	.bend  ; runtimeResidentName

; A0=build Frame with borrowed interface. D0/CCR=status; others preserved.
runtimeBuildObject	.block
	movem.l d1/a0-a2, -(sp)
	movea.l a0, a2
	clr.l image.Frame.Output(a2)
	jsr image.build
	bne.w done
	move.l image.Frame.Used(a2), d0
	lea RuntimeOwner, a0
	jsr memory.reserveExact
	bne.w done
	move.l memory.Block.Pointer(a0), image.Frame.Output(a2)
	move.l memory.Block.Capacity(a0), image.Frame.Capacity(a2)
	movea.l a2, a0
	jsr image.build
	bne.w done
	move.l image.Frame.Used(a2), RuntimeOwner+memory.Block.Used
done
	movem.l (sp)+, d1/a0-a2
	tst.l d0
	rts
	.bend  ; runtimeBuildObject

; A0=callback context,D1=consumer module,D2=import ordinal.
; D0/status,D1=source ID; all other registers preserved.
runtimeImportIdentity	.block
	cmpa.l #RuntimeCallbackCount, a0
	bne.w bad
	cmpi.l #1, d1
	bne.w bad
	cmpi.l #1, d2
	bne.w bad
	addq.l #1, RuntimeCallbackCount
	moveq #37, d1
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; runtimeImportIdentity

; Actual assembly AddReloc ABI: source section,target section,field offset,
; source symbol and patched value must survive the import-identity callback.
runtimeRelocation	.block
	cmpa.l #RuntimeFrame, a0
	bne.w bad
	cmpi.l #2, d0
	bne.w bad
	cmpi.l #1, d1
	bne.w bad
	cmpi.l #8, d2
	bne.w bad
	cmpi.l #37, d3
	bne.w bad
	cmpi.l #$10, d4
	bne.w bad
	addq.l #1, RuntimeRelocationCount
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; runtimeRelocation

; No caller retains a runtime lease after this component check.
releaseRuntimeRelocation	.block
	movem.l a0, -(sp)
	jsr storedRuntime.release
	lea RuntimeRegistry, a0
	jsr registry.release
	lea RuntimeOwner, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea RuntimeRecords, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movem.l (sp)+, a0
	rts
	.bend  ; releaseRuntimeRelocation
	.endsection
	.section data, kind=data
	.align 8
RuntimeValues
	.long semantics.MAGIC, 88, 1, semantics.HEADER_BYTES, 0, 68, 68, 16, 88, 0
	.long 7, 68, 16, semantics.FRAGMENT_ADDRESS, 0, 1, 0
	.byte "provider.address", 0, 0, 0, 0
	.align 4
RuntimeInterfaceBuild
	.long RuntimeValues, 88, 0, 0, InterfaceBuffer, 256, 0, 0, 0, 0, 0
RuntimeProviderInput
	.long 1, RuntimePayload, 4, 0, 0, image.KIND_DATA, 1, 4
RuntimeConsumerInput
	.long 1, RuntimePayload, 8, 0, 0, image.KIND_DATA, 1, 8
RuntimeImports
	.long 0, 7, 0, 7
RuntimeFixup
	.long 1, 4, $80000001, 0, 4, image.PROGRAM_ABSOLUTE, 0
RuntimeProviderBuild
	.long RuntimeProviderInput, 1, 0, 0, 0, 0, 0, InterfaceBuffer, 0
RuntimeConsumerBuild
	.long RuntimeConsumerInput, 1, RuntimeFixup, 1, 0, 0, 0, InterfaceBuffer, 0
RuntimeProviderName	.byte "provider"
RuntimeConsumerName	.byte "consumer"
	.align 4
RuntimePayload	.long 0, 0
RuntimeOrigins	.long 0, $1000, $2000, 0, 0, 0, 0, 0
	.endsection
	.section bss, kind=bss
	.align 4
RuntimeRegistry	.res byte, registry.STATE_BYTES
RuntimeOwner	.res byte, memory.Block.Used+4
RuntimeRecords	.res byte, memory.Block.Used+4
RuntimeFrame	.res byte, assembly.FRAME_BYTES
RuntimeContext	.res byte, package.CONTEXT_BYTES
RuntimeSections	.res byte, sectionState.SCRATCH_BYTES
RuntimeCallbackCount	.res long, 1
RuntimeRelocationCount	.res long, 1
	.endsection
	.section code, kind=code

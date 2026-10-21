; CLI orchestration for immutable object interfaces. Private and root consumers
; share readers; numeric linker bridges retain no source/frontend ownership.

; Ensure the shared final driver has a dependency reader. Called only while
; its semantic frontend is active, never against the discovery/configuration one.
; D0/CCR=status; other registers preserved.
beginStoredInterfaces	.block
	movem.l a0-a2, -(sp)
	tst.l StoredRoot+moduleProvider.State.Registry
	bne.w good
	lea StoredRoot, a0
	lea Front, a1
	lea PrivateRegistry, a2
	jsr moduleProvider.begin
	bne.w done
	move.l #storedInterfaceAddress, StoredRoot+moduleProvider.State.Address
	move.l #StoredRoot, StoredRoot+moduleProvider.State.AddressContext
good
	moveq #0, d0
done
	movem.l (sp)+, a0-a2
	tst.l d0
	rts
	.bend  ; beginStoredInterfaces

; A0=scope,A1=module name,D0=bytes -> D0/status,D1=module lexical index+1.
; Allocate just a normal closed module declaration, without any export install.
storedInterfaceModule	.block
	movem.l d2/a0-a2, -(sp)
	movea.l a0, a2
	jsr scopes.beginFileDerived
	bne.w done
	move.l d1, d2
	movea.l a2, a0
	jsr scopes.endFileDerived
	bne.w done
	movea.l a2, a0
	moveq #0, d0
	jsr scopes.endFile
	move.l d2, d1
done
	movem.l (sp)+, d2/a0-a2
	tst.l d0
	rts
	.bend  ; storedInterfaceModule

; D1=shared module lexical index+1 -> D0/status,D1=stored registry module ID.
; This small handle lookup never scans or copies an export interface.
storedInterfaceModuleId	.block
	movem.l d2/a0, -(sp)
	move.l StoredRoot+moduleProvider.State.Modules+memory.Block.Used, d2
	movea.l StoredRoot+moduleProvider.State.Modules+memory.Block.Pointer, a0
next
	tst.l d2
	beq.w bad
	cmp.l moduleProvider.Module.Local(a0), d1
	beq.w found
	adda.w #moduleProvider.MODULE_BYTES, a0
	subi.l #moduleProvider.MODULE_BYTES, d2
	bra.w next
found
	move.l moduleProvider.Module.Stored(a0), d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2/a0
	tst.l d0
	rts
	.bend  ; storedInterfaceModuleId

; D1=stored provider -> D0/status,D1=zero-based object dependency ordinal.
; Borrow filenames only until the current immutable object is constructed;
; registry rows cannot move during private compilation. The writer owns copies.
privateInterfaceDependency	.block
	movem.l d2-d6/a0-a2, -(sp)
	move.l d1, d6
	move.l PrivateObjectProviders+memory.Block.Used, d5
	movea.l PrivateObjectProviders+memory.Block.Pointer, a1
	moveq #0, d4
find
	cmp.l d5, d4
	bhs.w append
	cmp.l 0(a1, d4.l), d6
	beq.w found
	addq.l #4, d4
	bra.w find
append
	move.l d6, d0
	mulu.l #moduleRegistry.MODULE_BYTES, d0
	move.l d0, d2
	addi.l #moduleRegistry.MODULE_BYTES, d0
	cmp.l PrivateRegistry+moduleRegistry.State.Modules+memory.Block.Used, d0
	bhi.w bad
	movea.l PrivateRegistry+moduleRegistry.State.Modules+memory.Block.Pointer, a2
	adda.l d2, a2
	lea moduleRegistry.Module.FileName(a2), a2
	moveq #0, d3
name
	cmpi.l #moduleObjectStore.NAME_BYTES, d3
	bhs.w bad
	tst.b 0(a2, d3.l)
	beq.w nameReady
	addq.l #1, d3
	bra.w name
nameReady
	tst.l d3
	beq.w bad
	lea PrivateObjectDependencies, a0
	move.l memory.Block.Used(a0), d0
	addi.l #assemblyInterface.DEPENDENCY_BYTES, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l memory.Block.Used(a0), a1
	move.l a2, assemblyInterface.Dependency.Name(a1)
	move.l d3, assemblyInterface.Dependency.NameBytes(a1)
	addi.l #assemblyInterface.DEPENDENCY_BYTES, memory.Block.Used(a0)
	lea PrivateObjectProviders, a0
	move.l d5, d0
	addq.l #4, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	move.l d6, 0(a1, d5.l)
	addq.l #4, memory.Block.Used(a0)
found
	move.l d4, d1
	lsr.l #2, d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d6/a0-a2
	tst.l d0
	rts
	.bend  ; privateInterfaceDependency

; D1=private consumer identity,D2=required VALUE/TEMPLATE kind.
; D0/status,A1=borrowed sparse binding; other registers preserved. The caller
; copies fields before provider mutation. Same-name facets remain independent.
privateInterfaceBinding	.block
	movem.l d3/a0, -(sp)
	move.l StoredPrivate+moduleProvider.State.Bindings+memory.Block.Used, d3
	movea.l StoredPrivate+moduleProvider.State.Bindings+memory.Block.Pointer, a1
next
	tst.l d3
	beq.w bad
	cmp.l moduleProvider.Binding.Consumer(a1), d1
	bne.w advance
	cmp.l moduleProvider.Binding.Kind(a1), d2
	beq.w found
advance
	adda.w #moduleProvider.BINDING_BYTES, a1
	subi.l #moduleProvider.BINDING_BYTES, d3
	bra.w next
found
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d3/a0
	tst.l d0
	rts
	.bend  ; privateInterfaceBinding

; Compiler close callback A0=owned OFTM Block,A1=Context. Normalize demanded
; foreign closure identities before value-root capture. D0/status; others kept.
privateCloseTemplates	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l memory.Block.Pointer(a0), a6
	move.l moduleTemplates.Header.BindingCount(a6), d7
	adda.l moduleTemplates.Header.Bindings(a6), a6
next
	tst.l d7
	beq.w good
	move.l templateNames.Row.Facets(a6), d0
	andi.l #templateNames.VALUE_FACET, d0
	beq.w template
	move.l templateNames.Row.ValueEntity(a6), d1
	moveq #moduleProvider.VALUE, d2
	bsr.w privateInterfaceBinding
	bne.w template
	move.l moduleProvider.Binding.Entity(a1), d6
	move.l moduleProvider.Binding.Provider(a1), d1
	bsr.w privateInterfaceDependency
	bne.w bad
	addq.l #1, d1
	move.l d1, templateNames.Row.ValueDependency(a6)
	move.l d6, templateNames.Row.ValueEntity(a6)
template
	move.l templateNames.Row.Facets(a6), d0
	andi.l #templateNames.TEMPLATE_FACET, d0
	beq.w advance
	move.l templateNames.Row.TemplateEntity(a6), d1
	moveq #moduleProvider.TEMPLATE, d2
	bsr.w privateInterfaceBinding
	bne.w advance
	move.l moduleProvider.Binding.Entity(a1), d6
	move.l moduleProvider.Binding.Provider(a1), d1
	bsr.w privateInterfaceDependency
	bne.w bad
	addq.l #1, d1
	move.l d1, templateNames.Row.TemplateDependency(a6)
	move.l d6, templateNames.Row.TemplateEntity(a6)
advance
	adda.w #templateNames.ROW_BYTES, a6
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
	.bend  ; privateCloseTemplates

; A0=owned finalized OFMS Block,A1=Context. Preserve imported nominal identity
; by forwarding schema rows to the owning dependency instead of cloning types.
; D0/CCR=status; others preserved. Caller discards the image on failure.
privateCloseValues	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l memory.Block.Pointer(a0), a6
	move.l moduleInterface.Header.ExportCount(a6), d7
	adda.l moduleInterface.Header.Exports(a6), a6
next
	tst.l d7
	beq.w good
	cmpi.l #values.STRUCT_TYPE, moduleInterface.Export.Kind(a6)
	bne.w advance
	move.l moduleInterface.Export.Id(a6), d1
	moveq #moduleProvider.VALUE, d2
	bsr.w privateInterfaceBinding
	bne.w advance
	move.l moduleProvider.Binding.Entity(a1), d6
	move.l moduleProvider.Binding.Provider(a1), d1
	bsr.w privateInterfaceDependency
	bne.w bad
	move.l #moduleInterface.FORWARDED_ENTITY, moduleInterface.Export.Kind(a6)
	move.l d1, moduleInterface.Export.Low(a6)
	move.l d6, moduleInterface.Export.High(a6)
	clr.l moduleInterface.Export.Provenance(a6)
advance
	adda.w #moduleInterface.EXPORT_BYTES, a6
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
	.bend  ; privateCloseValues

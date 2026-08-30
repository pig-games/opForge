; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.module_use
	.cpu 68020
	.use opforge.cli.state
	.use opforge.cli.constants
	.use opforge.cli.token_util
	.use opforge.cli.text_output
	.use opforge.cli.strings
	.use opforge.cli.dos
	.use opforge.cli.path
	.use opforge.cli.module_discovery

	.section code, kind=code
	.pub

; Inputs:
;   state.NativeCliArgToken = module name token
;   state.NativeCliSourceLineNum = defining source line
;   state.NativeCliModuleDepth/state.NativeCliModuleCount describe the current module stack
; Outputs:
;   D0.L = 0 on success, 1 when the module table is full
;   state.NativeCliCurrentModuleId updated to the recorded module on success
;   state.NativeCliRootModuleId initialized on the first recorded module
; Clobbers:
;   D0-D3/A0-A1/CCR
; CCR:
;   Reflects D0.L on return
opforgeNativeCliRecordModule	.block
	movem.l d1-d3/a0-a1, -(sp)
	moveq #0, d0
	move.w state.NativeCliModuleCount, d0
	cmpi.w #constants.NATIVE_MODULE_TABLE_CAPACITY, d0
	bhs.w fail
	move.w d0, d3
	lea state.NativeCliArgToken, a0
	lea state.NativeCliModuleNameTable, a1
	moveq #0, d1
	move.w d3, d1
	lsl.l #6, d1
	adda.l d1, a1
	bsr.w token_util.opforgeNativeCliCopyTokenBuffer

	moveq #0, d1
	move.w d3, d1
	add.w d1, d1
	lea state.NativeCliModuleFileIdTable, a1
	move.w #1, 0(a1, d1.l)
	lea state.NativeCliModuleDepthTable, a1
	move.w state.NativeCliModuleDepth, 0(a1, d1.l)
	lea state.NativeCliModuleVisibilityTable, a1
	move.w #constants.NATIVE_PREPROCESS_VISIBILITY_PRIVATE, 0(a1, d1.l)

	moveq #0, d1
	move.w d3, d1
	lsl.l #2, d1
	lea state.NativeCliModuleLineTable, a1
	move.l state.NativeCliSourceLineNum, 0(a1, d1.l)

	tst.w state.NativeCliModuleCount
	bne.s haveRoot
	move.w d3, state.NativeCliRootModuleId

haveRoot
	move.w d3, state.NativeCliCurrentModuleId
	moveq #0, d1
	move.w state.NativeCliModuleDepth, d1
	add.w d1, d1
	lea state.NativeCliActiveModuleStack, a1
	move.w d3, 0(a1, d1.l)
	move.w #constants.NATIVE_PREPROCESS_VISIBILITY_PRIVATE, state.NativeCliPreprocessCurrentVisibility
	move.w state.NativeCliModuleCount, d0
	addq.w #1, d0
	move.w d0, state.NativeCliModuleCount
	move.w state.NativeCliModuleDepth, d0
	addq.w #1, d0
	move.w d0, state.NativeCliModuleDepth
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d3/a0-a1
	rts
	.bend  ; opforgeNativeCliRecordModule

; Inputs:
;   state.NativeCliCurrentModuleId = owning module
;   state.NativeCliSourceLineNum = import source line
;   state.NativeCliIncludeTarget = optional import alias token
; Outputs:
;   D0.L = 0 on success, 1 when the import table is full
;   A new import row is recorded for the current module on success
; Clobbers:
;   D0-D4/A0-A1/CCR
; CCR:
;   Reflects D0.L on return
opforgeNativeCliRecordImport	.block
	movem.l d1-d3/a0-a1, -(sp)
	moveq #0, d0
	move.w state.NativeCliImportCount, d0
	cmpi.w #constants.NATIVE_IMPORT_TABLE_CAPACITY, d0
	bhs.w fail
	move.w d0, d4
	moveq #0, d1
	move.w d4, d1
	add.w d1, d1
	lea state.NativeCliImportOwnerModuleTable, a1
	move.w state.NativeCliCurrentModuleId, 0(a1, d1.l)
	lea state.NativeCliImportModuleTable, a1
	clr.w 0(a1, d1.l)
	lea state.NativeCliImportFileIdTable, a1
	move.w #1, 0(a1, d1.l)

	moveq #0, d1
	move.w d4, d1
	lsl.l #2, d1
	lea state.NativeCliImportLineTable, a1
	move.l state.NativeCliSourceLineNum, 0(a1, d1.l)

	moveq #0, d1
	move.w d4, d1
	lsl.l #6, d1
	lea state.NativeCliImportAliasTable, a1
	adda.l d1, a1
	lea state.NativeCliIncludeTarget, a0
	bsr.w token_util.opforgeNativeCliCopyTokenBuffer

	move.w state.NativeCliImportCount, d0
	addq.w #1, d0
	move.w d0, state.NativeCliImportCount
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d3/a0-a1
	rts
	.bend  ; opforgeNativeCliRecordImport

; Inputs:
;   D3.W = import-select flags
;   D4.W = owning import table index
;   state.NativeCliArgToken = selected import token
;   state.NativeCliIncludeTarget = optional alias token
; Outputs:
;   D0.L = 0 on success, 1 when the import-select table is full
; Clobbers:
;   D0-D1, D6, A0-A1
; CCR:
;   Reflects D0.L on return
opforgeNativeCliRecordImportSelect	.block
	movem.l d1-d3/a0-a1, -(sp)
	moveq #0, d0
	move.w state.NativeCliImportSelectCount, d0
	cmpi.w #constants.NATIVE_IMPORT_SELECT_CAPACITY, d0
	bhs.w fail
	move.w d0, d6
	moveq #0, d1
	move.w d6, d1
	add.w d1, d1
	lea state.NativeCliImportSelectImportTable, a1
	move.w d4, 0(a1, d1.l)
	lea state.NativeCliImportSelectFlagsTable, a1
	move.w d3, 0(a1, d1.l)

	moveq #0, d1
	move.w d6, d1
	lsl.l #6, d1
	lea state.NativeCliImportSelectNameTable, a1
	adda.l d1, a1
	lea state.NativeCliArgToken, a0
	bsr.w token_util.opforgeNativeCliCopyTokenBuffer

	moveq #0, d1
	move.w d6, d1
	lsl.l #6, d1
	lea state.NativeCliImportSelectAliasTable, a1
	adda.l d1, a1
	lea state.NativeCliIncludeTarget, a0
	bsr.w token_util.opforgeNativeCliCopyTokenBuffer

	move.w state.NativeCliImportSelectCount, d0
	addq.w #1, d0
	move.w d0, state.NativeCliImportSelectCount
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d3/a0-a1
	rts
	.bend  ; opforgeNativeCliRecordImportSelect

; Record one generic logical-to-concrete section mapping owned by an import.
; Inputs: D4.W = import index; state.NativeCliArgToken = logical name;
;         state.NativeCliIncludeTarget = concrete name.
; Outputs: D0.L = 0 on success, 1 on capacity failure.
; Clobbers: D0-D3/A0-A1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliRecordImportSectionMapV1	.block
	movem.l d1-d3/a0-a1, -(sp)
	moveq #0, d0
	move.w state.NativeCliImportSectionMapCount, d0
	cmpi.w #constants.NATIVE_IMPORT_SECTION_MAP_CAPACITY, d0
	bhs.s fail
	move.w d0, d3
	move.l d3, d1
	add.l d1, d1
	lea state.NativeCliImportSectionMapImportTable, a1
	move.w d4, 0(a1, d1.l)
	move.l d3, d1
	lsl.l #6, d1
	lea state.NativeCliImportSectionMapLogicalTable, a1
	adda.l d1, a1
	lea state.NativeCliArgToken, a0
	bsr.w token_util.opforgeNativeCliCopyTokenBuffer
	move.l d3, d1
	lsl.l #6, d1
	lea state.NativeCliImportSectionMapConcreteTable, a1
	adda.l d1, a1
	lea state.NativeCliIncludeTarget, a0
	bsr.w token_util.opforgeNativeCliCopyTokenBuffer
	addq.w #1, state.NativeCliImportSectionMapCount
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d3/a0-a1
	rts
	.bend  ; opforgeNativeCliRecordImportSectionMapV1

; Record one public ordinary symbol for later `.use` alias resolution.
; Inputs: current parsed statement fields, module, and visibility in state.
; Outputs: D0 = 0 success/not applicable, 1 capacity or malformed name.
; Clobbers: D0-D4/A0-A1/CCR.
opforgeNativeCliRecordOrdinaryExportV1	.block
	movem.l d1-d5/a0-a1, -(sp)
	move.w state.NativeCliPreprocessCurrentVisibility, d0
	cmpi.w #constants.NATIVE_PREPROCESS_VISIBILITY_PUBLIC, d0
	bne.w ordinaryRecordOk
	tst.w state.NativeCliModuleDepth
	beq.w ordinaryRecordOk
	move.l state.NativeCliStmtLabelLen, d2
	beq.w ordinaryRecordOk
	cmpi.l #constants.TOKEN_BUFFER_CAPACITY, d2
	bcc.w ordinaryRecordFail
	moveq #0, d0
	move.w state.NativeCliOrdinaryExportCount, d0
	cmpi.w #constants.NATIVE_ORDINARY_EXPORT_CAPACITY, d0
	bhs.w ordinaryRecordFail
	move.l d0, d5
	move.l state.NativeCliOrdinaryExportNamePoolLen, d3
	move.l d3, d1
	add.l d2, d1
	addq.l #1, d1
	cmpi.l #constants.NATIVE_ORDINARY_EXPORT_NAME_POOL_CAPACITY, d1
	bhi.w ordinaryRecordFail
	move.l d1, d4
	move.l d5, d1
	add.l d1, d1
	lea state.NativeCliOrdinaryExportOwnerTable, a1
	move.w state.NativeCliCurrentModuleId, 0(a1, d1.l)
	move.l d5, d1
	lsl.l #2, d1
	lea state.NativeCliOrdinaryExportNameOffsetTable, a1
	move.l d3, 0(a1, d1.l)
	lea state.NativeCliOrdinaryExportNamePool, a1
	adda.l d3, a1
	lea state.NativeCliSourceLine, a0
	move.l state.NativeCliStmtLabelStart, d0
	beq.w ordinaryRecordFail
	subq.l #1, d0
	adda.l d0, a0
ordinaryRecordCopy
	move.b (a0)+, (a1)+
	subq.l #1, d2
	bne.s ordinaryRecordCopy
	clr.b (a1)
	move.l d4, state.NativeCliOrdinaryExportNamePoolLen
	move.l d5, d0
	lsl.l #2, d0
	lea state.NativeCliOrdinaryExportNextTable, a1
	moveq #0, d1
	move.w state.NativeCliCurrentModuleId, d1
	lsl.l #2, d1
	lea state.NativeCliModuleOrdinaryExportHeadTable, a0
	move.l 0(a0, d1.l), d3
	move.l d3, 0(a1, d0.l)
	addq.l #1, d5
	move.l d5, 0(a0, d1.l)
	addq.w #1, state.NativeCliOrdinaryExportCount
ordinaryRecordOk
	moveq #0, d0
	bra.s ordinaryRecordReturn
ordinaryRecordFail
	moveq #1, d0
ordinaryRecordReturn
	movem.l (sp)+, d1-d5/a0-a1
	rts
	.bend  ; opforgeNativeCliRecordOrdinaryExportV1

; Rewrite one imported ordinary symbol to its fully-qualified exported name.
; Inputs: A0/D0 = token; A1/D1 = active module name.
; Outputs: A0/D0 = qualified name and D1 = 0, or D1 = 1 when unmapped.
; Clobbers: D0-D7/A0-A5/CCR.
opforgeNativeCliResolveImportedOrdinaryNameV1	.block
	movem.l d2-d7/a2-a5, -(sp)
	movea.l a0, a3
	move.l d0, d6
	movea.l a1, a4
	move.l d1, d5
	tst.l d6
	beq.w ordinaryResolveNo
	tst.l d5
	beq.w ordinaryResolveNo
	moveq #0, d7
ordinaryResolveModuleLoop
	cmp.w state.NativeCliModuleCount, d7
	bhs.w ordinaryResolveNo
	move.l d7, d0
	lsl.l #6, d0
	lea state.NativeCliModuleNameTable, a0
	adda.l d0, a0
	bsr.w token_util.opforgeNativeCliTokenLen
	movea.l a4, a1
	move.l d5, d1
	bsr.w compareFoldedExact
	tst.l d0
	bne.s ordinaryResolveHaveOwner
	addq.w #1, d7
	bra.s ordinaryResolveModuleLoop

ordinaryResolveHaveOwner
	move.w d7, d2
	moveq #0, d7
ordinaryResolveImportLoop
	cmp.w state.NativeCliImportCount, d7
	bhs.w ordinaryResolveNo
	move.l d7, d0
	add.l d0, d0
	lea state.NativeCliImportOwnerModuleTable, a0
	cmp.w 0(a0, d0.l), d2
	bne.w ordinaryResolveImportNext
	lea state.NativeCliImportModuleTable, a0
	move.w 0(a0, d0.l), d4

	; First resolve explicit aliases, Rust's implicit final-component qualifier,
	; and complete `module.name.NAME` forms.
	moveq #0, d3
ordinaryResolveDotScan
	cmp.l d6, d3
	bhs.w ordinaryResolveSelections
	cmpi.b #'.', 0(a3, d3.l)
	beq.s ordinaryResolveQualifier
	addq.l #1, d3
	bra.s ordinaryResolveDotScan
ordinaryResolveQualifier
	tst.l d3
	beq.w ordinaryResolveSelections
	move.l d3, d5
	move.l d7, d0
	lsl.l #6, d0
	lea state.NativeCliImportAliasTable, a0
	adda.l d0, a0
	bsr.w token_util.opforgeNativeCliTokenLen
	tst.l d0
	beq.s ordinaryResolveOwnerQualifier
	movea.l a3, a1
	move.l d5, d1
	bsr.w compareFoldedExact
	move.l d5, d3
	tst.l d0
	bne.s ordinaryResolveQualifiedExport

ordinaryResolveOwnerQualifier
	move.l d4, d0
	lsl.l #6, d0
	lea state.NativeCliModuleNameTable, a0
	adda.l d0, a0
	bsr.w token_util.opforgeNativeCliTokenLen
	movea.l a0, a2
	move.l d0, d3
	move.l d0, d1
ordinaryResolveOwnerScan
	tst.l d1
	beq.s ordinaryResolveOwnerReady
	cmpi.b #'.', (a0)+
	bne.s ordinaryResolveOwnerNext
	movea.l a0, a2
	move.l d1, d3
	subq.l #1, d3
ordinaryResolveOwnerNext
	subq.l #1, d1
	bra.s ordinaryResolveOwnerScan
ordinaryResolveOwnerReady
	movea.l a2, a0
	move.l d3, d0
	movea.l a3, a1
	move.l d5, d1
	bsr.w compareFoldedExact
	move.l d5, d3
	tst.l d0
	bne.s ordinaryResolveQualifiedExport
ordinaryResolveModuleQualifier
	move.l d4, d0
	lsl.l #6, d0
	lea state.NativeCliModuleNameTable, a0
	adda.l d0, a0
	bsr.w token_util.opforgeNativeCliTokenLen
	move.l d0, d5
	cmp.l d5, d6
	bls.s ordinaryResolveSelections
	cmpi.b #'.', 0(a3, d5.l)
	bne.s ordinaryResolveSelections
	movea.l a3, a1
	move.l d5, d1
	bsr.w compareFoldedExact
	move.l d5, d3
	tst.l d0
	beq.s ordinaryResolveSelections
ordinaryResolveQualifiedExport
	movea.l a3, a2
	adda.l d3, a2
	addq.l #1, a2
	move.l d6, d0
	sub.l d3, d0
	subq.l #1, d0
	move.l d0, d3
	bsr.w ordinaryExportIndex
	tst.l d0
	bmi.s ordinaryResolveSelections
	bra.w ordinaryResolveBuild

ordinaryResolveSelections
	moveq #0, d3
ordinaryResolveSelectLoop
	cmp.w state.NativeCliImportSelectCount, d3
	bhs.w ordinaryResolveImportNext
	move.l d3, d0
	add.l d0, d0
	lea state.NativeCliImportSelectImportTable, a0
	cmp.w 0(a0, d0.l), d7
	bne.w ordinaryResolveSelectNext
	lea state.NativeCliImportSelectFlagsTable, a0
	move.w 0(a0, d0.l), d0
	btst #1, d0
	bne.s ordinaryResolveWildcard
	move.l d3, d0
	lsl.l #6, d0
	lea state.NativeCliImportSelectAliasTable, a0
	adda.l d0, a0
	bsr.w token_util.opforgeNativeCliTokenLen
	tst.l d0
	bne.s ordinaryResolveCompareSelected
	move.l d3, d0
	lsl.l #6, d0
	lea state.NativeCliImportSelectNameTable, a0
	adda.l d0, a0
	bsr.w token_util.opforgeNativeCliTokenLen
ordinaryResolveCompareSelected
	move.l d3, d5
	movea.l a3, a1
	move.l d6, d1
	bsr.w compareFoldedExact
	move.l d5, d3
	tst.l d0
	beq.s ordinaryResolveSelectNext
	move.l d3, d0
	lsl.l #6, d0
	lea state.NativeCliImportSelectNameTable, a2
	adda.l d0, a2
	movea.l a2, a0
	bsr.w token_util.opforgeNativeCliTokenLen
	move.l d0, d3
	bsr.w ordinaryExportIndex
	tst.l d0
	bpl.s ordinaryResolveBuild
	bra.w ordinaryResolveImportNext

ordinaryResolveWildcard
	; Wildcard imports expose only names recorded while the target module was
	; public; local scoped lookup already ran before this callback.
	movea.l a3, a2
	move.l d6, d3
	bsr.w ordinaryExportIndex
	tst.l d0
	bpl.s ordinaryResolveBuild

ordinaryResolveSelectNext
	addq.w #1, d3
	bra.w ordinaryResolveSelectLoop
ordinaryResolveImportNext
	addq.w #1, d7
	bra.w ordinaryResolveImportLoop

ordinaryResolveBuild
	; A2/D3 is the public export slice and D4 is its target module.
	movea.l a2, a5
	move.l d3, d2
	move.l d4, d0
	lsl.l #6, d0
	lea state.NativeCliModuleNameTable, a1
	adda.l d0, a1
	lea state.NativeCliResolvedImportName, a0
	moveq #0, d1
	moveq #constants.TOKEN_BUFFER_CAPACITY - 1, d3

ordinaryResolveBuildModule
	move.b (a1)+, d0
	beq.s ordinaryResolveBuildDot
	tst.w d3
	beq.w ordinaryResolveNo
	move.b d0, (a0)+
	addq.l #1, d1
	subq.w #1, d3
	bra.s ordinaryResolveBuildModule

ordinaryResolveBuildDot
	tst.w d3
	beq.w ordinaryResolveNo
	move.b #'.', (a0)+
	addq.l #1, d1
	subq.w #1, d3
	movea.l a5, a1
ordinaryResolveBuildName
	tst.l d2
	beq.s ordinaryResolveBuilt
	tst.w d3
	beq.w ordinaryResolveNo
	move.b (a1)+, (a0)+
	addq.l #1, d1
	subq.l #1, d2
	subq.w #1, d3
	bra.s ordinaryResolveBuildName
ordinaryResolveBuilt
	clr.b (a0)
	lea state.NativeCliResolvedImportName, a0
	move.l d1, d0
	moveq #0, d1
	bra.s ordinaryResolveReturn

ordinaryResolveNo
	moveq #0, d0
	moveq #1, d1
ordinaryResolveReturn
	movem.l (sp)+, d2-d7/a2-a5
	rts
	.bend  ; opforgeNativeCliResolveImportedOrdinaryNameV1

; Find public export A2/D3 owned by target module D4.W.
; Outputs: D0 = row or -1. Clobbers: D0-D3/A0-A1/CCR.
ordinaryExportIndex	.block
	movem.l d2-d6/a2-a5, -(sp)
	movea.l a2, a5
	move.l d3, d5
	moveq #0, d6
	move.w d4, d6
	lsl.l #2, d6
	lea state.NativeCliModuleOrdinaryExportHeadTable, a0
	move.l 0(a0, d6.l), d6
ordinaryExportLoop
	tst.l d6
	beq.s ordinaryExportNo
	subq.l #1, d6
	move.l d6, d0
	lsl.l #2, d0
	lea state.NativeCliOrdinaryExportNameOffsetTable, a0
	move.l 0(a0, d0.l), d0
	lea state.NativeCliOrdinaryExportNamePool, a0
	adda.l d0, a0
	bsr.w token_util.opforgeNativeCliTokenLen
	movea.l a5, a1
	move.l d5, d1
	bsr.w compareFoldedExact
	tst.l d0
	bne.s ordinaryExportYes
ordinaryExportNext
	move.l d6, d0
	lsl.l #2, d0
	lea state.NativeCliOrdinaryExportNextTable, a0
	move.l 0(a0, d0.l), d6
	bra.s ordinaryExportLoop
ordinaryExportYes
	move.l d6, d0
	bra.s ordinaryExportReturn
ordinaryExportNo
	moveq #-1, d0
ordinaryExportReturn
	movem.l (sp)+, d2-d6/a2-a5
	rts
	.bend  ; ordinaryExportIndex

; Materialize the preprocessor names exposed by one completed `.use` import.
; Inputs: D6.W = import table index. Outputs: D0 = 0 success, 1 overflow.
; Public definitions owned by the imported module become bindings owned by the
; importing module. Private definitions never receive a binding.
opforgeNativeCliBindImportDefinitionsV1	.block
	movem.l d1-d7/a0-a4, -(sp)
	moveq #0, d0
	move.w d6, d0
	add.w d0, d0
	lea state.NativeCliImportModuleTable, a0
	move.w 0(a0, d0.l), d5
	lea state.NativeCliImportOwnerModuleTable, a0
	move.w 0(a0, d0.l), d4
	moveq #0, d7
definitionLoop
	cmp.w state.NativeCliPreprocessDefinitionCount, d7
	bcc.w ok
	move.l d7, d0
	add.l d0, d0
	lea state.NativeCliPreprocessDefinitionOwner, a0
	cmp.w 0(a0, d0.l), d5
	bne.w nextDefinition
	lea state.NativeCliPreprocessDefinitionVisibility, a0
	cmpi.b #constants.NATIVE_PREPROCESS_VISIBILITY_PUBLIC, 0(a0, d7.w)
	bne.w nextDefinition
	bsr.w opforgeNativeCliDefinitionNamePtrV1
	tst.l d1
	bne.w fail
	movea.l a0, a3
	move.l d0, d3
	clr.w d2
	moveq #0, d1
selectLoop
	cmp.w state.NativeCliImportSelectCount, d1
	bcc.s selectDone
	move.l d1, d0
	add.l d0, d0
	lea state.NativeCliImportSelectImportTable, a0
	cmp.w 0(a0, d0.l), d6
	bne.s selectNext
	move.w #1, d2
	lea state.NativeCliImportSelectFlagsTable, a0
	move.w 0(a0, d0.l), d0
	btst #1, d0
	bne.s bindCanonical
	move.l d1, d0
	lsl.l #6, d0
	lea state.NativeCliImportSelectNameTable, a0
	adda.l d0, a0
	movea.l a3, a1
	move.l d3, d0
	bsr.w compareTokenToDefinitionName
	tst.l d0
	beq.s selectNext
	move.l d1, d0
	movea.l a3, a0
	bsr.w appendBindingDefinitionName
	tst.l d0
	bne.w fail
	bra.s selectNext
bindCanonical
	movea.l a3, a0
	bsr.w appendBindingDefinitionName
	tst.l d0
	bne.w fail
selectNext
	addq.w #1, d1
	bra.s selectLoop
selectDone
	tst.w d2
	bne.s nextDefinition
	moveq #0, d0
	move.w d6, d0
	lsl.l #6, d0
	lea state.NativeCliImportAliasTable, a0
	adda.l d0, a0
	tst.b (a0)
	beq.s bindModuleName
	bsr.w appendQualifiedBinding
	tst.l d0
	bne.w fail
bindModuleName
	moveq #0, d0
	move.w d5, d0
	lsl.l #6, d0
	lea state.NativeCliModuleNameTable, a0
	adda.l d0, a0
	bsr.w appendQualifiedBinding
	tst.l d0
	bne.w fail
nextDefinition
	addq.w #1, d7
	bra.w definitionLoop
ok
	moveq #0, d0
	bra.s returnBind
fail
	moveq #1, d0
returnBind
	movem.l (sp)+, d1-d7/a0-a4
	rts
	.bend  ; opforgeNativeCliBindImportDefinitionsV1

; Match one definition name against the names visible in the current module.
; Inputs: D7.W definition index, A0/D0 canonical name, A1/D1 invocation name.
; Outputs: D0 = 1 match, 0 no match.
opforgeNativeCliDefinitionInvocationNameMatchesV1	.block
	movem.l d2-d6/a0-a3, -(sp)
	movea.l a0, a2
	move.l d0, d2
	movea.l a1, a3
	move.l d1, d3
	moveq #-1, d4
	tst.w state.NativeCliModuleDepth
	beq.s haveOwner
	moveq #0, d4
	move.w state.NativeCliCurrentModuleId, d4
haveOwner
	move.l d7, d0
	add.l d0, d0
	lea state.NativeCliPreprocessDefinitionOwner, a0
	cmp.w 0(a0, d0.l), d4
	bne.s imported
	movea.l a2, a0
	move.l d2, d0
	movea.l a3, a1
	move.l d3, d1
	bsr.w compareFoldedExact
	bra.s returnMatch
imported
	moveq #0, d5
bindingLoop
	cmp.w state.NativeCliPreprocessImportBindingCount, d5
	bcc.s noMatch
	move.l d5, d0
	add.l d0, d0
	lea state.NativeCliPreprocessImportBindingOwnerTable, a0
	cmp.w 0(a0, d0.l), d4
	bne.s bindingNext
	lea state.NativeCliPreprocessImportBindingDefinitionTable, a0
	cmp.w 0(a0, d0.l), d7
	bne.s bindingNext
	move.l d5, d0
	mulu #constants.NATIVE_PREPROCESS_IMPORT_BINDING_NAME_CAPACITY, d0
	lea state.NativeCliPreprocessImportBindingNameTable, a0
	adda.l d0, a0
	movea.l a3, a1
	move.l d3, d1
	bsr.w compareTokenExact
	tst.l d0
	bne.s returnMatch
bindingNext
	addq.w #1, d5
	bra.s bindingLoop
noMatch
	moveq #0, d0
returnMatch
	movem.l (sp)+, d2-d6/a0-a3
	rts
	.bend  ; opforgeNativeCliDefinitionInvocationNameMatchesV1

opforgeNativeCliEmitImportRecord	.block
	movem.l d0-d4/a0-a1, -(sp)
	tst.w state.NativeCliDebugEnabled
	beq.w done
	move.l #strings.UseImportText, d1
	jsr dos.putStr
	moveq #0, d0
	move.w d4, d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	bsr.w text_output.opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea state.NativeCliImportOwnerModuleTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	bsr.w text_output.opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea state.NativeCliImportModuleTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	bsr.w text_output.opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea state.NativeCliImportFileIdTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	bsr.w text_output.opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	lsl.l #2, d0
	lea state.NativeCliImportLineTable, a0
	move.l 0(a0, d0.l), d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	bsr.w text_output.opforgeNativeCliPutSpace

	bsr.w opforgeNativeCliImportAliasPtr
	bsr.w token_util.opforgeNativeCliTokenLen
	move.w d0, d3
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	tst.w d3
	beq.s newline
	bsr.w text_output.opforgeNativeCliPutSpace
	bsr.w opforgeNativeCliImportAliasPtr
	move.l a0, d1
	jsr dos.putStr

newline
	move.l #strings.NewlineText, d1
	jsr dos.putStr
done
	movem.l (sp)+, d0-d4/a0-a1
	rts
	.bend  ; opforgeNativeCliEmitImportRecord

opforgeNativeCliEmitImportSelectRecord	.block
	movem.l d0-d4/d6-d7/a0-a1, -(sp)
	tst.w state.NativeCliDebugEnabled
	beq.w done
	move.l #strings.UseSelectText, d1
	jsr dos.putStr
	moveq #0, d0
	move.w d4, d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	bsr.w text_output.opforgeNativeCliPutSpace
	moveq #0, d0
	move.w d7, d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	bsr.w text_output.opforgeNativeCliPutSpace

	bsr.w opforgeNativeCliImportSelectNamePtr
	bsr.w token_util.opforgeNativeCliTokenLen
	move.w d0, d3
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	bsr.w text_output.opforgeNativeCliPutSpace
	bsr.w opforgeNativeCliImportSelectNamePtr
	move.l a0, d1
	jsr dos.putStr
	bsr.w text_output.opforgeNativeCliPutSpace

	bsr.w opforgeNativeCliImportSelectAliasPtr
	bsr.w token_util.opforgeNativeCliTokenLen
	move.w d0, d3
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	tst.w d3
	beq.s flags
	bsr.w text_output.opforgeNativeCliPutSpace
	bsr.w opforgeNativeCliImportSelectAliasPtr
	move.l a0, d1
	jsr dos.putStr

flags
	bsr.w text_output.opforgeNativeCliPutSpace
	moveq #0, d0
	move.w d6, d0
	add.w d0, d0
	lea state.NativeCliImportSelectFlagsTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	move.l #strings.NewlineText, d1
	jsr dos.putStr
done
	movem.l (sp)+, d0-d4/d6-d7/a0-a1
	rts
	.bend  ; opforgeNativeCliEmitImportSelectRecord

opforgeNativeCliEmitImportWildcardRecord	.block
	movem.l d0-d4, -(sp)
	tst.w state.NativeCliDebugEnabled
	beq.s done
	move.l #strings.UseWildcardText, d1
	jsr dos.putStr
	moveq #0, d0
	move.w d4, d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	bsr.w text_output.opforgeNativeCliPutSpace
	moveq #0, d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	move.l #strings.NewlineText, d1
	jsr dos.putStr
done
	movem.l (sp)+, d0-d4
	rts
	.bend  ; opforgeNativeCliEmitImportWildcardRecord

; Inputs:
;   state.NativeCliArgToken = bare imported module name token
;   state.NativeCliModulePathTable/state.NativeCliModulePathCount = candidate search roots
; Outputs:
;   D0.L = resolved module id on success
;   D1.L = 0 on success, 1 when no readable module source is found or path assembly fails
;   state.NativeCliIncludePath = last candidate path attempted
;   state.NativeCliResolvedModuleId updated on success
; Clobbers:
;   D0-D1/D6-D7/A0-A1/CCR
; CCR:
;   Reflects D1.L on return
opforgeNativeCliResolveBareUseModule	.block
	movem.l d2-d7/a0-a1, -(sp)
	clr.w d7

reuseLoop
	move.w state.NativeCliModuleCount, d0
	cmp.w d0, d7
	bhs.w searchPaths
	lea state.NativeCliArgToken, a0
	moveq #0, d0
	move.w d7, d0
	lsl.l #6, d0
	lea state.NativeCliModuleNameTable, a1
	adda.l d0, a1
	jsr token_util.opforgeNativeCliTokenEquals
	bne.s foundLoaded
	addq.w #1, d7
	bra.w reuseLoop

foundLoaded
	bsr.w moduleIsActive
	tst.l d0
	bne.s fail
	move.w d7, state.NativeCliResolvedModuleId
	moveq #0, d0
	move.w state.NativeCliResolvedModuleId, d0
	moveq #0, d1
	bra.w return

searchPaths
	jsr module_discovery.resolveDeclaredModuleV1
	tst.l d0
	bne.s fail
	move.w state.NativeCliModuleCount, d6
	move.w d6, state.NativeCliResolvedModuleId
	moveq #0, d0
	move.w state.NativeCliResolvedModuleId, d0
	moveq #0, d1
	bra.s return

fail
	moveq #1, d1

return
	movem.l (sp)+, d2-d7/a0-a1
	rts
	.bend  ; opforgeNativeCliResolveBareUseModule

opforgeNativeCliEmitModuleRecord	.block
	movem.l d0-d4/a0-a1, -(sp)
	tst.w state.NativeCliDebugEnabled
	beq.w done
	move.w d0, d4
	cmp.w state.NativeCliRootModuleId, d4
	bne.s def
	move.l #strings.ModRootText, d1
	jsr dos.putStr
	moveq #0, d0
	move.w d4, d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	move.l #strings.NewlineText, d1
	jsr dos.putStr

def
	move.l #strings.ModDefText, d1
	jsr dos.putStr
	moveq #0, d0
	move.w d4, d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	bsr.w text_output.opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea state.NativeCliModuleFileIdTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	bsr.w text_output.opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	lsl.l #2, d0
	lea state.NativeCliModuleLineTable, a0
	move.l 0(a0, d0.l), d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	bsr.w text_output.opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea state.NativeCliModuleDepthTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	bsr.w text_output.opforgeNativeCliPutSpace

	bsr.w opforgeNativeCliModuleNamePtr
	bsr.w token_util.opforgeNativeCliTokenLen
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	bsr.w text_output.opforgeNativeCliPutSpace
	bsr.w opforgeNativeCliModuleNamePtr
	move.l a0, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr
done
	movem.l (sp)+, d0-d4/a0-a1
	rts
	.bend  ; opforgeNativeCliEmitModuleRecord

opforgeNativeCliEmitModuleCompatibility	.block
	movem.l d0/d4/a0, -(sp)
	tst.w state.NativeCliDebugEnabled
	beq.s done
	move.w d0, d4
	move.l #strings.ModuleFoundText, d1
	jsr dos.putStr
	bsr.w opforgeNativeCliModuleNamePtr
	move.l a0, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr
done
	movem.l (sp)+, d0/d4/a0
	rts
	.bend  ; opforgeNativeCliEmitModuleCompatibility

; Inputs:
;   state.NativeCliCurrentModuleId/state.NativeCliModuleDepth describe the active module
;   state.NativeCliSourceLineNum = closing source line
; Outputs:
;   D0.L = 0 on success, 1 when there is no open module to close
;   state.NativeCliModuleDepth decremented and state.NativeCliCurrentModuleId restored on success
; Clobbers:
;   D0-D4/A0-A1/CCR
; CCR:
;   Reflects D0.L on return
opforgeNativeCliEmitCloseModule	.block
	movem.l d1-d4/a0-a1, -(sp)
	tst.w state.NativeCliModuleDepth
	beq.s fail
	moveq #0, d0
	move.w state.NativeCliModuleDepth, d0
	subq.w #1, d0
	move.w d0, state.NativeCliModuleDepth
	moveq #0, d0
	move.w state.NativeCliCurrentModuleId, d0
	bsr.w opforgeNativeCliEmitModuleEndRecord
	bsr.w opforgeNativeCliRestoreParentModule
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d4/a0-a1
	rts
	.bend  ; dos.closeModule

	.priv

; Return nonzero when loaded module D7.W is on the active source-module stack.
; Inputs: D7.W = module id. Outputs: D0 = boolean.
; Clobbers: D0-D3/A0/CCR. CCR: reflects D0.
moduleIsActive	.block
	moveq #0, d2

activeLoop
	cmp.w state.NativeCliModuleDepth, d2
	bhs.s activeNo
	move.l d2, d0
	add.l d0, d0
	lea state.NativeCliActiveModuleStack, a0
	cmp.w 0(a0, d0.l), d7
	beq.s activeYes
	addq.w #1, d2
	bra.s activeLoop

activeNo
	moveq #0, d0
	rts

activeYes
	moveq #1, d0
	rts
	.bend  ; moduleIsActive

opforgeNativeCliEmitModuleEndRecord	.block
	movem.l d0-d4/a0-a1, -(sp)
	tst.w state.NativeCliDebugEnabled
	beq.s done
	move.w d0, d4
	move.l #strings.ModEndText, d1
	jsr dos.putStr
	moveq #0, d0
	move.w d4, d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	bsr.w text_output.opforgeNativeCliPutSpace
	moveq #1, d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	bsr.w text_output.opforgeNativeCliPutSpace
	move.l state.NativeCliSourceLineNum, d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	bsr.w text_output.opforgeNativeCliPutSpace
	moveq #0, d0
	move.w state.NativeCliModuleDepth, d0
	bsr.w text_output.opforgeNativeCliPutU16Decimal
	move.l #strings.NewlineText, d1
	jsr dos.putStr
done
	movem.l (sp)+, d0-d4/a0-a1
	rts
	.bend  ; opforgeNativeCliEmitModuleEndRecord

opforgeNativeCliRestoreParentModule	.block
	movem.l d1-d3/a0, -(sp)
	tst.w state.NativeCliModuleDepth
	bne.s find
	clr.w state.NativeCliCurrentModuleId
	move.w #constants.NATIVE_PREPROCESS_VISIBILITY_PRIVATE, state.NativeCliPreprocessCurrentVisibility
	bra.s return

find
	move.w state.NativeCliModuleDepth, d0
	subq.w #1, d0
	move.w state.NativeCliModuleCount, d1
	beq.s clear
	subq.w #1, d1

loop
	moveq #0, d2
	move.w d1, d2
	add.w d2, d2
	lea state.NativeCliModuleDepthTable, a0
	move.w 0(a0, d2.l), d3
	cmp.w d0, d3
	beq.s found
	dbra d1, loop

clear
	clr.w state.NativeCliCurrentModuleId
	bra.s return

found
	move.w d1, state.NativeCliCurrentModuleId
	moveq #0, d2
	move.w d1, d2
	add.w d2, d2
	lea state.NativeCliModuleVisibilityTable, a0
	move.w 0(a0, d2.l), state.NativeCliPreprocessCurrentVisibility

return
	movem.l (sp)+, d1-d3/a0
	rts
	.bend  ; opforgeNativeCliRestoreParentModule

opforgeNativeCliModuleNamePtr	.block
	moveq #0, d0
	move.w d4, d0
	lsl.l #6, d0
	lea state.NativeCliModuleNameTable, a0
	adda.l d0, a0
	rts
	.bend  ; opforgeNativeCliModuleNamePtr

opforgeNativeCliImportAliasPtr	.block
	moveq #0, d0
	move.w d4, d0
	lsl.l #6, d0
	lea state.NativeCliImportAliasTable, a0
	adda.l d0, a0
	rts
	.bend  ; opforgeNativeCliImportAliasPtr

opforgeNativeCliImportSelectNamePtr	.block
	moveq #0, d0
	move.w d6, d0
	lsl.l #6, d0
	lea state.NativeCliImportSelectNameTable, a0
	adda.l d0, a0
	rts
	.bend  ; opforgeNativeCliImportSelectNamePtr

opforgeNativeCliImportSelectAliasPtr	.block
	moveq #0, d0
	move.w d6, d0
	lsl.l #6, d0
	lea state.NativeCliImportSelectAliasTable, a0
	adda.l d0, a0
	rts
	.bend  ; opforgeNativeCliImportSelectAliasPtr

; Extract a stored macro/segment/statement's canonical invocation name.
; Inputs: D7.W definition index. Outputs: A0/D0 name slice, D1 status.
opforgeNativeCliDefinitionNamePtrV1	.block
	move.l d7, d0
	mulu #constants.NATIVE_PREPROCESS_DEFINITION_HEADER_CAPACITY, d0
	lea state.NativeCliPreprocessDefinitionHeader, a0
	adda.l d0, a0
	move.l d7, d2
	add.l d2, d2
	lea state.NativeCliPreprocessDefinitionHeaderLen, a1
	moveq #0, d0
	move.w 0(a1, d2.l), d0
	bsr.w skipDefinitionWhitespace
	lea state.NativeCliPreprocessDefinitionKind, a1
	cmpi.b #constants.NATIVE_PREPROCESS_DEFINITION_KIND_STATEMENT, 0(a1, d7.w)
	bne.s scan
	cmpi.l #10, d0
	bcs.s nameFail
	adda.l #10, a0
	subi.l #10, d0
	bsr.w skipDefinitionWhitespace
scan
	moveq #0, d2
nameLoop
	cmp.l d0, d2
	bcc.s nameDone
	move.b 0(a0, d2.l), d1
	cmpi.b #' ', d1
	beq.s nameDone
	cmpi.b #9, d1
	beq.s nameDone
	cmpi.b #':', d1
	beq.s nameDone
	cmpi.b #';', d1
	beq.s nameDone
	addq.l #1, d2
	bra.s nameLoop
nameDone
	tst.l d2
	beq.s nameFail
	move.l d2, d0
	moveq #0, d1
	rts
nameFail
	moveq #1, d1
	rts
	.bend  ; opforgeNativeCliDefinitionNamePtrV1

skipDefinitionWhitespace	.block
	tst.l d0
	beq.s whitespaceDone
whitespaceLoop
	cmpi.b #' ', (a0)
	beq.s whitespaceOne
	cmpi.b #9, (a0)
	bne.s whitespaceDone
whitespaceOne
	addq.l #1, a0
	subq.l #1, d0
	bne.s whitespaceLoop
whitespaceDone
	rts
	.bend  ; skipDefinitionWhitespace

; Append A0/D3 as one direct binding for D4 owner and D7 definition.
appendBindingDefinitionName	.block
	movem.l d1-d3/a1-a2, -(sp)
	move.l d3, d1
	bsr.w appendBindingSlice
	movem.l (sp)+, d1-d3/a1-a2
	rts
	.bend  ; appendBindingDefinitionName

; Append the zero-terminated token at A0 as a binding.
appendBindingToken	.block
	movem.l d1-d3/a1-a2, -(sp)
	movea.l a0, a2
	moveq #0, d1
bindingTokenLen
	cmpi.l #constants.NATIVE_PREPROCESS_IMPORT_BINDING_NAME_CAPACITY - 1, d1
	bcc.s bindingTokenFail
	tst.b 0(a2, d1.l)
	beq.s bindingTokenReady
	addq.l #1, d1
	bra.s bindingTokenLen
bindingTokenReady
	bsr.w appendBindingSlice
	bra.s bindingTokenReturn
bindingTokenFail
	moveq #1, d0
bindingTokenReturn
	movem.l (sp)+, d1-d3/a1-a2
	rts
	.bend  ; appendBindingToken

; Append qualifier token A0 + '.' + canonical A3/D3.
appendQualifiedBinding	.block
	movem.l d1-d3/a0-a2, -(sp)
	movea.l a0, a2
	moveq #0, d2
qualifierLen
	cmpi.l #constants.NATIVE_PREPROCESS_IMPORT_BINDING_NAME_CAPACITY - 2, d2
	bcc.s qualifiedFail
	tst.b 0(a2, d2.l)
	beq.s qualifierReady
	addq.l #1, d2
	bra.s qualifierLen
qualifierReady
	move.l d2, d1
	addq.l #1, d1
	add.l d3, d1
	cmpi.l #constants.NATIVE_PREPROCESS_IMPORT_BINDING_NAME_CAPACITY, d1
	bcc.s qualifiedFail
	move.w state.NativeCliPreprocessImportBindingCount, d0
	cmpi.w #constants.NATIVE_PREPROCESS_IMPORT_BINDING_CAPACITY, d0
	bcc.s qualifiedFail
	mulu #constants.NATIVE_PREPROCESS_IMPORT_BINDING_NAME_CAPACITY, d0
	lea state.NativeCliPreprocessImportBindingNameTable, a1
	adda.l d0, a1
	move.l d2, d1
copyQualifier
	tst.l d1
	beq.s separator
	move.b (a2)+, (a1)+
	subq.l #1, d1
	bra.s copyQualifier
separator
	move.b #'.', (a1)+
	movea.l a3, a2
	move.l d3, d1
copyDefinition
	tst.l d1
	beq.s qualifiedStored
	move.b (a2)+, (a1)+
	subq.l #1, d1
	bra.s copyDefinition
qualifiedStored
	clr.b (a1)
	bsr.w finishBindingRow
	bra.s qualifiedReturn
qualifiedFail
	moveq #1, d0
qualifiedReturn
	movem.l (sp)+, d1-d3/a0-a2
	rts
	.bend  ; appendQualifiedBinding

; Append arbitrary A0/D1 name bytes.
appendBindingSlice	.block
	move.w state.NativeCliPreprocessImportBindingCount, d0
	cmpi.w #constants.NATIVE_PREPROCESS_IMPORT_BINDING_CAPACITY, d0
	bcc.s sliceFail
	cmpi.l #constants.NATIVE_PREPROCESS_IMPORT_BINDING_NAME_CAPACITY, d1
	bcc.s sliceFail
	move.l d0, d2
	mulu #constants.NATIVE_PREPROCESS_IMPORT_BINDING_NAME_CAPACITY, d2
	lea state.NativeCliPreprocessImportBindingNameTable, a1
	adda.l d2, a1
sliceCopy
	tst.l d1
	beq.s sliceStored
	move.b (a0)+, (a1)+
	subq.l #1, d1
	bra.s sliceCopy
sliceStored
	clr.b (a1)
	bsr.w finishBindingRow
	rts
sliceFail
	moveq #1, d0
	rts
	.bend  ; appendBindingSlice

finishBindingRow	.block
	moveq #0, d0
	move.w state.NativeCliPreprocessImportBindingCount, d0
	add.l d0, d0
	lea state.NativeCliPreprocessImportBindingOwnerTable, a1
	move.w d4, 0(a1, d0.l)
	lea state.NativeCliPreprocessImportBindingDefinitionTable, a1
	move.w d7, 0(a1, d0.l)
	addq.w #1, state.NativeCliPreprocessImportBindingCount
	moveq #0, d0
	rts
	.bend  ; finishBindingRow

compareTokenToDefinitionName	.block
	movem.l d1-d3/a0-a2, -(sp)
	moveq #0, d1
selectedLen
	tst.b 0(a0, d1.l)
	beq.s selectedReady
	addq.l #1, d1
	bra.s selectedLen
selectedReady
	exg d0, d1
	bsr.w compareFoldedExact
	movem.l (sp)+, d1-d3/a0-a2
	rts
	.bend  ; compareTokenToDefinitionName

compareTokenExact	.block
	movem.l d1-d3/a0-a2, -(sp)
	moveq #0, d0
tokenExactLen
	tst.b 0(a0, d0.l)
	beq.s tokenExactReady
	addq.l #1, d0
	bra.s tokenExactLen
tokenExactReady
	bsr.w compareFoldedExact
	movem.l (sp)+, d1-d3/a0-a2
	rts
	.bend  ; compareTokenExact

compareFoldedExact	.block
	move.l d4, -(sp)
	cmp.l d1, d0
	bne.s foldedNo
	moveq #0, d2
foldedLoop
	cmp.l d0, d2
	bcc.s foldedYes
	move.b 0(a0, d2.l), d3
	move.b 0(a1, d2.l), d4
	cmpi.b #'A', d3
	bcs.s foldRight
	cmpi.b #'Z', d3
	bhi.s foldRight
	addi.b #32, d3
foldRight
	cmpi.b #'A', d4
	bcs.s foldedCompare
	cmpi.b #'Z', d4
	bhi.s foldedCompare
	addi.b #32, d4
foldedCompare
	cmp.b d4, d3
	bne.s foldedNo
	addq.l #1, d2
	bra.s foldedLoop
foldedYes
	moveq #1, d0
	bra.s foldedReturn
foldedNo
	moveq #0, d0
foldedReturn
	move.l (sp)+, d4
	rts
	.bend  ; compareFoldedExact

	.endsection
	.endmodule

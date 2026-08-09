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
	move.w d7, state.NativeCliResolvedModuleId
	moveq #0, d0
	move.w state.NativeCliResolvedModuleId, d0
	moveq #0, d1
	bra.w return

searchPaths
	clr.w d7

loop
	move.w state.NativeCliModulePathCount, d0
	cmp.w d0, d7
	bhs.w fail
	moveq #0, d0
	move.w d7, d0
	lsl.l #8, d0
	lea state.NativeCliModulePathTable, a0
	adda.l d0, a0
	lea state.NativeCliIncludePath, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.w fail
	lea state.NativeCliArgToken, a0
	lea state.NativeCliIncludePath, a1
	jsr path.opforgeNativeCliAppendPathBuffer
	bne.w fail
	lea strings.ModuleSourceExtensionText, a0
	lea state.NativeCliIncludePath, a1
	jsr path.opforgeNativeCliAppendPathBuffer
	bne.w fail
	lea state.NativeCliIncludePath, a0
	jsr dos.openInput
	tst.l d0
	bne.s found
	addq.w #1, d7
	bra.w loop

found
	move.l d0, d1
	jsr dos.close
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

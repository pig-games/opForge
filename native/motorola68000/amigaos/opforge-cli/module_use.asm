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

opforgeNativeCliEmitImportRecord	.block
	movem.l d0-d4/a0-a1, -(sp)
	move.l #strings.UseImportText, d1
	jsr dos.putStr
	moveq #0, d0
	move.w d4, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea state.NativeCliImportOwnerModuleTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea state.NativeCliImportModuleTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea state.NativeCliImportFileIdTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	lsl.l #2, d0
	lea state.NativeCliImportLineTable, a0
	move.l 0(a0, d0.l), d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace

	bsr.w opforgeNativeCliImportAliasPtr
	bsr.w token_util.opforgeNativeCliTokenLen
	move.w d0, d3
	bsr.w text_output.opforgeNativeCliPutDecU16
	tst.w d3
	beq.s newline
	bsr.w text_output.opforgeNativeCliPutSpace
	bsr.w opforgeNativeCliImportAliasPtr
	move.l a0, d1
	jsr dos.putStr

newline
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	movem.l (sp)+, d0-d4/a0-a1
	rts
	.bend  ; opforgeNativeCliEmitImportRecord

opforgeNativeCliEmitImportSelectRecord	.block
	movem.l d0-d4/d6-d7/a0-a1, -(sp)
	move.l #strings.UseSelectText, d1
	jsr dos.putStr
	moveq #0, d0
	move.w d4, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace
	moveq #0, d0
	move.w d7, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace

	bsr.w opforgeNativeCliImportSelectNamePtr
	bsr.w token_util.opforgeNativeCliTokenLen
	move.w d0, d3
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace
	bsr.w opforgeNativeCliImportSelectNamePtr
	move.l a0, d1
	jsr dos.putStr
	bsr.w text_output.opforgeNativeCliPutSpace

	bsr.w opforgeNativeCliImportSelectAliasPtr
	bsr.w token_util.opforgeNativeCliTokenLen
	move.w d0, d3
	bsr.w text_output.opforgeNativeCliPutDecU16
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
	bsr.w text_output.opforgeNativeCliPutDecU16
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	movem.l (sp)+, d0-d4/d6-d7/a0-a1
	rts
	.bend  ; opforgeNativeCliEmitImportSelectRecord

opforgeNativeCliEmitImportWildcardRecord	.block
	movem.l d0-d4, -(sp)
	move.l #strings.UseWildcardText, d1
	jsr dos.putStr
	moveq #0, d0
	move.w d4, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace
	moveq #0, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	movem.l (sp)+, d0-d4
	rts
	.bend  ; opforgeNativeCliEmitImportWildcardRecord

opforgeNativeCliResolveBareUseModule	.block
	movem.l d2-d7/a0-a1, -(sp)
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
	move.w d0, d4
	cmp.w state.NativeCliRootModuleId, d4
	bne.s def
	move.l #strings.ModRootText, d1
	jsr dos.putStr
	moveq #0, d0
	move.w d4, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	move.l #strings.NewlineText, d1
	jsr dos.putStr

def
	move.l #strings.ModDefText, d1
	jsr dos.putStr
	moveq #0, d0
	move.w d4, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea state.NativeCliModuleFileIdTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	lsl.l #2, d0
	lea state.NativeCliModuleLineTable, a0
	move.l 0(a0, d0.l), d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea state.NativeCliModuleDepthTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace

	bsr.w opforgeNativeCliModuleNamePtr
	bsr.w token_util.opforgeNativeCliTokenLen
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace
	bsr.w opforgeNativeCliModuleNamePtr
	move.l a0, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	movem.l (sp)+, d0-d4/a0-a1
	rts
	.bend  ; opforgeNativeCliEmitModuleRecord

opforgeNativeCliEmitModuleCompatibility	.block
	movem.l d0/d4/a0, -(sp)
	move.w d0, d4
	move.l #strings.ModuleFoundText, d1
	jsr dos.putStr
	bsr.w opforgeNativeCliModuleNamePtr
	move.l a0, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	movem.l (sp)+, d0/d4/a0
	rts
	.bend  ; opforgeNativeCliEmitModuleCompatibility

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
	move.w d0, d4
	move.l #strings.ModEndText, d1
	jsr dos.putStr
	moveq #0, d0
	move.w d4, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace
	moveq #1, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace
	move.l state.NativeCliSourceLineNum, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	bsr.w text_output.opforgeNativeCliPutSpace
	moveq #0, d0
	move.w state.NativeCliModuleDepth, d0
	bsr.w text_output.opforgeNativeCliPutDecU16
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	movem.l (sp)+, d0-d4/a0-a1
	rts
	.bend  ; opforgeNativeCliEmitModuleEndRecord

opforgeNativeCliRestoreParentModule	.block
	movem.l d1-d3/a0, -(sp)
	tst.w state.NativeCliModuleDepth
	bne.s find
	clr.w state.NativeCliCurrentModuleId
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

	.endsection
	.endmodule

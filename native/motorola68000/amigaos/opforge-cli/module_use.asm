; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.module_use
	.cpu 68020
	.use opforge.cli.state (*)
	.use opforge.cli.constants (NATIVE_MODULE_TABLE_CAPACITY, NATIVE_IMPORT_TABLE_CAPACITY, NATIVE_IMPORT_SELECT_CAPACITY)
	.use opforge.cli.token_util (opforgeNativeCliCopyTokenBuffer, opforgeNativeCliTokenLen)
	.use opforge.cli.text_output (opforgeNativeCliPutDecU16, opforgeNativeCliPutSpace)
	.use opforge.cli.strings (ModRootText, ModDefText, ModEndText, ModuleFoundText, UseImportText, UseSelectText, UseWildcardText, NewlineText, ModuleSourceExtensionText)
	.use opforge.cli.dos (opforgeNativeCliPutStr, opforgeNativeCliOpenInput, opforgeNativeCliClose)
	.use opforge.cli.path (opforgeNativeCliCopyPathBuffer, opforgeNativeCliAppendPathBuffer)

	.section code, kind=code
	.pub

opforgeNativeCliRecordModule .block
	movem.l d1-d3/a0-a1, -(sp)
	moveq #0, d0
	move.w NativeCliModuleCount, d0
	cmpi.w #NATIVE_MODULE_TABLE_CAPACITY, d0
	bhs.w fail
	move.w d0, d3
	lea NativeCliArgToken, a0
	lea NativeCliModuleNameTable, a1
	moveq #0, d1
	move.w d3, d1
	lsl.l #6, d1
	adda.l d1, a1
	bsr.w opforgeNativeCliCopyTokenBuffer

	moveq #0, d1
	move.w d3, d1
	add.w d1, d1
	lea NativeCliModuleFileIdTable, a1
	move.w #1, 0(a1, d1.l)
	lea NativeCliModuleDepthTable, a1
	move.w NativeCliModuleDepth, 0(a1, d1.l)

	moveq #0, d1
	move.w d3, d1
	lsl.l #2, d1
	lea NativeCliModuleLineTable, a1
	move.l NativeCliSourceLineNum, 0(a1, d1.l)

	tst.w NativeCliModuleCount
	bne.s haveRoot
	move.w d3, NativeCliRootModuleId

haveRoot
	move.w d3, NativeCliCurrentModuleId
	move.w NativeCliModuleCount, d0
	addq.w #1, d0
	move.w d0, NativeCliModuleCount
	move.w NativeCliModuleDepth, d0
	addq.w #1, d0
	move.w d0, NativeCliModuleDepth
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d3/a0-a1
	rts
	.bend ; opforgeNativeCliRecordModule

opforgeNativeCliRecordImport .block
	movem.l d1-d3/a0-a1, -(sp)
	moveq #0, d0
	move.w NativeCliImportCount, d0
	cmpi.w #NATIVE_IMPORT_TABLE_CAPACITY, d0
	bhs.w fail
	move.w d0, d4
	moveq #0, d1
	move.w d4, d1
	add.w d1, d1
	lea NativeCliImportOwnerModuleTable, a1
	move.w NativeCliCurrentModuleId, 0(a1, d1.l)
	lea NativeCliImportModuleTable, a1
	clr.w 0(a1, d1.l)
	lea NativeCliImportFileIdTable, a1
	move.w #1, 0(a1, d1.l)

	moveq #0, d1
	move.w d4, d1
	lsl.l #2, d1
	lea NativeCliImportLineTable, a1
	move.l NativeCliSourceLineNum, 0(a1, d1.l)

	moveq #0, d1
	move.w d4, d1
	lsl.l #6, d1
	lea NativeCliImportAliasTable, a1
	adda.l d1, a1
	lea NativeCliIncludeTarget, a0
	bsr.w opforgeNativeCliCopyTokenBuffer

	move.w NativeCliImportCount, d0
	addq.w #1, d0
	move.w d0, NativeCliImportCount
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d3/a0-a1
	rts
	.bend ; opforgeNativeCliRecordImport

opforgeNativeCliRecordImportSelect .block
	movem.l d1-d3/a0-a1, -(sp)
	moveq #0, d0
	move.w NativeCliImportSelectCount, d0
	cmpi.w #NATIVE_IMPORT_SELECT_CAPACITY, d0
	bhs.w fail
	move.w d0, d6
	moveq #0, d1
	move.w d6, d1
	add.w d1, d1
	lea NativeCliImportSelectImportTable, a1
	move.w d4, 0(a1, d1.l)
	lea NativeCliImportSelectFlagsTable, a1
	move.w d3, 0(a1, d1.l)

	moveq #0, d1
	move.w d6, d1
	lsl.l #6, d1
	lea NativeCliImportSelectNameTable, a1
	adda.l d1, a1
	lea NativeCliArgToken, a0
	bsr.w opforgeNativeCliCopyTokenBuffer

	moveq #0, d1
	move.w d6, d1
	lsl.l #6, d1
	lea NativeCliImportSelectAliasTable, a1
	adda.l d1, a1
	lea NativeCliIncludeTarget, a0
	bsr.w opforgeNativeCliCopyTokenBuffer

	move.w NativeCliImportSelectCount, d0
	addq.w #1, d0
	move.w d0, NativeCliImportSelectCount
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d3/a0-a1
	rts
	.bend ; opforgeNativeCliRecordImportSelect

opforgeNativeCliEmitImportRecord .block
	movem.l d0-d4/a0-a1, -(sp)
	move.l #UseImportText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w d4, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea NativeCliImportOwnerModuleTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea NativeCliImportModuleTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea NativeCliImportFileIdTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	lsl.l #2, d0
	lea NativeCliImportLineTable, a0
	move.l 0(a0, d0.l), d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	bsr.w opforgeNativeCliImportAliasPtr
	bsr.w opforgeNativeCliTokenLen
	move.w d0, d3
	bsr.w opforgeNativeCliPutDecU16
	tst.w d3
	beq.s newline
	bsr.w opforgeNativeCliPutSpace
	bsr.w opforgeNativeCliImportAliasPtr
	move.l a0, d1
	jsr opforgeNativeCliPutStr

newline
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	movem.l (sp)+, d0-d4/a0-a1
	rts
	.bend ; opforgeNativeCliEmitImportRecord

opforgeNativeCliEmitImportSelectRecord .block
	movem.l d0-d4/d6-d7/a0-a1, -(sp)
	move.l #UseSelectText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w d4, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	moveq #0, d0
	move.w d7, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	bsr.w opforgeNativeCliImportSelectNamePtr
	bsr.w opforgeNativeCliTokenLen
	move.w d0, d3
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	bsr.w opforgeNativeCliImportSelectNamePtr
	move.l a0, d1
	jsr opforgeNativeCliPutStr
	bsr.w opforgeNativeCliPutSpace

	bsr.w opforgeNativeCliImportSelectAliasPtr
	bsr.w opforgeNativeCliTokenLen
	move.w d0, d3
	bsr.w opforgeNativeCliPutDecU16
	tst.w d3
	beq.s flags
	bsr.w opforgeNativeCliPutSpace
	bsr.w opforgeNativeCliImportSelectAliasPtr
	move.l a0, d1
	jsr opforgeNativeCliPutStr

flags
	bsr.w opforgeNativeCliPutSpace
	moveq #0, d0
	move.w d6, d0
	add.w d0, d0
	lea NativeCliImportSelectFlagsTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	movem.l (sp)+, d0-d4/d6-d7/a0-a1
	rts
	.bend ; opforgeNativeCliEmitImportSelectRecord

opforgeNativeCliEmitImportWildcardRecord .block
	movem.l d0-d4, -(sp)
	move.l #UseWildcardText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w d4, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	moveq #0, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	movem.l (sp)+, d0-d4
	rts
	.bend ; opforgeNativeCliEmitImportWildcardRecord

opforgeNativeCliResolveBareUseModule .block
	movem.l d2-d7/a0-a1, -(sp)
	clr.w d7

loop
	move.w NativeCliModulePathCount, d0
	cmp.w d0, d7
	bhs.w fail
	moveq #0, d0
	move.w d7, d0
	lsl.l #8, d0
	lea NativeCliModulePathTable, a0
	adda.l d0, a0
	lea NativeCliIncludePath, a1
	bsr.w opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.w fail
	lea NativeCliArgToken, a0
	lea NativeCliIncludePath, a1
	bsr.w opforgeNativeCliAppendPathBuffer
	tst.l d0
	bne.w fail
	lea ModuleSourceExtensionText, a0
	lea NativeCliIncludePath, a1
	bsr.w opforgeNativeCliAppendPathBuffer
	tst.l d0
	bne.w fail
	lea NativeCliIncludePath, a0
	jsr opforgeNativeCliOpenInput
	tst.l d0
	bne.s found
	addq.w #1, d7
	bra.w loop

found
	move.l d0, d1
	jsr opforgeNativeCliClose
	move.w NativeCliModuleCount, d6
	move.w d6, NativeCliResolvedModuleId
	moveq #0, d0
	move.w NativeCliResolvedModuleId, d0
	moveq #0, d1
	bra.s return

fail
	moveq #1, d1

return
	movem.l (sp)+, d2-d7/a0-a1
	rts
	.bend ; opforgeNativeCliResolveBareUseModule

opforgeNativeCliEmitModuleRecord .block
	movem.l d0-d4/a0-a1, -(sp)
	move.w d0, d4
	cmp.w NativeCliRootModuleId, d4
	bne.s def
	move.l #ModRootText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w d4, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr

def
	move.l #ModDefText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w d4, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea NativeCliModuleFileIdTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	lsl.l #2, d0
	lea NativeCliModuleLineTable, a0
	move.l 0(a0, d0.l), d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea NativeCliModuleDepthTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	bsr.w opforgeNativeCliModuleNamePtr
	bsr.w opforgeNativeCliTokenLen
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	bsr.w opforgeNativeCliModuleNamePtr
	move.l a0, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	movem.l (sp)+, d0-d4/a0-a1
	rts
	.bend ; opforgeNativeCliEmitModuleRecord

opforgeNativeCliEmitModuleCompatibility .block
	movem.l d0/d4/a0, -(sp)
	move.w d0, d4
	move.l #ModuleFoundText, d1
	jsr opforgeNativeCliPutStr
	bsr.w opforgeNativeCliModuleNamePtr
	move.l a0, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	movem.l (sp)+, d0/d4/a0
	rts
	.bend ; opforgeNativeCliEmitModuleCompatibility

opforgeNativeCliCloseModule .block
	movem.l d1-d4/a0-a1, -(sp)
	tst.w NativeCliModuleDepth
	beq.s fail
	moveq #0, d0
	move.w NativeCliModuleDepth, d0
	subq.w #1, d0
	move.w d0, NativeCliModuleDepth
	moveq #0, d0
	move.w NativeCliCurrentModuleId, d0
	bsr.w opforgeNativeCliEmitModuleEndRecord
	bsr.w opforgeNativeCliRestoreParentModule
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d4/a0-a1
	rts
	.bend ; opforgeNativeCliCloseModule

opforgeNativeCliEmitModuleEndRecord .block
	movem.l d0-d4/a0-a1, -(sp)
	move.w d0, d4
	move.l #ModEndText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w d4, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	moveq #1, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliSourceLineNum, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	moveq #0, d0
	move.w NativeCliModuleDepth, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	movem.l (sp)+, d0-d4/a0-a1
	rts
	.bend ; opforgeNativeCliEmitModuleEndRecord

opforgeNativeCliRestoreParentModule .block
	movem.l d1-d3/a0, -(sp)
	tst.w NativeCliModuleDepth
	bne.s find
	clr.w NativeCliCurrentModuleId
	bra.s return

find
	move.w NativeCliModuleDepth, d0
	subq.w #1, d0
	move.w NativeCliModuleCount, d1
	beq.s clear
	subq.w #1, d1

loop
	moveq #0, d2
	move.w d1, d2
	add.w d2, d2
	lea NativeCliModuleDepthTable, a0
	move.w 0(a0, d2.l), d3
	cmp.w d0, d3
	beq.s found
	dbra d1, loop

clear
	clr.w NativeCliCurrentModuleId
	bra.s return

found
	move.w d1, NativeCliCurrentModuleId

return
	movem.l (sp)+, d1-d3/a0
	rts
	.bend ; opforgeNativeCliRestoreParentModule

opforgeNativeCliModuleNamePtr .block	
	moveq #0, d0
	move.w d4, d0
	lsl.l #6, d0
	lea NativeCliModuleNameTable, a0
	adda.l d0, a0
	rts
	.bend ; opforgeNativeCliModuleNamePtr

opforgeNativeCliImportAliasPtr .block
	moveq #0, d0
	move.w d4, d0
	lsl.l #6, d0
	lea NativeCliImportAliasTable, a0
	adda.l d0, a0
	rts
	.bend ; opforgeNativeCliImportAliasPtr

opforgeNativeCliImportSelectNamePtr .block
	moveq #0, d0
	move.w d6, d0
	lsl.l #6, d0
	lea NativeCliImportSelectNameTable, a0
	adda.l d0, a0
	rts
	.bend ; opforgeNativeCliImportSelectNamePtr

opforgeNativeCliImportSelectAliasPtr .block
	moveq #0, d0
	move.w d6, d0
	lsl.l #6, d0
	lea NativeCliImportSelectAliasTable, a0
	adda.l d0, a0
	rts
	.bend ; opforgeNativeCliImportSelectAliasPtr

	.endsection
	.endmodule

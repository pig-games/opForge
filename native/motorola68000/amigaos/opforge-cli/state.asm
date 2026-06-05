; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.state
	.cpu 68020

	.use opforge.cli.constants

	.pub

	.section bss, kind=bss
	.align 4

NativeCliDosBase
	.res long, 1
NativeCliReturnCode
	.res long, 1
NativeCliInputStyle
	.res word, 1
NativeCliHunkRequested
	.res word, 1
NativeCliBinRequested
	.res word, 1
NativeCliOutputFormat
	.res word, 1
NativeCliParseStatus
	.res word, 1

NativeCliArgToken
	.res byte, constants.TOKEN_BUFFER_CAPACITY
NativeCliInputPath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliHunkPath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliBinPath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliOutfileBase
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliCpuName
	.res byte, constants.TOKEN_BUFFER_CAPACITY
NativeCliPackagePath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliSourceLineLen
	.res word, 1
NativeCliParserTailLen
	.res word, 1
NativeCliPackageLenActive
	.res word, 1
NativeCliPipelineRequestLen
	.res word, 1
NativeCliLineRequestLen
	.res word, 1
NativeCliEvalRequestLen
	.res word, 1
NativeCliEncodeRequestLen
	.res word, 1
	.align 4
NativeCliSourceLineNum
	.res long, 1
NativeCliSawCr
	.res word, 1
NativeCliIncludeDepth
	.res word, 1
NativeCliIncludePending
	.res word, 1
NativeCliModuleResolveDepth
	.res word, 1
NativeCliResolvedModuleId
	.res word, 1
NativeCliSavedLineLen
	.res word, 1
NativeCliSavedSawCr
	.res word, 1
NativeCliSavedLineNum
	.res long, 1
NativeCliModuleSavedLineLen
	.res word, 1
NativeCliModuleSavedSawCr
	.res word, 1
NativeCliModuleSavedLineNum
	.res long, 1
NativeCliStmtMnemFound
	.res word, 1
NativeCliStmtExprFound
	.res word, 1
NativeCliStmtDirectiveKind
	.res word, 1
NativeCliInputChar
	.res byte, 1
NativeCliDecimalChar
	.res byte, 2
NativeCliHexBuffer
	.res byte, 10
NativeCliStmtLabelStart
	.res long, 1
NativeCliStmtLabelEnd
	.res long, 1
NativeCliStmtLabelOff
	.res long, 1
NativeCliStmtLabelLen
	.res long, 1
NativeCliStmtMnemStart
	.res long, 1
NativeCliStmtMnemEnd
	.res long, 1
NativeCliStmtMnemOff
	.res long, 1
NativeCliStmtMnemLen
	.res long, 1
NativeCliStmtOperandStart
	.res long, 1
NativeCliStmtOperandEnd
	.res long, 1
NativeCliStmtExprOperandIndex
	.res long, 1
NativeCliStmtExprSlotIndex
	.res long, 1
NativeCliStmtExprStartToken
	.res long, 1
NativeCliStmtExprEndToken
	.res long, 1
NativeCliStmtExprSpanLine
	.res long, 1
NativeCliStmtExprSpanStart
	.res long, 1
NativeCliStmtExprSpanEnd
	.res long, 1
NativeCliSourceLine
	.res byte, constants.SOURCE_LINE_BUFFER_CAPACITY
NativeCliParserTailBuffer
	.res byte, constants.SOURCE_LINE_BUFFER_CAPACITY
NativeCliCurrentPath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliSavedPath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliModuleSavedPath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliIncludeTarget
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliIncludePath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliIncludeRootPath
	.res byte, constants.PATH_BUFFER_CAPACITY
OpforgeNativeCliPrvmRouteFrame
	.res byte, constants.PRVM_ROUTE_FRAME_SIZE
NativeCliPrvmRouteStatus
	.res long, 1
NativeCliPrvmResultCount
	.res word, 1
NativeCliPrvmPartialResultCount
	.res word, 1
OpforgeNativeCliPrvmResultBuffer
	.res byte, constants.PRVM_ROUTE_RESULT_CAPACITY
OpforgeNativeCliPrvmPartialResultBuffer
	.res byte, constants.PRVM_ROUTE_RESULT_CAPACITY
OpforgeNativeCliPrvmDiagBuffer
	.res byte, constants.PRVM_ROUTE_DIAG_CAPACITY
OpforgeNativeCliPrvmResumeBuffer
	.res byte, constants.PRVM_ROUTE_RESUME_CAPACITY
OpforgeNativeCliPrvmExprRequest
	.res byte, constants.PRVM_ROUTE_EXPR_REQUEST_SIZE
OpforgeNativeCliPrvmExprResultSlot
	.res byte, constants.PRVM_ROUTE_EXPR_RESULT_SIZE * constants.PRVM_ROUTE_EXPR_RESULT_CAPACITY

NativeCliModuleUseStateStart
NativeCliModuleCount
	.res word, 1
NativeCliImportCount
	.res word, 1
NativeCliModulePathCount
	.res word, 1
NativeCliImportSelectCount
	.res word, 1
NativeCliRootModuleId
	.res word, 1
NativeCliCurrentModuleId
	.res word, 1
NativeCliModuleDepth
	.res word, 1
NativeCliModuleNameTable
	.res byte, constants.NATIVE_MODULE_TABLE_CAPACITY * constants.TOKEN_BUFFER_CAPACITY
NativeCliModuleFileIdTable
	.res word, constants.NATIVE_MODULE_TABLE_CAPACITY
NativeCliModuleLineTable
	.res long, constants.NATIVE_MODULE_TABLE_CAPACITY
NativeCliModuleDepthTable
	.res word, constants.NATIVE_MODULE_TABLE_CAPACITY
NativeCliImportOwnerModuleTable
	.res word, constants.NATIVE_IMPORT_TABLE_CAPACITY
NativeCliImportModuleTable
	.res word, constants.NATIVE_IMPORT_TABLE_CAPACITY
NativeCliImportFileIdTable
	.res word, constants.NATIVE_IMPORT_TABLE_CAPACITY
NativeCliImportLineTable
	.res long, constants.NATIVE_IMPORT_TABLE_CAPACITY
NativeCliImportAliasTable
	.res byte, constants.NATIVE_IMPORT_TABLE_CAPACITY * constants.TOKEN_BUFFER_CAPACITY
NativeCliImportSelectImportTable
	.res word, constants.NATIVE_IMPORT_SELECT_CAPACITY
NativeCliImportSelectNameTable
	.res byte, constants.NATIVE_IMPORT_SELECT_CAPACITY * constants.TOKEN_BUFFER_CAPACITY
NativeCliImportSelectAliasTable
	.res byte, constants.NATIVE_IMPORT_SELECT_CAPACITY * constants.TOKEN_BUFFER_CAPACITY
NativeCliImportSelectFlagsTable
	.res word, constants.NATIVE_IMPORT_SELECT_CAPACITY
NativeCliModulePathTable
	.res byte, constants.NATIVE_MODULE_PATH_CAPACITY * constants.PATH_BUFFER_CAPACITY
nativeCliModuleUseStateEnd

	.endsection

	.endmodule

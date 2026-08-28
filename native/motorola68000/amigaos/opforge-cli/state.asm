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
NativeCliPrgRequested
	.res word, 1
NativeCliHexRequested
	.res word, 1
NativeCliLstRequested
	.res word, 1
NativeCliOutputFormat
	.res word, 1
NativeCliPrgLoadAddrSet
	.res word, 1
NativeCliParseStatus
	.res word, 1
NativeCliDebugEnabled
	.res word, 1
NativeCliPackagePipelineReady
	.res word, 1

NativeCliArgToken
	.res byte, constants.TOKEN_BUFFER_CAPACITY
NativeCliInputPath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliHunkPath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliBinPath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliPrgPath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliHexPath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliLstPath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliOutfileBase
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliCpuName
	.res byte, constants.TOKEN_BUFFER_CAPACITY
NativeCliInitialCpuName
	.res byte, constants.TOKEN_BUFFER_CAPACITY
NativeCliPackagePath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliSourceLineLen
	.res word, 1
NativeCliParserTailLen
	.res word, 1
NativeCliPackageLenActive
	.res long, 1
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
NativeCliPrgLoadAddr
	.res long, 1
NativeCliOutputBootstrapFromSource
	.res word, 1
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
NativeCliResolvedModuleStartOffset
	.res long, 1
NativeCliResolvedModuleEndOffset
	.res long, 1
NativeCliSavedLineLen
	.res word, 1
NativeCliSavedSawCr
	.res word, 1
NativeCliSavedLineNum
	.res long, 1
NativeCliModuleSavedLineLen
	.res word, constants.NATIVE_MODULE_RESOLVE_DEPTH_LIMIT
NativeCliModuleSavedSawCr
	.res word, constants.NATIVE_MODULE_RESOLVE_DEPTH_LIMIT
NativeCliModuleSavedLineNum
	.res long, constants.NATIVE_MODULE_RESOLVE_DEPTH_LIMIT
NativeCliModuleReadRemaining
	.res long, constants.NATIVE_MODULE_RESOLVE_DEPTH_LIMIT
NativeCliStmtMnemFound
	.res word, 1
NativeCliStmtExprFound
	.res word, 1
NativeCliStmtDirectiveKind
	.res word, 1
NativeCliStmtDirectiveKindAuthoritative
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
NativeCliOutputPathScratch
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliCurrentPath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliSavedPath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliModuleSavedPath
	.res byte, constants.NATIVE_MODULE_RESOLVE_DEPTH_LIMIT * constants.PATH_BUFFER_CAPACITY
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
NativeCliPrvmRouteDetail
	.res long, 1
NativeCliPrvmPipelineDetail
	.res long, 1
NativeCliPrvmTokenizerDetail
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

NativeCliPreprocessStateStart
NativeCliPreprocessExpansionDepth
	.res word, 1
NativeCliPreprocessSavedLineLen
	.res word, 1
NativeCliPreprocessDefinitionCount
	.res word, 1
NativeCliPreprocessActiveDefinition
	.res word, 1
NativeCliPreprocessCurrentVisibility
	.res word, 1
NativeCliPreprocessCliDefineCount
	.res word, 1
NativeCliPreprocessConditionalDepth
	.res word, 1
NativeCliPreprocessConditionalActive
	.res byte, constants.NATIVE_PREPROCESS_CONDITIONAL_DEPTH_CAPACITY
NativeCliPreprocessConditionalAnyTrue
	.res byte, constants.NATIVE_PREPROCESS_CONDITIONAL_DEPTH_CAPACITY
NativeCliPreprocessConditionalInElse
	.res byte, constants.NATIVE_PREPROCESS_CONDITIONAL_DEPTH_CAPACITY
NativeCliPreprocessCliDefines
	.res byte, constants.NATIVE_PREPROCESS_CLI_DEFINE_CAPACITY * constants.TOKEN_BUFFER_CAPACITY
NativeCliPreprocessConditionalName
	.res byte, constants.TOKEN_BUFFER_CAPACITY
; Shared structural-definition record contract (macro, segment, and statement):
; DefinitionHeader is the captured name/signature; DefinitionBody and
; DefinitionBodyCount describe the bounded body span; HeaderLen/BodyLen own
; exact byte lengths. DefinitionKind selects macro scope wrapping or inline
; segment/statement expansion. preprocessor_definitions owns capture and kind
; assignment; the kind-specific matchers own invocation capture state; and
; line_processor owns expansion/scope policy.
; Resource budget/lifetime: ResetPreprocessorV1 clears this whole contiguous
; region before each CLI session. Every owner rejects an over-capacity request
; before writing its slot: definition headers (including name/signature), body
; lines, positional arguments, full argument lists, labels, saved caller lines,
; and expansion lines are separate fixed buffers despite equal byte capacities.
NativeCliPreprocessInvocationDefinition
	.res word, 1
NativeCliPreprocessInvocationArgCount
	.res word, 1
NativeCliPreprocessInvocationBodyIndex
	.res word, 1
NativeCliPreprocessInvocationArgLen
	.res word, constants.NATIVE_PREPROCESS_MACRO_ARG_CAPACITY
NativeCliPreprocessInvocationFullArgsLen
	.res word, 1
NativeCliPreprocessInvocationLabelLen
	.res word, 1
NativeCliPreprocessDefinitionKind
	.res byte, constants.NATIVE_PREPROCESS_DEFINITION_CAPACITY
NativeCliPreprocessDefinitionOwner
	.res word, constants.NATIVE_PREPROCESS_DEFINITION_CAPACITY
NativeCliPreprocessDefinitionVisibility
	.res byte, constants.NATIVE_PREPROCESS_DEFINITION_CAPACITY
NativeCliPreprocessDefinitionBodyCount
	.res word, constants.NATIVE_PREPROCESS_DEFINITION_CAPACITY
NativeCliPreprocessDefinitionHeaderLen
	.res word, constants.NATIVE_PREPROCESS_DEFINITION_CAPACITY
NativeCliPreprocessDefinitionBodyLen
	.res word, constants.NATIVE_PREPROCESS_DEFINITION_CAPACITY * constants.NATIVE_PREPROCESS_BODY_LINE_CAPACITY
NativeCliPreprocessDefinitionHeader
	.res byte, constants.NATIVE_PREPROCESS_DEFINITION_CAPACITY * constants.NATIVE_PREPROCESS_DEFINITION_HEADER_CAPACITY
NativeCliPreprocessDefinitionBody
	.res byte, constants.NATIVE_PREPROCESS_DEFINITION_CAPACITY * constants.NATIVE_PREPROCESS_BODY_LINE_CAPACITY * constants.NATIVE_PREPROCESS_BODY_LINE_TEXT_CAPACITY
NativeCliPreprocessInvocationArgs
	.res byte, constants.NATIVE_PREPROCESS_MACRO_ARG_CAPACITY * constants.NATIVE_PREPROCESS_INVOCATION_ARG_TEXT_CAPACITY
NativeCliPreprocessInvocationFullArgs
	.res byte, constants.NATIVE_PREPROCESS_INVOCATION_FULL_ARGS_CAPACITY
NativeCliPreprocessInvocationLabel
	.res byte, constants.NATIVE_PREPROCESS_INVOCATION_LABEL_CAPACITY
NativeCliPreprocessSavedLine
	.res byte, constants.NATIVE_PREPROCESS_SAVED_LINE_CAPACITY
NativeCliPreprocessExpansionLine
	.res byte, constants.NATIVE_PREPROCESS_EXPANSION_LINE_CAPACITY
NativeCliPreprocessExpansionLineLen
	.res word, 1
NATIVE_CLI_PREPROCESS_STATE_END
NATIVE_CLI_PREPROCESS_STATE_BYTES = NATIVE_CLI_PREPROCESS_STATE_END - NativeCliPreprocessStateStart

NativeCliModuleUseStateStart
NativeCliModuleCount
	.res word, 1
NativeCliImportCount
	.res word, 1
NativeCliModulePathCount
	.res word, 1
NativeCliIncludePathCount
	.res word, 1
NativeCliImportSelectCount
	.res word, 1
NativeCliImportSectionMapCount
	.res word, 1
NativeCliRootModuleId
	.res word, 1
NativeCliCurrentModuleId
	.res word, 1
NativeCliModuleDepth
	.res word, 1
NativeCliActiveModuleStack
	.res word, constants.NATIVE_MODULE_TABLE_CAPACITY
NativeCliPreprocessImportBindingCount
	.res word, 1
NativeCliModuleNameTable
	.res byte, constants.NATIVE_MODULE_TABLE_CAPACITY * constants.TOKEN_BUFFER_CAPACITY
NativeCliModuleFileIdTable
	.res word, constants.NATIVE_MODULE_TABLE_CAPACITY
NativeCliModuleLineTable
	.res long, constants.NATIVE_MODULE_TABLE_CAPACITY
NativeCliModuleDepthTable
	.res word, constants.NATIVE_MODULE_TABLE_CAPACITY
NativeCliModuleVisibilityTable
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
NativeCliImportSectionMapImportTable
	.res word, constants.NATIVE_IMPORT_SECTION_MAP_CAPACITY
NativeCliImportSectionMapLogicalTable
	.res byte, constants.NATIVE_IMPORT_SECTION_MAP_CAPACITY * constants.TOKEN_BUFFER_CAPACITY
NativeCliImportSectionMapConcreteTable
	.res byte, constants.NATIVE_IMPORT_SECTION_MAP_CAPACITY * constants.TOKEN_BUFFER_CAPACITY
NativeCliPreprocessImportBindingOwnerTable
	.res word, constants.NATIVE_PREPROCESS_IMPORT_BINDING_CAPACITY
NativeCliPreprocessImportBindingDefinitionTable
	.res word, constants.NATIVE_PREPROCESS_IMPORT_BINDING_CAPACITY
NativeCliPreprocessImportBindingNameTable
	.res byte, constants.NATIVE_PREPROCESS_IMPORT_BINDING_CAPACITY * constants.NATIVE_PREPROCESS_IMPORT_BINDING_NAME_CAPACITY
NativeCliOrdinaryExportCount
	.res word, 1
NativeCliOrdinaryExportOwnerTable
	.res word, constants.NATIVE_ORDINARY_EXPORT_CAPACITY
NativeCliOrdinaryExportNameTable
	.res byte, constants.NATIVE_ORDINARY_EXPORT_CAPACITY * constants.TOKEN_BUFFER_CAPACITY
NativeCliResolvedImportName
	.res byte, constants.TOKEN_BUFFER_CAPACITY
NativeCliModulePathTable
	.res byte, constants.NATIVE_MODULE_PATH_CAPACITY * constants.PATH_BUFFER_CAPACITY
NativeCliIncludePathTable
	.res byte, constants.NATIVE_INCLUDE_PATH_CAPACITY * constants.PATH_BUFFER_CAPACITY
NATIVE_CLI_MODULE_USE_STATE_END
NATIVE_CLI_MODULE_USE_STATE_BYTES = NATIVE_CLI_MODULE_USE_STATE_END - NativeCliModuleUseStateStart

NativeCliArtifactRequestStateStart
NativeCliArtifactRequestCount
	.res word, 1
NativeCliMetadataDepth
	.res word, 1
NativeCliMetadataOutputDepth
	.res word, 1
NativeCliMetadataTargetDepth
	.res word, 1
NativeCliMetadataTargetNameLen
	.res word, 1
NativeCliMetadataTargetName
	.res byte, constants.TOKEN_BUFFER_CAPACITY
NativeCliSourceOutputSectionCount
	.res word, 1
NativeCliSourceOutputSectionListActive
	.res word, 1
NativeCliSourceOutputImageSet
	.res word, 1
NativeCliSourceOutputFillSet
	.res word, 1
NativeCliSourceOutputContiguous
	.res word, 1
NativeCliSourceOutputFill
	.res byte, 1
	.align 4
NativeCliSourceOutputImageStart
	.res long, 1
NativeCliSourceOutputImageEnd
	.res long, 1
NativeCliSourceOutputSectionNames
	.res byte, constants.NATIVE_SOURCE_OUTPUT_SECTION_CAPACITY * constants.NATIVE_SOURCE_OUTPUT_SECTION_NAME_CAPACITY
NativeCliMapPath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliExportDirPath
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliArtifactPathScratch
	.res byte, constants.PATH_BUFFER_CAPACITY
NativeCliArtifactRequestKinds
	.res byte, constants.NATIVE_ARTIFACT_REQUEST_CAPACITY
	.align 2
NativeCliArtifactRequestLengths
	.res word, constants.NATIVE_ARTIFACT_REQUEST_CAPACITY
NativeCliArtifactRequestTexts
	.res byte, constants.NATIVE_ARTIFACT_REQUEST_CAPACITY * constants.SOURCE_LINE_BUFFER_CAPACITY
NATIVE_CLI_ARTIFACT_REQUEST_STATE_END
NATIVE_CLI_ARTIFACT_REQUEST_STATE_BYTES = NATIVE_CLI_ARTIFACT_REQUEST_STATE_END - NativeCliArtifactRequestStateStart

	.endsection

	.endmodule

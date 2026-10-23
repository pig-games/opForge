# Native runtime boundary inventory

This is a reviewed static inventory of 25 selected native ownership boundaries,
including their literal helper includes (82 unique source files). It is not an
inventory of every native module or proof of runtime parity.
The complementary [import contract](native-runtime-boundary-contract-v0_1.md)
documents the retained typed-value and package-validation dependencies beyond
the selected VM surfaces here; those semantic edges are not wire-only imports.

### Scope and reviewed responsibilities

| Owner | Responsibility and boundary |
| --- | --- |
| CLI entry | Resolve arguments, input and target; construct the driver request and present results. No grammar or encoding. |
| Assembly driver and helpers | Acquire and order sources, coordinate private module compilation, publish stored interfaces, manage capture/prepared storage lifetimes and route finalized outputs. The shared persistent `.ofm` image is adopted for incremental reuse and final linking; its validity metadata is invalidated before replacement. Publication retires the pending image borrow. |
| Preparation/frontend | Bind numeric source records, construct bounded VM requests and coordinate shared declarations/templates. Included request builders borrow session storage; grammar and expansion remain with shared language/VM owners. |
| Layout, dependencies and sections | Select reachable units, resolve availability, size/place sections and emit bytes/fixups. Shared directive dispatch precedes instruction encoding; unresolved directives do not fall through. Instruction candidates, predicates and encodings remain package-defined. |
| Module provider | Resolve demanded exports, remap identities and hydrate templates from stored interfaces. No CPU parsing. |
| Output plan and Hunk image | Validate finalized geometry and serialize caller-owned bytes/relocations. No source acquisition or instruction selection. |
| Package loader/validation | Acquire external or borrowed embedded packages and validate identity, bounds, regions and records. Preparation policy validation delegates to its shared VM owner. |
| VM executors | Execute numeric package contracts over bounded requests and caller storage. TKVM normalization/composition and encoding interval helpers belong here. EXVM's source-record import supplies wire constants, not assembler orchestration. ExprVM's configured string-provider callback is an explicit service ABI; its existing session state is not reentrant. |

The reviewed sources preserve package ownership of CPU/family semantics and
shared handling of generic directives. Existing native VM version checks and
multi-version executors remain a known contract migration gap; this inventory
does not endorse them as the intended endpoint. See the
[current parity gaps](../plans/native-runtime-reset.md#remaining-parity-and-qualification-gaps).

### How to read and maintain the audit

`scripts/workflow/check_native_runtime_boundary_inventory.py` pins full source
text and repository-relative paths for each audited module's literal include
closure. Includes resolve relative to the including file, including nested
same-directory helpers. Shared includes are covered once per module. Imported
modules are independent owners and are not recursively included in that closure.

The lists below summarize possible static definitions across **both conditional
arms**. Debug imports are possible dependencies, not assertions about a release
executable. Macro bodies are hashed but not expanded or counted as module
routines, imports or sections. Repeated section declarations are retained;
public/private visibility is not inferred. The report's file-qualified diagnostic
mentions are lexical review hints, including comments, rather than reachable
error-path proof. Dynamic or template-local includes, missing files and include
cycles fail closed because this audit cannot safely summarize them.

The guard checks these documented facts as well as source hashes, representative
routine definitions and required imports. A helper-only change therefore needs
review too. Review responsibilities, callbacks, state and imports before changing
snapshots; do not regenerate them merely to silence drift. `--report` prints the
validated inventory and diagnostic hints. This check supplies structural drift
evidence, not semantic enforcement, timing, memory or fresh native proof.

## `opforge.assembler.driver`

Source: `native/motorola68000/amigaos/assembler/driver.asm`

Entry: `execute`. Static routines: 185. Possible imports: 74. Section declarations: `code`, `data`, `bss`, `bss`, `bss`, `code`, `bss`, `code`, `bss`, `code`, `bss`, `data`, `code`, `data`, `code`.

Literal include closure (43 files): `native/motorola68000/amigaos/debug/memory_telemetry.i`, `native/motorola68000/amigaos/debug/memory_telemetry_noop.i`, `native/motorola68000/amigaos/debug/binding_telemetry.i`, `native/motorola68000/amigaos/debug/binding_telemetry_noop.i`, `native/motorola68000/amigaos/debug/capture_telemetry.i`, `native/motorola68000/amigaos/debug/capture_telemetry_noop.i`, `native/motorola68000/amigaos/debug/capture_lifetime.i`, `native/motorola68000/amigaos/debug/lazy_audit.i`, `native/motorola68000/amigaos/debug/lazy_work.i`, `native/motorola68000/amigaos/debug/lazy_work_context.i`, `native/motorola68000/amigaos/debug/hotspot_telemetry.i`, `native/motorola68000/amigaos/debug/hotspot_telemetry_noop.i`, `native/motorola68000/amigaos/assembler/driver_progress.i`, `native/motorola68000/amigaos/assembler/driver_diagnostics.i`, `native/motorola68000/amigaos/assembler/driver_lazy_report.i`, `native/motorola68000/amigaos/assembler/driver_flat_output.i`, `native/motorola68000/amigaos/assembler/driver_metadata_output.i`, `native/motorola68000/amigaos/assembler/driver_hunk_output.i`, `native/motorola68000/amigaos/assembler/driver_prepared_store.i`, `native/motorola68000/amigaos/assembler/driver_lexical_store.i`, `native/motorola68000/amigaos/assembler/source/static_include.i`, `native/motorola68000/amigaos/assembler/source/schedule.i`, `native/motorola68000/amigaos/assembler/modules/private.i`, `native/motorola68000/amigaos/assembler/modules/requests.i`, `native/motorola68000/amigaos/assembler/modules/private_seed.i`, `native/motorola68000/amigaos/assembler/modules/private_pub.i`, `native/motorola68000/amigaos/assembler/modules/cache.i`, `native/motorola68000/amigaos/assembler/modules/cache_key.i`, `native/motorola68000/amigaos/assembler/modules/cache_pub.i`, `native/motorola68000/amigaos/assembler/modules/cache_sources.i`, `native/motorola68000/amigaos/assembler/modules/cache_store.i`, `native/motorola68000/amigaos/assembler/modules/cache_tool.i`, `native/motorola68000/amigaos/assembler/modules/cache_save.i`, `native/motorola68000/amigaos/assembler/modules/iface_driver.i`, `native/motorola68000/amigaos/assembler/modules/iface_addr.i`, `native/motorola68000/amigaos/assembler/modules/iface_links.i`, `native/motorola68000/amigaos/assembler/modules/pipeline.i`, `native/motorola68000/amigaos/assembler/source/discovery_idx.i`, `native/motorola68000/amigaos/assembler/source/candidate_order.i`, `native/motorola68000/amigaos/assembler/source/selection.i`, `native/motorola68000/amigaos/assembler/source/includes.i`, `native/motorola68000/amigaos/assembler/source/assets.i`, `native/motorola68000/amigaos/assembler/source/cursor.i`.

Routines: `execute`, `readExact`, `prepare`, `prepareCliInputs`, `releaseGraph`, `openSource`, `readManifestPath`, `appendCandidate`, `closeSource`, `fileSpan`, `closeInput`, `readByte`, `lowerLine`, `appendPrepared`, `run`, `appendEmission`, `allocateOutput`, `appendReloc`, `writeOutputs`, `writeSelected`, `writeRecords`, `copy`, `beginProgress`, `finishModuleProgress`, `finishProgress`, `replayReportedSpan`, `reportFailure`, `sourceOriginPath`, `locateFailure`, `hexField`, `reportByteWarning`, `writeDiagnostic`, `workOriginPaths`, `workWritePath`, `workTemplatePaths`, `writeFlat`, `writeMetadataBins`, `writeHunk`, `selectOutput`, `collectRelocs`, `completePreparedStore`, `visitPreparedPages`, `indexPreparedStore`, `materializePreparedStore`, `configureLexicalStorage`, `sealLexicalStorage`, `staticCaptureBegin`, `staticCaptureLine`, `bindStaticRecord`, `captureFileBegin`, `newCaptureRegion`, `currentCaptureRegion`, `capturePhysicalLine`, `captureFileEnd`, `locateCapture`, `captureSourcePath`, `configureCapturedModule`, `locateConfigurationFailure`, `orderCapturedSources`, `beginCapturedReplay`, `replayCapturedSources`, `replayCapturedSpan`, `prepareCapturedSources`, `releaseCapturedSources`, `releaseScheduledFrontend`, `tryPrivateModule`, `privateCheckText`, `privateCheckStoredText`, `privateFindCapture`, `privateInitialize`, `privateFindGlobalModule`, `privateFold`, `privateReplay`, `privateRejectFile`, `privateAppendBlock`, `privateSwap`, `privateEligibleImage`, `buildPrivateRequests`, `appendRequestName`, `privateSelectRequest`, `releasePrivateRequests`, `privateSeed`, `privateRetainKnown`, `privateGlobalSeed`, `privateExternal`, `privateInventory`, `privateResetRecord`, `privatePublish`, `privateMapSections`, `privateBuildLists`, `privateAppendId`, `privateClear`, `privateReleaseAttempt`, `privateAppendCarrier`, `incrementalBegin`, `incrementalTry`, `incrementalCheckDependencies`, `incrementalPaths`, `incrementalModulePaths`, `incrementalJoin`, `incrementalDigest`, `incrementalDigestBytes`, `incrementalBuildKey`, `incrementalKeyString`, `incrementalKeyNumber`, `incrementalKeyField`, `incrementalKeyAppend`, `incrementalSavePublication`, `incrementalRestorePublication`, `incrementalRecordSource`, `incrementalCheckSources`, `incrementalCurrentSources`, `configureModuleObjects`, `storeModuleObject`, `invalidateModuleCache`, `incrementalToolIdentity`, `incrementalSave`, `incrementalPublished`, `incrementalEqual`, `incrementalReleaseAttempt`, `incrementalFinish`, `incrementalHex`, `beginStoredInterfaces`, `storedInterfaceModule`, `storedInterfaceModuleId`, `privateInterfaceDependency`, `privateInterfaceBinding`, `privateCloseTemplates`, `privateCloseValues`, `storedInterfaceSwapCache`, `storedInterfaceRootValue`, `storedInterfaceAddress`, `privateInterfaceImport`, `storedInterfaceImportSource`, `privatePublishImportLinks`, `privateInterfaceImportSource`, `privateSealInterfaces`, `beginModulePipeline`, `switchModuleSession`, `publishCapturedModule`, `seedModuleConfiguration`, `swapModuleGraphs`, `finishModulePipeline`, `indexCandidates`, `pathStem`, `indexLine`, `resolveGraph`, `selectCandidate`, `releaseDiscovery`, `orderCandidatePaths`, `selectLine`, `matchSelectionWord`, `foldSelectionByte`, `saveRoot`, `parentPath`, `copyIncludeBase`, `resolveIncludedPath`, `resolveIncludeFrom`, `openIncludedPath`, `openIncludedFrom`, `normalizeIncludePath`, `authorizeIncludePath`, `allowedCandidate`, `sameNormalizedPath`, `samePath`, `foldInclude`, `pushInclude`, `popInclude`, `closeIncludes`, `clearIncludeText`, `appendOrigin`, `includeSelected`, `queueSource`, `enterPendingSource`, `includeBinary`, `openBinaryPath`, `appendBinaryData`, `retainOriginPath`, `captureView`, `releaseCaptureView`, `sourceCursor`, `sourcePeek`, `drainPrepared`, `replaySourceSlice`, `materializePendingSource`.

Imports: `opforge.assembler.symbols.lexical_store`, `opforge.assembler.modules.bindings`, `opforge.assembler.prepare.frontend`, `opforge.assembler.language.statement_execution`, `opforge.assembler.source.capture`, `opforge.assembler.source.capture_store`, `opforge.assembler.modules.configuration`, `opforge.assembler.language.conditionals`, `opforge.assembler.symbols.name_store`, `opforge.assembler.symbols.scopes`, `opforge.assembler.layout.assembly`, `opforge.assembler.layout.dependencies`, `opforge.assembler.layout.sections`, `opforge.assembler.output.hunk_image`, `opforge.assembler.output.hunk_stream`, `opforge.assembler.output.plan`, `opforge.assembler.output.io`, `opforge.assembler.output.records`, `opforge.assembler.output.spans`, `opforge.assembler.output.flat`, `opforge.assembler.prepare.metadata`, `opforge.assembler.output.metadata`, `opforge.packages.format`, `opforge.packages.loader`, `opforge.cli.progress`, `opforge.platform.files`, `opforge.platform.memory`, `opforge.assembler.source.records`, `opforge.assembler.symbols.storage`, `opforge.assembler.values.values`, `opforge.assembler.source.discovery`, `opforge.assembler.source.input_plan`, `opforge.assembler.source.line_input`, `opforge.assembler.source.declarations`, `opforge.assembler.modules.graph`, `opforge.assembler.prepare.record_order`, `opforge.assembler.prepare.record_store`, `opforge.assembler.source.record_view`, `opforge.assembler.layout.block_index`, `opforge.assembler.symbols.scope_layout`, `opforge.assembler.source.file_data`, `opforge.assembler.modules.compiler`, `opforge.assembler.modules.scope`, `opforge.assembler.modules.templates`, `opforge.assembler.modules.image`, `opforge.assembler.modules.registry`, `opforge.assembler.modules.object_store`, `opforge.assembler.modules.semantic_exports`, `opforge.assembler.modules.assembly_interface`, `opforge.assembler.modules.provider`, `opforge.assembler.modules.interface_store`, `opforge.assembler.modules.template_names`, `opforge.assembler.modules.runtime`, `opforge.assembler.modules.carrier`, `opforge.assembler.modules.admission`, `opforge.assembler.prepare.sections`, `opforge.assembler.symbols.imports`, `opforge.vm.exprvm.runtime`, `opforge.vm.prvm.abi`, `opforge.assembler.language.repeat_activation`, `opforge.assembler.language.branches`, `opforge.assembler.language.repetition`, `opforge.assembler.symbols.scalar_observations`, `opforge.assembler.layout.encoding`, `opforge.assembler.source.control`, `opforge.assembler.symbols.binding_records`, `opforge.debug.memory_profile`, `opforge.debug.binding_diagnostic`, `opforge.debug.capture_profile`, `opforge.debug.capture_lifetime`, `opforge.debug.lazy_audit`, `opforge.debug.lazy_units`, `opforge.debug.lazy_work`, `opforge.debug.hotspot_profile`.

## `opforge.assembler.prepare.frontend`

Source: `native/motorola68000/amigaos/assembler/prepare/frontend.asm`

Entry: `captureStatementRole`. Static routines: 96. Possible imports: 52. Section declarations: `bss`, `code`, `data`, `bss`.

Literal include closure (13 files): `native/motorola68000/amigaos/debug/memory_telemetry.i`, `native/motorola68000/amigaos/debug/memory_telemetry_noop.i`, `native/motorola68000/amigaos/debug/capture_telemetry.i`, `native/motorola68000/amigaos/debug/capture_telemetry_noop.i`, `native/motorola68000/amigaos/debug/lazy_audit.i`, `native/motorola68000/amigaos/debug/lazy_work.i`, `native/motorola68000/amigaos/debug/lazy_work_context.i`, `native/motorola68000/amigaos/debug/macro_packing_audit.i`, `native/motorola68000/amigaos/debug/candidate_audit.i`, `native/motorola68000/amigaos/debug/hotspot_telemetry.i`, `native/motorola68000/amigaos/debug/hotspot_telemetry_noop.i`, `native/motorola68000/amigaos/assembler/prepare/frontend_fragment_req.i`, `native/motorola68000/amigaos/assembler/language/statement_frontend.i`.

Routines: `captureStatementRole`, `scratchSize`, `begin`, `beginGraph`, `copyMemberKeys`, `exportBindings`, `exportSeeds`, `beginFileDerived`, `endFileDerived`, `setLine`, `line`, `captureLine`, `replayLine`, `previewCapture`, `bindCapture`, `copyCaptureBytes`, `captureCandidates`, `captureCandidate`, `tokenizeLine`, `lowerTokens`, `writeWithHeadPolicy`, `selectHeadPolicy`, `writeTokens`, `namedFragmentCandidate`, `configureWriter`, `bodyFragmentCandidate`, `retainedFragmentBody`, `fragmentLinePlan`, `textNameBody`, `numericSuffixPlan`, `numericSuffixOverlap`, `retainedSuffixOverlap`, `fragmentSuffixOverlap`, `fragmentLine`, `initialPlan`, `bindCapturedPlan`, `physicalLineRecipe`, `sourceDescriptors`, `captureFragments`, `generatedPlan`, `relexGeneratedCall`, `expansionDepth`, `expansionCursor`, `templateRole`, `previewExpansion`, `scopeState`, `conditionalState`, `nextExpansion`, `nextTemplate`, `expandRecord`, `processRecord`, `data`, `sourceScalar`, `classifyFile`, `runFilePlan`, `fileLine`, `endFile`, `orderGraph`, `complete`, `completeBegin`, `completeLayouts`, `completeLayoutsEnd`, `completeOutputs`, `completeBindings`, `completeRecords`, `completeEnd`, `parameterBytes`, `copyParameters`, `indexBlocks`, `selectBlocks`, `activate`, `finish`, `checkLexicalSession`, `configure`, `activateControl`, `validateMacroPrograms`, `validateMemberBindings`, `updateRole`, `declarationRole`, `loopRole`, `fileRole`, `captureFile`, `bindMember`, `lookupPackage`, `bind`, `bindTextName`, `templateIdentity`, `hash`, `equal`, `statementLine`, `statementRequest`, `statementInvoke`, `statementInvokeExpanded`, `invokeStatement`, `statementIntern`, `statementKeywordIntern`.

Imports: `opforge.assembler.symbols.lexical_store`, `opforge.assembler.symbols.name_store`, `opforge.packages.format`, `opforge.assembler.layout.state`, `opforge.assembler.source.records`, `opforge.assembler.symbols.members`, `opforge.assembler.prepare.member_syntax`, `opforge.assembler.prepare.prepare`, `opforge.assembler.prepare.operand_binding`, `opforge.assembler.prepare.expression`, `opforge.assembler.prepare.data`, `opforge.assembler.prepare.metadata`, `opforge.vm.prvm.packed_metadata`, `opforge.assembler.symbols.scopes`, `opforge.assembler.symbols.scope_layout`, `opforge.assembler.symbols.binding_records`, `opforge.assembler.language.structs`, `opforge.assembler.language.conditionals`, `opforge.assembler.language.templates`, `opforge.assembler.source.capture`, `opforge.assembler.language.macro_plans`, `opforge.assembler.language.statement_heads`, `opforge.assembler.language.statement_session`, `opforge.assembler.language.statement_execution`, `opforge.assembler.language.statements`, `opforge.assembler.symbols.imports`, `opforge.assembler.modules.graph`, `opforge.assembler.layout.block_index`, `opforge.assembler.modules.bindings`, `opforge.platform.memory`, `opforge.assembler.values.values`, `opforge.assembler.values.structs`, `opforge.vm.tkvm.runtime`, `opforge.vm.tkvm.fragments`, `opforge.assembler.language.macro_fragments`, `opforge.vm.tkvm.control`, `opforge.vm.prvm.macro_runtime`, `opforge.vm.prvm.packed_file`, `opforge.assembler.language.declaration`, `opforge.vm.prvm.packed_declaration`, `opforge.vm.prvm.packed_loop`, `opforge.vm.prvm.runtime`, `opforge.vm.prvm.abi`, `opforge.vm.prvm.macro_spelling`, `opforge.assembler.language.text_names`, `opforge.vm.prvm.packed_text`, `opforge.debug.memory_profile`, `opforge.debug.capture_profile`, `opforge.debug.lazy_audit`, `opforge.debug.lazy_units`, `opforge.debug.lazy_work`, `opforge.debug.hotspot_profile`.

## `opforge.assembler.prepare.prepare`

Source: `native/motorola68000/amigaos/assembler/prepare/prepare.asm`

Entry: `line`. Static routines: 13. Possible imports: 12. Section declarations: `code`, `bss`.

Literal include closure (0 files): (none).

Routines: `line`, `lineUnfolded`, `lineMode`, `copy`, `tupleTail`, `callTail`, `tuplePrepare`, `name`, `validateNameSequence`, `packageRegister`, `prepareTextDirective`, `string`, `compile`.

Imports: `opforge.packages.format`, `opforge.assembler.layout.state`, `opforge.assembler.source.records`, `opforge.assembler.prepare.expression`, `opforge.assembler.prepare.operand_wrappers`, `opforge.assembler.prepare.products`, `opforge.assembler.prepare.nested_operands`, `opforge.vm.prvm.packed_loop`, `opforge.vm.prvm.abi`, `opforge.vm.prvm.packed_branch`, `opforge.vm.prvm.packed_text`, `opforge.vm.prvm.macro_runtime`.

## `opforge.assembler.prepare.operand_binding`

Source: `native/motorola68000/amigaos/assembler/prepare/operand_binding.asm`

Entry: `line`. Static routines: 18. Possible imports: 8. Section declarations: `code`.

Literal include closure (2 files): `native/motorola68000/amigaos/debug/telemetry_macros.i`, `native/motorola68000/amigaos/debug/telemetry_macros_noop.i`.

Routines: `line`, `copyRecord`, `needsCandidates`, `split`, `shape`, `originalSpan`, `trialSpan`, `span`, `table`, `rowProjections`, `projections`, `projection`, `tupleLeaf`, `scalarSyntax`, `exactName`, `registerName`, `candidateNames`, `merge`.

Imports: `opforge.packages.format`, `opforge.assembler.symbols.scopes`, `opforge.assembler.source.records`, `opforge.assembler.prepare.nested_operands`, `opforge.assembler.prepare.register_mask`, `opforge.assembler.prepare.expression`, `opforge.vm.prvm.abi`, `opforge.debug.runtime_profile`.

## `opforge.assembler.prepare.expression`

Source: `native/motorola68000/amigaos/assembler/prepare/expression.asm`

Entry: `configure`. Static routines: 19. Possible imports: 14. Section declarations: `code`, `bss`.

Literal include closure (6 files): `native/motorola68000/amigaos/debug/telemetry_macros.i`, `native/motorola68000/amigaos/debug/telemetry_macros_noop.i`, `native/motorola68000/amigaos/debug/memory_telemetry.i`, `native/motorola68000/amigaos/debug/memory_telemetry_noop.i`, `native/motorola68000/amigaos/debug/hotspot_telemetry.i`, `native/motorola68000/amigaos/debug/hotspot_telemetry_noop.i`.

Routines: `configure`, `deferCompile`, `configureBuiltin`, `isBuiltin`, `capsuleSpan`, `singleSymbol`, `bindFields`, `rawTokenEnd`, `compile`, `compileUnfolded`, `compileValue`, `compileValueUnfolded`, `compileMode`, `requiresTyped`, `compoundSymbols`, `bareTypeScalar`, `evaluate`, `evaluateWithSymbols`, `evaluateValue`.

Imports: `opforge.vm.exprvm.runtime`, `opforge.assembler.prepare.fold`, `opforge.vm.exvm.runtime`, `opforge.assembler.prepare.expression_lowering`, `opforge.vm.exprvm.values_runtime`, `opforge.assembler.values.values`, `opforge.assembler.values.records`, `opforge.assembler.values.structs`, `opforge.assembler.symbols.bindings`, `opforge.platform.memory`, `opforge.assembler.source.records`, `opforge.debug.runtime_profile`, `opforge.debug.memory_profile`, `opforge.debug.hotspot_profile`.

## `opforge.assembler.prepare.data`

Source: `native/motorola68000/amigaos/assembler/prepare/data.asm`

Entry: `check`. Static routines: 1. Possible imports: 8. Section declarations: `code`, `bss`.

Literal include closure (0 files): (none).

Routines: `check`.

Imports: `opforge.assembler.symbols.lexical_store`, `opforge.packages.format`, `opforge.assembler.symbols.scope_layout`, `opforge.assembler.symbols.binding_records`, `opforge.assembler.symbols.imports`, `opforge.vm.prvm.abi`, `opforge.vm.prvm.macro_runtime`, `opforge.assembler.source.records`.

## `opforge.assembler.layout.assembly`

Source: `native/motorola68000/amigaos/assembler/layout/assembly.asm`

Entry: `beginSizing`. Static routines: 23. Possible imports: 31. Section declarations: `bss`, `code`.

Literal include closure (5 files): `native/motorola68000/amigaos/debug/memory_telemetry.i`, `native/motorola68000/amigaos/debug/memory_telemetry_noop.i`, `native/motorola68000/amigaos/assembler/layout/data_reserve.i`, `native/motorola68000/amigaos/assembler/layout/fragments.i`, `native/motorola68000/amigaos/assembler/layout/data_emit.i`.

Routines: `beginSizing`, `advance`, `finishSizing`, `emit`, `release`, `assemble`, `execute`, `moduleEntries`, `refreshViews`, `stateRecord`, `statementSelected`, `statement`, `warnByte`, `markInstructionRelocs`, `markDataReloc`, `name`, `emitGap`, `emitBytes`, `reserveDataBytes`, `reserveSectionBytes`, `emitAlignment`, `emitCompiledFragment`, `emitData`.

Imports: `opforge.packages.format`, `opforge.assembler.layout.session`, `opforge.assembler.values.values`, `opforge.assembler.symbols.parameters`, `opforge.assembler.layout.state`, `opforge.assembler.prepare.expression`, `opforge.vm.exprvm.runtime`, `opforge.vm.prvm.abi`, `opforge.vm.prvm.macro_runtime`, `opforge.assembler.layout.encoding`, `opforge.assembler.layout.dependencies`, `opforge.assembler.symbols.scalar_observations`, `opforge.assembler.symbols.mutable`, `opforge.assembler.symbols.constants`, `opforge.assembler.source.records`, `opforge.assembler.source.payload_bounds`, `opforge.assembler.layout.sections`, `opforge.assembler.layout.hunk_mapping`, `opforge.assembler.layout.hunk_placement`, `opforge.assembler.layout.hunk_references`, `opforge.assembler.language.repetition`, `opforge.assembler.language.branches`, `opforge.assembler.values.guards`, `opforge.assembler.language.repeat_inventory`, `opforge.assembler.language.text_execution`, `opforge.assembler.language.text`, `opforge.assembler.language.text_layout`, `opforge.assembler.language.string_provider`, `opforge.platform.memory`, `opforge.assembler.modules.records`, `opforge.debug.memory_profile`.

## `opforge.assembler.layout.encoding`

Source: `native/motorola68000/amigaos/assembler/layout/encoding.asm`

Entry: `encode`. Static routines: 63. Possible imports: 18. Section declarations: `bss`, `code`.

Literal include closure (4 files): `native/motorola68000/amigaos/debug/telemetry_macros.i`, `native/motorola68000/amigaos/debug/telemetry_macros_noop.i`, `native/motorola68000/amigaos/debug/memory_telemetry.i`, `native/motorola68000/amigaos/debug/memory_telemetry_noop.i`.

Routines: `encode`, `outputFixupCount`, `outputPositionProof`, `outputPositionCancellation`, `outputRelativeField`, `outputPositionDependent`, `outputPositionRecipe`, `outputPositionRequestedCandidate`, `configureScalarCapture`, `positionInterval`, `outputGeometryProof`, `patchPosition`, `outputFixup`, `outputFixupIdentity`, `patchOutputFixup`, `splitOperands`, `knownRegister`, `findRegisterClass`, `excludedName`, `requiredForms`, `tupleClasses`, `tupleOperandClass`, `skipToken`, `tryRow`, `hasWiderRow`, `sequence`, `geometryUnproven`, `observeOperandGeometry`, `failedProjectionGeometry`, `captureScalar`, `evaluateScalar`, `evaluateOperandZero`, `storeWord`, `program`, `programId`, `project`, `projectionTargetExpression`, `projectionAtomicTarget`, `projectFixup`, `projectionExpression`, `projectionHashScalar`, `scalarIdentity`, `projectionRegister`, `projectionNamed`, `memberBase`, `projectionMemberShape`, `projectionMember`, `projectionWrappedRegister`, `projectionTupleItem`, `tupleRegisterLeaf`, `tupleProjectionSpan`, `tupleArity`, `projectionScalarExpression`, `projectionWrappedScalar`, `projectionTupleNamed`, `projectionRegisterMask`, `operandSpan`, `exactTarget`, `exactName`, `register`, `lookupRegister`, `valueProgram`, `prepareExecution`.

Imports: `opforge.packages.format`, `opforge.assembler.layout.state`, `opforge.assembler.source.records`, `opforge.assembler.prepare.shapes`, `opforge.assembler.prepare.operand_wrappers`, `opforge.assembler.prepare.tuples`, `opforge.assembler.prepare.products`, `opforge.assembler.prepare.operand_paths`, `opforge.assembler.layout.dependencies`, `opforge.assembler.symbols.mutable`, `opforge.assembler.layout.hunk_references`, `opforge.assembler.prepare.register_mask`, `opforge.assembler.prepare.expression`, `opforge.vm.exprvm.runtime`, `opforge.vm.encoding.runtime`, `opforge.vm.value.runtime`, `opforge.debug.runtime_profile`, `opforge.debug.memory_profile`.

## `opforge.assembler.layout.dependencies`

Source: `native/motorola68000/amigaos/assembler/layout/dependencies.asm`

Entry: `resolve`. Static routines: 16. Possible imports: 16. Section declarations: `bss`, `code`.

Literal include closure (2 files): `native/motorola68000/amigaos/debug/telemetry_macros.i`, `native/motorola68000/amigaos/debug/telemetry_macros_noop.i`.

Routines: `resolve`, `resolveLocal`, `activateLocal`, `activateGlobal`, `beginGlobal`, `release`, `observationMetadata`, `memberPlan`, `push`, `index`, `compiledLabels`, `indexBranch`, `indexLoop`, `discardViews`, `resetLayout`, `graphStorage`.

Imports: `opforge.packages.format`, `opforge.platform.memory`, `opforge.assembler.values.values`, `opforge.assembler.symbols.bindings`, `opforge.assembler.prepare.expression`, `opforge.vm.exprvm.runtime`, `opforge.assembler.source.records`, `opforge.assembler.modules.records`, `opforge.assembler.language.repeat_inventory`, `opforge.assembler.language.repeat_dependencies`, `opforge.assembler.language.repeat_activation`, `opforge.assembler.layout.dependency_layout`, `opforge.vm.prvm.packed_branch`, `opforge.vm.prvm.packed_loop`, `opforge.vm.prvm.abi`, `opforge.debug.runtime_profile`.

## `opforge.assembler.layout.sections`

Source: `native/motorola68000/amigaos/assembler/layout/sections.asm`

Entry: `scan`. Static routines: 13. Possible imports: 2. Section declarations: `code`.

Literal include closure (0 files): (none).

Routines: `scan`, `scanHunkControl`, `slotAddress`, `beginPass`, `control`, `finishHunk`, `checkHunkPc`, `routeHunk`, `reserve`, `checkInitialized`, `checkEmit`, `finishPass`, `sectionKind`.

Imports: `opforge.packages.format`, `opforge.assembler.source.records`.

## `opforge.assembler.source.control`

Source: `native/motorola68000/amigaos/assembler/source/control.asm`

Entry: `begin`. Static routines: 16. Possible imports: 11. Section declarations: `code`, `bss`.

Literal include closure (0 files): (none).

Routines: `begin`, `line`, `classifyLoop`, `scanBody`, `evaluateHeader`, `selectIterator`, `peek`, `seek`, `fileAction`, `openIteration`, `openCollection`, `boundary`, `beginBatch`, `scalarDestination`, `appendScalar`, `finishBatch`.

Imports: `opforge.assembler.prepare.frontend`, `opforge.packages.format`, `opforge.assembler.symbols.scopes`, `opforge.assembler.symbols.scope_layout`, `opforge.assembler.language.conditionals`, `opforge.assembler.symbols.imports`, `opforge.assembler.values.values`, `opforge.assembler.source.records`, `opforge.assembler.symbols.materialized_scope`, `opforge.vm.prvm.packed_loop`, `opforge.vm.prvm.abi`.

## `opforge.assembler.language.statement_execution`

Source: `native/motorola68000/amigaos/assembler/language/statement_execution.asm`

Entry: `begin`. Static routines: 8. Possible imports: 12. Section declarations: `bss`, `code`.

Literal include closure (3 files): `native/motorola68000/amigaos/debug/memory_telemetry.i`, `native/motorola68000/amigaos/debug/memory_telemetry_noop.i`, `native/motorola68000/amigaos/assembler/language/statement_exec_req.i`.

Routines: `begin`, `finish`, `run`, `prepareRequest`, `tailRequest`, `selectRequest`, `packRequest`, `callRequest`.

Imports: `opforge.platform.memory`, `opforge.packages.format`, `opforge.assembler.source.records`, `opforge.assembler.language.statements`, `opforge.assembler.language.statement_session`, `opforge.assembler.language.statement_call`, `opforge.assembler.language.statement_tail`, `opforge.assembler.language.statement_arguments`, `opforge.assembler.language.templates`, `opforge.assembler.prepare.tokenizer_capsule`, `opforge.vm.prvm.statement_abi`, `opforge.debug.memory_profile`.

## `opforge.assembler.modules.provider`

Source: `native/motorola68000/amigaos/assembler/modules/provider.asm`

Entry: `begin`. Static routines: 27. Possible imports: 18. Section declarations: `code`.

Literal include closure (1 files): `native/motorola68000/amigaos/assembler/modules/provider_tmpl.i`.

Routines: `begin`, `add`, `lookup`, `acquire`, `materialize`, `resolve`, `admitMemberBase`, `drain`, `hydrate`, `valueSlot`, `address`, `prepareRecord`, `ensure`, `bindMember`, `reference`, `equal`, `fold`, `owners`, `seed`, `seal`, `release`, `templateName`, `ensureTemplate`, `drainTemplates`, `templateRow`, `templateDependency`, `templateResolve`.

Imports: `opforge.assembler.prepare.frontend`, `opforge.assembler.symbols.scope_layout`, `opforge.assembler.symbols.lexical_store`, `opforge.assembler.symbols.binding_records`, `opforge.assembler.modules.interface_store`, `opforge.assembler.modules.registry`, `opforge.assembler.modules.semantic_exports`, `opforge.assembler.modules.scope`, `opforge.platform.memory`, `opforge.assembler.values.values`, `opforge.assembler.layout.assembly`, `opforge.packages.format`, `opforge.assembler.layout.dependencies`, `opforge.assembler.symbols.scopes`, `opforge.assembler.modules.remap`, `opforge.assembler.modules.templates`, `opforge.assembler.modules.template_names`, `opforge.vm.exprvm.runtime`.

## `opforge.assembler.output.plan`

Source: `native/motorola68000/amigaos/assembler/output/plan.asm`

Entry: `next`. Static routines: 2. Possible imports: 1. Section declarations: `code`.

Literal include closure (0 files): (none).

Routines: `next`, `range`.

Imports: `opforge.assembler.layout.sections`.

## `opforge.assembler.output.hunk_image`

Source: `native/motorola68000/amigaos/assembler/output/hunk_image.asm`

Entry: `build`. Static routines: 4. Possible imports: 0. Section declarations: `code`.

Literal include closure (0 files): (none).

Routines: `build`, `measure`, `countTarget`, `emit`.

Imports: (none).

## `opforge.packages.loader`

Source: `native/motorola68000/amigaos/packages/loader.asm`

Entry: `acquire`. Static routines: 4. Possible imports: 5. Section declarations: `code`, `bss`.

Literal include closure (2 files): `native/motorola68000/amigaos/debug/memory_telemetry.i`, `native/motorola68000/amigaos/debug/memory_telemetry_noop.i`.

Routines: `acquire`, `release`, `readExact`, `close`.

Imports: `opforge.packages.catalog`, `opforge.packages.format`, `opforge.packages.validation`, `opforge.platform.memory`, `opforge.debug.memory_profile`.

## `opforge.packages.validation`

Source: `native/motorola68000/amigaos/packages/validation.asm`

Entry: `validate`. Static routines: 5. Possible imports: 11. Section declarations: `code`.

Literal include closure (0 files): (none).

Routines: `validate`, `table`, `macro`, `preparationProgram`, `span`.

Imports: `opforge.packages.format`, `opforge.assembler.layout.state`, `opforge.platform.memory`, `opforge.vm.prvm.abi`, `opforge.vm.prvm.packed_branch`, `opforge.vm.prvm.packed_declaration`, `opforge.vm.prvm.packed_text`, `opforge.vm.prvm.statement_abi`, `opforge.vm.prvm.statement_compile`, `opforge.vm.prvm.statement_match`, `opforge.vm.prvm.statement_head`.

## `opforge.vm.exvm.runtime`

Source: `native/motorola68000/amigaos/vm/exvm/runtime.asm`

Entry: `compile`. Static routines: 14. Possible imports: 4. Section declarations: `code`, `data`.

Literal include closure (3 files): `native/motorola68000/amigaos/debug/lazy_work.i`, `native/motorola68000/amigaos/debug/telemetry_macros.i`, `native/motorola68000/amigaos/debug/telemetry_macros_noop.i`.

Routines: `compile`, `execute`, `parseSequence`, `parseStruct`, `parseIndex`, `parseMember`, `programByte`, `target`, `token`, `kindMatch`, `operatorMatch`, `allocate`, `push`, `pop`.

Imports: `opforge.vm.tkvm.runtime`, `opforge.assembler.source.records`, `opforge.debug.lazy_work`, `opforge.debug.runtime_profile`.

## `opforge.vm.exprvm.runtime`

Source: `native/motorola68000/amigaos/vm/exprvm/runtime.asm`

Entry: `configureStringProvider`. Static routines: 13. Possible imports: 2. Section declarations: `code`, `bss`.

Literal include closure (2 files): `native/motorola68000/amigaos/debug/telemetry_macros.i`, `native/motorola68000/amigaos/debug/telemetry_macros_noop.i`.

Routines: `configureStringProvider`, `evaluateString`, `evalNumeric64`, `evalCompact64`, `exprvmEvalProgramV1`, `exprvmGetLastResultHighV1`, `compareI64`, `pushD3`, `popD3`, `readU8`, `readU16`, `readI64`, `readLiteralLong`.

Imports: `opforge.vm.exprvm.i64_math`, `opforge.debug.runtime_profile`.

## `opforge.vm.exprvm.i64_math`

Source: `native/motorola68000/amigaos/vm/exprvm/i64_math.asm`

Entry: `multiplyV1`. Static routines: 4. Possible imports: 0. Section declarations: `code`.

Literal include closure (0 files): (none).

Routines: `multiplyV1`, `powerV1`, `divideModuloV1`, `multiplyCore`.

Imports: (none).

## `opforge.vm.prvm.runtime`

Source: `native/motorola68000/amigaos/vm/prvm/runtime.asm`

Entry: `prvmRun68000`. Static routines: 24. Possible imports: 3. Section declarations: `data`, `code`.

Literal include closure (2 files): `native/motorola68000/amigaos/debug/telemetry_macros.i`, `native/motorola68000/amigaos/debug/telemetry_macros_noop.i`.

Routines: `prvmRun68000`, `nextBoundary`, `operandSpan`, `validateResumeLocals`, `validateContinuationPc`, `currentTokenPtr`, `tokenPtrByIndex`, `readProgramTarget`, `pushCheckpoint`, `popCheckpointAddress`, `checkpointAddressForDepth`, `peekKind`, `resultRecordPtr`, `commitResultRecord`, `emitBeginStatement`, `emitLabelText`, `emitMnemonicText`, `emitDirectiveText`, `emitOperandTextSpan`, `emitOperandExprSlot`, `emitFinishLine`, `writeExpressionRequest`, `writeResumeState`, `validateExpressionResultSlot`.

Imports: `opforge.vm.prvm.abi`, `opforge.vm.prvm.macro_runtime`, `opforge.debug.runtime_profile`.

## `opforge.vm.tkvm.runtime`

Source: `native/motorola68000/amigaos/vm/tkvm/runtime.asm`

Entry: `tkvmRun68000`. Static routines: 7. Possible imports: 5. Section declarations: `code`.

Literal include closure (4 files): `native/motorola68000/amigaos/debug/memory_telemetry.i`, `native/motorola68000/amigaos/debug/memory_telemetry_noop.i`, `native/motorola68000/amigaos/vm/tkvm/numeric_normalization.i`, `native/motorola68000/amigaos/vm/tkvm/composed_names.i`.

Routines: `tkvmRun68000`, `normalizeNumbers`, `markerEqual`, `composeNames`, `resetIterator`, `nextByte`, `markerRecord`.

Imports: `opforge.vm.tkvm.state`, `opforge.vm.tkvm.char_predicates`, `opforge.vm.tkvm.scanner`, `opforge.debug.runtime_profile`, `opforge.debug.memory_profile`.

## `opforge.vm.encoding.runtime`

Source: `native/motorola68000/amigaos/vm/encoding/runtime.asm`

Entry: `semantic`. Static routines: 23. Possible imports: 1. Section declarations: `data`, `code`.

Literal include closure (3 files): `native/motorola68000/amigaos/debug/telemetry_macros.i`, `native/motorola68000/amigaos/debug/telemetry_macros_noop.i`, `native/motorola68000/amigaos/vm/encoding/branch_interval.i`.

Routines: `semantic`, `branchProgram`, `runBranch`, `fitsSignedWidth`, `runFixup`, `observeFixupField`, `applyTransform`, `validateAlignment`, `readI64`, `requireI32`, `loadFixupInput`, `recordFixup`, `normalizeFixupLength`, `validateFixupRange`, `runEncoding`, `requireBytes`, `readU32`, `validateUnit`, `emitUnit`, `loadInput`, `table`, `validateBranchInterval`, `intervalAvailable`.

Imports: `opforge.debug.runtime_profile`.

## `opforge.vm.value.runtime`

Source: `native/motorola68000/amigaos/vm/value/runtime.asm`

Entry: `execute`. Static routines: 2. Possible imports: 1. Section declarations: `code`.

Literal include closure (2 files): `native/motorola68000/amigaos/debug/telemetry_macros.i`, `native/motorola68000/amigaos/debug/telemetry_macros_noop.i`.

Routines: `execute`, `readI64Le`.

Imports: `opforge.debug.runtime_profile`.

## `opforge.cli.entry`

Source: `native/motorola68000/amigaos/cli/entry.asm`

Entry: `start`. Static routines: 2. Possible imports: 4. Section declarations: `code`, `data`, `bss`.

Literal include closure (0 files): (none).

Routines: `start`, `closeDos`.

Imports: `opforge.assembler.driver`, `opforge.cli.arguments`, `opforge.cli.input`, `opforge.cli.target`.

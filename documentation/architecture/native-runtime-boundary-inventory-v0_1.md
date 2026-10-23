# Native runtime boundary inventory

The promoted native pipeline owns source preparation, module linking, layout and output. Shared VM executors consume package/wire contracts; they do not invoke CLI entry or assembler orchestration. The guard snapshots this current source surface so ownership changes require review and a synchronized inventory.

## `opforge.assembler.driver`

Source: `native/motorola68000/amigaos/assembler/driver.asm`

Entry: `execute`. Routines: 22. Imports: 66. Sections: `code`, `data`, `bss`, `bss`.

Routines: `execute`, `readExact`, `prepare`, `prepareCliInputs`, `releaseGraph`, `openSource`, `readManifestPath`, `appendCandidate`, `closeSource`, `fileSpan`, `closeInput`, `readByte`, `lowerLine`, `appendPrepared`, `run`, `appendEmission`, `allocateOutput`, `appendReloc`, `writeOutputs`, `writeSelected`, `writeRecords`, `copy`.

Imports: `opforge.assembler.symbols.lexical_store`, `opforge.assembler.modules.bindings`, `opforge.assembler.prepare.frontend`, `opforge.assembler.language.statement_execution`, `opforge.assembler.source.capture`, `opforge.assembler.source.capture_store`, `opforge.assembler.modules.configuration`, `opforge.assembler.language.conditionals`, `opforge.assembler.symbols.name_store`, `opforge.assembler.symbols.scopes`, `opforge.assembler.layout.assembly`, `opforge.assembler.layout.dependencies`, `opforge.assembler.layout.sections`, `opforge.assembler.output.hunk_image`, `opforge.assembler.output.hunk_stream`, `opforge.assembler.output.plan`, `opforge.assembler.output.io`, `opforge.assembler.output.records`, `opforge.assembler.output.spans`, `opforge.assembler.output.flat`, `opforge.assembler.prepare.metadata`, `opforge.assembler.output.metadata`, `opforge.packages.format`, `opforge.packages.loader`, `opforge.cli.progress`, `opforge.platform.files`, `opforge.platform.memory`, `opforge.assembler.source.records`, `opforge.assembler.symbols.storage`, `opforge.assembler.values.values`, `opforge.assembler.source.discovery`, `opforge.assembler.source.input_plan`, `opforge.assembler.source.line_input`, `opforge.assembler.source.declarations`, `opforge.assembler.modules.graph`, `opforge.assembler.prepare.record_order`, `opforge.assembler.prepare.record_store`, `opforge.assembler.source.record_view`, `opforge.assembler.layout.block_index`, `opforge.assembler.symbols.scope_layout`, `opforge.assembler.source.file_data`, `opforge.assembler.modules.compiler`, `opforge.assembler.modules.scope`, `opforge.assembler.modules.templates`, `opforge.assembler.modules.image`, `opforge.assembler.modules.registry`, `opforge.assembler.modules.object_store`, `opforge.assembler.modules.semantic_exports`, `opforge.assembler.modules.assembly_interface`, `opforge.assembler.modules.provider`, `opforge.assembler.modules.interface_store`, `opforge.assembler.modules.template_names`, `opforge.assembler.modules.runtime`, `opforge.assembler.modules.carrier`, `opforge.assembler.modules.admission`, `opforge.assembler.prepare.sections`, `opforge.assembler.symbols.imports`, `opforge.vm.exprvm.runtime`, `opforge.vm.prvm.abi`, `opforge.assembler.language.repeat_activation`, `opforge.assembler.language.branches`, `opforge.assembler.language.repetition`, `opforge.assembler.symbols.scalar_observations`, `opforge.assembler.layout.encoding`, `opforge.assembler.source.control`, `opforge.assembler.symbols.binding_records`.

## `opforge.assembler.prepare.frontend`

Source: `native/motorola68000/amigaos/assembler/prepare/frontend.asm`

Entry: `captureStatementRole`. Routines: 89. Imports: 46. Sections: `bss`, `code`, `data`, `bss`.

Routines: `captureStatementRole`, `scratchSize`, `begin`, `beginGraph`, `copyMemberKeys`, `exportBindings`, `exportSeeds`, `beginFileDerived`, `endFileDerived`, `setLine`, `line`, `captureLine`, `replayLine`, `previewCapture`, `bindCapture`, `copyCaptureBytes`, `captureCandidates`, `captureCandidate`, `tokenizeLine`, `lowerTokens`, `writeWithHeadPolicy`, `selectHeadPolicy`, `writeTokens`, `namedFragmentCandidate`, `configureWriter`, `bodyFragmentCandidate`, `retainedFragmentBody`, `fragmentLinePlan`, `textNameBody`, `numericSuffixPlan`, `numericSuffixOverlap`, `retainedSuffixOverlap`, `fragmentSuffixOverlap`, `fragmentLine`, `initialPlan`, `bindCapturedPlan`, `physicalLineRecipe`, `sourceDescriptors`, `captureFragments`, `generatedPlan`, `relexGeneratedCall`, `expansionDepth`, `expansionCursor`, `templateRole`, `previewExpansion`, `scopeState`, `conditionalState`, `nextExpansion`, `nextTemplate`, `expandRecord`, `processRecord`, `data`, `sourceScalar`, `classifyFile`, `runFilePlan`, `fileLine`, `endFile`, `orderGraph`, `complete`, `completeBegin`, `completeLayouts`, `completeLayoutsEnd`, `completeOutputs`, `completeBindings`, `completeRecords`, `completeEnd`, `parameterBytes`, `copyParameters`, `indexBlocks`, `selectBlocks`, `activate`, `finish`, `checkLexicalSession`, `configure`, `activateControl`, `validateMacroPrograms`, `validateMemberBindings`, `updateRole`, `declarationRole`, `loopRole`, `fileRole`, `captureFile`, `bindMember`, `lookupPackage`, `bind`, `bindTextName`, `templateIdentity`, `hash`, `equal`.

Imports: `opforge.assembler.symbols.lexical_store`, `opforge.assembler.symbols.name_store`, `opforge.packages.format`, `opforge.assembler.layout.state`, `opforge.assembler.source.records`, `opforge.assembler.symbols.members`, `opforge.assembler.prepare.member_syntax`, `opforge.assembler.prepare.prepare`, `opforge.assembler.prepare.operand_binding`, `opforge.assembler.prepare.expression`, `opforge.assembler.prepare.data`, `opforge.assembler.prepare.metadata`, `opforge.vm.prvm.packed_metadata`, `opforge.assembler.symbols.scopes`, `opforge.assembler.symbols.scope_layout`, `opforge.assembler.symbols.binding_records`, `opforge.assembler.language.structs`, `opforge.assembler.language.conditionals`, `opforge.assembler.language.templates`, `opforge.assembler.source.capture`, `opforge.assembler.language.macro_plans`, `opforge.assembler.language.statement_heads`, `opforge.assembler.language.statement_session`, `opforge.assembler.language.statement_execution`, `opforge.assembler.language.statements`, `opforge.assembler.symbols.imports`, `opforge.assembler.modules.graph`, `opforge.assembler.layout.block_index`, `opforge.assembler.modules.bindings`, `opforge.platform.memory`, `opforge.assembler.values.values`, `opforge.assembler.values.structs`, `opforge.vm.tkvm.runtime`, `opforge.vm.tkvm.fragments`, `opforge.assembler.language.macro_fragments`, `opforge.vm.tkvm.control`, `opforge.vm.prvm.macro_runtime`, `opforge.vm.prvm.packed_file`, `opforge.assembler.language.declaration`, `opforge.vm.prvm.packed_declaration`, `opforge.vm.prvm.packed_loop`, `opforge.vm.prvm.runtime`, `opforge.vm.prvm.abi`, `opforge.vm.prvm.macro_spelling`, `opforge.assembler.language.text_names`, `opforge.vm.prvm.packed_text`.

## `opforge.assembler.prepare.prepare`

Source: `native/motorola68000/amigaos/assembler/prepare/prepare.asm`

Entry: `line`. Routines: 13. Imports: 12. Sections: `code`, `bss`.

Routines: `line`, `lineUnfolded`, `lineMode`, `copy`, `tupleTail`, `callTail`, `tuplePrepare`, `name`, `validateNameSequence`, `packageRegister`, `prepareTextDirective`, `string`, `compile`.

Imports: `opforge.packages.format`, `opforge.assembler.layout.state`, `opforge.assembler.source.records`, `opforge.assembler.prepare.expression`, `opforge.assembler.prepare.operand_wrappers`, `opforge.assembler.prepare.products`, `opforge.assembler.prepare.nested_operands`, `opforge.vm.prvm.packed_loop`, `opforge.vm.prvm.abi`, `opforge.vm.prvm.packed_branch`, `opforge.vm.prvm.packed_text`, `opforge.vm.prvm.macro_runtime`.

## `opforge.assembler.prepare.operand_binding`

Source: `native/motorola68000/amigaos/assembler/prepare/operand_binding.asm`

Entry: `line`. Routines: 18. Imports: 8. Sections: `code`.

Routines: `line`, `copyRecord`, `needsCandidates`, `split`, `shape`, `originalSpan`, `trialSpan`, `span`, `table`, `rowProjections`, `projections`, `projection`, `tupleLeaf`, `scalarSyntax`, `exactName`, `registerName`, `candidateNames`, `merge`.

Imports: `opforge.packages.format`, `opforge.assembler.symbols.scopes`, `opforge.assembler.source.records`, `opforge.assembler.prepare.nested_operands`, `opforge.assembler.prepare.register_mask`, `opforge.assembler.prepare.expression`, `opforge.vm.prvm.abi`, `opforge.debug.runtime_profile`.

## `opforge.assembler.prepare.expression`

Source: `native/motorola68000/amigaos/assembler/prepare/expression.asm`

Entry: `configure`. Routines: 19. Imports: 12. Sections: `code`, `bss`.

Routines: `configure`, `deferCompile`, `configureBuiltin`, `isBuiltin`, `capsuleSpan`, `singleSymbol`, `bindFields`, `rawTokenEnd`, `compile`, `compileUnfolded`, `compileValue`, `compileValueUnfolded`, `compileMode`, `requiresTyped`, `compoundSymbols`, `bareTypeScalar`, `evaluate`, `evaluateWithSymbols`, `evaluateValue`.

Imports: `opforge.vm.exprvm.runtime`, `opforge.assembler.prepare.fold`, `opforge.vm.exvm.runtime`, `opforge.assembler.prepare.expression_lowering`, `opforge.vm.exprvm.values_runtime`, `opforge.assembler.values.values`, `opforge.assembler.values.records`, `opforge.assembler.values.structs`, `opforge.assembler.symbols.bindings`, `opforge.platform.memory`, `opforge.assembler.source.records`, `opforge.debug.runtime_profile`.

## `opforge.assembler.prepare.data`

Source: `native/motorola68000/amigaos/assembler/prepare/data.asm`

Entry: `check`. Routines: 1. Imports: 8. Sections: `code`, `bss`.

Routines: `check`.

Imports: `opforge.assembler.symbols.lexical_store`, `opforge.packages.format`, `opforge.assembler.symbols.scope_layout`, `opforge.assembler.symbols.binding_records`, `opforge.assembler.symbols.imports`, `opforge.vm.prvm.abi`, `opforge.vm.prvm.macro_runtime`, `opforge.assembler.source.records`.

## `opforge.assembler.layout.assembly`

Source: `native/motorola68000/amigaos/assembler/layout/assembly.asm`

Entry: `beginSizing`. Routines: 18. Imports: 30. Sections: `bss`, `code`.

Routines: `beginSizing`, `advance`, `finishSizing`, `emit`, `release`, `assemble`, `execute`, `moduleEntries`, `refreshViews`, `stateRecord`, `statementSelected`, `statement`, `warnByte`, `markInstructionRelocs`, `markDataReloc`, `name`, `emitGap`, `emitBytes`.

Imports: `opforge.packages.format`, `opforge.assembler.layout.session`, `opforge.assembler.values.values`, `opforge.assembler.symbols.parameters`, `opforge.assembler.layout.state`, `opforge.assembler.prepare.expression`, `opforge.vm.exprvm.runtime`, `opforge.vm.prvm.abi`, `opforge.vm.prvm.macro_runtime`, `opforge.assembler.layout.encoding`, `opforge.assembler.layout.dependencies`, `opforge.assembler.symbols.scalar_observations`, `opforge.assembler.symbols.mutable`, `opforge.assembler.symbols.constants`, `opforge.assembler.source.records`, `opforge.assembler.source.payload_bounds`, `opforge.assembler.layout.sections`, `opforge.assembler.layout.hunk_mapping`, `opforge.assembler.layout.hunk_placement`, `opforge.assembler.layout.hunk_references`, `opforge.assembler.language.repetition`, `opforge.assembler.language.branches`, `opforge.assembler.values.guards`, `opforge.assembler.language.repeat_inventory`, `opforge.assembler.language.text_execution`, `opforge.assembler.language.text`, `opforge.assembler.language.text_layout`, `opforge.assembler.language.string_provider`, `opforge.platform.memory`, `opforge.assembler.modules.records`.

## `opforge.assembler.layout.encoding`

Source: `native/motorola68000/amigaos/assembler/layout/encoding.asm`

Entry: `encode`. Routines: 63. Imports: 17. Sections: `bss`, `code`.

Routines: `encode`, `outputFixupCount`, `outputPositionProof`, `outputPositionCancellation`, `outputRelativeField`, `outputPositionDependent`, `outputPositionRecipe`, `outputPositionRequestedCandidate`, `configureScalarCapture`, `positionInterval`, `outputGeometryProof`, `patchPosition`, `outputFixup`, `outputFixupIdentity`, `patchOutputFixup`, `splitOperands`, `knownRegister`, `findRegisterClass`, `excludedName`, `requiredForms`, `tupleClasses`, `tupleOperandClass`, `skipToken`, `tryRow`, `hasWiderRow`, `sequence`, `geometryUnproven`, `observeOperandGeometry`, `failedProjectionGeometry`, `captureScalar`, `evaluateScalar`, `evaluateOperandZero`, `storeWord`, `program`, `programId`, `project`, `projectionTargetExpression`, `projectionAtomicTarget`, `projectFixup`, `projectionExpression`, `projectionHashScalar`, `scalarIdentity`, `projectionRegister`, `projectionNamed`, `memberBase`, `projectionMemberShape`, `projectionMember`, `projectionWrappedRegister`, `projectionTupleItem`, `tupleRegisterLeaf`, `tupleProjectionSpan`, `tupleArity`, `projectionScalarExpression`, `projectionWrappedScalar`, `projectionTupleNamed`, `projectionRegisterMask`, `operandSpan`, `exactTarget`, `exactName`, `register`, `lookupRegister`, `valueProgram`, `prepareExecution`.

Imports: `opforge.packages.format`, `opforge.assembler.layout.state`, `opforge.assembler.source.records`, `opforge.assembler.prepare.shapes`, `opforge.assembler.prepare.operand_wrappers`, `opforge.assembler.prepare.tuples`, `opforge.assembler.prepare.products`, `opforge.assembler.prepare.operand_paths`, `opforge.assembler.layout.dependencies`, `opforge.assembler.symbols.mutable`, `opforge.assembler.layout.hunk_references`, `opforge.assembler.prepare.register_mask`, `opforge.assembler.prepare.expression`, `opforge.vm.exprvm.runtime`, `opforge.vm.encoding.runtime`, `opforge.vm.value.runtime`, `opforge.debug.runtime_profile`.

## `opforge.assembler.layout.dependencies`

Source: `native/motorola68000/amigaos/assembler/layout/dependencies.asm`

Entry: `resolve`. Routines: 16. Imports: 16. Sections: `bss`, `code`.

Routines: `resolve`, `resolveLocal`, `activateLocal`, `activateGlobal`, `beginGlobal`, `release`, `observationMetadata`, `memberPlan`, `push`, `index`, `compiledLabels`, `indexBranch`, `indexLoop`, `discardViews`, `resetLayout`, `graphStorage`.

Imports: `opforge.packages.format`, `opforge.platform.memory`, `opforge.assembler.values.values`, `opforge.assembler.symbols.bindings`, `opforge.assembler.prepare.expression`, `opforge.vm.exprvm.runtime`, `opforge.assembler.source.records`, `opforge.assembler.modules.records`, `opforge.assembler.language.repeat_inventory`, `opforge.assembler.language.repeat_dependencies`, `opforge.assembler.language.repeat_activation`, `opforge.assembler.layout.dependency_layout`, `opforge.vm.prvm.packed_branch`, `opforge.vm.prvm.packed_loop`, `opforge.vm.prvm.abi`, `opforge.debug.runtime_profile`.

## `opforge.assembler.layout.sections`

Source: `native/motorola68000/amigaos/assembler/layout/sections.asm`

Entry: `scan`. Routines: 13. Imports: 2. Sections: `code`.

Routines: `scan`, `scanHunkControl`, `slotAddress`, `beginPass`, `control`, `finishHunk`, `checkHunkPc`, `routeHunk`, `reserve`, `checkInitialized`, `checkEmit`, `finishPass`, `sectionKind`.

Imports: `opforge.packages.format`, `opforge.assembler.source.records`.

## `opforge.assembler.source.control`

Source: `native/motorola68000/amigaos/assembler/source/control.asm`

Entry: `begin`. Routines: 16. Imports: 11. Sections: `code`, `bss`.

Routines: `begin`, `line`, `classifyLoop`, `scanBody`, `evaluateHeader`, `selectIterator`, `peek`, `seek`, `fileAction`, `openIteration`, `openCollection`, `boundary`, `beginBatch`, `scalarDestination`, `appendScalar`, `finishBatch`.

Imports: `opforge.assembler.prepare.frontend`, `opforge.packages.format`, `opforge.assembler.symbols.scopes`, `opforge.assembler.symbols.scope_layout`, `opforge.assembler.language.conditionals`, `opforge.assembler.symbols.imports`, `opforge.assembler.values.values`, `opforge.assembler.source.records`, `opforge.assembler.symbols.materialized_scope`, `opforge.vm.prvm.packed_loop`, `opforge.vm.prvm.abi`.

## `opforge.assembler.language.statement_execution`

Source: `native/motorola68000/amigaos/assembler/language/statement_execution.asm`

Entry: `begin`. Routines: 3. Imports: 11. Sections: `bss`, `code`.

Routines: `begin`, `finish`, `run`.

Imports: `opforge.platform.memory`, `opforge.packages.format`, `opforge.assembler.source.records`, `opforge.assembler.language.statements`, `opforge.assembler.language.statement_session`, `opforge.assembler.language.statement_call`, `opforge.assembler.language.statement_tail`, `opforge.assembler.language.statement_arguments`, `opforge.assembler.language.templates`, `opforge.assembler.prepare.tokenizer_capsule`, `opforge.vm.prvm.statement_abi`.

## `opforge.assembler.modules.provider`

Source: `native/motorola68000/amigaos/assembler/modules/provider.asm`

Entry: `begin`. Routines: 21. Imports: 18. Sections: `code`.

Routines: `begin`, `add`, `lookup`, `acquire`, `materialize`, `resolve`, `admitMemberBase`, `drain`, `hydrate`, `valueSlot`, `address`, `prepareRecord`, `ensure`, `bindMember`, `reference`, `equal`, `fold`, `owners`, `seed`, `seal`, `release`.

Imports: `opforge.assembler.prepare.frontend`, `opforge.assembler.symbols.scope_layout`, `opforge.assembler.symbols.lexical_store`, `opforge.assembler.symbols.binding_records`, `opforge.assembler.modules.interface_store`, `opforge.assembler.modules.registry`, `opforge.assembler.modules.semantic_exports`, `opforge.assembler.modules.scope`, `opforge.platform.memory`, `opforge.assembler.values.values`, `opforge.assembler.layout.assembly`, `opforge.packages.format`, `opforge.assembler.layout.dependencies`, `opforge.assembler.symbols.scopes`, `opforge.assembler.modules.remap`, `opforge.assembler.modules.templates`, `opforge.assembler.modules.template_names`, `opforge.vm.exprvm.runtime`.

## `opforge.assembler.output.plan`

Source: `native/motorola68000/amigaos/assembler/output/plan.asm`

Entry: `next`. Routines: 2. Imports: 1. Sections: `code`.

Routines: `next`, `range`.

Imports: `opforge.assembler.layout.sections`.

## `opforge.assembler.output.hunk_image`

Source: `native/motorola68000/amigaos/assembler/output/hunk_image.asm`

Entry: `build`. Routines: 4. Imports: 0. Sections: `code`.

Routines: `build`, `measure`, `countTarget`, `emit`.

Imports: (none).

## `opforge.packages.loader`

Source: `native/motorola68000/amigaos/packages/loader.asm`

Entry: `acquire`. Routines: 4. Imports: 4. Sections: `code`, `bss`.

Routines: `acquire`, `release`, `readExact`, `close`.

Imports: `opforge.packages.catalog`, `opforge.packages.format`, `opforge.packages.validation`, `opforge.platform.memory`.

## `opforge.packages.validation`

Source: `native/motorola68000/amigaos/packages/validation.asm`

Entry: `validate`. Routines: 5. Imports: 11. Sections: `code`.

Routines: `validate`, `table`, `macro`, `preparationProgram`, `span`.

Imports: `opforge.packages.format`, `opforge.assembler.layout.state`, `opforge.platform.memory`, `opforge.vm.prvm.abi`, `opforge.vm.prvm.packed_branch`, `opforge.vm.prvm.packed_declaration`, `opforge.vm.prvm.packed_text`, `opforge.vm.prvm.statement_abi`, `opforge.vm.prvm.statement_compile`, `opforge.vm.prvm.statement_match`, `opforge.vm.prvm.statement_head`.

## `opforge.vm.exvm.runtime`

Source: `native/motorola68000/amigaos/vm/exvm/runtime.asm`

Entry: `compile`. Routines: 14. Imports: 3. Sections: `code`, `data`.

Routines: `compile`, `execute`, `parseSequence`, `parseStruct`, `parseIndex`, `parseMember`, `programByte`, `target`, `token`, `kindMatch`, `operatorMatch`, `allocate`, `push`, `pop`.

Imports: `opforge.vm.tkvm.runtime`, `opforge.assembler.source.records`, `opforge.debug.runtime_profile`.

## `opforge.vm.exprvm.runtime`

Source: `native/motorola68000/amigaos/vm/exprvm/runtime.asm`

Entry: `configureStringProvider`. Routines: 13. Imports: 2. Sections: `code`, `bss`.

Routines: `configureStringProvider`, `evaluateString`, `evalNumeric64`, `evalCompact64`, `exprvmEvalProgramV1`, `exprvmGetLastResultHighV1`, `compareI64`, `pushD3`, `popD3`, `readU8`, `readU16`, `readI64`, `readLiteralLong`.

Imports: `opforge.vm.exprvm.i64_math`, `opforge.debug.runtime_profile`.

## `opforge.vm.exprvm.i64_math`

Source: `native/motorola68000/amigaos/vm/exprvm/i64_math.asm`

Entry: `multiplyV1`. Routines: 4. Imports: 0. Sections: `code`.

Routines: `multiplyV1`, `powerV1`, `divideModuloV1`, `multiplyCore`.

Imports: (none).

## `opforge.vm.prvm.runtime`

Source: `native/motorola68000/amigaos/vm/prvm/runtime.asm`

Entry: `prvmRun68000`. Routines: 24. Imports: 3. Sections: `data`, `code`.

Routines: `prvmRun68000`, `nextBoundary`, `operandSpan`, `validateResumeLocals`, `validateContinuationPc`, `currentTokenPtr`, `tokenPtrByIndex`, `readProgramTarget`, `pushCheckpoint`, `popCheckpointAddress`, `checkpointAddressForDepth`, `peekKind`, `resultRecordPtr`, `commitResultRecord`, `emitBeginStatement`, `emitLabelText`, `emitMnemonicText`, `emitDirectiveText`, `emitOperandTextSpan`, `emitOperandExprSlot`, `emitFinishLine`, `writeExpressionRequest`, `writeResumeState`, `validateExpressionResultSlot`.

Imports: `opforge.vm.prvm.abi`, `opforge.vm.prvm.macro_runtime`, `opforge.debug.runtime_profile`.

## `opforge.vm.tkvm.runtime`

Source: `native/motorola68000/amigaos/vm/tkvm/runtime.asm`

Entry: `tkvmRun68000`. Routines: 1. Imports: 4. Sections: `code`.

Routines: `tkvmRun68000`.

Imports: `opforge.vm.tkvm.state`, `opforge.vm.tkvm.char_predicates`, `opforge.vm.tkvm.scanner`, `opforge.debug.runtime_profile`.

## `opforge.vm.encoding.runtime`

Source: `native/motorola68000/amigaos/vm/encoding/runtime.asm`

Entry: `semantic`. Routines: 21. Imports: 1. Sections: `data`, `code`.

Routines: `semantic`, `branchProgram`, `runBranch`, `fitsSignedWidth`, `runFixup`, `observeFixupField`, `applyTransform`, `validateAlignment`, `readI64`, `requireI32`, `loadFixupInput`, `recordFixup`, `normalizeFixupLength`, `validateFixupRange`, `runEncoding`, `requireBytes`, `readU32`, `validateUnit`, `emitUnit`, `loadInput`, `table`.

Imports: `opforge.debug.runtime_profile`.

## `opforge.vm.value.runtime`

Source: `native/motorola68000/amigaos/vm/value/runtime.asm`

Entry: `execute`. Routines: 2. Imports: 1. Sections: `code`.

Routines: `execute`, `readI64Le`.

Imports: `opforge.debug.runtime_profile`.

## `opforge.cli.entry`

Source: `native/motorola68000/amigaos/cli/entry.asm`

Entry: `start`. Routines: 2. Imports: 4. Sections: `code`, `data`, `bss`.

Routines: `start`, `closeDos`.

Imports: `opforge.assembler.driver`, `opforge.cli.arguments`, `opforge.cli.input`, `opforge.cli.target`.

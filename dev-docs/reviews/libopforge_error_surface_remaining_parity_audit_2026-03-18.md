# libopforge facade error-surface remaining parity audit - 2026-03-18

## Scope

This artifact completes Item 2 of `dev-docs/NextSteps/libopforge_rust_facade_error_surface_coverage_plan_2026-03-18_rev1.md` after Item 1 landed commit `0d3bed25d3225bbd21b59c3e254d05f883c8fbb0`.

Touched public surfaces audited here:

- `libopforge::opcore::CoreError`
- `libopforge::opcore::CoreErrorKind`
- `libopforge::processing::ProcessorError`
- `libopforge::processing::ProcessorErrorKind`
- `libopforge::processing::ProcessorFailureDetail`
- `libopforge::asm::AssemblerWorkflowError`
- `libopforge::diagnostics::AsmRunError` as the payload carried by `AssemblerWorkflowError::Assemble`

## Remaining path inventory after Item 1

### Core-domain and processing-domain routing

- `libopforge::processing::editor_route_line_with_model(...)` reaches `engine::editor_route_line_with_model(...)` and can surface routed core failures as public `processing::EngineError::Core`.
- Engine-side statement routing can execute in `Rust`, `Vm`, or `Lockstep` mode through `editor_route_line_with_model_in_mode(...)`.
- Lockstep comparison is implemented for opcore expression processing and for opasm statement processing reached through statement routing.

### Module-item routing

- `libopforge::processing::route_module_item_line_with_model(...)` reaches engine-side `route_module_item_line_with_model(...)`.
- That engine path directly calls `process_module_item_request_vm(...)` and does not have a parallel `Rust` or `Lockstep` execution mode.
- Because of that implementation shape, module-item routing currently has VM-path coverage but no truthful lockstep parity path to exercise.

### High-level workflow mapping

- High-level borrowed and owned assembly entry points return `AssemblerWorkflowError`.
- After Item 1, the suite covers real failed assembly on default or VM paths and on lockstep paths.
- Core-to-workflow and processor-to-workflow mappings for the remaining categories are still partly exercised through dedicated helper-based tests rather than by live injected runtime paths.

## Coverage inventory after Item 1

Direct public-contract coverage already present:

- `public_opcore_core_error_classifies_leaf_failures`
- `public_opcore_module_item_errors_classify_module_use_and_import_failures`
- `public_opcore_core_error_classifies_macro_conditional_and_repetition_failures`
- `public_opcore_core_error_classifies_namespace_scope_and_preprocess_failures`
- `public_opcore_core_error_classifies_struct_and_segment_failures`
- `public_processing_api_routes_core_failures_through_core_error`
- `public_processing_api_routes_module_item_failures_through_specific_core_error`
- `public_processing_processor_error_surface_exposes_stable_inspection`
- `public_asm_run_error_exposes_stable_inspection_and_workflow_conversion`
- `public_borrowed_asm_workflow_wraps_failed_assembly_path`
- `public_owned_asm_workflow_wraps_failed_assembly_path`
- `public_asm_workflow_invalid_argument_category_survives_borrowed_and_owned_paths`
- `public_asm_workflow_io_category_survives_borrowed_and_owned_paths`
- `public_asm_workflow_minimal_core_and_processor_mappings_are_stable`
- `public_asm_workflow_preserves_diagnostic_error_payload_structure`
- `public_asm_workflow_lockstep_failed_assembly_preserves_assemble_category`

Lockstep-specific coverage already present:

- `public_api_supports_explicit_lockstep_execution_mode`
- `public_free_prepare_preserves_explicit_execution_mode_for_reuse`
- `public_portable_opasm_processor_supports_lockstep_processing`
- `public_asm_workflow_lockstep_failed_assembly_preserves_assemble_category`

## Adequacy conclusion

### Adequate in the current narrowed scope

- The touched core-domain classification surface is directly covered for the listed public concern families.
- The touched processor-domain inspection surface is directly covered.
- The touched workflow-domain surface now has both success-path lockstep coverage and failed-assembly lockstep coverage on the live high-level path.
- No remaining touched-surface blind spot is small enough to justify another narrow regression without first adding new runtime capability or dedicated injection seams.

### Remaining gaps that are real but broader than this slice

- Module-item lockstep parity is not currently testable on the live public path because the engine only exposes VM execution for `route_module_item_line_with_model(...)`.
- `ProcessorErrorKind::Internal` and `ProcessorErrorKind::ProcessorDiagnostic` still reach high-level workflow mapping coverage mainly through helper-based tests. There is no narrow live-path harness today to inject those outcomes through real VM or lockstep assembly execution without adding new seams.
- Those gaps are broader VM-parity and harness-shape questions, not small regressions on the currently shipped touched public path.

## Required further regression additions

None for Item 2 within the current narrowed scope.

The remaining gaps identified above should be handled only if a later dedicated VM parity plan decides to add:

- a truthful lockstep-capable module-item routing path, or
- a dedicated live-path harness for processor-internal and processor-diagnostic workflow injection

## Deferred broader parity debt

This audit finds broader VM parity debt beyond the narrowed touched-surface scope.

That broader debt should be deferred out of this item and captured as a separate dedicated VM parity plan covering:

- whether module-item routing should gain a Rust or lockstep parity path
- whether live-path workflow harnesses are needed for `ProcessorErrorKind::Internal` and `ProcessorErrorKind::ProcessorDiagnostic`
- whether any additional lockstep parity expectations belong below the touched public facade error surfaces audited here

## Validation evidence

Validation executed for this audit slice:

- `cargo test --locked -p libopforge public_processing_api_routes_core_failures_through_core_error`
- `cargo test --locked -p libopforge public_processing_api_routes_module_item_failures_through_specific_core_error`
- `cargo test --locked -p libopforge public_asm_workflow_minimal_core_and_processor_mappings_are_stable`
- `cargo test --locked -p libopforge public_asm_workflow_lockstep_failed_assembly_preserves_assemble_category`
- `cargo test --locked -p libopforge`

Full quality gates remain required before the Item 2 commit boundary:

- `cargo fmt --all`
- `cargo clippy --workspace -- -D warnings`
- `cargo audit`
- `cargo test --locked --workspace`

All four full-gate commands completed successfully in the current working tree while this audit artifact was present.

## Conclusion

Item 1 closed the only remaining live-path parity gap that was narrow enough to justify a regression without new runtime capability. After that change, the touched public error surfaces are adequately covered for this narrowed follow-up. The remaining parity gaps are real, but they depend on broader VM-path or test-harness capability decisions and should be deferred into a separate dedicated VM parity plan rather than expanded inside Item 2.
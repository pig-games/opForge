# libopforge facade error-surface parity item 1 blind-spot note - 2026-03-18

## Scope

This artifact records the concrete Item 1 blind spot selected from `dev-docs/NextSteps/libopforge_rust_facade_error_surface_coverage_plan_2026-03-18_rev1.md` and the focused regression that closes it in the current working tree.

Touched public surfaces in scope:

- `libopforge::opcore::CoreError`
- `libopforge::opcore::CoreErrorKind`
- `libopforge::processing::ProcessorError`
- `libopforge::processing::ProcessorErrorKind`
- `libopforge::processing::ProcessorFailureDetail`
- `libopforge::asm::AssemblerWorkflowError`
- `libopforge::diagnostics::AsmRunError` as the payload of `AssemblerWorkflowError::Assemble`

## Path note

### Processing boundary

- `libopforge::processing::editor_route_line(...)` and `route_module_item_line(...)` in `crates/opforge-lib/src/lib.rs` forward to the engine layer and map engine failures into the public `processing::EngineError` surface.
- Engine-side classification lives in `crates/opforge-engine/src/processing.rs`.
- `editor_route_line_with_model_in_mode(...)` can execute in `Rust`, `Vm`, or `Lockstep` mode, but lockstep comparison is currently limited to opcore expression requests and opasm statement processing.
- `route_module_item_line_with_model(...)` is VM-only today. It uses `process_module_item_request_vm(...)` directly and has no lockstep parity path.

### High-level assembler workflow boundary

- High-level borrowed and owned assembly entry points return `AssemblerWorkflowError` from `crates/opforge-lib/src/lib.rs`.
- Workflow mapping helpers in that file normalize core, processor, and assembler failures into the public workflow taxonomy:
  - `map_core_error_to_workflow(...)`
  - `map_processor_error_to_workflow(...)`
  - `map_asm_run_error_to_workflow(...)`
- `AsmRunError` remains the structured diagnostic payload carried by `AssemblerWorkflowError::Assemble`.

## Existing coverage before this slice

The shipped `libopforge` test surface already covered most of the named public contracts directly:

- `public_opcore_core_error_classifies_leaf_failures`
- `public_opcore_module_item_errors_classify_module_use_and_import_failures`
- `public_opcore_core_error_classifies_macro_conditional_and_repetition_failures`
- `public_opcore_core_error_classifies_namespace_scope_and_preprocess_failures`
- `public_opcore_core_error_classifies_struct_and_segment_failures`
- `public_processing_processor_error_surface_exposes_stable_inspection`
- `public_processing_api_default_helpers_split_core_and_processor_runtime_model_contracts`
- `public_processing_api_routes_core_failures_through_core_error`
- `public_processing_api_routes_module_item_failures_through_specific_core_error`
- `public_asm_run_error_exposes_stable_inspection_and_workflow_conversion`
- `public_borrowed_asm_workflow_wraps_failed_assembly_path`
- `public_owned_asm_workflow_wraps_failed_assembly_path`
- `public_asm_workflow_invalid_argument_category_survives_borrowed_and_owned_paths`
- `public_asm_workflow_io_category_survives_borrowed_and_owned_paths`
- `public_asm_workflow_minimal_core_and_processor_mappings_are_stable`
- `public_asm_workflow_preserves_diagnostic_error_payload_structure`

The same suite also already proved that lockstep is alive on success paths:

- `public_api_supports_explicit_lockstep_execution_mode`
- `public_free_prepare_preserves_explicit_execution_mode_for_reuse`
- `public_portable_opasm_processor_supports_lockstep_processing`

## Blind spot selected for Item 1

The missing live-path regression was not the public error-type shape itself. It was the absence of any end-to-end high-level failed-assembly assertion in `ExecutionMode::Lockstep`.

Before this slice:

- `AssemblerWorkflowError::Assemble` was covered on borrowed and owned high-level paths, but only through the default or VM-oriented execution flow.
- Lockstep was covered only on successful assembly or per-statement processing paths.
- The suite therefore did not prove that a real high-level failed assembly still lands in `AssemblerWorkflowError::Assemble` with an `AsmRunError` payload when the workflow is executed through lockstep continuation selection.

Landed regression:

- `public_asm_workflow_lockstep_failed_assembly_preserves_assemble_category`

## Traceability

- Active plan item: Item 1 in `dev-docs/NextSteps/libopforge_rust_facade_error_surface_coverage_plan_2026-03-18_rev1.md`
- Requirement IDs closed by this slice: `FUP-VM-001`, `FUP-VM-002`, `FUP-VM-003`
- Relevant rev7 validation expectation: high-level workflow failures must preserve `AssemblerWorkflowError` category behavior while `AsmRunError` remains the structured failure payload carried by `AssemblerWorkflowError::Assemble`.
- Implemented regression location: `crates/opforge-lib/src/lib.rs`, test `public_asm_workflow_lockstep_failed_assembly_preserves_assemble_category`

What it proves:

- the borrowed high-level workflow still returns `AssemblerWorkflowErrorKind::Assemble` in lockstep mode
- the owned high-level workflow still returns `AssemblerWorkflowErrorKind::Assemble` in lockstep mode
- both lockstep continuation heads (`Rust` and `Vm`) preserve the `AsmRunError` payload shape rather than reclassifying the failure

## Out-of-scope observations for this slice

- `route_module_item_line_with_model(...)` is still VM-only in the engine layer, so broader module-item lockstep parity is not something this Item 1 regression can truthfully close.
- Some workflow mappings such as `ProcessorErrorKind::Internal` and `ProcessorErrorKind::ProcessorDiagnostic` still rely on helper-based coverage rather than a live injected high-level path. That is a later audit question, not part of this first regression slice.

## Validation evidence

Validation executed for this slice:

- `cargo test --locked -p libopforge public_asm_workflow_lockstep_failed_assembly_preserves_assemble_category`
- `cargo test --locked -p libopforge`
- `cargo fmt --all`
- `cargo clippy --workspace -- -D warnings`
- `cargo audit`
- `cargo test --locked --workspace`

All six commands completed successfully in the current working tree while this Item 1 regression and supporting blind-spot note were present.

## Slice result

Item 1 now has one concrete live-path parity regression in the working tree. This note intentionally stops at that boundary so later audit, traceability, and broader VM-parity decisions can remain separate plan items.
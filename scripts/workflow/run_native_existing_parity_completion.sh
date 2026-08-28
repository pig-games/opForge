#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/run_native_existing_parity_completion.sh --verify
  scripts/workflow/run_native_existing_parity_completion.sh --check-config

Runs every established fail-closed native CLI Level D parity group required by
the runtime-stabilization plan. The command accepts a staged worktree and
writes no retained receipt; source-identity receipts remain owned by their
feature-specific completion wrappers.
EOF
}

mode="${1:-}"
[[ $# -eq 1 && ( "${mode}" == "--verify" || "${mode}" == "--check-config" ) ]] || {
  usage >&2
  exit 2
}

required_env=(OPFORGE_FS_UAE_SMOKE OPFORGE_FS_UAE_BIN OPFORGE_FS_UAE_CONFIG_TEMPLATE OPFORGE_FS_UAE_ARGS)
for name in "${required_env[@]}"; do
  [[ -n "${!name:-}" ]] || { echo "error: ${name} is required for fail-closed native parity completion" >&2; exit 2; }
done
[[ "${OPFORGE_FS_UAE_SMOKE}" == "1" ]] || { echo "error: OPFORGE_FS_UAE_SMOKE must be 1" >&2; exit 2; }
[[ -x "${OPFORGE_FS_UAE_BIN}" ]] || { echo "error: FS-UAE binary is not executable: ${OPFORGE_FS_UAE_BIN}" >&2; exit 2; }
[[ -f "${OPFORGE_FS_UAE_CONFIG_TEMPLATE}" ]] || { echo "error: FS-UAE config template is missing: ${OPFORGE_FS_UAE_CONFIG_TEMPLATE}" >&2; exit 2; }
if [[ "${mode}" == "--check-config" ]]; then
  echo "PASS: fail-closed established native parity configuration is present"
  exit 0
fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../.." && pwd)"
cd "${repo_root}"

tests=(
  external_fs_uae_opforge_native_cli_schema_binary_parity_matches_live_rust_cli
  external_fs_uae_opforge_native_cli_schema_listing_parity_matches_live_rust_cli
  external_fs_uae_opforge_native_cli_schema_diagnostic_parity_matches_live_rust_cli
  external_fs_uae_opforge_native_cli_expression_metadata_fallback_matches_live_rust_cli
  external_fs_uae_opforge_native_cli_source_cpu_normalization_matches_live_rust_cli
  external_fs_uae_opforge_native_cli_debug_output_isolation_preserves_normal_output
  external_fs_uae_opforge_native_cli_65c02_expr_syntax_matches_rust_bin
  native_column_one_directive_routing_fs_uae
  native_opcore_counted_for_fs_uae
  native_opcore_sequence_assignment_fs_uae
  native_opcore_iterable_for_fs_uae
  native_opcore_while_fs_uae
  native_opcore_conditionals_fs_uae
  native_opcore_scopes_fs_uae
  native_macro_invocation_fixture_fs_uae
  # The combined canonical module/macro/statement test is intentionally not
  # established parity yet: parent-plan Items 7.4-7.7 remain open, beginning
  # with the INLINE .segment form in macro_syntax.asm.
  native_macro_preprocessor_harness_fs_uae_proves_capture_lookup_and_nested_frame_rejection
  external_fs_uae_native_cli_directive_router_emits_org_and_data_fixture
  external_fs_uae_native_cli_flow_navigation_preserves_nested_structural_skips
  native_module_local_symbol_fs_uae
  native_opcore_text_encoding_fs_uae
  native_text_encoding_definition_steps_fs_uae
  native_opcore_structs_fs_uae
  native_expression_context_forward_label_fs_uae
  native_expression_suffix_literals_fs_uae
  native_expression_multiplicative_fs_uae
  native_expression_shift_fs_uae
  external_fs_uae_opforge_native_cli_item7_layout_directives_match_rust_guided_bytes
  native_pipeline_select_harness_fs_uae_proves_embedded_65c02_selection
  external_fs_uae_tkpkg_native_motorola68000_family_corpus_matches_vm_authoritative_rows
  external_fs_uae_tkpkg_native_operator_surface_matches_vm_authoritative_rows
  external_fs_uae_tkpkg_native_percent_prefix_context_matches_vm_authoritative_rows
  external_fs_uae_tkpkg_native_mos6502_family_corpus_matches_vm_authoritative_rows
  external_fs_uae_tkpkg_native_intel8080_family_corpus_matches_vm_authoritative_rows
  external_fs_uae_tkpkg_native_motorola6800_family_corpus_matches_vm_authoritative_rows
  external_fs_uae_native_m68000_move_control_parity
  external_fs_uae_native_m68000_remaining_base_parity
  external_fs_uae_native_m68010_delta_parity
  external_fs_uae_native_m68020_full_extension_addressing_parity
  external_fs_uae_native_m68020_later_integer_group_a_parity
  external_fs_uae_native_m68020_later_integer_group_b_parity
  external_fs_uae_native_m68k_runtime_directive_state_parity
  external_fs_uae_native_m68k_runtime_directive_state_switch_parity
  external_fs_uae_native_m68k_runtime_directive_state_illegal_pair_rejection
  external_fs_uae_native_m68030_m68040_integer_mmu_parity
  external_fs_uae_native_m68881_m68882_core_parity
  external_fs_uae_native_m68881_m68882_extended_math_parity
  external_fs_uae_native_m68040_integrated_fpu_parity
  external_fs_uae_native_m68080_integer_parity
  external_fs_uae_native_m68080_ammx_parity
  external_fs_uae_native_rust_package_composition_matrix_parity
)

test_source="crates/opforge-asm/src"
for test_name in "${tests[@]}"; do
  rg -q "^fn ${test_name}\\(\\)" "${test_source}" || {
    echo "error: required Level D test is missing: ${test_name}" >&2
    exit 2
  }
done

cargo_bin="${CARGO:-cargo}"
for test_name in "${tests[@]}"; do
  output_file="$(mktemp)"
  trap 'rm -f "${output_file}"' EXIT
  printf '==> Established native Level D parity: %s\n' "${test_name}"
  if ! "${cargo_bin}" test -p asm "${test_name}" -- --nocapture --test-threads=1 >"${output_file}" 2>&1; then
    cat "${output_file}"
    echo "error: required Level D test failed: ${test_name}" >&2
    exit 1
  fi
  cat "${output_file}"
  if rg -q '^SKIP:' "${output_file}" \
    || ! rg -q '^running 1 test$' "${output_file}" \
    || ! rg -q "test .*${test_name}" "${output_file}" \
    || ! rg -q '^test result: ok\. 1 passed; 0 failed;' "${output_file}"; then
    echo "error: required Level D test did not run and pass exactly once: ${test_name}" >&2
    exit 1
  fi
  rm -f "${output_file}"
  trap - EXIT
done

echo "PASS: complete established native Level D parity corpus verified (${#tests[@]} tests)"

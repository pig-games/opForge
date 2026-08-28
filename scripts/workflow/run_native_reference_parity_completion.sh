#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/run_native_reference_parity_completion.sh --verify
  scripts/workflow/run_native_reference_parity_completion.sh --check-config

Runs the mandatory fail-closed Level D completion suite for the active native
reference scope, including the complete top-level Motorola 68000 corpus. Every
exact test must run and pass once; missing configuration, skips, zero-test
filters, and failures make the command fail. All tests are attempted before
failures are reported.
EOF
}

mode="${1:-}"
[[ $# -eq 1 && ( "${mode}" == "--verify" || "${mode}" == "--check-config" ) ]] || {
  usage >&2
  exit 2
}

required_env=(OPFORGE_FS_UAE_SMOKE OPFORGE_FS_UAE_BIN OPFORGE_FS_UAE_CONFIG_TEMPLATE OPFORGE_FS_UAE_ARGS)
for name in "${required_env[@]}"; do
  [[ -n "${!name:-}" ]] || { echo "error: ${name} is required for fail-closed native reference parity completion" >&2; exit 2; }
done
[[ "${OPFORGE_FS_UAE_SMOKE}" == "1" ]] || { echo "error: OPFORGE_FS_UAE_SMOKE must be 1" >&2; exit 2; }
[[ -x "${OPFORGE_FS_UAE_BIN}" ]] || { echo "error: FS-UAE binary is not executable: ${OPFORGE_FS_UAE_BIN}" >&2; exit 2; }
[[ -f "${OPFORGE_FS_UAE_CONFIG_TEMPLATE}" ]] || { echo "error: FS-UAE config template is missing: ${OPFORGE_FS_UAE_CONFIG_TEMPLATE}" >&2; exit 2; }
if [[ "${mode}" == "--check-config" ]]; then
  echo "PASS: fail-closed native reference parity configuration is present"
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
  native_mos_forward_ref_stability_fs_uae
  native_reference_opcore_syntax_expression_fs_uae
  native_reference_opcore_module_macro_statement_fs_uae
  native_reference_opcore_layout_output_fs_uae
  native_reference_opcore_diagnostic_fs_uae
  external_fs_uae_native_motorola68000_complete_reference_parity
)

test_source="crates/opforge-asm/src"
for test_name in "${tests[@]}"; do
  rg -q "^fn ${test_name}\\(\\)" "${test_source}" || {
    echo "error: required active Level D test is missing: ${test_name}" >&2
    exit 2
  }
done

cargo_bin="${CARGO:-cargo}"
output_dir="$(mktemp -d)"
trap 'rm -rf "${output_dir}"' EXIT
failures=()

for test_name in "${tests[@]}"; do
  output_file="${output_dir}/${test_name}.log"
  printf '==> Active native reference Level D parity: %s\n' "${test_name}"
  cargo_status=0
  "${cargo_bin}" test -p asm "${test_name}" -- --nocapture --test-threads=1 >"${output_file}" 2>&1 || cargo_status=$?
  cat "${output_file}"
  if [[ ${cargo_status} -ne 0 ]] \
    || rg -q 'SKIP:' "${output_file}" \
    || ! rg -q '^running 1 test$' "${output_file}" \
    || ! rg -q "test .*${test_name} .*ok" "${output_file}" \
    || ! rg -q '^test result: ok\. 1 passed; 0 failed;' "${output_file}"; then
    failures+=("${test_name}")
    echo "error: required active Level D test did not run and pass exactly once: ${test_name}" >&2
  fi
done

if [[ ${#failures[@]} -ne 0 ]]; then
  printf 'error: native reference parity completion failed for %d/%d tests:' "${#failures[@]}" "${#tests[@]}" >&2
  printf ' %s' "${failures[@]}" >&2
  printf '\n' >&2
  exit 1
fi

echo "PASS: complete active native reference Level D parity verified (${#tests[@]} tests)"

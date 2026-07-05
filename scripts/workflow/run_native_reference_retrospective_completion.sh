#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/run_native_reference_retrospective_completion.sh [--check-config]

Runs the fail-closed Level D completion evidence for native reference parity
Items 1-3. Unlike optional FS-UAE tests, this command rejects missing emulator
configuration instead of accepting a skipped result.
EOF
}

check_only=0
if [[ "${1:-}" == "--check-config" ]]; then
  check_only=1
  shift
elif [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  usage
  exit 0
fi
if [[ $# -ne 0 ]]; then
  usage >&2
  exit 2
fi

required_env=(
  OPFORGE_FS_UAE_SMOKE
  OPFORGE_FS_UAE_BIN
  OPFORGE_FS_UAE_CONFIG_TEMPLATE
  OPFORGE_FS_UAE_ARGS
)
for name in "${required_env[@]}"; do
  if [[ -z "${!name:-}" ]]; then
    echo "error: ${name} is required for fail-closed native completion" >&2
    exit 2
  fi
done
if [[ "${OPFORGE_FS_UAE_SMOKE}" != "1" ]]; then
  echo "error: OPFORGE_FS_UAE_SMOKE must be 1 for fail-closed native completion" >&2
  exit 2
fi
if [[ ! -x "${OPFORGE_FS_UAE_BIN}" ]]; then
  echo "error: FS-UAE binary is not executable: ${OPFORGE_FS_UAE_BIN}" >&2
  exit 2
fi
if [[ ! -f "${OPFORGE_FS_UAE_CONFIG_TEMPLATE}" ]]; then
  echo "error: FS-UAE config template is missing: ${OPFORGE_FS_UAE_CONFIG_TEMPLATE}" >&2
  exit 2
fi
if [[ ${check_only} -eq 1 ]]; then
  echo "PASS: fail-closed native completion configuration is present"
  exit 0
fi

tests=(
  external_fs_uae_opforge_native_cli_schema_binary_parity_matches_live_rust_cli
  external_fs_uae_opforge_native_cli_schema_listing_parity_matches_live_rust_cli
  external_fs_uae_opforge_native_cli_schema_diagnostic_parity_matches_live_rust_cli
  external_fs_uae_opforge_native_cli_expression_metadata_fallback_matches_live_rust_cli
  external_fs_uae_opforge_native_cli_source_cpu_normalization_matches_live_rust_cli
  external_fs_uae_opforge_native_cli_debug_output_isolation_preserves_normal_output
)
for test_name in "${tests[@]}"; do
  printf '==> Level D completion: %s\n' "${test_name}"
  cargo test -p asm "${test_name}" -- --nocapture --test-threads=1
done

echo "PASS: native reference retrospective Level D completion"

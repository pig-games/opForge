#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/run_native_macro_completion.sh --verify
  scripts/workflow/run_native_macro_completion.sh --manifest <path>
  scripts/workflow/run_native_macro_completion.sh --check-config

Runs the fail-closed Level D completion evidence for the native macro
preprocessor slice. --verify accepts a staged worktree and writes nothing.
--manifest requires a clean worktree and writes a source-identity receipt.
EOF
}

mode=""
manifest_path=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --verify|--check-config)
      [[ -z "${mode}" ]] || { usage >&2; exit 2; }
      mode="$1"
      shift
      ;;
    --manifest)
      [[ -z "${mode}" && $# -ge 2 ]] || { usage >&2; exit 2; }
      mode="$1"
      manifest_path="$2"
      shift 2
      ;;
    --help|-h) usage; exit 0 ;;
    *) usage >&2; exit 2 ;;
  esac
done
[[ -n "${mode}" ]] || { usage >&2; exit 2; }

required_env=(OPFORGE_FS_UAE_SMOKE OPFORGE_FS_UAE_BIN OPFORGE_FS_UAE_CONFIG_TEMPLATE OPFORGE_FS_UAE_ARGS)
for name in "${required_env[@]}"; do
  [[ -n "${!name:-}" ]] || { echo "error: ${name} is required for fail-closed native macro completion" >&2; exit 2; }
done
[[ "${OPFORGE_FS_UAE_SMOKE}" == "1" ]] || { echo "error: OPFORGE_FS_UAE_SMOKE must be 1 for fail-closed native macro completion" >&2; exit 2; }
[[ -x "${OPFORGE_FS_UAE_BIN}" ]] || { echo "error: FS-UAE binary is not executable: ${OPFORGE_FS_UAE_BIN}" >&2; exit 2; }
[[ -f "${OPFORGE_FS_UAE_CONFIG_TEMPLATE}" ]] || { echo "error: FS-UAE config template is missing: ${OPFORGE_FS_UAE_CONFIG_TEMPLATE}" >&2; exit 2; }
if [[ "${mode}" == "--check-config" ]]; then
  echo "PASS: fail-closed native macro completion configuration is present"
  exit 0
fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../.." && pwd)"
cd "${repo_root}"
if [[ "${mode}" == "--manifest" && -n "$(git status --porcelain)" ]]; then
  echo "error: --manifest requires a clean worktree for retained source identity" >&2
  exit 2
fi

tests=(
  native_macro_invocation_fixture_fs_uae
  native_macro_preprocessor_harness_fs_uae_proves_capture_lookup_and_nested_frame_rejection
)
test_source="crates/opforge-asm/src/tests"
for test_name in "${tests[@]}"; do
  rg -q "^fn ${test_name}\\(\\)" "${test_source}" || { echo "error: required Level D test is missing: ${test_name}" >&2; exit 2; }
done

source_commit="$(git rev-parse HEAD)"
source_tree="$(git rev-parse HEAD^{tree})"
timestamp="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
cargo_bin="${CARGO:-cargo}"
for test_name in "${tests[@]}"; do
  output_file="$(mktemp)"
  trap 'rm -f "${output_file}"' EXIT
  printf '==> Native macro Level D completion: %s\n' "${test_name}"
  if ! "${cargo_bin}" test -p asm "${test_name}" -- --nocapture --test-threads=1 >"${output_file}" 2>&1; then
    cat "${output_file}"
    echo "error: required Level D test failed: ${test_name}" >&2
    exit 1
  fi
  cat "${output_file}"
  if rg -q '^SKIP:' "${output_file}" || ! rg -q '^running 1 test$' "${output_file}" || ! rg -q "test .*${test_name} .*ok" "${output_file}" || ! rg -q '^test result: ok\. 1 passed; 0 failed;' "${output_file}"; then
    echo "error: required Level D test did not run and pass exactly once: ${test_name}" >&2
    exit 1
  fi
  rm -f "${output_file}"
  trap - EXIT
done

if [[ "${mode}" == "--verify" ]]; then
  echo "PASS: native macro Level D completion verified"
  exit 0
fi

manifest_dir="$(dirname "${manifest_path}")"
mkdir -p "${manifest_dir}"
manifest_tmp="${manifest_path}.tmp"
{
  printf '{\n  "manifest_version": 1,\n  "kind": "native-macro-preprocessor-level-d",\n'
  printf '  "scope": "macro-substitution-reentry",\n  "status": "PASS",\n'
  printf '  "completed_at_utc": "%s",\n' "${timestamp}"
  printf '  "source": {"commit": "%s", "tree": "%s"},\n  "tests": [\n' "${source_commit}" "${source_tree}"
  for index in "${!tests[@]}"; do
    comma=','; [[ ${index} -eq $((${#tests[@]} - 1)) ]] && comma=''
    printf '    {"name": "%s", "command": "cargo test -p asm %s -- --nocapture --test-threads=1", "result": "PASS"}%s\n' "${tests[${index}]}" "${tests[${index}]}" "${comma}"
  done
  printf '  ]\n}\n'
} >"${manifest_tmp}"
mv "${manifest_tmp}" "${manifest_path}"
echo "PASS: native macro Level D completion recorded at ${manifest_path}"

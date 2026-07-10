#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/run_native_cli_expansion_completion.sh --manifest <path>
  scripts/workflow/run_native_cli_expansion_completion.sh --check-config

Runs the fail-closed Level D completion evidence for native CLI expansion
Items 5.1–5.6. Every configured FS-UAE test must complete successfully; a
skipped test, dirty worktree, missing test, or missing emulator configuration
fails the command. On success, --manifest writes a JSON receipt identifying
the tested HEAD commit/tree, exact commands, tests, results, and UTC time.
EOF
}

manifest_path=""
check_only=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --manifest)
      [[ $# -ge 2 ]] || { usage >&2; exit 2; }
      manifest_path="$2"
      shift 2
      ;;
    --check-config)
      check_only=1
      shift
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      usage >&2
      exit 2
      ;;
  esac
done

if [[ ${check_only} -eq 1 && -n "${manifest_path}" ]]; then
  usage >&2
  exit 2
fi
if [[ ${check_only} -eq 0 && -z "${manifest_path}" ]]; then
  echo "error: --manifest is required for fail-closed native CLI expansion completion" >&2
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
  echo "PASS: fail-closed native CLI expansion completion configuration is present"
  exit 0
fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../.." && pwd)"
cd "${repo_root}"

if [[ -n "$(git status --porcelain)" ]]; then
  echo "error: fail-closed completion requires a clean worktree for retained source identity" >&2
  exit 2
fi

tests=(
  native_column_one_directive_routing_fs_uae
  native_opcore_counted_for_fs_uae
  native_opcore_sequence_assignment_fs_uae
  native_opcore_iterable_for_fs_uae
  native_opcore_while_fs_uae
  native_opcore_conditionals_fs_uae
  native_opcore_scopes_fs_uae
)
test_source="crates/opforge-asm/src/tests.rs"
for test_name in "${tests[@]}"; do
  if ! rg -q "^fn ${test_name}\(\)" "${test_source}"; then
    echo "error: required Level D test is missing: ${test_name}" >&2
    exit 2
  fi
done

source_commit="$(git rev-parse HEAD)"
source_tree="$(git rev-parse HEAD^{tree})"
timestamp="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
cargo_bin="${CARGO:-cargo}"

for test_name in "${tests[@]}"; do
  output_file="$(mktemp)"
  trap 'rm -f "${output_file}"' EXIT
  printf '==> Level D completion: %s\n' "${test_name}"
  if ! "${cargo_bin}" test -p asm "${test_name}" -- --nocapture --test-threads=1 \
    >"${output_file}" 2>&1; then
    cat "${output_file}"
    echo "error: required Level D test failed: ${test_name}" >&2
    exit 1
  fi
  cat "${output_file}"
  if rg -q '^SKIP:' "${output_file}"; then
    echo "error: required Level D test skipped: ${test_name}" >&2
    exit 1
  fi
  if ! rg -q '^running 1 test$' "${output_file}" \
    || ! rg -q "test tests::${test_name} .*ok" "${output_file}" \
    || ! rg -q '^test result: ok\. 1 passed; 0 failed;' "${output_file}"; then
    echo "error: required Level D test did not run and pass exactly once: ${test_name}" >&2
    exit 1
  fi
  rm -f "${output_file}"
  trap - EXIT
done

manifest_dir="$(dirname "${manifest_path}")"
mkdir -p "${manifest_dir}"
manifest_tmp="${manifest_path}.tmp"
{
  printf '{\n'
  printf '  "manifest_version": 1,\n'
  printf '  "kind": "native-cli-expansion-level-d",\n'
  printf '  "scope": "aggregate-baseline-items-5.1-to-5.6",\n'
  printf '  "status": "PASS",\n'
  printf '  "completed_at_utc": "%s",\n' "${timestamp}"
  printf '  "source": {"commit": "%s", "tree": "%s"},\n' "${source_commit}" "${source_tree}"
  printf '  "tests": [\n'
  for index in "${!tests[@]}"; do
    test_name="${tests[${index}]}"
    comma=","
    if [[ ${index} -eq $((${#tests[@]} - 1)) ]]; then
      comma=""
    fi
    printf '    {"name": "%s", "command": "cargo test -p asm %s -- --nocapture --test-threads=1", "result": "PASS"}%s\n' \
      "${test_name}" "${test_name}" "${comma}"
  done
  printf '  ]\n'
  printf '}\n'
} >"${manifest_tmp}"
mv "${manifest_tmp}" "${manifest_path}"

echo "PASS: native CLI expansion Level D completion recorded at ${manifest_path}"

#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/run_native_68000_ccr_cleanup_round.sh [--help]

Runs the explicit native Motorola 68000 CCR cleanup round:
  1. assembler/native-oriented baseline tests
  2. redundant tst cleanup in --write --explain mode
  3. native Motorola 68000 formatting
  4. the same assembler/native-oriented tests again

Normal quality gates remain non-mutating. Use this wrapper when you want to
apply safe cleanup and immediately validate whether source-shape tests broke.

Override the default test command with NATIVE_68000_TEST_CMD if needed.
EOF
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  usage
  exit 0
fi

if [[ $# -ne 0 ]]; then
  usage >&2
  exit 1
fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../.." && pwd)"
cd "${repo_root}"

default_test_cmd=(
  cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture
)

run_native_tests() {
  if [[ -n "${NATIVE_68000_TEST_CMD:-}" ]]; then
    bash -lc "${NATIVE_68000_TEST_CMD}"
  else
    "${default_test_cmd[@]}"
  fi
}

run_phase() {
  local phase="$1"
  shift
  printf '\n==> %s\n' "${phase}"
  if ! "$@"; then
    printf '\nFAIL: %s\n' "${phase}" >&2
    return 1
  fi
}

printf 'Using native Motorola 68000 baseline test command: '
if [[ -n "${NATIVE_68000_TEST_CMD:-}" ]]; then
  printf '%s\n' "${NATIVE_68000_TEST_CMD}"
else
  printf '%s\n' "${default_test_cmd[*]}"
fi

run_phase \
  "Baseline assembler/native tests (pre-cleanup)" \
  run_native_tests

run_phase \
  "Apply redundant native Motorola 68000 tst cleanup (--write --explain)" \
  python3 "${script_dir}/check_native_68000_redundant_tests.py" native/motorola68000 --write --explain

run_phase \
  "Run native Motorola 68000 formatter (--write)" \
  "${script_dir}/run_native_68000_format_gate.sh" --write

run_phase \
  "Assembler/native tests after cleanup" \
  run_native_tests

printf '\nPASS: native Motorola 68000 CCR cleanup round complete.\n'

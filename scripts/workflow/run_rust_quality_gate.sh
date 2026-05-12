#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/run_rust_quality_gate.sh [--help]

Runs the canonical Rust quality gate for opForge code changes:
  1. native Motorola 68000 formatter check
  2. cargo fmt --all
  3. cargo clippy -- -D warnings
  4. cargo audit
  5. C compiler availability check for FFI ABI coverage
  6. cargo test --locked

Use this single command when a plan, review, or implementation slice requires
the full Rust quality gate.
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

run_step() {
  local name="$1"
  shift
  printf '\n==> %s\n' "${name}"
  "$@"
}

require_c_compiler() {
  for compiler in cc clang gcc; do
    if command -v "${compiler}" >/dev/null 2>&1; then
      "${compiler}" --version
      return 0
    fi
  done

  echo "error: no supported C compiler found for FFI ABI contract coverage" >&2
  return 1
}

run_step "Run native Motorola 68000 formatter gate" "${script_dir}/run_native_68000_format_gate.sh"
run_step "Run Rust formatter" cargo fmt --all
run_step "Run Rust clippy" cargo clippy -- -D warnings
run_step "Run cargo audit" cargo audit
run_step "Require C compiler for FFI ABI coverage" require_c_compiler
run_step "Run locked Rust test suite" cargo test --locked

printf '\nPASS: Rust quality gate complete.\n'

#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/run_native_68000_format_gate.sh [--help] [--check|--write]

Runs the repository-native Motorola 68000 formatter pass against the supported
AmigaOS root sources using the shared repository formatter config:
  - default / --check: verify formatting without writing changes
  - --write: apply formatting changes in place

This wrapper owns the canonical module paths, root inputs, and formatter config
for native/motorola68000 assembly work in this repository.
EOF
}

mode="--fmt-check"
while [[ $# -gt 0 ]]; do
  case "$1" in
    --help|-h)
      usage
      exit 0
      ;;
    --check)
      mode="--fmt-check"
      shift
      ;;
    --write)
      mode="--fmt-write"
      shift
      ;;
    *)
      usage >&2
      exit 1
      ;;
  esac
done

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../.." && pwd)"
cd "${repo_root}"

config_path="${repo_root}/.opforgefmt.toml"
if [[ ! -f "${config_path}" ]]; then
  echo "error: missing formatter config: ${config_path}" >&2
  exit 1
fi

args=(
  --cpu 68020
  -M native/motorola68000/amigaos
  -I native/motorola68000/amigaos/debug
  -I native/motorola68000/amigaos/vm/tkvm
  -I native/motorola68000/amigaos/vm/encoding
  -i native/motorola68000/amigaos/main.asm
  -i native/motorola68000/amigaos/tests/prvm/prvm_debug_cli.asm
  -i native/motorola68000/amigaos/tests/prvm/prvm_line_iterator_smoke.asm
  -i native/motorola68000/amigaos/tests/prvm/prvm_smoke.asm
  -i native/motorola68000/amigaos/tests/tkvm/tokvm_interpreter.asm
  -i native/motorola68000/amigaos/tests/tkvm/tokvm_test_input.asm
  -i native/motorola68000/amigaos/tests/debug/debug_contract_harness.asm
  "${mode}"
  --fmt-config "${config_path}"
)

printf '==> Run native Motorola 68000 formatter gate (%s)\n' "${mode#--fmt-}"
cargo run -p cli --bin opforge -- "${args[@]}"

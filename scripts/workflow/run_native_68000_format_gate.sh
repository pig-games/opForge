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
  -M native/motorola68000/amigaos/opforge-cli
  -M native/motorola68000/amigaos/tkpkg
  -M native/motorola68000/amigaos/tkvm
  -M native/motorola68000/amigaos/test-harnesses/tkvm
  -M native/motorola68000/amigaos/prvm
  -M native/motorola68000/amigaos/exprvm
  -M native/motorola68000/amigaos/opcore
  -M native/motorola68000/amigaos/opasm
  -M native/motorola68000/amigaos/debug
  -I native/motorola68000/amigaos/debug
  -i native/motorola68000/amigaos/main.asm
  -i native/motorola68000/amigaos/test-harnesses/prvm/prvm_debug_cli.asm
  -i native/motorola68000/amigaos/test-harnesses/prvm/prvm_line_iterator_smoke.asm
  -i native/motorola68000/amigaos/test-harnesses/prvm/prvm_smoke.asm
  -i native/motorola68000/amigaos/test-harnesses/tkpkg/tkpkg_entry.asm
  -i native/motorola68000/amigaos/test-harnesses/tkvm/tokvm_interpreter.asm
  -i native/motorola68000/amigaos/test-harnesses/tkvm/tokvm_test_input.asm
  -i native/motorola68000/amigaos/test-harnesses/debug/debug_contract_harness.asm
  -i native/motorola68000/amigaos/test-harnesses/debug/cli_debug_event_harness.asm
  "${mode}"
  --fmt-config "${config_path}"
)

printf '==> Run native Motorola 68000 formatter gate (%s)\n' "${mode#--fmt-}"
cargo run -p cli --bin opforge -- "${args[@]}"

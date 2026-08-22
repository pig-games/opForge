#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/run_m68k_package_runtime_completion.sh [--require-clean]

Runs the Item 13 package-first completion proofs. The suite exercises the
freshly serialized 680x0 package for all six profiles, proves zero family/CPU/
dialect callbacks, assembles the complete checked-in 680x0 example corpus,
checks the Hunk symbol/fixup path, verifies 680x0-only, 65x02-only, combined,
and all-family composition, and verifies the embedded native package and its
capacity/overflow contract.

Use --require-clean for the retained final receipt after the Item 13 commit.
This package-phase gate does not run FS-UAE and changes no native CPU behavior.
EOF
}

require_clean=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --require-clean)
      require_clean=1
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

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../.." && pwd)"
cd "${repo_root}"

if [[ ${require_clean} -eq 1 && -n "$(git status --porcelain)" ]]; then
  echo "error: Item 13 retained completion requires a clean worktree" >&2
  exit 2
fi

cargo_bin="${CARGO:-cargo}"

run_test() {
  local package="$1"
  local test_name="$2"
  printf '==> Item 13 package completion: %s / %s\n' "${package}" "${test_name}"
  "${cargo_bin}" test -p "${package}" --lib "${test_name}" -- --nocapture
}

run_test families package_improvement_applicability_matrix_covers_every_registered_family_and_item
run_test vm serialized_m68k_scalar_register_programs_execute_for_all_six_profiles
run_test vm serialized_m68k_state_program_matches_all_six_cpu_profiles
run_test vm serialized_m68k_state_program_owns_transitions_legality_and_reset
run_test vm serialized_m68k_only_mos65x02_only_and_combined_packages_are_order_independent
run_test asm m68k_serialized_scalar_register_path_uses_zero_family_cpu_or_dialect_callbacks
run_test asm motorola68000_family_example_programs_assemble_in_reference_workflow
run_test asm m68k_package_runtime_assembles_flat_amiga_hunk_symbols
run_test asm motorola68020_embedded_native_cli_package_matches_rust_default_runtime_package

echo "PASS: Item 13 680x0 package-first runtime completion"

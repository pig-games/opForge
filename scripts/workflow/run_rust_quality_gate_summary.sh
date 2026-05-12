#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/run_rust_quality_gate_summary.sh [--help] [--tail-lines <n>]

Behavior:
  - Runs `scripts/workflow/run_rust_quality_gate.sh`.
  - Writes the full transcript to `target/workflow-logs/rust-quality-gate.log`.
  - Prints the final tail of the transcript plus the stable log path.
  - Exits with the same status as the underlying quality gate.

Use this wrapper when tool output retrieval is flaky or when an agent needs a
short pass/fail summary without improvising temp-log shell one-liners.
EOF
}

tail_lines=40
while [[ $# -gt 0 ]]; do
  case "$1" in
    --help|-h)
      usage
      exit 0
      ;;
    --tail-lines)
      if [[ $# -lt 2 ]]; then
        usage >&2
        exit 1
      fi
      tail_lines="$2"
      shift 2
      ;;
    *)
      usage >&2
      exit 1
      ;;
  esac
done

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../.." && pwd)"
log_dir="${repo_root}/target/workflow-logs"
log_path="${log_dir}/rust-quality-gate.log"
mkdir -p "${log_dir}"

set +e
"${script_dir}/run_rust_quality_gate.sh" >"${log_path}" 2>&1
gate_exit_code=$?
set -e

printf 'Rust quality gate log: %s\n' "${log_path}"
printf 'Rust quality gate tail (%s lines):\n' "${tail_lines}"
tail -n "${tail_lines}" "${log_path}" || true

if [[ ${gate_exit_code} -eq 0 ]]; then
  printf '\nPASS: Rust quality gate summary wrapper completed successfully.\n'
else
  printf '\nFAIL: Rust quality gate summary wrapper observed exit code %s.\n' "${gate_exit_code}" >&2
fi

exit "${gate_exit_code}"

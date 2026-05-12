#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/update_references.sh <allowed-reference-path> [more-paths...] -- <command> [args...]

Behavior:
  - Requires one or more explicit reference/golden paths to allow.
  - Runs the provided update command with `OPFORGE_UPDATE_REFERENCE=1`.
  - Fails if the command changes any reference/golden file outside the allowlist.

Example:
  scripts/workflow/update_references.sh \
    examples/reference/motorola68000/amigaos/opforge/opforge_cli.hunk \
    examples/reference/motorola68000/amigaos/opforge/opforge_cli.lst \
    -- cargo test -p asm examples_match_reference_outputs -- --nocapture
EOF
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  usage
  exit 0
fi

allowed_paths=()
command=()
seen_separator=0

while [[ $# -gt 0 ]]; do
  if [[ "$1" == "--" ]]; then
    seen_separator=1
    shift
    command=("$@")
    break
  fi
  allowed_paths+=("$1")
  shift
done

if [[ ${#allowed_paths[@]} -eq 0 || $seen_separator -ne 1 || ${#command[@]} -eq 0 ]]; then
  usage >&2
  exit 1
fi

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "${repo_root}"

normalize() {
  python3 - "$1" <<'PY'
from pathlib import Path
import sys
print(Path(sys.argv[1]).as_posix())
PY
}

allowed_norm=()
for path in "${allowed_paths[@]}"; do
  normalized="$(normalize "$path")"
  case "$normalized" in
    examples/reference/*|crates/opforge-asm/tests/goldens/*)
      allowed_norm+=("$normalized")
      ;;
    *)
      echo "FAIL: allowlist path is not a governed reference/golden path: $path" >&2
      exit 1
      ;;
  esac
done

OPFORGE_UPDATE_REFERENCE=1 "${command[@]}" >/dev/null 2>&1 && command_status=0 || command_status=$?
if [[ $command_status -ne 0 ]]; then
  echo "FAIL: reference update command failed with status $command_status" >&2
  exit "$command_status"
fi

changed_references=()
while IFS= read -r path; do
  [[ -n "$path" ]] || continue
  changed_references+=("$path")
done < <(
  git diff --name-only --diff-filter=ACMR |
    grep -E '^(examples/reference/|crates/opforge-asm/tests/goldens/)' || true
)

if [[ ${#changed_references[@]} -eq 0 ]]; then
  echo "PASS: no governed reference/golden outputs changed"
  exit 0
fi

for path in "${changed_references[@]}"; do
  normalized="$(normalize "$path")"
  is_allowed=0
  for allowed in "${allowed_norm[@]}"; do
    if [[ "$normalized" == "$allowed" ]]; then
      is_allowed=1
      break
    fi
  done
  if [[ $is_allowed -ne 1 ]]; then
    echo "FAIL: update changed reference/golden outside allowlist: $normalized" >&2
    exit 1
  fi
done

echo "PASS: reference update stayed within explicit allowlist (${#changed_references[@]} file(s))"

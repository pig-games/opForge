#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/stage_and_commit.sh --message "<commit message>" <path> [more-paths...]

Behavior:
  - Prints `git status --short`.
  - Stages only the explicit paths you provide.
  - Prints `git diff --cached --stat` and `git diff --cached --name-only`.
  - Creates the commit with the provided message.

Rules:
  - You must provide at least one explicit path.
  - The script never stages everything implicitly.
  - Use this wrapper instead of chaining `git status`, `git add`, and
    `git commit` in one ad hoc shell command.

Example:
  scripts/workflow/stage_and_commit.sh \
    --message "Persist native PRVM expression metadata" \
    native/motorola68000/amigaos/opasm/opasm_engine.asm \
    native/motorola68000/amigaos/opforge-cli/opforge_cli.asm \
    crates/opforge-asm/src/tests.rs
EOF
}

commit_message=""
paths=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --help|-h)
      usage
      exit 0
      ;;
    --message)
      if [[ $# -lt 2 ]]; then
        usage >&2
        exit 1
      fi
      commit_message="$2"
      shift 2
      ;;
    --)
      shift
      while [[ $# -gt 0 ]]; do
        paths+=("$1")
        shift
      done
      ;;
    *)
      paths+=("$1")
      shift
      ;;
  esac
done

if [[ -z "$commit_message" || ${#paths[@]} -eq 0 ]]; then
  usage >&2
  exit 1
fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../.." && pwd)"
cd "${repo_root}"

printf '==> Git status\n'
git status --short

printf '\n==> Staging explicit paths\n'
git add "${paths[@]}"

printf '\n==> Staged diff summary\n'
git diff --cached --stat

printf '\n==> Staged paths\n'
git diff --cached --name-only

printf '\n==> Commit\n'
git commit -m "${commit_message}"

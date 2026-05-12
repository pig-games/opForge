#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/archive_completed_plan.sh [--timestamp <UTC-stamp>] <plan-path>

Behavior:
  - Verifies the plan has at least one checkbox and that all checkboxes are complete.
  - Moves the plan into documentation/plans/completed/.
  - Appends a UTC completion timestamp to the archived filename:
      -completed-YYYY-MM-DDTHHMMSSZ.md
  - Moves the companion quality-gate sidecar if it exists.

Examples:
  scripts/workflow/archive_completed_plan.sh \
    documentation/plans/my-plan-v0_1.md

  scripts/workflow/archive_completed_plan.sh \
    --timestamp 2026-05-11T183500Z \
    documentation/plans/my-plan-v0_1.md
EOF
}

timestamp=""
positionals=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --help|-h)
      usage
      exit 0
      ;;
    --timestamp)
      if [[ $# -lt 2 ]]; then
        usage >&2
        exit 1
      fi
      timestamp="$2"
      shift 2
      ;;
    *)
      positionals+=("$1")
      shift
      ;;
  esac
done

if [[ ${#positionals[@]} -ne 1 ]]; then
  usage >&2
  exit 1
fi

plan_path="${positionals[0]}"

if [[ -z "$timestamp" ]]; then
  timestamp="$(date -u '+%Y-%m-%dT%H%M%SZ')"
fi

if [[ ! "$timestamp" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{6}Z$ ]]; then
  echo "FAIL: timestamp must match YYYY-MM-DDTHHMMSSZ" >&2
  exit 1
fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../.." && pwd)"
cd "${repo_root}"

if [[ ! -f "$plan_path" ]]; then
  echo "FAIL: plan file not found: $plan_path" >&2
  exit 1
fi

case "$plan_path" in
  documentation/plans/*.md|documentation/plans/*/*.md)
    ;;
  *)
    echo "FAIL: plan path must be under documentation/plans/: $plan_path" >&2
    exit 1
    ;;
esac

if [[ "$plan_path" == documentation/plans/completed/* ]]; then
  echo "FAIL: plan is already under documentation/plans/completed/: $plan_path" >&2
  exit 1
fi

python3 - "$plan_path" <<'PY'
import re
import sys
from pathlib import Path

path = Path(sys.argv[1])
text = path.read_text(encoding="utf-8")
checkboxes = re.findall(r"^\s*-\s\[( |x|X)\]\s+", text, flags=re.MULTILINE)
if not checkboxes:
    print(f"FAIL: no checkboxes found in {path}", file=sys.stderr)
    raise SystemExit(1)
if any(mark == " " for mark in checkboxes):
    print(f"FAIL: not all checkboxes are complete in {path}", file=sys.stderr)
    raise SystemExit(1)
PY

plan_name="$(basename "$plan_path")"
plan_stem="${plan_name%.md}"
archived_dir="documentation/plans/completed"
archived_path="${archived_dir}/${plan_stem}-completed-${timestamp}.md"

if [[ -e "$archived_path" ]]; then
  echo "FAIL: archived plan already exists: $archived_path" >&2
  exit 1
fi

mkdir -p "$archived_dir"

gate_path="${plan_path}.quality-gate.txt"
archived_gate_path="${archived_path}.quality-gate.txt"

printf 'Archiving plan:\n  %s\n-> %s\n' "$plan_path" "$archived_path"
mv "$plan_path" "$archived_path"

if [[ -f "$gate_path" ]]; then
  if [[ -e "$archived_gate_path" ]]; then
    echo "FAIL: archived quality-gate path already exists: $archived_gate_path" >&2
    exit 1
  fi
  printf 'Archiving quality gate:\n  %s\n-> %s\n' "$gate_path" "$archived_gate_path"
  mv "$gate_path" "$archived_gate_path"
fi

printf 'PASS: archived completed plan to %s\n' "$archived_path"
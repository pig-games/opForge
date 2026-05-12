#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/run_closure_workflow.sh [--check-once] [--root <root>] <closure-path> <finding-id> <plan-item> [review-path]

Behavior:
  - Creates the closure artifact from the template if it does not exist.
  - Prefills the finding ID, plan item, and optional original summary from a review artifact.
  - Validates the closure artifact bundle.
  - Requires a companion gate result file with PASS from finding-closure-reviewer.
  - Allows at most 3 failed re-check cycles, then halts and asks the user to resolve the blockage.

Companion gate file:
  <closure-path>.quality-gate.txt

Expected gate file contents:
  PASS: <short technical explanation>
  or
  status: PASS

Examples:
  scripts/workflow/run_closure_workflow.sh \
    documentation/finding-closures/my-closure.md \
    RVW-2026-05-10-001 \
    "Item 3" \
    documentation/reviews/my-review.md
EOF
}

check_once=0
bundle_root="."
positionals=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --help|-h)
      usage
      exit 0
      ;;
    --check-once|--non-interactive)
      check_once=1
      shift
      ;;
    --root)
      if [[ $# -lt 2 ]]; then
        usage >&2
        exit 1
      fi
      bundle_root="$2"
      shift 2
      ;;
    *)
      positionals+=("$1")
      shift
      ;;
  esac
done

if [[ ${#positionals[@]} -lt 3 || ${#positionals[@]} -gt 4 ]]; then
  usage >&2
  exit 1
fi

closure_path="${positionals[0]}"
finding_id="${positionals[1]}"
plan_item="${positionals[2]}"
review_path="${positionals[3]:-}"
gate_path="${closure_path}.quality-gate.txt"
max_attempts=3

if [[ ! -e "$closure_path" ]]; then
  bootstrap_args=(
    closure
    "$closure_path"
    --title "$(basename "${closure_path%.md}" | tr '-' ' ')"
    --finding-id "$finding_id"
    --plan-item "$plan_item"
  )
  if [[ -n "$review_path" ]]; then
    bootstrap_args+=(--review "$review_path")
  fi
  scripts/workflow/new_artifact_from_template.sh "${bootstrap_args[@]}"
fi

python3 scripts/workflow/stamp_workflow_provenance.py \
  "$closure_path" \
  --skill opforge-review-closure \
  --entrypoint run_closure_workflow.sh

if [[ ! -e "$gate_path" ]]; then
  python3 - <<'PY' "$closure_path" "$gate_path"
from pathlib import Path
import sys
sys.path.insert(0, str(Path("scripts/workflow").resolve()))
from workflow_common import render_gate_text

closure_path = Path(sys.argv[1])
gate_path = Path(sys.argv[2])
gate_path.write_text(
    render_gate_text(
        status="PENDING",
        gate="finding-closure",
        artifact=closure_path.as_posix(),
        summary="pending finding closure review",
        reviewer="finding-closure-reviewer",
    ),
    encoding="utf-8",
)
PY
fi

print_instructions() {
  cat <<EOF
Closure artifact: $closure_path
Finding ID: $finding_id
Plan item: $plan_item
Quality gate file: $gate_path

Required workflow:
1. Run the branch-local finding-closure-reviewer using:
   - AGENTS.md
   - $closure_path
   - finding ID: $finding_id
   - plan item: $plan_item
2. Save the reviewer result to:
   $gate_path
3. The gate file must report PASS.
EOF

  if [[ -n "$review_path" ]]; then
    echo "Review artifact for original summary: $review_path"
  fi

  cat <<'EOF'

The script will keep checking until:
- check_workflow_artifact_bundle.py passes for the closure
- the quality gate file reports PASS

The script allows at most 3 failed re-check cycles.
If that limit is reached, stop and ask the user how to resolve the blockage.

Press Enter after each closure/gate update to re-check, or use --check-once for one-shot validation.
EOF
}

check_gate_file() {
  python3 scripts/workflow/check_quality_gate_evidence.py "$gate_path"
}

print_instructions

attempt=0
while true; do
  closure_ok=0
  gate_ok=0

  if python3 scripts/workflow/check_workflow_artifact_bundle.py --root "$bundle_root" closure "$closure_path"; then
    closure_ok=1
  fi

  if check_gate_file; then
    gate_ok=1
  fi

  if [[ $closure_ok -eq 1 && $gate_ok -eq 1 ]]; then
    echo "PASS: closure workflow complete for $closure_path"
    exit 0
  fi

  attempt=$((attempt + 1))
  if [[ $attempt -ge $max_attempts ]]; then
    echo
    echo "FAIL: closure workflow reached the retry limit ($max_attempts failed re-check cycles)." >&2
    echo "Stop iterating and ask the user to resolve the blockage before continuing." >&2
    exit 1
  fi

  echo
  echo "Closure workflow not complete yet. Failed re-check cycle: $attempt/$max_attempts."
  if [[ $check_once -eq 1 ]]; then
    exit 1
  fi
  echo "Update the closure artifact and/or quality gate result, then press Enter to re-check."
  read -r _
done

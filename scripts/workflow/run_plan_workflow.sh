#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/run_plan_workflow.sh <plan-path> <plan-source-summary> [constraints]

Behavior:
  - Creates the plan artifact from the template if it does not exist.
  - Prints the exact branch-local plan workflow instructions.
  - Validates plan checkbox discipline.
  - Requires a companion gate result file with PASS from Plan Quality Orchestrator.
  - Allows at most 3 failed re-check cycles, then halts and asks the user to resolve the blockage.

Companion gate file:
  <plan-path>.quality-gate.txt

Expected gate file contents:
  PASS: <short technical explanation>

Examples:
  scripts/workflow/run_plan_workflow.sh \
    documentation/my-plan.md \
    "spec: documentation/my-spec.md" \
    "focus on slice size and source mapping"
EOF
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  usage
  exit 0
fi

if [[ $# -lt 2 || $# -gt 3 ]]; then
  usage >&2
  exit 1
fi

plan_path="$1"
plan_source_summary="$2"
constraints="${3:-}"
gate_path="${plan_path}.quality-gate.txt"
max_attempts=3

if [[ ! -e "$plan_path" ]]; then
  scripts/workflow/new_artifact_from_template.sh plan "$plan_path"
fi

print_instructions() {
  cat <<EOF
Plan artifact: $plan_path
Plan source summary: $plan_source_summary
Quality gate file: $gate_path

Required workflow:
1. Run the branch-local Plan Quality Orchestrator and review:
   - AGENTS.md
   - $plan_path
   - source: $plan_source_summary
   - templates/plan-template.md
   - references/workflow/plan-slice-rules.md
   - references/workflow/definition-of-done-matrix.md
2. Run the orchestrator in an environment that can launch its configured reviewer agents.
3. Save the final orchestrator result to:
   $gate_path
4. The gate file must begin with:
   PASS: <short technical explanation>
EOF

  if [[ -n "$constraints" ]]; then
    echo "Additional constraints: $constraints"
  fi

  cat <<'EOF'

The script will keep checking until:
- check_plan_checkboxes.py passes
- the quality gate file starts with PASS:

The script allows at most 3 failed re-check cycles.
If that limit is reached, stop and ask the user how to resolve the blockage.

Press Enter after each plan/gate update to re-check.
EOF
}

check_gate_file() {
  if [[ ! -f "$gate_path" ]]; then
    echo "FAIL: missing quality gate file: $gate_path" >&2
    return 1
  fi

  if ! grep -Eq '^PASS:' "$gate_path"; then
    echo "FAIL: quality gate file must begin with 'PASS:'" >&2
    echo "Current contents:" >&2
    sed -n '1,20p' "$gate_path" >&2
    return 1
  fi
}

print_instructions

attempt=0
while true; do
  plan_ok=0
  gate_ok=0

  if python3 scripts/workflow/check_plan_checkboxes.py "$plan_path"; then
    plan_ok=1
  fi

  if check_gate_file; then
    gate_ok=1
  fi

  if [[ $plan_ok -eq 1 && $gate_ok -eq 1 ]]; then
    echo "PASS: plan workflow complete for $plan_path"
    exit 0
  fi

  attempt=$((attempt + 1))
  if [[ $attempt -ge $max_attempts ]]; then
    echo
    echo "FAIL: plan workflow reached the retry limit ($max_attempts failed re-check cycles)." >&2
    echo "Stop iterating and ask the user to resolve the blockage before continuing." >&2
    exit 1
  fi

  echo
  echo "Plan workflow not complete yet. Failed re-check cycle: $attempt/$max_attempts."
  echo "Update the plan artifact and/or quality gate result, then press Enter to re-check."
  read -r _
done
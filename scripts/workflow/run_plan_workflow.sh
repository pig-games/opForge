#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/run_plan_workflow.sh [--check-once] [--mode <mode>] [--root <root>] <plan-path> <plan-source-summary> [constraints]

Behavior:
  - Creates the plan artifact from the template if it does not exist.
  - Prints the exact branch-local plan workflow instructions.
  - Validates the plan artifact bundle.
  - Requires a companion gate result file with PASS from Plan Quality Reviewer.
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

check_once=0
plan_mode="implementation"
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
    --mode)
      if [[ $# -lt 2 ]]; then
        usage >&2
        exit 1
      fi
      plan_mode="$2"
      shift 2
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

if [[ ${#positionals[@]} -lt 2 || ${#positionals[@]} -gt 3 ]]; then
  usage >&2
  exit 1
fi

plan_path="${positionals[0]}"
plan_source_summary="${positionals[1]}"
constraints="${positionals[2]:-}"
gate_path="${plan_path}.quality-gate.txt"
max_attempts=3

if [[ ! -e "$plan_path" ]]; then
  scripts/workflow/new_artifact_from_template.sh \
    plan \
    "$plan_path" \
    --title "$(basename "${plan_path%.md}" | tr '-' ' ')" \
    --source "$plan_source_summary" \
    --mode "$plan_mode"
fi

python3 scripts/workflow/stamp_workflow_provenance.py \
  "$plan_path" \
  --skill opforge-plan-authoring \
  --entrypoint run_plan_workflow.sh

print_instructions() {
  cat <<EOF
Plan artifact: $plan_path
Plan source summary: $plan_source_summary
Quality gate file: $gate_path

Required workflow:
1. Run the branch-local plan-quality-reviewer and review:
   - AGENTS.md
   - $plan_path
   - source: $plan_source_summary
   - templates/plan-template.md
   - references/workflow/plan-slice-rules.md
   - references/workflow/definition-of-done-matrix.md
2. Save the reviewer result to:
   $gate_path
3. The gate file must begin with:
   PASS: <short technical explanation>
EOF

  if [[ -n "$constraints" ]]; then
    echo "Additional constraints: $constraints"
  fi

  cat <<'EOF'

The script will keep checking until:
- check_workflow_artifact_bundle.py passes for the plan
- the quality gate file reports PASS

The script allows at most 3 failed re-check cycles.
If that limit is reached, stop and ask the user how to resolve the blockage.

Press Enter after each plan/gate update to re-check, or use --check-once for one-shot validation.
EOF
}

check_gate_file() {
  python3 scripts/workflow/check_quality_gate_evidence.py "$gate_path"
}

print_instructions

attempt=0
while true; do
  plan_ok=0
  gate_ok=0

  if python3 scripts/workflow/check_workflow_artifact_bundle.py --root "$bundle_root" plan "$plan_path"; then
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
  if [[ $check_once -eq 1 ]]; then
    exit 1
  fi
  echo "Update the plan artifact and/or quality gate result, then press Enter to re-check."
  read -r _
done

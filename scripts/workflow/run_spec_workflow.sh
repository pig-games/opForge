#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/run_spec_workflow.sh [--check-once] [--root <root>] <spec-path> [constraints]

Behavior:
  - Creates the spec artifact from the template if it does not exist.
  - Prints the exact branch-local spec workflow instructions.
  - Validates the spec artifact bundle.
  - Requires a companion gate result file with PASS from Spec Quality Orchestrator.
  - Allows at most 3 failed re-check cycles, then halts and asks the user to resolve the blockage.

Companion gate file:
  <spec-path>.quality-gate.txt

Expected gate file contents:
  PASS: <short technical explanation>

Examples:
  scripts/workflow/run_spec_workflow.sh \
    documentation/my-spec.md \
    "focus on acceptance criteria and boundary behavior"
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

if [[ ${#positionals[@]} -lt 1 || ${#positionals[@]} -gt 2 ]]; then
  usage >&2
  exit 1
fi

spec_path="${positionals[0]}"
constraints="${positionals[1]:-}"
gate_path="${spec_path}.quality-gate.txt"
max_attempts=3

if [[ ! -e "$spec_path" ]]; then
  scripts/workflow/new_artifact_from_template.sh spec "$spec_path" --title "$(basename "${spec_path%.md}" | tr '-' ' ')"
fi

python3 scripts/workflow/stamp_workflow_provenance.py \
  "$spec_path" \
  --skill opforge-spec-authoring \
  --entrypoint run_spec_workflow.sh

print_instructions() {
  cat <<EOF
Spec artifact: $spec_path
Quality gate file: $gate_path

Required workflow:
1. Run the branch-local Spec Quality Orchestrator and review:
   - AGENTS.md
   - $spec_path
   - templates/spec-template.md
   - references/workflow/spec-quality-checklist.md
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
- check_workflow_artifact_bundle.py passes for the spec
- the quality gate file reports PASS

The script allows at most 3 failed re-check cycles.
If that limit is reached, stop and ask the user how to resolve the blockage.

Press Enter after each spec/gate update to re-check, or use --check-once for one-shot validation.
EOF
}

check_gate_file() {
  python3 scripts/workflow/check_quality_gate_evidence.py "$gate_path"
}

print_instructions

attempt=0
while true; do
  spec_ok=0
  gate_ok=0

  if python3 scripts/workflow/check_workflow_artifact_bundle.py --root "$bundle_root" spec "$spec_path"; then
    spec_ok=1
  fi

  if check_gate_file; then
    gate_ok=1
  fi

  if [[ $spec_ok -eq 1 && $gate_ok -eq 1 ]]; then
    echo "PASS: spec workflow complete for $spec_path"
    exit 0
  fi

  attempt=$((attempt + 1))
  if [[ $attempt -ge $max_attempts ]]; then
    echo
    echo "FAIL: spec workflow reached the retry limit ($max_attempts failed re-check cycles)." >&2
    echo "Stop iterating and ask the user to resolve the blockage before continuing." >&2
    exit 1
  fi

  echo
  echo "Spec workflow not complete yet. Failed re-check cycle: $attempt/$max_attempts."
  if [[ $check_once -eq 1 ]]; then
    exit 1
  fi
  echo "Update the spec artifact and/or quality gate result, then press Enter to re-check."
  read -r _
done

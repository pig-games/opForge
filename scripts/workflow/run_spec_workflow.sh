#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/run_spec_workflow.sh <spec-path> [constraints]

Behavior:
  - Creates the spec artifact from the template if it does not exist.
  - Prints the exact branch-local spec workflow instructions.
  - Validates the spec artifact structure.
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

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  usage
  exit 0
fi

if [[ $# -lt 1 || $# -gt 2 ]]; then
  usage >&2
  exit 1
fi

spec_path="$1"
constraints="${2:-}"
gate_path="${spec_path}.quality-gate.txt"
max_attempts=3

if [[ ! -e "$spec_path" ]]; then
  scripts/workflow/new_artifact_from_template.sh spec "$spec_path"
fi

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
- check_spec_artifact.py passes
- the quality gate file starts with PASS:

The script allows at most 3 failed re-check cycles.
If that limit is reached, stop and ask the user how to resolve the blockage.

Press Enter after each spec/gate update to re-check.
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
  spec_ok=0
  gate_ok=0

  if python3 scripts/workflow/check_spec_artifact.py "$spec_path"; then
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
  echo "Update the spec artifact and/or quality gate result, then press Enter to re-check."
  read -r _
done
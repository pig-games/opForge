#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/run_review_workflow.sh <review-path> <review-scope> [focus]

Behavior:
  - Creates the review artifact from the template if it does not exist.
  - Prints the exact branch-local review workflow instructions.
  - Validates the review artifact structure.
  - Requires a companion gate result file with PASS from review-report-quality-reviewer.
  - Allows at most 3 failed re-check cycles, then halts and asks the user to resolve the blockage.

Companion gate file:
  <review-path>.quality-gate.txt

Expected gate file contents:
  PASS: <short technical explanation>

Examples:
  scripts/workflow/run_review_workflow.sh \
    dev-docs/reviews/my-review.md \
    "review feature/libopforge-lib vs origin/main" \
    "focus on FFI and workflow docs"
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

review_path="$1"
review_scope="$2"
focus="${3:-}"
gate_path="${review_path}.quality-gate.txt"
max_attempts=3

if [[ ! -e "$review_path" ]]; then
  scripts/workflow/new_artifact_from_template.sh review "$review_path"
fi

print_instructions() {
  cat <<EOF
Review artifact: $review_path
Review scope: $review_scope
Quality gate file: $gate_path

Required workflow:
1. Run the branch-local Triple Review Orchestrator and write the final review to:
   $review_path
2. Resolve all clarification questions with the user before finalizing.
3. Ensure every finding has one decisive fix direction.
4. Run review-report-quality-reviewer using:
   - AGENTS.md
   - $review_path
   - scope: $review_scope
5. Save the reviewer result to:
   $gate_path
   Expected PASS format:
   PASS: <short technical explanation>
EOF

  if [[ -n "$focus" ]]; then
    echo "Additional focus: $focus"
  fi

  cat <<'EOF'

The script will keep checking until:
- check_review_report.py passes
- the quality gate file starts with PASS:

The script allows at most 3 failed re-check cycles.
If that limit is reached, stop and ask the user how to resolve the blockage.

Press Enter after each review/gate update to re-check.
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
  review_ok=0
  gate_ok=0

  if python3 scripts/workflow/check_review_report.py "$review_path"; then
    review_ok=1
  fi

  if check_gate_file; then
    gate_ok=1
  fi

  if [[ $review_ok -eq 1 && $gate_ok -eq 1 ]]; then
    echo "PASS: review workflow complete for $review_path"
    exit 0
  fi

  attempt=$((attempt + 1))
  if [[ $attempt -ge $max_attempts ]]; then
    echo
    echo "FAIL: review workflow reached the retry limit ($max_attempts failed re-check cycles)." >&2
    echo "Stop iterating and ask the user to resolve the blockage before continuing." >&2
    exit 1
  fi

  echo
  echo "Review workflow not complete yet. Failed re-check cycle: $attempt/$max_attempts."
  echo "Update the review artifact and/or quality gate result, then press Enter to re-check."
  read -r _
done

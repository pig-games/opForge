#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/run_review_workflow.sh [--check-once] [--root <root>] <review-path> <review-scope> [focus]

Behavior:
  - Creates the review artifact from the template if it does not exist.
  - Prints the exact branch-local review workflow instructions.
  - Validates the review artifact bundle.
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

if [[ ${#positionals[@]} -lt 2 || ${#positionals[@]} -gt 3 ]]; then
  usage >&2
  exit 1
fi

review_path="${positionals[0]}"
review_scope="${positionals[1]}"
focus="${positionals[2]:-}"
gate_path="${review_path}.quality-gate.txt"
max_attempts=3

if [[ ! -e "$review_path" ]]; then
  scripts/workflow/new_artifact_from_template.sh \
    review \
    "$review_path" \
    --title "$(basename "${review_path%.md}" | tr '-' ' ')" \
    --scope "$review_scope"
fi

python3 scripts/workflow/stamp_workflow_provenance.py \
  "$review_path" \
  --skill opforge-review-reporting \
  --entrypoint run_review_workflow.sh

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
- check_workflow_artifact_bundle.py passes for the review
- the quality gate file reports PASS

The script allows at most 3 failed re-check cycles.
If that limit is reached, stop and ask the user how to resolve the blockage.

Press Enter after each review/gate update to re-check, or use --check-once for one-shot validation.
EOF
}

check_gate_file() {
  python3 scripts/workflow/check_quality_gate_evidence.py "$gate_path"
}

print_instructions

attempt=0
while true; do
  review_ok=0
  gate_ok=0

  if python3 scripts/workflow/check_workflow_artifact_bundle.py --root "$bundle_root" review "$review_path"; then
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
  if [[ $check_once -eq 1 ]]; then
    exit 1
  fi
  echo "Update the review artifact and/or quality gate result, then press Enter to re-check."
  read -r _
done

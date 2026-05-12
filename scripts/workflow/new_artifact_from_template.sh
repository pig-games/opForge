#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/new_artifact_from_template.sh <kind> <output-path> [bootstrap-args...]

Kinds:
  spec
  plan
  review
  closure

Examples:
  scripts/workflow/new_artifact_from_template.sh spec documentation/my-spec.md
  scripts/workflow/new_artifact_from_template.sh plan documentation/my-plan.md
  scripts/workflow/new_artifact_from_template.sh \
    plan \
    documentation/my-plan.md \
    --title "My Plan" \
    --source "spec: documentation/my-spec.md" \
    --mode implementation
EOF
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  usage
  exit 0
fi

if [[ $# -lt 2 ]]; then
  usage >&2
  exit 1
fi

kind="$1"
output_path="$2"
shift 2

case "$kind" in
  spec)
    ;;
  plan)
    ;;
  review)
    ;;
  closure)
    ;;
  *)
    echo "Unknown kind: $kind" >&2
    usage >&2
    exit 1
    ;;
esac

python3 scripts/workflow/start_artifact.py "$kind" "$output_path" --entrypoint "new_artifact_from_template.sh" "$@"

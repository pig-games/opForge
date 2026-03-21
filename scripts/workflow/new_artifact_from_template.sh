#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/workflow/new_artifact_from_template.sh <kind> <output-path>

Kinds:
  spec
  plan
  review
  closure

Examples:
  scripts/workflow/new_artifact_from_template.sh spec documentation/my-spec.md
  scripts/workflow/new_artifact_from_template.sh plan documentation/my-plan.md
EOF
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  usage
  exit 0
fi

if [[ $# -ne 2 ]]; then
  usage >&2
  exit 1
fi

kind="$1"
output_path="$2"

case "$kind" in
  spec)
    template="templates/spec-template.md"
    ;;
  plan)
    template="templates/plan-template.md"
    ;;
  review)
    template="templates/review-report-template.md"
    ;;
  closure)
    template="templates/finding-closure-report-template.md"
    ;;
  *)
    echo "Unknown kind: $kind" >&2
    usage >&2
    exit 1
    ;;
esac

if [[ -e "$output_path" ]]; then
  echo "Refusing to overwrite existing file: $output_path" >&2
  exit 1
fi

mkdir -p "$(dirname "$output_path")"
cp "$template" "$output_path"
echo "Created $output_path from $template"


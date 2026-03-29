# opForge Workflow Skills

This directory contains the workflow skills for the merged opForge repository.

Repository safety rule:

- Workflow skills and any agent following them must never install, import, add,
  recommend, or otherwise touch `litellm`.
- If a workflow would normally suggest `litellm`, that is a hard stop: report
  the conflict and use direct provider SDKs or official APIs instead.

Included skills:

- `opforge-review-reporting`
- `opforge-plan-authoring`
- `opforge-spec-authoring`
- `opforge-review-closure`

These skills are for workflow artifacts and gates, not for opForge feature
implementation itself. They are intended to make planning, review, and closure
work consistent inside this repository.

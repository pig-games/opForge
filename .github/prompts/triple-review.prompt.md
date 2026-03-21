---
mode: agent
description: "Run a three-model parallel code review using GPT-5.4, Claude Opus 4.6, and Gemini 3.1 Pro, then write a single GPT-5.4-finalized Markdown review artifact."
model: "GPT-5.4 (copilot)"
tools: [agent, read, search, execute]
---
Use the `Triple Review Orchestrator` agent to review:

${input:reviewScope:Describe review scope (PR/branch/files/commit range)}

Review artifact path (`.md`):
${input:reviewPath:Where should the Markdown review be written?}

Additional focus (optional):
${input:focusAreas:Security, performance, parser, tests, etc.}

Run the same review scope through GPT-5.4, Claude Opus 4.6, and Gemini 3.1 Pro
in parallel. Resolve any material clarification questions with the user before
finalizing. Write one merged final review to the requested Markdown file,
following the active worktree `AGENTS.md`, with findings first, then testing
gaps, residual risks, and a brief summary.

# Review Severity Guide

Use stable finding IDs in review reports.

Suggested format:

- `RVW-YYYY-MM-DD-001`

Severity meanings:

- `critical`: data loss, security compromise, or a likely release blocker
- `high`: serious correctness or regression risk with broad impact
- `medium`: real bug, risk, or missing validation with limited scope
- `low`: smaller but still material maintainability or behavior concern

Do not use severity for:

- formatting preferences
- style-only comments
- abstract architectural opinions without concrete risk


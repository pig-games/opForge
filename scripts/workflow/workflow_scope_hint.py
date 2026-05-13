#!/usr/bin/env python3
"""Suggest opForge workflow rule packs for changed paths.

This helper is intentionally simple. It does not enforce policy; it helps agents
avoid loading every rule pack for every task.
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

RULES: list[tuple[str, str]] = []


def add(rule: str, reason: str) -> None:
    item = (rule, reason)
    if item not in RULES:
        RULES.append(item)


def main(argv: list[str]) -> int:
    root = Path.cwd().resolve()
    paths = [Path(p) for p in argv[1:]]

    if not paths:
        print("No paths supplied. Load only AGENTS.md, then task-specific files as needed.")
        return 0

    for p in paths:
        if p.is_absolute():
            try:
                p = p.resolve().relative_to(root)
            except ValueError:
                p = Path(os.fspath(p))

        s = str(p)
        name = p.name

        if s.startswith("native/motorola68000/") and s.endswith(".asm"):
            add("agents/rules/native-68000.md", f"native Motorola 68000 source: {s}")

        if "fs_uae" in s.lower() or "fs-uae" in s.lower():
            add("agents/rules/fs-uae.md", f"FS-UAE-related path: {s}")

        if name.startswith("RELEASE_NOTES_v") or "release" in s.lower():
            add("agents/rules/release-notes.md", f"release-related path: {s}")

        if "reference" in s.lower() or "golden" in s.lower() or "goldens" in s.lower():
            add("agents/rules/reference-refresh.md", f"reference/golden-related path: {s}")

        if (
            s.startswith("agents/")
            or s.startswith("skills/")
            or s.startswith("templates/")
            or s.startswith("scripts/workflow/")
            or "/plans/" in s
            or "/reviews/" in s
            or "/spec" in s.lower()
            or "closure" in s.lower()
        ):
            add("agents/rules/workflow-artifacts.md", f"workflow artifact/infrastructure path: {s}")

        if "orchestrator" in s.lower() or "triple" in s.lower() or "multi-agent" in s.lower():
            add("agents/rules/multi-agent-gates.md", f"multi-agent gate path: {s}")

    if not RULES:
        print("Suggested rule packs: none. Use root AGENTS.md only unless task details require more.")
        return 0

    print("Suggested rule packs:")
    for rule, reason in RULES:
        print(f"- {rule}  # {reason}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))

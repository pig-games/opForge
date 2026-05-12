#!/usr/bin/env python3
"""Stamp deterministic workflow provenance onto a governed artifact."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from workflow_common import apply_workflow_provenance, read_text, write_text


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Insert or update a workflow provenance comment on a Markdown artifact."
    )
    parser.add_argument("path", help="Artifact path to stamp")
    parser.add_argument("--skill", required=True, help="Canonical skill name")
    parser.add_argument("--entrypoint", required=True, help="Workflow entrypoint command or script")
    args = parser.parse_args()

    path = Path(args.path)
    if not path.exists():
        print(f"FAIL: file not found: {path}", file=sys.stderr)
        return 1

    text = read_text(path)
    stamped = apply_workflow_provenance(text, args.skill, args.entrypoint)
    if stamped != text:
        write_text(path, stamped)
    print(f"Stamped workflow provenance on {path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""Allocate the next stable review finding ID for a review artifact."""

from __future__ import annotations

import argparse
from datetime import date
from pathlib import Path

from workflow_common import next_review_finding_id, read_text


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Print the next stable RVW-YYYY-MM-DD-NNN finding ID for a review artifact."
    )
    parser.add_argument("review_path", help="Review artifact path")
    parser.add_argument(
        "--date",
        dest="date_text",
        help="Date prefix to use, in YYYY-MM-DD format. Defaults to today.",
    )
    args = parser.parse_args()

    review_path = Path(args.review_path)
    text = read_text(review_path) if review_path.exists() else ""
    date_text = args.date_text or date.today().isoformat()
    print(next_review_finding_id(text, date_text))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""Decode current assembly-work text counters; completion is independently proven."""
import argparse
import json
from pathlib import Path
import re


class AssemblyWorkDecodeError(ValueError):
    pass


def decode_assembly_work(text: str) -> dict[str, object] | None:
    """Decode phase 300 from one compact CLI capture; completion is independent."""
    event = re.compile(
        r"^progress p=0000012[cC] f=([0-9a-fA-F]{8}) "
        r"l=([0-9a-fA-F]{8}) r=([0-9a-fA-F]{8}) m=([0-9a-fA-F]{8})$"
    )
    result = None
    for line in text.splitlines():
        if not line.strip().lower().startswith("progress p=0000012c"):
            continue
        match = event.fullmatch(line.strip())
        if match is None:
            raise AssemblyWorkDecodeError("malformed assembly-work progress line")
        if result is not None:
            raise AssemblyWorkDecodeError("duplicate assembly-work progress line")
        traversals, visits, reserved, _ = (int(word, 16) for word in match.groups())
        if reserved != 0:
            raise AssemblyWorkDecodeError("assembly-work reserved field must be zero")
        result = {"scheduled_source_sweeps": traversals,
                  "selected_record_visits": visits, "localization_only": True}
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--assembly-progress-text", required=True, type=Path)
    args = parser.parse_args()
    try:
        print(json.dumps({"assembly_work": decode_assembly_work(args.assembly_progress_text.read_text())}))
    except (OSError, AssemblyWorkDecodeError) as error:
        parser.exit(1, f"{error}\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

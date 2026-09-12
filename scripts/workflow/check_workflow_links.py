#!/usr/bin/env python3
"""Check local Markdown links in active workflow entrypoints, without prose schemas."""
from pathlib import Path
import re
from urllib.parse import unquote, urlsplit

ROOT = Path(__file__).resolve().parents[2]


def check(path: Path) -> list[str]:
    text = re.sub(r"```.*?```", "", path.read_text(encoding="utf-8"), flags=re.S)
    errors = []
    for target in re.findall(r"\[[^\]\n]*\]\(([^)]+)\)", text):
        target = target.strip()
        if target.startswith("<"):
            target = target[1:target.index(">")]
        else:
            target = target.split()[0]
        url = urlsplit(target)
        if url.scheme or url.netloc or not url.path:
            continue
        resolved = path.parent / unquote(url.path)
        if not resolved.exists():
            errors.append(f"{path}: missing local link: {target}")
    return errors


def main() -> int:
    paths = [ROOT / "AGENTS.md", ROOT / "documentation/README.md",
             ROOT / "documentation/workflow/README.md", ROOT / "skills/README.md"]
    paths += sorted((ROOT / "agents/rules").glob("*.md"))
    paths += sorted((ROOT / "skills").glob("*/SKILL.md"))
    errors = [error for path in paths for error in check(path)]
    if errors:
        print("\n".join(errors))
        return 1
    print(f"PASS: local workflow links in {len(paths)} documents")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""Remove direct crate-local Cargo target contents within this repository."""

from pathlib import Path
import shutil
import sys


def clean_crate_targets(repository: Path) -> list[Path]:
    root = repository.resolve(strict=True)
    crates = root / "crates"
    if not crates.is_dir() or crates.is_symlink():
        return []

    removed = []
    for crate in crates.iterdir():
        # Do not traverse symlinked crate directories or targets. Their actual
        # locations may be outside the repository or owned by another project.
        if crate.is_symlink() or not crate.is_dir():
            continue
        target = crate / "target"
        if target.is_symlink() or not target.is_dir():
            continue
        resolved = target.resolve(strict=True)
        if not resolved.is_relative_to(root):
            continue
        for artifact in target.iterdir():
            if artifact.is_symlink() or not artifact.is_dir():
                artifact.unlink()
            else:
                shutil.rmtree(artifact)
        removed.append(target)
    return removed


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print(f"usage: {Path(argv[0]).name} REPOSITORY_ROOT", file=sys.stderr)
        return 2
    root = Path(argv[1])
    for target in clean_crate_targets(root):
        print(f"cleaned {target}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))

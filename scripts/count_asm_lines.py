#!/usr/bin/env python3
"""Count non-blank, non-comment (comment-only) lines in assembly files.

Usage:
  python3 scripts/count_asm_lines.py [path] [--ext .asm] [--min-width N]

Defaults to scanning the `native/` directory in the repository root when no
path is provided.

By default this treats lines that are blank or that begin (after optional
leading whitespace) with ';' as comments and excludes them from the count.
Lines with code followed by a trailing ';' are counted as code lines.
"""
from __future__ import annotations

import argparse
import os
import sys
from typing import Dict, List, Tuple


def count_code_lines(path: str, exts: List[str], excludes: List[str] | None = None) -> Tuple[Dict[str, int], int]:
    per_file: Dict[str, int] = {}
    total = 0
    excludes = excludes or []
    for root, dirs, files in os.walk(path):
        # skip hidden dirs like .git
        dirs[:] = [d for d in dirs if not d.startswith('.')]
        # skip any tree that contains an excluded directory name
        rel_root = os.path.relpath(root, path)
        rel_parts = [] if rel_root == '.' else rel_root.split(os.sep)
        if any(part in excludes for part in rel_parts):
            continue
        for fn in files:
            if any(fn.lower().endswith(ext.lower()) for ext in exts):
                fp = os.path.join(root, fn)
                try:
                    with open(fp, 'r', encoding='utf-8') as f:
                        lines = f.readlines()
                except UnicodeDecodeError:
                    with open(fp, 'r', encoding='latin-1') as f:
                        lines = f.readlines()

                count = 0
                for line in lines:
                    if not line.strip():
                        continue
                    # comment-only if leading non-space starts with ';'
                    if line.lstrip().startswith(';'):
                        continue
                    count += 1

                rel = os.path.relpath(fp)
                per_file[rel] = count
                total += count
    return per_file, total


def print_loc_like(per_file: Dict[str, int], total: int, min_width: int = 5) -> None:
    # sort by count desc
    items = sorted(per_file.items(), key=lambda kv: kv[1], reverse=True)
    max_num_width = max((len(str(n)) for n, _ in ((v, k) for k, v in per_file.items())), default=1)
    num_w = max(max_num_width, min_width)

    for path, cnt in items:
        print(f"{cnt:>{num_w}} {path}")

    print()
    print(f"{total:>{num_w}} total lines ({len(per_file)} files)")


def print_dir_totals(per_file: Dict[str, int], min_width: int = 5) -> None:
    # Aggregate by directory
    dirs: Dict[str, Tuple[int, int]] = {}  # dir -> (lines, file_count)
    for path, cnt in per_file.items():
        d = os.path.dirname(path) or '.'
        if d not in dirs:
            dirs[d] = (cnt, 1)
        else:
            lines, files = dirs[d]
            dirs[d] = (lines + cnt, files + 1)

    items = sorted(dirs.items(), key=lambda kv: kv[1][0], reverse=True)
    max_num_width = max((len(str(v[0])) for v in dirs.values()), default=1)
    num_w = max(max_num_width, min_width)

    print('Per-directory totals:')
    for d, (lines, files) in items:
        print(f"{lines:>{num_w}} {files:>4} files  {d}")


def main(argv: List[str] | None = None) -> int:
    p = argparse.ArgumentParser(description="Count assembler code lines (exclude blank/comment-only)")
    p.add_argument('path', nargs='?', default='native', help='Path to scan (default: native)')
    p.add_argument('--ext', action='append', default=['.asm', '.s', '.h'], help='File extension to include (can repeat). Default: .asm,.s,.h')
    p.add_argument('--exclude', action='append', default=['test-harnesses'], help="Directory names to exclude (can repeat). Default: test-harnesses")
    p.add_argument('--min-width', type=int, default=5, help='Minimum numeric column width')
    args = p.parse_args(argv)

    if not os.path.exists(args.path):
        print(f"Path not found: {args.path}", file=sys.stderr)
        return 2

    per_file, total = count_code_lines(args.path, args.ext, excludes=args.exclude)
    print_loc_like(per_file, total, min_width=args.min_width)
    print()
    print_dir_totals(per_file, min_width=args.min_width)
    return 0


if __name__ == '__main__':
    raise SystemExit(main())

#!/usr/bin/env python3
"""Run a current native case with bounded completion and artifact checks."""
import json
import os
import re
import shutil
import signal
import subprocess

import vm_efficiency as base


def hunk_segments(data: bytes) -> list[dict]:
    """Fail-closed reader for the producer's CODE/DATA/BSS + RELOC32 subset."""
    cursor = 0

    def word():
        nonlocal cursor
        if cursor + 4 > len(data):
            raise ValueError("truncated Hunk")
        value = int.from_bytes(data[cursor:cursor + 4], "big")
        cursor += 4
        return value

    if word() != 0x3f3 or word() != 0:
        raise ValueError("unsupported Hunk header")
    count, first, last = word(), word(), word()
    if not 1 <= count <= 16 or first != 0 or last != count - 1:
        raise ValueError("unsupported Hunk table")
    sizes = [(word() & 0x3fffffff) * 4 for _ in range(count)]
    result = []
    for index in range(count):
        kind, size = word(), word() * 4
        if kind not in (0x3e9, 0x3ea, 0x3eb) or size > sizes[index]:
            raise ValueError("unsupported Hunk segment")
        payload = b""
        if kind != 0x3eb:
            if cursor + size > len(data):
                raise ValueError("truncated Hunk payload")
            payload = data[cursor:cursor + size]
            cursor += size
        relocations = {}
        marker = word()
        if marker == 0x3ec:
            while (entries := word()) != 0:
                target = word()
                if entries > len(payload) // 2 or target >= count:
                    raise ValueError("invalid Hunk relocation group")
                for _ in range(entries):
                    offset = word()
                    # The producer accepts byte offsets, including odd DATA
                    # locations. Do not impose an instruction-alignment rule.
                    if offset + 4 > len(payload) or offset in relocations:
                        raise ValueError(f"invalid Hunk relocation offset: segment={index}, offset={offset}, "
                                         f"payload={len(payload)}, duplicate={offset in relocations}")
                    relocations[offset] = target
            marker = word()
        if marker != 0x3f2:
            raise ValueError("unsupported Hunk trailer")
        result.append({"index": index, "kind": kind, "bytes": payload,
                       "allocation_bytes": sizes[index], "relocations": relocations})
    if cursor != len(data):
        raise ValueError("unconsumed Hunk data")
    return result


def native(binary, source, budget, log, profile="off", native_root=None,
           *, test, result_prefix='RUNTIME_COMPARISON ', extra_env=None,
           guest_timeout_ms=35000, post_start_timeout_ms=35000):
    env = dict(base.clean_env(), OPFORGE_COMPARE_SOURCE=str(source),
               OPFORGE_FS_UAE_SMOKE='1',
               OPFORGE_COMPARE_PROFILE=profile,
               OPFORGE_FS_UAE_POLL_MS='20',
               OPFORGE_FS_UAE_TIMEOUT_MS=str(guest_timeout_ms),
               OPFORGE_FS_UAE_POST_START_TIMEOUT_MS=str(post_start_timeout_ms))
    env.update({key: value for key, value in os.environ.items()
                if key.startswith('OPFORGE_FS_UAE_') and key not in {
                    'OPFORGE_FS_UAE_POLL_MS', 'OPFORGE_FS_UAE_TIMEOUT_MS',
                    'OPFORGE_FS_UAE_POST_START_TIMEOUT_MS', 'OPFORGE_FS_UAE_SMOKE'}})
    if native_root is not None:
        env["OPFORGE_COMPARE_NATIVE_ROOT"] = str(native_root)
    if extra_env:
        env.update(extra_env)
    # The existing runner owns normal cleanup. Also cover external timeout/unwind
    # of this serial test process; only newly created runner trees are candidates.
    pattern = 'fs-uae-hunk-smoke-native-*'
    target = (native_root or base.ROOT) / 'target'
    before = set(target.glob(pattern))
    try:
        code, stdout, stderr, elapsed = base.run_process(
            [str(binary), test, '--exact', '--ignored', '--nocapture'], base.ROOT, env, budget)
        text = (stdout + stderr).decode(errors='replace')
        log.write_text(text)
        rows = [json.loads(line.split(result_prefix, 1)[1])
                for line in text.splitlines() if line.startswith(result_prefix)]
        if code or len(rows) != 1 or rows[0]['guest_exit'] != 0:
            raise ValueError(f'native completion/parity failed: {log}')
        if result_prefix == 'RUNTIME_COMPARISON ' and (
                rows[0].get('profile') != profile
                or (profile != 'off' and not rows[0].get('counters'))):
            raise ValueError('requested native telemetry was not decoded from the completed run')
        return dict(rows[0], invocation_seconds=elapsed)
    finally:
        for tree in set(target.glob(pattern)) - before:
            # A surviving emulator can have detached from the test process group.
            result = subprocess.run(['pgrep', '-f', re.escape(str(tree))],
                                    capture_output=True, text=True, timeout=5)
            if result.returncode not in (0, 1):
                raise RuntimeError('cannot identify owned emulator during cleanup')
            for pid in result.stdout.split():
                try:
                    os.kill(int(pid), signal.SIGKILL)
                except ProcessLookupError:
                    pass
            shutil.rmtree(tree)


def native_source_digest(root):
    """Hash source paths and bytes, including untracked new native modules."""
    entries = []
    for path in sorted((root / "native").rglob("*")):
        if path.is_file() and path.suffix in {".asm", ".i"}:
            entries.append([str(path.relative_to(root)), base.digest(path.read_bytes())])
    return base.digest(json.dumps(entries, separators=(",", ":")).encode())

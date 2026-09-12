#!/usr/bin/env python3
"""Bounded, cross-family package-VM baseline; no native/emulator execution."""
from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import signal
import statistics
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
FAMILIES = ("m6502", "z80", "m68000")
BUILD = ["cargo", "build", "--release", "--locked", "-p", "cli", "--bin",
         "opforge", "--bin", "build_vm_package", "--features", "vm-runtime-only",
         "--message-format=json-render-diagnostics"]


def digest(data):
    return hashlib.sha256(data).hexdigest()


def workload(cpu, blocks):
    """Ordinary source and independent ISA byte contract, including label values."""
    if cpu not in FAMILIES or not 1 <= blocks <= 128:
        raise ValueError("unsupported family or block count (1..128)")
    lines = [f'.cpu {cpu}', '.org $1000']
    expected = bytearray()
    for i in range(blocks):
        value, other = i % 64 + 1, (i * 3) % 64 + 1
        address = 0x1000 + len(expected)
        lines.append(f'block{i}:')
        if cpu == "m6502":
            lines += [f' lda #{value}', f' ldx #{other}', ' sta $2000',
                      f' bne next{i}', ' .byte 0', f'next{i}: nop', f' .word block{i}+1']
            expected += bytes([0xA9, value, 0xA2, other, 0x8D, 0, 0x20,
                               0xD0, 1, 0, 0xEA]) + (address + 1).to_bytes(2, 'little')
        elif cpu == "z80":
            # Current VM package accepts Intel-compatible immediates on Z80;
            # Zilog LD r,n currently fails. Keep that coverage gap explicit.
            lines += [f' mvi a,{value}', f' mvi b,{other}', ' ld ($2000),a',
                      f' jr nz,next{i}', ' .byte 0', f'next{i}: nop', f' .word block{i}+1']
            expected += bytes([0x3E, value, 0x06, other, 0x32, 0, 0x20,
                               0x20, 1, 0, 0]) + (address + 1).to_bytes(2, 'little')
        else:
            lines += [f' moveq #{value},d0', f' moveq #{other},d1', ' move.b d0,($2000).w',
                      f' bne.s next{i}', ' .word 0', f'next{i}: nop', f' .long block{i}+2']
            expected += bytes([0x70, value, 0x72, other, 0x11, 0xC0, 0x20, 0,
                               0x66, 2, 0, 0, 0x4E, 0x71]) + (address + 2).to_bytes(4, 'big')
    lines.append('.end')
    return '\n'.join(lines) + '\n', bytes(expected)


def clean_env(profile=False):
    # Ambient experiment toggles must not silently change the measured route.
    env = {k: v for k, v in os.environ.items() if not k.startswith('OPFORGE_')}
    env['NO_COLOR'] = '1'
    if profile:
        env.update(OPFORGE_PROFILE_PHASES='1', OPFORGE_PROFILE_EXECUTION_PATHS='1')
    return env


class Budget:
    def __init__(self, seconds=300):
        self.deadline = time.monotonic() + seconds

    def allowance(self, cap):
        remaining = self.deadline - time.monotonic()
        if remaining <= 0:
            raise TimeoutError('measurement batch deadline exhausted')
        return min(cap, remaining)


def run_process(command, cwd, env, budget, cap=60):
    """POSIX process-group deadline includes descendants holding output pipes."""
    timeout = budget.allowance(cap)
    start = time.monotonic()
    process = subprocess.Popen(command, cwd=cwd, env=env, start_new_session=True,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    try:
        stdout, stderr = process.communicate(timeout=timeout)
    except BaseException:
        try:
            os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        process.communicate()
        raise
    elapsed = time.monotonic() - start
    if elapsed > timeout:
        raise TimeoutError('invocation exceeded measurement deadline')
    return process.returncode, stdout, stderr, elapsed


def build_binary(directory):
    start = time.monotonic()
    code, stdout, stderr, _ = run_process(BUILD, ROOT, clean_env(), Budget(600), 600)
    (directory / 'build.stderr').write_bytes(stderr)
    if code:
        raise ValueError(f'build failed: see {directory / "build.stderr"}')
    rows = [json.loads(line) for line in stdout.decode().splitlines()]
    artifacts = [row for row in rows
                 if row.get('reason') == 'compiler-artifact'
                 and row.get('target', {}).get('name') == 'opforge' and row.get('executable')]
    if len(artifacts) != 1 or 'vm-runtime-only' not in artifacts[0]['features']:
        raise ValueError('Cargo must identify one vm-runtime-only executable')
    artifact = artifacts[0]
    binary = Path(artifact['executable']).resolve(strict=True)
    generators = [row['executable'] for row in rows if row.get('reason') == 'compiler-artifact'
                  and row.get('target', {}).get('name') == 'build_vm_package' and row.get('executable')]
    if len(generators) != 1:
        raise ValueError('Cargo did not identify the canonical package generator')
    package = directory / 'runtime.opasm'
    code, _, stderr, _ = run_process([generators[0], str(package)], ROOT, clean_env(), Budget(60))
    if code or not package.is_file():
        raise ValueError(f'package generation failed: {stderr.decode(errors="replace")}')
    return binary, {'command': BUILD, 'seconds': time.monotonic() - start,
                    'profile': artifact['profile'], 'features': artifact['features'],
                    'binary': str(binary), 'binary_bytes': binary.stat().st_size,
                    'binary_sha256': digest(binary.read_bytes()),
                    'package': str(package), 'package_bytes': package.stat().st_size,
                    'package_sha256': digest(package.read_bytes()),
                    'cargo_lock_sha256': digest((ROOT / 'Cargo.lock').read_bytes())}


def parse_profile(text):
    rows, section = [], ''
    for line in text.splitlines():
        if line.startswith('[opforge') or (line and not line.startswith(' ') and ' ms ' not in line):
            section = line.strip()
        match = re.fullmatch(r'\s*(.*?)\s+([\d.]+) ms\s+([\d.]+)%(?:\s+\((\d+)x\))?', line)
        if match:
            rows.append({'section': section, 'label': match[1], 'milliseconds': float(match[2]),
                         'count': int(match[4]) if match[4] else None})
    for label in ('vm.encode', 'vm.parse'):
        if not any(row['label'] == label and (row['count'] or 0) > 0 for row in rows):
            raise ValueError(f'attribution did not demonstrate {label} execution')
    if any(row['label'].startswith(('rust.encode', 'rust.parse')) and (row['count'] or 0) > 0 for row in rows):
        raise ValueError('unexpected host execution bypass in VM attribution')
    return rows


def assemble(binary, directory, cpu, budget, expected, *, profile=False, negative=False):
    output = directory / 'output.bin'
    output.unlink(missing_ok=True)
    command = [str(binary), '--cpu', cpu, '--infile', 'input.asm', '--bin', 'output.bin',
               '--opasm-package', str(directory.parent / 'runtime.opasm')]
    code, stdout, stderr, elapsed = run_process(command, directory, clean_env(profile), budget)
    text = stderr.decode(errors='replace')
    if negative:
        if code < 0 or 'ERROR' not in text:
            raise ValueError(f'{cpu}: crashed or missing explicit error diagnostic')
        if code != 1 or 'ERROR [asm401]' not in text or 'ERROR: Label not found: missing_symbol' not in text:
            raise ValueError(f'{cpu}: negative case did not report the expected undefined-symbol error: {text}')
    elif code or stdout or (stderr and not profile) or not output.is_file() or output.read_bytes() != expected:
        raise ValueError(f'{cpu}: failed assembly/output contract (exit {code}): {text}')
    return elapsed, text, command


def measure(binary, directory, sizes, budget):
    results = []
    for cpu in FAMILIES:
        for blocks in sizes:
            case = directory / f'{cpu}-{blocks}'
            case.mkdir()
            source, expected = workload(cpu, blocks)
            (case / 'input.asm').write_text(source)
            (case / 'expected.bin').write_bytes(expected)
            _, _, command = assemble(binary, case, cpu, budget, expected)  # warmup
            samples = [assemble(binary, case, cpu, budget, expected)[0] for _ in range(3)]
            row = {'cpu': cpu, 'blocks': blocks, 'source_bytes': len(source.encode()),
                   'source_lines': len(source.splitlines()), 'output_bytes': len(expected),
                   'source_sha256': digest(source.encode()), 'output_sha256': digest(expected),
                   'command': command, 'samples_seconds': samples,
                   'median_seconds': statistics.median(samples), 'min_seconds': min(samples),
                   'max_seconds': max(samples)}
            if blocks == sizes[len(sizes) // 2]:
                _, profile, _ = assemble(binary, case, cpu, budget, expected, profile=True)
                (case / 'profile.txt').write_text(profile)
                row['profile'] = parse_profile(profile)
                row['work_counts'] = {label: sum(r['count'] or 0 for r in row['profile'] if r['label'] == label)
                                      for label in ('vm.parse', 'vm.parse_cache_hit', 'vm.encode', 'vm.model.bootstrap')}
            results.append(row)
            print(f'{cpu:8} {blocks:3} blocks: {row["median_seconds"]:.4f}s; bytes verified', flush=True)
        case = directory / f'{cpu}-negative'
        case.mkdir()
        (case / 'input.asm').write_text(f'.cpu {cpu}\n.byte missing_symbol\n.end\n')
        _, diagnostic, _ = assemble(binary, case, cpu, budget, b'', negative=True)
        (case / 'diagnostic.txt').write_text(diagnostic)
    return results


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('workload', choices=['selection'])
    parser.add_argument('--blocks', default='8,32,128')
    parser.add_argument('--output', type=Path, help='new output directory (default: ignored build directory)')
    args = parser.parse_args()
    if os.name != 'posix':
        parser.error('this initial runner requires POSIX process-group termination')
    try:
        sizes = [int(value) for value in args.blocks.split(',')]
        if len(sizes) != 3 or sizes != sorted(set(sizes)) or not 1 <= min(sizes) <= max(sizes) <= 128:
            raise ValueError('provide three increasing block counts in 1..128')
        if args.output:
            directory = args.output.resolve()
            directory.mkdir(parents=True, exist_ok=False)
        else:
            (ROOT / 'build').mkdir(exist_ok=True)
            directory = Path(tempfile.mkdtemp(prefix='vm-efficiency-', dir=ROOT / 'build'))
        summary = {'status': 'incomplete', 'workload': args.workload,
                   'host': platform.platform(), 'blocks': sizes,
                   'measurement_limits_seconds': {'invocation': 60, 'batch': 300},
                   'runner_sha256': digest(Path(__file__).read_bytes()),
                   'limitations': ['Z80 uses supported MVI spelling; LD r,n VM-only support is not established.',
                                   'Fixed-order small sample baseline, not a statistically qualified speedup.',
                                   'No native execution or full-self-host performance claim.'],
                   'memory': {'peak_bytes': None, 'native_footprint_bytes': None,
                              'limitation': 'Not measured; host heap is not native footprint.'}}
        print(f'Output: {directory}\nBuilding VM-only release once (outside measurement batch).', flush=True)
        try:
            binary, summary['build'] = build_binary(directory)
            summary['source_revision'] = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip()
            summary['tracked_diff_sha256'] = digest(subprocess.check_output(['git', 'diff', 'HEAD'], cwd=ROOT))
            summary['compiler'] = subprocess.check_output(['rustc', '--version'], text=True).strip()
            summary['build_environment'] = {k: v for k, v in clean_env().items()
                                            if k.startswith(('CARGO_', 'RUST'))}
            start = time.monotonic()
            summary['cases'] = measure(binary, directory, sizes, Budget())
            summary['measurement_seconds'] = time.monotonic() - start
            summary['memory']['sensitivity_only'] = {
                'explanation': 'Illustrative source+output+canonical-package+statement-record storage; excludes OS, '
                               'runtime code, decoded tables, symbols and scratch. Not a peak or feasibility claim.',
                'largest_case_bytes_by_record_size': {
                    str(width): max(c['source_bytes'] + c['output_bytes'] + (c['blocks'] * 8 + 3) * width
                                    for c in summary['cases']) + summary['build']['package_bytes']
                    for width in (8, 16, 32)}}
            summary['status'] = 'passed'
        except Exception as exc:
            summary['error'] = str(exc)
            raise
        finally:
            (directory / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
        print(f'PASS: {directory / "summary.json"}')
        return 0
    except (ValueError, OSError, TimeoutError, subprocess.TimeoutExpired) as exc:
        print(f'FAIL: {exc}')
        return 1


if __name__ == '__main__':
    raise SystemExit(main())

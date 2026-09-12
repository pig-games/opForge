#!/usr/bin/env python3
"""Bounded report/refuse probe for package-controlled target callbacks."""
from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import platform
import re
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
VM_PATH = Path(__file__).with_name('vm_efficiency.py')
SPEC = importlib.util.spec_from_file_location('vm_efficiency', VM_PATH)
vm = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(vm)

CALLBACK_PREFIX = '[opforge target callbacks] '
MODES = ('baseline', 'report', 'refuse')


def digest(data):
    return hashlib.sha256(data).hexdigest()


def clean_env(profile=False, callback_mode='baseline'):
    """Use vm_efficiency's sanitized environment and opt into one probe mode."""
    if callback_mode not in MODES:
        raise ValueError('unknown callback mode')
    env = vm.clean_env(profile=profile)
    if callback_mode != 'baseline':
        env['OPFORGE_TARGET_CALLBACKS'] = callback_mode
    return env


def parse_callbacks(text, expected_mode):
    reports = [json.loads(line[len(CALLBACK_PREFIX):]) for line in text.splitlines()
               if line.startswith(CALLBACK_PREFIX)]
    if len(reports) != 1:
        raise ValueError('missing or duplicate target-callback report')
    report = reports[0]
    if (report.get('schema') != 1 or report.get('mode') != expected_mode
            or report.get('overflow') is not False):
        raise ValueError('invalid target-callback report schema or mode')
    if not isinstance(report.get('attempts'), list):
        raise ValueError('target-callback attempts must be a list')
    for attempt in report['attempts']:
        if (not isinstance(attempt, dict)
                or any(not isinstance(attempt.get(key), str) or not attempt[key]
                       for key in ('boundary', 'family', 'cpu'))
                or not isinstance(attempt.get('detail'), str)
                or not isinstance(attempt.get('count'), int) or attempt['count'] < 1):
            raise ValueError('invalid target-callback attempt')
    refusal = report.get('first_refusal')
    if refusal is not None and not isinstance(refusal, str):
        raise ValueError('first_refusal must be null or a string')
    return report


def normalize_diagnostics(text):
    """Remove known instrumentation and profile rows while retaining diagnostics."""
    kept = []
    in_profile = False
    metric = re.compile(r'\s*.*?\s+[\d.]+ ms\s+[\d.]+%(?:\s+\(\d+x\))?')
    section = re.compile(r'[A-Za-z0-9_-]+(?:\.[A-Za-z0-9_.-]+)+')
    for line in text.splitlines():
        if line.startswith('[opforge '):
            in_profile = line in ('[opforge execution profile]', '[opforge phase profile]')
            continue
        if in_profile and (not line.strip() or line == 'global:' or metric.fullmatch(line) or section.fullmatch(line.strip())):
            continue
        in_profile = False
        kept.append(line)
    return '\n'.join(kept).strip()


def assemble(binary, directory, cpu, blocks, source, expected, budget, mode):
    output = directory / 'output.bin'
    output.unlink(missing_ok=True)
    command = [str(binary), '--cpu', cpu, '--infile', 'input.asm', '--bin', 'output.bin',
               '--opasm-package', str(directory.parent / 'runtime.opasm')]
    code, stdout, stderr, elapsed = vm.run_process(
        command, directory, clean_env(profile=True, callback_mode=mode), budget, cap=60)
    text = stderr.decode(errors='replace')
    result = {'mode': mode, 'cpu': cpu, 'blocks': blocks, 'command': command,
              'elapsed_seconds': elapsed, 'exit_code': code, 'stdout_bytes': len(stdout),
              'diagnostics': text}
    if mode == 'refuse':
        report = parse_callbacks(text, 'refuse')
        if code != 1 or output.exists() or not report['first_refusal']:
            raise ValueError(f'{cpu}/{blocks}: refuse mode did not fail closed')
        result['callbacks'] = report
        return result

    if code or stdout or not output.is_file() or output.read_bytes() != expected:
        raise ValueError(f'{cpu}/{blocks}/{mode}: failed assembly/output contract (exit {code})')
    if mode == 'report':
        result['callbacks'] = parse_callbacks(text, 'report')
    profile = vm.parse_profile(text)
    source_bytes = len(source.encode())
    result['profile'] = profile
    result['vm_work'] = vm.parse_vm_work(text, blocks, source_bytes, len(source.splitlines()))
    result['work_counts'] = {
        label: sum(row['count'] or 0 for row in profile if row['label'] == label)
        for label in ('vm.parse', 'vm.parse_cache_hit', 'vm.encode', 'vm.model.bootstrap')}
    (directory / f'{mode}.bin').write_bytes(output.read_bytes())
    result['output_sha256'] = digest(output.read_bytes())
    return result


def assemble_negative(binary, directory, cpu, budget, mode):
    """Confirm an ordinary undefined-symbol error remains visible in both modes."""
    output = directory / 'negative.bin'
    output.unlink(missing_ok=True)
    (directory / 'negative.asm').write_text(f'.cpu {cpu}\n.byte missing_symbol\n.end\n')
    command = [str(binary), '--cpu', cpu, '--infile', 'negative.asm', '--bin', 'negative.bin',
               '--opasm-package', str(directory.parent / 'runtime.opasm')]
    code, stdout, stderr, elapsed = vm.run_process(
        command, directory, clean_env(callback_mode=mode), budget, cap=60)
    text = stderr.decode(errors='replace')
    if code != 1 or stdout or output.exists() or 'ERROR [asm401]' not in text \
            or 'ERROR: Label not found: missing_symbol' not in text:
        raise ValueError(f'{cpu}: negative case did not report the expected undefined-symbol error')
    result = {'mode': mode, 'exit_code': code, 'elapsed_seconds': elapsed,
              'command': command, 'diagnostics': text}
    if mode == 'report':
        result['callbacks'] = parse_callbacks(text, 'report')
    return result


def run(binary, directory, sizes, budget, cases=None):
    if cases is None:
        cases = []
    for cpu in vm.FAMILIES:
        for blocks in sizes:
            case = directory / f'{cpu}-{blocks}'
            case.mkdir()
            source, expected = vm.workload(cpu, blocks)
            (case / 'input.asm').write_text(source)
            (case / 'expected.bin').write_bytes(expected)
            rows = {}
            for mode in MODES:
                rows[mode] = assemble(binary, case, cpu, blocks, source, expected, budget, mode)
                (case / f'{mode}.diagnostics.txt').write_text(rows[mode]['diagnostics'])
            baseline, report = rows['baseline'], rows['report']
            if baseline['output_sha256'] != digest(expected) or report['output_sha256'] != digest(expected):
                raise ValueError(f'{cpu}/{blocks}: baseline or report bytes differ from independent oracle')
            if normalize_diagnostics(baseline['diagnostics']) != normalize_diagnostics(report['diagnostics']):
                raise ValueError(f'{cpu}/{blocks}: report mode changed normalized diagnostics')
            negative = {mode: assemble_negative(binary, case, cpu, budget, mode)
                        for mode in ('baseline', 'report')}
            (case / 'negative-baseline.diagnostics.txt').write_text(negative['baseline']['diagnostics'])
            (case / 'negative-report.diagnostics.txt').write_text(negative['report']['diagnostics'])
            if normalize_diagnostics(negative['baseline']['diagnostics']) != normalize_diagnostics(
                    negative['report']['diagnostics']):
                raise ValueError(f'{cpu}/{blocks}: report mode changed negative-case diagnostics')
            attempts = rows['report']['callbacks']['attempts']
            cases.append({'cpu': cpu, 'blocks': blocks, 'source_bytes': len(source.encode()),
                          'source_lines': len(source.splitlines()), 'output_bytes': len(expected),
                          'source_sha256': digest(source.encode()), 'output_sha256': digest(expected),
                          'modes': rows,
                          'negative_case': negative,
                          'callback_attempts_by_boundary': _attempt_counts(attempts, 'boundary'),
                          'callback_attempts_by_family': _attempt_counts(attempts, 'family'),
                          'callback_attempts_by_cpu': _attempt_counts(attempts, 'cpu')})
            print(f'{cpu:8} {blocks:3} blocks: {len(attempts)} callback classes; bytes verified; refusal closed',
                  flush=True)
    return cases


def shared_control(binary, directory, budget):
    """Strict mode must still allow a complete callback-free package operation."""
    case = directory / 'shared-control'
    case.mkdir()
    # CPU comes from the command line: a .cpu operand itself consults the
    # current family surface parser, which is one of the dependencies under audit.
    (case / 'input.asm').write_text(' nop\n.end\n')
    command = [str(binary), '--cpu', 'm68000', '--infile', 'input.asm',
               '--bin', 'output.bin', '--opasm-package', str(directory / 'runtime.opasm')]
    code, stdout, stderr, elapsed = vm.run_process(
        command, case, clean_env(callback_mode='refuse'), budget, cap=60)
    text = stderr.decode(errors='replace')
    (case / 'refuse.diagnostics.txt').write_text(text)
    callbacks = parse_callbacks(text, 'refuse')
    output = case / 'output.bin'
    if (code or stdout or not output.is_file() or output.read_bytes() != bytes.fromhex('4e71')
            or callbacks['first_refusal'] is not None or callbacks['attempts']):
        raise ValueError('strict mode rejected or changed callback-free package emission')
    return {'command': command, 'exit_code': code, 'callbacks': callbacks,
            'elapsed_seconds': elapsed, 'output_sha256': digest(output.read_bytes())}


def _attempt_counts(attempts, key):
    counts = {}
    for attempt in attempts:
        label = attempt[key]
        counts[label] = counts.get(label, 0) + attempt['count']
    return counts


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--blocks', default='8,32,128')
    parser.add_argument('--output', type=Path, help='new output directory (default: ignored build directory)')
    args = parser.parse_args()
    if os.name != 'posix':
        parser.error('this runner requires POSIX process-group termination')
    try:
        sizes = [int(value) for value in args.blocks.split(',')]
        if len(sizes) != 3 or sizes != sorted(set(sizes)) or not 1 <= min(sizes) <= max(sizes) <= 128:
            raise ValueError('provide three increasing block counts in 1..128')
        if args.output:
            directory = args.output.resolve()
            directory.mkdir(parents=True, exist_ok=False)
        else:
            (ROOT / 'build').mkdir(exist_ok=True)
            directory = Path(tempfile.mkdtemp(prefix='package-boundaries-', dir=ROOT / 'build'))
        summary = {'status': 'incomplete', 'workload': 'existing-cross-family-selection',
                   'host': platform.platform(), 'blocks': sizes,
                   'measurement_limits_seconds': {'invocation': 60, 'batch': 300},
                   'runner_sha256': digest(Path(__file__).read_bytes()),
                   'source_revision': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
                   'tracked_diff_sha256': digest(subprocess.check_output(['git', 'diff', 'HEAD'], cwd=ROOT)),
                   'limitations': ['Fixed-order bounded classification run, not a performance qualification.',
                                   'VM work/profile counters classify observed execution; they do not price helper internals.',
                                   'No native execution or full-self-host claim.']}
        try:
            binary, summary['build'] = vm.build_binary(directory)
            summary['binary_sha256'] = summary['build']['binary_sha256']
            summary['package_sha256'] = summary['build']['package_sha256']
            start = time.monotonic()
            summary['cases'] = []
            budget = vm.Budget(300)
            run(binary, directory, sizes, budget, summary['cases'])
            summary['shared_control'] = shared_control(binary, directory, budget)
            summary['measurement_seconds'] = time.monotonic() - start
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

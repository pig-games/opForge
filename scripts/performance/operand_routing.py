#!/usr/bin/env python3
"""Differential probe for shared operand parsing and callback routing."""
from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import platform
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
VM_PATH = Path(__file__).with_name('vm_efficiency.py')
SPEC = importlib.util.spec_from_file_location('vm_efficiency', VM_PATH)
vm = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(vm)
BOUNDARIES_PATH = Path(__file__).with_name('package_boundaries.py')
SPEC = importlib.util.spec_from_file_location('package_boundaries', BOUNDARIES_PATH)
boundaries = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(boundaries)


def digest(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def compact_cases():
    """Small directive/expression and addressing cases beyond the scale sweep."""
    common = '.cpu {cpu}\n.org $1000\n.byte 1+2, (4*5)-3\n.word $1234+1\n.end\n'
    cases = [
        {'name': 'directives-m6502', 'cpu': 'm6502', 'source': common.format(cpu='m6502')},
        {'name': 'directives-z80', 'cpu': 'z80', 'source': common.format(cpu='z80')},
        {'name': 'directives-m68000', 'cpu': 'm68000', 'source':
         '.cpu m68000\n.org $1000\n.byte 1+2\n.word (4*5)-3\n.long $1234+1\n.end\n'},
        {'name': 'addressing-m68000', 'cpu': 'm68000', 'source':
         '.org $1000\n move.w 4(a0,d1.w),d0\n.end\n'},
        {'name': 'malformed-expression-m6502', 'cpu': 'm6502', 'source':
         '.cpu m6502\n.byte (1+\n.end\n', 'negative': True},
        {'name': 'malformed-addressing-m68000', 'cpu': 'm68000', 'source':
         '.cpu m68000\n move.w 4(a0,d1.w*3),d0\n.end\n', 'negative': True},
        {'name': 'directive-hash-expression-m68000', 'cpu': 'm68000', 'source':
         '.byte #1\n.end\n'},
        {'name': 'directive-grouped-expression-m68000', 'cpu': 'm68000', 'source':
         '.word (1+2)\n.end\n'},
        {'name': 'directive-parenthesized-list-m68000', 'cpu': 'm68000', 'source':
         '.byte (1,2)\n.end\n', 'negative': True},
        {'name': 'directive-postincrement-spelling-m68000', 'cpu': 'm68000', 'source':
         '.word (a0)+\n.end\n', 'negative': True},
        {'name': 'org-grouped-expression-m68000', 'cpu': 'm68000', 'source':
         '.org (1+2)*3\n.byte 7\n.end\n'},
        {'name': 'directive-grouped-and-list-expressions-m68000', 'cpu': 'm68000', 'source':
         '.byte 1, (2+3), 4\n.word ((1+2)*3), 9\n.end\n'},
    ]
    return cases


def all_cases(sizes):
    cases = []
    for cpu in vm.FAMILIES:
        for blocks in sizes:
            source, expected = vm.workload(cpu, blocks)
            cases.append({'name': f'{cpu}-{blocks}', 'cpu': cpu, 'blocks': blocks,
                          'source': source, 'expected': expected, 'kind': 'scale'})
    for case in compact_cases():
        case.update(kind='compact', blocks=1)
        cases.append(case)
    return cases


def invoke(binary, package, case, directory, mode, budget, profile=False):
    output = directory / f'{mode}.bin'
    output.unlink(missing_ok=True)
    (directory / 'input.asm').write_text(case['source'])
    command = [str(binary), '--cpu', case['cpu'], '--infile', 'input.asm', '--bin', output.name,
               '--opasm-package', str(package)]
    env = boundaries.clean_env(profile=profile, callback_mode=mode)
    code, stdout, stderr, elapsed = vm.run_process(command, directory, env, budget, cap=60)
    text = stderr.decode(errors='replace')
    normalized = boundaries.normalize_diagnostics(text)
    callbacks = boundaries.parse_callbacks(text, mode) if mode != 'baseline' else None
    negative = case.get('negative', False)
    data = output.read_bytes() if output.is_file() else b''
    if mode == 'refuse':
        if callbacks['attempts']:
            if code != 1 or output.exists() or not callbacks['first_refusal']:
                raise ValueError(f"{case['name']}: strict mode did not refuse a callback")
        elif negative:
            if code != 1 or stdout or output.exists() or 'ERROR' not in normalized:
                raise ValueError(f"{case['name']}: malformed strict input lost its diagnostic")
        elif (code or stdout or callbacks['first_refusal'] is not None or not output.is_file()):
            raise ValueError(f"{case['name']}: callback-free strict mode failed")
        elif case.get('expected') is not None and data != case['expected']:
            raise ValueError(f"{case['name']}: strict output differs from independent workload bytes")
        return {'exit_code': code, 'elapsed_seconds': elapsed, 'output_sha256': digest(data),
                'output_bytes': len(data), 'diagnostics': text,
                'normalized_diagnostics': normalized, 'callbacks': callbacks,
                'command': command}
    if negative:
        if code != 1 or stdout or output.exists() or 'ERROR' not in normalized:
            raise ValueError(f"{case['name']}/{mode}: malformed input did not fail with diagnostics")
    elif code or stdout or not output.is_file():
        raise ValueError(f"{case['name']}/{mode}: assembly failed (exit {code}): {text}")
    if not negative and case.get('expected') is not None and data != case['expected']:
        raise ValueError(f"{case['name']}/{mode}: output differs from independent workload bytes")
    result = {'exit_code': code, 'elapsed_seconds': elapsed, 'output_sha256': digest(data),
              'output_bytes': len(data), 'diagnostics': text,
              'normalized_diagnostics': normalized, 'callbacks': callbacks,
              'command': command}
    if profile and not negative and mode != 'refuse':
        result['profile'] = vm.parse_profile(text)
        result['vm_work'] = vm.parse_vm_work(text, case['blocks'], len(case['source'].encode()),
                                              len(case['source'].splitlines()))
        result['work_counts'] = {
            label: sum(row['count'] or 0 for row in result['profile'] if row['label'] == label)
            for label in ('vm.parse', 'vm.parse_cache_hit', 'vm.encode', 'vm.model.bootstrap')}
    return result


def executable_info(binary, package):
    return {'binary': str(binary), 'binary_sha256': digest(binary.read_bytes()),
            'binary_bytes': binary.stat().st_size, 'package': str(package),
            'package_sha256': digest(package.read_bytes()), 'package_bytes': package.stat().st_size}


def validate_reference_identity(saved, reference):
    # Output comparisons and paired timing must use the same preserved executable
    # and canonical package, even if their filesystem paths have changed.
    if any(saved.get('reference', {}).get(key) != reference[key]
           for key in ('binary_sha256', 'package_sha256')):
        raise ValueError('reference binary/package differs from the captured oracle')


def capture_reference(binary, package, cases, output, budget):
    rows = []
    for case in cases:
        workdir = output / case['name']
        workdir.mkdir()
        modes = {}
        for mode in ('baseline', 'report'):
            modes[mode] = invoke(binary, package, case, workdir, mode, budget,
                                 profile=(case['kind'] == 'scale'))
        if modes['baseline']['normalized_diagnostics'] != modes['report']['normalized_diagnostics']:
            raise ValueError(f"{case['name']}: report instrumentation changed diagnostics")
        if modes['baseline']['output_sha256'] != modes['report']['output_sha256']:
            raise ValueError(f"{case['name']}: report mode changed output")
        strict = invoke(binary, package, case, workdir, 'refuse', budget)
        modes['refuse'] = strict
        rows.append({'name': case['name'], 'cpu': case['cpu'], 'kind': case['kind'],
                     'blocks': case['blocks'], 'negative': case.get('negative', False),
                     'source': case['source'], 'source_sha256': digest(case['source'].encode()),
                     'expected_sha256': (digest(case['expected']) if case.get('expected') is not None else None),
                     'modes': modes})
        counts = modes['report']['callbacks']['attempts']
        print(f"reference {case['name']:32} {sum(x['count'] for x in counts):5} callback attempts; "
              f"{modes['baseline']['output_bytes']} bytes; strict {'passed' if not strict['callbacks']['attempts'] else 'refused'}",
              flush=True)
    return rows


def compare_candidate(binary, package, cases, reference, output, budget):
    if len(cases) != len(reference):
        raise ValueError('candidate case set does not match reference')
    rows, mismatches = [], []
    for case, baseline in zip(cases, reference):
        if (baseline['name'] != case['name'] or baseline['cpu'] != case['cpu']
                or baseline['source_sha256'] != digest(case['source'].encode())):
            raise ValueError(f"{case['name']}: candidate source differs from captured reference")
        workdir = output / case['name']
        workdir.mkdir(exist_ok=True)
        report = invoke(binary, package, case, workdir, 'report', budget,
                        profile=(case['kind'] == 'scale'))
        base = baseline['modes']['baseline']
        same_bytes = report['output_sha256'] == base['output_sha256']
        same_diagnostics = report['normalized_diagnostics'] == base['normalized_diagnostics']
        if not same_bytes or not same_diagnostics:
            mismatches.append({'name': case['name'], 'same_bytes': same_bytes,
                               'same_normalized_diagnostics_and_spans': same_diagnostics})
        strict = invoke(binary, package, case, workdir, 'refuse', budget)
        if not case.get('negative', False):
            if strict['callbacks']['attempts']:
                if strict['exit_code'] != 1 or strict['output_bytes']:
                    raise ValueError(f"{case['name']}: strict mode had callbacks without failing closed")
            elif strict['exit_code'] or strict['output_sha256'] != base['output_sha256']:
                raise ValueError(f"{case['name']}: callback-free strict output differs from baseline")
        rows.append({'name': case['name'], 'same_bytes': same_bytes,
                     'same_normalized_diagnostics_and_spans': same_diagnostics,
                     'candidate_report': report, 'candidate_refuse': strict})
        attempts = report['callbacks']['attempts']
        strict_status = ('refused' if strict['callbacks']['attempts'] else
                         'passed callback-free')
        comparison = ('bytes and diagnostics match' if same_bytes and same_diagnostics else
                      f'bytes match={same_bytes}, diagnostics/spans match={same_diagnostics}')
        print(f"candidate {case['name']:32} {sum(x['count'] for x in attempts):5} callback attempts; "
              f'{comparison}; strict {strict_status}', flush=True)
    return rows, mismatches


def measure_unprofiled(binary_pair, cases, output, budget):
    """Three bounded unprofiled samples per side for the existing 32-block cases."""
    selected = [case for case in cases if case['kind'] == 'scale' and case['blocks'] == 32]
    records = {case['name']: {'source_sha256': digest(case['source'].encode()),
                              'output_sha256': digest(case['expected']), 'samples_seconds': {}}
               for case in selected}
    directories = {}
    for case in selected:
        directory = output / f"timing-{case['name']}"
        directory.mkdir()
        (directory / 'input.asm').write_text(case['source'])
        directories[case['name']] = directory
    for label, (binary, package) in binary_pair.items():
        for case in selected:
            result = invoke(binary, package, case, directories[case['name']], 'baseline', budget)
            if result['output_sha256'] != records[case['name']]['output_sha256']:
                raise ValueError(f"{case['name']}/{label}: timing run changed output bytes")
    order = ('reference', 'candidate', 'candidate', 'reference', 'reference', 'candidate')
    for label in order:
        binary, package = binary_pair[label]
        for case in selected:
            result = invoke(binary, package, case, directories[case['name']], 'baseline', budget)
            records[case['name']]['samples_seconds'].setdefault(label, []).append(
                result['elapsed_seconds'])
    for row in records.values():
        for label, samples in row['samples_seconds'].items():
            row.setdefault('median_seconds', {})[label] = sorted(samples)[len(samples) // 2]
        row['candidate_over_reference'] = (
            row['median_seconds']['candidate'] / row['median_seconds']['reference'])
    return records


def main():
    started = time.monotonic()
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--blocks', default='8,32,128')
    parser.add_argument('--output', type=Path, help='new result directory (default: unique ignored build directory)')
    parser.add_argument('--reference-binary', type=Path, required=True)
    parser.add_argument('--reference-package', type=Path, required=True)
    parser.add_argument('--candidate-binary', type=Path)
    parser.add_argument('--candidate-package', type=Path)
    parser.add_argument('--reference-summary', type=Path)
    args = parser.parse_args()
    if os.name != 'posix':
        parser.error('this runner requires POSIX process-group termination')
    summary = None
    try:
        for name in ('output', 'reference_binary', 'reference_package', 'candidate_binary',
                     'candidate_package', 'reference_summary'):
            value = getattr(args, name)
            if value is not None:
                setattr(args, name, value.resolve())
        sizes = [int(value) for value in args.blocks.split(',')]
        if len(sizes) != 3 or sizes != sorted(set(sizes)) or not 1 <= min(sizes) <= max(sizes) <= 128:
            raise ValueError('provide three increasing block counts in 1..128')
        if args.output is None:
            (ROOT / 'build').mkdir(exist_ok=True)
            args.output = Path(tempfile.mkdtemp(prefix='operand-routing-', dir=ROOT / 'build'))
        else:
            args.output.mkdir(parents=True, exist_ok=False)
        cases = all_cases(sizes)
        budget = vm.Budget(300)
        summary = {'status': 'incomplete', 'host': platform.platform(), 'blocks': sizes,
                   'measurement_limits_seconds': {'invocation': 60, 'batch': 300},
                   'reference': executable_info(args.reference_binary, args.reference_package),
                   'candidate': None, 'limitations': [
                       'Fixed-order bounded comparison, not a statistically qualified speedup.',
                       'VM dispatch counts exclude helper internals, allocations and table work.',
                       'No native execution or full-self-host performance claim.']}
        if args.candidate_binary is None:
            if args.reference_summary:
                raise ValueError('--reference-summary is only valid with --candidate-binary')
            summary['reference_cases'] = capture_reference(
                args.reference_binary, args.reference_package, cases, args.output, budget)
            summary['status'] = 'reference-captured'
        else:
            if args.reference_summary is None or args.candidate_package is None:
                raise ValueError('candidate evaluation requires --reference-summary and --candidate-package')
            saved = json.loads(args.reference_summary.read_text())
            if saved.get('status') != 'reference-captured' or len(saved.get('reference_cases', [])) != len(cases):
                raise ValueError('reference summary is incomplete or has a different case count')
            validate_reference_identity(saved, summary['reference'])
            summary['reference_summary'] = str(args.reference_summary.resolve())
            summary['candidate'] = executable_info(args.candidate_binary, args.candidate_package)
            summary['candidate_cases'], summary['mismatches'] = compare_candidate(
                args.candidate_binary, args.candidate_package, cases, saved['reference_cases'],
                args.output, budget)
            summary['unprofiled_32_block_timings'] = measure_unprofiled(
                {'reference': (args.reference_binary, args.reference_package),
                 'candidate': (args.candidate_binary, args.candidate_package)},
                cases, args.output, budget)
            summary['status'] = 'passed' if not summary['mismatches'] else 'mismatched'
        summary['runner_sha256'] = digest(Path(__file__).read_bytes())
        summary['source_revision'] = subprocess.check_output(
            ['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip()
        summary['measurement_seconds'] = time.monotonic() - started
        (args.output / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
        if summary['status'] == 'mismatched':
            print(f"COMPLETED WITH DIFFERENCES: {args.output / 'summary.json'}")
            return 1
        print(f"PASS: {summary['status']} {args.output / 'summary.json'}")
        return 0
    except (ValueError, OSError, TimeoutError, subprocess.TimeoutExpired) as exc:
        if summary is not None:
            summary.update(status='incomplete', error=str(exc),
                           measurement_seconds=time.monotonic() - started)
            (args.output / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
        print(f'FAIL: {exc}')
        return 1


if __name__ == '__main__':
    raise SystemExit(main())

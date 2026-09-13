#!/usr/bin/env python3
"""Small completed-output comparison; no self-host run or runtime changes."""
import argparse
import json
import os
import platform
from pathlib import Path
import re
import shutil
import signal
import statistics
import subprocess
import time

import vm_efficiency as base

TEST = 'tests::native_runtime_comparison::native_runtime_comparison_fs_uae'


def native(binary, source, package, budget, log, profile="off"):
    env = dict(base.clean_env(), OPFORGE_COMPARE_SOURCE=str(source),
               OPFORGE_COMPARE_PACKAGE=str(package), OPFORGE_FS_UAE_SMOKE='1',
               OPFORGE_COMPARE_PROFILE=profile,
               OPFORGE_FS_UAE_POLL_MS='20', OPFORGE_FS_UAE_TIMEOUT_MS='35000', OPFORGE_FS_UAE_POST_START_TIMEOUT_MS='35000')
    env.update({key: value for key, value in os.environ.items()
                if key.startswith('OPFORGE_FS_UAE_') and key not in {
                    'OPFORGE_FS_UAE_POLL_MS', 'OPFORGE_FS_UAE_TIMEOUT_MS',
                    'OPFORGE_FS_UAE_POST_START_TIMEOUT_MS', 'OPFORGE_FS_UAE_SMOKE'}})
    # The existing runner owns normal cleanup. Also cover external timeout/unwind
    # of this serial test process; only newly created runner trees are candidates.
    pattern = 'fs-uae-hunk-smoke-opforge_cli-*'
    before = set((base.ROOT / 'target').glob(pattern))
    try:
        code, stdout, stderr, elapsed = base.run_process(
            [str(binary), TEST, '--exact', '--ignored', '--nocapture'], base.ROOT, env, budget)
        text = (stdout + stderr).decode(errors='replace')
        log.write_text(text)
        rows = [json.loads(line.split('RUNTIME_COMPARISON ', 1)[1])
                for line in text.splitlines() if line.startswith('RUNTIME_COMPARISON ')]
        if code or len(rows) != 1 or rows[0]['guest_exit'] != 0:
            raise ValueError(f'native completion/parity failed: {log}')
        if rows[0].get('profile') != profile or (profile != 'off' and not rows[0].get('counters')):
            raise ValueError('requested native telemetry was not decoded from the completed run')
        return dict(rows[0], invocation_seconds=elapsed)
    finally:
        for tree in set((base.ROOT / 'target').glob(pattern)) - before:
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


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rust-binary', type=Path, required=True)
    parser.add_argument('--native-test', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--cpus', nargs='+', choices=('m6502', 'm68020'), default=['m6502', 'm68020'])
    parser.add_argument('--blocks', nargs='+', type=int, choices=(8, 16, 32), default=[8, 32])
    parser.add_argument('--native-profile', choices=('off', 'runtime', 'all'), default='off', help='collect existing native counters; timings are instrumented')
    args = parser.parse_args()
    rust, native_test = args.rust_binary.resolve(strict=True), args.native_test.resolve(strict=True)
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    package = out / 'runtime.opasm'
    shutil.copyfile(base.ROOT / 'native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm', package)
    template = Path(os.environ['OPFORGE_FS_UAE_CONFIG_TEMPLATE']).resolve(strict=True)
    report = {'schema_version': 1, 'complete': False, 'cases': [],
              'head': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=base.ROOT, text=True).strip(),
              'working_diff_sha256': base.digest(subprocess.check_output(['git', 'diff', 'HEAD', '--binary'], cwd=base.ROOT)),
              'package_bytes': package.stat().st_size, 'package_sha256': base.digest(package.read_bytes()),
              'rust_binary_bytes': rust.stat().st_size, 'rust_binary_sha256': base.digest(rust.read_bytes()),
              'native_test_sha256': base.digest(native_test.read_bytes()),
              'host': platform.platform(),
              'workload_generator_sha256': base.digest(Path(base.__file__).read_bytes()),
              'runner_sha256': base.digest(Path(__file__).read_bytes()),
              'emulator_template': template.read_text(),
              'native_profile': args.native_profile,
              'runner_config_overrides': {'zorro_iii_memory_kib': 65536},
              'limits': {'batch_seconds': 300, 'invocation_seconds': 60, 'guest_seconds': 35, 'poll_ms': 20},
              'limitations': ['Native is one observation per case, not a statistically stable speed ratio.',
                             'Rust process time and guest START-to-DONE include loading/output; neither is VM-only time.',
                             'Native interval is host-observed and polled; emulator boot is excluded from it.',
                             'Emulator configuration is not physical 68020/2MB calibration. No peak-memory measurement.']}
    budget = base.Budget(300)
    start = time.monotonic()
    try:
        for cpu in args.cpus:
            for blocks in args.blocks:
                folder = out / f'{cpu}-{blocks}'
                folder.mkdir()
                source, expected = base.workload('m68000' if cpu == 'm68020' else cpu, blocks)
                source = source.replace('.cpu m68000\n', '.cpu m68020\n', 1)
                (folder / 'input.asm').write_text(source)
                row = {'cpu': cpu, 'blocks': blocks, 'assembly_instructions': blocks * 5,
                       'source_bytes': len(source.encode()), 'source_lines': len(source.splitlines()),
                       'source_sha256': base.digest(source.encode()), 'output_bytes': len(expected),
                       'output_sha256': base.digest(expected), 'rust': {}}
                report['cases'].append(row)
                for mode in ('auto', 'generic'):
                    # Both CLIs start on m6502; the identical source selects the target.
                    base.assemble(rust, folder, 'm6502', budget, expected, tokenizer_mode=mode)
                    samples = [base.assemble(rust, folder, 'm6502', budget, expected,
                                             tokenizer_mode=mode)[0] for _ in range(3)]
                    _, profile, command = base.assemble(rust, folder, 'm6502', budget, expected,
                                                       profile=True, tokenizer_mode=mode)
                    (folder / f'{mode}-profile.txt').write_text(profile)
                    work = base.parse_vm_work(profile, blocks, len(source.encode()), len(source.splitlines()))
                    row['rust'][mode] = {'seconds': samples, 'median_seconds': statistics.median(samples),
                                         'command': command, 'work': work['aggregate']}
                try:
                    receipt = native(native_test, folder / 'input.asm', package, budget, folder / 'native.log', args.native_profile)
                    if bytes(receipt.pop('exact_output')) != expected:
                        raise ValueError('live native/Rust result differs from independent workload bytes')
                    row['native'] = receipt
                except (ValueError, TimeoutError, subprocess.TimeoutExpired) as error:
                    row['native_error'] = str(error)
                (out / 'summary.json').write_text(json.dumps(report, indent=2) + '\n')
        report['complete'] = all('native' in case for case in report['cases'])
    except (ValueError, TimeoutError, subprocess.TimeoutExpired) as error:
        report['error'] = str(error)
    finally:
        report['batch_seconds'] = time.monotonic() - start
        (out / 'summary.json').write_text(json.dumps(report, indent=2) + '\n')
    print(out / 'summary.json')
    return 0 if report['complete'] else 1


if __name__ == '__main__':
    raise SystemExit(main())

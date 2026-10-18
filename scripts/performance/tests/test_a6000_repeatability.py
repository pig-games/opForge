import argparse
import contextlib
import io
import json
import shutil
from pathlib import Path
import sys
import tempfile
import unittest
from unittest import mock

sys.path.insert(0, str(Path(__file__).parents[1]))
import run_a6000_repeatability as repeat
import test_a6000_comparison as fixtures


class RepeatabilityTests(unittest.TestCase):
    def inputs(self, root):
        fixture = fixtures.FixedWorkloadComparisonTests('runTest')
        release, _, _ = fixture.exports(root)
        memory = fixture.memory_export(release, root / 'memory', 'current')
        manifest, files, oracle, _ = repeat.runner.load_bundle(release)
        frozen = root / 'frozen'
        frozen.mkdir()
        spec = {'schema': 1, 'files': []}
        for key, data in [('image', b'old image'), ('oracle', oracle)]:
            (frozen / key).write_bytes(data)
            spec[key] = dict(path=key, digest=repeat.runner.fnv(data))
        sources = {k: v for k, v in files.items() if k.startswith('src/')}
        for i in range(214 - len(sources)):
            sources[f'src/frozen{i}.asm'] = f'; frozen {i}'.encode()
        for name, data in sorted(sources.items()):
            path = frozen / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(data)
            spec['files'].append(dict(path=name, guest_path=name, digest=repeat.runner.fnv(data)))
        spec['staged_inputs_sha256'] = repeat.comparison.digest_parts([
            part for k, v in sorted(sources.items()) for part in (k.encode(), v)])
        path = frozen / 'case.json'
        path.write_text(json.dumps(spec))
        return argparse.Namespace(frozen_case=path, release_bundle=release, memory_bundle=memory,
                                  output=root / 'output', state=root / 'state.json', dry_run=False,
                                  host=None, volume='Development', timeout=3600)

    def test_eight_cases_order_and_identical_frozen_inputs(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            cases = repeat.prepare_cases(args)
            self.assertEqual([(c['role'], c['storage'], c['repetition']) for c in cases],
                             [('frozen_release', s, i) for i in range(1, 4) for s in ('resident', 'disk')]
                             + [('current_source_selfhost', 'disk', 1), ('frozen_memory', 'disk', 1)])
            original = repeat.runner.load_bundle(args.release_bundle)[0]
            self.assertEqual(cases[6]['manifest']['source_manifest_digest'], original['source_manifest_digest'])
            self.assertEqual(cases[6]['manifest']['source_root'], original['source_root'])
            frozen_sources = {k: v for k, v in cases[0]['files'].items() if k.startswith('src/')}
            expected_identity = repeat.runner.fnv(b''.join(
                k[4:].encode() + b'\0' + v + b'\0' for k, v in sorted(frozen_sources.items())))
            self.assertEqual(cases[0]['manifest']['source_manifest_digest'], expected_identity)
            self.assertNotIn('source_root', cases[0]['manifest'])
            self.assertEqual(cases[0]['manifest']['frozen_case'], str(args.frozen_case))
            self.assertEqual(cases[0]['metadata']['workload_sha256'], cases[7]['metadata']['workload_sha256'])
            self.assertNotEqual(cases[0]['metadata']['workload_sha256'], cases[6]['metadata']['workload_sha256'])
            for case in cases[:6]:
                self.assertEqual(case['files'], cases[0]['files'])
                self.assertEqual(case['oracle'], cases[0]['oracle'])
                self.assertEqual(case['manifest']['source_files'], 214)
                self.assertEqual(case['metadata']['workload_sha256'], cases[0]['metadata']['workload_sha256'])
                self.assertNotIn('candidate_source_selfhost_proven', case['metadata'])
                self.assertEqual(case['manifest']['native_validation'], 'not_run')
                self.assertEqual('--capture-dir scratch' in case['manifest']['command'], case['storage'] == 'disk')
            self.assertEqual(cases[0]['case_sha256'], cases[2]['case_sha256'])
            self.assertNotEqual(cases[0]['case_sha256'], cases[1]['case_sha256'])

    def test_invalid_checksum_package_and_traversal_fail_before_network(self):
        for mutation in ('checksum', 'package', 'traversal'):
            with self.subTest(mutation=mutation), tempfile.TemporaryDirectory() as directory:
                args = self.inputs(Path(directory))
                spec = json.loads(args.frozen_case.read_text())
                if mutation == 'checksum':
                    spec['oracle']['digest'] = 'wrong'
                elif mutation == 'traversal':
                    spec['image']['path'] = '../outside'
                else:
                    row = next(row for row in spec['files'] if '/packages/' in row['guest_path'])
                    data = b'wrong package'
                    (args.frozen_case.parent / row['path']).write_bytes(data)
                    row['digest'] = repeat.runner.fnv(data)
                    spec['staged_inputs_sha256'] = repeat.comparison.digest_parts([
                        part for row in spec['files'] for part in (row['guest_path'].encode(),
                        (args.frozen_case.parent / row['path']).read_bytes())])
                args.frozen_case.write_text(json.dumps(spec))
                with self.assertRaises(ValueError):
                    repeat.prepare_cases(args)

    def test_disk_shell_limit_is_preflighted_before_claim_or_network(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            for bundle in (args.release_bundle, args.memory_bundle):
                path = bundle / 'manifest.json'
                manifest = json.loads(path.read_text())
                prefix = manifest['command'] + ' -I src/'
                command = prefix + 'a' * (238 - len(prefix))
                manifest['command'] = command
                path.write_text(json.dumps(manifest))
                (bundle / 'command.txt').write_text(command)
            repeat.runner.guest_script('Development:opforge-' + '0' * 12,
                                       '0' * 32 + ' ' + 'a' * 64, command)
            argv = [part for key in ('frozen_case', 'release_bundle', 'memory_bundle', 'output')
                    for part in ('--' + key.replace('_', '-'), str(getattr(args, key)))]
            with (mock.patch.object(repeat.runner, 'run_case', side_effect=AssertionError('runner called')) as run,
                  mock.patch.object(repeat.subprocess, 'run', side_effect=AssertionError('network'))):
                with self.assertRaisesRegex(ValueError, 'Shell command exceeds 255'):
                    repeat.main(argv)
            run.assert_not_called()
            self.assertFalse((args.output / 'state.json').exists())

    def test_serial_success_and_no_reentry(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            cases = repeat.prepare_cases(args)
            calls = []
            def run(*pos, before_execute, **kw):
                self.assertTrue(args.state.is_file())
                host = json.loads(args.state.read_text())['host_state']
                self.assertGreater(host['unix_seconds'], 0)
                self.assertGreater(host['monotonic_seconds'], 0)
                self.assertTrue(host['utc'].endswith('+00:00'))
                self.assertTrue(host['platform'])
                self.assertTrue(host['python_version'])
                calls.append(pos[5])
                before_execute(dict(remote_directory='Development:run', run_uuid=str(len(calls)),
                                    local_results=directory, remote_host='host'))
                return dict(success=True, guest_protocol_completed=True, guest_overall_seconds=10), Path(directory)
            with mock.patch.object(repeat.runner, 'run_case', side_effect=run):
                self.assertTrue(repeat.execute_cases(args, cases))
                with self.assertRaises(FileExistsError):
                    repeat.execute_cases(args, cases)
            self.assertEqual(calls, [c['case_sha256'] for c in cases])
            state = json.loads(args.state.read_text())
            self.assertEqual(state['status'], 'completed')
            self.assertEqual(len(state['runs']), 8)
            self.assertEqual(set(state['summary']['groups']), {'resident', 'disk'})

    def test_incomplete_or_exception_leaves_durable_guard_without_retry(self):
        for failure in (KeyboardInterrupt(), TimeoutError('transport'), 'incomplete', 'completed_failure'):
            with self.subTest(failure=failure), tempfile.TemporaryDirectory() as directory:
                args = self.inputs(Path(directory))
                cases = repeat.prepare_cases(args)
                def run(*pos, before_execute, **kw):
                    before_execute(dict(remote_directory='Development:unresolved', run_uuid='uuid',
                                        local_results=directory, remote_host='host'))
                    if isinstance(failure, BaseException):
                        raise failure
                    return dict(success=False, guest_protocol_completed=failure == 'completed_failure'), Path(directory)
                with mock.patch.object(repeat.runner, 'run_case', side_effect=run) as execute:
                    if failure == 'completed_failure':
                        self.assertFalse(repeat.execute_cases(args, cases))
                    else:
                        with self.assertRaises(BaseException):
                            repeat.execute_cases(args, cases)
                    with self.assertRaises(FileExistsError):
                        repeat.execute_cases(args, cases)
                execute.assert_called_once()
                state = json.loads(args.state.read_text())
                self.assertEqual(state['status'], 'failed' if failure == 'completed_failure' else 'unresolved')
                if failure != 'completed_failure':
                    self.assertEqual(state['active']['remote_directory'], 'Development:unresolved')
                    self.assertEqual(state['active']['run_uuid'], 'uuid')

    def test_dry_run_never_calls_runner_or_network_or_claims_state(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            argv = [part for key in ('frozen_case', 'release_bundle', 'memory_bundle', 'output')
                    for part in ('--' + key.replace('_', '-'), str(getattr(args, key)))] + ['--dry-run']
            with (mock.patch.object(repeat.runner, 'run_case', side_effect=AssertionError('runner called')),
                  mock.patch.object(repeat.subprocess, 'run', side_effect=AssertionError('network')),
                  contextlib.redirect_stdout(io.StringIO()) as output):
                self.assertEqual(repeat.main(argv), 0)
            self.assertIn('Run once in your macOS Terminal:', output.getvalue())
            self.assertFalse((args.output / 'state.json').exists())

    def test_callback_follows_transfer_verification_and_precedes_execute(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.inputs(Path(directory))
            case = repeat.prepare_cases(args)[0]
            transferred = []
            commands = []
            def transport(command, **kwargs):
                commands.append(command)
                if command[0] == 'acp' and command[2].startswith('/'):
                    transferred.append(Path(command[2]))
                elif command[0] == 'acp':
                    shutil.copytree(transferred[0], Path(command[3]) / transferred[0].name)
            def callback(evidence):
                self.assertEqual(len(transferred), 1)
                copied = Path(evidence['local_results']) / 'copy-check' / transferred[0].name
                repeat.runner.verify_files(copied, case['files'])
                self.assertEqual(evidence['case_sha256'], case['case_sha256'])
                self.assertEqual(evidence['remote_host'], 'a6000.local')
                self.assertTrue(evidence['run_uuid'])
                raise RuntimeError('callback stopped execution')
            args.host = 'a6000.local'
            with (mock.patch.object(repeat.runner.shutil, 'which', side_effect=lambda name: name),
                  mock.patch.object(repeat.runner.subprocess, 'run', side_effect=transport),
                  contextlib.redirect_stdout(io.StringIO())):
                with self.assertRaisesRegex(RuntimeError, 'callback stopped'):
                    repeat.runner.run_case(args, case['bundle'], case['manifest'], case['files'],
                                           case['oracle'], case['case_sha256'], before_execute=callback)
            self.assertFalse(any(any('Execute ' in str(part) for part in command) for command in commands))

    def test_summary_requires_three_successful_completed_clocks(self):
        rows = [dict(role='frozen_release', storage='resident', result=dict(
            success=True, guest_protocol_completed=True, guest_overall_seconds=n)) for n in (8, 10, 12)]
        self.assertEqual(repeat.summary(rows[:2])['groups'], {})
        self.assertEqual(repeat.summary(rows)['groups']['resident']['median'], 10)
        rows[1]['result']['success'] = False
        self.assertEqual(repeat.summary(rows)['groups'], {})


if __name__ == '__main__':
    unittest.main()

# Workflow notebook

A shared scratchpad for Erik and agents: recurring friction, promising approaches
not yet incorporated, and ideas worth trying. Add a few sentences when useful.
We revisit these at natural checkpoints using the [workflow](README.md#learn-as-we-work),
then remove notes once incorporated or obsolete. These are observations, not rules.

- **Repeated workspace approvals.** During the workflow cleanup, editing the sibling
  worktree repeatedly required escalation because it was outside the session's
  writable roots. Worth exploring: start future work in a session whose writable
  workspace includes the chosen worktree. Verify that setup before assuming it
  removes the friction; do not bypass permissions.

- **Staging failure can leave a staged result.** The commit helper reported an
  ignored-directory error while staging tracked historical deletions, although
  the selected changes had entered the index. We inspected the index before
  continuing. Possible improvement: make the helper report partial completion
  clearly and handle tracked deletions under ignored directories reliably.

- **Host test startup delays.** Fresh Rust debug test executables have sometimes
  produced no harness output for a long interval; one B3 host-only invocation hit
  its 60-second deadline before any test output. Keep build/startup delays separate
  from workload and guest timings. Identify the cause before changing automation
  or raising timeouts; a host startup timeout is not native failure evidence.
  This recurred during shared-data qualification even with valid executable
  signatures; parallel test-list probing did not reliably resolve it.

- **Historical snapshots in broad checks.** The shared-data repair exposed stale
  B3 source assertions and tests pinning exact source bytes/import/export counts.
  Capacity tests now retain their real limits and report measured usage without
  rejecting every valid edit. The native inventory still pins whole-file hashes;
  consider whether checks of responsibility boundaries can retain its value with
  less refresh work. Do not silently raise capacity limits to pass a test.

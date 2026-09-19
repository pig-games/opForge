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

- **Exercise assembly language features in real output formats.** The first native
  prepared-table implementation exposed two struct inconsistencies: the documented
  numeric struct-size expression is rejected by the instruction expression path,
  and struct-derived constants were treated as relocatable in Hunk output. Keep
  a small Hunk regression alongside the native use; named fields still express
  the layout, with frame size derived from the final field until the scalar-size
  contract is resolved.
  The binary-source harness exposed a further Hunk relocation gap: an absolute
  address written as `buffer + imported.Struct.field` was emitted without the
  buffer relocation. Loading the buffer address with `lea` and using the named
  field as an address-register displacement avoids that form. The authorized
  debugger localized the hang; this workaround does not fix the assembler's
  relocation handling. Cover this expression form in the future Hunk regression.

- **Document checks still depend on magic phrases.** M2's FS-UAE guide update
  exposed a check that rejects an existing valid invocation unless the document
  contains a literal allowance phrase. The guide now satisfies that check;
  consider replacing this wording requirement with validation of actual commands
  and execution safeguards when next improving workflow automation.

- **Guest startup dominates tiny native contracts.** M3's individual boundary
  checks took roughly 25 seconds per invocation, while small guest work completed
  in under a second. Consider batching independent cases through the existing
  runner while retaining each case's fresh challenge, exit, diagnostic/output and
  cleanup proof. This is an automation opportunity, not a reason to raise deadlines.

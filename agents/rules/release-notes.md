# Release notes

Use this guide for release-bearing work. Update `RELEASE_NOTES_v*.md` when the
agreed task changes the upcoming release, keeping entries scoped to that release
and its user-visible or contributor-visible impact. Routine checkpoints do not
need release notes. Explain version impact when it matters; no separate evidence
form is required.

Never edit or delete notes for an already-tagged release. The
[tagged-release check](../../scripts/workflow/check_release_notes_policy.py)
protects this rule and runs in CI. Describe corrections in the appropriate
upcoming release rather than rewriting released history.

Validate referenced commands, examples and links. Release notes do not substitute
for updating affected product documentation or completing release qualification.
Use the [repository workflow](../../documentation/workflow/README.md) for those
completion requirements. This guide grants no permission to tag, publish or push.

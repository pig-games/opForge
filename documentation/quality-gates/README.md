# Retained native Level D evidence

`run_native_cli_expansion_completion.sh` writes a JSON completion receipt only
after every required configured FS-UAE test completes and passes. The command
requires a clean worktree so the receipt can identify the exact tested HEAD
commit and tree.

Receipts are committed as evidence-only follow-up commits. They name the
already-tested implementation source identity and must not claim that an older
historical implementation commit was tested unless that exact source identity
is recorded.

Use a source-identity filename when creating a new receipt:

```sh
scripts/workflow/run_native_cli_expansion_completion.sh \
  --manifest "documentation/quality-gates/native-cli-expansion-items-5-1-to-5-6-$(git rev-parse --short HEAD).json"
```

Validate a receipt with:

```sh
python3 scripts/workflow/check_native_level_d_manifest.py <receipt.json>
```

Use `--expect-head` only when validating a receipt against the currently
checked-out source identity.

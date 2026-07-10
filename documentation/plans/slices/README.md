# Native parity slice contract

Native parity slice records are TOML files consumed by
`scripts/workflow/check_native_porting_slice.py`.

Unversioned records are legacy records retained only while the active migration
is incomplete. `schema_version = 1` is permitted only with a
`legacy_contract_migration` explanation. New or migrated records must use
`schema_version = 2` and provide these `[slice]` fields:

- `expected_inputs`
- `expected_outputs`
- `known_non_equivalences`
- `fast_proof_command`
- `level_d_command`
- `level_d_fail_closed = true`

The fast command is the non-emulator proof. The Level D command must be the
exact configured real-native proof and must fail rather than silently accept a
skipped emulator result.

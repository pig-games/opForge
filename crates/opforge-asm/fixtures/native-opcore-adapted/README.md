# Native opcore additive MOS fixtures

These sources are line-preserving copies of the matching canonical files under
`examples/opcore/`. They exist only for native 65C02 parity coverage. Unlisted
lines are byte-identical to their canonical source; each changed line is named
in the adjacent `.adapt.tsv` file with a concrete CPU spelling reason.

Sources without an internal `.cpu` directive are assembled with the explicit
CLI override `--cpu 65c02`. Every canonical `.org` line remains unchanged.
Fixture-local `.hex` files are generated solely by the live Rust CLI.

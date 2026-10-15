# Native preparation benchmarks

`modules` exercises four modules, repeated include definitions, qualified whole-block references, constants, sections and an unused block. `macros` contains four modules and 1,000 explicit varied calls with nested/default macro arguments. Both target 68020 and emit source-directed `build/out.bin` and `build/out.hunk`. The native adapter requests these source outputs and compares both artifacts with live Rust outputs.

Use source-only output selection, matching existing native section controls. A CLI
BIN request combined with Hunk geometry is outside this comparison. Full-extension
indexed MOVE is also excluded: the preserved reference rejects that form. No
production parity repair was made to qualify these workloads.

Fresh expanded-memory FS-UAE runs with a 64 KiB guest stack qualify both outputs
on the eager/chunked reference and scaled-bucket candidate. `modules` emits a
104-byte BIN and 204-byte Hunk; `macros` emits a 14,066-byte BIN and 14,168-byte
Hunk. The preserved contiguous reference also passes `modules`. The capture-store
component probe passes the same project. These are correctness and instrumented
work/heap comparisons; they establish neither release speed nor 2 MiB fit.

Run the ignored `native_preparation_benchmark_fs_uae` test with
`OPFORGE_PREPARATION_IMAGE`, `OPFORGE_PREPARATION_WORKLOAD` set to `modules` or
`macros`, and a
fresh absolute `OPFORGE_PREPARATION_RESULT_DIR` outside `target`. Use the
[FS-UAE environment](../../agents/rules/fs-uae.md). The adapter freezes inputs,
creates live Rust oracles, and retains the two exact artifact proofs and existing
telemetry. It adds no production selector or observer.

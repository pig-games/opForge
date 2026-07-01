# Native Rust-to-68000 Parity Porting Rule Pack

Load this rule pack when porting Rust VM or CLI behavior to native
68000/AmigaOS, fixing native behavior expected to match Rust, or adding native
parser, expression, selector, encoder, output, source, or session behavior.

Also load:

- `agents/rules/native-68000.md` when changing 68000 assembly
- `agents/rules/native-parity-failure-triage.md` when investigating a failure
- `agents/rules/native-68000-safe-instrumentation.md` before instrumenting
- `agents/rules/fs-uae.md` when running FS-UAE tests

## Boundary contract

Before editing production code, record:

```text
Slice name:
Rust reference files/functions:
Native target files/functions:
Boundary type:
Contract:
Expected native inputs:
Expected native outputs:
Known non-equivalences:
Proof-level tests:
Fast proof:
FS-UAE proof:
```

Boundary types include source reader, tokenizer, parser, statement store,
expression request, EXVM result, selector, encoder, and output.

Do not invent native behavior when Rust reference behavior exists. Native
divergence is limited to memory layout, calling convention, register pressure,
fixed-buffer constraints, AmigaOS host I/O, and 68000 control-flow
representation. Document the divergence and preserve Rust semantics.

## Find the first divergence

Compare boundaries in this order:

1. Source line read
2. Tokenization
3. Parser or portable AST/statement shape
4. Native statement/session record
5. Expression request envelope
6. EXVM/EXPR parse/evaluation result
7. Selector candidate
8. Encoder output
9. Session image bytes
10. Output artifact

Patch only the first divergent boundary. One slice corrects one named invariant.

## Evidence levels

Classify every test or observation:

| Level | Evidence |
|---|---|
| A | Pure Rust semantic oracle |
| B | Rust-side package/native harness contract |
| C | Host-side native request-shape simulator |
| D | Real native 68000/AmigaOS execution through FS-UAE |
| E | Temporary localization or debug probe |

Every test summary must state:

```text
This test proves:
This test does not prove:
```

Levels A-C can provide fast boundary proof but cannot replace a required Level D
confirmation. Level E never proves production behavior.

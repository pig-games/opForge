# Focused package-VM efficiency measurements

Run from the repository root on a POSIX host:

```sh
python3 scripts/performance/vm_efficiency.py selection --blocks 8,32,128
python3 -m unittest discover -s scripts/performance/tests -p test_vm_efficiency.py
```

The runner builds the release CLI and package generator once with
`vm-runtime-only`, generates a canonical package and supplies it explicitly to
every invocation. Build/setup has a separate ten-minute build cap and is reported
separately; it is not a measurement sample. Measurements have a 60-second
invocation cap and one five-minute batch deadline across all families. Timeout
terminates the owned process group, fails the run and leaves an incomplete summary.
It never launches native self-hosting or an emulator, extends a deadline, or
automatically retries a timeout. Smaller block counts can be selected explicitly.

Default output goes to a new ignored `build/vm-efficiency-*` directory. Use
`--output <new-directory>` to choose another location; existing directories are
rejected. Each case retains `input.asm`, independently calculated `expected.bin`
and the actual `output.bin`. Middle-size cases also retain `profile.txt`; negative
cases retain their diagnostics. `summary.json` includes build features/profile,
binary/package/source hashes, source revision and tracked-diff identity, compiler,
build environment, raw timing samples, profile rows and limitations. Results are
local inspection artifacts, not a committed historical ledger.

## What the workload proves

Three variants exercise shared selection, operand, expression and pass mechanisms:
MOS 6502, Z80 and Motorola 68000. Each block mixes immediate loads, a memory store,
a local forward branch, data and a symbol-derived address. Distances stay bounded
as block count increases. Seven emitting statements and a separate block label
produce 13 bytes per block for 6502/Z80 and 18 for 68000. Instruction bytes and
address fields are calculated from the fixture's ISA contract independently of
opForge. This is a focused contract, not an external-oracle claim for entire ISAs.

The current VM-only package rejects Zilog `LD r,n`; the Z80 case explicitly uses
supported Intel-compatible `MVI r,n`, while retaining Z80 branch and memory forms.
That uncovered dialect gap is not fixed or hidden as a benchmark optimization.
Each family also checks an undefined-symbol diagnostic and nonzero exit; a signal
is not an acceptable negative result.

The build's `vm-runtime-only` feature removes the host instruction-encoder fallback
in `AsmLine::process_instruction_ast`. Profile validation additionally requires
positive `vm.parse` and `vm.encode` counts and rejects host parse/encode labels.
This proves the measured parser/encoder route, not that every directive or other
assembly operation is implemented by bytecode. Profiling counters include failed
encoding attempts. `vm.model.bootstrap` measures the assembler runtime-model
bootstrap wrapper while profiling is active; it is not a count of every model
construction elsewhere in the process.

## Interpreting the result

Each size/family gets one warmup and three uninstrumented timing runs; each family
gets one separate middle-size attribution run. Profiling percentages and nested
rows overlap: do not sum them indiscriminately or compare their timings directly
with uninstrumented samples. Timing is fresh-process, fixed-order and subject to
host activity. Small sample ranges are descriptive, not confidence intervals.
Repeated agreement is supplemented by independent byte checks on every run.

Model/package setup can dominate these small complete assemblies. Compare size
slopes and per-family results before treating a repeated operation as a worthwhile
optimization. Compare future candidates on identical inputs/settings. No result
here predicts full-self-host duration or proves a native speedup.

Memory reporting is deliberately limited: file sizes are measured; process peak
memory and native working memory are not. The summary includes a labelled
sensitivity calculation for 8/16/32-byte per-statement records alongside source,
output and canonical-package storage. It excludes code, symbols, decoded tables,
scratch and the OS, so it is neither a peak estimate nor proof of fitting 2 MB.
The next representation experiment must account for those missing terms.

The current work and brief decision are tracked in the
[reset plan](../plans/native-runtime-reset.md), not in this tool guide.

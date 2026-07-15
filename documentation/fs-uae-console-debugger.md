# FS-UAE Console Debugger Contract

This document defines the first, deliberately narrow contract for opForge's
opt-in FS-UAE console-debugger tooling. It applies to the stock FS-UAE binary
used by the native smoke harness; it is not a general remote-debugging API.

## Scope and proof level

The normal FS-UAE smoke path remains the Level D confirmation gate. Console
debugger captures are Level E localization evidence: they identify a native
guest control-flow position but do not, by themselves, prove Rust/native
parity.

The controller must never enable this mode unless explicitly requested. It
must use a terminal-backed PTY, keep the normal smoke configuration unchanged,
and collect artifacts in the run-specific `target/fs-uae-*` directory.

## Installed baseline

On the maintained macOS development machine, the stock executable is:

```text
/Applications/FS-UAE.app/Contents/MacOS/fs-uae
```

The current smoke template is:

```text
/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae
```

The binary contains the UAE debugger help text and accepts the
`console_debugger` option. This is a binary-capability check only; a future
PTY runner must record the exact FS-UAE version and live transcript for every
capture.

## Launch contract

The generated debug configuration is the existing smoke configuration plus:

```ini
console_debugger = 1
```

FS-UAE documents that the console debugger works only when FS-UAE is launched
from a terminal. On macOS, `Cmd+D` (`Mod+D`) enters the console debugger. The
first implementation must not synthesize this GUI shortcut. It should first
attempt terminal/PTY command interaction; if interactive entry is still
required, it must emit `manual-debugger-entry-required` and retain the PTY
transcript rather than pretending the run was automated.

The controller must also:

- set an explicit per-run timeout;
- capture the generated `.fs-uae` config;
- record parent/child FS-UAE process IDs;
- save raw PTY bytes and a UTF-8-lossy normalized transcript separately;
- terminate only processes it launched; and
- report cleanup success or failure.

## Verified UAE debugger command grammar

The installed binary's embedded `HELP for UAE Debugger` text defines the
following command subset. Future parsers must treat output as human-oriented
and tolerate banners, prompts, and unrelated emulator messages.

| Need | Command | Required captured result |
| --- | --- | --- |
| CPU register frame | `r` | PC, SR, D0-D7, A0-A7 where printed |
| Read stack words | `m <address> <lines>` | address-labelled memory dump |
| Disassemble PC | `d <address> <lines>` | address-labelled instructions |
| Add/remove execution breakpoint | `f <address>` | breakpoint acknowledgement |
| List breakpoints | `fl` | bounded breakpoint list |
| Clear breakpoints | `fd` | acknowledgement or empty list |
| Continue | `g` or `g <address>` | execution resumes; no transcript success inference |
| Instruction step | `t [instructions]` | next debugger stop frame |
| Step through call/loop control | `z` | next debugger stop frame |
| CPU history | `H <count>` or `HH <count>` | bounded history transcript |
| Inspect tracked segments | `Z`, `Zl`, `Za <address>` | segment-tracker output when enabled |
| Locate loaded debug data | `Zf <hostfile>`, `Zy <symbol>`, `Zc <file> <line>` | optional, not required for v0.1 |
| Quit emulator | `q` | controller cleanup must treat this as expected termination |

Command input must be newline-terminated ASCII. The v0.1 controller may issue
only the register, memory, disassembly, breakpoint-list/clear, history,
continue, and quit commands above. It must not issue write (`W`), register
mutation (`r <reg> <value>`), memory-watch, disk-debug, or debugger state
commands unless a later reviewed contract explicitly adds them.

## Stop report contract

Each capture writes these files under a unique run artifact directory:

```text
console-debugger.config.fs-uae
console-debugger.raw.log
console-debugger.transcript.txt
console-debugger.report.json
```

`console-debugger.report.json` must include:

```json
{
  "schema_version": 1,
  "mode": "fs-uae-console-debugger",
  "proof_level": "E",
  "fs_uae_binary": "/Applications/FS-UAE.app/Contents/MacOS/fs-uae",
  "fs_uae_version": "captured value or unknown",
  "entry": "pty-command | manual-debugger-entry-required",
  "stop_reason": "breakpoint | timeout | process-exit | manual",
  "pc": "optional hexadecimal string",
  "sr": "optional hexadecimal string",
  "registers": {},
  "stack_dump": "relative transcript path",
  "disassembly": "relative transcript path",
  "process_ids": [],
  "cleanup": "complete | incomplete",
  "raw_transcript_sha256": "hex"
}
```

The report must never claim a PC/register value that was not parsed from the
saved transcript. Missing values are represented by omitted fields, not zero.

## Macro-hang investigation script

Once a controlled stop can be entered, the first macro-hang collection is:

```text
r
d <pc> 16
m <a7> 16
H 32
fl
```

The address used for a breakpoint must come from a captured runtime address or
approved debug-contract event; opForge must not assume native source labels
are fixed Hunk addresses. The resulting report must distinguish a breakpoint
stop from a timeout with the CPU still running.

## Non-goals and safety

- No patched FS-UAE, GDB-RSP server, `uae-dap`, TCP serial agent, or editor
  protocol is included here.
- No debugger command is part of CI or required for ordinary FS-UAE parity.
- No guest-side ad-hoc output is authorized. Native probes use the approved
  debug/assert framework and the native safe-instrumentation rule pack.
- Console transcript parsing is diagnostic-only; a moved stop location is not
  a parity fix.

## Sources

- FS-UAE, [console_debugger option](https://fs-uae.net/docs/options/console-debugger/)
- FS-UAE, [keyboard shortcuts](https://fs-uae.net/docs/keyboard-shortcuts/)
- Local binary capability inspection on 2026-07-15 using the installed
  `fs-uae` executable's embedded debugger help text.

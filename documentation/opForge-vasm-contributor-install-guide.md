# opForge Contributor Guide: Installing `vasm` and the 68k Wrapper

This short guide helps contributors install `vasm` for the current opForge
external-oracle 680x0 workflow.

## What opForge currently expects

The first 680x0 external-oracle slice currently:

- is opt-in via `OPFORGE_EXTERNAL_ORACLE_VASM=1`
- defaults to looking for `vasmm68k_mot` on `PATH`
- can be pointed at another executable with `OPFORGE_VASM_BIN`

Relevant code:

- [crates/opforge-asm/src/oracle/vasm.rs](../crates/opforge-asm/src/oracle/vasm.rs)

## Recommended install layout

Install these binaries into `~/.local/bin`:

- `vasmm68k_mot`
- `vasmm68k_std`
- wrapper: `opforge-vasm68k`
- optional alias: `vasm68k`

`~/.local/bin` should be on your `PATH`.

## Build `vasm` from source

Homebrew does not currently provide a `vasm` formula here, so the simplest
repeatable path is a local source build.

```bash
tmpdir=$(mktemp -d /tmp/vasm-build.XXXXXX)
git clone https://github.com/dbuchwald/vasm.git "$tmpdir"
cd "$tmpdir"
make CPU=m68k SYNTAX=mot
make CPU=m68k SYNTAX=std
mkdir -p "$HOME/.local/bin"
install -m 755 vasmm68k_mot "$HOME/.local/bin/vasmm68k_mot"
install -m 755 vasmm68k_std "$HOME/.local/bin/vasmm68k_std"
```

This produces:

- `vasmm68k_mot`: Motorola syntax
- `vasmm68k_std`: standard syntax

## Install the wrapper

The wrapper gives contributors and automation one stable command to call.

Create `~/.local/bin/opforge-vasm68k`:

```sh
#!/bin/sh
set -eu

syntax="${OPFORGE_VASM68K_SYNTAX:-mot}"

case "$syntax" in
  mot|std) ;;
  *)
    echo "opforge-vasm68k: unsupported OPFORGE_VASM68K_SYNTAX '$syntax' (expected 'mot' or 'std')" >&2
    exit 2
    ;;
esac

while [ "$#" -gt 0 ]; do
  case "$1" in
    --mot)
      syntax="mot"
      shift
      ;;
    --std)
      syntax="std"
      shift
      ;;
    --show-bin)
      echo "$HOME/.local/bin/vasmm68k_${syntax}"
      exit 0
      ;;
    --help)
      cat <<'USAGE'
opforge-vasm68k: thin wrapper around vasm 68k binaries

Usage:
  opforge-vasm68k [--mot|--std] [vasm args...]
  vasm68k [--mot|--std] [vasm args...]

Defaults:
  --mot is the default syntax
  OPFORGE_VASM68K_SYNTAX may be set to 'mot' or 'std'

Helpers:
  --show-bin   print the selected underlying vasm binary and exit
  --help       show this help and exit
  --           stop wrapper parsing and pass the remaining args through
USAGE
      exit 0
      ;;
    --)
      shift
      break
      ;;
    *)
      break
      ;;
  esac
done

exec "$HOME/.local/bin/vasmm68k_${syntax}" "$@"
```

Then make it executable and add the short alias:

```bash
chmod +x "$HOME/.local/bin/opforge-vasm68k"
ln -sf "$HOME/.local/bin/opforge-vasm68k" "$HOME/.local/bin/vasm68k"
```

## Verify the install

```bash
command -v vasmm68k_mot
command -v vasmm68k_std
command -v opforge-vasm68k
opforge-vasm68k --show-bin
opforge-vasm68k --std --show-bin
```

If the commands resolve, the install is good enough for the current opForge
680x0 oracle workflow.

## Use it with opForge

The current harness can be enabled like this:

```bash
export OPFORGE_EXTERNAL_ORACLE_VASM=1
export OPFORGE_VASM_BIN="$HOME/.local/bin/opforge-vasm68k"
cargo test -p asm external_oracle_vasm_68000_68010_success_path_fixtures -- --nocapture
```

Notes:

- `OPFORGE_VASM_BIN` is optional if `vasmm68k_mot` is already on `PATH`.
- Pointing `OPFORGE_VASM_BIN` at the wrapper is recommended because it gives us
  one stable command for future harness work.
- The current first slice only covers curated `68000` and `68010`
  success-path fixtures.

## Troubleshooting

- `Could not find vasmm68k_mot on PATH`
  Set `OPFORGE_VASM_BIN` to the wrapper path or add `~/.local/bin` to `PATH`.

- `unsupported OPFORGE_VASM68K_SYNTAX`
  Set `OPFORGE_VASM68K_SYNTAX` to `mot` or `std`.

- `vasm exited with status ...`
  Re-run the wrapper directly on the same `.asm` file and inspect stderr.

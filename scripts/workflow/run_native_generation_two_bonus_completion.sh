#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Bonus closure: first prove that generation 1 builds a Rust-identical,
# runnable generation 2, then run every other member of the same established
# 53-test fail-closed corpus. The delegated wrapper owns the canonical list so
# this entry point cannot silently drift to a smaller inventory.
exec "${script_dir}/run_native_existing_parity_completion.sh" --verify-generation-two-first

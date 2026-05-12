# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright (C) 2026 Erik van der Tier

.PHONY: build release clean fmt clippy audit quality-gate workflow-gate native-68000-format-check native-68000-format reference reference-test test test-core test-external-oracle test-external-oracle-mos6502-64tass test-vm-runtime test-vm-runtime-artifact test-vm-runtime-intel test-vm-rollout-criteria test-vm-parity test-vm-opasm-modes test-build-profile-matrix test-build-combo-smoke ci-core ci-vm-mos6502 ci-vm-intel8080 build-cli build-lsp build-ffi build-ffi-release test-ffi-packaging build-vm-package build-vm-runtime-artifact vm-only-build vm-only-release vm-only-build-embedded vm-only-release-embedded vm-only-build-unbundled vm-only-release-unbundled vm-only-build-unbundled-artifact vm-only-release-unbundled-artifact manual-pdf

MANUAL_MD := documentation/opForge-reference-manual.md
MANUAL_PDF := documentation/opForge-reference-manual.pdf
VM_RUNTIME_ARTIFACT := target/vm/opforge-vm-runtime.opasm
EXTERNAL_ORACLE_VASM_ENV := $(if $(OPFORGE_VASM_BIN),OPFORGE_VASM_BIN="$(OPFORGE_VASM_BIN)")
EXTERNAL_ORACLE_64TASS_ENV := $(if $(OPFORGE_64TASS_BIN),OPFORGE_64TASS_BIN="$(OPFORGE_64TASS_BIN)")

build:
	cargo clippy --workspace -- -D warnings
	cargo build --workspace

build-cli:
	cargo build -p cli

build-lsp:
	cargo build -p lsp

build-ffi:
	cargo build -p ffi

build-ffi-release:
	cargo build -p ffi --profile release-ffi --locked --lib

test-ffi-packaging: build-ffi-release
	@if [ -f target/release-ffi/libopforge.dylib ] || [ -f target/release-ffi/libopforge.so ] || [ -f target/release-ffi/opforge.dll ]; then \
		true; \
	else \
		echo "expected shared library basename libopforge in target/release-ffi"; \
		exit 1; \
	fi

release:
	cargo clippy --workspace -- -D warnings
	cargo build --workspace --release --exclude ffi
	$(MAKE) build-ffi-release

clean:
	cargo clean

fmt:
	cargo fmt --all

clippy:
	cargo clippy --workspace -- -D warnings

audit:
	cargo audit

quality-gate:
	scripts/workflow/run_rust_quality_gate.sh

native-68000-format-check:
	scripts/workflow/run_native_68000_format_gate.sh

native-68000-format:
	scripts/workflow/run_native_68000_format_gate.sh --write

workflow-gate:
	python3 scripts/workflow/check_agent_symlinks.py
	python3 scripts/workflow/check_supply_chain_ban.py
	find documentation dev-docs -name '*.quality-gate.txt' -print0 | xargs -0 python3 scripts/workflow/check_quality_gate_evidence.py
	python3 scripts/workflow/check_reference_update_scope.py
	python3 scripts/workflow/check_release_notes_policy.py

test:
	cargo test --workspace
	../scripts/cleanup-build-artifacts.sh ..

test-core:
	cargo test --workspace --no-default-features
	../scripts/cleanup-build-artifacts.sh ..

test-external-oracle:
	$(EXTERNAL_ORACLE_VASM_ENV) OPFORGE_EXTERNAL_ORACLE_VASM=1 cargo test -p asm external_oracle_ -- --nocapture
	../scripts/cleanup-build-artifacts.sh ..

test-external-oracle-mos6502-64tass:
	$(EXTERNAL_ORACLE_64TASS_ENV) OPFORGE_EXTERNAL_ORACLE_64TASS=1 cargo test -p asm external_oracle_64tass_mos6502_ -- --nocapture
	../scripts/cleanup-build-artifacts.sh ..

test-vm-runtime:
	cargo test vm_runtime_mos6502_

test-vm-runtime-artifact:
	cargo test --features vm-runtime-opasm-artifact vm_runtime_artifact_

test-vm-runtime-intel:
	cargo test vm_runtime_intel8080_
	cargo test vm_runtime_intel8085_
	cargo test vm_runtime_z80_

test-vm-rollout-criteria:
	cargo test vm_rollout_criteria_

test-vm-parity:
	cargo test --features vm-parity vm_parity_smoke_instruction_bytes_and_diagnostics

ci-core:
	make test-core

ci-vm-mos6502:
	make test-core
	make test-vm-runtime
	make test-vm-runtime-artifact
	make test-vm-rollout-criteria
	make test-vm-parity

ci-vm-intel8080:
	make test-core
	make test-vm-rollout-criteria
	make test-vm-runtime-intel

build-vm-package:
	cargo run -p cli --bin build_vm_package -- target/vm/hierarchy.opasm

build-vm-runtime-artifact:
	cargo run -p cli --features vm-runtime-only,vm-runtime-opasm-artifact --bin build_vm_package -- $(VM_RUNTIME_ARTIFACT)

vm-only-build: build-vm-runtime-artifact
	cargo build -p cli --features vm-runtime-only,vm-runtime-opasm-artifact --bin opforge

vm-only-release: build-vm-runtime-artifact
	cargo build -p cli --release --features vm-runtime-only,vm-runtime-opasm-artifact --bin opforge

vm-only-build-embedded:
	cargo build -p cli --features vm-runtime-only --bin opforge

vm-only-release-embedded:
	cargo build -p cli --release --features vm-runtime-only --bin opforge

vm-only-build-unbundled:
	cargo build -p cli --features vm-runtime-only,vm-runtime-opasm-unbundled --bin opforge

vm-only-release-unbundled:
	cargo build -p cli --release --features vm-runtime-only,vm-runtime-opasm-unbundled --bin opforge

vm-only-build-unbundled-artifact: build-vm-runtime-artifact
	cargo build -p cli --features vm-runtime-only,vm-runtime-opasm-unbundled,vm-runtime-opasm-artifact --bin opforge

vm-only-release-unbundled-artifact: build-vm-runtime-artifact
	cargo build -p cli --release --features vm-runtime-only,vm-runtime-opasm-unbundled,vm-runtime-opasm-artifact --bin opforge

test-vm-opasm-modes:
	CARGO_TARGET_DIR=target/vmcheck-embedded cargo build -p cli --features vm-runtime-only --bin opforge
	target/vmcheck-embedded/debug/opforge --print-cpusupport >/dev/null
	CARGO_TARGET_DIR=target/vmcheck-unbundled cargo build -p cli --features vm-runtime-only,vm-runtime-opasm-unbundled --bin opforge
	@if target/vmcheck-unbundled/debug/opforge -i examples/mos6502/6502_simple.asm -l >/dev/null 2>&1; then \
		echo "expected vm-only unbundled run without package to fail"; \
		exit 1; \
	fi
	$(MAKE) build-vm-runtime-artifact
	target/vmcheck-unbundled/debug/opforge --opasm-package $(abspath $(VM_RUNTIME_ARTIFACT)) -i examples/mos6502/6502_simple.asm -l >/dev/null
	CARGO_TARGET_DIR=target/vmcheck-unbundled-artifact cargo build -p cli --features vm-runtime-only,vm-runtime-opasm-unbundled,vm-runtime-opasm-artifact --bin opforge
	target/vmcheck-unbundled-artifact/debug/opforge --opasm-package $(abspath $(VM_RUNTIME_ARTIFACT)) -i examples/mos6502/6502_simple.asm -l >/dev/null

test-build-profile-matrix:
	cargo test version_flag_reports_build_profile
	cargo test --features vm-runtime-only version_flag_reports_build_profile
	cargo test --features vm-runtime-only,vm-runtime-opasm-artifact version_flag_reports_build_profile
	cargo test --features vm-runtime-only,vm-runtime-opasm-unbundled version_flag_reports_build_profile
	cargo test --features vm-runtime-only,vm-runtime-opasm-unbundled,vm-runtime-opasm-artifact version_flag_reports_build_profile

test-build-combo-smoke:
	cargo test load_module_graph_resolves_mforth_style_use_directives
	cargo test --features vm-runtime-only load_module_graph_resolves_mforth_style_use_directives
	cargo test --features vm-runtime-only,vm-runtime-opasm-artifact load_module_graph_resolves_mforth_style_use_directives
	cargo test --features vm-runtime-only,vm-runtime-opasm-unbundled load_module_graph_resolves_mforth_style_use_directives
	cargo test --features vm-runtime-only,vm-runtime-opasm-unbundled,vm-runtime-opasm-artifact load_module_graph_resolves_mforth_style_use_directives

reference-test:
	cargo test -p asm examples_match_reference_outputs
	../scripts/cleanup-build-artifacts.sh ..

reference:
	opForge_UPDATE_REFERENCE=1 cargo test -p asm examples_match_reference_outputs -- --nocapture

manual-pdf:
	mkdir -p documentation
	pandoc $(MANUAL_MD) --from gfm --pdf-engine=xelatex -V geometry:margin=1in -V mainfont='Arial Unicode MS' -V sansfont='Arial' -V monofont='Menlo' -o $(MANUAL_PDF)

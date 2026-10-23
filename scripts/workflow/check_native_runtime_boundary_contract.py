#!/usr/bin/env python3
"""Validate native runtime ownership and import boundaries."""

from __future__ import annotations

import re
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
# Shared wire-contract imports remain allowed; executors cannot call orchestration.
FORBIDDEN_IMPORTS = {'native/motorola68000/amigaos/vm/encoding/runtime.asm': ('opforge.cli.entry',
                                                          'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/exprvm/assignment.asm': ('opforge.cli.entry',
                                                           'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/exprvm/i64_math.asm': ('opforge.cli.entry',
                                                         'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/exprvm/runtime.asm': ('opforge.cli.entry',
                                                        'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/exprvm/values_runtime.asm': ('opforge.cli.entry',
                                                               'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/exvm/runtime.asm': ('opforge.cli.entry',
                                                      'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/abi.asm': ('opforge.cli.entry', 'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/line_iterator.asm': ('opforge.cli.entry',
                                                            'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/line_router.asm': ('opforge.cli.entry',
                                                          'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/macro_descriptors.asm': ('opforge.cli.entry',
                                                                'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/macro_fragments.asm': ('opforge.cli.entry',
                                                              'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/macro_runtime.asm': ('opforge.cli.entry',
                                                            'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/macro_spelling.asm': ('opforge.cli.entry',
                                                             'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/packed_branch.asm': ('opforge.cli.entry',
                                                            'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/packed_data.asm': ('opforge.cli.entry',
                                                          'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/packed_declaration.asm': ('opforge.cli.entry',
                                                                 'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/packed_file.asm': ('opforge.cli.entry',
                                                          'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/packed_loop.asm': ('opforge.cli.entry',
                                                          'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/packed_macro.asm': ('opforge.cli.entry',
                                                           'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/packed_metadata.asm': ('opforge.cli.entry',
                                                              'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/packed_text.asm': ('opforge.cli.entry',
                                                          'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/runtime.asm': ('opforge.cli.entry',
                                                      'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_abi.asm': ('opforge.cli.entry',
                                                            'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_compile.asm': ('opforge.cli.entry',
                                                                'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_decimal.asm': ('opforge.cli.entry',
                                                                'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_facets.asm': ('opforge.cli.entry',
                                                               'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_fallback.asm': ('opforge.cli.entry',
                                                                 'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_head.asm': ('opforge.cli.entry',
                                                             'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_inputs.asm': ('opforge.cli.entry',
                                                               'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_invoke_req.asm': ('opforge.cli.entry',
                                                                   'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_match.asm': ('opforge.cli.entry',
                                                              'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_overload.asm': ('opforge.cli.entry',
                                                                 'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_plan.asm': ('opforge.cli.entry',
                                                             'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_policy.asm': ('opforge.cli.entry',
                                                               'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_request.asm': ('opforge.cli.entry',
                                                                'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_route.asm': ('opforge.cli.entry',
                                                              'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_scope.asm': ('opforge.cli.entry',
                                                              'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_select.asm': ('opforge.cli.entry',
                                                               'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_text.asm': ('opforge.cli.entry',
                                                             'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/statement_text_match.asm': ('opforge.cli.entry',
                                                                   'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/prvm/target_bootstrap.asm': ('opforge.cli.entry',
                                                               'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/tkvm/char_predicates.asm': ('opforge.cli.entry',
                                                              'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/tkvm/control.asm': ('opforge.cli.entry',
                                                      'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/tkvm/demo_program.asm': ('opforge.cli.entry',
                                                           'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/tkvm/fragments.asm': ('opforge.cli.entry',
                                                        'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/tkvm/runtime.asm': ('opforge.cli.entry',
                                                      'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/tkvm/scanner.asm': ('opforge.cli.entry',
                                                      'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/tkvm/state.asm': ('opforge.cli.entry',
                                                    'opforge.assembler.driver'),
 'native/motorola68000/amigaos/vm/value/runtime.asm': ('opforge.cli.entry',
                                                       'opforge.assembler.driver'),
 'native/motorola68000/amigaos/packages/catalog.asm': ('opforge.cli.entry',
                                                       'opforge.assembler.driver'),
 'native/motorola68000/amigaos/packages/format.asm': ('opforge.cli.entry',
                                                      'opforge.assembler.driver'),
 'native/motorola68000/amigaos/packages/loader.asm': ('opforge.cli.entry',
                                                      'opforge.assembler.driver'),
 'native/motorola68000/amigaos/packages/names.asm': ('opforge.cli.entry',
                                                     'opforge.assembler.driver'),
 'native/motorola68000/amigaos/packages/validation.asm': ('opforge.cli.entry',
                                                          'opforge.assembler.driver')}
RETAINED_OWNER_IMPORTS = {'native/motorola68000/amigaos/cli/entry.asm': ('opforge.assembler.driver',
                                                'opforge.cli.arguments',
                                                'opforge.cli.input',
                                                'opforge.cli.target'),
 'native/motorola68000/amigaos/packages/loader.asm': ('opforge.packages.catalog',
                                                      'opforge.packages.format',
                                                      'opforge.packages.validation',
                                                      'opforge.platform.memory')}

def imports(path: Path) -> list[str]:
    return re.findall(r"^\s*\.use\s+([^\s;]+)", path.read_text(encoding="utf-8"), re.MULTILINE)


def validate(root: Path = ROOT) -> list[str]:
    errors: list[str] = []
    for relative, forbidden in FORBIDDEN_IMPORTS.items():
        found = imports(root / relative)
        for edge in forbidden:
            if any(imported.startswith(edge) for imported in found):
                errors.append(f"prohibited current reverse import: {relative} -> {edge}")
    for relative, expected in RETAINED_OWNER_IMPORTS.items():
        found = tuple(imports(root / relative))
        if found != expected:
            errors.append(
                f"retained-owner imports changed: {relative}: "
                f"expected {expected}, found {found}"
            )
    return errors


def main() -> int:
    errors = validate()
    if errors:
        print("native runtime boundary contract: FAIL")
        for error in errors:
            print(f"  - {error}")
        return 1
    print("native runtime boundary contract: PASS")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

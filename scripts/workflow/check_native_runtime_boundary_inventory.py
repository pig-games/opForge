#!/usr/bin/env python3
"""Fail closed when the runtime inventory drifts from its audited sources."""

from __future__ import annotations

import argparse
from dataclasses import dataclass
import hashlib
import re
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
INVENTORY = ROOT / "documentation/architecture/native-runtime-boundary-inventory-v0_1.md"
TARGETS = {'opforge.assembler.driver': ('native/motorola68000/amigaos/assembler/driver.asm',
                              'execute',
                              ('opforge.assembler.symbols.lexical_store',
                               'opforge.assembler.modules.bindings',
                               'opforge.assembler.prepare.frontend',
                               'opforge.assembler.language.statement_execution',
                               'opforge.assembler.source.capture',
                               'opforge.assembler.source.capture_store',
                               'opforge.assembler.modules.configuration',
                               'opforge.assembler.language.conditionals',
                               'opforge.assembler.symbols.name_store',
                               'opforge.assembler.symbols.scopes',
                               'opforge.assembler.layout.assembly',
                               'opforge.assembler.layout.dependencies',
                               'opforge.assembler.layout.sections',
                               'opforge.assembler.output.hunk_image',
                               'opforge.assembler.output.hunk_stream',
                               'opforge.assembler.output.plan',
                               'opforge.assembler.output.io',
                               'opforge.assembler.output.records',
                               'opforge.assembler.output.spans',
                               'opforge.assembler.output.flat',
                               'opforge.assembler.prepare.metadata',
                               'opforge.assembler.output.metadata',
                               'opforge.packages.format',
                               'opforge.packages.loader',
                               'opforge.cli.progress',
                               'opforge.platform.files',
                               'opforge.platform.memory',
                               'opforge.assembler.source.records',
                               'opforge.assembler.symbols.storage',
                               'opforge.assembler.values.values',
                               'opforge.assembler.source.discovery',
                               'opforge.assembler.source.input_plan',
                               'opforge.assembler.source.line_input',
                               'opforge.assembler.source.declarations',
                               'opforge.assembler.modules.graph',
                               'opforge.assembler.prepare.record_order',
                               'opforge.assembler.prepare.record_store',
                               'opforge.assembler.source.record_view',
                               'opforge.assembler.layout.block_index',
                               'opforge.assembler.symbols.scope_layout',
                               'opforge.assembler.source.file_data',
                               'opforge.assembler.modules.compiler',
                               'opforge.assembler.modules.scope',
                               'opforge.assembler.modules.templates',
                               'opforge.assembler.modules.image',
                               'opforge.assembler.modules.registry',
                               'opforge.assembler.modules.object_store',
                               'opforge.assembler.modules.semantic_exports',
                               'opforge.assembler.modules.assembly_interface',
                               'opforge.assembler.modules.provider',
                               'opforge.assembler.modules.interface_store',
                               'opforge.assembler.modules.template_names',
                               'opforge.assembler.modules.runtime',
                               'opforge.assembler.modules.carrier',
                               'opforge.assembler.modules.admission',
                               'opforge.assembler.prepare.sections',
                               'opforge.assembler.symbols.imports',
                               'opforge.vm.exprvm.runtime',
                               'opforge.vm.prvm.abi',
                               'opforge.assembler.language.repeat_activation',
                               'opforge.assembler.language.branches',
                               'opforge.assembler.language.repetition',
                               'opforge.assembler.symbols.scalar_observations',
                               'opforge.assembler.layout.encoding',
                               'opforge.assembler.source.control',
                               'opforge.assembler.symbols.binding_records')),
 'opforge.assembler.prepare.frontend': ('native/motorola68000/amigaos/assembler/prepare/frontend.asm',
                                        'captureStatementRole',
                                        ('opforge.assembler.symbols.lexical_store',
                                         'opforge.assembler.symbols.name_store',
                                         'opforge.packages.format',
                                         'opforge.assembler.layout.state',
                                         'opforge.assembler.source.records',
                                         'opforge.assembler.symbols.members',
                                         'opforge.assembler.prepare.member_syntax',
                                         'opforge.assembler.prepare.prepare',
                                         'opforge.assembler.prepare.operand_binding',
                                         'opforge.assembler.prepare.expression',
                                         'opforge.assembler.prepare.data',
                                         'opforge.assembler.prepare.metadata',
                                         'opforge.vm.prvm.packed_metadata',
                                         'opforge.assembler.symbols.scopes',
                                         'opforge.assembler.symbols.scope_layout',
                                         'opforge.assembler.symbols.binding_records',
                                         'opforge.assembler.language.structs',
                                         'opforge.assembler.language.conditionals',
                                         'opforge.assembler.language.templates',
                                         'opforge.assembler.source.capture',
                                         'opforge.assembler.language.macro_plans',
                                         'opforge.assembler.language.statement_heads',
                                         'opforge.assembler.language.statement_session',
                                         'opforge.assembler.language.statement_execution',
                                         'opforge.assembler.language.statements',
                                         'opforge.assembler.symbols.imports',
                                         'opforge.assembler.modules.graph',
                                         'opforge.assembler.layout.block_index',
                                         'opforge.assembler.modules.bindings',
                                         'opforge.platform.memory',
                                         'opforge.assembler.values.values',
                                         'opforge.assembler.values.structs',
                                         'opforge.vm.tkvm.runtime',
                                         'opforge.vm.tkvm.fragments',
                                         'opforge.assembler.language.macro_fragments',
                                         'opforge.vm.tkvm.control',
                                         'opforge.vm.prvm.macro_runtime',
                                         'opforge.vm.prvm.packed_file',
                                         'opforge.assembler.language.declaration',
                                         'opforge.vm.prvm.packed_declaration',
                                         'opforge.vm.prvm.packed_loop',
                                         'opforge.vm.prvm.runtime',
                                         'opforge.vm.prvm.abi',
                                         'opforge.vm.prvm.macro_spelling',
                                         'opforge.assembler.language.text_names',
                                         'opforge.vm.prvm.packed_text')),
 'opforge.assembler.prepare.prepare': ('native/motorola68000/amigaos/assembler/prepare/prepare.asm',
                                       'line',
                                       ('opforge.packages.format',
                                        'opforge.assembler.layout.state',
                                        'opforge.assembler.source.records',
                                        'opforge.assembler.prepare.expression',
                                        'opforge.assembler.prepare.operand_wrappers',
                                        'opforge.assembler.prepare.products',
                                        'opforge.assembler.prepare.nested_operands',
                                        'opforge.vm.prvm.packed_loop',
                                        'opforge.vm.prvm.abi',
                                        'opforge.vm.prvm.packed_branch',
                                        'opforge.vm.prvm.packed_text',
                                        'opforge.vm.prvm.macro_runtime')),
 'opforge.assembler.prepare.operand_binding': ('native/motorola68000/amigaos/assembler/prepare/operand_binding.asm',
                                               'line',
                                               ('opforge.packages.format',
                                                'opforge.assembler.symbols.scopes',
                                                'opforge.assembler.source.records',
                                                'opforge.assembler.prepare.nested_operands',
                                                'opforge.assembler.prepare.register_mask',
                                                'opforge.assembler.prepare.expression',
                                                'opforge.vm.prvm.abi',
                                                'opforge.debug.runtime_profile')),
 'opforge.assembler.prepare.expression': ('native/motorola68000/amigaos/assembler/prepare/expression.asm',
                                          'configure',
                                          ('opforge.vm.exprvm.runtime',
                                           'opforge.assembler.prepare.fold',
                                           'opforge.vm.exvm.runtime',
                                           'opforge.assembler.prepare.expression_lowering',
                                           'opforge.vm.exprvm.values_runtime',
                                           'opforge.assembler.values.values',
                                           'opforge.assembler.values.records',
                                           'opforge.assembler.values.structs',
                                           'opforge.assembler.symbols.bindings',
                                           'opforge.platform.memory',
                                           'opforge.assembler.source.records',
                                           'opforge.debug.runtime_profile')),
 'opforge.assembler.prepare.data': ('native/motorola68000/amigaos/assembler/prepare/data.asm',
                                    'check',
                                    ('opforge.assembler.symbols.lexical_store',
                                     'opforge.packages.format',
                                     'opforge.assembler.symbols.scope_layout',
                                     'opforge.assembler.symbols.binding_records',
                                     'opforge.assembler.symbols.imports',
                                     'opforge.vm.prvm.abi',
                                     'opforge.vm.prvm.macro_runtime',
                                     'opforge.assembler.source.records')),
 'opforge.assembler.layout.assembly': ('native/motorola68000/amigaos/assembler/layout/assembly.asm',
                                       'beginSizing',
                                       ('opforge.packages.format',
                                        'opforge.assembler.layout.session',
                                        'opforge.assembler.values.values',
                                        'opforge.assembler.symbols.parameters',
                                        'opforge.assembler.layout.state',
                                        'opforge.assembler.prepare.expression',
                                        'opforge.vm.exprvm.runtime',
                                        'opforge.vm.prvm.abi',
                                        'opforge.vm.prvm.macro_runtime',
                                        'opforge.assembler.layout.encoding',
                                        'opforge.assembler.layout.dependencies',
                                        'opforge.assembler.symbols.scalar_observations',
                                        'opforge.assembler.symbols.mutable',
                                        'opforge.assembler.symbols.constants',
                                        'opforge.assembler.source.records',
                                        'opforge.assembler.source.payload_bounds',
                                        'opforge.assembler.layout.sections',
                                        'opforge.assembler.layout.hunk_mapping',
                                        'opforge.assembler.layout.hunk_placement',
                                        'opforge.assembler.layout.hunk_references',
                                        'opforge.assembler.language.repetition',
                                        'opforge.assembler.language.branches',
                                        'opforge.assembler.values.guards',
                                        'opforge.assembler.language.repeat_inventory',
                                        'opforge.assembler.language.text_execution',
                                        'opforge.assembler.language.text',
                                        'opforge.assembler.language.text_layout',
                                        'opforge.assembler.language.string_provider',
                                        'opforge.platform.memory',
                                        'opforge.assembler.modules.records')),
 'opforge.assembler.layout.encoding': ('native/motorola68000/amigaos/assembler/layout/encoding.asm',
                                       'encode',
                                       ('opforge.packages.format',
                                        'opforge.assembler.layout.state',
                                        'opforge.assembler.source.records',
                                        'opforge.assembler.prepare.shapes',
                                        'opforge.assembler.prepare.operand_wrappers',
                                        'opforge.assembler.prepare.tuples',
                                        'opforge.assembler.prepare.products',
                                        'opforge.assembler.prepare.operand_paths',
                                        'opforge.assembler.layout.dependencies',
                                        'opforge.assembler.symbols.mutable',
                                        'opforge.assembler.layout.hunk_references',
                                        'opforge.assembler.prepare.register_mask',
                                        'opforge.assembler.prepare.expression',
                                        'opforge.vm.exprvm.runtime',
                                        'opforge.vm.encoding.runtime',
                                        'opforge.vm.value.runtime',
                                        'opforge.debug.runtime_profile')),
 'opforge.assembler.layout.dependencies': ('native/motorola68000/amigaos/assembler/layout/dependencies.asm',
                                           'resolve',
                                           ('opforge.packages.format',
                                            'opforge.platform.memory',
                                            'opforge.assembler.values.values',
                                            'opforge.assembler.symbols.bindings',
                                            'opforge.assembler.prepare.expression',
                                            'opforge.vm.exprvm.runtime',
                                            'opforge.assembler.source.records',
                                            'opforge.assembler.modules.records',
                                            'opforge.assembler.language.repeat_inventory',
                                            'opforge.assembler.language.repeat_dependencies',
                                            'opforge.assembler.language.repeat_activation',
                                            'opforge.assembler.layout.dependency_layout',
                                            'opforge.vm.prvm.packed_branch',
                                            'opforge.vm.prvm.packed_loop',
                                            'opforge.vm.prvm.abi',
                                            'opforge.debug.runtime_profile')),
 'opforge.assembler.layout.sections': ('native/motorola68000/amigaos/assembler/layout/sections.asm',
                                       'scan',
                                       ('opforge.packages.format',
                                        'opforge.assembler.source.records')),
 'opforge.assembler.source.control': ('native/motorola68000/amigaos/assembler/source/control.asm',
                                      'begin',
                                      ('opforge.assembler.prepare.frontend',
                                       'opforge.packages.format',
                                       'opforge.assembler.symbols.scopes',
                                       'opforge.assembler.symbols.scope_layout',
                                       'opforge.assembler.language.conditionals',
                                       'opforge.assembler.symbols.imports',
                                       'opforge.assembler.values.values',
                                       'opforge.assembler.source.records',
                                       'opforge.assembler.symbols.materialized_scope',
                                       'opforge.vm.prvm.packed_loop',
                                       'opforge.vm.prvm.abi')),
 'opforge.assembler.language.statement_execution': ('native/motorola68000/amigaos/assembler/language/statement_execution.asm',
                                                    'begin',
                                                    ('opforge.platform.memory',
                                                     'opforge.packages.format',
                                                     'opforge.assembler.source.records',
                                                     'opforge.assembler.language.statements',
                                                     'opforge.assembler.language.statement_session',
                                                     'opforge.assembler.language.statement_call',
                                                     'opforge.assembler.language.statement_tail',
                                                     'opforge.assembler.language.statement_arguments',
                                                     'opforge.assembler.language.templates',
                                                     'opforge.assembler.prepare.tokenizer_capsule',
                                                     'opforge.vm.prvm.statement_abi')),
 'opforge.assembler.modules.provider': ('native/motorola68000/amigaos/assembler/modules/provider.asm',
                                        'begin',
                                        ('opforge.assembler.prepare.frontend',
                                         'opforge.assembler.symbols.scope_layout',
                                         'opforge.assembler.symbols.lexical_store',
                                         'opforge.assembler.symbols.binding_records',
                                         'opforge.assembler.modules.interface_store',
                                         'opforge.assembler.modules.registry',
                                         'opforge.assembler.modules.semantic_exports',
                                         'opforge.assembler.modules.scope',
                                         'opforge.platform.memory',
                                         'opforge.assembler.values.values',
                                         'opforge.assembler.layout.assembly',
                                         'opforge.packages.format',
                                         'opforge.assembler.layout.dependencies',
                                         'opforge.assembler.symbols.scopes',
                                         'opforge.assembler.modules.remap',
                                         'opforge.assembler.modules.templates',
                                         'opforge.assembler.modules.template_names',
                                         'opforge.vm.exprvm.runtime')),
 'opforge.assembler.output.plan': ('native/motorola68000/amigaos/assembler/output/plan.asm',
                                   'next',
                                   ('opforge.assembler.layout.sections',)),
 'opforge.assembler.output.hunk_image': ('native/motorola68000/amigaos/assembler/output/hunk_image.asm',
                                         'build',
                                         ()),
 'opforge.packages.loader': ('native/motorola68000/amigaos/packages/loader.asm',
                             'acquire',
                             ('opforge.packages.catalog',
                              'opforge.packages.format',
                              'opforge.packages.validation',
                              'opforge.platform.memory')),
 'opforge.packages.validation': ('native/motorola68000/amigaos/packages/validation.asm',
                                 'validate',
                                 ('opforge.packages.format',
                                  'opforge.assembler.layout.state',
                                  'opforge.platform.memory',
                                  'opforge.vm.prvm.abi',
                                  'opforge.vm.prvm.packed_branch',
                                  'opforge.vm.prvm.packed_declaration',
                                  'opforge.vm.prvm.packed_text',
                                  'opforge.vm.prvm.statement_abi',
                                  'opforge.vm.prvm.statement_compile',
                                  'opforge.vm.prvm.statement_match',
                                  'opforge.vm.prvm.statement_head')),
 'opforge.vm.exvm.runtime': ('native/motorola68000/amigaos/vm/exvm/runtime.asm',
                             'compile',
                             ('opforge.vm.tkvm.runtime',
                              'opforge.assembler.source.records',
                              'opforge.debug.runtime_profile')),
 'opforge.vm.exprvm.runtime': ('native/motorola68000/amigaos/vm/exprvm/runtime.asm',
                               'configureStringProvider',
                               ('opforge.vm.exprvm.i64_math', 'opforge.debug.runtime_profile')),
 'opforge.vm.exprvm.i64_math': ('native/motorola68000/amigaos/vm/exprvm/i64_math.asm',
                                'multiplyV1',
                                ()),
 'opforge.vm.prvm.runtime': ('native/motorola68000/amigaos/vm/prvm/runtime.asm',
                             'prvmRun68000',
                             ('opforge.vm.prvm.abi',
                              'opforge.vm.prvm.macro_runtime',
                              'opforge.debug.runtime_profile')),
 'opforge.vm.tkvm.runtime': ('native/motorola68000/amigaos/vm/tkvm/runtime.asm',
                             'tkvmRun68000',
                             ('opforge.vm.tkvm.state',
                              'opforge.vm.tkvm.char_predicates',
                              'opforge.vm.tkvm.scanner',
                              'opforge.debug.runtime_profile')),
 'opforge.vm.encoding.runtime': ('native/motorola68000/amigaos/vm/encoding/runtime.asm',
                                 'semantic',
                                 ('opforge.debug.runtime_profile',)),
 'opforge.vm.value.runtime': ('native/motorola68000/amigaos/vm/value/runtime.asm',
                              'execute',
                              ('opforge.debug.runtime_profile',)),
 'opforge.cli.entry': ('native/motorola68000/amigaos/cli/entry.asm',
                       'start',
                       ('opforge.assembler.driver',
                        'opforge.cli.arguments',
                        'opforge.cli.input',
                        'opforge.cli.target'))}
# Reviewed source closures, not an automatically accepted refresh baseline.
# Each digest covers the ordered repository-relative file names and full texts.
# Counts summarize static definitions; all gates are included, macros unexpanded.
SNAPSHOTS = {'opforge.assembler.driver': ('19b0daddb8c703fbf3771600528b0736e5230cf5f1358f9801979c6c209191b1',
                              185,
                              74,
                              ('code',
                               'data',
                               'bss',
                               'bss',
                               'bss',
                               'code',
                               'bss',
                               'code',
                               'bss',
                               'code',
                               'bss',
                               'data',
                               'code',
                               'data',
                               'code'),
                              195),
 'opforge.assembler.prepare.frontend': ('ace3cc85c9d82f68fc43b1fb6bb7b25159d7e73a7960af4f5299f33fcb522071',
                                        96,
                                        52,
                                        ('bss', 'code', 'data', 'bss'),
                                        155),
 'opforge.assembler.prepare.prepare': ('e4b63ae85852e9c30f763911a0f7bc99ec6586e9e50670000d2330ea7b5bb556',
                                       13,
                                       12,
                                       ('code', 'bss'),
                                       4),
 'opforge.assembler.prepare.operand_binding': ('1803e055988f07884ccdb792394643851d0b0006f49bfe1e41c482188014b2e6',
                                               18,
                                               8,
                                               ('code',),
                                               37),
 'opforge.assembler.prepare.expression': ('c44fc63767a5b1312bd9d4fb0412526ad7bb64609f02a53aaaa628a257ce956a',
                                          19,
                                          14,
                                          ('code', 'bss'),
                                          55),
 'opforge.assembler.prepare.data': ('c31ca6f82213eb4289acd1d23346eb53d4e2fa61a466ef44c4bf11f78fee7dc2',
                                    1,
                                    8,
                                    ('code', 'bss'),
                                    1),
 'opforge.assembler.layout.assembly': ('8528701a5ff9ab6028dee6305f567691fdb36d0882e85c6d7fbdb2fa11ac6f16',
                                       23,
                                       31,
                                       ('bss', 'code'),
                                       39),
 'opforge.assembler.layout.encoding': ('951927b9566350530e76b542e381318bb90ee446a83a0d557446f163b9e00dcb',
                                       63,
                                       18,
                                       ('bss', 'code'),
                                       42),
 'opforge.assembler.layout.dependencies': ('1df2100efda54726c18e2cf752e18455c1def0af5cc114333df3d3a4c93459cc',
                                           16,
                                           16,
                                           ('bss', 'code'),
                                           17),
 'opforge.assembler.layout.sections': ('045cea456b37f31d5b8cff2ba3b6568f440f8224ba8604c9b45e0e487f2cefa9',
                                       13,
                                       2,
                                       ('code',),
                                       9),
 'opforge.assembler.source.control': ('6bfc6727c1e78493210985e50a39b61f7bf1c3e3b4b047bc23b1e6d878701ac6',
                                      16,
                                      11,
                                      ('code', 'bss'),
                                      5),
 'opforge.assembler.language.statement_execution': ('35aee1f2b0aebfb068c9bf331ec698b9a9741fa69e3c8a0f98790a5cc44ac2e5',
                                                    8,
                                                    12,
                                                    ('bss', 'code'),
                                                    16),
 'opforge.assembler.modules.provider': ('3aa997548c1aa944246c5e497b5dc1d8f5bb256c6e59fc0282151f6420f853ef',
                                        27,
                                        18,
                                        ('code',),
                                        17),
 'opforge.assembler.output.plan': ('b73e8fc74c043277116243b24cac922bfd8350c8e78b073e38d2628e3f40c391',
                                   2,
                                   1,
                                   ('code',),
                                   1),
 'opforge.assembler.output.hunk_image': ('2f0cffda4deaa4f6ccaa339f261ca9f743a4ac0e0f6fe0bf2dbdbdb61f44070c',
                                         4,
                                         0,
                                         ('code',),
                                         9),
 'opforge.packages.loader': ('6487ae03a826be7115db79a934981e943d3620dfc0c355810c546774efbe912b',
                             4,
                             5,
                             ('code', 'bss'),
                             18),
 'opforge.packages.validation': ('058758ec69bf15b5f6ce5f0231b2ee3977ca596fe32fa77a2cc6d3f8f5367d6f',
                                 5,
                                 11,
                                 ('code',),
                                 3),
 'opforge.vm.exvm.runtime': ('c2a60578ef92de6753f5d944319e1ecc7b8b87b5d8fc5d9ea7eed160ce49c5e9',
                             14,
                             4,
                             ('code', 'data'),
                             66),
 'opforge.vm.exprvm.runtime': ('c9f13c37b088a8c16aad8acd122e4923f8fd0bf7fff3010ae5384e5aa6109bec',
                               13,
                               2,
                               ('code', 'bss'),
                               6),
 'opforge.vm.exprvm.i64_math': ('4687693b54bb682fe512ced3f588b5fa689a2f3ae27b629e90a66636f7949269',
                                4,
                                0,
                                ('code',),
                                0),
 'opforge.vm.prvm.runtime': ('e99dd24fa86f13597f1a4f07d57d85405555cccab3a985824812df26f8890766',
                             24,
                             3,
                             ('data', 'code'),
                             30),
 'opforge.vm.tkvm.runtime': ('6e0a18cc8da48023966adbca7a32fa444bc28bd96674f6bd5f1ef619a46d38c6',
                             7,
                             5,
                             ('code',),
                             43),
 'opforge.vm.encoding.runtime': ('42cbb47b3172f020f9359fa57f187c686c558ac0a3493c82114044629de300bd',
                                 23,
                                 1,
                                 ('data', 'code'),
                                 15),
 'opforge.vm.value.runtime': ('9a047bf090569560009d98e9458906764b055f313ee1aa53903fae358be4f0bb',
                              2,
                              1,
                              ('code',),
                              4),
 'opforge.cli.entry': ('673f7f8109448d9fbfcdd58ccbd9552c2aa6e36917e080461e8d1860b929c2f6',
                       2,
                       4,
                       ('code', 'data', 'bss'),
                       12)}


@dataclass
class SourceSurface:
    """Unique literal source files owned by one module, in traversal order."""

    sources: dict[str, str]
    blocks: list[str]
    imports: list[str]
    sections: list[str]
    diagnostics: list[str]

    def snapshot(self) -> tuple[str, int, int, tuple[str, ...], int]:
        digest = hashlib.sha256()
        for path, text in self.sources.items():
            digest.update(path.encode() + b"\0" + text.encode() + b"\0")
        return (digest.hexdigest(), len(self.blocks), len(self.imports),
                tuple(self.sections), len(self.diagnostics))


def extract_inventory(source_text: str) -> tuple[list[str], list[str], list[str], list[str]]:
    """Static definitions, including both conditional arms; not assembled output.

    Macro bodies are hashed but are not module routines/imports/sections. Their
    dependencies are covered by the source hash, not simulated expansion.
    Diagnostic matches are lexical review hints, not reachable error paths.
    """
    blocks, imports, sections, diagnostics = [], [], [], []
    macro_depth = 0
    for number, line in enumerate(source_text.splitlines(), 1):
        if re.search(r"(?:diag|debug|error|event|status)", line, re.IGNORECASE):
            diagnostics.append(f"{number}: {line.strip()}")
        if re.match(r"^\s*[A-Za-z_][A-Za-z0-9_]*\s+\.macro\b", line, re.IGNORECASE):
            macro_depth += 1
        elif re.match(r"^\s*\.endmacro\b", line, re.IGNORECASE):
            macro_depth -= 1
            if macro_depth < 0:
                raise ValueError("unmatched .endmacro")
        elif not macro_depth:
            match = re.match(r"^([A-Za-z_][A-Za-z0-9_]*)\s+\.block\b", line, re.IGNORECASE)
            if match:
                blocks.append(match[1])
            match = re.match(r"^\s*\.use\s+([^\s;]+)", line, re.IGNORECASE)
            if match:
                imports.append(match[1])
            match = re.match(r"^\s*\.section\s+([^,\s]+)", line, re.IGNORECASE)
            if match:
                sections.append(match[1])
    if macro_depth:
        raise ValueError("unclosed .macro")
    return blocks, imports, sections, diagnostics


def source_surface(relative: str, root: Path = ROOT) -> SourceSurface:
    """Follow literal includes relative to their including file, fail closed.

    Both conditional arms are covered; template-local includes require review.
    Files are inventoried once per module; this describes ownership, not include
    execution counts. Dynamic includes need an explicit audit design, not a
    silent omission. Imported modules retain their own ownership boundaries.
    """
    surface = SourceSurface({}, [], [], [], [])
    active: list[str] = []
    root = root.resolve()

    def visit(path: Path) -> None:
        path = path.resolve()
        try:
            name = path.relative_to(root).as_posix()
        except ValueError as error:
            raise ValueError(f"include outside repository: {path}") from error
        if name in active:
            raise ValueError("include cycle: " + " -> ".join(active + [name]))
        if name in surface.sources:
            return
        if not path.is_file():
            raise ValueError(f"missing source/include: {name}")
        text = path.read_text(encoding="utf-8")
        surface.sources[name] = text
        blocks, imports, sections, diagnostics = extract_inventory(text)
        surface.blocks.extend(blocks)
        surface.imports.extend(imports)
        surface.sections.extend(sections)
        surface.diagnostics.extend(f"{name}:{item}" for item in diagnostics)
        active.append(name)
        macro_depth = 0
        for number, line in enumerate(text.splitlines(), 1):
            if re.match(r"^\s*[A-Za-z_][A-Za-z0-9_]*\s+\.macro\b", line, re.IGNORECASE):
                macro_depth += 1
            elif re.match(r"^\s*\.endmacro\b", line, re.IGNORECASE):
                macro_depth -= 1
            if not re.match(r"^\s*\.include\b", line, re.IGNORECASE):
                continue
            if macro_depth:
                raise ValueError(f"{name}:{number}: template-local include needs explicit audit")
            match = re.fullmatch(r'\s*\.include\s+"([^"\n]+)"\s*(?:;.*)?', line, re.IGNORECASE)
            if not match:
                raise ValueError(f"{name}:{number}: unsupported nonliteral include")
            visit(path.parent / match[1])
        active.pop()

    visit(root / relative)
    surface.imports = list(dict.fromkeys(surface.imports))
    return surface


def inventory_section(module: str, relative: str, entry: str, surface: SourceSurface) -> str:
    """Canonical reviewed facts; prose about responsibilities stays in the intro."""
    def names(items: list[str]) -> str:
        return ", ".join(f"`{item}`" for item in items) if items else "(none)"

    includes = list(surface.sources)[1:]
    return (f"## `{module}`\n\nSource: `{relative}`\n\n"
            f"Entry: `{entry}`. Static routines: {len(surface.blocks)}. "
            f"Possible imports: {len(surface.imports)}. "
            f"Section declarations: {names(surface.sections)}.\n\n"
            f"Literal include closure ({len(includes)} files): {names(includes)}.\n\n"
            f"Routines: {names(surface.blocks)}.\n\n"
            f"Imports: {names(surface.imports)}.\n")


def validate(root: Path = ROOT) -> list[str]:
    inventory = root / INVENTORY.relative_to(ROOT)
    errors: list[str] = []
    if not inventory.exists():
        return [f"missing inventory: {inventory.relative_to(root)}"]
    inventory_text = inventory.read_text(encoding="utf-8")
    sections = re.findall(r"^## `([^`]+)`\n(.*?)(?=^## |\Z)",
                          inventory_text, re.MULTILINE | re.DOTALL)
    documented = {module: f"## `{module}`\n{body}" for module, body in sections}
    if len(documented) != len(sections):
        errors.append("duplicate inventory module section")
    for module, (relative, entry, imports) in TARGETS.items():
        try:
            surface = source_surface(relative, root)
        except ValueError as error:
            errors.append(f"{module}: {error}")
            continue
        source_text = surface.sources[relative]
        declarations = re.findall(r"^\s*\.module\s+([^\s;]+)", source_text, re.MULTILINE)
        if declarations != [module]:
            errors.append(f"{module}: source module declaration missing or mismatched")
        if entry not in surface.blocks:
            errors.append(f"{module}: representative routine `{entry}` missing")
        if documented.get(module, "").strip() != inventory_section(module, relative, entry, surface).strip():
            errors.append(f"{module}: documented source surface drifted")
        for imported in imports:
            if imported not in surface.imports:
                errors.append(f"{module}: expected import `{imported}` missing")
        actual = surface.snapshot()
        expected = SNAPSHOTS[module]
        if actual[0] != expected[0]:
            errors.append(f"{module}: source/include changed; review before refreshing the inventory")
        if actual[1:] != expected[1:]:
            errors.append(f"{module}: routine/import/state/diagnostic inventory drifted")
    if set(documented) != set(TARGETS):
        errors.append("inventory module set differs from audited targets")
    return errors


def report(root: Path = ROOT) -> None:
    """Print the checked inventory and file-qualified diagnostic review hints."""
    for module, (relative, entry, _imports) in TARGETS.items():
        surface = source_surface(relative, root)
        print(inventory_section(module, relative, entry, surface))
        print("Lexical diagnostic mentions:")
        for diagnostic in surface.diagnostics:
            print(f"- {diagnostic}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--staged", action="store_true", help="Accepted for gate composition; the complete inventory is always checked.")
    parser.add_argument("--report", action="store_true", help="print the complete source inventory after validation")
    args = parser.parse_args()
    errors = validate(args.root)
    if errors:
        print("native runtime boundary inventory: FAIL")
        for error in errors:
            print(f"  - {error}")
        return 1
    print("native runtime boundary inventory: PASS")
    if args.report:
        report(args.root)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

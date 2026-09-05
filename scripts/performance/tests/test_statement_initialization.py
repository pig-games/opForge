#!/usr/bin/env python3
"""Bounded Level C proof for native statement-row initialization.

The restricted interpreter below executes selected routines from the real
``opasm_engine.asm`` source.  It fails closed on any instruction, directive,
conditional, call, or address form outside this proof's explicit model.

This test proves the modeled memory, register, stack, and call-order effects of
``initSessionV1``, ``clearStatementRecord``, ``statementLabelNamePtr``,
``clearBytes``, and the bounded statement-store entry on the covered inputs.
It does not prove instruction encoding, cycle counts, actual 68020 execution,
AmigaOS integration, or the unstubbed ``storeStatementRecord`` body.  Those are
Level D/FS-UAE responsibilities.
"""

from __future__ import annotations

import ast
import dataclasses
import re
import unittest
from pathlib import Path
from typing import Callable

try:
    from test_session_clear import ClearBytesMachine, _load_clear_program
except ModuleNotFoundError:
    from scripts.performance.tests.test_session_clear import (
        ClearBytesMachine,
        _load_clear_program,
    )


ROOT = Path(__file__).resolve().parents[3]
ENGINE_SOURCE = ROOT / "native/motorola68000/amigaos/opasm/opasm_engine.asm"
U32_MASK = 0xFFFF_FFFF
POISON = 0xA5


@dataclasses.dataclass(frozen=True)
class Instruction:
    opcode: str
    operands: str
    line_number: int
    source: str


@dataclasses.dataclass
class Routine:
    instructions: list[Instruction]
    labels: dict[str, int]


@dataclasses.dataclass(frozen=True)
class BssLayout:
    symbols: dict[str, int]
    reservations: dict[str, tuple[int, int, int]]
    end: int


def _constant_expressions(source: str) -> dict[str, str]:
    expressions: dict[str, str] = {}
    for line in source.splitlines():
        code = line.split(";", 1)[0]
        match = re.match(r"^\s*([A-Z][A-Z0-9_]*)\s*=\s*(.*?)\s*$", code)
        if match:
            expressions[match.group(1)] = match.group(2)
    return expressions


class Constants:
    def __init__(self, source: str) -> None:
        self.expressions = _constant_expressions(source)
        self.active: set[str] = set()

    def get(self, name: str) -> int:
        if name in self.active:
            raise AssertionError(f"cyclic constant definition involving {name}")
        try:
            expression = self.expressions[name]
        except KeyError as error:
            raise AssertionError(f"unknown constant {name}") from error
        self.active.add(name)
        try:
            return self.eval(expression)
        finally:
            self.active.remove(name)

    def eval(self, expression: str) -> int:
        expression = re.sub(r"\$([0-9a-fA-F]+)", r"0x\1", expression)

        def evaluate(node: ast.AST) -> int:
            if isinstance(node, ast.Constant) and isinstance(node.value, int):
                return node.value
            if isinstance(node, ast.Name):
                return self.get(node.id)
            if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.USub):
                return -evaluate(node.operand)
            if isinstance(node, ast.BinOp) and isinstance(
                node.op, (ast.Add, ast.Sub, ast.Mult)
            ):
                left = evaluate(node.left)
                right = evaluate(node.right)
                if isinstance(node.op, ast.Add):
                    return left + right
                if isinstance(node.op, ast.Sub):
                    return left - right
                return left * right
            raise AssertionError(
                f"unsupported constant expression {expression!r}: {ast.dump(node)}"
            )

        return evaluate(ast.parse(expression, mode="eval").body)


def _extract_block(source: str, name: str) -> list[tuple[int, str]]:
    start = re.compile(rf"^\s*{re.escape(name)}\s+\.block\b")
    lines = source.splitlines()
    for index, line in enumerate(lines):
        if start.match(line):
            break
    else:
        raise AssertionError(f"missing {name} .block")
    block: list[tuple[int, str]] = []
    for line_number, line in enumerate(lines[index + 1 :], index + 2):
        code = line.split(";", 1)[0].rstrip()
        if re.match(r"^\s*\.bend\b", code):
            return block
        block.append((line_number, code))
    raise AssertionError(f"unterminated {name} .block")


def _select_conditionals(
    lines: list[tuple[int, str]], enabled: frozenset[str]
) -> list[tuple[int, str]]:
    known = {
        "OPFORGE_PROGRESS_PLATFORM_COUNTERS",
        "OPFORGE_SESSION_CLEAR_ALL_STATEMENTS",
        "OPFORGE_SESSION_CLEAR_BYTE_REFERENCE",
    }
    active = True
    stack: list[tuple[bool, bool, bool]] = []
    selected: list[tuple[int, str]] = []
    for line_number, line in lines:
        stripped = line.strip()
        match = re.fullmatch(r"\.(ifdef|ifndef)\s+([A-Z0-9_]+)", stripped)
        if match:
            kind, symbol = match.groups()
            if symbol not in known:
                raise AssertionError(
                    f"unsupported conditional {symbol} on line {line_number}"
                )
            condition = symbol in enabled
            if kind == "ifndef":
                condition = not condition
            stack.append((active, condition, False))
            active = active and condition
            continue
        if stripped == ".else":
            if not stack:
                raise AssertionError(f"unmatched .else on line {line_number}")
            parent, condition, seen_else = stack[-1]
            if seen_else:
                raise AssertionError(f"duplicate .else on line {line_number}")
            stack[-1] = (parent, condition, True)
            active = parent and not condition
            continue
        if stripped == ".endif":
            if not stack:
                raise AssertionError(f"unmatched .endif on line {line_number}")
            parent, _, _ = stack.pop()
            active = parent
            continue
        if active and stripped:
            selected.append((line_number, line.rstrip()))
    if stack:
        raise AssertionError("unterminated conditional")
    return selected


def _load_routine(source: str, name: str, enabled: frozenset[str]) -> Routine:
    instructions: list[Instruction] = []
    labels: dict[str, int] = {}
    for line_number, line in _select_conditionals(_extract_block(source, name), enabled):
        stripped = line.strip()
        if not line[0].isspace():
            if not re.fullmatch(r"[A-Za-z][A-Za-z0-9_]*", stripped):
                raise AssertionError(f"unsupported label on line {line_number}: {line}")
            label = stripped.lower()
            if label in labels:
                raise AssertionError(f"duplicate label {label} in {name}")
            labels[label] = len(instructions)
            continue
        parts = stripped.split(None, 1)
        opcode = parts[0].lower()
        if opcode.startswith("."):
            raise AssertionError(f"unsupported directive on line {line_number}: {line}")
        operands = re.sub(r"\s+", "", parts[1]).lower() if len(parts) == 2 else ""
        instructions.append(Instruction(opcode, operands, line_number, line))
    return Routine(instructions, labels)


def _parse_bss(source: str, constants: Constants) -> BssLayout:
    in_bss = False
    offset = 0
    pending: list[str] = []
    symbols: dict[str, int] = {}
    reservations: dict[str, tuple[int, int, int]] = {}
    for line_number, raw in enumerate(source.splitlines(), 1):
        code = raw.split(";", 1)[0].strip()
        if re.match(r"^\.section\s+bss\b", code):
            in_bss = True
            continue
        if not in_bss:
            continue
        if code == ".endsection":
            for label in pending:
                symbols[label.lower()] = offset
            return BssLayout(symbols, reservations, offset)
        if not code:
            continue
        if match := re.fullmatch(r"\.align\s+(\d+)", code):
            alignment = int(match.group(1))
            offset = (offset + alignment - 1) // alignment * alignment
            continue
        if re.fullmatch(r"[A-Za-z][A-Za-z0-9_]*", code):
            pending.append(code)
            continue
        if match := re.fullmatch(r"\.res\s+(byte|word|long)\s*,\s*(.+)", code):
            if not pending:
                raise AssertionError(f"reservation without label on line {line_number}")
            width = {"byte": 1, "word": 2, "long": 4}[match.group(1)]
            count = constants.eval(match.group(2))
            for label in pending:
                lowered = label.lower()
                symbols[lowered] = offset
                reservations[lowered] = (offset, width, count)
            pending.clear()
            offset += width * count
            continue
        raise AssertionError(f"unsupported BSS directive on line {line_number}: {code}")
    raise AssertionError("missing BSS endsection")


class NativeMachine:
    """Fail-closed interpreter for the exact Step 13 proof surface."""

    def __init__(
        self,
        routines: dict[str, Routine],
        constants: Constants,
        layout: BssLayout,
        memory: bytearray,
        registers: dict[str, int],
        clear_program: object,
        external_calls: dict[str, Callable[["NativeMachine"], int]] | None = None,
    ) -> None:
        self.routines = {name.lower(): routine for name, routine in routines.items()}
        self.constants = constants
        self.layout = layout
        self.memory = memory
        self.registers = registers.copy()
        self.clear_program = clear_program
        self.ccr = {"n": False, "z": False, "v": False, "c": False, "x": True}
        self.external_calls = {
            name.lower(): call for name, call in (external_calls or {}).items()
        }
        self.call_stack: list[tuple[str, int, int]] = []
        self.movem_frames: dict[int, tuple[list[str], list[int]]] = {}
        self.write_ranges: list[tuple[int, int]] = []
        self.accelerated_blocks: list[tuple[str, str, int]] = []
        self.routine = ""
        self.pc = 0

    def _check(self, address: int, width: int) -> None:
        if address < 0 or address + width > len(self.memory):
            raise AssertionError(f"memory access outside model: [{address}, {address + width})")

    def read(self, address: int, width: int) -> int:
        self._check(address, width)
        return int.from_bytes(self.memory[address : address + width], "big")

    def write(self, address: int, width: int, value: int = 0) -> None:
        self._check(address, width)
        if width in {2, 4} and address % 2:
            raise AssertionError(f"word/long access is odd-addressed: {address}")
        self.memory[address : address + width] = (value & ((1 << (width * 8)) - 1)).to_bytes(
            width, "big"
        )
        self.write_ranges.append((address, address + width))

    def _set_nzvc(self, value: int, width: int) -> None:
        mask = (1 << width) - 1
        value &= mask
        self.ccr.update(
            n=bool(value & (1 << (width - 1))), z=value == 0, v=False, c=False
        )

    def _compare(self, destination: int, source: int) -> None:
        destination &= U32_MASK
        source &= U32_MASK
        result = (destination - source) & U32_MASK
        overflow = bool(((destination ^ source) & (destination ^ result)) & 0x8000_0000)
        self.ccr.update(
            n=bool(result & 0x8000_0000),
            z=result == 0,
            v=overflow,
            c=destination < source,
        )

    def _value(self, expression: str) -> int:
        return self.constants.eval(expression.upper()) & U32_MASK

    def _symbol(self, token: str) -> int:
        token = re.sub(r"\.l$", "", token).lower()
        try:
            return self.layout.symbols[token]
        except KeyError as error:
            raise AssertionError(f"unknown BSS symbol {token}") from error

    @staticmethod
    def _expand_registers(specification: str) -> list[str]:
        result: list[str] = []
        for part in specification.split("/"):
            match = re.fullmatch(r"([da])(\d)-\1(\d)", part)
            if match:
                prefix, first, last = match.groups()
                result.extend(f"{prefix}{number}" for number in range(int(first), int(last) + 1))
            elif re.fullmatch(r"[da][0-7]", part):
                result.append(part)
            else:
                raise AssertionError(f"unsupported register list {specification}")
        return result

    def _movem_push(self, specification: str) -> None:
        names = self._expand_registers(specification)
        address = (self.registers["a7"] - 4 * len(names)) & U32_MASK
        if address in self.movem_frames:
            raise AssertionError("overlapping MOVEM frame")
        self.registers["a7"] = address
        self.movem_frames[address] = (names, [self.registers[name] for name in names])

    def _movem_pop(self, specification: str) -> None:
        address = self.registers["a7"]
        try:
            names, values = self.movem_frames.pop(address)
        except KeyError as error:
            raise AssertionError("MOVEM stack underflow") from error
        expected = self._expand_registers(specification)
        if names != expected:
            raise AssertionError(f"MOVEM restore mismatch: saved {names}, restored {expected}")
        for name, value in zip(names, values):
            self.registers[name] = value
        self.registers["a7"] = (address + 4 * len(names)) & U32_MASK

    def _branch(self, label: str) -> None:
        try:
            self.pc = self.routines[self.routine].labels[label]
        except KeyError as error:
            raise AssertionError(f"unknown branch {label} in {self.routine}") from error

    def _call(self, target: str) -> None:
        target = target.lower()
        return_slot = (self.registers["a7"] - 4) & U32_MASK
        self.registers["a7"] = return_slot
        if target == "clearbytes":
            start = self.registers["a1"]
            length = self.registers["d0"] & U32_MASK
            clear = ClearBytesMachine(
                self.clear_program, self.memory, self.registers, self.ccr
            )
            clear.run()
            self.registers = clear.registers
            self.ccr = clear.ccr
            if length:
                self.write_ranges.append((start, start + length))
            self.accelerated_blocks.extend(
                ("clearbytes", label, count)
                for label, count in clear.accelerated_blocks
            )
            if self.registers["a7"] != return_slot:
                raise AssertionError("clearBytes returned with an unbalanced stack")
            self.registers["a7"] = (return_slot + 4) & U32_MASK
            return
        if target in self.external_calls:
            self.registers["d0"] = self.external_calls[target](self) & U32_MASK
            self._set_nzvc(self.registers["d0"], 32)
            self.registers["a7"] = (return_slot + 4) & U32_MASK
            return
        if target not in self.routines:
            raise AssertionError(f"unsupported call target {target}")
        self.call_stack.append((self.routine, self.pc, return_slot))
        self.routine = target
        self.pc = 0

    def _return(self) -> bool:
        if not self.call_stack:
            return True
        routine, pc, return_slot = self.call_stack.pop()
        if self.registers["a7"] != return_slot:
            raise AssertionError("callee returned with an unbalanced stack")
        self.registers["a7"] = (return_slot + 4) & U32_MASK
        self.routine = routine
        self.pc = pc
        return False

    def invoke(self, name: str, instruction_budget: int = 1_000_000) -> None:
        name = name.lower()
        if name not in self.routines:
            raise AssertionError(f"unknown entry routine {name}")
        if self.call_stack or self.movem_frames:
            raise AssertionError("machine stack is dirty before invocation")
        self.routine = name
        self.pc = 0
        for _ in range(instruction_budget):
            routine = self.routines[self.routine]
            if self.pc >= len(routine.instructions):
                raise AssertionError(f"{self.routine} fell off its instruction stream")
            instruction = routine.instructions[self.pc]
            self.pc += 1
            try:
                if self._execute(instruction):
                    if self.call_stack or self.movem_frames:
                        raise AssertionError("entry returned with dirty modeled stacks")
                    return
            except AssertionError as error:
                raise AssertionError(
                    f"{error} (line {instruction.line_number}: {instruction.source})"
                ) from error
        raise AssertionError(f"{name} exceeded the bounded instruction budget")

    def _execute(self, instruction: Instruction) -> bool:
        opcode, operands = instruction.opcode, instruction.operands
        if opcode == "movem.l":
            if operands.endswith(",-(sp)"):
                self._movem_push(operands[: -len(",-(sp)")])
            elif operands.startswith("(sp)+,"):
                self._movem_pop(operands[len("(sp)+,") :])
            else:
                self._unsupported(instruction)
        elif opcode == "lea":
            match = re.fullmatch(r"([a-z][a-z0-9_]*\.l),(a[0-7])", operands)
            if not match:
                self._unsupported(instruction)
            self.registers[match.group(2)] = self._symbol(match.group(1))
        elif opcode == "movea.l":
            match = re.fullmatch(r"(a[0-7]),(a[0-7])", operands)
            if not match:
                self._unsupported(instruction)
            self.registers[match.group(2)] = self.registers[match.group(1)]
        elif opcode in {"move.l", "move.b"}:
            self._move(opcode, operands, instruction)
        elif opcode in {"clr.b", "clr.w", "clr.l"}:
            self._clear(opcode, operands, instruction)
        elif opcode == "lsl.l":
            match = re.fullmatch(r"#(\d+),(d[0-7])", operands)
            if not match:
                self._unsupported(instruction)
            shift, register = int(match.group(1)), match.group(2)
            old = self.registers[register] & U32_MASK
            result = (old << shift) & U32_MASK
            self.registers[register] = result
            self._set_nzvc(result, 32)
        elif opcode in {"add.l", "adda.l"}:
            match = re.fullmatch(r"([da][0-7]),([da][0-7])", operands)
            if not match:
                self._unsupported(instruction)
            source, destination = match.groups()
            result = (self.registers[destination] + self.registers[source]) & U32_MASK
            self.registers[destination] = result
            if opcode == "add.l":
                self._set_nzvc(result, 32)
        elif opcode in {"addq.l", "subq.l"}:
            match = re.fullmatch(r"#1,(d[0-7])", operands)
            if not match:
                self._unsupported(instruction)
            register = match.group(1)
            old = self.registers[register] & U32_MASK
            result = (
                (old + 1) & U32_MASK
                if opcode == "addq.l"
                else (old - 1) & U32_MASK
            )
            self.registers[register] = result
            carry = old == U32_MASK if opcode == "addq.l" else old == 0
            self.ccr.update(
                n=bool(result & 0x8000_0000),
                z=result == 0,
                v=(
                    old == 0x7FFF_FFFF
                    if opcode == "addq.l"
                    else old == 0x8000_0000
                ),
                c=carry,
                x=carry,
            )
        elif opcode in {"cmp.l", "cmpi.l"}:
            match = re.fullmatch(r"#(.+),(d[0-7])", operands)
            if not match:
                self._unsupported(instruction)
            self._compare(self.registers[match.group(2)], self._value(match.group(1)))
        elif opcode == "tst.l":
            if re.fullmatch(r"d[0-7]", operands):
                value = self.registers[operands]
            else:
                match = re.fullmatch(r"([a-z][a-z0-9_]*|[a-z0-9_]+)\((a[0-7])\)", operands)
                if not match:
                    self._unsupported(instruction)
                offset = self._value(match.group(1))
                value = self.read(self.registers[match.group(2)] + offset, 4)
            self._set_nzvc(value, 32)
        elif opcode.startswith("b") and opcode not in {"bsr.w"}:
            condition = opcode.split(".", 1)[0]
            take = {
                "bra": True,
                "beq": self.ccr["z"],
                "bne": not self.ccr["z"],
                "bhi": not self.ccr["c"] and not self.ccr["z"],
                "bhs": not self.ccr["c"],
            }.get(condition)
            if take is None:
                self._unsupported(instruction)
            if take:
                self._branch(operands)
        elif opcode == "bsr.w":
            self._call(operands)
        elif opcode == "moveq":
            match = re.fullmatch(r"#(-?\d+),(d[0-7])", operands)
            if not match:
                self._unsupported(instruction)
            value = int(match.group(1)) & U32_MASK
            self.registers[match.group(2)] = value
            self._set_nzvc(value, 32)
        elif opcode == "rts" and not operands:
            return self._return()
        else:
            self._unsupported(instruction)
        return False

    def _move(self, opcode: str, operands: str, instruction: Instruction) -> None:
        width = 1 if opcode == "move.b" else 4
        register_move = re.fullmatch(r"([da][0-7]),([da][0-7])", operands)
        if register_move:
            source, destination = register_move.groups()
            value = self.registers[source] & ((1 << (width * 8)) - 1)
            if width == 1:
                self.registers[destination] = (self.registers[destination] & 0xFFFF_FF00) | value
            else:
                self.registers[destination] = value
            self._set_nzvc(value, width * 8)
            return
        immediate = re.fullmatch(r"#(.+),(d[0-7])", operands)
        if immediate:
            value = self._value(immediate.group(1))
            self.registers[immediate.group(2)] = value
            self._set_nzvc(value, 32)
            return
        symbol_read = re.fullmatch(r"([a-z][a-z0-9_]*\.l),(d[0-7])", operands)
        if symbol_read:
            value = self.read(self._symbol(symbol_read.group(1)), 4)
            self.registers[symbol_read.group(2)] = value
            self._set_nzvc(value, 32)
            return
        indexed_read = re.fullmatch(r"([a-z0-9_]+)\((a[0-7])\),(d[0-7])", operands)
        if indexed_read:
            offset, address_register, destination = indexed_read.groups()
            value = self.read(self.registers[address_register] + self._value(offset), 4)
            self.registers[destination] = value
            self._set_nzvc(value, 32)
            return
        postincrement_read = re.fullmatch(r"\((a[0-7])\)\+,(d[0-7])", operands)
        if postincrement_read and width == 1:
            address_register, destination = postincrement_read.groups()
            address = self.registers[address_register]
            value = self.read(address, 1)
            self.registers[address_register] = address + 1
            self.registers[destination] = (self.registers[destination] & 0xFFFF_FF00) | value
            self._set_nzvc(value, 8)
            return
        postincrement_write = re.fullmatch(r"(d[0-7]),\((a[0-7])\)\+", operands)
        if postincrement_write and width == 1:
            source, address_register = postincrement_write.groups()
            address = self.registers[address_register]
            self.write(address, 1, self.registers[source])
            self.registers[address_register] = address + 1
            self._set_nzvc(self.registers[source], 8)
            return
        symbol_write = re.fullmatch(r"(d[0-7]),([a-z][a-z0-9_]*\.l)", operands)
        if symbol_write and width == 4:
            self.write(
                self._symbol(symbol_write.group(2)),
                4,
                self.registers[symbol_write.group(1)],
            )
            self._set_nzvc(self.registers[symbol_write.group(1)], 32)
            return
        self._unsupported(instruction)

    def _clear(self, opcode: str, operands: str, instruction: Instruction) -> None:
        width = {"clr.b": 1, "clr.w": 2, "clr.l": 4}[opcode]
        postincrement = re.fullmatch(r"\((a[0-7])\)\+", operands)
        if postincrement:
            register = postincrement.group(1)
            address = self.registers[register]
            if width == 4 and self.routine == "clearbytes" and address % 4:
                raise AssertionError("clearBytes longword write is not four-byte aligned")
            self.write(address, width)
            self.registers[register] = address + width
        elif predecrement := re.fullmatch(r"-\((a[0-7])\)", operands):
            register = predecrement.group(1)
            self.registers[register] -= width
            self.write(self.registers[register], width)
        elif indexed := re.fullmatch(r"0\((a[0-7]),(d[0-7])\.l\)", operands):
            address_register, index_register = indexed.groups()
            self.write(self.registers[address_register] + self.registers[index_register], width)
        elif symbol := re.fullmatch(r"([a-z][a-z0-9_]*\.l)", operands):
            self.write(self._symbol(symbol.group(1)), width)
        elif indirect := re.fullmatch(r"\((a[0-7])\)", operands):
            self.write(self.registers[indirect.group(1)], width)
        else:
            self._unsupported(instruction)
        self._set_nzvc(0, width * 8)

    @staticmethod
    def _unsupported(instruction: Instruction) -> None:
        raise AssertionError(
            f"unsupported instruction {instruction.opcode} {instruction.operands}"
        )


ROW_FIELDS = (
    ("OpasmEngineStmtLineTable", 4),
    ("OpasmEngineStmtSourceRecordIndexTable", 4),
    ("OpasmEngineStmtLabelLenTable", 2),
    ("OpasmEngineStmtMnemLenTable", 2),
    ("OpasmEngineStmtOperandLenTable", 2),
    ("OpasmEngineStmtOwnerLenTable", 2),
    ("OpasmEngineStmtDirectiveKindTable", 2),
    ("OpasmEngineStmtOutputAddrTable", 4),
    ("OpasmEngineStmtOperandStartTable", 4),
    ("OpasmEngineStmtOperandEndTable", 4),
    ("OpasmEngineStmtOutputOffsetTable", 4),
    ("OpasmEngineStmtOutputByteCountTable", 4),
    ("OpasmEngineStmtMnemStartTable", 4),
    ("OpasmEngineStmtLabelNameTable", 108),
    ("OpasmEngineStmtOperandNameTable", 64),
    ("OpasmEngineStmtOwnerNameTable", 64),
    ("OpasmEngineStmtExprFlagsTable", 2),
    ("OpasmEngineStmtExprOperandIndexTable", 4),
    ("OpasmEngineStmtExprSlotIndexTable", 4),
    ("OpasmEngineStmtExprStartTokenTable", 4),
    ("OpasmEngineStmtExprEndTokenTable", 4),
    ("OpasmEngineStmtExprSpanLineTable", 4),
    ("OpasmEngineStmtExprSpanStartTable", 4),
    ("OpasmEngineStmtExprSpanEndTable", 4),
)


def _registers() -> dict[str, int]:
    registers = {
        **{f"d{i}": (0x1020_3040 + i * 0x0101_0101) & U32_MASK for i in range(8)},
        **{f"a{i}": (0x5060_7080 + i * 0x0101_0101) & U32_MASK for i in range(8)},
    }
    registers["a7"] = 0x7000_1000
    return registers


def _merged(ranges: list[tuple[int, int]]) -> list[tuple[int, int]]:
    merged: list[list[int]] = []
    for start, end in sorted(ranges):
        if merged and start <= merged[-1][1]:
            merged[-1][1] = max(merged[-1][1], end)
        else:
            merged.append([start, end])
    return [(start, end) for start, end in merged]


class StatementInitializationLevelCTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = ENGINE_SOURCE.read_text(encoding="utf-8")
        cls.constants = Constants(cls.source)
        cls.layout = _parse_bss(cls.source, cls.constants)
        cls.clear_program = _load_clear_program(cls.source, byte_reference=False)
        cls.capacity = cls.constants.get("NATIVE_STATEMENT_TABLE_CAPACITY")
        cls.session_start = cls.layout.symbols["opasmengineassemblysessionstart"]
        cls.session_end = cls.layout.symbols["opasmengineassemblysessionend"]
        cls.statement_start = cls.layout.symbols["opasmenginestmtlinetable"]
        cls.statement_end = cls.layout.symbols["opasmenginelabelvaluetable"]
        cls.routine_names = (
            "initSessionV1",
            "clearStatementRecord",
            "statementLabelNamePtr",
            "opasmEngineStoreStatementRecordV1",
            "opasmEngineCommitStatementRecordV1",
            "resetStatementCollectionV1",
        )

    def _routines(self, reference: bool = False) -> dict[str, Routine]:
        enabled = (
            frozenset({"OPFORGE_SESSION_CLEAR_ALL_STATEMENTS"})
            if reference
            else frozenset()
        )
        return {
            name: _load_routine(self.source, name, enabled) for name in self.routine_names
        }

    def _memory(self, extra: int = 512) -> bytearray:
        return bytearray([POISON]) * (self.layout.end + extra)

    def _row_ranges(self, index: int) -> list[tuple[int, int]]:
        return [
            (
                self.layout.symbols[name.lower()] + index * width,
                self.layout.symbols[name.lower()] + (index + 1) * width,
            )
            for name, width in ROW_FIELDS
        ]

    def _row_bytes(self, memory: bytearray, index: int) -> bytes:
        return b"".join(memory[start:end] for start, end in self._row_ranges(index))

    def _set_long(self, memory: bytearray, symbol: str, value: int) -> None:
        address = self.layout.symbols[symbol.lower()]
        memory[address : address + 4] = value.to_bytes(4, "big")

    def _make_machine(
        self,
        memory: bytearray,
        reference: bool = False,
        external_calls: dict[str, Callable[[NativeMachine], int]] | None = None,
    ) -> NativeMachine:
        return NativeMachine(
            self._routines(reference),
            self.constants,
            self.layout,
            memory,
            _registers(),
            self.clear_program,
            external_calls,
        )

    def _install_cpu_name(self, machine: NativeMachine, text: bytes = b"68020\0") -> int:
        address = self.layout.end + 64
        machine.memory[address : address + len(text)] = text
        machine.registers["a0"] = address
        return address

    def _install_valid_request(self, machine: NativeMachine) -> int:
        address = self.layout.end + 128
        machine.memory[address : address + 76] = bytes(76)
        machine.registers["a0"] = self.layout.end + 16
        machine.registers["a1"] = self.layout.end + 32
        machine.registers["a2"] = address
        return address

    def test_bss_layout_independently_derives_all_24_tables_and_split_constants(self) -> None:
        self.assertEqual(len(ROW_FIELDS), 24)
        self.assertEqual(sum(width for _, width in ROW_FIELDS), 308)
        cursor = self.statement_start
        for name, width in ROW_FIELDS:
            address, declared_width, count = self.layout.reservations[name.lower()]
            self.assertEqual(address, cursor, name)
            if width in {64, 108}:
                self.assertEqual(declared_width, 1, name)
                self.assertEqual(count, self.capacity * width, name)
            else:
                self.assertEqual(declared_width, width, name)
                self.assertEqual(count, self.capacity, name)
            cursor += width * self.capacity
        self.assertEqual(cursor, self.statement_end)
        self.assertEqual(self.statement_end - self.statement_start, self.capacity * 308)
        self.assertEqual(self.statement_start % 4, 2)
        self.assertEqual(self.statement_end % 4, 2)

        derived_prefix = self.statement_start - self.session_start
        derived_suffix = self.session_start + self.constants.get(
            "OPASM_ENGINE_ASSEMBLY_SESSION_BYTES"
        ) - self.statement_end
        self.assertEqual(derived_prefix, 5_194_402)
        self.assertEqual(
            derived_prefix,
            self.constants.get("OPASM_ENGINE_SESSION_STATEMENT_OFFSET"),
        )
        self.assertEqual(
            derived_suffix,
            self.constants.get("OPASM_ENGINE_SESSION_AFTER_STATEMENT_BYTES"),
        )
        self.assertEqual(
            self.session_end - self.session_start,
            self.constants.get("OPASM_ENGINE_ASSEMBLY_SESSION_BYTES") + 6,
        )

    def test_init_executes_exact_selective_and_reference_memory_effects(self) -> None:
        candidate_memory = self._memory()
        candidate = self._make_machine(candidate_memory)
        cpu_address = self._install_cpu_name(candidate)
        before_registers = candidate.registers.copy()
        candidate.invoke("initSessionV1")

        cpu_name = self.layout.symbols["opasmenginesessioncpuname"]
        declared_end = self.session_start + self.constants.get(
            "OPASM_ENGINE_ASSEMBLY_SESSION_BYTES"
        )
        expected_prefix = bytearray(self.statement_start - self.session_start)
        expected_prefix[
            cpu_name - self.session_start : cpu_name - self.session_start + 6
        ] = b"68020\0"
        self.assertEqual(
            candidate_memory[self.session_start : self.statement_start], expected_prefix
        )
        self.assertEqual(
            candidate_memory[self.statement_start : self.statement_end],
            bytes([POISON]) * (self.statement_end - self.statement_start),
        )
        self.assertEqual(
            candidate_memory[self.statement_end : declared_end],
            bytes(declared_end - self.statement_end),
        )
        self.assertEqual(
            candidate_memory[declared_end : self.session_end], bytes([POISON]) * 6
        )
        self.assertEqual(candidate.registers["d0"], 0)
        for register in [f"d{i}" for i in range(1, 8)] + [f"a{i}" for i in range(7)]:
            self.assertEqual(candidate.registers[register], before_registers[register], register)
        self.assertEqual(candidate.registers["a7"], before_registers["a7"])
        self.assertEqual(candidate.registers["a0"], cpu_address)
        self.assertFalse(candidate.call_stack)
        self.assertFalse(candidate.movem_frames)
        self.assertGreaterEqual(len(candidate.accelerated_blocks), 2)

        reference_memory = self._memory()
        reference = self._make_machine(reference_memory, reference=True)
        self._install_cpu_name(reference)
        reference.invoke("initSessionV1")
        expected_reference = bytearray(declared_end - self.session_start)
        expected_reference[
            cpu_name - self.session_start : cpu_name - self.session_start + 6
        ] = b"68020\0"
        self.assertEqual(
            reference_memory[self.session_start : declared_end], expected_reference
        )
        self.assertEqual(
            reference_memory[declared_end : self.session_end], bytes([POISON]) * 6
        )

    def test_clear_statement_executes_all_fields_at_boundary_indices(self) -> None:
        for index in (0, 1, 65_535, 65_536, 99_999):
            with self.subTest(index=index):
                memory = self._memory(0)
                machine = self._make_machine(memory)
                before_registers = machine.registers.copy()
                machine.registers["d0"] = index
                expected_registers = machine.registers.copy()
                machine.invoke("clearStatementRecord")
                self.assertEqual(self._row_bytes(memory, index), bytes(308))
                self.assertEqual(_merged(machine.write_ranges), self._row_ranges(index))
                for start, end in self._row_ranges(index):
                    if start > self.statement_start:
                        self.assertEqual(memory[start - 1], POISON)
                    if end < self.statement_end:
                        self.assertEqual(memory[end], POISON)
                self.assertEqual(machine.registers, expected_registers)
                self.assertEqual(machine.registers["a7"], before_registers["a7"])
                self.assertFalse(machine.call_stack)
                self.assertFalse(machine.movem_frames)

    def test_store_boundary_rejects_capacity_before_any_row_write(self) -> None:
        memory = self._memory()
        store_called = False

        def unexpected_store(_: NativeMachine) -> int:
            nonlocal store_called
            store_called = True
            return 0

        machine = self._make_machine(
            memory, external_calls={"storeStatementRecord": unexpected_store}
        )
        self._install_valid_request(machine)
        self._set_long(memory, "OpasmEngineStmtCount", self.capacity)
        before = bytes(memory)
        before_registers = machine.registers.copy()
        machine.invoke("opasmEngineStoreStatementRecordV1")
        self.assertEqual(machine.registers["d0"], 1)
        self.assertFalse(store_called)
        self.assertEqual(memory, before)
        for register in [f"d{i}" for i in range(1, 8)] + [f"a{i}" for i in range(6)]:
            self.assertEqual(machine.registers[register], before_registers[register], register)
        self.assertEqual(machine.registers["a7"], before_registers["a7"])

        candidate = _select_conditionals(
            _extract_block(self.source, "opasmEngineStoreStatementRecordV1"), frozenset()
        )
        normalized = [re.sub(r"\s+", "", line).lower() for _, line in candidate]
        capacity_check = normalized.index("cmpi.l#native_statement_table_capacity,d0")
        capacity_branch = normalized.index("bhs.wfail")
        clear_call = normalized.index("bsr.wclearstatementrecord")
        store_call = normalized.index("bsr.wstorestatementrecord")
        self.assertLess(capacity_check, capacity_branch)
        self.assertLess(capacity_branch, clear_call)
        self.assertLess(clear_call, store_call)
        reference = _select_conditionals(
            _extract_block(self.source, "opasmEngineStoreStatementRecordV1"),
            frozenset({"OPFORGE_SESSION_CLEAR_ALL_STATEMENTS"}),
        )
        self.assertNotIn(
            "bsr.wclearstatementrecord",
            [re.sub(r"\s+", "", line).lower() for _, line in reference],
        )

    def test_failed_store_retry_count_reset_and_live_rows_match_reference(self) -> None:
        def run_store(machine: NativeMachine, status: int, seed: int) -> None:
            def stub(active: NativeMachine) -> int:
                count = active.read(
                    self.layout.symbols["opasmenginestmtcount"], 4
                )
                for field_number, (start, end) in enumerate(self._row_ranges(count)):
                    active.write(start, 1, (seed + field_number) & 0xFF)
                    if status:
                        break
                return status

            machine.external_calls["storestatementrecord"] = stub
            self._install_valid_request(machine)
            machine.invoke("opasmEngineStoreStatementRecordV1")

        candidate_memory = self._memory()
        candidate = self._make_machine(candidate_memory)
        self._install_cpu_name(candidate)
        candidate.invoke("initSessionV1")

        # A failed store may leave partial bytes, but it cannot advance the count.
        run_store(candidate, status=1, seed=0xE0)
        self.assertEqual(candidate.read(self.layout.symbols["opasmenginestmtcount"], 4), 0)
        self.assertEqual(candidate_memory[self._row_ranges(0)[0][0]], 0xE0)

        # Retrying the same uncommitted row clears the partial rollback residue.
        run_store(candidate, status=0, seed=0x20)
        self.assertEqual(candidate.registers["d0"], 0)
        candidate.invoke("opasmEngineCommitStatementRecordV1")
        self.assertEqual(candidate.read(self.layout.symbols["opasmenginestmtcount"], 4), 1)

        # Commit a second row, reset only the count, then reuse row zero.
        run_store(candidate, status=0, seed=0x40)
        candidate.invoke("opasmEngineCommitStatementRecordV1")
        self.assertEqual(candidate.read(self.layout.symbols["opasmenginestmtcount"], 4), 2)
        candidate.invoke("resetStatementCollectionV1")
        self.assertEqual(candidate.read(self.layout.symbols["opasmenginestmtcount"], 4), 0)
        run_store(candidate, status=0, seed=0x60)
        candidate.invoke("opasmEngineCommitStatementRecordV1")
        self.assertEqual(candidate.read(self.layout.symbols["opasmenginestmtcount"], 4), 1)

        # The reference build clears every statement up front and omits row clears.
        reference_memory = self._memory()
        reference = self._make_machine(reference_memory, reference=True)
        self._install_cpu_name(reference)
        reference.invoke("initSessionV1")
        run_store(reference, status=0, seed=0x60)
        reference.invoke("opasmEngineCommitStatementRecordV1")
        live_count = reference.read(self.layout.symbols["opasmenginestmtcount"], 4)
        self.assertEqual(live_count, 1)
        for index in range(live_count):
            self.assertEqual(
                self._row_bytes(candidate_memory, index),
                self._row_bytes(reference_memory, index),
            )
            self.assertEqual(len(self._row_bytes(candidate_memory, index)), 308)

    def test_unknown_instruction_and_unknown_store_call_fail_closed(self) -> None:
        modified = self.source.replace(
            "\trts\n\t.bend  ; clearStatementRecord",
            "\tnop\n\trts\n\t.bend  ; clearStatementRecord",
            1,
        )
        routines = self._routines()
        routines["clearStatementRecord"] = _load_routine(
            modified, "clearStatementRecord", frozenset()
        )
        machine = NativeMachine(
            routines,
            self.constants,
            self.layout,
            self._memory(0),
            _registers(),
            self.clear_program,
        )
        machine.registers["d0"] = 0
        with self.assertRaisesRegex(AssertionError, "unsupported instruction nop"):
            machine.invoke("clearStatementRecord")

        store_machine = self._make_machine(self._memory())
        self._install_valid_request(store_machine)
        self._set_long(store_machine.memory, "OpasmEngineStmtCount", 0)
        with self.assertRaisesRegex(
            AssertionError, "unsupported call target storestatementrecord"
        ):
            store_machine.invoke("opasmEngineStoreStatementRecordV1")


if __name__ == "__main__":
    unittest.main()

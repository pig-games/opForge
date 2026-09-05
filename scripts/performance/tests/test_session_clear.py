#!/usr/bin/env python3
"""Bounded Level C proof for the native session-clear helper.

This executes the instructions selected from the real ``clearBytes`` assembly
block with a deliberately small interpreter.  It proves the memory, register,
stack, and CCR contract for the covered inputs on the modeled instructions.
It does not prove instruction encoding, timing, AmigaOS integration, or behavior
on a real 68020; those remain native/FS-UAE proof responsibilities.
"""

from __future__ import annotations

import ast
import dataclasses
import re
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
ENGINE_SOURCE = ROOT / "native/motorola68000/amigaos/opasm/opasm_engine.asm"
SESSION_BYTES = 41_221_928
U32_MASK = 0xFFFF_FFFF


@dataclasses.dataclass(frozen=True)
class Instruction:
    opcode: str
    operands: str
    line_number: int
    source: str


@dataclasses.dataclass
class Program:
    instructions: list[Instruction]
    labels: dict[str, int]

    @property
    def labels_by_pc(self) -> dict[int, str]:
        return {pc: label for label, pc in self.labels.items()}


def _extract_block(source: str, name: str) -> list[tuple[int, str]]:
    start_re = re.compile(rf"^\s*{re.escape(name)}\s+\.block\b")
    lines = source.splitlines()
    for index, line in enumerate(lines):
        if start_re.match(line):
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
    lines: list[tuple[int, str]], enabled_symbols: frozenset[str]
) -> list[tuple[int, str]]:
    """Apply only the two conditionals permitted in clearBytes."""

    known_symbols = {
        "OPFORGE_PROGRESS_PLATFORM_COUNTERS",
        "OPFORGE_SESSION_CLEAR_BYTE_REFERENCE",
    }
    active = True
    stack: list[tuple[bool, bool, bool]] = []
    selected: list[tuple[int, str]] = []

    for line_number, line in lines:
        stripped = line.strip()
        if match := re.fullmatch(r"\.ifdef\s+([A-Z0-9_]+)", stripped):
            symbol = match.group(1)
            if symbol not in known_symbols:
                raise AssertionError(f"unsupported conditional {symbol} on line {line_number}")
            condition = symbol in enabled_symbols
            stack.append((active, condition, False))
            active = active and condition
            continue
        if stripped == ".else":
            if not stack:
                raise AssertionError(f"unmatched .else on line {line_number}")
            parent_active, condition, seen_else = stack[-1]
            if seen_else:
                raise AssertionError(f"duplicate .else on line {line_number}")
            stack[-1] = (parent_active, condition, True)
            active = parent_active and not condition
            continue
        if stripped == ".endif":
            if not stack:
                raise AssertionError(f"unmatched .endif on line {line_number}")
            parent_active, _, _ = stack.pop()
            active = parent_active
            continue
        if active and stripped:
            selected.append((line_number, line.rstrip()))

    if stack:
        raise AssertionError("unterminated conditional in clearBytes")
    return selected


def _load_clear_program(source: str, byte_reference: bool) -> Program:
    enabled = (
        frozenset({"OPFORGE_SESSION_CLEAR_BYTE_REFERENCE"})
        if byte_reference
        else frozenset()
    )
    lines = _select_conditionals(_extract_block(source, "clearBytes"), enabled)
    instructions: list[Instruction] = []
    labels: dict[str, int] = {}

    known_opcodes = {
        "andi.l",
        "beq.s",
        "bne.s",
        "bra.s",
        "clr.b",
        "clr.l",
        "lsr.l",
        "move.l",
        "movem.l",
        "rts",
        "subq.l",
        "tst.l",
    }
    for line_number, line in lines:
        stripped = line.strip()
        parts = stripped.split(None, 1)
        first = parts[0].lower()
        if not line[0].isspace() and len(parts) == 1 and first not in known_opcodes:
            if first in labels:
                raise AssertionError(f"duplicate label {first} on line {line_number}")
            labels[first] = len(instructions)
            continue
        if first.startswith("."):
            raise AssertionError(f"unsupported directive on line {line_number}: {line}")
        operands = re.sub(r"\s+", "", parts[1]).lower() if len(parts) == 2 else ""
        instructions.append(Instruction(first, operands, line_number, line))

    return Program(instructions, labels)


class ClearBytesMachine:
    """Restricted interpreter for exactly the instructions used by clearBytes."""

    ACCELERATION_THRESHOLD = 1_024

    def __init__(
        self,
        program: Program,
        memory: bytearray,
        registers: dict[str, int],
        ccr: dict[str, bool],
    ) -> None:
        self.program = program
        self.memory = memory
        self.registers = registers.copy()
        self.ccr = ccr.copy()
        self.pc = 0
        self.stack: dict[int, int] = {}
        self.accelerated_blocks: list[tuple[str, int]] = []

    def _set_nzvc(self, value: int, width: int) -> None:
        mask = (1 << width) - 1
        value &= mask
        self.ccr.update(
            n=bool(value & (1 << (width - 1))), z=value == 0, v=False, c=False
        )

    def _subq_one(self, register: str) -> None:
        old = self.registers[register] & U32_MASK
        result = (old - 1) & U32_MASK
        borrow = old == 0
        overflow = old == 0x8000_0000
        self.registers[register] = result
        self.ccr.update(
            n=bool(result & 0x8000_0000),
            z=result == 0,
            v=overflow,
            c=borrow,
            x=borrow,
        )

    def _clear(self, width: int) -> None:
        start = self.registers["a1"]
        end = start + width
        if width == 4 and start % 4:
            raise AssertionError("longword write is not four-byte aligned")
        if start < 0 or end > len(self.memory):
            raise AssertionError(f"clear outside modeled memory: [{start}, {end})")
        self.memory[start:end] = bytes(width)
        self.registers["a1"] = end
        self.ccr.update(n=False, z=True, v=False, c=False)

    def _branch(self, label: str) -> None:
        try:
            self.pc = self.program.labels[label]
        except KeyError as error:
            raise AssertionError(f"branch to unknown label {label}") from error

    def _try_accelerated_clear_loop(self) -> bool:
        """Collapse a verified CLR/SUBQ/BNE self-loop for large counts only."""

        label = self.program.labels_by_pc.get(self.pc)
        if label is None or self.pc + 2 >= len(self.program.instructions):
            return False
        clear, subtract, branch = self.program.instructions[self.pc : self.pc + 3]
        if (
            clear.opcode not in {"clr.b", "clr.l"}
            or clear.operands != "(a1)+"
            or subtract.opcode != "subq.l"
            or branch.opcode != "bne.s"
            or branch.operands != label
        ):
            return False
        match = re.fullmatch(r"#1,(d[01])", subtract.operands)
        if match is None:
            return False
        counter = match.group(1)
        count = self.registers[counter] & U32_MASK
        if count <= self.ACCELERATION_THRESHOLD:
            return False

        width = 1 if clear.opcode == "clr.b" else 4
        start = self.registers["a1"]
        end = start + width * count
        if width == 4 and start % 4:
            raise AssertionError("accelerated longword writes are not four-byte aligned")
        if end > len(self.memory):
            raise AssertionError(f"accelerated clear outside modeled memory: [{start}, {end})")
        self.memory[start:end] = bytes(end - start)
        self.registers["a1"] = end
        # The last modeled loop iteration is CLR; SUBQ 1 from 1 to 0; BNE not taken.
        self.ccr.update(n=False, z=True, v=False, c=False, x=False)
        self.registers[counter] = 0
        self.accelerated_blocks.append((label, count))
        self.pc += 3
        return True

    def run(self) -> None:
        for _ in range(1_000_000):
            if self._try_accelerated_clear_loop():
                continue
            if self.pc >= len(self.program.instructions):
                raise AssertionError("clearBytes fell off its instruction stream")

            instruction = self.program.instructions[self.pc]
            self.pc += 1
            opcode = instruction.opcode
            operands = instruction.operands

            if opcode == "tst.l" and operands == "d0":
                self._set_nzvc(self.registers["d0"], 32)
            elif opcode == "beq.s":
                if self.ccr["z"]:
                    self._branch(operands)
            elif opcode == "bne.s":
                if not self.ccr["z"]:
                    self._branch(operands)
            elif opcode == "bra.s":
                self._branch(operands)
            elif opcode == "move.l" and operands == "a1,d1":
                self.registers["d1"] = self.registers["a1"] & U32_MASK
                self._set_nzvc(self.registers["d1"], 32)
            elif opcode == "move.l" and operands == "d0,d1":
                self.registers["d1"] = self.registers["d0"] & U32_MASK
                self._set_nzvc(self.registers["d1"], 32)
            elif opcode == "andi.l" and operands in {"#3,d0", "#3,d1"}:
                register = operands[-2:]
                self.registers[register] &= 3
                self._set_nzvc(self.registers[register], 32)
            elif opcode == "lsr.l" and operands == "#2,d1":
                old = self.registers["d1"] & U32_MASK
                result = old >> 2
                carry = bool(old & 0b10)
                self.registers["d1"] = result
                self.ccr.update(n=False, z=result == 0, v=False, c=carry, x=carry)
            elif opcode in {"clr.b", "clr.l"} and operands == "(a1)+":
                self._clear(1 if opcode == "clr.b" else 4)
            elif opcode == "subq.l":
                match = re.fullmatch(r"#1,(d[01])", operands)
                if match is None:
                    self._unsupported(instruction)
                self._subq_one(match.group(1))
            elif opcode == "movem.l" and operands == "d1,-(sp)":
                self.registers["a7"] = (self.registers["a7"] - 4) & U32_MASK
                address = self.registers["a7"]
                if address in self.stack:
                    raise AssertionError("modeled stack slot already occupied")
                self.stack[address] = self.registers["d1"] & U32_MASK
            elif opcode == "movem.l" and operands == "(sp)+,d1":
                address = self.registers["a7"]
                if address not in self.stack:
                    raise AssertionError("modeled stack underflow")
                self.registers["d1"] = self.stack.pop(address)
                self.registers["a7"] = (address + 4) & U32_MASK
            elif opcode == "rts" and not operands:
                return
            else:
                self._unsupported(instruction)

        raise AssertionError("clearBytes exceeded the bounded instruction budget")

    @staticmethod
    def _unsupported(instruction: Instruction) -> None:
        raise AssertionError(
            f"unsupported instruction on line {instruction.line_number}: "
            f"{instruction.source}"
        )


def _constant_expressions(source: str) -> dict[str, str]:
    expressions: dict[str, str] = {}
    for line in source.splitlines():
        code = line.split(";", 1)[0]
        match = re.match(r"^\s*([A-Z][A-Z0-9_]*)\s*=\s*(.*?)\s*$", code)
        if match:
            expressions[match.group(1)] = match.group(2)
    return expressions


def _evaluate_constant(name: str, expressions: dict[str, str]) -> int:
    active: set[str] = set()

    def evaluate_name(current: str) -> int:
        if current in active:
            raise AssertionError(f"cyclic constant definition involving {current}")
        try:
            expression = expressions[current]
        except KeyError as error:
            raise AssertionError(f"unknown constant {current}") from error
        active.add(current)
        try:
            return evaluate_node(ast.parse(expression, mode="eval").body)
        finally:
            active.remove(current)

    def evaluate_node(node: ast.AST) -> int:
        if isinstance(node, ast.Constant) and isinstance(node.value, int):
            return node.value
        if isinstance(node, ast.Name):
            return evaluate_name(node.id)
        if isinstance(node, ast.BinOp) and isinstance(node.op, (ast.Add, ast.Sub, ast.Mult)):
            left = evaluate_node(node.left)
            right = evaluate_node(node.right)
            if isinstance(node.op, ast.Add):
                return left + right
            if isinstance(node.op, ast.Sub):
                return left - right
            return left * right
        raise AssertionError(f"unsupported constant expression: {ast.dump(node)}")

    return evaluate_name(name)


class SessionClearLevelCTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = ENGINE_SOURCE.read_text(encoding="utf-8")
        cls.programs = {
            "candidate": _load_clear_program(cls.source, byte_reference=False),
            "byte_reference": _load_clear_program(cls.source, byte_reference=True),
        }

    def _run_case(self, program_name: str, length: int, alignment: int, x: bool) -> None:
        guard = 16
        start = guard + alignment
        memory = bytearray([0xA5]) * (start + length + guard)
        expected = bytearray(memory)
        expected[start : start + length] = bytes(length)

        registers = {
            **{
                f"d{number}": (0x1020_3040 + number * 0x0101_0101) & U32_MASK
                for number in range(8)
            },
            **{
                f"a{number}": (0x5060_7080 + number * 0x0101_0101) & U32_MASK
                for number in range(8)
            },
        }
        registers["d0"] = length
        registers["a1"] = start
        registers["a7"] = 0x7000_1000
        initial_registers = registers.copy()
        initial_ccr = {"n": True, "z": False, "v": True, "c": True, "x": x}

        machine = ClearBytesMachine(
            self.programs[program_name], memory, registers, initial_ccr
        )
        machine.run()

        self.assertEqual(memory, expected)
        self.assertEqual(machine.registers["d0"], 0)
        self.assertEqual(machine.registers["a1"], start + length)
        preserved = [
            "d1", "d2", "d3", "d4", "d5", "d6", "d7",
            "a0", "a2", "a3", "a4", "a5", "a6",
        ]
        for register in preserved:
            self.assertEqual(machine.registers[register], initial_registers[register])
        self.assertEqual(machine.registers["a7"], initial_registers["a7"])
        self.assertEqual(machine.stack, {})
        self.assertEqual(
            machine.ccr,
            {"n": False, "z": True, "v": False, "c": False, "x": x if length == 0 else False},
        )
        if length > ClearBytesMachine.ACCELERATION_THRESHOLD:
            self.assertTrue(machine.accelerated_blocks)

    def test_source_contract_has_one_exact_session_clear(self) -> None:
        expressions = _constant_expressions(self.source)
        self.assertEqual(
            _evaluate_constant("OPASM_ENGINE_ASSEMBLY_SESSION_BYTES", expressions),
            SESSION_BYTES,
        )
        calls = re.findall(r"(?m)^\s*bsr\.w\s+clearBytes\s*$", self.source)
        self.assertEqual(len(calls), 1)

        init_lines = _select_conditionals(_extract_block(self.source, "initSessionV1"), frozenset())
        normalized = [re.sub(r"\s+", "", line).lower() for _, line in init_lines]
        call_index = normalized.index("bsr.wclearbytes")
        self.assertEqual(normalized[call_index - 2], "leaopasmengineassemblysessionstart.l,a1")
        self.assertEqual(
            normalized[call_index - 1],
            "move.l#opasm_engine_assembly_session_bytes,d0",
        )

    def test_zero_length_preserves_each_x_state_in_both_branches(self) -> None:
        for program_name in self.programs:
            for x in (False, True):
                with self.subTest(program=program_name, x=x):
                    self._run_case(program_name, length=0, alignment=0, x=x)

    def test_alignments_small_lengths_and_tails_in_both_branches(self) -> None:
        for program_name in self.programs:
            for alignment in range(4):
                for length in range(1, 13):
                    with self.subTest(
                        program=program_name, alignment=alignment, length=length
                    ):
                        self._run_case(program_name, length, alignment, x=True)

    def test_lengths_beyond_word_range_in_both_branches(self) -> None:
        for program_name in self.programs:
            with self.subTest(program=program_name):
                self._run_case(program_name, length=65_539, alignment=3, x=True)

    def test_exact_session_range_in_both_branches(self) -> None:
        for program_name in self.programs:
            with self.subTest(program=program_name):
                self._run_case(program_name, SESSION_BYTES, alignment=1, x=True)

    def test_unknown_instruction_fails_closed(self) -> None:
        modified_source = self.source.replace(
            "\trts\n\t.bend  ; clearBytes",
            "\tnop\n\trts\n\t.bend  ; clearBytes",
            1,
        )
        program = _load_clear_program(modified_source, byte_reference=False)
        registers = {**{f"d{i}": 0 for i in range(8)}, **{f"a{i}": 0 for i in range(8)}}
        machine = ClearBytesMachine(
            program,
            bytearray(1),
            registers,
            {"n": False, "z": False, "v": False, "c": False, "x": False},
        )
        with self.assertRaisesRegex(AssertionError, "unsupported instruction"):
            machine.run()


if __name__ == "__main__":
    unittest.main()

#!/usr/bin/env python3
"""Bounded Level C proof for native module-candidate buffered reads.

The restricted interpreter executes ``readCandidateByte`` from the checked-in
assembly source. It fails closed on instructions, directives, address forms,
conditionals, and calls outside the explicit model below.

This test proves the modeled request shape, buffer state, byte result, and status
contract for the covered host-side inputs. The consumer after the helper call is
only source-audited; parser and encoding semantics are outside this model. This
does not prove instruction encoding, timing, real 68000 execution, or AmigaOS DOS
integration. Those remain Level D/FS-UAE responsibilities.
"""

from __future__ import annotations

import dataclasses
import re
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
SOURCE_PATH = ROOT / "native/motorola68000/amigaos/opforge-cli/module_discovery.asm"
BUFFER_SIZE = 8192
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


def _extract_block(source: str, name: str) -> list[tuple[int, str]]:
    lines = source.splitlines()
    start = re.compile(rf"^\s*{re.escape(name)}\s+\.block\b")
    for index, line in enumerate(lines):
        if start.match(line):
            break
    else:
        raise AssertionError(f"missing {name} .block")

    result: list[tuple[int, str]] = []
    for line_number, line in enumerate(lines[index + 1 :], index + 2):
        code = line.split(";", 1)[0].rstrip()
        if re.match(r"^\s*\.bend\b", code):
            return result
        if code.strip():
            result.append((line_number, code))
    raise AssertionError(f"unterminated {name} .block")


def _without_profile_counter(lines: list[tuple[int, str]]) -> list[tuple[int, str]]:
    """Select the production path with optional profile counters disabled."""
    active = True
    stack: list[bool] = []
    result: list[tuple[int, str]] = []
    for line_number, line in lines:
        stripped = line.strip()
        if match := re.fullmatch(r"\.ifdef\s+([A-Z0-9_]+)", stripped):
            if match.group(1) != "OPFORGE_PROGRESS_PLATFORM_COUNTERS":
                raise AssertionError(
                    f"unsupported conditional {match.group(1)} on line {line_number}"
                )
            stack.append(active)
            active = False
        elif stripped in {".else", ".endif"}:
            if stripped == ".else":
                raise AssertionError(f"unsupported .else on line {line_number}")
            if not stack:
                raise AssertionError(f"unmatched .endif on line {line_number}")
            active = stack.pop()
        elif active:
            result.append((line_number, line))
    if stack:
        raise AssertionError("unterminated conditional in readCandidateByte")
    return result


def _load_program(source: str) -> Program:
    known = {
        "addq.w", "bcs.s", "ble.s", "clr.w", "cmp.w", "jsr", "lea",
        "move.b", "move.l", "move.w", "moveq", "rts", "tst.l",
    }
    instructions: list[Instruction] = []
    labels: dict[str, int] = {}
    for line_number, line in _without_profile_counter(
        _extract_block(source, "readCandidateByte")
    ):
        stripped = line.strip()
        parts = stripped.split(None, 1)
        first = parts[0].lower()
        if not line[0].isspace() and len(parts) == 1 and first not in known:
            if first in labels:
                raise AssertionError(f"duplicate label {first} on line {line_number}")
            labels[first] = len(instructions)
            continue
        if first.startswith("."):
            raise AssertionError(f"unsupported directive on line {line_number}: {line}")
        operands = re.sub(r"\s+", "", parts[1]).lower() if len(parts) == 2 else ""
        instructions.append(Instruction(first, operands, line_number, line))
    return Program(instructions, labels)


class MockDos:
    def __init__(self, files: dict[int, bytes], short_reads: list[int] | None = None):
        self.files = files
        self.offsets = {handle: 0 for handle in files}
        self.short_reads = list(short_reads or [])
        self.requests: list[tuple[int, int]] = []
        self.failure: int | None = None

    def fail_next(self, status: int = -1) -> None:
        self.failure = status

    def read_input(self, handle: int, maximum: int) -> tuple[int, bytes]:
        self.requests.append((handle, maximum))
        if self.failure is not None:
            status, self.failure = self.failure, None
            return status, b""
        if handle not in self.files:
            raise AssertionError(f"dos.readInput received unknown handle {handle}")
        start = self.offsets[handle]
        available = len(self.files[handle]) - start
        limit = self.short_reads.pop(0) if self.short_reads else maximum
        count = min(maximum, limit, available)
        data = self.files[handle][start : start + count]
        self.offsets[handle] += count
        return count, data


class ReadCandidateMachine:
    """Restricted interpreter for the actual readCandidateByte helper."""

    def __init__(self, program: Program, dos: MockDos, poison: int = 0xA5):
        self.program = program
        self.dos = dos
        self.d = {f"d{i}": 0 for i in range(4)}
        self.cursor = 0
        self.count = 0
        self.char = poison
        self.buffer = bytearray([poison] * BUFFER_SIZE)
        self.pc = 0
        self.ccr = {"n": False, "z": False, "c": False, "v": False}
        self.a0: str | None = None

    def reset_candidate(self, handle: int) -> None:
        self.cursor = 0
        self.count = 0
        self.d["d1"] = handle & U32_MASK

    def _nz(self, value: int, bits: int) -> None:
        value &= (1 << bits) - 1
        self.ccr.update(n=bool(value & (1 << (bits - 1))), z=value == 0, c=False, v=False)

    def _branch(self, label: str) -> None:
        try:
            self.pc = self.program.labels[label]
        except KeyError as error:
            raise AssertionError(f"branch to unknown label {label}") from error

    def call(self) -> int:
        self.pc = 0
        for _ in range(40):
            if self.pc >= len(self.program.instructions):
                raise AssertionError("readCandidateByte fell off its instruction stream")
            ins = self.program.instructions[self.pc]
            self.pc += 1
            op, args = ins.opcode, ins.operands
            if op == "moveq" and args == "#0,d2":
                self.d["d2"] = 0
                self._nz(0, 32)
            elif op == "moveq" and args in {"#1,d0"}:
                self.d["d0"] = 1
                self._nz(1, 32)
            elif op == "move.w" and args == "modulescanreadcursor.l,d2":
                self.d["d2"] = (self.d["d2"] & 0xFFFF_0000) | self.cursor
                self._nz(self.cursor, 16)
            elif op == "move.w" and args == "d0,modulescanreadcount.l":
                self.count = self.d["d0"] & 0xFFFF
                self._nz(self.count, 16)
            elif op == "cmp.w" and args == "modulescanreadcount.l,d2":
                left, right = self.d["d2"] & 0xFFFF, self.count
                result = (left - right) & 0xFFFF
                self.ccr.update(n=bool(result & 0x8000), z=result == 0, c=left < right, v=False)
            elif op == "bcs.s":
                if self.ccr["c"]:
                    self._branch(args)
            elif op == "lea" and args == "modulescanreadbuffer.l,a0":
                self.a0 = "ModuleScanReadBuffer"
            elif op == "move.l" and args == "#8192,d0":
                self.d["d0"] = BUFFER_SIZE
                self._nz(BUFFER_SIZE, 32)
            elif op == "jsr" and args == "dos.readinput":
                if self.a0 != "ModuleScanReadBuffer":
                    raise AssertionError("dos.readInput destination is not the module buffer")
                status, data = self.dos.read_input(self.d["d1"], self.d["d0"])
                if status > 0 and (status > self.d["d0"] or status != len(data)):
                    raise AssertionError("mock dos.readInput violated its modeled contract")
                if status > 0:
                    self.buffer[:status] = data
                self.d["d0"] = status & U32_MASK
                self._nz(self.d["d0"], 32)
            elif op == "tst.l" and args == "d0":
                self._nz(self.d["d0"], 32)
            elif op == "ble.s":
                if self.ccr["z"] or self.ccr["n"] != self.ccr["v"]:
                    self._branch(args)
            elif op == "clr.w" and args == "modulescanreadcursor.l":
                self.cursor = 0
                self._nz(0, 16)
            elif op == "move.b" and args == "0(a0,d2.w),modulescanchar.l":
                if self.a0 != "ModuleScanReadBuffer":
                    raise AssertionError("buffered byte read uses unexpected base")
                self.char = self.buffer[self.d["d2"] & 0xFFFF]
                self._nz(self.char, 8)
            elif op == "addq.w" and args == "#1,modulescanreadcursor.l":
                self.cursor = (self.cursor + 1) & 0xFFFF
                self._nz(self.cursor, 16)
            elif op == "rts" and not args:
                value = self.d["d0"]
                return value - (1 << 32) if value & 0x8000_0000 else value
            else:
                raise AssertionError(
                    f"unsupported instruction on line {ins.line_number}: {ins.source}"
                )
        raise AssertionError("readCandidateByte exceeded bounded instruction budget")

    def drain(self) -> tuple[bytes, int]:
        output = bytearray()
        while True:
            status = self.call()
            if status != 1:
                return bytes(output), status
            output.append(self.char)


class ModuleBufferProof(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = SOURCE_PATH.read_text(encoding="utf-8")
        cls.program = _load_program(cls.source)

    def _run(self, data: bytes, short_reads: list[int] | None = None) -> tuple[ReadCandidateMachine, bytes, int]:
        dos = MockDos({17: data}, short_reads)
        machine = ReadCandidateMachine(self.program, dos)
        machine.reset_candidate(17)
        output, status = machine.drain()
        self.assertEqual(output, data)
        return machine, output, status

    def test_empty_eof_and_minus_one_error(self) -> None:
        machine, _, status = self._run(b"")
        self.assertEqual(status, 0)
        self.assertEqual(machine.dos.requests, [(17, BUFFER_SIZE)])

        dos = MockDos({17: b"ignored"})
        dos.fail_next(-1)
        machine = ReadCandidateMachine(self.program, dos)
        machine.reset_candidate(17)
        self.assertEqual(machine.call(), -1)
        self.assertEqual(machine.cursor, 0)
        self.assertEqual(machine.count, 0)

    def test_boundaries_multiples_and_long_stream(self) -> None:
        sizes = [BUFFER_SIZE - 1, BUFFER_SIZE, BUFFER_SIZE + 1, 2 * BUFFER_SIZE, 100_003]
        for size in sizes:
            with self.subTest(size=size):
                data = bytes((index * 37 + 13) & 0xFF for index in range(size))
                machine, _, status = self._run(data)
                self.assertEqual(status, 0)
                expected_reads = (size + BUFFER_SIZE - 1) // BUFFER_SIZE + 1
                self.assertEqual(len(machine.dos.requests), expected_reads)
                self.assertTrue(all(request == BUFFER_SIZE for _, request in machine.dos.requests))

    def test_short_positive_reads_are_consumed_before_refill(self) -> None:
        data = b"short-read-contract"
        machine, output, status = self._run(data, [3, 1, 7, 2, 99])
        self.assertEqual(status, 0)
        self.assertEqual(output, data)
        self.assertEqual(len(machine.dos.requests), 6)

    def test_bytes_are_logical_and_crlf_is_preserved(self) -> None:
        data = b"a\r\nb\rc\n" + bytes(range(256)) + b"\x00\xff"
        _, output, status = self._run(data, [2, 4, 9, 17, 99, 999])
        self.assertEqual(status, 0)
        self.assertEqual(output, data)

    def test_poisoned_buffer_and_reset_between_two_files(self) -> None:
        first, second = b"first-file-stale-tail", b"B\x00\r\nsecond"
        dos = MockDos({3: first, 4: second}, [5, 99, 2, 99])
        machine = ReadCandidateMachine(self.program, dos, poison=0xCC)
        machine.reset_candidate(3)
        self.assertEqual(machine.drain(), (first, 0))
        machine.reset_candidate(4)
        self.assertEqual(machine.drain(), (second, 0))
        self.assertEqual([handle for handle, _ in dos.requests], [3, 3, 3, 4, 4])

    def test_caller_resets_state_and_retains_reference_switch(self) -> None:
        caller = _extract_block(self.source, "scanCandidateFile")
        normalized = [re.sub(r"\s+", "", line).lower() for _, line in caller]
        self.assertIn("clr.wmodulescanreadcursor", normalized)
        self.assertIn("clr.wmodulescanreadcount", normalized)
        source = "\n".join(line for _, line in caller)
        self.assertRegex(source, r"\.ifdef\s+OPFORGE_MODULE_SCAN_BYTE_READ_REFERENCE")
        self.assertRegex(source, r"(?m)^\s*bsr\.w\s+readCandidateByte\s*$")

    def test_unknown_instruction_and_call_fail_closed(self) -> None:
        for injected in ("\tnop", "\tjsr mystery.read"):
            with self.subTest(injected=injected):
                modified = self.source.replace(
                    "\trts\n\t.bend  ; readCandidateByte",
                    f"{injected}\n\trts\n\t.bend  ; readCandidateByte",
                    1,
                )
                machine = ReadCandidateMachine(_load_program(modified), MockDos({1: b""}))
                machine.reset_candidate(1)
                with self.assertRaisesRegex(AssertionError, "unsupported instruction"):
                    machine.call()


if __name__ == "__main__":
    unittest.main()

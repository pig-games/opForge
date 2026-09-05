#!/usr/bin/env python3
"""Bounded Level C proof for the native source-stream buffered byte helper.

This restricted interpreter executes the checked-in helper and fails closed on
unsupported instructions or calls. It proves modeled refill, status, cursor,
and recursive stream-slot behavior; it does not prove encoding, timing, DOS
integration, line parsing, or real 68000 execution.
Stream enter/leave operations are modeled test setup, not executed production code.
"""

from __future__ import annotations

import re
import unittest

try:
    from .test_module_buffer import MockDos, Program, Instruction, _extract_block, _without_profile_counter, ROOT
except ImportError:
    from test_module_buffer import MockDos, Program, Instruction, _extract_block, _without_profile_counter, ROOT


SOURCE = ROOT / "native/motorola68000/amigaos/opforge-cli/source_reader.asm"
CONSTANTS = ROOT / "native/motorola68000/amigaos/opforge-cli/constants.asm"
MASK = 0xFFFF_FFFF


def source_limits(source: str) -> tuple[int, int]:
    constants = CONSTANTS.read_text()
    values = {
        name: int(match.group(1))
        for name in ("NATIVE_MODULE_RESOLVE_DEPTH_LIMIT", "NATIVE_INCLUDE_DEPTH_LIMIT")
        if (match := re.search(rf"(?m)^{name}\s*=\s*(\d+)\s*$", constants))
    }
    if len(values) != 2:
        raise AssertionError("missing source recursion limit constant")
    capacity = re.search(r"(?m)^SOURCE_READ_BUFFER_CAPACITY\s*=\s*(\d+)\s*$", source)
    stream_expr = re.search(r"(?m)^SOURCE_READ_STREAM_CAPACITY\s*=\s*(.+)$", source)
    expected_expr = "constants.NATIVE_MODULE_RESOLVE_DEPTH_LIMIT + constants.NATIVE_INCLUDE_DEPTH_LIMIT + 1"
    if capacity is None or stream_expr is None or stream_expr.group(1).strip() != expected_expr:
        raise AssertionError("source buffer capacity or stream bound definition drifted")
    reservations = (
        r"SourceReadCursor\s+\.res word, SOURCE_READ_STREAM_CAPACITY",
        r"SourceReadLength\s+\.res word, SOURCE_READ_STREAM_CAPACITY",
        r"SourceReadBuffer\s+\.res byte, SOURCE_READ_STREAM_CAPACITY \* SOURCE_READ_BUFFER_CAPACITY",
    )
    if not all(re.search(pattern, source) for pattern in reservations):
        raise AssertionError("source buffer storage reservation drifted")
    return int(capacity.group(1)), values["NATIVE_MODULE_RESOLVE_DEPTH_LIMIT"] + values["NATIVE_INCLUDE_DEPTH_LIMIT"] + 1


def load_program(source: str) -> Program:
    known = {"add.l", "addq.w", "adda.l", "blo.s", "ble.s", "clr.w", "cmp.w",
             "jsr", "lea", "lsl.l", "lsr.l", "move.b", "move.l", "move.w",
             "moveq", "rts", "subq.l", "tst.l"}
    instructions, labels = [], {}
    lines = _without_profile_counter(_extract_block(source, "opforgeNativeCliReadSourceByte"))
    for number, line in lines:
        parts = line.strip().split(None, 1)
        op = parts[0].lower()
        if not line[0].isspace() and len(parts) == 1 and op not in known:
            labels[op] = len(instructions)
            continue
        if op.startswith("."):
            raise AssertionError(f"unsupported directive on line {number}: {line}")
        args = re.sub(r"\s+", "", parts[1]).lower() if len(parts) == 2 else ""
        instructions.append(Instruction(op, args, number, line))
    return Program(instructions, labels)


class SourceMachine:
    def __init__(self, program: Program, dos: MockDos, capacity: int, streams: int):
        self.program, self.dos = program, dos
        self.capacity, self.streams = capacity, streams
        self.d = {f"d{i}": 0 for i in range(6)}
        self.depth, self.cursor, self.length = 0, [0] * streams, [0] * streams
        self.buffers = [bytearray(capacity) for _ in range(streams)]
        self.a0 = self.a1 = ("", 0)
        self.stack, self.char, self.pc = [], 0, 0
        self.n = self.z = self.c = False

    def enter(self, handle: int) -> None:
        if self.depth >= self.streams:
            raise AssertionError("modeled source stream overflow")
        self.cursor[self.depth] = self.length[self.depth] = 0
        self.depth += 1
        self.d["d5"] = handle

    def leave(self) -> None:
        self.depth -= 1

    def _nz(self, value: int, bits: int = 32) -> None:
        value &= (1 << bits) - 1
        self.n, self.z, self.c = bool(value & (1 << (bits - 1))), value == 0, False

    def _slot(self, address: tuple[str, int], expected: str, extra: int = 0) -> int:
        if address[0] != expected or address[1] % 2:
            raise AssertionError(f"invalid {expected} address {address}")
        slot = address[1] // 2 + extra
        if not 0 <= slot < self.streams:
            raise AssertionError(f"out-of-range {expected} slot {slot}")
        return slot

    def _buffer_slot(self, address: tuple[str, int]) -> int:
        if address[0] != "sourcereadbuffer" or address[1] % self.capacity:
            raise AssertionError(f"invalid source buffer address {address}")
        slot = address[1] // self.capacity
        if not 0 <= slot < self.streams:
            raise AssertionError(f"out-of-range source buffer slot {slot}")
        return slot

    def call(self) -> int:
        self.pc = 0
        for _ in range(64):
            ins = self.program.instructions[self.pc]
            self.pc += 1
            op, a = ins.opcode, ins.operands
            if op == "moveq":
                value, reg = a.split(",")
                self.d[reg] = int(value[1:]) & MASK; self._nz(self.d[reg])
            elif op == "move.w" and a == "sourcereadstreamdepth,d3":
                self.d["d3"] = self.depth; self._nz(self.depth, 16)
            elif op == "subq.l" and a == "#1,d3":
                self.d["d3"] = (self.d["d3"] - 1) & MASK; self._nz(self.d["d3"])
            elif op == "add.l":
                src, dst = a.split(","); self.d[dst] = (self.d[dst] + self.d[src]) & MASK; self._nz(self.d[dst])
            elif op in {"lsr.l", "lsl.l"}:
                count, reg = a.split(","); count = int(count[1:])
                self.d[reg] = (self.d[reg] >> count if op == "lsr.l" else self.d[reg] << count) & MASK; self._nz(self.d[reg])
            elif op == "lea":
                name, reg = a.split(","); setattr(self, reg, (name, 0))
            elif op == "adda.l":
                reg, addr = a.split(","); base, off = getattr(self, addr); setattr(self, addr, (base, off + self.d[reg]))
            elif op == "move.l" and a == "d3,d2": self.d["d2"] = self.d["d3"]
            elif op == "move.l" and a == "#source_read_buffer_capacity,d0": self.d["d0"] = self.capacity
            elif op == "move.l" and a == "d5,d1": self.d["d1"] = self.d["d5"]
            elif op == "move.l" and a == "d3,-(sp)": self.stack.append(self.d["d3"])
            elif op == "move.l" and a == "(sp)+,d3": self.d["d3"] = self.stack.pop()
            elif op == "move.w" and a == "0(a0,d3.l),d0": self.d["d0"] = self.cursor[self._slot(self.a0, "sourcereadcursor", self.d["d3"] // 2)]
            elif op == "cmp.w" and a == "0(a1,d3.l),d0":
                left = self.d["d0"] & 0xFFFF
                right = self.length[self._slot(self.a1, "sourcereadlength", self.d["d3"] // 2)]
                self.c, self.z, self.n = left < right, left == right, bool((left - right) & 0x8000)
            elif op == "blo.s":
                if self.c: self.pc = self.program.labels[a]
            elif op == "jsr" and a == "dos.readinput":
                slot = self._buffer_slot(self.a0)
                requested = self.d["d0"]
                self.d["d3"] = requested
                status, data = self.dos.read_input(self.d["d1"], self.d["d0"])
                if status > 0: self.buffers[slot][:status] = data
                self.d["d0"] = status & MASK; self._nz(self.d["d0"])
            elif op == "tst.l" and a == "d0": self._nz(self.d["d0"])
            elif op == "ble.s":
                if self.z or self.n: self.pc = self.program.labels[a]
            elif op == "move.w" and a == "d0,0(a1,d3.l)": self.length[self._slot(self.a1, "sourcereadlength", self.d["d3"] // 2)] = self.d["d0"] & 0xFFFF
            elif op == "clr.w" and a == "0(a0,d3.l)": self.cursor[self._slot(self.a0, "sourcereadcursor", self.d["d3"] // 2)] = 0; self._nz(0, 16)
            elif op == "move.w" and a == "0(a0,d3.l),d2": self.d["d2"] = self.cursor[self._slot(self.a0, "sourcereadcursor", self.d["d3"] // 2)]
            elif op == "move.b" and a == "0(a1,d2.l),state.nativecliinputchar":
                slot = self._buffer_slot(self.a1)
                if not 0 <= self.d["d2"] < self.length[slot]: raise AssertionError("source buffer byte outside refill")
                self.char = self.buffers[slot][self.d["d2"]]
            elif op == "addq.w" and a == "#1,0(a0,d3.l)": self.cursor[self._slot(self.a0, "sourcereadcursor", self.d["d3"] // 2)] += 1
            elif op == "rts":
                value = self.d["d0"]; return value - (1 << 32) if value & 0x8000_0000 else value
            else: raise AssertionError(f"unsupported instruction on line {ins.line_number}: {ins.source}")
        raise AssertionError("source byte helper exceeded instruction budget")


class SourceBufferProof(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = SOURCE.read_text(); cls.capacity, cls.streams = source_limits(cls.source)
        cls.program = load_program(cls.source)

    def machine(self, dos: MockDos, program: Program | None = None) -> SourceMachine:
        return SourceMachine(program or self.program, dos, self.capacity, self.streams)

    def test_short_refills_then_eof(self) -> None:
        data = b"ab\r\ncd"; m = self.machine(MockDos({7: data}, [2, 1, 3]))
        m.enter(7); out = bytearray()
        while (status := m.call()) == 1: out.append(m.char)
        self.assertEqual((bytes(out), status, m.cursor[0], m.length[0]), (data, 0, 3, 3))
        self.assertEqual(m.dos.requests, [(7, self.capacity)] * 4)

    def test_minus_one_preserves_empty_slot(self) -> None:
        dos = MockDos({7: b"x"}); dos.fail_next(-1); m = self.machine(dos); m.enter(7)
        self.assertEqual((m.call(), m.cursor[0], m.length[0]), (-1, 0, 0))

    def test_recursive_child_does_not_discard_parent_buffer(self) -> None:
        dos = MockDos({1: b"parent", 2: b"kid"}); m = self.machine(dos); m.enter(1)
        self.assertEqual(m.call(), 1); parent_cursor, parent_length = m.cursor[0], m.length[0]
        m.enter(2); self.assertEqual([m.call() and m.char for _ in range(3)], list(b"kid")); self.assertEqual(m.call(), 0)
        m.leave(); m.d["d5"] = 1; tail = bytearray()
        while (status := m.call()) == 1: tail.append(m.char)
        self.assertEqual((parent_cursor, parent_length, bytes(tail), status), (1, 6, b"arent", 0))
        self.assertEqual([handle for handle, _ in dos.requests], [1, 2, 2, 1])

    def test_unknown_instruction_fails_closed(self) -> None:
        changed = self.source.replace("\trts\n\t.bend  ; opforgeNativeCliReadSourceByte", "\tnop\n\trts\n\t.bend  ; opforgeNativeCliReadSourceByte", 1)
        machine = self.machine(MockDos({1: b""}), load_program(changed)); machine.enter(1)
        with self.assertRaisesRegex(AssertionError, "unsupported instruction"):
            machine.call()

    def test_wrong_base_and_missing_d3_restore_fail_closed(self) -> None:
        wrong_base = self.source.replace("\tlea SourceReadCursor, a0", "\tlea SourceReadLength, a0", 1)
        machine = self.machine(MockDos({1: b"x"}), load_program(wrong_base)); machine.enter(1)
        with self.assertRaisesRegex(AssertionError, "invalid sourcereadcursor address"):
            machine.call()
        no_restore = self.source.replace("\tmove.l (sp)+, d3\n", "", 1)
        machine = self.machine(MockDos({1: b"x"}), load_program(no_restore)); machine.enter(1)
        with self.assertRaisesRegex(AssertionError, "out-of-range sourcereadlength slot"):
            machine.call()


if __name__ == "__main__": unittest.main()

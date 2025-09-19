from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Any

from src.lang_types import Type, IntType, BoolType, StructType, FloatType, StringType


@dataclass
class Cell:
    typ: Type
    val: Any


class Heap:
    def __init__(self):
        self.store: Dict[int, Cell] = {}
        self.next_addr = 1
        self.blocks: dict[int, int] = {}  # base_addr -> count (for new T[n])

    def alloc(self, typ: Type, init: Any = None) -> int:
        addr = self.next_addr
        self.next_addr += 1
        if init is None:
            init = 0 if isinstance(typ, IntType) else False if isinstance(typ, BoolType) else 0
        if isinstance(typ, StructType):
            init = {fname: (0 if isinstance(ft, IntType) else False if isinstance(ft, BoolType)
            else 0.0 if isinstance(ft, FloatType) else "" if isinstance(ft, StringType)
            else 0)  # for pointers and other types, set to 0/NULL
                    for fname, ft in typ.fields}

        self.store[addr] = Cell(typ, init)
        return addr

    def alloc_block(self, typ: Type, count: int) -> int:
        if count < 1: raise ValueError("alloc_block count must be >= 1")
        base = self.next_addr
        for _ in range(count):
            self.alloc(typ)
        self.blocks[base] = count
        return base

    def free(self, addr: int):
        # Require base pointer, like in C (otherwise error)
        if addr in self.blocks:
            cnt = self.blocks.pop(addr)
            for a in range(addr, addr + cnt):
                if a in self.store:
                    del self.store[a]
            return
        # single cell
        if addr in self.store:
            del self.store[addr]
            return
        raise RuntimeError(f"invalid or double free at {addr}")

    def load(self, addr: int) -> Any:
        return self.store[addr].val

    def store_val(self, addr: int, val: Any):
        self.store[addr].val = val

    def typeof(self, addr: int) -> Type:
        return self.store[addr].typ
from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Optional, Any, List, Tuple

from src.heap import Heap
from src.lang_types import (
    Type, IntType, BoolType, FloatType, StringType, PtrType
)


@dataclass
class VarEntry:
    typ: Type
    value: Any  # current value (if not boxed)
    boxed_addr: Optional[int]  # address of heap cell if variable is "boxed" (&)


def _default_for_type(t: Type) -> Any:
    if isinstance(t, IntType):   return 0
    if isinstance(t, BoolType):  return False
    if isinstance(t, FloatType): return 0.0
    if isinstance(t, StringType): return ""
    if isinstance(t, PtrType):   return 0  # null
    # for other types (StructType etc.) we won't get here: "bare" types are forbidden
    return 0


class Memory:
    def __init__(self):
        self.scopes: List[Dict[str, VarEntry]] = [{}]
        self.heap = Heap()

    # ---- scopes ----
    def push(self):
        self.scopes.append({})

    def pop(self):
        self.scopes.pop()

    # ---- variables (stack) ----
    def declare(self, name: str, typ: Type, init: Any = None):
        frame = self.scopes[-1]
        if name in frame:
            raise RuntimeError(f"variable redefinition: {name}")
        if init is None:
            init = _default_for_type(typ)
        frame[name] = VarEntry(typ=typ, value=init, boxed_addr=None)

    def _lookup(self, name: str) -> VarEntry:
        for frame in reversed(self.scopes):
            if name in frame: return frame[name]
        raise NameError(f"undefined variable {name}")

    def typeof_var(self, name: str) -> Type:
        return self._lookup(name).typ

    def get(self, name: str) -> Tuple[Type, Any]:
        e = self._lookup(name)
        if e.boxed_addr is not None:
            return e.typ, self.heap.load(e.boxed_addr)
        return e.typ, e.value

    def set(self, name: str, value: Any):
        e = self._lookup(name)
        if e.boxed_addr is not None:
            self.heap.store_val(e.boxed_addr, value)
        else:
            e.value = value

    # &name → heap address; on first access we "box" the variable
    def addr_of(self, name: str) -> Tuple[PtrType, int]:
        e = self._lookup(name)
        if e.boxed_addr is None:
            e.boxed_addr = self.heap.alloc(e.typ, e.value)
        return PtrType(e.typ), e.boxed_addr
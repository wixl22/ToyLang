from __future__ import annotations

from typing import Dict, Optional, Any, List

from src.heap import Heap
from src.lang_types import Type


class Memory:
    def __init__(self):
        self.frames: List[Dict[str, int]] = [{}]  # имя -> адрес в хипе
        self.heap = Heap()

    def push(self):
        self.frames.append({})

    def pop(self):
        self.frames.pop()

    def declare(self, name: str, typ: Type, init_val: Optional[Any] = None):
        if name in self.frames[-1]: raise NameError(f"{name} already declared")
        addr = self.heap.alloc(typ, init_val)
        self.frames[-1][name] = addr

    def find(self, name: str) -> int:
        for frame in reversed(self.frames):
            if name in frame: return frame[name]
        raise NameError(name)

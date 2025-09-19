from __future__ import annotations

import os
import sys

from executor import Executor
from lexer import Lexer
from parser import Parser


def run(src: str, entry_file_path: str | None = None):
    exe = Executor(entry_file_path=entry_file_path)
    prog = Parser(Lexer(src).tokens()).parse()
    exe.run_program(prog)
    return exe.mem

if __name__ == "__main__":
    path = os.path.join(os.path.dirname(__file__), "code.tl")
    if not os.path.exists(path):
        print("Нет файла code.tl рядом.")
        sys.exit(1)
    with open(path, "r", encoding="utf-8") as f:
        src = f.read()
    mem = run(src, entry_file_path=path)
    print("Конечное состояние хипа:")
    print({addr: (str(type(cell.typ)), cell.val) for addr, cell in mem.heap.store.items()})

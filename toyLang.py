from __future__ import annotations

import os
import sys

from src.executor import Executor
from src.lang_types import PtrType
from src.lexer import Lexer
from src.parser import Parser


def run(src: str, entry_file_path: str | None = None):
    exe = Executor(entry_file_path=entry_file_path)
    prog = Parser(Lexer(src).tokens()).parse()
    exe.run_program(prog)
    return exe.mem

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="ToyLang interpreter")
    parser.add_argument(
        "-d", "--debug",
        action="store_true",
        help="печать состояния хипа после выполнения"
    )
    args = parser.parse_args()

    path = os.path.join(os.path.dirname(__file__), "code.tl")
    if not os.path.exists(path):
        print("Нет файла code.tl рядом.")
        sys.exit(1)

    with open(path, "r", encoding="utf-8") as f:
        src = f.read()
    try:
        mem = run(src, entry_file_path=path)
        if args.debug:
            print("Конечное состояние хипа:")
            for addr, cell in mem.heap.store.items():
                tname = getattr(cell.typ, "name", str(cell.typ))
                is_ptr = isinstance(cell.typ, PtrType)
                if addr in mem.heap.blocks:
                    count = mem.heap.blocks[addr]
                    print(f"  @{addr:04d} : {tname:<10} [array of {count}]")
                elif is_ptr:
                    print(f"  @{addr:04d} : {tname:<10} -> {cell.val}")
                else:
                    print(f"  @{addr:04d} : {tname:<10} = {cell.val}")
    except Exception as e:
        print("Error: " + str(e))

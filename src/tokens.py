from dataclasses import dataclass


@dataclass
class Token:
    kind: str
    value: str
    index: int
    line: int = 1
    col: int = 1
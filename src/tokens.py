from dataclasses import dataclass


@dataclass
class Token:
    kind: str
    value: str
    pos: int

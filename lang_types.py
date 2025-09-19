from __future__ import annotations

from dataclasses import dataclass
from typing import List, Tuple


@dataclass(frozen=True)
class Type: ...
@dataclass(frozen=True)
class IntType(Type): ...
@dataclass(frozen=True)
class BoolType(Type): ...
@dataclass(frozen=True)
class FloatType(Type): ...
@dataclass(frozen=True)
class StringType(Type): ...
@dataclass(frozen=True)
class VoidType(Type): ...
@dataclass(frozen=True)
class PtrType(Type): inner: Type

# ссылка на именованный тип до разрешения
@dataclass(frozen=True)
class TypeName(Type):
    name: str

# определение структуры
@dataclass(frozen=True)
class StructType(Type):
    name: str
    fields: List[Tuple[str, Type]]  # [(field_name, field_type), ...]

INT    = IntType()
BOOL   = BoolType()
FLOAT  = FloatType()
STRING = StringType()
VOID   = VoidType()
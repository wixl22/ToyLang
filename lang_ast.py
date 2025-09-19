from __future__ import annotations

from dataclasses import dataclass
from typing import Optional, List, Tuple

from lang_types import Type


@dataclass
class Expr: ...


@dataclass
class Stmt: ...


@dataclass
class Num(Expr): val: int


@dataclass
class FloatLit(Expr): val: float  # ← новый


@dataclass
class StrLit(Expr): val: str  # ← новый


@dataclass
class BoolLit(Expr): val: bool


@dataclass
class NullLit(Expr): pass


@dataclass
class Var(Expr): name: str


@dataclass
class Unary(Expr): op: str; right: Expr  # '*' deref, '&' address, '-' negate, '!' not, 'new'


@dataclass
class Binary(Expr): left: Expr; op: str; right: Expr  # + - * / % и сравнения


@dataclass
class Assign(Stmt): target: Expr; val: Expr  # target: Var | Unary('*', expr)


@dataclass
class VarDecl(Stmt): name: str; typ: Type; init: Optional[Expr]


@dataclass
class ExprStmt(Stmt): expr: Expr


@dataclass
class Block(Stmt): body: List[Stmt]


@dataclass
class IfStmt(Stmt): cond: Expr; then: Block; other: Optional[Block]


@dataclass
class WhileStmt(Stmt): cond: Expr; body: Block


@dataclass
class BreakStmt(Stmt): pass


@dataclass
class ContinueStmt(Stmt): pass


@dataclass
class NewExpr(Expr):
    typ: Type  # например, INT, PtrType(INT) и т.п.
    count: Optional[Expr]  # None для одиночного new T; Expr для new T[n]


@dataclass
class ImportStmt(Stmt):
    path: str


@dataclass
class FuncDef(Stmt):
    name: str
    params: List[Tuple[str, Type]]  # [(имя, тип), ...]
    ret_type: Type
    body: Block


@dataclass
class ReturnStmt(Stmt):
    expr: Optional[Expr]  # return; или return expr;


@dataclass
class Call(Expr):
    name: str
    args: List[Expr]

@dataclass
class StructDef(Stmt):
    name: str
    fields: List[Tuple[str, Type]]

@dataclass
class FieldAccess(Expr):
    obj: Expr
    field: str

# Метод с явным приёмником
@dataclass
class MethodDef(Stmt):
    recv_name: str        # как назовёшь приёмник (p, this, self, ...)
    recv_type: Type       # TypeName или PtrType(TypeName) и т.д.
    name: str
    params: List[Tuple[str, Type]]
    ret_type: Type
    body: Block

# Вызов метода: recv.method(args)
@dataclass
class MethodCall(Expr):
    recv: Expr
    name: str
    args: List[Expr]
from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Dict, List, Tuple, Optional
import os

from src.lang_ast import (
    Expr, Stmt,
    Num, BoolLit, NullLit, StrLit, FloatLit,
    Var, Unary, Binary, FieldAccess, MethodCall, Call,
    Assign, VarDecl, ExprStmt, Block, IfStmt, WhileStmt,
    BreakStmt, ContinueStmt, ReturnStmt,
    NewExpr, ImportStmt,
    FuncDef, MethodDef, StructDef,
)
from src.lexer import Lexer
from src.parser import Parser
from src.memory import Memory
from src.lang_types import (
    Type, IntType, BoolType, FloatType, StringType, PtrType, StructType, TypeName,
    INT, BOOL, FLOAT, STRING, VOID,
)


# =====================
# Control-flow signals
# =====================
class BreakSignal(Exception):
    pass


class ContinueSignal(Exception):
    pass


@dataclass
class ReturnSignal(Exception):
    value: 'RTVal'


# =====================
# Runtime value
# =====================
@dataclass
class RTVal:
    typ: Type
    val: Any


# =====================
# Helpers (typing/ops)
# =====================

def is_ptr(t: Type) -> bool:
    return isinstance(t, PtrType)


def same_type(a: Type, b: Type) -> bool:
    if type(a) is not type(b):
        return False
    if isinstance(a, PtrType):
        return same_type(a.inner, b.inner)
    if isinstance(a, StructType):
        return a.name == b.name
    return True


def check_assign(dst_t: Type, src_t: Type):
    # null to any pointer is allowed (handled by caller using value == 0)
    if isinstance(dst_t, IntType) and isinstance(src_t, IntType):
        return
    if isinstance(dst_t, BoolType) and isinstance(src_t, BoolType):
        return
    if isinstance(dst_t, FloatType) and isinstance(src_t, FloatType):
        return
    if isinstance(dst_t, StringType) and isinstance(src_t, StringType):
        return
    if is_ptr(dst_t) and is_ptr(src_t) and same_type(dst_t, src_t):
        return
    raise TypeError(f"cannot assign {src_t} to {dst_t}")


def to_bool_val(v: RTVal) -> bool:
    if v.typ == BOOL:
        return bool(v.val)
    if v.typ == INT:
        return v.val != 0
    # optionally accept float
    return bool(v.val) if v.typ == FLOAT else False


def to_int(v: RTVal) -> int:
    if v.typ == INT:
        return int(v.val)
    if v.typ == BOOL:
        return 1 if v.val else 0
    raise TypeError("expected int/bool")


def is_num(t: Type) -> bool:
    return t in (INT, FLOAT, BOOL)


def num_coerce(l: RTVal, r: RTVal) -> Tuple[RTVal, RTVal, Type]:
    # if any float -> float
    if l.typ == FLOAT or r.typ == FLOAT:
        lv = float(l.val if l.typ != BOOL else (1 if l.val else 0))
        rv = float(r.val if r.typ != BOOL else (1 if r.val else 0))
        return RTVal(FLOAT, lv), RTVal(FLOAT, rv), FLOAT
    # else treat as ints (bool -> 0/1)
    return RTVal(INT, to_int(l)), RTVal(INT, to_int(r)), INT


# =====================
# Executor
# =====================
class Executor:
    def __init__(self, entry_file_path: Optional[str] = None):
        self.mem = Memory()
        self.funcs: Dict[str, FuncDef] = {}
        # methods keyed by (StructName, method_name)
        self.methods: Dict[Tuple[str, str], MethodDef] = {}
        # type environment: name -> Type
        self.type_env: Dict[str, Type] = {
            'int': INT, 'bool': BOOL, 'float': FLOAT, 'string': STRING, 'void': VOID,
        }
        self.func_stack: List[FuncDef | MethodDef] = []
        self.entry_dir = os.path.dirname(entry_file_path) if entry_file_path else os.getcwd()
        # exception aliases
        self._EType = TypeError
        self._ERuntime = RuntimeError
        self._EName = NameError
        self._EValue = ValueError

    # ---- error helpers ----
    def _fmt_where(self, node):
        if node is not None and hasattr(node, 'pos') and node.pos:
            line, col = node.pos
            return f" (at line {line}, col {col})"
        return ""

    def terr(self, msg: str, node=None):
        raise self._EType(msg + self._fmt_where(node))

    def rerr(self, msg: str, node=None):
        raise self._ERuntime(msg + self._fmt_where(node))

    def nerr(self, msg: str, node=None):
        raise self._EName(msg + self._fmt_where(node))

    def verr(self, msg: str, node=None):
        raise self._EValue(msg + self._fmt_where(node))

    # ---- type resolution ----
    def resolve_type(self, t: Type) -> Type:
        if isinstance(t, PtrType):
            return PtrType(self.resolve_type(t.inner))
        if isinstance(t, TypeName):
            name = t.name
            if name in self.type_env:
                return self.type_env[name]
            self.terr(f"unknown type {name}")
        return t

    # ---- blocks ----
    def exec_block(self, b: Block):
        self.mem.push()
        try:
            for s in b.body:
                self.exec_stmt(s)
        finally:
            self.mem.pop()

    # ---- expressions ----
    def eval_expr(self, e: Expr) -> RTVal:
        # literals
        if isinstance(e, Num):
            return RTVal(INT, e.val)
        if isinstance(e, FloatLit):
            return RTVal(FLOAT, e.val)
        if isinstance(e, BoolLit):
            # store as 0/1
            return RTVal(BOOL, bool(e.val))
        if isinstance(e, StrLit):
            return RTVal(STRING, e.val)
        if isinstance(e, NullLit):
            # null is pointer 0 (inner type irrelevant)
            return RTVal(PtrType(INT), 0)

        # new / new T[n]
        if isinstance(e, NewExpr):
            t = self.resolve_type(e.typ)
            if e.count is None:
                addr = self.mem.heap.alloc(t)
                return RTVal(PtrType(t), addr)
            n = self.eval_expr(e.count)
            if n.typ != INT:
                self.terr("array size in new T[n] must be int", e)
            if n.val < 1:
                self.verr("array size must be >= 1", e)
            base = self.mem.heap.alloc_block(t, n.val)
            return RTVal(PtrType(t), base)

        # variables
        if isinstance(e, Var):
            if isinstance(e.name, str) and e.name.startswith('#'):
                # internal type code path (optional)
                return RTVal(INT, 0)
            t, v = self.mem.get(e.name)
            return RTVal(t, v)

        # unary
        if isinstance(e, Unary):
            if e.op == '-':
                r = self.eval_expr(e.right)
                if r.typ not in (INT, FLOAT, BOOL):
                    self.terr("unary - expects number", e)
                if r.typ == FLOAT:
                    return RTVal(FLOAT, -float(r.val))
                return RTVal(INT, -to_int(r))
            if e.op == '!':
                r = self.eval_expr(e.right)
                return RTVal(BOOL, not to_bool_val(r))
            if e.op == '&':
                if not isinstance(e.right, Var):
                    self.terr("& expects variable", e)
                pt, addr = self.mem.addr_of(e.right.name)
                return RTVal(pt, addr)
            if e.op == '*':
                r = self.eval_expr(e.right)
                if not is_ptr(r.typ):
                    self.terr("* expects pointer", e)
                if r.val == 0:
                    self.rerr("dereference null", e)
                t = r.typ.inner
                v = self.mem.heap.load(r.val)
                return RTVal(t, v)

        # field access / method call
        if isinstance(e, FieldAccess):
            base = self.eval_expr(e.obj)
            if isinstance(base.typ, PtrType) and isinstance(base.typ.inner, StructType):
                if base.val == 0:
                    self.rerr("dereference null", e)
                obj = self.mem.heap.load(base.val)
                field_t = dict(base.typ.inner.fields)[e.field]
                return RTVal(field_t, obj[e.field])
            self.terr("field access on non-struct pointer", e)

        if isinstance(e, MethodCall):
            # receiver evaluation
            recv = self.eval_expr(e.recv)
            # resolve method
            if isinstance(recv.typ, PtrType) and isinstance(recv.typ.inner, StructType):
                base_struct = recv.typ.inner
                key = (base_struct.name, e.name)
            else:
                self.terr("method call on non-struct", e)
            if key not in self.methods:
                self.nerr(f"unknown method {base_struct.name}.{e.name}", e)
            m = self.methods[key]

            # args
            argvals = [self.eval_expr(a) for a in e.args]
            if len(argvals) != len(m.params):
                self.terr(f"method {e.name} expects {len(m.params)} args", e)

            # call frame
            self.func_stack.append(m)
            self.mem.push()
            try:
                # receiver as variable
                self.mem.declare(m.recv_name, m.recv_type, recv.val)
                # params
                for (pname, ptype), aval in zip(m.params, argvals):
                    if aval.val == 0 and is_ptr(ptype) and is_ptr(aval.typ):
                        self.mem.declare(pname, ptype, 0)
                    else:
                        check_assign(ptype, aval.typ)
                        self.mem.declare(pname, ptype, aval.val)
                # body
                ret: Optional[RTVal] = None
                try:
                    self.exec_block(m.body)
                    if m.ret_type != VOID:
                        self.terr(f"non-void method {base_struct.name}.{e.name} must return a value", e)
                    ret = RTVal(VOID, None)
                except ReturnSignal as rs:
                    ret = rs.value
                    if m.ret_type == VOID and ret.typ != VOID:
                        self.terr("void method cannot return a value", e)
                    if m.ret_type != VOID:
                        if ret.val == 0 and is_ptr(m.ret_type) and is_ptr(ret.typ):
                            ret = RTVal(m.ret_type, 0)
                        else:
                            check_assign(m.ret_type, ret.typ)
                return ret if ret is not None else RTVal(VOID, None)
            finally:
                self.mem.pop()
                self.func_stack.pop()

        if isinstance(e, Call):
            # builtins
            name = e.name.lower()
            args = [self.eval_expr(a) for a in e.args]
            if name in ("print", "печать"):
                out = []
                for v in args:
                    if v.typ == BOOL:
                        out.append(str(1 if v.val else 0))
                    else:
                        out.append(str(v.val))
                print(" ".join(out))
                return RTVal(INT, 0)
            if name in ("free", "освободи"):
                if len(args) != 1:
                    self.terr("free expects 1 argument", e)
                p = args[0]
                if not is_ptr(p.typ):
                    self.terr("free expects pointer", e)
                if p.val == 0:
                    self.rerr("free(null)", e)
                self.mem.heap.free(p.val)
                return RTVal(INT, 0)

            # user function
            if e.name not in self.funcs:
                self.nerr(f"unknown function {e.name}", e)
            fdef = self.funcs[e.name]
            if len(args) != len(fdef.params):
                self.terr(f"function {e.name} expects {len(fdef.params)} args, got {len(args)}", e)

            self.func_stack.append(fdef)
            self.mem.push()
            try:
                # bind params
                for (pname, ptype), aval in zip(fdef.params, args):
                    if aval.val == 0 and is_ptr(ptype) and is_ptr(aval.typ):
                        self.mem.declare(pname, ptype, 0)
                    else:
                        check_assign(ptype, aval.typ)
                        self.mem.declare(pname, ptype, aval.val)
                # body
                ret: Optional[RTVal] = None
                try:
                    self.exec_block(fdef.body)
                    if fdef.ret_type != VOID:
                        self.terr(f"non-void function {e.name} must return a value", e)
                    ret = RTVal(VOID, None)
                except ReturnSignal as rs:
                    ret = rs.value
                    if fdef.ret_type == VOID and ret.typ != VOID:
                        self.terr("void function cannot return a value", e)
                    if fdef.ret_type != VOID:
                        if ret.val == 0 and is_ptr(fdef.ret_type) and is_ptr(ret.typ):
                            ret = RTVal(fdef.ret_type, 0)
                        else:
                            check_assign(fdef.ret_type, ret.typ)
                return ret if ret is not None else RTVal(VOID, None)
            finally:
                self.mem.pop()
                self.func_stack.pop()

        if isinstance(e, Binary):
            op = e.op
            # compute left first
            l = self.eval_expr(e.left)

            # logical short-circuit
            if op == '&&':
                if not to_bool_val(l):
                    return RTVal(BOOL, False)
                r = self.eval_expr(e.right)
                return RTVal(BOOL, to_bool_val(r))
            if op == '||':
                if to_bool_val(l):
                    return RTVal(BOOL, True)
                r = self.eval_expr(e.right)
                return RTVal(BOOL, to_bool_val(r))

            # other ops need right
            r = self.eval_expr(e.right)

            # arithmetic (+-*/%) with bool as 0/1
            if op in ['+', '-', '*', '/', '%'] and is_num(l.typ) and is_num(r.typ):
                if op == '%':
                    l2, r2, kind = num_coerce(l, r)
                    if kind != INT:
                        self.terr("% expects int % int", e)
                    return RTVal(INT, int(l2.val) % int(r2.val))
                l2, r2, kind = num_coerce(l, r)
                if op == '+':
                    return RTVal(kind, l2.val + r2.val)
                if op == '-':
                    return RTVal(kind, l2.val - r2.val)
                if op == '*':
                    return RTVal(kind, l2.val * r2.val)
                if op == '/':
                    return RTVal(FLOAT, float(l2.val) / float(r2.val))

            # pointer arithmetic: ptr +/- int
            if op in ['+', '-'] and is_ptr(l.typ) and r.typ == INT:
                return RTVal(l.typ, l.val + (r.val if op == '+' else -r.val))
            if op in ['+', '-'] and l.typ == INT and is_ptr(r.typ):
                return RTVal(r.typ, (l.val if op == '+' else -l.val) + r.val)

            # numeric comparisons (including bool as number)
            if op in ['==', '!=', '<', '<=', '>', '>='] and is_num(l.typ) and is_num(r.typ):
                l2, r2, _ = num_coerce(l, r)
                res = {
                    '==': l2.val == r2.val,
                    '!=': l2.val != r2.val,
                    '<':  l2.val <  r2.val,
                    '<=': l2.val <= r2.val,
                    '>':  l2.val >  r2.val,
                    '>=': l2.val >= r2.val,
                }[op]
                return RTVal(BOOL, res)

            # pointer equality/inequality; allow null on either side
            if is_ptr(l.typ) and is_ptr(r.typ) and op in ['==', '!=']:
                if l.val == 0 or r.val == 0:
                    res = (l.val == r.val) if op == '==' else (l.val != r.val)
                    return RTVal(BOOL, res)
                if same_type(l.typ, r.typ):
                    res = (l.val == r.val) if op == '==' else (l.val != r.val)
                    return RTVal(BOOL, res)

            self.terr(f"bad operands for {op}: {l.typ} and {r.typ}", e)

        self.rerr("bad expr", e)

    # ---- lvalue helper (for a.b = x) ----
    def resolve_field_lvalue(self, fa: FieldAccess) -> Tuple[int, StructType, str]:
        # Var.field where Var is *Struct
        if isinstance(fa.obj, Var):
            t, v = self.mem.get(fa.obj.name)
            if isinstance(t, PtrType) and isinstance(t.inner, StructType):
                if v == 0:
                    self.rerr("dereference null", fa)
                return v, t.inner, fa.field
            self.terr("field access on non-struct pointer", fa)

        # (*expr).field
        if isinstance(fa.obj, Unary) and fa.obj.op == '*':
            p = self.eval_expr(fa.obj.right)
            if not isinstance(p.typ, PtrType) or not isinstance(p.typ.inner, StructType):
                self.terr("dereference non-struct pointer", fa)
            if p.val == 0:
                self.rerr("dereference null", fa)
            return p.val, p.typ.inner, fa.field

        self.terr("assignment target must be name.field or (*ptr).field", fa)
        return 0, StructType("", []), ""  # unreachable

    # ---- statements ----
    def exec_stmt(self, s: Stmt):
        if isinstance(s, BreakStmt):
            raise BreakSignal()
        if isinstance(s, ContinueStmt):
            raise ContinueSignal()

        if isinstance(s, ImportStmt):
            # relative to entry_dir
            abs_path = s.path
            if not os.path.isabs(abs_path):
                abs_path = os.path.join(self.entry_dir, s.path)
            if not os.path.isfile(abs_path):
                self.nerr(f"module {s.path} not found (resolved: {abs_path})", s)
            with open(abs_path, 'r', encoding='utf-8') as f:
                src = f.read()
            prog = Parser(Lexer(src).tokens()).parse()
            # same env
            for st in prog:
                self.exec_stmt(st)
            return

        if isinstance(s, StructDef):
            # two-phase: register empty, then resolve fields
            st = StructType(s.name, [])
            self.type_env[s.name] = st
            fields: List[Tuple[str, Type]] = []
            for fname, ft in s.fields:
                fields.append((fname, self.resolve_type(ft)))
            st.fields.extend(fields)
            return

        if isinstance(s, MethodDef):
            recv_t = self.resolve_type(s.recv_type)
            # only pointer receiver allowed now
            if not (isinstance(recv_t, PtrType) and isinstance(recv_t.inner, StructType)):
                self.terr("method receiver must be *Struct", s)
            key = (recv_t.inner.name, s.name)
            params = [(pname, self.resolve_type(ptype)) for (pname, ptype) in s.params]
            self.methods[key] = MethodDef(s.recv_name, recv_t, s.name, params, self.resolve_type(s.ret_type), s.body)
            return

        if isinstance(s, FuncDef):
            # store resolved signature (resolve param/ret types now)
            params: List[Tuple[str, Type]] = []
            for pname, ptype in s.params:
                params.append((pname, self.resolve_type(ptype)))
            self.funcs[s.name] = FuncDef(s.name, params, self.resolve_type(s.ret_type), s.body)
            return

        if isinstance(s, VarDecl):
            t = self.resolve_type(s.typ)
            # forbid bare struct values
            if isinstance(t, StructType):
                self.terr(f"struct variables must be pointers: use '*{t.name}' and 'нов {t.name}'", s)
            init_val = None
            if s.init is not None:
                v = self.eval_expr(s.init)
                if v.val == 0 and is_ptr(t) and is_ptr(v.typ):
                    init_val = 0
                else:
                    check_assign(t, v.typ)
                    init_val = v.val
            self.mem.declare(s.name, t, init_val)
            return

        if isinstance(s, Assign):
            # variable target
            if isinstance(s.target, Var):
                dst_t = self.mem.typeof_var(s.target.name)
                v = self.eval_expr(s.val)
                if v.val == 0 and is_ptr(dst_t) and is_ptr(v.typ):
                    self.mem.set(s.target.name, 0)
                else:
                    check_assign(dst_t, v.typ)
                    self.mem.set(s.target.name, v.val)
                return
            # *expr = val
            if isinstance(s.target, Unary) and s.target.op == '*':
                ptr = self.eval_expr(s.target.right)
                if not is_ptr(ptr.typ):
                    self.terr("*target must be pointer", s.target)
                if ptr.val == 0:
                    self.rerr("store to null", s.target)
                addr = ptr.val
                dst_t = self.mem.heap.typeof(addr)
                v = self.eval_expr(s.val)
                if v.val == 0 and is_ptr(dst_t) and is_ptr(v.typ):
                    self.mem.heap.store_val(addr, 0)
                else:
                    check_assign(dst_t, v.typ)
                    self.mem.heap.store_val(addr, v.val)
                return
            # a.b = val
            if isinstance(s.target, FieldAccess):
                addr, struct_t, field = self.resolve_field_lvalue(s.target)
                field_types = dict(struct_t.fields)
                if field not in field_types:
                    self.nerr(f"no such field {struct_t.name}.{field}", s.target)
                dst_t = field_types[field]
                v = self.eval_expr(s.val)
                if v.val == 0 and is_ptr(dst_t) and is_ptr(v.typ):
                    obj = self.mem.heap.load(addr)
                    obj[field] = 0
                    self.mem.heap.store_val(addr, obj)
                else:
                    check_assign(dst_t, v.typ)
                    obj = self.mem.heap.load(addr)
                    obj[field] = v.val
                    self.mem.heap.store_val(addr, obj)
                return
            self.terr("assignment target must be name, *expr or field", s)

        if isinstance(s, ExprStmt):
            self.eval_expr(s.expr)
            return

        if isinstance(s, IfStmt):
            c = self.eval_expr(s.cond)
            if to_bool_val(c):
                self.exec_block(s.then)
            elif s.other:
                self.exec_block(s.other)
            return

        if isinstance(s, WhileStmt):
            while True:
                c = self.eval_expr(s.cond)
                if not to_bool_val(c):
                    break
                try:
                    self.exec_block(s.body)
                except ContinueSignal:
                    continue
                except BreakSignal:
                    break
            return

        if isinstance(s, Block):
            self.exec_block(s)
            return

        if isinstance(s, ReturnStmt):
            # determine expected ret type from current function/method
            expected = VOID
            if not self.func_stack:
                self.terr("return outside of function")
            expected = self.func_stack[-1].ret_type
            if expected == VOID:
                if s.expr is not None:
                    self.terr("void function cannot return a value", s)
                raise ReturnSignal(RTVal(VOID, None))
            else:
                if s.expr is None:
                    self.terr("non-void function must return a value", s)
                v = self.eval_expr(s.expr)
                if v.val == 0 and is_ptr(expected) and is_ptr(v.typ):
                    raise ReturnSignal(RTVal(expected, 0))
                check_assign(expected, v.typ)
                raise ReturnSignal(RTVal(expected, v.val))

        self.rerr("bad stmt", s)

    # ---- program ----
    def run_program(self, prog: List[Stmt]):
        for st in prog:
            self.exec_stmt(st)
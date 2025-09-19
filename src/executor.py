from __future__ import annotations
import os
from dataclasses import dataclass
from typing import List, Any, Tuple

from src.lang_ast import Expr, Stmt, Num, BoolLit, NullLit, Var, Unary, Binary, Assign, VarDecl, ExprStmt, Block, IfStmt, \
    WhileStmt, BreakStmt, ContinueStmt, NewExpr, ImportStmt, FuncDef, ReturnStmt, Call, StrLit, FloatLit, MethodDef, \
    StructDef, FieldAccess, MethodCall
from src.lexer import Lexer
from src.memory import Memory
from src.parser import Parser
from src.lang_types import PtrType, INT, BOOL, Type, IntType, BoolType, VOID, FLOAT, STRING, TypeName, StructType


def is_ptr(t): return isinstance(t, PtrType)

def is_num(t):  # число = int/float/bool (bool как int)
    return t in (INT, FLOAT, BOOL)

def to_int(v):  # RTVal -> python int (bool → 0/1)
    if v.typ == INT:  return int(v.val)
    if v.typ == BOOL: return 1 if bool(v.val) else 0
    raise TypeError("expected int/bool")

def to_bool_val(v):  # RTVal -> python bool, допускаем int/bool
    if v.typ == BOOL: return bool(v.val)
    if v.typ == INT:  return v.val != 0
    # (опционально: разрешить FLOAT → v.val != 0.0)
    raise TypeError("condition/logical expects int/bool")

def num_coerce(l, r):
    # если есть float → оба в float; иначе оба в int (bool→int)
    if l.typ == FLOAT or r.typ == FLOAT:
        lv = float(l.val if l.typ != BOOL else (1 if l.val else 0))
        rv = float(r.val if r.typ != BOOL else (1 if r.val else 0))
        return RTVal(FLOAT, lv), RTVal(FLOAT, rv), FLOAT
    # всё остальное → в int (bool→0/1)
    return RTVal(INT, to_int(l)), RTVal(INT, to_int(r)), INT


class Executor:
    def __init__(self, entry_file_path: str | None = None):
        self.mem = Memory()
        base_dir = os.path.dirname(os.path.abspath(entry_file_path)) if entry_file_path else os.getcwd()
        self.dir_stack: List[str] = [base_dir]
        self.funcs: dict[str, FuncDef] = {}
        self.type_env: dict[str, Type] = {
            "int": INT, "bool": BOOL, "float": FLOAT, "string": STRING, "void": VOID
        }
        self.methods: dict[tuple[str, str], MethodDef] = {}  # (StructName, methodName) -> def

    # ---- helpers ----
    def ensure_bool(self, v: RTVal):
        if v.typ != BOOL:
            raise TypeError("condition must be bool")

    def exec_block(self, b: Block):
        self.mem.push()
        try:
            for s in b.body:
                self.exec_stmt(s)
        finally:
            self.mem.pop()

    def error(self, msg: str, node=None):
        if node is not None and hasattr(node, "pos") and node.pos:
            line, col = node.pos
            raise RuntimeError(f"{msg} (at line {line}, col {col})")
        raise RuntimeError(msg)

    # ---- expressions ----
    def eval_expr(self, e: Expr) -> RTVal:
        if isinstance(e, FloatLit): return RTVal(FLOAT, e.val)
        if isinstance(e, StrLit):   return RTVal(STRING, e.val)
        if isinstance(e, Num):     return RTVal(INT, e.val)
        if isinstance(e, BoolLit): return RTVal(BOOL, e.val)
        if isinstance(e, NullLit): return RTVal(PtrType(INT), 0)

        if isinstance(e, MethodCall):
            recv = self.eval_expr(e.recv)
            # определяем тип структуры и проверяем совместимость с методом
            if isinstance(recv.typ, StructType):
                base_struct = recv.typ
                recv_kind = "value"
            elif isinstance(recv.typ, PtrType) and isinstance(recv.typ.inner, StructType):
                base_struct = recv.typ.inner
                recv_kind = "ptr"
            else:
                raise TypeError("method call on non-struct")

            key = (base_struct.name, e.name)
            if key not in self.methods:
                raise NameError(f"unknown method {base_struct.name}.{e.name}")
            m = self.methods[key]

            # сверяем вид приёмника
            expects_ptr = isinstance(m.recv_type, PtrType)
            if expects_ptr and recv_kind != "ptr":
                raise TypeError(f"method {base_struct.name}.{e.name} expects pointer receiver")
            if not expects_ptr and recv_kind != "value":
                # допускаем обращение через указатель к value-методу? для простоты — запрещаем
                raise TypeError(f"method {base_struct.name}.{e.name} expects value receiver")

            # вычисляем и проверяем аргументы
            argvals = [self.eval_expr(a) for a in e.args]
            if len(argvals) != len(m.params):
                raise TypeError(f"method {e.name} expects {len(m.params)} args")

            # --- ВАЖНО: подготовим стек функций, чтобы ReturnStmt видел правильный ret_type
            if not hasattr(self, "func_stack"):
                self.func_stack = []
            self.func_stack.append(m)  # у MethodDef есть .ret_type

            self.mem.push()
            try:
                # приёмник
                self.mem.declare(m.recv_name, m.recv_type, recv.val)

                # параметры
                for (pname, ptype), aval in zip(m.params, argvals):
                    if aval.val == 0 and isinstance(ptype, PtrType) and isinstance(aval.typ, PtrType):
                        self.mem.declare(pname, ptype, 0)
                    else:
                        check_assign(ptype, aval.typ)
                        self.mem.declare(pname, ptype, aval.val)

                try:
                    self.exec_block(m.body)
                    if m.ret_type != VOID:
                        raise TypeError(f"non-void method {base_struct.name}.{e.name} must return a value")
                    return RTVal(VOID, None)
                except ReturnSignal as rs:
                    ret = rs.value
                    if m.ret_type == VOID and ret.typ != VOID:
                        raise TypeError("void method cannot return a value")
                    if m.ret_type != VOID:
                        if ret.val == 0 and isinstance(m.ret_type, PtrType) and isinstance(ret.typ, PtrType):
                            return RTVal(m.ret_type, 0)
                        check_assign(m.ret_type, ret.typ)
                    return ret
            finally:
                self.mem.pop()
                self.func_stack.pop()
        if isinstance(e, Call):
            # вычисляем аргументы заранее
            args = [self.eval_expr(a) for a in e.args]

            # ---- builtins ----
            name = e.name.lower()

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
                    raise TypeError("free expects 1 argument")
                p = args[0]
                if not is_ptr(p.typ):
                    raise TypeError("free expects pointer")
                if p.val == 0:
                    raise RuntimeError("free(null)")
                self.mem.heap.free(p.val)
                return RTVal(INT, 0)

            # ---- user-defined ----
            if e.name not in self.funcs:
                raise NameError(f"unknown function {e.name}")
            fdef = self.funcs[e.name]

            if len(args) != len(fdef.params):
                raise TypeError(f"function {e.name} expects {len(fdef.params)} args, got {len(args)}")

            # стек активных функций
            if not hasattr(self, "func_stack"):
                self.func_stack = []
            self.func_stack.append(fdef)

            self.mem.push()
            try:
                # биндим параметры
                for (pname, ptype), aval in zip(fdef.params, args):
                    if aval.val == 0 and isinstance(ptype, PtrType) and isinstance(aval.typ, PtrType):
                        init_val = 0
                    else:
                        check_assign(ptype, aval.typ)
                        init_val = aval.val
                    self.mem.declare(pname, ptype, init_val)

                # выполняем тело
                try:
                    self.exec_block(fdef.body)
                    # если дошли до конца без return:
                    if fdef.ret_type == VOID:
                        return RTVal(VOID, None)
                    else:
                        raise TypeError(f"non-void function '{fdef.name}' must end with return")
                except ReturnSignal as rs:
                    # убедимся, что тип совпадает с объявленным (на всякий случай)
                    ret = rs.value
                    if fdef.ret_type == VOID and ret.typ != VOID:
                        raise TypeError("void function cannot return a value")
                    if fdef.ret_type != VOID:
                        if ret.val == 0 and isinstance(fdef.ret_type, PtrType) and isinstance(ret.typ, PtrType):
                            return RTVal(fdef.ret_type, 0)
                        check_assign(fdef.ret_type, ret.typ)
                    return ret
            finally:
                self.mem.pop()
                self.func_stack.pop()

        if isinstance(e, FieldAccess):
            base = self.eval_expr(e.obj)
            if isinstance(base.typ, PtrType) and isinstance(base.typ.inner, StructType):
                if base.val == 0: raise RuntimeError("dereference null")
                obj = self.mem.heap.load(base.val)  # dict полей
                field_t = dict(base.typ.inner.fields)[e.field]
                return RTVal(field_t, obj[e.field])
            raise TypeError("field access on non-struct pointer")

        if isinstance(e, NewExpr):
            t = self.resolve_type(e.typ)  # ← было: t = e.typ
            if e.count is None:
                addr = self.mem.heap.alloc(t)
                return RTVal(PtrType(t), addr)
            n = self.eval_expr(e.count)
            if n.typ != INT: raise TypeError("array size in new T[n] must be int")
            if n.val < 1:   raise ValueError("array size must be >= 1")
            base = self.mem.heap.alloc_block(t, n.val)
            return RTVal(PtrType(t), base)

        if isinstance(e, Var):
            # тип-”код” (#int/…) оставь как было, если используешь
            if e.name.startswith('#'):
                return RTVal(_decode_type(e.name), None)
            t, v = self.mem.get(e.name)
            return RTVal(t, v)

        if isinstance(e, Unary):
            if e.op == '-':
                r = self.eval_expr(e.right)
                if r.typ != INT: self.error("unary - expects int", e)
                return RTVal(INT, -int(r.val))
            if e.op == '!':
                r = self.eval_expr(e.right)
                b = to_bool_val(r)
                return RTVal(BOOL, (not b))
            if e.op == '&':
                if not isinstance(e.right, Var):
                    self.error("& expects variable", e)
                pt, addr = self.mem.addr_of(e.right.name)
                return RTVal(pt, addr)
            if e.op == '*':
                r = self.eval_expr(e.right)
                if not is_ptr(r.typ): self.error("* expects pointer", e)
                if r.val == 0: self.error("dereference null", e)
                t = r.typ.inner
                v = self.mem.heap.load(r.val)
                return RTVal(t, v)
            if e.op == 'new':
                t = _decode_type(e.right.name)
                addr = self.mem.heap.alloc(t)
                return RTVal(PtrType(t), addr)

        if isinstance(e, Binary):
            l = self.eval_expr(e.left)
            op = e.op

            if op == '&&':
                lb = to_bool_val(l)  # принимает int/bool
                if not lb:
                    return RTVal(BOOL, False)
                r = self.eval_expr(e.right)
                rb = to_bool_val(r)
                return RTVal(BOOL, rb)

            if op == '||':
                lb = to_bool_val(l)
                if lb:
                    return RTVal(BOOL, True)
                r = self.eval_expr(e.right)
                rb = to_bool_val(r)
                return RTVal(BOOL, rb)

            r = self.eval_expr(e.right)

            if op in ['+', '-', '*', '/', '%'] and is_num(l.typ) and is_num(r.typ):
                if op == '%':
                    l2, r2, _ = num_coerce(l, r)
                    # модуль определён только для int → округляем заранее
                    if _ != INT:
                        raise TypeError("% expects int % int")
                    return RTVal(INT, int(l2.val) % int(r2.val))
                l2, r2, RT = num_coerce(l, r)
                if op == '+': return RTVal(RT, l2.val + r2.val)
                if op == '-': return RTVal(RT, l2.val - r2.val)
                if op == '*': return RTVal(RT, l2.val * r2.val)
                if op == '/': return RTVal(FLOAT, float(l2.val) / float(r2.val))

            if op == '+' and l.typ == STRING and r.typ == STRING:
                return RTVal(STRING, str(l.val) + str(r.val))
            if op in ['==', '!='] and l.typ == STRING and r.typ == STRING:
                return RTVal(BOOL, (l.val == r.val) if op == '==' else (l.val != r.val))
            if op in ['+', '-'] and is_ptr(l.typ) and r.typ == INT:
                return RTVal(l.typ, l.val + (int(r.val) if op == '+' else -int(r.val)))
            if op in ['+', '-'] and l.typ == INT and is_ptr(r.typ):
                return RTVal(r.typ, (int(l.val) if op == '+' else -int(l.val)) + r.val)

            if op in ['==', '!=', '<', '<=', '>', '>='] and is_num(l.typ) and is_num(r.typ):
                l2, r2, _ = num_coerce(l, r)
                return RTVal(BOOL, {
                    '==': l2.val == r2.val,
                    '!=': l2.val != r2.val,
                    '<': l2.val < r2.val,
                    '<=': l2.val <= r2.val,
                    '>': l2.val > r2.val,
                    '>=': l2.val >= r2.val,
                }[op])

            if is_ptr(l.typ) and is_ptr(r.typ) and op in ['==', '!=']:
                # если один из адресов = 0 (null) — сравниваем без проверки same_type
                if l.val == 0 or r.val == 0:
                    return RTVal(BOOL, (l.val == r.val) if op == '==' else (l.val != r.val))
                # иначе — строго по одинаковому типу
                if same_type(l.typ, r.typ):
                    return RTVal(BOOL, (l.val == r.val) if op == '==' else (l.val != r.val))

            if op in ['==', '!='] and l.typ == BOOL and r.typ == BOOL:
                return RTVal(BOOL, (bool(l.val) == bool(r.val)) if op == '==' else (bool(l.val) != bool(r.val)))

            raise TypeError(f"bad operands for {op}: {l.typ} and {r.typ}")

        raise RuntimeError("bad expr kind")

    def resolve_type(self, t: Type) -> Type:
        if isinstance(t, PtrType):
            return PtrType(self.resolve_type(t.inner))
        if isinstance(t, TypeName):
            real = self.type_env.get(t.name)
            if real is None:
                raise TypeError(f"unknown type {t.name}")
            return real
        return t

    def resolve_field_lvalue(self, fa: FieldAccess):
        # допускаем:
        #   Var.field               // если Var: StructType
        #   VarPtr.field            // если Var: PtrType(StructType) → авто *Var
        #   (*expr).field
        # возвращаем: (addr_struct_obj, struct_type, field_name)

        # 1) obj — Var
        if isinstance(fa.obj, Var):
            t, v = self.mem.get(fa.obj.name)  # t: PtrType(StructType) ожидаем
            if isinstance(t, PtrType) and isinstance(t.inner, StructType):
                if v == 0: raise RuntimeError("dereference null")
                return v, t.inner, fa.field  # (адрес структуры в хипе, тип структуры, имя поля)
            raise TypeError("field access on non-struct pointer")

        # 2) obj — Unary('*', ...)
        if isinstance(fa.obj, Unary) and fa.obj.op == '*':
            p = self.eval_expr(fa.obj.right)
            if not isinstance(p.typ, PtrType) or not isinstance(p.typ.inner, StructType):
                raise TypeError("dereference non-struct pointer")
            if p.val == 0:
                raise RuntimeError("dereference null")
            return p.val, p.typ.inner, fa.field

        raise TypeError("assignment target must be name.field or (*ptr).field")

    # ---- statements ----
    def exec_stmt(self, s: Stmt):
        if isinstance(s, BreakStmt):    raise BreakSignal()
        if isinstance(s, ContinueStmt): raise ContinueSignal()
        if isinstance(s, FuncDef):
            self.funcs[s.name] = s
            return

        if isinstance(s, MethodDef):
            # резолвим тип приёмника, параметры и рет-тип
            rcv_t = self.resolve_type(s.recv_type)
            params = [(n, self.resolve_type(t)) for n, t in s.params]
            ret_t = self.resolve_type(s.ret_type)
            mdef = MethodDef(s.recv_name, rcv_t, s.name, params, ret_t, s.body)
            # ключ: имя структуры (не PtrType)
            key_t = rcv_t.inner if isinstance(rcv_t, PtrType) else rcv_t
            if not isinstance(key_t, StructType):
                raise TypeError("method receiver must be struct or *struct")
            self.methods[(key_t.name, s.name)] = mdef
            return

        if isinstance(s, StructDef):
            # 1) сначала кладём пустую структуру в type_env, чтобы разрешались самоссылки
            st = StructType(s.name, [])  # важна МУТАБЕЛЬНАЯ [] внутри!
            self.type_env[s.name] = st

            # 2) теперь можно резолвить поля (в т.ч. *Node на только что созданный st)
            resolved = []
            for fname, ftype in s.fields:
                resolved.append((fname, self.resolve_type(ftype)))

            # 3) дозаполняем поля
            st.fields.extend(resolved)
            return

        if isinstance(s, Assign) and isinstance(s.target, FieldAccess):
            addr, struct_t, field = self.resolve_field_lvalue(s.target)
            # проверяем, что поле есть
            field_types = dict(struct_t.fields)
            if field not in field_types:
                raise NameError(f"no such field {struct_t.name}.{field}")
            dst_t = field_types[field]
            v = self.eval_expr(s.val)
            # допускаем null для *T
            if v.val == 0 and isinstance(dst_t, PtrType) and isinstance(v.typ, PtrType):
                obj = self.mem.heap.load(addr)
                obj[field] = 0
                self.mem.heap.store_val(addr, obj)
                return
            check_assign(dst_t, v.typ)
            obj = self.mem.heap.load(addr)
            obj[field] = v.val
            self.mem.heap.store_val(addr, obj)
            return

        # return
        if isinstance(s, ReturnStmt):
            # узнаём ожидаемый ret_type текущей функции
            # самый простой способ — хранить стек активных FuncDef'ов:
            # self.func_stack[-1].ret_type
            ret_t = self.func_stack[-1].ret_type if getattr(self, "func_stack", None) else VOID

            if s.expr is None:
                # return; допустим ТОЛЬКО для void
                if ret_t != VOID:
                    raise TypeError("non-void function must return a value")
                raise ReturnSignal(RTVal(VOID, None))
            else:
                v = self.eval_expr(s.expr)
                if ret_t == VOID:
                    raise TypeError("void function cannot return a value")
                # допускаем null в *T
                if v.val == 0 and isinstance(ret_t, PtrType) and isinstance(v.typ, PtrType):
                    raise ReturnSignal(RTVal(ret_t, 0))
                # обычная проверка типов
                check_assign(ret_t, v.typ)
                raise ReturnSignal(RTVal(ret_t, v.val))

        if isinstance(s, ImportStmt):
            # resolve path relative to current file on the stack
            cur_dir = self.dir_stack[-1]
            abs_path = s.path if os.path.isabs(s.path) else os.path.normpath(os.path.join(cur_dir, s.path))
            if not os.path.isfile(abs_path):
                raise FileNotFoundError(f"module {s.path} not found (resolved: {abs_path})")
            with open(abs_path, "r", encoding="utf-8") as f:
                src = f.read()
            prog = Parser(Lexer(src).tokens()).parse()
            new_dir = os.path.dirname(abs_path)
            self.dir_stack.append(new_dir)
            try:
                for st in prog:
                    self.exec_stmt(st)
            finally:
                self.dir_stack.pop()
            return

        if isinstance(s, VarDecl):
            t = self.resolve_type(s.typ)  # ← добавили
            if isinstance(t, StructType):
                raise TypeError(f"struct variables must be pointers: use '*{t.name}' and 'нов {t.name}'")
            init_val = None
            if s.init is not None:
                v = self.eval_expr(s.init)
                if v.val == 0 and isinstance(t, PtrType) and isinstance(v.typ, PtrType):
                    init_val = 0
                else:
                    check_assign(t, v.typ)
                    init_val = v.val
            self.mem.declare(s.name, t, init_val)  # ← кладём РЕШЁННЫЙ тип
            return

        if isinstance(s, Assign):
            if isinstance(s.target, Var):
                dst_t = self.mem.typeof_var(s.target.name)
                v = self.eval_expr(s.val)
                if v.val == 0 and is_ptr(dst_t) and is_ptr(v.typ):
                    self.mem.set(s.target.name, 0)
                else:
                    check_assign(dst_t, v.typ)
                    self.mem.set(s.target.name, v.val)
                return
            elif isinstance(s.target, Unary) and s.target.op == '*':
                ptr = self.eval_expr(s.target.right)
                if not is_ptr(ptr.typ): raise TypeError("*target must be pointer")
                if ptr.val == 0: raise RuntimeError("store to null")
                addr = ptr.val
                dst_t = self.mem.heap.typeof(addr)
            else:
                raise TypeError("assignment target must be name or *expr")
            v = self.eval_expr(s.val)
            if v.val == 0 and is_ptr(dst_t) and is_ptr(v.typ):
                self.mem.heap.store_val(addr, 0)
            else:
                check_assign(dst_t, v.typ)
                self.mem.heap.store_val(addr, v.val)
            return

        if isinstance(s, ExprStmt):
            self.eval_expr(s.expr);
            return

        if isinstance(s, IfStmt):
            c = self.eval_expr(s.cond)
            if not to_bool_val(c):  # ← вместо ensure_bool + прямой .val
                self.exec_block(s.then)
            elif s.other:
                self.exec_block(s.other)
            return

        if isinstance(s, WhileStmt):
            while True:
                c = self.eval_expr(s.cond)
                if not to_bool_val(c): break
                try:
                    self.exec_block(s.body)
                except ContinueSignal:
                    continue
                except BreakSignal:
                    break
            return

        if isinstance(s, Block):
            self.exec_block(s);
            return

        raise RuntimeError("bad stmt")

    # ---- entry points ----
    def run_program(self, prog: list[Stmt]) -> Memory:
        for st in prog:
            self.exec_stmt(st)
        return self.mem

    def run_source(self, src: str) -> Memory:
        prog = Parser(Lexer(src).tokens()).parse()
        return self.run_program(prog)


@dataclass
class RTVal:
    typ: Type
    val: Any


def _decode_type(code: str) -> Type:
    assert code.startswith('#')
    s = code[1:]

    def parse(i: int) -> Tuple[Type, int]:
        if s[i] == 'i':
            assert s[i:i + 3] == 'int';
            return (INT, i + 3)
        if s[i] == 'b':
            assert s[i:i + 4] == 'bool';
            return (BOOL, i + 4)
        if s[i] == '*':
            inner, j = parse(i + 1)
            return (PtrType(inner), j)
        raise RuntimeError('bad type code')

    t, _ = parse(0)
    return t


class ContinueSignal(Exception): pass


class BreakSignal(Exception): pass


class ReturnSignal(Exception):
    def __init__(self, value):
        self.value = value


def same_type(a: Type, b: Type) -> bool:
    # одинаковые указатели → сравниваем внутренние типы рекурсивно
    if is_ptr(a) and is_ptr(b):
        return same_type(a.inner, b.inner)
    # для не-указателей: совпадение по классу типа (IntType, FloatType, StringType, BoolType, VoidType)
    return type(a) == type(b)


def check_assign(dst_t: Type, src_t: Type):
    # всё сводим к same_type
    if same_type(dst_t, src_t):
        return
    # (разрешение null для указателей остаётся в исполнителе, как у тебя уже сделано:
    # if v.val == 0 and is_ptr(dst_t) and is_ptr(v.typ): ok)
    raise TypeError(f"cannot assign {src_t} to {dst_t}")


def same_type(a: Type, b: Type) -> bool:
    if type(a) != type(b): return False
    if is_ptr(a): return same_type(a.inner, b.inner)
    return True

from __future__ import annotations

from src.lang_types import Type, IntType, BoolType, PtrType, INT, BOOL, VOID, FLOAT, STRING, TypeName
from src.lang_ast import Expr, Num, BoolLit, NullLit, Var, Unary, Binary, Assign, VarDecl, ExprStmt, Block, IfStmt, \
    WhileStmt, BreakStmt, ContinueStmt, NewExpr, ImportStmt, Call, ReturnStmt, FuncDef, StrLit, FloatLit, \
    StructDef, MethodDef, FieldAccess, MethodCall


class Parser:
    def __init__(self, toks):
        self.toks = toks
        self.i = 0

    def peek(self):
        return self.toks[self.i]

    def ahead(self, k=1):
        j = self.i + k
        return self.toks[j] if j < len(self.toks) else self.toks[-1]

    def match(self, *kinds):
        peek = self.peek()
        if peek.kind in kinds:
            self.i += 1
            return True
        return False

    def expect(self, kind, msg=None):
        if not self.match(kind):
            got = self.peek().kind
            raise SyntaxError(msg or f"expected {kind} got {got}")

    def parse(self):
        out = []
        while self.peek().kind != "EOF":
            out.append(self.statement())
        return out

    # ---------------- statements ----------------
    def statement(self):
        # ---- import / импорт ----
        if self.match("IMPORT"):
            t = self.peek()
            self.expect("STRING", "expected string path")
            path = t.value
            self.expect("SEMI", ";")
            return ImportStmt(path)

        # ---- let / создПер ----
        if self.match("LET"):
            # Две формы:
            # A) let name: type [= expr] ;
            # B) создПер type name [:=|= expr] ;
            # Определим по lookahead: IDENT ':' → форма A, иначе форма B
            if self.peek().kind == "IDENT" and self.ahead().kind == "COLON":
                # старая форма
                name_tok = self.peek(); self.expect("IDENT", "ident")
                self.expect("COLON", ":")
                typ = self.parse_type()
                init = None
                if self.match("ASSIGN") or self.match("ASSIGN2"):
                    init = self.expression()
                self.expect("SEMI", ";")
                return VarDecl(name_tok.value, typ, init)
            else:
                # новая форма: создПер type name [:=|= expr];
                typ = self.parse_type()
                name_tok = self.peek(); self.expect("IDENT", "var name")
                init = None
                if self.match("ASSIGN2") or self.match("ASSIGN"):
                    init = self.expression()
                self.expect("SEMI", ";")
                return VarDecl(name_tok.value, typ, init)

        # ---- if / если ----
        if self.match("IF"):
            self.expect("LP", "(")
            cond = self.expression()
            self.expect("RP", ")")
            then = self.block_required()
            other = None
            if self.match("ELSE"):
                other = self.block_required()
            return IfStmt(cond, then, other)

        # ---- while / пока ----
        if self.match("WHILE"):
            self.expect("LP", "(")
            cond = self.expression()
            self.expect("RP", ")")
            body = self.block_required()
            return WhileStmt(cond, body)

        # ---- break / прервать ----
        if self.match("BREAK"):
            self.expect("SEMI", ";")
            return BreakStmt()

        # ---- continue / продолжить ----
        if self.match("CONTINUE"):
            self.expect("SEMI", ";")
            return ContinueStmt()

        # ---- return / вернуть ----
        if self.match("RETURN"):
            expr = None
            if not self.match("SEMI"):
                expr = self.expression()
                self.expect("SEMI", ";")
            return ReturnStmt(expr)

        # ---- struct / создСтрук ----
        if self.match("STRUCT"):
            name_tok = self.peek()
            self.expect("IDENT", "struct name")
            self.expect("LB", "{")
            fields = []
            while not self.match("RB"):
                # две формы поля:
                # 1) name : type ;
                # 2) атр type name ;
                if self.match("ATTR"):
                    f_type = self.parse_type()
                    f_name = self.peek(); self.expect("IDENT", "field name")
                    self.expect("SEMI", ";")
                    fields.append((f_name.value, f_type))
                    continue
                f_name = self.peek(); self.expect("IDENT", "field name")
                self.expect("COLON", ":")
                f_type = self.parse_type()
                self.expect("SEMI", ";")
                fields.append((f_name.value, f_type))
            return StructDef(name_tok.value, fields)

        # ---- func / создФунк (обычные функции и методы) ----
        if self.match("FUNC"):
            # метод? начинаем с '('
            if self.match("LP"):
                # Две формы ресивера:
                #   (RECV type name)   — RU-алиас
                #   (name : type)      — старый синтаксис
                recv_name = None
                recv_type = None
                # пробуем RECV
                if self.match("RECV"):
                    recv_type = self.parse_type()
                    r_name_tok = self.peek(); self.expect("IDENT", "recv name")
                    recv_name = r_name_tok.value
                    self.expect("RP", ")")
                else:
                    # старый: (name : type)
                    r_name_tok = self.peek(); self.expect("IDENT", "recv name")
                    self.expect("COLON", ":")
                    recv_type = self.parse_type()
                    recv_name = r_name_tok.value
                    self.expect("RP", ")")

                m_name_tok = self.peek(); self.expect("IDENT", "method name")
                params = self.parse_param_list()
                ret_type = self.parse_return_type()
                body = self.block_required()
                return MethodDef(recv_name, recv_type, m_name_tok.value, params, ret_type, body)

            # обычная функция
            name_tok = self.peek(); self.expect("IDENT", "function name")
            params = self.parse_param_list()
            ret_type = self.parse_return_type()
            body = self.block_required()
            return FuncDef(name_tok.value, params, ret_type, body)

        # ---- assignment or expr ; ----
        save_i = self.i
        lhs = self.expression()  # парсим левую часть полностью (включая .field, (*p).field, вызовы нам не подойдут)
        if isinstance(lhs, (Var, FieldAccess)) or (isinstance(lhs, Unary) and lhs.op == '*'):
            if self.match("ASSIGN2") or self.match("ASSIGN"):
                val = self.expression()
                self.expect("SEMI")
                return Assign(lhs, val)
        # не присваивание → откатываемся и парсим просто выражение;
        self.i = save_i
        e = self.expression()
        self.expect("SEMI")
        return ExprStmt(e)

    # ---------------- helpers ----------------
    def parse_param_list(self):
        self.expect("LP", "(")
        params = []
        if not self.match("RP"):
            while True:
                # допускаем: name : type    ИЛИ    ARG type name
                if self.match("ARG"):
                    p_type = self.parse_type()
                    p_name = self.peek(); self.expect("IDENT", "param name")
                else:
                    p_name = self.peek(); self.expect("IDENT", "param name")
                    self.expect("COLON", ":")
                    p_type = self.parse_type()
                params.append((p_name.value, p_type))
                if self.match("RP"): break
                self.expect("COMMA", ",")
        return params

    def parse_return_type(self):
        if self.match("COLON"):
            return self.parse_type()
        self.expect("RET", "expected ':' or 'возвр'")
        return self.parse_type()

    def block_required(self) -> Block:
        self.expect("LB", "{")
        body = []
        while not self.match("RB"):
            body.append(self.statement())
        return Block(body)

    def lvalue(self) -> Expr:
        if self.match("STAR"):
            return Unary('*', self.expression())
        t = self.peek()
        self.expect("IDENT", "lvalue ident")
        return Var(t.value)

    def parse_type(self) -> Type:
        if self.match("STAR"):  return PtrType(self.parse_type())
        if self.match("INT"):   return INT
        if self.match("BOOL"):  return BOOL
        if self.match("FLOAT"): return FLOAT
        if self.match("STRING"): return STRING
        if self.match("VOID"):  return VOID
        if self.peek().kind == "IDENT":
            t = self.peek(); self.match("IDENT")
            return TypeName(t.value)
        raise SyntaxError("type?")

    # ---------------- expressions ----------------
    def expression(self):
        return self.equality()

    def equality(self):
        expr = self.comparison()
        while self.match("EQ", "NE"):
            op = '==' if self.toks[self.i - 1].kind == 'EQ' else '!='
            expr = Binary(expr, op, self.comparison())
        return expr

    def comparison(self):
        expr = self.term()
        while self.match("LT", "LE", "GT", "GE"):
            m = self.toks[self.i - 1].kind
            op = {'LT': '<', 'LE': '<=', 'GT': '>', 'GE': '>='}[m]
            expr = Binary(expr, op, self.term())
        return expr

    def term(self):
        expr = self.factor()
        while self.match("PLUS", "MINUS"):
            op = '+' if self.toks[self.i - 1].kind == 'PLUS' else '-'
            expr = Binary(expr, op, self.factor())
        return expr

    def factor(self):
        expr = self.unary()
        while self.match("STAR", "SLASH", "PERCENT"):
            k = self.toks[self.i - 1].kind
            op = {'STAR': '*', 'SLASH': '/', 'PERCENT': '%'}[k]
            expr = Binary(expr, op, self.unary())
        return expr

    def unary(self):
        if self.match("MINUS"): return Unary('-', self.unary())
        if self.match("AMP"):   return Unary('&', self.unary_var())
        if self.match("STAR"):  return Unary('*', self.unary())
        if self.match("BANG"):  return Unary('!', self.unary())
        return self.primary()

    def unary_var(self):
        t = self.peek(); self.expect("IDENT", "ident after &")
        return Var(t.value)

    def postfix(self, expr: Expr) -> Expr:
        while True:
            if self.match("DOT"):
                m = self.peek(); self.expect("IDENT", "field/method name")
                # метод?
                if self.match("LP"):
                    args = []
                    if not self.match("RP"):
                        while True:
                            args.append(self.expression())
                            if self.match("RP"): break
                            self.expect("COMMA", ",")
                    expr = MethodCall(expr, m.value, args)
                    continue
                # поле
                expr = FieldAccess(expr, m.value)
                continue
            break
        return expr

    def primary(self):
        t = self.peek()

        if self.match("NUMBER"):
            lex = t.value
            expr = FloatLit(float(lex)) if '.' in lex else Num(int(lex))
            return self.postfix(expr)

        if self.match("STRING"):
            return self.postfix(StrLit(t.value))

        if self.match("TRUE"):
            return self.postfix(BoolLit(True))

        if self.match("FALSE"):
            return self.postfix(BoolLit(False))

        if self.match("NULL"):
            return self.postfix(NullLit())

        if self.match("NEW"):
            typ = self.parse_type()
            cnt = None
            if self.match("LBRACK"):  # '['
                cnt = self.expression()
                self.expect("RBRACK", "]")  # ']'
            return self.postfix(NewExpr(typ, cnt))

        if self.match("IDENT"):
            name = t.value
            # вызов функции: ident(...)
            if self.match("LP"):
                args = []
                if not self.match("RP"):
                    while True:
                        args.append(self.expression())
                        if self.match("RP"): break
                        self.expect("COMMA", ",")
                expr = Call(name, args)
                return self.postfix(expr)
            # просто переменная
            return self.postfix(Var(name))

        if self.match("LP"):
            e = self.expression()
            self.expect("RP", ")")
            return self.postfix(e)

        raise SyntaxError("expr?")

    def _encode_type(self, t: Type) -> str:
        if isinstance(t, IntType): return "#int"
        if isinstance(t, BoolType): return "#bool"
        if isinstance(t, PtrType): return "#*" + self._encode_type(t.inner)
        raise RuntimeError("bad type")

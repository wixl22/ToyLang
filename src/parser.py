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
            got = self.peek()
            where = f" at line {got.line}, col {got.col}"
            raise SyntaxError((msg or f"expected {kind} got {got.kind}") + where)

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
                tok_ret = self.toks[self.i - 1]
                return self._with_pos(VarDecl(name_tok.value, typ, init), tok_ret)
            else:
                # новая форма: создПер type name [:=|= expr];
                typ = self.parse_type()
                name_tok = self.peek(); self.expect("IDENT", "var name")
                init = None
                if self.match("ASSIGN2") or self.match("ASSIGN"):
                    init = self.expression()
                self.expect("SEMI", ";")
                tok_ret = self.toks[self.i - 1]
                return self._with_pos(VarDecl(name_tok.value, typ, init), tok_ret)

        # ---- if / если ----
        if self.match("IF"):
            tok_if = self.toks[self.i - 1]
            self.expect("LP", "(")
            cond = self.expression()
            self.expect("RP", ")")
            then = self.block_required()
            other = None
            if self.match("ELSE"):
                other = self.block_required()
            return self._with_pos(IfStmt(cond, then, other), tok_if)

        # ---- while / пока ----
        if self.match("WHILE"):
            tok_wh = self.toks[self.i - 1]
            self.expect("LP", "(")
            cond = self.expression()
            self.expect("RP", ")")
            body = self.block_required()
            return self._with_pos(WhileStmt(cond, body), tok_wh)

        # ---- break / прервать ----
        if self.match("BREAK"):
            tok_ret = self.toks[self.i - 1]
            self.expect("SEMI", ";")
            return self._with_pos(BreakStmt(), tok_ret)

        # ---- continue / продолжить ----
        if self.match("CONTINUE"):
            tok_ret = self.toks[self.i - 1]
            self.expect("SEMI", ";")
            return self._with_pos(ContinueStmt(), tok_ret)

        # ---- return / вернуть ----
        if self.match("RETURN"):
            expr = None
            if not self.match("SEMI"):
                expr = self.expression()
                self.expect("SEMI", ";")
            tok_ret = self.toks[self.i - 1]
            return self._with_pos(ReturnStmt(expr), tok_ret)

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
            tok_ret = self.toks[self.i - 1]
            return self._with_pos(StructDef(name_tok.value, fields), tok_ret)

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
                tok_ret = self.toks[self.i - 1]
                return self._with_pos(MethodDef(recv_name, recv_type, m_name_tok.value, params, ret_type, body), tok_ret)

            # обычная функция
            name_tok = self.peek(); self.expect("IDENT", "function name")
            params = self.parse_param_list()
            ret_type = self.parse_return_type()
            body = self.block_required()
            tok_ret = self.toks[self.i-1]
            return self._with_pos(FuncDef(name_tok.value, params, ret_type, body), tok_ret)

        # ---- assignment or expr ; ----
        save_i = self.i
        lhs = self.expression()
        if isinstance(lhs, (Var, FieldAccess)) or (isinstance(lhs, Unary) and lhs.op == '*'):
            if self.match("ASSIGN2") or self.match("ASSIGN"):
                assign_tok = getattr(lhs, "pos", self.toks[self.i - 1])
                val = self.expression()
                self.expect("SEMI")
                return self._with_pos(Assign(lhs, val), assign_tok)
        # не присваивание → откатываемся и парсим просто выражение;
        self.i = save_i
        e = self.expression()
        self.expect("SEMI")
        tok_ret = self.toks[self.i - 1]

        return self._with_pos(ExprStmt(e), tok_ret)

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
        return self.logical_or()

    def logical_or(self):
        expr = self.logical_and()
        while self.match("OR"):
            op = '||'
            tok = getattr(expr, "pos", self.toks[self.i - 1])  # если есть
            expr = self._with_pos(Binary(expr, op, self.logical_and()), tok)
        return expr

    def logical_and(self):
        expr = self.equality()
        while self.match("AND"):
            op = '&&'
            tok = getattr(expr, "pos", self.toks[self.i - 1])  # если есть
            expr = self._with_pos(Binary(expr, op, self.equality()), tok)
        return expr

    def equality(self):
        expr = self.comparison()
        while self.match("EQ", "NE"):
            op = '==' if self.toks[self.i - 1].kind == 'EQ' else '!='
            tok = getattr(expr, "pos", self.toks[self.i - 1])  # если есть
            expr = self._with_pos(Binary(expr, op, self.comparison()), tok)
        return expr

    def comparison(self):
        expr = self.term()
        while self.match("LT", "LE", "GT", "GE"):
            m = self.toks[self.i - 1].kind
            op = {'LT': '<', 'LE': '<=', 'GT': '>', 'GE': '>='}[m]
            tok = getattr(expr, "pos", self.toks[self.i - 1])  # если есть
            expr = self._with_pos(Binary(expr, op, self.term()), tok)
        return expr

    def term(self):
        expr = self.factor()
        while self.match("PLUS", "MINUS"):
            op = '+' if self.toks[self.i - 1].kind == 'PLUS' else '-'
            tok = getattr(expr, "pos", self.toks[self.i - 1])  # если есть
            expr = self._with_pos(Binary(expr, op, self.factor()), tok)
        return expr

    def factor(self):
        expr = self.unary()
        while self.match("STAR", "SLASH", "PERCENT"):
            k = self.toks[self.i - 1].kind
            op = {'STAR': '*', 'SLASH': '/', 'PERCENT': '%'}[k]
            tok = getattr(expr, "pos", self.toks[self.i - 1])  # если есть
            expr = self._with_pos(Binary(expr, op, self.unary()), tok)
        return expr

    def unary(self):
        if self.match("MINUS"):
            tok = self.toks[self.i - 1]
            return self._with_pos(Unary('-', self.unary()), tok)
        if self.match("AMP"):
            tok = self.toks[self.i - 1]
            return self._with_pos(Unary('&', self.unary_var()), tok)
        if self.match("STAR"):
            tok = self.toks[self.i - 1]
            return self._with_pos(Unary('*', self.unary()), tok)
        if self.match("BANG"):
            tok = self.toks[self.i - 1]
            return self._with_pos(Unary('!', self.unary()), tok)
        return self.primary()

    def unary_var(self):
        t = self.peek(); self.expect("IDENT", "ident after &")
        return Var(t.value)

    def postfix(self, expr: Expr) -> Expr:
        while True:
            if self.match("DOT"):
                m = self.peek(); self.expect("IDENT", "field/method name")
                tok_name = m  # токен имени после DOT
                # метод?
                if self.match("LP"):
                    args = []
                    if not self.match("RP"):
                        while True:
                            args.append(self.expression())
                            if self.match("RP"): break
                            self.expect("COMMA", ",")
                    expr = self._with_pos(MethodCall(expr, m.value, args), tok_name)
                    continue
                # поле
                expr = self._with_pos(FieldAccess(expr, m.value), tok_name)
                continue
            break
        return expr

    def _with_pos(self, node, tok):
        # присваиваем .pos = (line, col)
        try:
            node.pos = (tok.line, tok.col)
        except Exception:
            pass
        return node

    def primary(self):
        t = self.peek()
        if self.match("NUMBER"):
            lex = t.value
            node = FloatLit(float(lex)) if '.' in lex else Num(int(lex))
            return self.postfix(self._with_pos(node, t))

        if self.match("STRING"):
            return self.postfix(self._with_pos(StrLit(t.value), t))

        if self.match("TRUE"):
            return self.postfix(self._with_pos(BoolLit(True), t))

        if self.match("FALSE"):
            return self.postfix(self._with_pos(BoolLit(False), t))

        if self.match("NULL"):
            return self.postfix(self._with_pos(NullLit(), t))

        if self.match("NEW"):
            typ = self.parse_type();
            cnt = None
            if self.match("LBRACK"):
                cnt = self.expression();
                self.expect("RBRACK", "]")
            return self.postfix(self._with_pos(NewExpr(typ, cnt), t))

        if self.match("IDENT"):
            name = t.value
            if self.match("LP"):
                args = []
                if not self.match("RP"):
                    while True:
                        args.append(self.expression())
                        if self.match("RP"): break
                        self.expect("COMMA", ",")
                return self.postfix(self._with_pos(Call(name, args), t))
            return self.postfix(self._with_pos(Var(name), t))

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

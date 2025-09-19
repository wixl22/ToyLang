from __future__ import annotations

from lang_types import Type, IntType, BoolType, PtrType, INT, BOOL, VOID, FLOAT, STRING, TypeName
from lang_ast import Expr, Num, BoolLit, NullLit, Var, Unary, Binary, Assign, VarDecl, ExprStmt, Block, IfStmt, \
    WhileStmt, \
    BreakStmt, ContinueStmt, NewExpr, ImportStmt, Call, ReturnStmt, FuncDef, StrLit, FloatLit, StructDef, MethodDef, \
    FieldAccess, MethodCall


class Parser:
    def __init__(self, toks):
        self.toks = toks
        self.i = 0

    def peek(self):
        return self.toks[self.i]

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

    # Разбираем как отдельные стейтменты с точкой с запятой.
    def statement(self):
        # let decl
        if self.match("LET"):
            name_tok = self.peek();
            self.expect("IDENT", "ident")
            self.expect("COLON", ":")
            typ = self.parse_type()
            init = None
            if self.match("ASSIGN"):
                init = self.expression()
            self.expect("SEMI", ";")
            return VarDecl(name_tok.value, typ, init)
        if self.match("IMPORT"):
            t = self.peek()
            self.expect("STRING", "expected string path")
            path = t.value
            self.expect("SEMI", ";")
            return ImportStmt(path)
        if self.match("IF"):
            self.expect("LP", "(")
            cond = self.expression()
            self.expect("RP", ")")
            then = self.block_required()
            other = None
            if self.match("ELSE"):
                other = self.block_required()
            return IfStmt(cond, then, other)
        if self.match("FUNC"):
            if self.match("LP"):  # метод: func (p:*Type) Name(...)
                r_name_tok = self.peek();
                self.expect("IDENT", "recv name")
                self.expect("COLON", ":")
                r_type = self.parse_type()
                self.expect("RP", ")")

                m_name_tok = self.peek();
                self.expect("IDENT", "method name")
                self.expect("LP", "(")
                params = []
                if not self.match("RP"):
                    while True:
                        p_name = self.peek();
                        self.expect("IDENT", "param name")
                        self.expect("COLON", ":")
                        p_type = self.parse_type()
                        params.append((p_name.value, p_type))
                        if self.match("RP"): break
                        self.expect("COMMA", ",")
                self.expect("COLON", ":")
                ret_type = self.parse_type()
                body = self.block_required()
                return MethodDef(r_name_tok.value, r_type, m_name_tok.value, params, ret_type, body)
            else:
                name_tok = self.peek();
                self.expect("IDENT", "function name")
                self.expect("LP", "(")
                params = []
                if not self.match("RP"):
                    while True:
                        p_name = self.peek();
                        self.expect("IDENT", "param name")
                        self.expect("COLON", ":")
                        p_type = self.parse_type()
                        params.append((p_name.value, p_type))
                        if self.match("RP"): break
                        self.expect("COMMA", ",")
                self.expect("COLON", ":")
                ret_type = self.parse_type()
                body = self.block_required()
                return FuncDef(name_tok.value, params, ret_type, body)
        if self.match("RETURN"):
            expr = None
            # допускаем 'return;' и 'return expr;'
            if not self.match("SEMI"):
                expr = self.expression()
                self.expect("SEMI", ";")
            return ReturnStmt(expr)
        # while (...) { ... }
        if self.match("WHILE"):
            self.expect("LP", "(")
            cond = self.expression()
            self.expect("RP", ")")
            body = self.block_required()
            return WhileStmt(cond, body)

        if self.match("STRUCT"):
            name_tok = self.peek()
            self.expect("IDENT", "struct name")
            self.expect("LB", "{")
            fields = []
            while not self.match("RB"):
                f_name = self.peek();
                self.expect("IDENT", "field name")
                self.expect("COLON", ":")
                f_type = self.parse_type()
                self.expect("SEMI", ";")
                fields.append((f_name.value, f_type))
            return StructDef(name_tok.value, fields)

        # ----- HERE: break; -----
        if self.match("BREAK"):
            self.expect("SEMI", ";")
            return BreakStmt()

        # ----- HERE: continue; -----
        if self.match("CONTINUE"):
            self.expect("SEMI", ";")
            return ContinueStmt()

        # assignment or expr;
        save_i = self.i
        lhs = self.expression()  # распарсим полностью левую часть, включая .field и (*p).field
        if isinstance(lhs, (Var, FieldAccess)) or (isinstance(lhs, Unary) and lhs.op == '*'):
            if self.match("ASSIGN"):
                val = self.expression()
                self.expect("SEMI")
                return Assign(lhs, val)
        # не присваивание — откатываемся и читаем обычное выражение;
        self.i = save_i
        e = self.expression()
        self.expect("SEMI")
        return ExprStmt(e)

    def block_required(self) -> Block:
        self.expect("LB", "{")
        body = []
        while not self.match("RB"):
            body.append(self.statement())
        return Block(body)

    def lvalue(self) -> Expr:
        if self.match("STAR"):
            return Unary('*', self.expression())
        t = self.peek();
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
            t = self.peek()
            self.match("IDENT")
            return TypeName(t.value)
        raise SyntaxError("type?")

    # ===== выражения =====
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
        t = self.peek();
        self.expect("IDENT", "ident after &")
        return Var(t.value)

    def postfix(self, expr: Expr) -> Expr:
        while True:
            if self.match("DOT"):
                m = self.peek();
                self.expect("IDENT", "field/method name")
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

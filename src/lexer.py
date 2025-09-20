from src.tokens import Token

KEYWORDS = {
    # ---- types ----
    "int": "INT",
    "bool": "BOOL",
    "float": "FLOAT",
    "string": "STRING",
    "void": "VOID",

    # ---- statements / control ----
    "func": "FUNC",
    "return": "RETURN",
    "struct": "STRUCT",
    "if": "IF",
    "else": "ELSE",
    "while": "WHILE",
    "break": "BREAK",
    "continue": "CONTINUE",
    "import": "IMPORT",
    "let": "LET",
    "new": "NEW",

    # ---- russian aliases ----
    "создфунк": "FUNC",
    "вернуть": "RETURN",
    "создструк": "STRUCT",
    "если": "IF",
    "или": "ELSE",
    "пока": "WHILE",
    "прервать": "BREAK",
    "продолжить": "CONTINUE",
    "импорт": "IMPORT",
    "создпер": "LET",
    "нов": "NEW",

    # ---- extra markers (syntax sugar) ----
    # return type after parameters: ': Type' OR 'возвр Type'
    "возвр": "RET",
    # parameter: 'name: type' OR 'арг type name'
    "арг": "ARG",
    # struct field: 'name: type;' OR 'атр type name;'
    "атр": "ATTR",
    # method: 'func (влад *Type p) Name(...) ...'
    "влад": "RECV",

    # ---- literals (bool) ----
    "true": "TRUE",
    "false": "FALSE",
    "null": "NULL",
}


class Lexer:
    def __init__(self, src: str, filename: str = "<stdin>"):
        self.src = src
        self.i = 0
        self.n = len(src)
        self.line = 1
        self.col = 1
        self.filename = filename

    def peek(self, k=0):
        j = self.i + k
        return self.src[j] if j < self.n else '\0'

    def advance(self):
        ch = self.peek()
        if ch == '\0':
            return ch
        self.i += 1
        if ch == '\n':
            self.line += 1
            self.col = 1
        else:
            self.col += 1
        return ch

    def _tok(self, kind: str, value: str, start_i: int, start_line: int, start_col: int):
        return Token(kind, value, start_i, start_line, start_col)

    def skip_ws(self):
        while True:
            # Skip whitespace/tabs/newlines
            while self.peek().isspace():
                self.advance()

            # // single-line comment until end of line or EOF
            if self.peek() == '/' and self.peek(1) == '/':
                while self.peek() not in ['\n', '\r', '\0']:
                    self.advance()
                # consume newline if present
                if self.peek() in ['\n', '\r']:
                    self.advance()
                continue

            # /* ... */ multi-line comment
            if self.peek() == '/' and self.peek(1) == '*':
                self.advance()
                self.advance()  # consume '/*'
                while not (self.peek() == '*' and self.peek(1) == '/'):
                    if self.peek() == '\0':
                        raise SyntaxError('Unterminated block comment')
                    self.advance()
                self.advance()
                self.advance()  # consume '*/'
                continue

            break

    def number(self):
        start_i, sl, sc = self.i, self.line, self.col
        s = ""
        has_dot = False
        while True:
            ch = self.peek()
            if ch.isdigit():
                s += self.advance()
                continue
            if ch == '.' and not has_dot and self.peek(1).isdigit():
                has_dot = True
                s += self.advance()
                continue
            break
        return self._tok("NUMBER", s, start_i, sl, sc)

    def ident_or_kw(self):
        start_i, sl, sc = self.i, self.line, self.col
        s = ""
        while self.peek().isalnum() or self.peek() == '_' or ('\u0400' <= self.peek() <= '\u04FF'):
            s += self.advance()
        low = s.lower()
        if low in KEYWORDS:
            return self._tok(KEYWORDS[low], s, start_i, sl, sc)
        return self._tok("IDENT", s, start_i, sl, sc)

    def tokens(self):
        two = {
            '==': 'EQ',
            '!=': 'NE',
            '<=': 'LE',
            '>=': 'GE',
            ':=': 'ASSIGN2',
            '&&': 'AND',
            '||': 'OR'
        }
        one = {
            '+': 'PLUS',
            '-': 'MINUS',
            '*': 'STAR',
            '/': 'SLASH',
            '%': 'PERCENT',
            '=': 'ASSIGN',
            ';': 'SEMI',
            '&': 'AMP',
            ':': 'COLON',
            '(': 'LP',
            ')': 'RP',
            '{': 'LB',
            '}': 'RB',
            '<': 'LT',
            '>': 'GT',
            '!': 'BANG',
            '[': 'LBRACK',
            ']': 'RBRACK',
            '.': 'DOT',
            ',': 'COMMA'
        }
        toks = []
        while True:
            self.skip_ws()
            ch = self.peek()

            start_i, sl, sc = self.i, self.line, self.col

            if ch == '\0':
                toks.append(self._tok("EOF", "", start_i, sl, sc))
                break

            pair = ch + self.peek(1)
            if pair in two:
                t = self._tok(two[pair], pair, start_i, sl, sc)
                self.advance()
                self.advance()
                toks.append(t)
                continue

            if ch.isdigit():
                toks.append(self.number())
                continue

            if ch.isalpha() or ch == '_':
                toks.append(self.ident_or_kw())
                continue

            if ch in one:
                toks.append(self._tok(one[ch], ch, start_i, sl, sc))
                self.advance()
                continue

            if ch == '"':
                toks.append(self.string())
                continue

            raise SyntaxError(f"bad char {ch!r} at {self.i}")
        return toks

    def string(self):
        # Expect current character to be '"'
        start, sl, sc = self.i, self.line, self.col
        if self.peek() != '"':
            raise SyntaxError('internal: string() called not at a quote')
        self.advance()  # consume opening quote

        out = []
        while True:
            ch = self.peek()

            # End of file without closing quote
            if ch == '\0':
                raise SyntaxError("unterminated string literal")

            # Disallow raw newlines inside strings
            if ch == '\n' or ch == '\r':
                raise SyntaxError("unterminated string literal (newline in string)")

            # Handle escape sequences
            if ch == '\\':
                self.advance()  # consume backslash
                esc = self.peek()
                if esc == '\0':
                    raise SyntaxError("unterminated string escape")
                self.advance()
                maps = {
                    'n': '\n',
                    'r': '\r',
                    't': '\t',
                    '"': '"',
                    '\\': '\\',
                    '0': '\0',
                }
                out.append(maps.get(esc, esc))  # treat unknown \x as x
                continue

            # Closing quote - finish token
            if ch == '"':
                self.advance()
                break

            # Regular character
            out.append(self.advance())

        return self._tok("STRING", "".join(out), start, sl, sc)

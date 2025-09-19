from src.keywords import KEYWORDS
from src.tokens import Token


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
            # Пробелы/табы/переводы строк
            while self.peek().isspace():
                self.advance()

            # // до конца строки или EOF
            if self.peek() == '/' and self.peek(1) == '/':
                while self.peek() not in ['\n', '\r', '\0']:
                    self.advance()
                # съедим перевод строки, если есть
                if self.peek() in ['\n', '\r']:
                    self.advance()
                continue

            # /* ... */ многострочный
            if self.peek() == '/' and self.peek(1) == '*':
                self.advance();
                self.advance()  # съели '/*'
                while not (self.peek() == '*' and self.peek(1) == '/'):
                    if self.peek() == '\0':
                        raise SyntaxError('Unterminated block comment')
                    self.advance()
                self.advance();
                self.advance()  # съели '*/'
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
        two = {'==': 'EQ', '!=': 'NE', '<=': 'LE', '>=': 'GE', ':=': 'ASSIGN2',
               '&&': 'AND', '||': 'OR'}
        one = {
            '+': 'PLUS', '-': 'MINUS', '*': 'STAR', '/': 'SLASH', '%': 'PERCENT',
            '=': 'ASSIGN', ';': 'SEMI', '&': 'AMP', ':': 'COLON',
            '(': 'LP', ')': 'RP', '{': 'LB', '}': 'RB', '<': 'LT', '>': 'GT', '!': 'BANG',
            '[': 'LBRACK',
            ']': 'RBRACK',
            '.': 'DOT',
            ',': 'COMMA',
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
        # Ожидаем, что current == '"'
        start, sl, sc = self.i, self.line, self.col
        if self.peek() != '"':
            raise SyntaxError('internal: string() called not at a quote')
        self.advance()  # съели открывающую "

        out = []
        while True:
            ch = self.peek()

            # Конец файла, так и не встретили закрывающую кавычку
            if ch == '\0':
                raise SyntaxError("unterminated string literal")

            # Запрещаем сырой перевод строки внутри строки
            if ch == '\n' or ch == '\r':
                raise SyntaxError("unterminated string literal (newline in string)")

            # Экранирования
            if ch == '\\':
                self.advance()  # съели \
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
                out.append(maps.get(esc, esc))  # неизвестное \x трактуем как x
                continue

            # Закрывающая кавычка — завершаем токен
            if ch == '"':
                self.advance()
                break

            # Обычный символ
            out.append(self.advance())

        return self._tok("STRING", "".join(out), start, sl, sc)

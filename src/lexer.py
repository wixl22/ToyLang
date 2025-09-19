from src.keywords import KEYWORDS
from src.tokens import Token


class Lexer:
    def __init__(self, src: str):
        self.src = src
        self.i = 0
        self.n = len(src)

    def peek(self, k=0):
        j = self.i + k
        return self.src[j] if j < self.n else '\0'  # явный EOF

    def advance(self):
        ch = self.peek()
        if ch != '\0':
            self.i += 1
        return ch

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
        start = self.i
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
        return Token("NUMBER", s, start)

    def ident_or_kw(self):
        start = self.i
        s = ""
        while self.peek().isalnum() or self.peek() == '_' or ('\u0400' <= self.peek() <= '\u04FF'):
            s += self.advance()
        low = s.lower()
        if low in KEYWORDS: return Token(KEYWORDS[low], s, start)
        return Token("IDENT", s, start)

    def tokens(self):
        two = {'==': 'EQ', '!=': 'NE', '<=': 'LE', '>=': 'GE', ':=': 'ASSIGN2'}
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
            if ch == '\0':
                toks.append(Token("EOF", "", self.i))
                break

            pair = ch + self.peek(1)
            if pair in two:
                toks.append(Token(two[pair], pair, self.i))
                self.advance();
                self.advance()
                continue

            if ch.isdigit():
                toks.append(self.number())
                continue

            if ch.isalpha() or ch == '_':
                toks.append(self.ident_or_kw())
                continue

            if ch in one:
                toks.append(Token(one[ch], ch, self.i))
                self.advance()
                continue

            if ch == '"':
                toks.append(self.string())
                continue

            raise SyntaxError(f"bad char {ch!r} at {self.i}")
        return toks

    def string(self):
        # Ожидаем, что current == '"'
        start = self.i
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

        return Token("STRING", "".join(out), start)

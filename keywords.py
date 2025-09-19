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
    # ретурн-тип после параметров:  ': Type' ИЛИ 'возвр Type'
    "возвр": "RET",
    # параметр: 'name: type' ИЛИ 'арг type name'
    "арг": "ARG",
    # поле в struct: 'name: type;' ИЛИ 'атр type name;'
    "атр": "ATTR",
    # метод: 'func (влад *Type p) Name(...) ...'
    "влад": "RECV",

    # ---- literals (bool) ----
    "true":  "TRUE",
    "false": "FALSE",
    "null":  "NULL",
}
(KEYWORD class)
(ID "Stack")
(PUNCT "(")
(ID "Container")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD def)
(ID "__init__")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(ID "super")
(PUNCT "(")
(ID "Stack")
(PUNCT ",")
(ID "self")
(PUNCT ")")
(PUNCT ".")
(ID "__init__")
(PUNCT "(")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(KEYWORD def)
(ID "getTop")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(ID "getTop")
(PUNCT "=")
(ID "abstractmethod")
(PUNCT "(")
(ID "getTop")
(PUNCT ")")
(NEWLINE)
(ID "top")
(PUNCT "=")
(ID "property")
(PUNCT "(")
(ID "fget")
(PUNCT "=")
(KEYWORD lambda)
(ID "self")
(PUNCT ":")
(ID "self")
(PUNCT ".")
(ID "getTop")
(PUNCT "(")
(PUNCT ")")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "push")
(PUNCT "(")
(ID "self")
(PUNCT ",")
(ID "obj")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(ID "push")
(PUNCT "=")
(ID "abstractmethod")
(PUNCT "(")
(ID "push")
(PUNCT ")")
(NEWLINE)
(KEYWORD def)
(ID "pop")
(PUNCT "(")
(ID "self")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD pass)
(NEWLINE)
(DEDENT)
(ID "pop")
(PUNCT "=")
(ID "abstractmethod")
(PUNCT "(")
(ID "pop")
(PUNCT ")")
(NEWLINE)
(DEDENT)
(ENDMARKER)

# Phillip Mates
# u0284736

PACKAGE = package
VERSION = ` date "+%Y.%m%d%" `
RELEASE_DIR  = ..
RELEASE_FILE = $(PACKAGE)-$(VERSION)

# build parser
all:
	ghc -c -O DerRegex.hs
	ghc -c -O LexerRegex.hs
	ghc -c -O PythonLexer.hs
	ghc -main-is PythonParser -o pyparser PythonParser.hs PythonLexer.o DerRegex.o LexerRegex.o

# build lexer
pylexer:
	rm -f PythonLexer.o
	ghc -c -O DerRegex.hs
	ghc -c -O LexerRegex.hs
	ghc -main-is PythonLexer -o pylexer PythonLexer.hs DerRegex.o LexerRegex.o
	rm -f PythonLexer.o

# target: help - Display callable targets.
help:
	egrep "^# target:" [Mm]akefile

# parse program input from STDIN
run:
	./pyparser

# parse lexed input from STDIN
parse:
	./pyparser -l

# lex input from STDIN
lex: pylexer
	./pylexer

# target: clean - Remove build files
clean:
	rm -f *.o
	rm -f *.hi
	rm -f pyparser
	rm -f pylexer

# Phillip Mates
# u0284736

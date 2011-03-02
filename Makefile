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
	ghc -c -O PythonParser.hs
	ghc -o pyparser --make Main.hs -package data-reify

# build lexer
pylexer:
	rm -f PythonLexer.o
	ghc -c -O DerRegex.hs
	ghc -c -O LexerRegex.hs
	ghc -c -O PythonLexer.hs
	ghc -o pylexer --make Main.hs
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

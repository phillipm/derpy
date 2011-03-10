# Phillip Mates
# u0284736

PACKAGE = package
VERSION = ` date "+%Y.%m%d%" `
RELEASE_DIR  = ..
RELEASE_FILE = $(PACKAGE)-$(VERSION)

# build parser
all:
	cabal configure
	cabal build

# build parser
pyparser:
	cabal configure
	cabal build

# build lexer
pylexer:
	cabal configure
	cabal build

# target: help - Display callable targets.
help:
	egrep "^# target:" [Mm]akefile

# parse program input from STDIN
run:
	./dist/build/pyparser/pyparser

# parse lexed input from STDIN
parse:
	./dist/build/pyparser/pyparser -l

# lex input from STDIN
lex: pylexer
	./dist/build/pylexer/pylexer

# Phillip Mates
# u0284736

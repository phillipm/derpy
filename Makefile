# Phillip Mates
# u0284736

# build parser
all: compile

compile:
	@cabal configure > /dev/null
	@cabal build > /dev/null

# target: help - Display callable targets.
help:
	egrep "^# target:" [Mm]akefile

# parse program input from STDIN
run:
	./dist/build/pyparser/pyparser

# parse lexed input from STDIN
parse:
	@./dist/build/pyparser/pyparser

# lex input from STDIN
lex:
	@./dist/build/pylexer/pylexer

clean:
	cabal clean

sdiff: sdiff.rkt
	raco exe sdiff.rkt

test: compile sdiff
	for i in tests/*.py; do make -s parse < $$i > $$i.out; ./sdiff $$i.out $$i.expected; done

# Phillip Mates
# u0284736

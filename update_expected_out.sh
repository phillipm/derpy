#/bin/zsh
for i in tests/*.py; do make lex < $i | racket ../pyparse/pyparse_rkt.zo > $i.expected; done

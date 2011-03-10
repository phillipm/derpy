
-- Haskell imports
import System.Environment
import System.IO (hGetContents, hGetLine, stdin)

import qualified PythonLexer as Pylex
import PythonParser

import Text.Derp

{-
<funcdef> ::= (def (<NAME> <NAME>*) <suite>)
(funcdef       ($--> (seq* "def" NAME parameters ":" suite)
                    `(def (,($ 1) ,@($ 2)) ,($ 4))))
-}

ptest :: Parser String
ptest = ter "def" <~> ter "id" <~> parameters <~> ter ":" <~> suite ==> (\(s1,(s2,(s3,(s4,s5)))) -> "(" ++ s1 ++ " (" ++ s2 ++ " " ++ s3 ++ ") (" ++ s5 ++ "))")

parameters :: Parser String
parameters = ter "(" <~> paramlist <~> ter ")" ==> (\(s1,(s2,s3)) -> s2)

paramlist :: Parser String
paramlist = eps "" <|> ter "id" <~> paramlistRep ==> (\(s1,s2) -> s1 ++ " " ++ s2) <|> ter ","

paramlistRep :: Parser String
paramlistRep = p
  where p = eps "" <|> ter "," <~> ter "id" <~> p ==> (\(s1,(s2,s3)) -> s2 ++ " " )

suite :: Parser String
suite = simple_stmt <|> ter "newline" <~> ter "indent" <~> stmtRep <~> ter "dedent" ==> (\(nw,(ind,(stR,ddnt))) -> "expr " ++ stR)

stmtRep :: Parser String
stmtRep = stmt <~> p ==> (\_->"") -- TODO: fix reduction
  where p = eps "" <|> stmt <|> stmt <~> p ==> (\(s1,s2) -> s1 ++ " " ++ s2)


inp :: [Token]
inp = map (\x -> Token x x) $ words "( p )"
inp2 :: [Token]
inp2 = [Token "def" "def", Token "id" "test", Token "(" "(", Token "id" "p1", Token "," ",", Token "id" "p2", Token ")" ")", Token ":" ":"]
-- TODO: take in -l flag to specify between prelexed input
main :: IO ()
--main = (hGetContents stdin) >>= do putStrLn . show . makeTokens . Pylex.lexInput
main = do putStrLn . show $ runParse ptest inp2

-- convert PyLex tokens to Derp Tokens
makeTokens :: [Pylex.Token] -> [Token]
makeTokens (x:xs) = (toToken x) : makeTokens xs
makeTokens [] = []

-- convert a PyLex token to a Derp Token
toToken :: Pylex.Token-> Token
toToken (Pylex.Id s) = Token "id" s
toToken (Pylex.Lit s) = Token "lit" s
toToken (Pylex.Keyword s) = Token "lit" s
toToken (Pylex.Punct s) = Token "punct" s
toToken (Pylex.Complex s) = Token "complex" s
toToken (Pylex.OctHexBin s) = Token "octhexbin" s
toToken (Pylex.StringLit s) = Token "string" s
toToken (Pylex.Error s) = Token "error" s
toToken Pylex.Newline = Token "newline" "newline"
toToken Pylex.Indent = Token "indent" "indent"
toToken Pylex.Dedent = Token "dedent" "dedent"
toToken Pylex.Endmarker = Token "endmarker" "endmarker"
toToken Pylex.LineCont = Token "linecont" "linecont"

program :: Parser String
program = stmt ==> (\x -> "(program " ++ x ++ ")")

stmt :: Parser String
stmt = simple_stmt <|> compound_stmt

compound_stmt :: Parser String
compound_stmt = if_stmt <|> while_stmt <|> for_stmt <|> try_stmt <|> funcdef

simple_stmt :: Parser String
simple_stmt = small_stmt <|> small_stmtRep <|> ter ";" <~> ter "newline" ==> (\_->"")

small_stmtRep :: Parser String
small_stmtRep = p
  where p = eps "" <|> ter ";" <~> small_stmt <~> p ==> (\(s1,(s2,s3)) -> "(" ++ s2 ++ " " ++ s3 ++ ")")

small_stmt :: Parser String
small_stmt = expr_stmt <|> del_stmt <|> pass_stmt <|> flow_stmt <|> global_stmt <|> nonlocal_stmt <|> assert_stmt

expr_stmt :: Parser String
expr_stmt = undefined

star_expr :: Parser String
star_expr = undefined

del_stmt :: Parser String
del_stmt = ter "del" <~> star_expr ==> (\_->"") -- FIXME: reduction

pass_stmt :: Parser String
pass_stmt = ter "pass" ==> (\_->"") -- FIXME: reduction

flow_stmt :: Parser String
flow_stmt = break_stmt <|> continue_stmt <|> return_stmt <|> raise_stmt

break_stmt :: Parser String
break_stmt = ter "break" ==> (\_->"") -- FIXME: reduction

continue_stmt :: Parser String
continue_stmt = ter "continue" ==> (\_->"") -- FIXME: reduction

return_stmt :: Parser String
return_stmt = ter "return" <|> testlist

raise_stmt :: Parser String
raise_stmt = ter "raise" <~> (eps "" <|> (test <~> (eps "" <|> ter "from" <~> test ==> (\(f,t) -> f ++ " " ++ t))==> (\_->""))) ==> (\(s1,s2) -> s1 ++ " " ++ s2) --FIXME: reduction

testlist :: Parser String
testlist = undefined

global_stmt :: Parser String
global_stmt = undefined

nonlocal_stmt :: Parser String
nonlocal_stmt = undefined

assert_stmt:: Parser String
assert_stmt= undefined

if_stmt :: Parser String
if_stmt = undefined

while_stmt :: Parser String
while_stmt = undefined

for_stmt :: Parser String
for_stmt = undefined

try_stmt :: Parser String
try_stmt = undefined

funcdef :: Parser String
funcdef = undefined -- ter "def" <~> parameters <~> ter ":" <~> suite

{-let e = ter "num" <|> e <~> ter "+" <~> e ==> (\(x1,(o,x2)) -> "(" ++ x1 ++ o ++ x2 ++ ")")-}
{-runParse e [Token "num" "1", Token "+" "+", Token "num" "3", Token "+" "+", Token "num" "5"]-}
{-

(parameters    (car (seq! "(" `(opt paramlist '()) ")")))
(paramlist     (car (seq! `(seq NAME (rep (car (seq! "," `NAME)))) (opt ","))))

(namelist  (seq NAME (rep (car (seq! "," `NAME)))))

-}


trailer :: Parser String
trailer = p
  where
    p = ter "(" <~> (eps "" <|> arglist) <~> ter ")" ==> (\(s1,(s2,s3)) -> s1 ++ s2 ++ s3 ) <|> ter "[" <~> tuple_or_test <~> ter "]" ==> (\_ -> "") -- <|> ter "." <~> NAME

arglist :: Parser String
arglist = test <~> argRep ==> (\_->"") <|> ter ","

-- just like arglist
tuple_or_test :: Parser String
tuple_or_test = test <~> argRep ==> (\_->"") <|> ter ","

argRep :: Parser String
argRep = p
  where
    p = eps "" <|> ter "," <~> test ==> (\_ -> "") <|> p

test :: Parser String
test = eps "" ==> (\_ -> "test")


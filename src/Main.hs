-- Haskell imports
import System.Environment
import System.IO (hGetContents, hGetLine, stdin)
import Debug.Trace (trace)

import qualified PythonLexer as Pylex
import PythonParser

import Text.Derp

inp :: [Token]
inp = map (\x -> Token x x) $ words "( p )"
inp2 :: [Token]
inp2 = [Token "def" "def", Token "id" "test", Token "(" "(", Token "id" "p1", Token "," ",", Token "id" "p2", Token ")" ")", Token ":" ":"]
inp3 :: [Token]
inp3 = [Token "id" "x", Token "=" "=", Token "lit" "5"]
inp4 :: [Token]
inp4 = [Token "lit" "4", Token "**" "**", Token "lit" "5"]
inp5 :: [Token]
inp5 = [Token "[" "[", Token "lit" "1", Token "," ",", Token "lit" "2", Token "," ",", Token "lit" "3", Token "]" "]"]

-- TODO: take in -l flag to specify between prelexed input
main :: IO ()
main = do input <- (hGetContents stdin)
          putStrLn . show $ runParse expr_stmt $ makeTokens . Pylex.lexInput $ input
{-
main = do putStrLn . show $ runParse expr_stmt inp3
          putStrLn . show $ runParse expr_stmt inp4
          putStrLn . show $ runParse expr_stmt inp5
-}

-- -- -- -- -- -- -- --
-- Parsing Functions
-- -- -- -- -- -- -- --
file_input :: Parser String
file_input = file_inputRep <~> ter "endmarker" ==> reduce
  where reduce (x,_) | trace ("prog") False = undefined
        reduce (x,_) = "(program " ++ x ++ ")"

file_inputRep :: Parser String
file_inputRep = p
  where p =     eps ""
            <|> ter "newline" <~> p ==> (\(_,x) -> x)
            <|> stmt <~> p ==> (\(s,x) -> catWSpace s x)

funcdef :: Parser String
funcdef = ter "def" <~> ter "id" <~> parameters <~> ter ":" <~> suite ==>
            (\(_,(fname,(params,(_,rest)))) ->
            "(def (" ++ fname ++ " " ++ params ++ ") (" ++ rest ++ "))")

parameters :: Parser String
parameters = ter "(" <~> paramlist <~> ter ")" ==> (\(s1,(s2,s3)) -> s2)

paramlist :: Parser String
paramlist =     eps ""
            <|> ter "id" <~> paramlistRep ==> (\(s1,s2) -> catWSpace s1 s2)
            <|> ter ","

paramlistRep :: Parser String
paramlistRep = p
  where p =     eps ""
            <|> ter "," <~> ter "id" <~> p ==> (\(s1,(s2,s3)) -> s2)

stmt :: Parser String
stmt = simple_stmt <|> compound_stmt

simple_stmt :: Parser String
simple_stmt = small_stmt <|> small_stmtRep <~> (eps "" <|> ter ";") <~> ter "newline" ==> (\(x,_)->x)

small_stmt :: Parser String
small_stmt =     expr_stmt <|> del_stmt <|> pass_stmt <|> flow_stmt
             <|> global_stmt <|> nonlocal_stmt <|> assert_stmt

small_stmtRep :: Parser String
small_stmtRep = p
  where p =     eps ""
            <|> ter ";" <~> small_stmt <~> p ==> (\(s1,(s2,s3)) -> "(" ++ (catWSpace s2 s3) ++ ")")

expr_stmt :: Parser String
expr_stmt =     testlist <~> augassign <~> testlist
                 ==> reduce
            <|> testlist <~> ter "=" <~> testlist
                 ==> (\(var,(eq,by))-> "(" ++ eq ++ " (" ++ var ++ ") " ++ by ++ ")")
            <|> tuple_or_test
    where reduce (var,(aug,by)) | trace ("expr") False = undefined
          reduce (var,(aug,by)) = "\"" ++ aug ++ "\" " ++ "(" ++ var ++ ") " ++ by

augassign :: Parser String
augassign =     ter "+=" <|> ter "-=" <|> ter "*=" <|> ter "/="
            <|> ter "%=" <|> ter "&=" <|> ter "|=" <|> ter "^="
            <|> ter "<<=" <|> ter ">>=" <|> ter "**=" <|> ter "//="

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
raise_stmt =     ter "raise"
            <~> (eps "" <|> (test <~>
                              (eps "" <|> ter "from" <~> test ==> (\(f,t) -> catWSpace f t))
                              ==> (\_->""))
                ) ==> (\(s1,s2) -> catWSpace s1 s2) --FIXME: reduction

global_stmt :: Parser String
global_stmt = ter "global" <~> ter "id" <~> idRep ==> (\_->"") -- FIXME: reduction

idRep :: Parser String
idRep = p
  where p = eps "" <|> ter "," <~> ter "id" <~> p ==> (\_->"") -- FIXME: reduction

nonlocal_stmt :: Parser String
nonlocal_stmt = ter "nonlocal" <~> ter "id" <~> idRep ==> (\_->"") -- FIXME: reduction

assert_stmt:: Parser String
assert_stmt =     ter "assert" <~> test
              <~> (eps "" <|> (ter "," <~> test) ==> (\_->""))
              ==> (\_->"") -- FIXME: reduction

compound_stmt :: Parser String
compound_stmt = if_stmt <|> while_stmt <|> for_stmt <|> try_stmt <|> funcdef

if_stmt :: Parser String
if_stmt =     ter "if" <~> test <~> ter ":" <~> suite
          <~> elifRep
          <~> (eps "" <|> (ter "else" <~> ter ":" <~> suite ==> (\_->"")))
          ==> (\_->"") -- FIXME: reduction

elifRep :: Parser String
elifRep = p
  where p = eps "" <|> ter "elif" <~> test <~> ter ":" <~> suite <~> p ==> (\_->"") -- FIXME: reduction

while_stmt :: Parser String
while_stmt =    ter "while" <~> test <~> ter ":"
            <~> (eps "" <|> ter "else" <~> ter ":" <~> suite ==> (\_->""))
            ==> (\_->"") -- FIXME: reduction

for_stmt :: Parser String
for_stmt =    ter "for" <~> ter "id" <~> ter "in" <~> test <~> ter ":" <~> suite
          <~> (eps "" <|> (ter "else" <~> ter ":" <~> suite ==> (\_->"")))
          ==> (\_->"") -- FIXME: reduction

try_stmt :: Parser String
try_stmt = ter "try" <~> ter ":" <~> suite <~> (x1 <|> x2) ==> (\_->"") -- FIXME: reduction
  where x1 =     except_clause <~> ter ":" <~> suite <~> excptRep
             <~> (eps "" <|> ter "else" <~> ter ":" <~> suite ==> (\_->""))
             <~> (eps "" <|> ter "finally" <~> ter ":" <~> suite ==> (\_->""))
             ==> (\_->"") -- FIXME: reduction
        x2 = ter "finally" <~> ter ":" <~> suite ==> (\_->"") -- FIXME: reduction

excptRep :: Parser String
excptRep = p
  where p = eps "" <|> except_clause <~> ter ":" <~> suite <~> p ==> (\_->"") -- FIXME: reduction


except_clause :: Parser String
except_clause =     ter "except"
                <~> (eps "" <|> (test <~> (eps "" <|> ter "as" <~> ter "id" ==> (\_->""))
                                ==> (\_->"")))
                ==> (\_->"") -- FIXME: reduction

suite :: Parser String
suite =     simple_stmt
        <|> ter "newline" <~> ter "indent" <~> stmtRep <~> ter "dedent"
            ==> (\(nw,(ind,(stR,ddnt))) -> "suite " ++ stR)

stmtRep :: Parser String
stmtRep = stmt <~> p ==> (\_->"") -- TODO: fix reduction
  where p = eps "" <|> stmt <~> p ==> (\(s1,s2) -> catWSpace s1 s2)

test :: Parser String
test =     or_test <~> ter "if" <~> or_test <~> ter "else" <~> test ==> (\_ -> "test") -- FIXME: reduction
       <|> or_test <|> lambdef

lambdef :: Parser String
lambdef = ter "lamda" <~> (eps "" <|> paramlist) <~> ter ":" <~> test ==> (\_->"") -- FIXME: reduction

or_test :: Parser String
or_test = and_test <~> and_testRep ==> (\(s1,s2) -> catWSpace s1 s2) -- FIXME: reduction

and_testRep :: Parser String
and_testRep = p
  where p = eps "" <|> ter "or" <~> and_test <~> p ==> (\_->"") -- FIXME: reduction

and_test:: Parser String
and_test = not_test <~> not_testRep ==> (\(s1,s2)-> catWSpace s1 s2) -- FIXME: reduction

not_testRep :: Parser String
not_testRep = p
  where p = eps "" <|> ter "and" <~> not_test <~> p ==> (\_->"") -- FIXME: reduction

not_test :: Parser String
not_test =    ter "not" <~> not_test ==> (\(s1,s2)-> catWSpace s1 s2) -- FIXME: reduction
          <|> comparison

comparison :: Parser String
comparison = star_expr <~> comp_opRep ==> (\(e,c)-> e ++ c) -- FIXME: reduction

comp_opRep :: Parser String
comp_opRep = p
  where p = eps "" <|> comp_op <~> star_expr <~> p ==> (\(c,(e,r))-> c ++ e ++ r) -- FIXME: reduction

comp_op :: Parser String
comp_op =     ter "<" <|> ter ">" <|> ter "==" <|> ter ">="
          <|> ter "<=" <|> ter "<>" <|> ter "!=" <|> ter "in"
          <|> (ter "not" <~> ter "in" ==> (\_->""))
          <|> (ter "is" <~> ter "not" ==> (\_->"")) -- FIXME: reduction

star_expr :: Parser String
star_expr = (eps "" <|> ter "*") <~> expr ==> (\(s,e) -> catWSpace s e) -- FIXME: reduction

expr :: Parser String
expr = xor_expr <~> xor_exprRep ==> reduce
  where reduce (s1,s2) | trace ("expr1") False = undefined
        reduce (s1,s2) = catWSpace s1 s2

xor_exprRep :: Parser String
xor_exprRep = p
  where p = eps "" <|> ter "|" <~> xor_expr <~> p ==> (\(s1,(s2,s3)) -> "") -- FIXME: reduction

xor_expr :: Parser String
xor_expr = and_expr <~> and_exprRep ==> (\(s1,s2)-> s1 ++ s2) -- FIXME: reduction

and_exprRep :: Parser String
and_exprRep = p
  where p = eps "" <|> ter "^" <~> and_expr <~> p ==> (\(s1,(s2,s3)) -> "") -- FIXME: reduction

and_expr :: Parser String
and_expr = shift_expr <~> shift_exprRep ==> (\(s1,s2)-> catWSpace s1 s2) -- FIXME: reduction

shift_exprRep :: Parser String
shift_exprRep = p
  where p = eps "" <|> ter "&" <~> shift_expr <~> p ==> (\(s1,(s2,s3)) -> "") -- FIXME: reduction

shift_expr :: Parser String
shift_expr = arith_expr <~> arith_exprRep ==> (\(s1,s2)-> catWSpace s1 s2) -- FIXME: reduction

arith_exprRep :: Parser String
arith_exprRep = p
  where p = eps "" <|> (ter "<<" <|> ter ">>") <~> arith_expr <~> p ==> (\(s1,(s2,s3)) -> "") -- FIXME: reduction

arith_expr :: Parser String
arith_expr = term <~> addRep ==> red
  where red (t, []) = t
        red (t, op) = "(arith " ++ catWSpace t op ++ ")"

addRep :: Parser String
addRep = p
  where p = eps "" <|> (ter "+" <|> ter "-") <~> term <~> p ==> (\(op,(var,rest))-> "(\"" ++ op ++ "\" " ++ var ++ ")" ++ rest) -- FIXME: reduction

term :: Parser String
term = factor <~> multRep ==> (\(s1,s2)-> catWSpace s1 s2) -- FIXME: reduction

multRep :: Parser String
multRep = p
  where p = eps "" <|>
           (ter "*" <|> ter "/" <|> ter "%" <|> ter "//") <~> factor <~> p ==> (\_->"") -- FIXME: reduction


factor :: Parser String
factor = ((ter "+" <|> ter "-" <|> ter "~") <~> factor ==> (\(p,f)-> p ++ f)) <|> power

indexed:: Parser String
indexed = atom <~> trailerRep ==> red
  where red (x,[]) = x
        red (x,y) = "indexed " ++ catWSpace x y

power :: Parser String
power = indexed <~> (powFac <|> eps "") ==> red
  where powFac = (ter "**" <~> factor) ==> (\(_,f) -> f)
        red (ind, []) = ind
        red (ind, pf) = "power " ++ ind ++ " " ++ pf

atom :: Parser String
atom = ter "(" <~> (tuple_or_test <|> eps "") <~> ter ")" ==> (\(_,(tup,_)) -> "tuple " ++ tup) <|>
       ter "[" <~> (testlist <|> eps "") <~> ter "]" ==> (\(_,(list,_))-> "list " ++ list) <|>
       ter "{" <~> (dictorsetmaker <|> eps "") <~> ter "}" ==> (\(_,(dict,_))-> "dict " ++ dict) <|>
       ter "id" ==> (\x->x) <|>
       ter "lit" ==> (\x->x) <|>
       strRep <|>
       ter "..." ==> (\_->"") <|> -- FIXME: reduction
       ter "None" ==> (\_->"None") <|> -- FIXME: reduction
       ter "True" ==> (\_->"True") <|> -- FIXME: reduction
       ter "False" ==> (\_->"False") -- FIXME: reduction

strRep:: Parser String
strRep = p
  where p = eps "" <|> ter "string" <~> p ==> (\(x,y) -> x ++ y) -- FIXME: reduction

trailer :: Parser String
trailer = p
  where
    p = ter "(" <~> (eps "" <|> arglist) <~> ter ")" ==> (\(s1,(s2,s3)) -> s1 ++ s2 ++ s3 ) <|>
        ter "[" <~> tuple_or_test <~> ter "]" ==> (\_ -> "") -- <|> ter "." <~> NAME

trailerRep:: Parser String
trailerRep = p
  where p = eps "" <|> trailer <~> p ==> (\(s1,s2) -> s1 ++ s2)

testlist :: Parser String
testlist = test <~> testComRep <~> (eps "" <|> ter ",") ==> (\(s1,(s2,_))-> catWSpace s1 s2) -- FIXME:red

-- just like arglist
tuple_or_test :: Parser String
tuple_or_test = test <~> argRep <~> (eps "" <|> ter ",") ==> out --FIXME: reduction
  where out (x, ([],[])) = "(expr (" ++ x ++ "))"
        out (x, (y,z)) = "(tuple " ++ x ++ " " ++ y ++ " " ++ z ++ ")"

argRep :: Parser String
argRep = p
  where
    p = eps "" <|> ter "," <~> test <~> p ==> (\_ -> "")

dictorsetmaker :: Parser String
dictorsetmaker = x1 <|> x2
  where x1 = test <~> ter ":" <~> test <~> testColRep <~> (eps "" <|> ter ",") ==> (\_->"") -- FIXME: reduction
        x2 = test <~> testComRep <~> (eps "" <|> ter ",") ==> (\_->"") -- FIXME: reduction

testComRep :: Parser String
testComRep = p
  where p = eps "" <|> ter "," <~> test <~> p ==> (\(_,(t,r))-> catWSpace t r) -- FIXME: reduction

testColRep :: Parser String
testColRep = p
  where p = eps "" <|> ter "," <~> test <~> ter ":" <~> test <~> p ==> (\_->"") -- FIXME: reduction

arglist :: Parser String
arglist = test <~> argRep ==> (\_->"") <|> ter ","

-- -- -- -- -- -- -- --
-- Util Functions
-- -- -- -- -- -- -- --
-- convert PyLex tokens to Derp Tokens
makeTokens :: [Pylex.Token] -> [Token]
makeTokens (x:xs) = (toToken x) : makeTokens xs
makeTokens [] = []

-- convert a PyLex token to a Derp Token
toToken :: Pylex.Token-> Token
toToken (Pylex.Id s) = Token "id" s
toToken (Pylex.Lit s) = Token "lit" s
toToken (Pylex.Keyword s) = Token s s
toToken (Pylex.Punct s) = Token s s
toToken (Pylex.Complex s) = Token "complex" s
toToken (Pylex.OctHexBin s) = Token "octhexbin" s
toToken (Pylex.StringLit s) = Token "string" s
toToken (Pylex.Error s) = Token "error" s
toToken Pylex.Newline = Token "newline" "newline"
toToken Pylex.Indent = Token "indent" "indent"
toToken Pylex.Dedent = Token "dedent" "dedent"
toToken Pylex.Endmarker = Token "endmarker" "endmarker"
toToken Pylex.LineCont = Token "linecont" "linecont"

catWSpace :: String -> String -> String
catWSpace x (y:ys) = x ++ " " ++ (y:ys)
catWSpace x [] = x

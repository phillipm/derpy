-- Haskell imports
import System.IO (hGetContents, stdin)
import Data.Set (toList)
{-import Debug.Trace (trace)-}

import qualified PythonLexer as Pylex
{-import PythonParser-}

import Text.Derp

-- TODO: take in -l flag to specify between prelexed input
main :: IO ()
main = do input <- (hGetContents stdin)
          let result = toList $ runParse file_input $ makeTokens . Pylex.lexInput $ input
          case result of
               [] -> putStrLn "#f"
               x -> showNL x

showNL :: [String] -> IO()
showNL (t:ts) = do putStrLn t
                   showNL ts
showNL [] = do putStrLn ""

-- -- -- -- -- -- -- --
-- Parsing Functions
-- -- -- -- -- -- -- --
file_input :: Parser String
file_input = file_inputRep <~> ter "endmarker" ==> (\(x,_) -> "(program " ++ x ++ ")")

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
parameters = ter "(" <~> paramlist <~> ter ")" ==> (\(_,(pl,_)) -> pl)

paramlist :: Parser String
paramlist =     eps ""
            <|> ter "id" <~> paramlistRep ==> (\(s1,s2) -> catWSpace s1 s2)
            <|> ter ","

paramlistRep :: Parser String
paramlistRep = p
  where p =     eps ""
            <|> ter "," <~> ter "id" <~> p ==> (\(_,(name,_)) -> name)

stmt :: Parser String
stmt = simple_stmt <|> compound_stmt

simple_stmt :: Parser String
simple_stmt = small_stmt <~> small_stmtRep <~> (eps "" <|> ter ";") <~> ter "newline"
              ==> (\(x,(x2,_)) -> catWSpace x x2)

small_stmt :: Parser String
small_stmt =     expr_stmt <|> del_stmt <|> pass_stmt <|> flow_stmt
             <|> global_stmt <|> nonlocal_stmt <|> assert_stmt

small_stmtRep :: Parser String
small_stmtRep = p
  where p =     eps ""
            <|> ter ";" <~> small_stmt <~> p ==> (\(_,(stm,r)) -> "(" ++ (catWSpace stm r) ++ ")")

expr_stmt :: Parser String
expr_stmt =     testlist <~> augassign <~> testlist
                 ==> red
            <|> testlist <~> ter "=" <~> testlist
                 ==> (\(var,(_,by))-> "(= (" ++ var ++ ") (" ++ by ++ "))")
            <|> tuple_or_test ==> (\x-> "(expr " ++ x ++ ")")
    where red (var,(aug,by)) = "(\"" ++ aug ++ "\" " ++ "(" ++ var ++ ") (" ++ by ++ "))"

augassign :: Parser String
augassign =     ter "+=" <|> ter "-=" <|> ter "*=" <|> ter "/="
            <|> ter "%=" <|> ter "&=" <|> ter "|=" <|> ter "^="
            <|> ter "<<=" <|> ter ">>=" <|> ter "**=" <|> ter "//="

del_stmt :: Parser String
del_stmt = ter "del" <~> star_expr ==> (\(_,e)-> "(del " ++ e ++ ")")

pass_stmt :: Parser String
pass_stmt = ter "pass" ==> (\_->"(pass)")

flow_stmt :: Parser String
flow_stmt = break_stmt <|> continue_stmt <|> return_stmt <|> raise_stmt

break_stmt :: Parser String
break_stmt = ter "break" ==> (\_->"(break)")

continue_stmt :: Parser String
continue_stmt = ter "continue" ==> (\_->"continue")

return_stmt :: Parser String
return_stmt = ter "return" <~> (eps "" <|> testlist) ==> (\(_,tl) -> (catWSpace "(return" tl) ++ ")")

raise_stmt :: Parser String
raise_stmt =     ter "raise"
            <~> (eps "" <|> (test <~>
                              (eps "" <|> ter "from" <~> test ==> (\(_,t) -> t))
                              ==> (\(t,f)-> catWSpace t f))
                ) ==> (\(s1,s2) -> "(" ++ (catWSpace s1 s2) ++ ")")

global_stmt :: Parser String
global_stmt = ter "global" <~> ter "id" <~> idRep
              ==> (\(_,(i,ir))-> "(global " ++ (catWSpace i ir) ++ ")")

idRep :: Parser String
idRep = p
  where p = eps "" <|> ter "," <~> ter "id" <~> p ==> (\(_,(i,r))-> catWSpace i r)

nonlocal_stmt :: Parser String
nonlocal_stmt = ter "nonlocal" <~> ter "id" <~> idRep
                ==> (\(_,(i,ir))->"(nonlocal " ++ (catWSpace i ir) ++ ")")

assert_stmt:: Parser String
assert_stmt =     ter "assert" <~> test
              <~> (eps "" <|> (ter "," <~> test) ==> (\(_,t)-> t))
              ==> (\(_,(t1,t2))->"(assert " ++ (catWSpace t1 t2) ++ ")")

compound_stmt :: Parser String
compound_stmt = if_stmt <|> while_stmt <|> for_stmt <|> try_stmt <|> funcdef

if_stmt :: Parser String
if_stmt =     ter "if" <~> test <~> ter ":" <~> suite
          <~> elifRep
          <~> (eps "" <|> (ter "else" <~> ter ":" <~> suite ==> (\(_,(_,s))->"(else (" ++ s ++ "))")))
          ==> red
  where red (_,(t,(_,(s,(elif,els))))) = 
          (catWSpace (catWSpace ("(cond (" ++ t ++ " (" ++ s ++ "))") elif) els) ++ ")"


elifRep :: Parser String
elifRep = p
  where p = eps "" <|> ter "elif" <~> test <~> ter ":" <~> suite <~> p
            ==> (\(_,(t,(_,(s,r)))) -> catWSpace ("(" ++ t ++ " (" ++ s ++ "))") r)

while_stmt :: Parser String
while_stmt =    ter "while" <~> test <~> ter ":" <~> suite
            <~> (eps "" <|> ter "else" <~> ter ":" <~> suite ==> (\(_,(_,s))-> "(" ++ s ++ ")"))
            ==> (\(_,(t,(_,(s,(els))))) -> (catWSpace ("(while " ++ t ++ " (" ++ s ++ ")") els) ++ ")")

for_stmt :: Parser String
for_stmt =    ter "for" <~> ter "id" <~> ter "in" <~> test <~> ter ":" <~> suite
          <~> (eps "" <|> (ter "else" <~> ter ":" <~> suite
              ==> (\(_,(_,s))->"(" ++ s ++ ")")))
          ==> (\(_,(i,(_,(t,(_,(s,e))))))
              -> (catWSpace ("(for " ++ i ++ " (" ++ t ++ ") (" ++ s ++ ")") e) ++ ")")


try_stmt :: Parser String
try_stmt = ter "try" <~> ter ":" <~> suite <~> (x1 <|> x2)
           ==> (\(_,(_,(s,e)))->"(try (" ++ s ++ ") " ++  e ++ ")")
  where x1 =     except_clause <~> ter ":" <~> suite <~> excptRep
             <~> ((eps "" <|> ter "else" <~> ter ":" <~> suite
                  ==> (\(_,(_,s)) -> "(" ++ s ++ ")")) ==> redFinally)
             <~> ((eps "" <|> ter "finally" <~> ter ":" <~> suite
                  ==> (\(_,(_,s)) -> "(" ++ s ++ ")")) ==> redFinally)
             ==> redExcepts
        x2 = ter "finally" <~> ter ":" <~> suite ==> (\(_,(_,s))-> "() #f (" ++ s ++ ")")

        redFinally [] = "#f"
        redFinally s = s



        -- no extra excepts
        redExcepts (e,(_,(s,([],(el,fi))))) = foldr (++) "" ["((", e, " (", s, "))) ", el, " ", fi]
        redExcepts (e,(_,(s,(er,(el,fi))))) = foldr (++) "" ["((", e, " (", s, ")) ", er, ") ", el, " ", fi]

excptRep :: Parser String
excptRep = p
  where p = eps "" <|> except_clause <~> ter ":" <~> suite <~> p
            ==> (\(e,(_,(s,r))) -> catWSpace ("(" ++ e ++ " (" ++ s ++ "))") r)



except_clause :: Parser String
except_clause =     ter "except"
                <~> (eps "" <|> (test <~> (eps "" <|> ter "as" <~> ter "id" ==> (\(_,i)->i))
                                ==> (\(t,as)-> catWSpace t as)))
                ==> (\(_,t)-> (catWSpace "(except" t) ++ ")")

suite :: Parser String
suite =     simple_stmt
        <|> ter "newline" <~> ter "indent" <~> stmtRep <~> ter "dedent"
            ==> (\(_,(_,(stR,_))) -> "suite " ++ stR)

stmtRep :: Parser String
stmtRep = stmt <~> p ==> (\(s,r)-> catWSpace s r)
  where p = eps "" <|> stmt <~> p ==> (\(s,r) -> catWSpace s r)

test :: Parser String
test =     or_test <~> ter "if" <~> or_test <~> ter "else" <~> test ==> red
       <|> or_test <|> lambdef
  where red (p1,(_,(p2,(_,p3)))) = "(expr (if " ++ p1 ++ " " ++ p2 ++ " " ++ p3 ++ "))"

lambdef :: Parser String
lambdef = ter "lambda" <~> (eps "" <|> paramlist) <~> ter ":" <~> test ==> red
  where red (_,(p,(_,t))) = "(expr (lambda (" ++ p ++ ") (" ++ t ++ ")))"

or_test :: Parser String
or_test = and_test <~> and_testRep ==> red
  where red (at, []) = at
        red (at, atr) = "(or " ++ (catWSpace at atr) ++ ")"

and_testRep :: Parser String
and_testRep = p
  where p = eps "" <|> ter "or" <~> and_test <~> p
            ==> (\(_,(at,r)) -> catWSpace at r)

and_test:: Parser String
and_test = not_test <~> not_testRep ==> red
  where red (nt,[]) = nt
        red (nt, ntr) = "(and " ++ nt ++ " " ++ ntr ++ ")"

not_testRep :: Parser String
not_testRep = p
  where p = eps "" <|> ter "and" <~> not_test <~> p
            ==> (\(_,(nt,r)) -> catWSpace nt r)

not_test :: Parser String
not_test =    ter "not" <~> not_test ==> (\(_,s2)-> "(not " ++ s2 ++ ")")
          <|> comparison

comparison :: Parser String
comparison = star_expr <~> comp_opRep ==> red
  where red (e,[]) = e
        red (e,c) = "(comparison " ++ e ++ " " ++ c ++ ")"

comp_opRep :: Parser String
comp_opRep = p
  where p = eps "" <|> comp_op <~> star_expr <~> p
             ==> (\(cop,(e,r)) -> catWSpace ("(\"" ++ cop ++ "\" " ++ e ++ ")") r)

comp_op :: Parser String
comp_op =     ter "<" <|> ter ">" <|> ter "==" <|> ter ">="
          <|> ter "<=" <|> ter "<>" <|> ter "!=" <|> ter "in"
          <|> (ter "not" <~> ter "in" ==> (\_->"not-in"))
          <|> (ter "is" <~> ter "not" ==> (\_->"is-not"))

star_expr :: Parser String
star_expr = (eps "" <|> ter "*") <~> expr ==> red
  where red ([], e) = e
        red (_, e) = "(star " ++ e ++ ")"

expr :: Parser String
expr = xor_expr <~> xor_exprRep ==> red
  where red (xe, []) = xe
        red (xe, xer) = "bitwise-xor (" ++ xe ++ ") (" ++ xer ++ "))"

xor_exprRep :: Parser String
xor_exprRep = p
  where p = eps "" <|> ter "|" <~> xor_expr <~> p ==> (\(_,(s2,s3)) -> catWSpace s2 s3)

xor_expr :: Parser String
xor_expr = and_expr <~> and_exprRep ==> red
  where red (ae, []) = ae
        red (ae, aer) = "(bitwise-or (" ++ ae ++ ") (" ++ aer ++ "))"

and_exprRep :: Parser String
and_exprRep = p
  where p = eps "" <|> ter "^" <~> and_expr <~> p ==> (\(_,(s2,s3)) -> catWSpace s2 s3)

and_expr :: Parser String
and_expr = shift_expr <~> shift_exprRep ==> red
  where red (se, []) = se
        red (se, ser) = "(bitwise-and (" ++ se ++ ") (" ++ ser ++ "))"

shift_exprRep :: Parser String
shift_exprRep = p
  where p = eps "" <|> ter "&" <~> shift_expr <~> p ==> (\(_,(s2,s3)) -> catWSpace s2 s3)

shift_expr :: Parser String
shift_expr = arith_expr <~> arith_exprRep ==> red
  where red (ae, []) = ae
        red (ae, aer) = "shift " ++ catWSpace ae aer

arith_exprRep :: Parser String
arith_exprRep = p
  where p = eps "" <|> (ter "<<" <|> ter ">>") <~> arith_expr <~> p
            ==> (\(op,(ae,r)) -> catWSpace ("(" ++ op ++ " " ++ ae ++ ")") r)

arith_expr :: Parser String
arith_expr = term <~> addRep ==> red
  where red (t, []) = t
        red (t, op) = "(arith " ++ catWSpace t op ++ ")"

addRep :: Parser String
addRep = p
  where p = eps "" <|> (ter "+" <|> ter "-") <~> term <~> p
             ==> (\(op,(var,rest))-> "(\"" ++ op ++ "\" " ++ var ++ ")" ++ rest)

term :: Parser String
term = factor <~> multRep ==> red
  where red (f, []) = f
        red (f, op) = "(term " ++ (catWSpace f op) ++ ")"

multRep :: Parser String
multRep = p
  where p = eps "" <|>
           (ter "*" <|> ter "/" <|> ter "%" <|> ter "//") <~> factor <~> p
              ==> (\(op,(f,r))-> catWSpace ("(\"" ++ op ++ "\" " ++ f ++ ")") r)


factor :: Parser String
factor = ((ter "+" <|> ter "-" <|> ter "~") <~> factor ==> (\(p,f)-> "(\"" ++ p ++ "\" " ++ f ++ ")")) <|> power

indexed:: Parser String
indexed = atom <~> trailerRep ==> red
  where red (x,[]) = x
        red (x,y) = "(indexed " ++ (catWSpace x y) ++ ")"

power :: Parser String
power = indexed <~> (powFac <|> eps "") ==> red
  where powFac = (ter "**" <~> factor) ==> (\(_,f) -> f)
        red (ind, []) = ind
        red (ind, pf) = "power " ++ ind ++ " " ++ pf

atom :: Parser String
atom = ter "(" <~> (tuple_or_test <|> eps "") <~> ter ")" ==> (\(_,(tup,_)) -> tup) <|>
       ter "[" <~> (testlist <|> eps "") <~> ter "]" ==> (\(_,(list,_))-> "list " ++ list) <|>
       ter "{" <~> (dictorsetmaker <|> eps "") <~> ter "}" ==> (\(_,(dict,_))-> dict) <|>
       ter "id" ==> (\x->x) <|>
       ter "lit" ==> (\x->x) <|>
       strRep ==> (\x-> "\"" ++ x ++ "\"") <|>
       ter "..." ==> (\_->"") <|> -- FIXME: reduction
       ter "None" ==> (\_->"None") <|> -- FIXME: reduction
       ter "True" ==> (\_->"True") <|> -- FIXME: reduction
       ter "False" ==> (\_->"False") -- FIXME: reduction

strRep:: Parser String
strRep = ter "string" <~> p ==> (\(x,y) -> x ++ y)
  where p = eps "" <|> ter "string" <~> p ==> (\(x,y) -> x ++ y) -- FIXME: reduction

trailer :: Parser String
trailer = p
  where
    p =     ter "(" <~> (eps "" <|> arglist) <~> ter ")"
              ==> (\(_,(args,_)) -> "(called " ++ args ++ ")")
        <|> ter "[" <~> tuple_or_test <~> ter "]"
              ==> (\(_,(t,_)) -> "subscript " ++ t)
        <|> ter "." <~> ter "id"
              ==> (\(_,n) -> "dot " ++ n)

trailerRep:: Parser String
trailerRep = p
  where p = eps "" <|> trailer <~> p ==> (\(s1,s2) -> s1 ++ s2)

testlist :: Parser String
testlist = test <~> testComRep <~> (eps "" <|> ter ",") ==> (\(s1,(s2,_))-> catWSpace s1 s2) -- FIXME:red

tuple_or_test :: Parser String
tuple_or_test = test <~> argRep <~> (eps "" <|> ter ",") ==> red
  where red (x, ([],[])) = x
        red (x, (y,_)) = "(tuple " ++ x ++ " " ++ y ++ ")"

argRep :: Parser String
argRep = p
  where
    p = eps "" <|> ter "," <~> test <~> p ==> (\(_,(t,r)) -> catWSpace t r)

dictorsetmaker :: Parser String
dictorsetmaker = x1 <|> x2
  where x1 = test <~> ter ":" <~> test <~> testColRep <~> (eps "" <|> ter ",") ==> (\(k,(_,(v,(r,_)))) -> (catWSpace ("(dict (" ++ k ++ v ++ ")") r) ++ ")") -- FIXME: reduction
        x2 = test <~> testComRep <~> (eps "" <|> ter ",") ==> (\(t,(tr,(_)))-> (catWSpace ("(set " ++ t) tr) ++ ")") -- FIXME: reduction

testComRep :: Parser String
testComRep = p
  where p = eps "" <|> ter "," <~> test <~> p ==> (\(_,(t,r))-> catWSpace t r) -- FIXME: reduction

testColRep :: Parser String
testColRep = p
  where p = eps "" <|> ter "," <~> test <~> ter ":" <~> test <~> p ==> (\(_,(k,(_,(v,r))))-> catWSpace ("(" ++ k ++ " " ++ v ++ ")") r) -- FIXME: reduction

arglist :: Parser String
arglist = test <~> argRep <~> (eps "" <|> ter ",") ==> (\(t,(ar,_)) -> catWSpace t ar)

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
toToken _ = error "incorrect input token from lexer"

catWSpace :: String -> String -> String
catWSpace x (y:ys) = x ++ " " ++ (y:ys)
catWSpace x [] = x

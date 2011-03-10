{-
Phillip Mates
u0284736
-}

module PythonLexer (lexInput, showLexOutput, Token(..)) where

-- Haskell imports
--import Debug.Trace (trace)
import Data.Char (chr, isSpace)
import Numeric (readOct, readHex)
import Data.List (stripPrefix, elemIndex, delete)
import Data.Maybe (catMaybes)

-- Lexer's Regular Expression Definitions
import LexerRegex
-- David Darais' Derivative Regular Expressions
import DerRegex

data Token = Id String
           | Lit String
           | Keyword String
           | Punct String
           | Complex String
           | OctHexBin String
           | StringLit String
           | InterStringLit String String -- intermediate string for multiline strings
           | RString String
           | RStringInt String String-- intermediate raw string for linecont
           | Error String
           | Newline
           | Indent
           | Dedent
           | Endmarker
           | Comment
           | LineCont
           deriving (Eq)

-- print list of Tokens, each on newline
showLexOutput :: [Token] -> IO ()
showLexOutput (t:ts) = do putStrLn . show $ t
                          showLexOutput ts
showLexOutput [] = return ()

-- -- -- -- -- -- -- --
-- Post Processing
-- -- -- -- -- -- -- --
-- Convert raw string tokens to normal string tokens
convertRStrings :: [Token] -> [Token]
convertRStrings (t:ts) = cTok : convertRStrings ts
  where cTok = case t of
                  RStringInt s q -> InterStringLit ((escapeRaw s) ++ "\\\\\\n") q
                  RString s -> StringLit (escapeRaw s)
                  x -> x
convertRStrings _ = []

-- Convert escaped oct numbers to chars
convertOct :: String -> String
convertOct [] = []
convertOct ('\\':a:b:c:cs) = case and $ map (`elem` (map chr [48..55])) [a,b,c] of
                             True -> (chr $ fromIntegral $ fst . head . readOct $ a:b:c:[]):(convertOct cs)
                             False -> '\\':a:b:c:(convertOct cs)
convertOct (c:cs) = c:(convertOct cs)

-- Convert escaped hex numbers to chars
convertHex :: String -> String
convertHex [] = []
convertHex ('\\':a:b:c:cs) = case and $ (a=='x') : map (`elem` (map chr ([48..57]++[97..102]))) [b,c] of
                             True -> (chr $ fromIntegral $ fst . head . readHex $ b:c:[]):(convertHex cs)
                             False -> '\\':a:b:c:(convertHex cs)
convertHex (c:cs) = c:(convertHex cs)

-- Join consecutive InterStringLit tokens (along with the trailing StringLit token) into one StringLit
joinStringLits :: [Token] -> [Token] -> [Token]
joinStringLits (t:ts) toJoin =
  case t of
    InterStringLit x _ -> joinStringLits ts (t:toJoin)
    StringLit x -> if null toJoin
                      then (escapeToken t) : joinStringLits ts []
                      else joined : joinStringLits ts [] where
                      joined = escapeToken $ StringLit $ joinTokens $ t:toJoin
    _ -> t : joinStringLits ts toJoin

joinStringLits [] [] = []
joinStringLits [] (x:xs) =
  [Error "Error joining strings in post processing. Look for unmatched strings"]

-- Join tokens into a string
joinTokens :: [Token] -> String
joinTokens (t:ts) = joinTokens ts ++ (extractString t)
joinTokens [] = ""

-- Grab the value in the Token
extractString :: Token -> String
extractString t = case t of
                       StringLit s -> s
                       InterStringLit s ['\''] -> s
                       InterStringLit s ['"'] -> s
                       InterStringLit s "\'\'\'" -> s ++ "\\n"
                       InterStringLit s "\"\"\"" -> s ++ "\\n"
                       _ -> []

escapeToken :: Token -> Token
escapeToken t =
  case t of
       StringLit s -> StringLit $ escapeBackslash $ convertHex $ convertOct s
       InterStringLit s _ -> InterStringLit (escapeBackslash $ convertHex $ convertOct s) ""

-- -- -- -- -- -- -- --
-- Lexer functions
-- -- -- -- -- -- -- --
-- Top level lexer function
lexInput :: String -> [Token]
lexInput input = joinStringLits  (convertRStrings (processLines [0] [] (lines input))) []

-- [IndentStack] -> [ParenthesisStack] -> [StringToLex] -> [Tokens]
processLines :: [Int] -> [String] -> [String] -> [Token]
--processLines indStack pStack strs | trace ("processLines: " ++ show indStack ++ " with parenStack " ++ show pStack ++ " strs: " ++ show strs) False = undefined
processLines indStack pStack (i:is) =
  tokens ++ newLineToken ++ processLines newIndStack newPStack is
  where
    -- if last token is multiline string, add to Parenthesis Stack
    newPStack = if not . null $ tokens
                   then case last tokens of
                             RStringInt _ q -> addIfNotThere ('r':q) interPStack
                             InterStringLit _ q -> addIfNotThere q interPStack
                             _ -> interPStack
                   else interPStack

    addIfNotThere :: String -> [String] -> [String]
    addIfNotThere q stack = if elem q stack
                               then stack
                               else q : stack

    (tokens, newIndStack, pStack') = lexLine indStack pStack i

    (interPStack, newLineToken) = if pStack' == ["\\"]
                                    then (["\\"], [])
                                    else (pStack', addNewLineToken tokens pStack' is)

-- At end of file, check Parenthesis Stack for \
processLines [0] pStack [] =
  if elem "\\" pStack
    then [(Error "Trailing backslash")]
    else [Endmarker]

-- At end of file, Dedent the indent stack
processLines (i:is) pStack [] =
  if elem "\\" pStack
    then [(Error "Trailing backslash")]
    else dedentTokens ++ [Endmarker]
    where (dedentTokens, _) = dedentStack (i:is) 0

addNewLineToken :: [Token] -> [String] -> [int] -> [Token]
addNewLineToken [] _ _ = [] -- no tokens found, no newline
addNewLineToken (x:xs) [] _ = case last (x:xs) of
                                   InterStringLit _ _ -> []
                                   RStringInt _ _ -> []
                                   _ -> [Newline]
addNewLineToken (x:xs) (p:ps) _ = [] -- in nested paren, no newline

-- [IndentStack] -> [ParenthesisStack] -> string -> ([Token], [IndentStack], [ParenthesisStack])
-- Lex Single Line
lexLine :: [Int] -> [String] -> String -> ([Token], [Int], [String])
--lexLine indStack pStack str | trace ("lexLine " ++ show indStack ++ " with parenStack " ++ show pStack ++ " s: " ++ show str) False = undefined
-- lex indents
lexLine indStack [] i = (indentTokens ++ restTokens, currentIndStack, newPStack)
  where
    -- Add in the error token if it exists
    restTokens | (errorToken/=Comment) = errorToken : intRestTokens
               | otherwise = intRestTokens

    -- if LineCont at EOL, push \ to pStack and remove LineCont from tokens
    (newPStack, intRestTokens) = case correctLineContPos of
                                   True -> ("\\" : pStack, delete LineCont restTokens')
                                   False -> (pStack, restTokens')

    (restTokens', pStack) = tokenizeString restOfI []
    (currentLevel, restOfI) = leadingWS i

    (indentTokens, currentIndStack) = case intRestTokens of
         [] ->  ([], indStack) -- don't emit indent tokens if line is lexically empty
         _ -> updateStack indStack currentLevel

    (correctLineContPos, errorToken) = case checkLineContPos restTokens' of
                                            Just True -> (True, Comment) -- use Comment to state there is no error
                                            Just False -> (False, Comment)
                                            Nothing -> (False, Error "Backslash in the wrong place")

-- don't lex indents
lexLine indStack pStack i  = (restTokens, indStack, newPStack)
  where
                -- if \ in restTokens and \ in pStack, remove \ from restTokens
                -- if pStack is empty, we don't do line cont., so Delete
    restTokens | and [correctLineContPos, (elem "\\" pStack')] = delete LineCont restTokens'
               | (null pStack') = delete LineCont restTokens'
               | (errorToken/=Comment) = errorToken : restTokens'
               | otherwise = restTokens'

                -- if \ in restTokens and \ not in pStack, push \ to pStack
    newPStack | and [correctLineContPos, (not $ elem "\\" pStack')] = "\\" : pStack'
               -- if \ in pStack and no \ in restTokens, remove \ from pStack
              | and [(not correctLineContPos), (elem "\\" pStack')] = delete "\\" pStack'
              | otherwise = pStack'

    (restTokens', pStack') = if elem pHead quoteChars
                                then lexStringCont i pStack
                                else case pHead of
                                         "\\" -> tokenizeString i (tail pStack)
                                         _ -> tokenizeString i pStack
                                where pHead = pStack !! 0

    quoteChars = ["r\"", "r'", "r'''", "r\"\"\"", "\"", "\'", "'''", "\"\"\""]

    (correctLineContPos, errorToken) = case checkLineContPos restTokens' of
                                            Just True -> (True, Comment) -- use Comment to state there is no error
                                            Just False -> (False, Comment)
                                            Nothing -> (False, Error "Backslash in the wrong place")

-- LineCont (\) exists (and is at end of token list, else error!)
checkLineContPos :: [Token] -> Maybe Bool
checkLineContPos tokens = case (elem LineCont tokens) of
                               True -> if (ind == ((length tokens) - 1))
                                          then Just True
                                          else Nothing -- error "Backslash in wrong place"
                               False -> Just False
                               where
                                 ind = case elemIndex LineCont tokens of
                                            Just x -> x
                                            Nothing -> -2

-- Lex a line if the last line ended in a string continuation
lexStringCont :: String -> [String] -> ([Token], [String])
--lexStringCont s pStack | trace ("lexStringCont " ++ show s ++ " with parenStack " ++ show pStack) False = undefined
lexStringCont s (p:ps) = (tokens, newPStack)
  where
  (tokens, newPStack) = case matchedToken of
                             StringLit _ -> (matchedToken : restTokens, newP)
                             RString _ -> (matchedToken : restTokens, newP)
                             InterStringLit _ _ -> ([matchedToken], p:ps)
                             RStringInt _ _ -> ([matchedToken], p:ps)
                             Error x -> ([Error x], ps)
                             _ -> ([Error "String Lexing error"], p:ps)
                             where (restTokens, newP) = tokenizeString restStr ps

  (matchedToken, restStr) = case inter of
                                 Nothing -> (Error "String Lexing error", "")
                                 Just (t, _, s) -> (t, s)
  inter = case p of
               "\"" -> matchRegexWithToken (doubleQuoteShortContRegex, StringLit "") s "\""
               "'" -> matchRegexWithToken (singleQuoteShortContRegex, StringLit "") s "\'"
               "'''" -> matchRegexWithToken (singleQuoteLongContRegex, StringLit "") s "'''"
               "\"\"\"" -> matchRegexWithToken (doubleQuoteLongContRegex, StringLit "") s "\"\"\""
               "r\"" -> matchRegexWithToken (doubleQuoteShortContRegex, RString "") s "\""
               "r'" -> matchRegexWithToken (singleQuoteShortContRegex, RString "") s "'"
               "r'''" -> matchRegexWithToken (singleQuoteLongContRegex, RString "") s "'''"
               "r\"\"\"" -> matchRegexWithToken (doubleQuoteLongContRegex, RString "") s "\"\"\""
               _ -> Nothing

-- pop paren of Paren Stack; skip over \
popPStack :: [String] -> [String]
popPStack (p:ps) | (or [p'==")", p'=="]", p'=="}"]) = slashes ++ ps'
                 | otherwise = []
                    where (slashes, (p':ps')) = span (\x-> x=="\\") $ (p:ps)


-- StringToTokenize -> [ParenthesisStack] -> ([Token], [ParenthesisStack])
tokenizeString :: String -> [String] -> ([Token], [String])
--tokenizeString s pStack | trace ("tokenizeString " ++ show s ++ " with parenStack " ++ show pStack) False = undefined
tokenizeString s (p:ps) = case getLongestMatchToken $ trimS of
  Nothing -> case trimS of
                  "" -> ([], (p:ps))
                  _ -> ([Error $ "Not all of the line was matched: " ++ trimS], [])
  (Just (t, rest)) -> case t of
                           -- match closing paren
                           Punct ")" -> (t : restTokens, restPStack)
                           Punct "}" -> (t : restTokens, restPStack)
                           Punct "]" -> (t : restTokens, restPStack)
                           _ -> handleToken t rest (p:ps)
                           where (restTokens, restPStack) = tokenizeString rest (popPStack (p:ps))
  where
    trimS = dropWhile isSpace s

tokenizeString s [] = case getLongestMatchToken $ trimS of
  Nothing -> case trimS of
                  "" -> ([], [])
                  _ -> ([Error $ "Not all of the line was matched: " ++ trimS], [])
  (Just (t, rest)) -> handleToken t rest []
  where
    trimS = dropWhile isSpace s

-- Lex rest of string depending on current token
handleToken :: Token -> String -> [String] -> ([Token], [String])
--handleToken t restOfStr pStack | trace ("handleToken " ++ show t ++ " with parenStack " ++ show pStack ++ " rest " ++ restOfStr) False = undefined
handleToken t restOfStr pStack =
  case t of
       -- check if \ appears at EOL
       LineCont -> if not . null $ restOfStr
                      then ((Error "Floating line escape char: \\") : t : restTokens, restPStack)
                      else case pStack of -- if we are in a nested paren, we don't care about LineCont tokens
                                [] -> (t : restTokens, restPStack)
                                _ -> (restTokens, restPStack)
                      where (restTokens, restPStack) = tokenizeString restOfStr pStack
       Comment -> ([], pStack)
       -- add new paren
       Punct "(" -> (t : restTokens, restPStack) where (restTokens, restPStack) = tokenizeString restOfStr (")":pStack)
       Punct "{" -> (t : restTokens, restPStack) where (restTokens, restPStack) = tokenizeString restOfStr ("}":pStack)
       Punct "[" -> (t : restTokens, restPStack) where (restTokens, restPStack) = tokenizeString restOfStr ("]":pStack)
       -- add new string
       InterStringLit _ "'" -> contLexIfBSEscaped
       InterStringLit _ "\"" -> contLexIfBSEscaped
       _ -> (t : restTokens, restPStack) where (restTokens, restPStack) = tokenizeString restOfStr pStack

   where
   contLexIfBSEscaped = if not $ null restOfStr
                          then ((Error "Floating line escape char: \\") : t : restTokens, restPStack)
                          else (t : restTokens, restPStack)
                          where (restTokens, restPStack) = tokenizeString restOfStr pStack


getLongestMatchToken :: String -> Maybe (Token, String)
getLongestMatchToken input = longestMatch
  where
    f :: (Regex, Token) -> Maybe (Token, Int, String)
    f r = matchRegexWithToken r input []

    intermediate :: [(Token, Int, String)]
    intermediate = catMaybes $ map f tokenRegexs

    maxLength :: Int
    maxLength = maximum $ map (\(_, n, _) -> n) intermediate

    contestants :: [(Token, Int, String)]
    contestants = filter (\(_, n, _) -> n == maxLength) intermediate

    longestMatch :: Maybe (Token, String)
    longestMatch = case contestants of
      [] -> Nothing
      (x:_) -> Just ((\(t, _, s) -> (t,s)) x)

-- Match regex and return corresponding token, match length, rest of string
matchRegexWithToken :: (Regex, Token) -> String -> String -> Maybe (Token, Int, String)
matchRegexWithToken (rex, tok) inp quote =
  case match rex inp of
    Just (matchResult, matchlen, rest) -> Just (token, matchlen, rest) where
      token = case tok of
                Id _ -> Id matchResult
                Lit _ -> Lit matchResult
                Complex _ -> Complex $ '+' : (replace "j" "i" $ replace "J" "i" matchResult )
                OctHexBin _ -> OctHexBin $ '#' : (tail matchResult)
                Keyword _ -> Keyword matchResult
                Punct _ -> Punct matchResult
                RString _ -> formatString matchResult quote
                StringLit _ -> formatString matchResult quote
                x -> x
    Nothing -> Nothing

formatString :: String -> String -> Token
--formatString str quote | trace ("formatString : " ++ str ++ " with quote " ++ (show . length $ quote)) False = undefined
formatString (s:ss) quote = endMatch
  where
    endMatch = if hasCloseQuote
                  then if isR
                          then RString closedStr
                          else StringLit closedStr
                  else interStr

    interStr = getIntermediateStrTok shortInterStr longInterStr begQuote endInBackslash isR

    closedStr = if null quote
                   then drop qCount $ take (length str' - qCount) str'
                   else take (length str' - qCount) str'

    (shortInterStr, longInterStr) = if null quote
                                       then (init $ drop qCount str', drop qCount str')
                                       else if endInBackslash
                                         then (init str', init str')
                                         else (str', str')

    hasCloseQuote = drop (length str' - qCount) str' == begQuote

    begQuote = if null quote
                  then take qCount str'
                  else quote

    endInBackslash = '\\' == (last str')

    qCount = if null quote
                then quoteLen str'
                else length quote

    (str', isR) = if s == 'r'
                     then (ss, True)
                     else ((s:ss), False)

formatString [] _ = StringLit ""

-- Calculate InterStringLit for format(R)String functions
getIntermediateStrTok :: String -> String -> String -> Bool -> Bool -> Token
getIntermediateStrTok shortStr longStr begQuote endInBackslash isR =
  case (begQuote, endInBackslash) of
       ("\"", False) -> Error "String not escaped"
       ("'", False) -> Error "String not escaped"
       ("\"", True) -> tokType shortStr begQuote
       ("'", True) -> tokType shortStr begQuote
       (_, _) -> tokType longStr begQuote
    where tokType = if isR
                       then RStringInt
                       else InterStringLit

quoteLen :: String -> Int
quoteLen str = case take 3 str of
                    "\"\"\"" -> 3
                    "\'\'\'" -> 3
                    '\'':_ -> 1
                    '\"':_ -> 1
                    _ -> 0

-- Remove leading whitespace out of a string
leadingWS :: String -> (Int, String)
leadingWS input = (length $ takeWhile isSpace input, dropWhile isSpace input)

-- -- -- -- -- -- -- --
-- Regex
-- -- -- -- -- -- -- --
-- Pair regular expressions with Token types
tokenRegexs :: [(Regex, Token)]
tokenRegexs = [ (keywordRegex, Keyword "")
              , (punctRegex, Punct "")
              , (indentifierRegex, Id "")
              , (literalRegex, Lit "")
              , (complexRegex, Complex "")
              , (octHexRegex, OctHexBin "")
              , (lineContRegex, LineCont)
              , (commentRegex, Comment)
              , (rStringRegex, RString "")
              , (stringRegex, StringLit "")
              ]

-- -- -- -- -- -- -- --
-- Indentation Stack
-- -- -- -- -- -- -- --
peekStack :: [a] -> a
peekStack (top:_) = top

-- [IndentStack] -> indentLevel -> ([indentTokens], [newIndentStack]
updateStack :: [Int] -> Int -> ([Token], [Int])
updateStack indStack level = case compare (peekStack indStack) level of
                    LT -> ([Indent], level : indStack)
                    GT -> dedentStack indStack level
                    EQ -> ([], indStack)

-- [IndentStack] -> indentLevel -> ([indentTokens], [newIndentStack]
dedentStack :: [Int] -> Int -> ([Token], [Int])
dedentStack (l:ls) newLevel = case compare l newLevel of
              LT -> ([Error "Indentation Error"], (l:ls))
              GT -> ([Dedent] ++ futTokens, newStack) where
                    (futTokens, newStack) = dedentStack ls newLevel
              EQ -> ([], (l:ls))

-- -- -- -- -- --
-- Display
-- -- -- -- -- --
showVal :: Token -> String
showVal (Id x) = "(ID \"" ++ x ++ "\")"
showVal (Lit x) = "(LIT " ++ x ++ ")"
showVal (Keyword x) = "(KEYWORD " ++ x ++ ")"
showVal (Punct x) = "(PUNCT \"" ++ x ++ "\")"
showVal (StringLit x) = "(LIT \"" ++ x ++ "\")"
showVal (InterStringLit x _) = "(STRING \"" ++ x ++ "\")"
showVal (Error x) = "(ERROR \"" ++ x ++ "\")"
showVal (Complex x) = "(LIT " ++ x ++ ")"
showVal (OctHexBin x) = "(LIT " ++ x ++ ")"
showVal (RString x) = "(rLIT \"" ++ x ++ "\")"
showVal (RStringInt x _) = "(rLITint \"" ++ (escapeBackslash x) ++ "\")"
showVal (Indent) = "(INDENT)"
showVal (Dedent) = "(DEDENT)"
showVal (Newline) = "(NEWLINE)"
showVal (Endmarker) = "(ENDMARKER)"
showVal (LineCont) = "(LINECONT)"
showVal (Comment) = "(COMMENT)"

instance Show Token where show = showVal

-- -- -- -- -- -- -- -- --
-- Helper Functions
-- -- -- -- -- -- -- -- --

-- finds first instance of a substring in a string
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace old new xs@(y:ys) =
  case stripPrefix old xs of
    Nothing -> y : replace old new ys
    Just ys' -> new ++ replace old new ys'

-- -- -- -- -- -- -- -- --
-- Escaping Functions
-- -- -- -- -- -- -- -- --

escapeBackslash :: String -> String
escapeBackslash  [] = []
escapeBackslash  ('\\':'\\':cs) = '\\':'\\':(escapeBackslash cs)
escapeBackslash  ('\\':s:cs) = case elem s escapeChars of
                             True -> '\\':s:(escapeBackslash cs)
                             False -> '\\':'\\':s:(escapeBackslash cs)
escapeBackslash ('\"':cs) = '\\':'\"':(escapeBackslash cs)
escapeBackslash ('\'':cs) = '\\':'\'':(escapeBackslash cs)
escapeBackslash (c:cs) = c:(escapeBackslash cs)

escapeRaw :: String -> String
escapeRaw [] = []
escapeRaw ('\\':'"':cs) = '\\':'\\':'\\':'"':(escapeRaw cs)
escapeRaw ('\\':'\'':cs) = '\\':'\\':'\\':'\'':(escapeRaw cs)
escapeRaw ('\\':cs) = '\\':'\\':(escapeRaw cs)
escapeRaw (c:cs) = c:(escapeRaw cs)

escapeChars :: String
escapeChars = "'\"abfnrtvoxNuU"

{-
Phillip Mates
u0284736
-}

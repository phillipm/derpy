module LexerRegex
where

import Data.Char
import DerRegex
import Data.List ((\\))

-- -- -- -- -- -- -- --
-- Regex
-- -- -- -- -- -- -- --
-- Complex number Regex
-- Literal Regex
literalRegex :: Regex
literalRegex = decimalRegex <|> floatNumberRegex

-- Integer Regex
nonZeroDigits :: String
nonZeroDigits = map chr [49..57] -- [1-9]

octDigit :: String
octDigit = map chr [48..55] -- [0-7]

hexDigit :: String
hexDigit = map chr ([48..57] ++ [97..102] ++ [65..70]) -- [0-9 a-f A-F]

binDigit :: String
binDigit = map chr [48..49] -- [0-7]

decimalRegex :: Regex
decimalRegex = (oneOf nonZeroDigits <.> star (oneOf numberCharacters)) <|> (plus (seqOf "0"))

octRegex :: Regex
octRegex = oneOf "0" <.> oneOf "oO" <.> plus (oneOf octDigit)

hexRegex :: Regex
hexRegex = oneOf "0" <.> oneOf "xX" <.> plus (oneOf hexDigit)

octHexRegex :: Regex
octHexRegex = hexRegex <|> octRegex <|> binRegex

binRegex :: Regex
binRegex = oneOf "0" <.> oneOf "bB" <.> plus (oneOf binDigit)

-- Floating Point Regex
floatNumberRegex :: Regex
floatNumberRegex = pointFloatRegex <|> exponentFloatRegex

pointFloatRegex :: Regex
pointFloatRegex = (op intPartRegex) <.> fractionRegex <|> (intPartRegex <.> oneOf ".")

exponentFloatRegex :: Regex
exponentFloatRegex = (intPartRegex <|> pointFloatRegex) <.> exponentRegex

intPartRegex :: Regex
intPartRegex = plus (oneOf numberCharacters)

fractionRegex :: Regex
fractionRegex = oneOf "." <.> intPartRegex

exponentRegex :: Regex
exponentRegex = oneOf "eE"  <.> op (oneOf "+-") <.> intPartRegex

-- Imaginary Regex
complexRegex :: Regex
complexRegex = (floatNumberRegex <|> intPartRegex) <.> oneOf "jJ"


commentRegex :: Regex
commentRegex = oneOf "#"

indentifierRegex :: Regex
indentifierRegex = oneOf alpha_
               <.> star (oneOf (alpha_ ++ numberCharacters))
                        where alpha_ = (lowerCaseCharacters ++ upperCaseCharacters ++ "_")

punctRegex :: Regex
punctRegex =    oneOf "/+-*%&|^~><()[]{},:.;@="
            <|> seqOf "**" -- operators
            <|> seqOf ">>"
            <|> seqOf "<<"
            <|> seqOf "<="
            <|> seqOf ">="
            <|> seqOf "=="
            <|> seqOf "!="
            <|> seqOf "//"
            <|> seqOf "+=" -- delimeters
            <|> seqOf "-="
            <|> seqOf "&="
            <|> seqOf "|="
            <|> seqOf "^="
            <|> seqOf "*="
            <|> seqOf "/="
            <|> seqOf "//="
            <|> seqOf "%="
            <|> seqOf "**="
            <|> seqOf "<<="
            <|> seqOf ">>="

keywordRegex :: Regex
keywordRegex =   seqOf "False"
             <|> seqOf "True"
             <|> seqOf "def"
             <|> seqOf "None"
             <|> seqOf "and"
             <|> seqOf "as"
             <|> seqOf "assert"
             <|> seqOf "break"
             <|> seqOf "class"
             <|> seqOf "continue"
             <|> seqOf "def"
             <|> seqOf "del"
             <|> seqOf "elif"
             <|> seqOf "else"
             <|> seqOf "except"
             <|> seqOf "finally"
             <|> seqOf "for"
             <|> seqOf "from"
             <|> seqOf "global"
             <|> seqOf "if"
             <|> seqOf "import"
             <|> seqOf "in"
             <|> seqOf "is"
             <|> seqOf "lambda"
             <|> seqOf "nonlocal"
             <|> seqOf "not"
             <|> seqOf "or"
             <|> seqOf "pass"
             <|> seqOf "raise"
             <|> seqOf "return"
             <|> seqOf "try"
             <|> seqOf "while"
             <|> seqOf "with"
             <|> seqOf "yield"

-- String Regex
quoteRegex :: Regex
quoteRegex = seqOf "\"\"\"" <|> seqOf "\'\'\'" <|> oneOf "\"" <|> oneOf "\'"

lineContRegex :: Regex
lineContRegex = seqOf "\\"

stringRegex :: Regex
stringRegex = doubleQuoteLongRegex <|> doubleQuoteShortRegex <|>
              singleQuoteLongRegex <|> singleQuoteShortRegex

rStringRegex :: Regex
rStringRegex = rdoubleQuoteLongRegex <|> rdoubleQuoteShortRegex <|>
               rsingleQuoteLongRegex <|> rsingleQuoteShortRegex

-- Short String
doubleQuoteShortRegex :: Regex
doubleQuoteShortRegex =
  lit '"'
  <.>
  (star $
    (oneOf $ (allInputCharacters  ++ allWhitespaceCharacters) \\ "\"")
    <|> seqOf "\\\"")
  <.>
  (lit '"' <|> lit '\\')

singleQuoteShortRegex :: Regex
singleQuoteShortRegex =
  lit '\''
  <.>
  (star $
    (oneOf $ (allInputCharacters  ++ allWhitespaceCharacters) \\ "\'")
    <|> seqOf "\\\'")
  <.>
  (lit '\'' <|> lit '\\')

rsingleQuoteShortRegex :: Regex
rsingleQuoteShortRegex = oneOf ("rR") <.> singleQuoteShortRegex

rdoubleQuoteShortRegex :: Regex
rdoubleQuoteShortRegex = oneOf ("rR") <.> doubleQuoteShortRegex

-- Long String
singleQuoteLongRegex :: Regex
singleQuoteLongRegex =
  seqOf "'''" <.>
  (star $ oneOf $ (allInputCharacters  ++ allWhitespaceCharacters))
  <|> seqOf "'''"

doubleQuoteLongRegex :: Regex
doubleQuoteLongRegex = seqOf "\"\"\"" <.>
  (star $ oneOf $ (allInputCharacters  ++ allWhitespaceCharacters))
  <|> seqOf "\"\"\""

rsingleQuoteLongRegex :: Regex
rsingleQuoteLongRegex = oneOf ("rR") <.> singleQuoteLongRegex

rdoubleQuoteLongRegex :: Regex
rdoubleQuoteLongRegex = oneOf ("rR") <.> doubleQuoteLongRegex

-- Long String Cont.
singleQuoteLongContRegex :: Regex
singleQuoteLongContRegex =
  (star $ oneOf $ (allInputCharacters  ++ allWhitespaceCharacters))
  <.> (lit '\n' <|> seqOf "'''")

doubleQuoteLongContRegex :: Regex
doubleQuoteLongContRegex =
  (star $ oneOf $ (allInputCharacters ++ allWhitespaceCharacters))
  <.> (lit '\n' <|> seqOf "\"\"\"")

-- Short String Cont
doubleQuoteShortContRegex :: Regex
doubleQuoteShortContRegex =
  (star $
    (oneOf $ (allInputCharacters  ++ allWhitespaceCharacters) \\ "\"")
    <|> seqOf "\\\"")
  <.>
  (lit '"' <|> lit '\\')

singleQuoteShortContRegex :: Regex
singleQuoteShortContRegex =
  (star $
    (oneOf $ (allInputCharacters  ++ allWhitespaceCharacters) \\ "\'")
    <|> seqOf "\\\'")
  <.>
  (lit '\'' <|> lit '\\')

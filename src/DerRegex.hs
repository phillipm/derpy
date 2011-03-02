{-# LANGUAGE TemplateHaskell #-}
{-
Phillip Mates
u0284736
-}

-- David Darais' Derivative Regex Library
-- The Regex module provides all the tools necessary for building and matching
-- regular expressions.

module DerRegex
  ( Regex
  , regexNullable, regexIsNone
  , lit, cat, (<.>), alt, (<|>), star, ep, none, op, plus
  , oneOf, seqOf
  , der, match
  , lowerCaseCharacters
  , upperCaseCharacters
  , numberCharacters
  , allInputCharacters
  , allWhitespaceCharacters
  -- , derRegexTests
  ) where

import Data.Char
-- import Test.HUnit

-- The Regex data type.  These constructors should never be used directly.
-- Instead regex builder functions should be used to ensure large bloaty
-- regexes are not formed.
data Regex =
    RegexCat  Regex Regex
  | RegexOr   Regex Regex
  | RegexStar Regex
  | RegexLit  Char
  | RegexEps
  | RegexNone
  deriving (Eq, Show)

-- Regex Nullability
regexNullable :: Regex -> Bool
regexNullable (RegexCat lhs rhs) = regexNullable lhs && regexNullable rhs
regexNullable (RegexOr lhs rhs) = regexNullable lhs || regexNullable rhs
regexNullable (RegexStar _) = True
regexNullable (RegexLit _) = False
regexNullable RegexEps = True
regexNullable RegexNone = False

-- Regex Equivalence to None
regexIsNone :: Regex -> Bool
regexIsNone (RegexCat lhs rhs) = regexIsNone lhs || regexIsNone rhs
regexIsNone (RegexOr lhs rhs) = regexIsNone lhs && regexIsNone rhs
regexIsNone (RegexStar _) = False
regexIsNone (RegexLit _) = False
regexIsNone RegexEps = False
regexIsNone RegexNone = True

-- Regex builders
cat :: Regex -> Regex -> Regex
cat x RegexEps = x
cat RegexEps y = y
cat _ RegexNone = RegexNone
cat RegexNone _ = RegexNone
cat (RegexCat x1 x2) y = cat x1 (RegexCat x2 y)
cat x y = RegexCat x y

(<.>) :: Regex -> Regex -> Regex
(<.>) = cat

alt :: Regex -> Regex -> Regex
alt x RegexEps | regexNullable x = x
alt RegexEps y | regexNullable y = y
alt x RegexNone = x
alt RegexNone y = y
alt (RegexOr x1 x2) y = alt x1 (RegexOr x2 y)
alt x y = RegexOr x y

(<|>) :: Regex -> Regex -> Regex
(<|>) = alt

star :: Regex -> Regex
star (RegexStar r) = RegexStar r
star r = RegexStar r

lit :: Char -> Regex
lit = RegexLit

ep :: Regex
ep = RegexEps

none :: Regex
none = RegexNone

op :: Regex -> Regex
op = alt ep

plus :: Regex -> Regex
plus r = r <.> star r

oneOf :: String -> Regex
oneOf = foldr (alt . lit) none

seqOf :: String -> Regex
seqOf = foldr (cat . lit) ep

-- Regex Derivative
der :: Char -> Regex -> Regex
der c (RegexLit rc) | c == rc   = ep
                    | otherwise = none
der c (RegexCat lhs rhs) | regexNullable lhs = (der c lhs <.> rhs) <|> der c rhs
                         | otherwise         = der c lhs <.> rhs
der c (RegexOr lhs rhs) = der c lhs <|> der c rhs
der c (RegexStar e) = der c e <.> star e
der _ RegexEps = none
der _ RegexNone = none

-- Regex Matching (longest)
match :: Regex -> String -> Maybe (String, Int, String)
match rex _ | regexIsNone rex = Nothing
match rex [] | regexNullable rex = Just ([], 0, [])
match _ [] | otherwise = Nothing
match rex (x:xs) =
  case match (x `der` rex) xs of
    Just (matchResult, matchlen, rest) -> Just (x:matchResult, matchlen + 1, rest)
    Nothing ->
      if regexNullable rex
        then Just ([], 0, x:xs)
        else Nothing

-- Useful character classes
lowerCaseCharacters :: String
lowerCaseCharacters = map chr [97..122] -- [a-z]
upperCaseCharacters :: String
upperCaseCharacters = map chr [65..90] -- [A-Z]
numberCharacters :: String
numberCharacters = map chr [48..57] -- [0-9]
allInputCharacters :: String
allInputCharacters = map chr [33..126] -- all letters, numbers, and symbols
allWhitespaceCharacters :: String
allWhitespaceCharacters = " \r\n\t"

{-
-- Regex Testing
derRegexTests :: Test
derRegexTests = TestList
  [ TestCase . assertBool "" $ seqOf "abc" `match` "abc" == Just ("abc", 3, "")
  , TestCase . assertBool "" $ seqOf "ab" `match` "abc" == Just ("ab", 2, "c")
  , TestCase . assertBool "" $ seqOf "ab" `match` "acb" == Nothing
  , TestCase . assertBool "" $ seqOf "" `match` "abc" == Just ("", 0, "abc")
  , TestCase . assertBool "" $ star (seqOf "a") `match` "aaaaab" == Just ("aaaaa", 5, "b")
  , TestCase . assertBool "" $ star (seqOf "a") `match` "bbbbba" == Just ("", 0, "bbbbba")
  , TestCase . assertBool "" $ op (seqOf "a") `match` "aaaaab" == Just ("a", 1, "aaaab")
  , TestCase . assertBool "" $ op (seqOf "a") `match` "bbbbba" == Just ("", 0, "bbbbba")
  , TestCase . assertBool "" $ plus (seqOf "a") `match` "bbbbba" == Nothing
  , TestCase . assertBool "" $ plus (seqOf "a") `match` "aaaaab" == Just ("aaaaa", 5, "b")
  , TestCase . assertBool "" $ seqOf "abc" <.> seqOf "xyz" `match` "abcxyzabcxyz" == Just ("abcxyz", 6, "abcxyz")
  , TestCase . assertBool "" $ seqOf "abc" <|> seqOf "xyz" `match` "abcxyzabcxyz" == Just ("abc", 3, "xyzabcxyz")
  , TestCase . assertBool "" $ seqOf "abc" <|> seqOf "xyz" `match` "xyzabcxyzabc" == Just ("xyz", 3, "abcxyzabc")
  , TestCase . assertBool "" $ star (seqOf "abc") <.> seqOf "x" `match` "abcabcd" == Nothing
  , TestCase . assertBool "" $ star (seqOf "abc") <.> seqOf "x" `match` "abcabcxd" == Just ("abcabcx", 7, "d")
  , TestCase . assertBool "" $ star (seqOf "abc") <.> op (seqOf "x") `match` "abcabcd" == Just ("abcabc", 6, "d")
  ]
  -}

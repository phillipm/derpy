-- Haskell imports
import System.IO (hGetContents, stdin)
import Data.Set (toList)

import qualified PythonLexer as Pylex
import PythonParser

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
toToken (Pylex.Complex s) = Token "lit" s
toToken (Pylex.OctHexBin s) = Token "lit" s
toToken (Pylex.StringLit s) = Token "string" s
toToken (Pylex.Error s) = Token "error" s
toToken Pylex.Newline = Token "newline" "newline"
toToken Pylex.Indent = Token "indent" "indent"
toToken Pylex.Dedent = Token "dedent" "dedent"
toToken Pylex.Endmarker = Token "endmarker" "endmarker"
toToken Pylex.LineCont = Token "linecont" "linecont"
toToken _ = error "incorrect input token from lexer"


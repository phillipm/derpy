-- Phillip Mates u0284736

-- Haskell imports
import System.Environment
import System.IO (hGetContents, hGetLine, stdin)

import qualified PythonLexer as Pylex

main :: IO ()
main = (hGetContents stdin) >>= Pylex.showLexOutput . Pylex.lexInput

-- Phillip Mates u0284736

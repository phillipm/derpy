-- Haskell imports
import System.IO (hGetContents, stdin)

import qualified PythonLexer as Pylex

main :: IO ()
main = (hGetContents stdin) >>= Pylex.showLexOutput . Pylex.lexInput

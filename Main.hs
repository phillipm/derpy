
-- Haskell imports
import System.Environment
import System.IO (hGetContents, hGetLine, stdin)

import qualified PythonLexer as Pylex
import PythonParser

--import DerParser

-- TODO: take in -l flag to specify between prelexed input
main :: IO ()
main = (hGetContents stdin) >>= Pylex.showLexOutput . Pylex.lexInput

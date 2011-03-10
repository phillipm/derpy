{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances, UndecidableInstances #-}

module Text.Derp
  ( -- * Data Types
    Parser, Token(..)
  , -- * Parser construction
    (<|>), (<~>), (==>), (==>|), nul, pzip, ter, eps, epsM, emp
  , -- * Parser computation steps
    derive, compact, parseNull
  , -- * Full parsing and result extraction
    defaultCompactSteps, compactNum, deriveStepNum, runParseNum
  , deriveStep, runParse
  , -- * Demos
    xsR, xsL, xsIn, parens, parensIn, amb, ambIn, sexp, sexpIn

  ) where

import Control.Monad
import Data.Function
import Data.IORef
import Data.List
import Data.Map (Map)
import System.IO.Unsafe
import System.Mem.StableName
import Text.Printf
import Unsafe.Coerce
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | Represents both a formal context-free language and the
--   reduction of a member of that language to a value of type `a'.

--   Languages range of `Token' values.

data Parser a = Parser
  { parserRec      :: ParserRec Parser a
  , parserNullable :: FPValue Bool
  , parserDerive   :: Token -> Parser a
  , parserCompact  :: Parser a
  }

data ParserRec p a where
  Alt :: (Ord a)        => p a -> p a -> ParserRec p a
  Con :: (Ord a, Ord b) => p a -> p b -> ParserRec p (a, b)
  Red :: (Ord a, Ord b) => (Set a -> Set b) -> p a -> ParserRec p b
  Nul :: (Ord a)        => p a -> ParserRec p a
  Zip :: (Ord a, Ord b) => p a -> ContextR p a b -> ParserRec p b
  Ter ::                   String -> ParserRec p String
  Eps :: (Ord a)        => Set a -> ParserRec p a
  Emp :: (Ord a)        => ParserRec p a

data ContextR p a b where
  ConContext :: (Ord a, Ord b) => p b -> ContextR p (a, b) c -> ContextR p a c
  RedContext :: (Ord a, Ord b) => (Set a -> Set b) -> ContextR p b c -> ContextR p a c
  TopContext :: (Ord a)        => ContextR p a a

type Context a b = ContextR Parser a b

-- | The input type for parsing.  For example the parser:
--
-- > (ter "x")
--
--   will parse:
--
-- > (Token "x" "foo")
--
--   into:
--
-- > (eps "foo")

data Token = Token { tokenClass :: String, tokenValue :: String }
  deriving (Eq, Ord, Show)

parser :: (Ord a) => ParserRec Parser a -> FPValue Bool -> Parser a
parser p n = fix $ \ self -> Parser p n (memoFun (deriveImp self)) (compactImp self)

-- | Alternation.
(<|>) :: (Ord a) => Parser a -> Parser a -> Parser a
(<|>) a b = parser (Alt a b) FPUndecided
-- | Concatenation.
(<~>) :: (Ord a, Ord b) => Parser a -> Parser b -> Parser (a, b)
(<~>) a b = parser (Con a b) FPUndecided
-- | Reduction.
(==>) :: (Ord a, Ord b) => Parser a -> (a -> b) -> Parser b
(==>) p f = p ==>| Set.map f
-- | Set generalized version of `==>'.
(==>|) :: (Ord a, Ord b) => Parser a -> (Set a -> Set b) -> Parser b
(==>|) p f = parser (Red f p) FPUndecided
-- | Null-parse extraction.
nul :: (Ord a) => Parser a -> Parser a
nul p = parser (Nul p) FPUndecided
-- | One-hole-context focus.
pzip :: (Ord a, Ord b) => Parser a -> Context a b -> Parser b
pzip p c = parser (Zip p c) (FPDecided False)
-- | Terminal.
ter :: String -> Parser String
ter t = parser (Ter t) (FPDecided False)
-- | Epsilon/empty-string.
eps :: (Ord a) => a -> Parser a
eps = epsM . Set.singleton
-- | Set generalized version of `eps'.
epsM :: (Ord a) => Set a -> Parser a
epsM e = parser (Eps e) (FPDecided True)
-- | The empty language.
emp :: (Ord a) => Parser a
emp = parser Emp (FPDecided False)

infixr 3 <~>
infixr 1 <|>
infix 2 ==>, ==>|

-- | The main derivative function.

derive :: Parser a -> Token -> Parser a
derive = parserDerive

deriveImp :: Parser a -> Token -> Parser a
deriveImp p' x' = deriveImpRec (parserRec p') x'
  where
    deriveImpRec (Alt a b) x = derive a x <|> derive b x
    deriveImpRec (Con a b) x = derive a x <~> b <|> nul a <~> derive b x
    deriveImpRec (Red f a) x = derive a x ==>| f
    deriveImpRec (Nul _) _ = emp
    deriveImpRec (Zip p c) t = pzip (derive p t) c
    deriveImpRec (Ter c) (Token x t) | c == x = eps t | otherwise = emp
    deriveImpRec (Eps _) _ = emp
    deriveImpRec Emp _ = emp

-- | The optimization step of the algorithm.

compact :: Parser a -> Parser a
compact = parserCompact

compactImp :: (Ord a) => Parser a -> Parser a
compactImp p = compactImpRec $ parserRec p
  where
    compactImpRec (Alt (Parser Emp _ _ _) (Parser Emp _ _ _)) = emp
    compactImpRec (Alt (Parser Emp _ _ _) b) = compact b
    compactImpRec (Alt a (Parser Emp _ _ _)) = compact a
    compactImpRec (Alt (Parser (Eps sM) _ _ _) (Parser (Eps tM) _ _ _)) = epsM (sM `Set.union` tM)
    compactImpRec (Alt a b) = (compact a <|> compact b) { parserNullable = parserNullable a <||> parserNullable b }
    compactImpRec (Con (Parser Emp _ _ _) _) = emp
    compactImpRec (Con _ (Parser Emp _ _ _)) = emp
    compactImpRec (Con (Parser (Eps sM) _ _ _) b) = compact b ==>| (\ xM -> Set.fromList [ (s, x) | s <- Set.toList sM, x <- Set.toList xM ])
    compactImpRec (Con a (Parser (Eps sM) _ _ _)) = compact a ==>| (\ xM -> Set.fromList [ (x, s) | x <- Set.toList xM, s <- Set.toList sM ])
    compactImpRec (Con a b) | parserNullable a == FPDecided False && parserNullable b == FPDecided False
      = pzip (compact a) (ConContext (compact b) TopContext)
    compactImpRec (Con a b) = (compact a <~> compact b) { parserNullable = parserNullable a <&&> parserNullable b }
    compactImpRec (Red _ (Parser Emp _ _ _)) = emp
    compactImpRec (Red f (Parser (Eps sM) _ _ _)) = epsM (f sM)
    compactImpRec (Red f (Parser (Red g a) _ _ _)) = compact a ==>| f . g
    compactImpRec (Red f a) = (compact a ==>| f) { parserNullable = parserNullable a }
    compactImpRec (Nul (Parser (Con a b) _ _ _)) = nul (compact a) <~> nul (compact b)
    compactImpRec (Nul (Parser (Alt a b) _ _ _)) = nul (compact a) <|> nul (compact b)
    compactImpRec (Nul (Parser (Red f a) _ _ _)) = nul (compact a) ==>| f
    compactImpRec (Nul (Parser (Zip a c) _ _ _)) = pzip (nul a) (nulContext c)
    compactImpRec (Nul a@(Parser (Nul _) _ _ _)) = compact a
    compactImpRec (Nul (Parser (Eps sM) _ _ _)) = epsM sM
    compactImpRec (Nul (Parser (Ter _) _ _ _)) = emp
    compactImpRec (Nul (Parser Emp _ _ _)) = emp
    compactImpRec (Zip a TopContext) = compact a
    compactImpRec (Zip (Parser Emp _ _ _) _) = emp
    compactImpRec (Zip a c) | parserNullable a /= FPDecided False = unfoldOne (compactImp a) c
    compactImpRec (Zip (Parser (Zip a c) _ _ _) d) = pzip (compact a) (thread c d)
    compactImpRec (Zip (Parser (Red f a) _ _ _) c) = pzip (compact a) (RedContext f c)
    compactImpRec (Zip a c) = pzip (compact a) c
    compactImpRec (Ter _) = p
    compactImpRec (Eps sM) | sM == Set.empty = emp
    compactImpRec (Eps _) = p
    compactImpRec Emp = p

    nulContext :: Context a b -> Context a b
    nulContext (ConContext a c) = ConContext (nul a) (nulContext c)
    nulContext (RedContext f c) = RedContext f (nulContext c)
    nulContext TopContext = TopContext

    thread :: (Ord a, Ord b, Ord c) => Context a b -> Context b c -> Context a c
    thread TopContext d = d
    thread (RedContext f c) d = RedContext f (thread c d)
    thread (ConContext a c) d = ConContext a (thread c d)

    unfoldOne :: (Ord a, Ord b) => Parser a -> Context a b -> Parser b
    unfoldOne a (ConContext b c) = pzip (a <~> b) c
    unfoldOne a (RedContext f c) = unfoldOne (a ==>| f) c
    unfoldOne _ TopContext = error "cannot unfold top"

-- | Extract the parse-null set of a parser.

parseNull :: (Ord a) => Parser a -> Set a
parseNull p = work $ nul p
  where
    work (Parser (Eps sM) _ _ _) = sM
    work (Parser Emp _ _ _) = Set.empty
    work other = work $ compact other

-- running parsers

-- | A specified number of compactions.
compactNum :: Int -> Parser a -> Parser a
compactNum 0 p = p
compactNum n p = compactNum (n - 1) (compact p)

-- | Derivation followed by specified number of compactions.
deriveStepNum :: Int -> Parser a -> Token -> Parser a
deriveStepNum n p i = compactNum n $ derive p i

-- | Parse using a specified number of intermediate compactions.
runParseNum :: (Ord a) => Int -> Parser a -> [Token] -> Set a
runParseNum _ p [] = parseNull p
runParseNum n p (i:is) = runParseNum n (deriveStepNum n p i) is

-- | The number of compact steps that usually keeps a parser constant in size
--   while parsing.
defaultCompactSteps :: Int
defaultCompactSteps = 10

-- | Derivation followed by the default number of compactions.
deriveStep :: Parser a -> Token -> Parser a
deriveStep = deriveStepNum defaultCompactSteps

-- | Parse using the default number of intermediate compactions.  This is the
--   main parsing function.  Examples:
--
-- > let e =     ter "num"
-- >         <|> e <~> ter "+" <~> e ==> (\(x1,(o,x2)) -> "(" ++ x1 ++ o ++ x2 ++ ")")
-- > in runParse e [Token "num" "1", Token "+" "+", Token "num" "3", Token "+" "+", Token "num" "5"]
--
-- evaluates to:
--
-- > Set.fromList ["((1+3)+5)", "(1+(3+5))"]
--
-- > let e =     ter "num" ==> read
-- >         <|> e <~> ter "+" <~> e ==> (\(x1,(_,x2)) -> x1 + x2)
-- > in runParse e [Token "num" "1", Token "+" "+", Token "num" "3", Token "+" "+", Token "num" "5"]
--
-- evaluates to:
--
-- > Set.fromList [9]
--
runParse :: (Ord a) => Parser a -> [Token] -> Set a
runParse = runParseNum defaultCompactSteps

-- inspecting parsers

parserChildren :: Parser a -> [GenParser]
parserChildren = parserRecChildren . parserRec
  where
    parserRecChildren (Con a b) = [genParser a, genParser b]
    parserRecChildren (Alt a b) = [genParser a, genParser b]
    parserRecChildren (Red _ a) = [genParser a]
    parserRecChildren (Nul a)   = [genParser a]
    parserRecChildren (Zip a _) = [genParser a]
    parserRecChildren (Ter _)   = []
    parserRecChildren (Eps _)   = []
    parserRecChildren Emp       = []

foldlParserChildrenM :: (forall b. t -> Parser b -> IO t) -> t -> Parser a -> IO t
foldlParserChildrenM f i p = foldM g i $ parserChildren p
  where
    g t (GenParser h) = h (f t)

newtype GenParser = GenParser { unGenParser :: forall c. (forall b. Parser b -> c) -> c }

genParser :: Parser a -> GenParser
genParser p = GenParser $ \ f -> f p

runGenParser :: (forall b. Parser b -> c) -> GenParser -> c
runGenParser f g = unGenParser g f

data ParserRecType = ConType | AltType | RedType | NulType | ZipType | TerType | EpsType | EmpType
  deriving (Eq, Ord, Show)

parserType :: Parser a -> ParserRecType
parserType = parserRecType . parserRec
  where
    parserRecType (Con _ _) = ConType
    parserRecType (Alt _ _) = AltType
    parserRecType (Red _ _) = RedType
    parserRecType (Nul _)   = NulType
    parserRecType (Zip _ _) = ZipType
    parserRecType (Ter _)   = TerType
    parserRecType (Eps _)   = EpsType
    parserRecType Emp       = EmpType

type ParserInspect t =  (forall a. Parser a -> IO Integer)
                     -> (forall a. Parser a -> IO Bool)
                     -> (forall a. Parser a -> IO t)

inspectParser :: ParserInspect t -> Parser a -> t
inspectParser f p = unsafePerformIO $ do
  reifiedPt <- newIORef Map.empty
  seenPt    <- newIORef Map.empty
  uidPt     <- newIORef 1
  f (lookupId reifiedPt uidPt) (seenId seenPt) p

lookupId :: IORef (Map Int [(StableName (), Integer)])
         -> IORef Integer
         -> Parser a
         -> IO Integer
lookupId reifiedPt uidPt p
  | p `seq` True = do
    stblName <- genericStableName p
    let stblNameHashed = hashStableName stblName
    lookupValM <- liftM (extraLookup stblNameHashed stblName) $ readIORef reifiedPt
    case lookupValM of
      (Just lookupVal) -> return lookupVal
      Nothing          -> do
        thisId <- readIORef uidPt
        modifyIORef uidPt (+ 1)
        modifyIORef reifiedPt $ Map.insertWith (++) stblNameHashed [(stblName, thisId)]
        return thisId
  | otherwise = error "seq failed"

seenId :: IORef (Map Int [(StableName (), ())]) -> Parser a -> IO Bool
seenId seenPt p
  | p `seq` True = do
    stblName <- genericStableName p
    let stblNameHashed = hashStableName stblName
    lookupValM <- liftM (extraLookup stblNameHashed stblName) $ readIORef seenPt
    case lookupValM of
      (Just ()) -> return True
      Nothing -> do
        modifyIORef seenPt $ Map.insertWith (++) stblNameHashed [(stblName, ())]
        return False
  | otherwise = error "seq failed"

genericStableName :: a -> IO (StableName ())
genericStableName = liftM unsafeCoerce . makeStableName

extraLookup :: Int -> StableName () -> Map Int [(StableName (), a)] -> Maybe a
extraLookup hashed key m = process $ Map.lookup hashed m
  where
    process x = case x of
      (Just [])                                 -> Nothing
      (Just ((key', reified):xs)) | key == key' -> Just reified
                                  | otherwise   -> process (Just xs)
      Nothing                                   -> Nothing

type ParserFoldL t = forall a. t -> Parser a -> Integer -> Integer -> [Integer] -> t

parserDeepFoldL :: ParserFoldL t -> t -> Parser a -> t
parserDeepFoldL f i = inspectParser $ inspectf f i

inspectf :: ParserFoldL t -> t -> ParserInspect t
inspectf f i uidM isSeenM p = do
  isSeen <- isSeenM p
  if isSeen then return i else do
    uid <- uidM p
    cuids <- mapM (runGenParser uidM) $ parserChildren p
    let pid = hashStableName (unsafePerformIO (genericStableName p))
    let next = f i p uid (fromIntegral pid) cuids
    foldlParserChildrenM (\t p' -> inspectf f t uidM isSeenM p') next p

data ParserInfo = ParserInfo Integer -- uid
                             Integer -- pid
                             ParserRecType -- type
                             (FPValue Bool) -- nullable
                             [Integer] -- children

parserToGraph :: Parser a -> [ParserInfo]
parserToGraph = reverse . parserDeepFoldL f []
  where
    f :: ParserFoldL [ParserInfo]
    f others p uid pid childrenids = ParserInfo uid
                                                pid
                                                (parserType p)
                                                (parserNullable p)
                                                childrenids
                                   : others

showParserGraph :: [ParserInfo] -> String
showParserGraph ps = printf "SIZE: %s \n" (show (length ps)) ++ intercalate "\n" (map showParserGraphSingle ps)
  where
    showParserGraphSingle :: ParserInfo -> String
    showParserGraphSingle (ParserInfo uid pid ptype n children) =
      printf "%-6s%-6s%-10s%-10s%-10s"
             (show uid)
             (show pid)
             (show ptype)
             (showFPBool n)
             (show children)

instance Show (Parser a) where
  show = showParserGraph . parserToGraph

-- FPValue

data FPValue a = FPDecided a | FPUndecided
  deriving (Eq, Ord, Show)

showFPBool :: FPValue Bool -> String
showFPBool (FPDecided True) = "True"
showFPBool (FPDecided False) = "False"
showFPBool FPUndecided = "Undecided"

(<&&>) :: FPValue Bool -> FPValue Bool -> FPValue Bool
(<&&>) (FPDecided False) _ = FPDecided False
(<&&>) _ (FPDecided False) = FPDecided False
(<&&>) FPUndecided _ = FPUndecided
(<&&>) _ FPUndecided = FPUndecided
(<&&>) (FPDecided x) (FPDecided y) = FPDecided (x && y)

(<||>) :: FPValue Bool -> FPValue Bool -> FPValue Bool
(<||>) (FPDecided True) _ = FPDecided True
(<||>) _ (FPDecided True) = FPDecided True
(<||>) FPUndecided _ = FPUndecided
(<||>) _ FPUndecided = FPUndecided
(<||>) (FPDecided x) (FPDecided y) = FPDecided (x || y)

-- util


memoFun :: (Ord a) => (a -> b) -> a -> b
memoFun f = unsafePerformIO $ do
  mapRef <- newIORef Map.empty
  return $ \a -> unsafePerformIO $ do
  currMap <- readIORef mapRef
  let vM = Map.lookup a currMap
  case vM of
    Just b -> return b
    Nothing -> do
      let b = f a
      writeIORef mapRef $ Map.insert a b currMap
      return b

-- demos

xsR :: () -> Parser String
xsR () = p
  where
    p = eps "" <|> ter "x" <~> p ==> uncurry (++)

xsL :: () -> Parser String
xsL () = p
  where
    p = eps "" <|> p <~> ter "x" ==> uncurry (++)

xsIn :: [Token]
xsIn = replicate 60 (Token "x" "x")

parens :: () -> Parser String
parens () = p
  where
    p = eps "" <|> ter "(" <~> p <~> ter ")" ==> (\(s1,(s2,s3)) -> s1 ++ s2 ++ s3)

parensIn :: [Token]
parensIn = replicate 80 (Token "(" "(") ++ replicate 80 (Token ")" ")")

amb :: () -> Parser String
amb () = p
  where
    p = ter "1" <|> p <~> ter "+" <~> p ==> (\(s1,(s2,s3)) -> "(" ++ s1 ++ s2 ++ s3 ++ ")")

ambIn :: [Token]
ambIn = intersperse (Token "+" "+") (replicate 7 (Token "1" "1"))

sexp :: () -> Parser String
sexp () = p
  where
    p = ter "(" <~> pl <~> ter ")" ==> (\(s1,(s2,s3)) -> s1 ++ s2 ++ s3) <|> ter "s"
    pl = p <~> pl ==> uncurry (++) <|> eps ""

sexpIn :: [Token]
sexpIn = map (\x -> Token x x) $ words "( s ( s ( s s ( s s s ( s s s ( s ) ( s s ) s s ) s s ) s ) s ) )"


--  --                                                          ; {{{1
--
--  File        : Koneko/Data.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-09-20
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

                                                              --  {{{1
-- |
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Maybe
-- >>> let id = fromJust . ident; q = KQuot . id
--
-- >>> nil
-- nil
-- >>> false
-- #f
-- >>> true
-- #t
-- >>> int 42
-- 42
-- >>> float (-1.23)
-- -1.23
-- >>> kwd "foo"
-- :foo
-- >>> pair (Kwd "answer") $ int 42
-- :answer 42 =>
-- >>> list [int 42, kwd "foo"]
-- ( 42 :foo )
-- >>> KIdent $ id "foo"
-- foo
-- >>> q "foo"
-- 'foo
-- >>> block [id "x", id "y"] [q "y", q "x"] undefined
-- [ x y . 'y 'x ]
--
-- >>> str "I like 猫s"
-- "I like 猫s"
--

                                                              --  }}}1

module Koneko.Data (
  Identifier, Module, PopResult, Kwd(..), Ident, List(..), Block(..),
  Scope(..), Context(..), Pair(..), KPrim(..), KValue(..), Stack,
  unIdent, ident, escapeFrom, escapeTo, initStack, Push, push', push,
  Pop, pop, pop', initContext, forkContext, forkScope, lookup, nil,
  false, true, bool, int, float, str, kwd, pair, list, block, Val, val
) where

import Data.Char (isPrint, ord)
import Data.List (intercalate)
import Data.Text.Lazy (Text)
import Numeric (showHex)
import Prelude hiding (lookup)

import qualified Data.HashMap.Lazy as H
import qualified Data.HashTable.IO as HT
import qualified Data.Text.Lazy as T
import qualified Prelude as P

import Koneko.Misc (isIdent)

-- TODO:
--  * Quoted Ident
--  * Dict
--  * Record
--  * RawBlock vs Quoted Block

-- types --

type HashTable k v      = HT.BasicHashTable k v
type ModuleLookupTable  = HashTable Identifier KValue
type ScopeLookupTable   = H.HashMap Identifier KValue

type Identifier         = Text
type Module             = ModuleLookupTable
type PopResult a        = Either String (a, Stack)

newtype Kwd = Kwd { unKwd :: Identifier }
  deriving (Eq, Ord)

newtype Ident = Ident { unIdent :: Identifier }
  deriving (Eq, Ord)

newtype List = List { unList :: [KValue] }
  deriving (Eq, Ord)

data Block = Block {
  blkArgs   :: [Ident],
  blkCode   :: [KValue],
  blkScope  :: Maybe Scope
}

data Scope = Scope {
  parent  :: Either Identifier Scope,
  table   :: ScopeLookupTable
}

data Context = Context {
  modules   :: HashTable Identifier Module,
  ctxScope  :: Scope
}

-- TODO
data Pair = Pair { key :: Kwd, value :: KValue }
  deriving (Eq, Ord)

-- TODO: + Rx
data KPrim
    = KNil | KBool Bool | KInt Integer | KFloat Double
    | KStr Text | KKwd Kwd
  deriving (Eq, Ord)

-- TODO
data KValue
    = KPrim KPrim
    | KPair Pair
    | KList List
    | KIdent Ident
    | KQuot Ident                                             --  TODO
    | KBlock Block
  deriving (Eq, Ord)

type Stack = [KValue]

-- instances --

-- TODO
instance Eq Block where
  Block a c _ == Block a' c' _ = (a,c) == (a',c')

-- TODO
instance Ord Block where
  Block a c _ `compare` Block a' c' _ = (a,c) `compare` (a',c')

instance Show Kwd where
  show (Kwd s) = ":" ++ if isIdent s then T.unpack s else show s

instance Show Ident where
  show (Ident s) = T.unpack s

instance Show List where
  show (List [])  = "()"                                      --  TODO
  show (List l)   = "( " ++ intercalate " " (map show l) ++ " )"

-- TODO
instance Show Block where
  show (Block a c _) = case (a,c) of
      ([], [])  -> "[ ]"
      ([], _ )  -> "[ " ++ cs ++ " ]"
      (_ , [])  -> "[ " ++ as ++ " . ]"
      (_ , _ )  -> "[ " ++ as ++ " . " ++ cs ++ " ]"
    where
      as = T.unpack $ T.intercalate " " (map unIdent a)
      cs = intercalate " " (map show c)

-- TODO
instance Show Pair where
  show (Pair k v) = show k ++ " " ++ show v ++ " =>"

instance Show KPrim where
  show KNil       = "nil"
  show (KBool b)  = if b then "#t" else "#f"
  show (KInt i)   = show i
  show (KFloat f) = show f
  show (KStr s)   = showStr s
  show (KKwd k)   = show k

-- TODO
instance Show KValue where
  show (KPrim p)  = show p
  show (KPair p)  = show p
  show (KList l)  = show l
  show (KIdent i) = show i
  show (KQuot x)  = "'" ++ show x
  show (KBlock b) = show b

ident :: Text -> Maybe Ident
ident s = if isIdent s then Just $ Ident s else Nothing

showStr :: Text -> String
showStr s = T.unpack $ T.concat ["\"", T.concatMap f s, "\""]
  where
    f c = maybe (g c) id $ P.lookup c bsl
    g c = if isPrint c then T.singleton c else h (ord c)
    h n = let (p,m) = if n <= 0xffff then ("\\u",4) else ("\\U",8)
          in p `T.append` T.justifyRight m '0' (T.pack $ showHex n "")
    bsl = zip (map T.head escapeTo) escapeFrom

escapeFrom, escapeTo :: [Text]
escapeFrom  = ["\\r","\\n","\\t","\\\"","\\\\"]
escapeTo    = [ "\r", "\n", "\t",  "\"",  "\\"]

-- Stack functions --

initStack :: Stack
initStack = []

push' :: Stack -> KValue -> Stack
push' = flip (:)

class Push a where
  push :: Stack -> a -> Stack

instance Push KValue where
  push = push'

instance (Push a, Push b) => Push (a, b) where
  push s (x, y) = s `push` x `push` y

instance (Push a, Push b, Push c) => Push (a, b, c) where
  push s (x, y, z) = s `push` x `push` y `push` z

instance Push Integer where
  push s x = s `push` (KPrim $ KInt x)

instance Push Text where
  push s x = s `push` (KPrim $ KStr x)

instance Push Kwd where
  push s x = s `push` (KPrim $ KKwd x)

-- ... TODO ...

class Pop a where
  pop :: Stack -> PopResult a

instance Pop KValue where
  pop (x:s) = Right (x,s)
  pop _     = Left $ stackFail "value"

-- | NB: returns popped items in "reverse" order
--
-- >>> let s = initStack `push` 1 `push` 2
-- >>> fst $ pop' s :: Integer
-- 2
-- >>> fst $ pop' s :: (Integer, Integer)
-- (1,2)
--
-- stack: ... 1 2 <- top
--
instance (Pop a, Pop b) => Pop (a, b) where
  pop s = do
    (x, s1) <- pop s
    (y, s2) <- pop s1
    return ((y, x), s2)

-- | NB: returns popped items in "reverse" order
--
-- >>> let s = initStack `push` (1, 2, 3)
-- >>> fst $ pop' s :: (Integer, Integer, Integer)
-- (1,2,3)
--
-- stack: ... 1 2 3 <- top
--
instance (Pop a, Pop b, Pop c) => Pop (a, b, c) where
  pop s = do
    (x, s1) <- pop s
    (y, s2) <- pop s1
    (z, s3) <- pop s2
    return ((z, y, x), s3)

instance Pop Integer where
  pop (KPrim (KInt x):s)  = Right (x, s)
  pop _                   = Left $ stackFail "int"

instance Pop Text where
  pop (KPrim (KStr x):s)  = Right (x, s)
  pop _                   = Left $ stackFail "str"

instance Pop Kwd where
  pop (KPrim (KKwd x):s)  = Right (x, s)
  pop _                   = Left $ stackFail "kwd"

-- ... TODO ...

pop' :: Pop a => Stack -> (a, Stack)
pop' s = either error id $ pop s

stackFail :: String -> String
stackFail s = "*** stack match failed: " ++ s ++ " ***"

-- Module/Scope functions --

mainModule :: Identifier
mainModule = "__main__"

initContext :: IO Context
initContext = do
  modules <- HT.new; main <- HT.new
  HT.insert modules mainModule main
  return Context { modules, ctxScope = _newScope mainModule }

-- TODO: error if already exists
forkContext :: Identifier -> Context -> IO Context
forkContext modName c = do
  newMod <- HT.new
  HT.insert (modules c) modName newMod
  return c { ctxScope = _newScope modName }

_newScope :: Identifier -> Scope
_newScope m = Scope { parent = Left m, table = H.empty }

forkScope :: [(Identifier, KValue)] -> Context -> Context
forkScope l c = c { ctxScope = Scope { parent = Right $ ctxScope c,
                                       table  = H.fromList l } }

lookup :: Context -> Identifier -> IO (Maybe KValue)
lookup c = lookup' $ ctxScope c
  where
    lookup' s k = case H.lookup k $ table s of
      Nothing -> up k $ parent s
      v       -> return v
    up k = either (lookupModule c k) (flip lookup' k)

lookupModule :: Context -> Identifier -> Identifier -> IO (Maybe KValue)
lookupModule c k modName = do
    m <- HT.lookup (modules c) modName
    maybe err (flip HT.lookup k) m
  where
    err = error $ "*** module not found: " ++ (T.unpack modName) ++ " ***"

-- "constructors" --

nil, false, true :: KValue
nil   = KPrim KNil
false = bool False
true  = bool True

bool :: Bool -> KValue
bool = KPrim . KBool

int :: Integer -> KValue
int = KPrim . KInt

float :: Double -> KValue
float = KPrim . KFloat

str :: Text -> KValue
str = KPrim . KStr

kwd :: Text -> KValue
kwd = KPrim . KKwd . Kwd

pair :: Kwd -> KValue -> KValue
pair k v = KPair $ Pair k v

list :: [KValue] -> KValue
list = KList . List

block :: [Ident] -> [KValue] -> Maybe Scope -> KValue
block blkArgs blkCode blkScope = KBlock Block{..}

class Val a where
  val :: a -> KValue

instance Val Bool where
  val = bool

instance Val Integer where
  val = int

instance Val Double where
  val = float

instance Val Text where
  val = str

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

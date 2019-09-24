--  --                                                          ; {{{1
--
--  File        : Koneko/Data.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-09-22
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
  Identifier, Module, PopResult, Evaluator, KException(..), Kwd(..),
  Ident, List(..), Block(..), Scope(..), Context(..), Pair(..),
  KPrim(..), KValue(..), KType(..), Stack, unIdent, ident, escapeFrom,
  escapeTo, emptyStack, Push, push', push, Pop, pop, pop', mainModule,
  preludeModule, initContext, forkContext, forkScope, defineIn,
  lookup, typeOf, typeToKwd, isNil, isBool, isInt, isFloat, isStr,
  isKwd, isPair, isList, isIdent, isQuot, isBlock, nil, false, true,
  bool, int, float, str, kwd, pair, list, block, Val, val
) where

import Control.Exception (Exception, throwIO)
import Data.Char (isPrint, ord)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Data.Typeable (Typeable)
import Numeric (showHex)
import Prelude hiding (lookup)

import qualified Data.HashMap.Lazy as H
import qualified Data.HashTable.IO as HT
import qualified Data.Text.Lazy as T
import qualified Prelude as P

import qualified Koneko.Misc as M

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
type PopResult a        = Either KException (a, Stack)

type Evaluator          = Context -> Stack -> IO Stack

data KException
    = ParseError !String      -- ^ parse error
    | EvalUnexpected !String  -- ^ unexpected value during eval
    | EvalScopelessBlock      -- ^ block w/o scope during eval
    | ModuleNotFound !String  -- ^ module not found
    | LookupFailed !String    -- ^ ident lookup failed
    | StackUnderflow          -- ^ stack was empty
    | StackExpected !String   -- ^ stack did not contain expected value
  deriving Typeable

instance Exception KException

newtype Kwd = Kwd { unKwd :: Identifier }
  deriving (Eq, Ord)

newtype Ident = Ident { unIdent :: Identifier }
  deriving (Eq, Ord)

ident :: Text -> Maybe Ident
ident s = if M.isIdent s then Just $ Ident s else Nothing

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

data KType
    = TNil | TBool | TInt | TFloat | TStr | TKwd | TPair | TList
    | TIdent | TQuot | TBlock
  deriving (Eq, Ord)

type Stack = [KValue]

-- instances --

instance Show KException where
  show (ParseError msg)       = "parse error: " ++ msg
  show (EvalUnexpected what)  = "cannot eval " ++ what
  show (EvalScopelessBlock)   = "cannot eval scopeless block"
  show (ModuleNotFound name)  = "no module named " ++ name
  show (LookupFailed name)    = "name " ++ name ++ " is not defined"
  show (StackUnderflow)       = "stack underflow"
  show (StackExpected what)   = "expected " ++ what ++ " on stack"

-- TODO
instance Eq Block where
  Block a c _ == Block a' c' _ = (a,c) == (a',c')

-- TODO
instance Ord Block where
  Block a c _ `compare` Block a' c' _ = (a,c) `compare` (a',c')

instance Show Kwd where
  show (Kwd s) = ":" ++ if M.isIdent s then T.unpack s else show s

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

-- TODO
instance Show KType where
  show TNil   = "<nil>"
  show TBool  = "<bool>"
  show TInt   = "<int>"
  show TFloat = "<float>"
  show TStr   = "<str>"
  show TKwd   = "<kwd>"
  show TPair  = "<pair>"
  show TList  = "<list>"
  show TIdent = "<ident>"
  show TQuot  = "<quot>"
  show TBlock = "<block>"

showStr :: Text -> String
showStr s = T.unpack $ T.concat ["\"", T.concatMap f s, "\""]
  where
    f c = maybe (g c) id $ P.lookup c bsl
    g c = if isPrint c then T.singleton c else h (ord c)
    h n = let (p,m) = if n <= 0xffff then ("\\u",4) else ("\\U",8)
          in p <> T.justifyRight m '0' (T.pack $ showHex n "")
    bsl = zip (map T.head escapeTo) escapeFrom

escapeFrom, escapeTo :: [Text]
escapeFrom  = ["\\r","\\n","\\t","\\\"","\\\\"]
escapeTo    = [ "\r", "\n", "\t",  "\"",  "\\"]

-- Stack functions --

emptyStack :: Stack
emptyStack = []

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

pop :: Pop a => Stack -> PopResult a
pop []  = Left StackUnderflow
pop s   = pop_ s

pop' :: Pop a => Stack -> IO (a, Stack)
pop' s = either throwIO return $ pop s

class Pop a where
  pop_ :: Stack -> PopResult a

instance Pop KValue where
  pop_ (x:s)  = Right (x, s)
  pop_ _      = Left StackUnderflow

-- | NB: returns popped items in "reverse" order
--
-- >>> let s = emptyStack `push` 1 `push` 2
-- >>> fst <$> pop' s :: IO Integer
-- 2
-- >>> fst <$> pop' s :: IO (Integer, Integer)
-- (1,2)
--
-- stack: ... 1 2 <- top
--
instance (Pop a, Pop b) => Pop (a, b) where
  pop_ s = do
    (x, s1) <- pop s
    (y, s2) <- pop s1
    return ((y, x), s2)

-- | NB: returns popped items in "reverse" order
--
-- >>> let s = emptyStack `push` (1, 2, 3)
-- >>> fst <$> pop' s :: IO (Integer, Integer, Integer)
-- (1,2,3)
--
-- stack: ... 1 2 3 <- top
--
instance (Pop a, Pop b, Pop c) => Pop (a, b, c) where
  pop_ s = do
    (x, s1) <- pop s
    (y, s2) <- pop s1
    (z, s3) <- pop s2
    return ((z, y, x), s3)

instance Pop Integer where
  pop_ (KPrim (KInt x):s) = Right (x, s)
  pop_ _                  = Left $ StackExpected "int"

instance Pop Text where
  pop_ (KPrim (KStr x):s) = Right (x, s)
  pop_ _                  = Left $ StackExpected "str"

instance Pop Kwd where
  pop_ (KPrim (KKwd x):s) = Right (x, s)
  pop_ _                  = Left $ StackExpected "kwd"

instance Pop Block where
  pop_ (KBlock x:s)       = Right (x, s)
  pop_ _                  = Left $ StackExpected "block"

-- ... TODO ...

-- Module/Scope functions --

mainModule :: Identifier
mainModule = "__main__"

preludeModule :: Identifier
preludeModule = "__prelude__"

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

forkScope :: [(Identifier, KValue)] -> Context -> Scope -> Context
forkScope l c s = c { ctxScope = Scope { parent = Right s,
                                         table  = H.fromList l } }

-- TODO: error if already exists
defineIn :: Context -> Identifier -> KValue -> IO ()
defineIn c k v = do curMod <- scopeModule c; HT.insert curMod k v

scopeModule :: Context -> IO Module
scopeModule c = let f s = either (getModule c) f $ parent s
                in f $ ctxScope c

lookup :: Context -> Identifier -> IO (Maybe KValue)
lookup c = lookup' $ ctxScope c
  where
    lookup' s k = case H.lookup k $ table s of
      Nothing -> up k $ parent s
      v       -> return v
    up k = either (modOrPrel k) (flip lookup' k)
    modOrPrel k modName = lookupModule c k modName >>= maybe
      (if modName /= preludeModule then lookupModule c k preludeModule
       else return Nothing) (return . Just)

lookupModule :: Context -> Identifier -> Identifier -> IO (Maybe KValue)
lookupModule c k modName = getModule c modName >>= flip HT.lookup k

getModule :: Context -> Identifier -> IO Module
getModule c modName = HT.lookup (modules c) modName >>= maybe err return
  where
    err = throwIO $ ModuleNotFound $ T.unpack modName

-- type predicates --

typeOf :: KValue -> KType
typeOf (KPrim p) = case p of
  KNil            ->  TNil
  KBool _         ->  TBool
  KInt _          ->  TInt
  KFloat _        ->  TFloat
  KStr _          ->  TStr
  KKwd _          ->  TKwd
typeOf (KPair _)  =   TPair
typeOf (KList _)  =   TList
typeOf (KIdent _) =   TIdent
typeOf (KQuot _)  =   TQuot
typeOf (KBlock _) =   TBlock

typeToKwd :: KType -> Kwd
typeToKwd TNil    = Kwd "nil"
typeToKwd TBool   = Kwd "bool"
typeToKwd TInt    = Kwd "int"
typeToKwd TFloat  = Kwd "float"
typeToKwd TStr    = Kwd "str"
typeToKwd TKwd    = Kwd "kwd"
typeToKwd TPair   = Kwd "pair"
typeToKwd TList   = Kwd "list"
typeToKwd TIdent  = Kwd "ident"
typeToKwd TQuot   = Kwd "quot"
typeToKwd TBlock  = Kwd "block"

isNil, isBool, isInt, isFloat, isStr, isKwd, isPair, isList, isIdent,
  isQuot, isBlock :: KValue -> Bool

isNil   = (TNil   ==) . typeOf
isBool  = (TBool  ==) . typeOf
isInt   = (TInt   ==) . typeOf
isFloat = (TFloat ==) . typeOf
isStr   = (TStr   ==) . typeOf
isKwd   = (TKwd   ==) . typeOf
isPair  = (TPair  ==) . typeOf
isList  = (TList  ==) . typeOf
isIdent = (TIdent ==) . typeOf
isQuot  = (TQuot  ==) . typeOf
isBlock = (TBlock ==) . typeOf

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

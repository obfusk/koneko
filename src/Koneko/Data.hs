--  --                                                          ; {{{1
--
--  File        : Koneko/Data.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-06
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

                                                              --  {{{1
-- |
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Maybe
-- >>> id = fromJust . ident; q = KQuot . id
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
-- >>> str "I like 猫s"
-- "I like 猫s"
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
-- ... TODO ...
--

                                                              --  }}}1

module Koneko.Data (
  Identifier, Module, Evaluator, KException(..), Kwd(..), Ident,
  unIdent, ident, List(..), Dict(..), Block(..), Builtin(..),
  Multi(..), RecordT(..), Record, recType, recValues, record, Scope,
  Context, ctxScope, Pair(..), KPrim(..), KValue(..), KType(..),
  Stack, escapeFrom, escapeTo, ToVal, toVal, FromVal, fromVal,
  emptyStack, push', push, rpush, rpush1, pop, pop2, pop3, pop',
  pop2', pop3', pop1push, pop2push, pop1push1, pop2push1, primModule,
  bltnModule, prldModule, mainModule, initMainContext, forkContext,
  forkScope, defineIn, lookup, typeOf, typeToKwd, typeToStr, isNil,
  isBool, isInt, isFloat, isStr, isKwd, isPair, isList, isDict,
  isIdent, isQuot, isBlock, isBuiltin, isMulti, isRecordT, isRecord,
  isCallable, nil, false, true, bool, int, float, str, kwd, pair,
  list, dict, block, mkPrim, mkBltn, defPrim, truthy
) where

import Control.DeepSeq (deepseq, NFData(..))
import Control.Exception (Exception, throw, throwIO)
import Data.Char (isPrint, ord)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text.Lazy (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Numeric (showHex)
import Prelude hiding (lookup)

import qualified Data.HashMap.Lazy as H
import qualified Data.HashTable.IO as HT
import qualified Data.Text.Lazy as T
import qualified Prelude as P

import qualified Koneko.Misc as M

-- TODO:
--  * Dict
--  * Record
--  * RawBlock vs Quoted Block

-- types --

type HashTable k v      = HT.BasicHashTable k v
type ModuleLookupTable  = HashTable Identifier KValue
type ScopeLookupTable   = H.HashMap Identifier KValue
type DictTable          = H.HashMap Identifier KValue
type MultiTable         = HashTable [Identifier] Block

type Identifier         = Text
type Module             = ModuleLookupTable
type Evaluator          = Context -> Stack -> IO Stack

-- TODO
data KException
    = ParseError !String
    | EvalUnexpected !String  -- ^ unexpected value during eval
    | EvalScopelessBlock      -- ^ block w/o scope during eval
    | ModuleNotFound !String
    | LookupFailed !String    -- ^ ident lookup failed
    | StackUnderflow          -- ^ stack was empty
    | StackExpected !String   -- ^ stack did not contain expected value
    | UncomparableType !String
    | UncallableType !String
    | UnknownField !String !String
    | EmptyList !String
    | IndexError !String
  deriving Typeable

instance Exception KException

newtype Kwd = Kwd { unKwd :: Identifier }
  deriving (Eq, Ord, Generic, NFData)

newtype Ident = Ident_ { unIdent :: Identifier }
  deriving (Eq, Ord, Generic, NFData)

ident :: Identifier -> Maybe Ident
ident s = if M.isIdent s then Just $ Ident_ s else Nothing

newtype List = List { unList :: [KValue] }
  deriving (Eq, Ord, Generic, NFData)

-- TODO
newtype Dict = Dict { unDict :: DictTable }
  deriving (Eq, Ord, Generic, NFData)

data Block = Block {
  blkArgs   :: [Ident],
  blkCode   :: [KValue],
  blkScope  :: Maybe Scope
} deriving (Generic, NFData)

data Builtin = Builtin {
  biPrim  :: Bool,
  biName  :: Identifier,
  biRun   :: Evaluator
}

-- TODO
instance NFData Builtin where
  rnf Builtin{..} = (biPrim, biName) `deepseq` ()

data Multi = Multi {
  mltArgs   :: Int,
  mltName   :: Identifier,
  mltTable  :: MultiTable
}

-- TODO
instance NFData Multi where
  rnf Multi{..} = (mltArgs, mltName) `deepseq` ()

data RecordT = RecordT {
  recName   :: Identifier,
  recFields :: [Identifier]
} deriving (Eq, Ord, Generic, NFData)

data Record = Record {
  recType   :: RecordT,
  recValues :: [KValue]
} deriving (Eq, Ord, Generic, NFData)

record :: RecordT -> [KValue] -> Maybe Record
record recType recValues
  | length (recFields recType) == length recValues  = Just Record{..}
  | otherwise                                       = Nothing

data Scope = Scope {
  parent  :: Either Identifier Scope,
  table   :: ScopeLookupTable
}

-- TODO
instance NFData Scope where
  rnf Scope{..} = parent `deepseq` ()

data Context = Context {
  modules   :: HashTable Identifier Module,
  ctxScope  :: Scope
}

data Pair = Pair { key :: Kwd, value :: KValue }
  deriving (Eq, Ord, Generic, NFData)

-- TODO: + Rx
data KPrim
    = KNil | KBool Bool | KInt Integer | KFloat Double
    | KStr Text | KKwd Kwd
  deriving (Eq, Ord, Generic, NFData)

-- TODO
data KValue
    = KPrim KPrim | KPair Pair | KList List | KDict Dict
    | KIdent Ident | KQuot Ident | KBlock Block | KBuiltin Builtin
    | KMulti Multi | KRecordT RecordT | KRecord Record
  deriving (Eq, Ord, Generic, NFData)

data KType
    = TNil | TBool | TInt | TFloat | TStr | TKwd | TPair | TList
    | TDict | TIdent | TQuot | TBlock | TBuiltin | TMulti | TRecordT
    | TRecord
  deriving (Eq, Ord, Generic, NFData)

type Stack = [KValue]

-- instances --

-- TODO: find some way of comparing these?

instance Eq Block where
  _ == _ = throw $ UncomparableType "block"

-- TODO
instance Eq Builtin where
  _ == _ = throw $ UncomparableType "builtin"

instance Eq Multi where
  _ == _ = throw $ UncomparableType "multi"

instance Ord Block where
  compare _ _ = throw $ UncomparableType "block"

-- TODO
instance Ord Builtin where
  compare _ _ = throw $ UncomparableType "builtin"

instance Ord Multi where
  compare _ _ = throw $ UncomparableType "multi"

instance Show KException where
  show (ParseError msg)         = "parse error: " ++ msg
  show (EvalUnexpected what)    = "cannot eval " ++ what
  show (EvalScopelessBlock)     = "cannot eval scopeless block"
  show (ModuleNotFound name)    = "no module named " ++ name
  show (LookupFailed name)      = "name " ++ name ++ " is not defined"
  show (StackUnderflow)         = "stack underflow"
  show (StackExpected what)     = "expected " ++ what ++ " on stack"
  show (UncomparableType what)  = "uncomparable type " ++ what
  show (UncallableType what)    = "uncallable type " ++ what
  show (UnknownField f t)       = "unknown field " ++ f ++ " for " ++ t
  show (EmptyList op)           = op ++ ": empty list"
  show (IndexError op)          = op ++ ": index error"

instance Show Kwd where
  show (Kwd s) = ":" ++ if M.isIdent s then T.unpack s else show s

instance Show Ident where
  show = T.unpack . unIdent

instance Show List where
  show (List [])  = "()"
  show (List l)   = "( " ++ intercalate " " (map show l) ++ " )"

instance Show Dict where
  show (Dict d) = "{ " ++ intercalate " " (map f $ H.toList d) ++ " }"
    where
      f (k, v) = show $ Pair (Kwd k) v

-- TODO
instance Show Block where
  show (Block a c _) = case (a, c) of
      ([], [])  -> "[ ]"
      ([], _ )  -> "[ " ++ f c ++ " ]"
      (_ , [])  -> "[ " ++ f a ++ " . ]"
      (_ , _ )  -> "[ " ++ f a ++ " . " ++ f c ++ " ]"
    where
      f :: Show a => [a] -> String
      f = intercalate " " . map show

instance Show Builtin where
  show Builtin{..} = "#<" ++ t ++ ":" ++ T.unpack biName ++ ">"
    where
      t = if biPrim then "primitive" else "builtin"

instance Show Multi where
  show Multi{..}  = "#<multi:" ++ show mltArgs ++ ":" ++
                    T.unpack mltName ++ ">"

instance Show RecordT where
  show RecordT{..} = "#<record-type:" ++ show recName ++ ">"

instance Show Record where
  show Record{..} = T.unpack (recName recType) ++ "{ " ++ flds ++ " }"
    where
      flds      = intercalate " " $ map f
                $ zip (recFields recType) recValues
      f (k, v)  = show $ Pair (Kwd k) v

instance Show Pair where
  show (Pair k v) = show k ++ " " ++ show v ++ " =>"

-- TODO
instance Show KPrim where
  show KNil       = "nil"
  show (KBool b)  = if b then "#t" else "#f"
  show (KInt i)   = show i
  show (KFloat f) = show f
  show (KStr s)   = showStr s
  show (KKwd k)   = show k

instance Show KValue where
  show (KPrim p)      = show p
  show (KPair p)      = show p
  show (KList l)      = show l
  show (KDict d)      = show d
  show (KIdent i)     = show i
  show (KQuot i)      = "'" ++ show i
  show (KBlock b)     = show b
  show (KBuiltin b)   = show b
  show (KMulti m)     = show m
  show (KRecordT r)   = show r
  show (KRecord r)    = show r

instance Show KType where
  show TNil       = "#<::nil>"
  show TBool      = "#<::bool>"
  show TInt       = "#<::int>"
  show TFloat     = "#<::float>"
  show TStr       = "#<::str>"
  show TKwd       = "#<::kwd>"
  show TPair      = "#<::pair>"
  show TList      = "#<::list>"
  show TDict      = "#<::dict>"
  show TIdent     = "#<::ident>"
  show TQuot      = "#<::quot>"
  show TBlock     = "#<::block>"
  show TBuiltin   = "#<::builtin>"
  show TMulti     = "#<::multi>"
  show TRecordT   = "#<::record-type>"
  show TRecord    = "#<::record>"

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

-- ToVal & FromVal --

class ToVal a where
  toVal :: a -> KValue

instance ToVal () where
  toVal () = KPrim KNil

instance ToVal Bool where
  toVal = KPrim . KBool

instance ToVal Integer where
  toVal = KPrim . KInt

instance ToVal Double where
  toVal = KPrim . KFloat

instance ToVal Text where
  toVal = KPrim . KStr

instance ToVal Kwd where
  toVal = KPrim . KKwd

instance ToVal Pair where
  toVal = KPair

instance ToVal [KValue] where
  toVal = KList . List

instance ToVal [Pair] where
  toVal = KDict . Dict . H.fromList . map f
    where
      f (Pair (Kwd k) v) = (k, v)

instance ToVal Block where
  toVal = KBlock

instance ToVal Builtin where
  toVal = KBuiltin

instance ToVal KValue where
  toVal = id

-- NB: no ToVal for
-- * ident, quot (both Ident)
-- * multi, record(-type) (no point)

class FromVal a where
  fromVal :: KValue -> Either KException a

instance FromVal () where
  fromVal (KPrim KNil)        = Right ()
  fromVal _                   = Left $ StackExpected "nil"

instance FromVal Bool where
  fromVal (KPrim (KBool x))   = Right x
  fromVal _                   = Left $ StackExpected "bool"

instance FromVal Integer where
  fromVal (KPrim (KInt x))    = Right x
  fromVal _                   = Left $ StackExpected "int"

instance FromVal Double where
  fromVal (KPrim (KFloat x))  = Right x
  fromVal _                   = Left $ StackExpected "float"

instance FromVal Text where
  fromVal (KPrim (KStr x))    = Right x
  fromVal _                   = Left $ StackExpected "str"

instance FromVal Kwd where
  fromVal (KPrim (KKwd x))    = Right x
  fromVal _                   = Left $ StackExpected "kwd"

instance FromVal Pair where
  fromVal (KPair x)           = Right x
  fromVal _                   = Left $ StackExpected "pair"

instance FromVal [KValue] where
  fromVal (KList (List x))    = Right x
  fromVal _                   = Left $ StackExpected "list"

-- NB: ToVal is for [Pair]
instance FromVal Dict where
  fromVal (KDict x)           = Right x
  fromVal _                   = Left $ StackExpected "dict"

instance FromVal Block where
  fromVal (KBlock x)          = Right x
  fromVal _                   = Left $ StackExpected "block"

instance FromVal KValue where
  fromVal x                   = Right x

-- NB: no FromVal for
-- * ident, quot (both Ident)
-- * builtin, multi, record(-type) (no need?)

-- Stack functions --

emptyStack :: Stack
emptyStack = []

push' :: Stack -> KValue -> Stack
push' = flip (:)

push :: ToVal a => Stack -> a -> Stack
push s = push' s . toVal

rpush :: ToVal a => Stack -> [a] -> IO Stack
rpush s = return . foldl push s

rpush1 :: ToVal a => Stack -> a -> IO Stack
rpush1 s = return . push s

pop :: FromVal a => Stack -> Either KException (a, Stack)
pop []    = Left StackUnderflow
pop (x:s) = do y <- fromVal x; Right (y, s)

-- | NB: returns popped items in "reverse" order
--
-- >>> s = emptyStack `push` 1 `push` 2
-- >>> fst <$> pop' s :: IO Integer
-- 2
-- >>> fst <$> pop2' s :: IO (Integer, Integer)
-- (1,2)
--
-- stack: ... 1 2 <- top
--
pop2 :: (FromVal a, FromVal b)
     => Stack -> Either KException ((a, b), Stack)
pop2 s0 = do
  (y, s1) <- pop s0
  (x, s2) <- pop s1
  return ((x, y), s2)

-- | NB: returns popped items in "reverse" order
--
-- >>> s = emptyStack `push` 1 `push` 2 `push` 3
-- >>> fst <$> pop3' s :: IO (Integer, Integer, Integer)
-- (1,2,3)
--
-- stack: ... 1 2 3 <- top
--
pop3 :: (FromVal a, FromVal b, FromVal c)
     => Stack -> Either KException ((a, b, c), Stack)
pop3 s0 = do
  (z, s1) <- pop s0
  (y, s2) <- pop s1
  (x, s3) <- pop s2
  return ((x, y, z), s3)

pop' :: FromVal a => Stack -> IO (a, Stack)
pop' = retOrThrow . pop

pop2' :: (FromVal a, FromVal b) => Stack -> IO ((a, b), Stack)
pop2' = retOrThrow . pop2

pop3' :: (FromVal a, FromVal b, FromVal c)
      => Stack -> IO ((a, b, c), Stack)
pop3' = retOrThrow . pop3

retOrThrow :: Either KException a -> IO a
retOrThrow = either throwIO return

pop1push :: (FromVal a, ToVal b) => (a -> [b]) -> Evaluator
pop1push f _ s = do (x, s') <- pop' s; rpush s' $ f x

pop2push :: (FromVal a, FromVal b, ToVal c)
         => (a -> b -> [c]) -> Evaluator
pop2push f _ s = do ((x, y), s') <- pop2' s; rpush s' $ f x y

pop1push1 :: (FromVal a, ToVal b) => (a -> b) -> Evaluator
pop1push1 f = pop1push $ \x -> [f x]

pop2push1 :: (FromVal a, FromVal b, ToVal c)
          => (a -> b -> c) -> Evaluator
pop2push1 f = pop2push $ \x y -> [f x y]

-- Module/Scope functions --

primModule, bltnModule, prldModule, mainModule :: Identifier
primModule = "__prim__"
bltnModule = "__bltn__"
prldModule = "__prld__"
mainModule = "__main__"

initMainContext :: IO Context
initMainContext = do
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

-- TODO: can this create unnecessary duplicates?
forkScope :: [(Identifier, KValue)] -> Context -> Scope -> Context
forkScope [] c s  = c { ctxScope = s }
forkScope l  c s  = c { ctxScope = Scope { parent = Right s,
                                           table  = H.fromList l } }

-- TODO: error if already exists (or prim, etc.)
defineIn :: Context -> Identifier -> KValue -> IO ()
defineIn c k v = do curMod <- scopeModule c; HT.insert curMod k v

scopeModule :: Context -> IO Module
scopeModule c = let f s = either (getModule c) f $ parent s
                in f $ ctxScope c

-- Prim -> Scope* -> Module -> Prel -> Bltn
lookup :: Context -> Identifier -> IO (Maybe KValue)
lookup c k = first [lookupPrim, lookupScope $ ctxScope c,
                    lookupPrel, lookupBltn]
  where
    lookupScope s = maybe (f s) (return . Just) $ H.lookup k $ table s
    f s           = either look lookupScope $ parent s
    lookupPrim    = look primModule
    lookupBltn    = look bltnModule
    lookupPrel    = look prldModule
    look          = lookupModule c k
    first []      = return Nothing
    first (x:xt)  = x >>= maybe (first xt) (return . Just)

lookupModule :: Context -> Identifier -> Identifier -> IO (Maybe KValue)
lookupModule c k modName = getModule c modName >>= flip HT.lookup k

getModule :: Context -> Identifier -> IO Module
getModule c modName = HT.lookup (modules c) modName >>= maybe err return
  where
    err = throwIO $ ModuleNotFound $ T.unpack modName

-- type predicates --

typeOf :: KValue -> KType
typeOf (KPrim p) = case p of
  KNil                ->  TNil
  KBool _             ->  TBool
  KInt _              ->  TInt
  KFloat _            ->  TFloat
  KStr _              ->  TStr
  KKwd _              ->  TKwd
typeOf (KPair _)      =   TPair
typeOf (KList _)      =   TList
typeOf (KDict _)      =   TDict
typeOf (KIdent _)     =   TIdent
typeOf (KQuot _)      =   TQuot
typeOf (KBlock _)     =   TBlock
typeOf (KBuiltin _)   =   TBuiltin
typeOf (KMulti _)     =   TMulti
typeOf (KRecordT _)   =   TRecordT
typeOf (KRecord _)    =   TRecord

typeToKwd :: KType -> Kwd
typeToKwd = Kwd . typeToStr

typeToStr :: IsString a => KType -> a
typeToStr TNil        = "nil"
typeToStr TBool       = "bool"
typeToStr TInt        = "int"
typeToStr TFloat      = "float"
typeToStr TStr        = "str"
typeToStr TKwd        = "kwd"
typeToStr TPair       = "pair"
typeToStr TList       = "list"
typeToStr TDict       = "dict"
typeToStr TIdent      = "ident"
typeToStr TQuot       = "quot"
typeToStr TBlock      = "block"
typeToStr TBuiltin    = "builtin"
typeToStr TMulti      = "multi"
typeToStr TRecordT    = "record-type"
typeToStr TRecord     = "record"

isNil, isBool, isInt, isFloat, isStr, isKwd, isPair, isList, isDict,
  isIdent, isQuot, isBlock, isBuiltin, isMulti, isRecordT, isRecord
    :: KValue -> Bool

isNil       = (TNil       ==) . typeOf
isBool      = (TBool      ==) . typeOf
isInt       = (TInt       ==) . typeOf
isFloat     = (TFloat     ==) . typeOf
isStr       = (TStr       ==) . typeOf
isKwd       = (TKwd       ==) . typeOf
isPair      = (TPair      ==) . typeOf
isList      = (TList      ==) . typeOf
isDict      = (TDict      ==) . typeOf
isIdent     = (TIdent     ==) . typeOf
isQuot      = (TQuot      ==) . typeOf
isBlock     = (TBlock     ==) . typeOf
isBuiltin   = (TBuiltin   ==) . typeOf
isMulti     = (TMulti     ==) . typeOf
isRecordT   = (TRecordT   ==) . typeOf
isRecord    = (TRecord    ==) . typeOf

isCallable :: KValue -> Bool
isCallable = (`elem` callableTypes) . typeOf

callableTypes :: [KType]
callableTypes = [
    TStr, TPair, TList, TDict, TBlock, TBuiltin, TMulti, TRecordT,
    TRecord
  ]

-- "constructors" --

nil, false, true :: KValue
nil   = toVal ()
false = toVal False
true  = toVal True

bool :: Bool -> KValue
bool = toVal

int :: Integer -> KValue
int = toVal

float :: Double -> KValue
float = toVal

str :: Text -> KValue
str = toVal

kwd :: Text -> KValue
kwd = toVal . Kwd

pair :: Kwd -> KValue -> KValue
pair k v = toVal $ Pair k v

list :: [KValue] -> KValue
list = toVal

dict :: [Pair] -> KValue
dict = toVal

-- NB: no ToVal for ident, quot

block :: [Ident] -> [KValue] -> Maybe Scope -> KValue
block blkArgs blkCode blkScope = KBlock Block{..}

-- TODO: multi, record(-type)?

-- utilities --

mkPrim, mkBltn :: Identifier -> Evaluator -> Builtin
mkPrim = Builtin True
mkBltn = Builtin False

defPrim :: Context -> Builtin -> IO ()
defPrim ctx f = defineIn ctx (biName f) $ KBuiltin f

truthy :: KValue -> Bool
truthy (KPrim KNil)           = False
truthy (KPrim (KBool False))  = False
truthy _                      = True

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

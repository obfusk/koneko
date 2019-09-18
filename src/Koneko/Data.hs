--  --                                                          ; {{{1
--
--  File        : Koneko/Data.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-09-17
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

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
-- Pair( :answer 42 )
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
  Kwd(..), Ident, List(..), Block(..), Scope(..), Pair(..), KPrim(..),
  KValue(..), Stack, unIdent, ident, newScope, extendScope, lookup,
  nil, false, true, bool, int, float, str, kwd, pair, list, block
) where

import Data.List (intercalate)
import Data.Text.Lazy (Text)
import Prelude hiding (lookup)

import qualified Data.HashTable.IO as H
import qualified Data.Text.Lazy as T

import Koneko.Misc (isIdent)

-- TODO:
--  * Quoted Ident
--  * Dict
--  * Record
--  * RawBlock vs Quoted Block

type HashTable k v = H.BasicHashTable k v                     --  TODO

newtype Kwd = Kwd { unKwd :: Text }
  deriving (Eq, Ord)

newtype Ident = Ident { unIdent :: Text }
  deriving (Eq, Ord)

newtype List = List { unList :: [KValue] }
  deriving (Eq, Ord)

data Block = Block {
  args  :: [Ident],
  code  :: [KValue],
  scope :: Maybe Scope
}

data Scope = Scope {
  parent  :: Maybe Scope,   -- no parent <=> is namespace
  table   :: HashTable Text KValue
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
  show (Pair k v) = "Pair( " ++ show k ++ " " ++ show v ++ " )"

instance Show KPrim where
  show KNil       = "nil"
  show (KBool b)  = if b then "#t" else "#f"
  show (KInt i)   = show i
  show (KFloat f) = show f
  show (KStr s)   = show s                                    --  TODO
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

newScope :: IO Scope
newScope = H.new >>= return . Scope Nothing

extendScope :: Scope -> IO Scope
extendScope p = do sc <- newScope; return sc { parent = Just p }

lookup :: Scope -> Text -> IO (Maybe KValue)
lookup (Scope p t) k = H.lookup t k >>= maybe up (return . Just)
  where up = maybe (return Nothing) (flip lookup k) p

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
block args code scope = KBlock Block{..}

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

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

module Koneko.Data (
  Kwd(..), Ident, Block(..), Scope(..), Pair(..), KPrim(..),
  KValue(..), Stack(..), ident, unIdent
) where

import Data.List (intercalate)
import Data.Text.Lazy (Text)

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

ident :: Text -> Maybe Ident
ident s = if isIdent s then Just $ Ident s else Nothing

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

-- Todo: Rx
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
  show (KStr s)   = show s
  show (KKwd k)   = show k

-- TODO
instance Show KValue where
  show (KPrim p)  = show p
  show (KPair p)  = show p
  show (KList l)  = show l
  show (KIdent i) = show i
  show (KBlock b) = show b

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

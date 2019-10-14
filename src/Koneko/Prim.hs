--  --                                                          ; {{{1
--
--  File        : Koneko/Prim.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-14
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Koneko.Prim (initCtx, replDef) where

import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.List (isSuffixOf, sort)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.Random (getStdRandom, randomR)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Koneko.Data
import Koneko.Misc (prompt')
import Paths_koneko (getDataFileName)

-- TODO
initCtx :: Context -> Evaluator -> Evaluator -> Evaluator -> IO Context
initCtx ctxMain call apply apply_dict = do
  ctxPrim <- forkContext primModule ctxMain
  traverse_ (defPrim ctxPrim) [
      mkPrim "call" call, mkPrim "apply" apply,
      mkPrim "apply-dict" apply_dict, if_ call,
      def, defmulti, defrecord, mkPair, mkDict, swap,
      show_, say, ask, type_, callable, function,
      moduleGet, moduleDefs, moduleName,
      not_, and_, or_,
      comp "=" (==), comp "/=" (/=), comp "<" (<),
      comp "<=" (<=), comp ">" (>), comp ">=" (>=),
      arithI "__int+__" (+), arithI "__int-__" (-),
      arithI "__int*__" (*),
      arithI "__div__" div, arithI "__mod__" mod,
      arithF "__float+__" (+), arithF "__float-__" (-),
      arithF "__float*__" (*), arithF "__float/__" (/),
      intToFloat, recordToDict, recordType,
      showStack, clearStack, nya
      -- ...
    ]
  return ctxPrim

-- primitives: important --

if_ :: Evaluator -> Builtin
def, defmulti, defrecord, mkPair, mkDict, swap :: Builtin

if_ call = mkPrim "if" $ \c s -> do
  ((cond, t_br, f_br), s') <- pop3' s
  call c $ push' s' $ if truthy cond then t_br else f_br

def = mkPrim "def" $ \c s -> do
  ((Kwd k, v), s') <- pop2' s; s' <$ defineIn c k v

defmulti = mkPrim "defmulti" $ \c s -> do
  ((Kwd k, sig, b), s') <- pop3' s
  sig' <- retOrThrow $ map unKwd <$> fromVals sig
  s' <$ defMulti c k sig' b

defrecord = mkPrim "defrecord" $ \c s -> do
  ((Kwd recName, fs), s') <- pop2' s
  recFields <- retOrThrow $ map unKwd <$> fromVals fs
  s' <$ defineIn c recName (KRecordT RecordT{..})

mkPair = mkPrim "=>" $ pop2push1 Pair

mkDict = mkPrim "dict" $ \_ s -> do
  (l, s') <- pop' s
  rpush1 s' =<< dict <$> (retOrThrow $ fromVals l)

-- needed as primitive by read for .foo
swap = mkPrim "swap" $ pop2push $ \x y -> [y, x] :: [KValue]

-- primitives: miscellaneous --

show_, say, ask, type_, callable, function :: Builtin

show_ = mkPrim "show" $ pop1push1 $ T.pack . (show :: KValue -> String)

-- NB: uses stdio
say = mkPrim "say" $ \_ s -> do (x, s') <- pop' s; s' <$ T.putStrLn x

-- NB: uses stdio
ask = mkPrim "ask" $ \_ s -> do
  (x, s') <- pop' s; maybeToNil <$> prompt' x >>= rpush1 s'

type_     = mkPrim "type"       $ pop1push1 $ typeToKwd . typeOf
callable  = mkPrim "callable?"  $ pop1push1 isCallable
function  = mkPrim "function?"  $ pop1push1 isFunction

-- primitives: modules --

moduleGet, moduleDefs, moduleName :: Builtin

moduleGet = mkPrim "__module-get__" $ \c s -> do
  ((Kwd k, Kwd m), s') <- pop2' s
  lookupModule' c k m >>= rpush1 s'

moduleDefs = mkPrim "__module-defs__" $ \c s -> do
  (Kwd m, s') <- pop' s
  sort . map kwd <$> moduleKeys c m >>= rpush1 s'

moduleName = mkPrim "__name__" $ \c s ->
  rpush1 s $ kwd $ scopeModuleName c

-- primitives: Eq, Ord --

not_, and_, or_ :: Builtin
not_  = mkPrim "not"  $ pop1push1 $ not . truthy
and_  = mkPrim "and"  $ pop2push1 $ \x y -> truthy x && truthy y
or_   = mkPrim "or"   $ pop2push1 $ \x y -> truthy x || truthy y

comp :: Identifier -> (KValue -> KValue -> Bool) -> Builtin
comp name op = mkPrim name $ pop2push1 op

-- primitives: arithmetic, comparison --

arith :: (FromVal a, ToVal a)
      => Identifier -> (a -> a -> a) -> Builtin
arith name op = mkPrim name $ pop2push1 op

arithI :: Identifier -> (Integer -> Integer -> Integer) -> Builtin
arithF :: Identifier -> (Double  -> Double  -> Double ) -> Builtin
arithI = arith
arithF = arith

-- primitives: conversion --

intToFloat, recordToDict, recordType :: Builtin

intToFloat
  = mkPrim "int->float" $ pop1push1 (fromInteger :: Integer -> Double)

recordToDict = mkPrim "record->dict" $ pop1push1 $ \r ->
  dict [ Pair (Kwd k) v | (k, v) <- zip (recFields $ recType r)
                                        (recValues r) ]

recordType = mkPrim "record-type" $ pop1push1 $ KRecordT . recType

-- repl --

replDef :: Context -> IO ()
replDef ctx = do
    alias "show-stack"  showStack
    alias "clear-stack" clearStack
  where
    alias n f = defineIn ctx n $ KBuiltin f

showStack, clearStack :: Builtin

-- TODO
showStack = mkPrim "__show-stack__" $ \_ s ->
  s <$ traverse_ (putStrLn . show) s

clearStack = mkPrim "__clear-stack__" $ \_ _ -> return emptyStack

-- nya --

nya :: Builtin
nya = mkPrim "__nya__" $ \_ s -> s <$ do
  nyaD  <- getDataFileName "nya"
  cats  <- filter (isSuffixOf ".cat") <$> listDirectory nyaD
  unless (null cats) $ do
    i   <- getStdRandom $ randomR (0, length cats -1)
    (T.readFile $ nyaD </> (cats !! i)) >>= T.putStr

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

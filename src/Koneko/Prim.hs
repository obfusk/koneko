--  --                                                          ; {{{1
--
--  File        : Koneko/Prim.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2020-01-10
--
--  Copyright   : Copyright (C) 2020  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Koneko.Prim (initCtx, replDef) where

import Control.DeepSeq (force, NFData)
import Control.Exception (catch, evaluate, throwIO)
import Control.Monad (unless)
import Data.Bits ((.|.))
import Data.Char (chr)
import Data.Foldable (traverse_)
import Data.List (isSuffixOf, sort)
import Data.Monoid ((<>))
import Prelude hiding (lookup)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.Random (getStdRandom, randomR)

import qualified Control.Exception as E
import qualified Data.Array as A
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Text.Lazy.IO as T
import qualified Text.Regex.PCRE as RE

import Koneko.Data
import Koneko.Misc (prompt')
import Paths_koneko (getDataFileName)

-- TODO
initCtx :: Context -> Evaluator -> Evaluator -> Evaluator ->
           (Block -> Evaluator) -> IO ()
initCtx ctxMain call apply apply_dict callBlock = do
  ctx <- forkContext primModule ctxMain
  traverse_ (defPrim ctx) [
      mkPrim "call" call, mkPrim "apply" apply,
      mkPrim "apply-dict" apply_dict, if_ call,
      def, defmulti, defrecord call, mkPair, mkDict, swap,
      show_, say, ask, type_, callable, function,
      defmodule call, modules, moduleGet, moduleDefs, moduleName,
      import_, importFrom,
      comp "=" (==), comp "not=" (/=), comp "<" (<),
      comp "<=" (<=), comp ">" (>), comp ">=" (>=),
      spaceship,
      arithI "int+" (+), arithI "int-" (-), arithI "int*" (*),
      arithI "div" div, arithI "mod" mod,
      arithF "float+" (+), arithF "float-" (-),
      arithF "float*" (*), arithF "float/" (/),
      chr_, intToFloat, recordToDict,
      recordType, recordVals,
      recordTypeName, recordTypeFields,
      mkThunk callBlock, fail_,
      mkIdent, mkQuot, mkBlock, blockParams, blockCode,
      rxMatch,
      showStack, clearStack, nya
    ]

-- primitives: important --

if_, defrecord :: Evaluator -> Builtin
def, defmulti, mkPair, mkDict, swap :: Builtin

if_ call = mkPrim "if" $ \c s -> do
  ((cond, t_br, f_br), s') <- pop3' s
  call c $ push' s' $ if truthy cond then t_br else f_br

def = mkPrim "def" $ \c s -> do
  ((Kwd k, v), s') <- pop2' s; s' <$ defineIn c k v

defmulti = mkPrim "defmulti" $ \c s -> do
    ((Kwd k, sig, b), s') <- pop3' s
    sig' <- traverse (f c) =<< unKwds sig
    s' <$ defMulti c k sig' b
  where
    f c k | k == "_" || k `elem` typeNames = return k
          | otherwise = lookup c k >>= \case
              Just (KRecordT t) -> return $ recordTypeSig t
              Just _  -> throwIO $ expected $ T.unpack k ++ " to be a record-type"
              _       -> throwIO $ LookupFailed $ T.unpack k

-- TODO
defrecord call = mkPrim "defrecord" $ \c s -> do
    ((Kwd recName, fs), s') <- pop2' s; recFields <- unKwds fs
    let t = RecordT{..}; e = err $ T.unpack recName
    defineIn c recName $ KRecordT t
    defX c (recName <> "?") $ pop1push1 $ m t (const True) False
    defX c ("^" <> recName) $ \c1 s1 -> do
      ((x, f), s2) <- pop2' s1
      let go r = rpush s2 $ recValues r ++ [f]
      call c1 =<< m t go e x
    defX c ("~" <> recName) $ \c1 s1 -> do
      ((x, f, g), s2) <- pop3' s1
      let go r = recValues r ++ [f]
      call c1 =<< rpush s2 (m t go [g] x)
    return s'
  where
    m t f d   = \case KRecord r | recType r == t -> f r; _ -> d
    defX c k  = defineIn c k . KBuiltin .
                mkBltn (modName (ctxScope c) <> ":" <> k)
    err t     = throwIO $ stackExpected $ "record of type " <> t

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
say = mkPrim "say!" $ \_ s -> do (x, s') <- pop' s; s' <$ T.putStrLn x

-- NB: uses stdio
ask = mkPrim "ask!" $ \_ s -> do
  (x, s') <- pop' s; prompt' x >>= rpush1 s'

type_     = mkPrim "type"       $ pop1push1 $ typeToKwd . typeOf
callable  = mkPrim "callable?"  $ pop1push1 isCallable
function  = mkPrim "function?"  $ pop1push1 isFunction

-- primitives: modules --

defmodule :: Evaluator -> Builtin
modules, moduleGet, moduleDefs, moduleName, import_, importFrom :: Builtin

defmodule call = mkPrim "defmodule" $ \c s -> do
  ((Kwd m, b), s') <- pop2' s; c' <- forkContext m c
  call c $ push s' $ KBlock b { blkScope = Just $ ctxScope c' }

modules = mkPrim "modules" $ \c s -> do
  sort . map kwd <$> moduleNames c >>= rpush1 s

moduleGet = mkPrim "module-get" $ \c s -> do
  ((Kwd k, Kwd m), s') <- pop2' s
  lookupModule' c k m >>= rpush1 s'

moduleDefs = mkPrim "module-defs" $ \c s -> do
  (Kwd m, s') <- pop' s
  sort . map kwd <$> moduleKeys c m >>= rpush1 s'

moduleName = mkPrim "name" $ \c s ->
  rpush1 s $ kwd $ modName $ ctxScope c

import_ = mkPrim "import" $ \c s -> do
  (Kwd m, s') <- pop' s; s' <$ importIn c m

importFrom = mkPrim "import-from" $ \c s -> do
  ((ks, Kwd m), s') <- pop2' s; s' <$ (importFromIn c m =<< unKwds ks)

-- primitives: Eq, Ord --

comp :: Identifier -> (KValue -> KValue -> Bool) -> Builtin
comp name op = mkPrim name $ pop2push1 op

spaceship :: Builtin
spaceship = mkPrim "<=>" $ pop2push1 f
  where
    f :: KValue -> KValue -> Integer
    f x y = case compare x y of LT -> -1; EQ -> 0; GT -> 1

-- primitives: arithmetic --

arith :: (FromVal a, ToVal a, NFData a)
      => Identifier -> (a -> a -> a) -> Builtin
arith name op = mkPrim name $ \_ s -> do
    ((x, y), s') <- pop2' s
    rpush1 s' =<< (f $ evaluate $ force $ op x y)
  where
    f = flip catch $ \case  E.DivideByZero  -> throwIO DivideByZero
                            e               -> throwIO e

arithI :: Identifier -> (Integer -> Integer -> Integer) -> Builtin
arithF :: Identifier -> (Double  -> Double  -> Double ) -> Builtin
arithI = arith
arithF = arith

-- primitives: conversion --

chr_, intToFloat, recordToDict :: Builtin

chr_ = mkPrim "chr" $ \_ s -> do
  (i, s') <- pop' s
  unless (0 <= i && i < 0x110000) $ throwIO $
    stackExpected "int in range [0,0x110000)"
  rpush1 s' $ T.singleton $ chr $ fromInteger i

intToFloat
  = mkPrim "int->float" $ pop1push1 (fromInteger :: Integer -> Double)

recordToDict = mkPrim "record->dict" $ pop1push1 $ \r ->
  dict [ Pair (Kwd k) v | (k, v) <- zip (recFields $ recType r)
                                        (recValues r) ]

-- primitives: record --

recordType, recordVals :: Builtin
recordType = mkPrim "record-type"   $ pop1push1 $ KRecordT . recType
recordVals = mkPrim "record-values" $ pop1push1 $ recValues

-- primitives: record-type info --

recordTypeName, recordTypeFields :: Builtin
recordTypeName    = mkPrim "record-type-name" $ pop1push1 $ Kwd . recName
recordTypeFields  = mkPrim "record-type-fields" $ pop1push1
                  $ map kwd . recFields

-- primitives: thunk --

mkThunk :: (Block -> Evaluator) -> Builtin
mkThunk callBlock = mkPrim "thunk" $ \c s -> do
  (b, s') <- pop' s
  t <- thunk $ do
    l <- callBlock b c emptyStack
    unless (length l == 1) $ throwIO $
      expected "thunk to produce exactly 1 value"
    return $ head l   -- safe!
  rpush1 s' $ KThunk t

fail_ :: Builtin
fail_ = mkPrim "fail" $ \_ s -> do
  (msg, _) <- pop' s; throwIO $ Fail $ T.unpack msg

-- primitives: homoiconicity --

mkIdent, mkQuot, mkBlock, blockParams, blockCode :: Builtin

mkIdent = _mkIQ "ident" KIdent
mkQuot  = _mkIQ "quot"  KQuot

-- NB: must use existing block to provide Scope
mkBlock = mkPrim "block" $ \_ s -> do
  ((ps, code, b), s') <- pop3' s
  ps' <- traverse _mkId =<< unKwds ps
  rpush1 s' $ block ps' code $ blkScope b

blockParams = mkPrim "block-params" $ pop1push1 $
  list . map (kwd . unIdent) . blkParams

blockCode = mkPrim "block-code" $ pop1push1 blkCode

_mkIQ :: Identifier -> (Ident -> KValue) -> Builtin
_mkIQ n f = mkPrim n $ \_ s -> do
  (Kwd k, s') <- pop' s; rpush1 s' =<< f <$> _mkId k

_mkId :: Identifier -> IO Ident
_mkId k = maybe err return $ ident k
  where
    err = throwIO $ expected $ T.unpack k ++ " to be a valid ident"

-- primitives: regex --

-- TODO: handle errors, ...
rxMatch :: Builtin
rxMatch = mkPrim "rx-match" $ pop2push1 f
  where
    f s r = g $ RE.matchOnceText (rx $ e r) (e s)
    g     = fmap $ \(_, m, _) -> list $ map (d . fst) $ A.elems m
    rx    = RE.makeRegexOpts (RE.defaultCompOpt .|. RE.compUTF8)
            RE.defaultExecOpt
    e     = LE.encodeUtf8; d = LE.decodeUtf8

-- repl --

replDef :: Context -> IO ()
replDef ctx = do
    alias "show-stack"  showStack
    alias "clear-stack" clearStack
  where
    alias n f = defineIn ctx n $ KBuiltin f

showStack, clearStack :: Builtin

-- TODO
showStack = mkPrim "show-stack" $ \_ s ->
  s <$ traverse_ (putStrLn . show) s

clearStack = mkPrim "clear-stack" $ \_ _ -> return emptyStack

-- nya --

nya :: Builtin
nya = mkPrim "nya!" $ \_ s -> s <$ do
  nyaD  <- getDataFileName "nya"
  cats  <- filter (isSuffixOf ".cat") <$> listDirectory nyaD
  unless (null cats) $ do
    i   <- getStdRandom $ randomR (0, length cats -1)
    (T.readFile $ nyaD </> (cats !! i)) >>= T.putStr

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

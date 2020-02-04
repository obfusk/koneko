--  --                                                          ; {{{1
--
--  File        : Koneko/Prim.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2020-02-04
--
--  Copyright   : Copyright (C) 2020  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Koneko.Prim (initCtx, replDef, swap) where

import Control.Arrow ((***))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.DeepSeq (($!!), force, NFData)
import Control.Exception (catch, evaluate, throwIO, try)
import Control.Monad (unless, when)
import Data.Bits ((.|.))
import Data.Char (chr, isDigit)
import Data.Data (toConstr)
import Data.Foldable (traverse_)
import Data.List (isSuffixOf, sort)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Data.Version (showVersion, versionBranch)
import Prelude hiding (lookup)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.Random (getStdRandom, randomR)

import qualified Control.Exception as E
import qualified Data.Array as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Text.Lazy.IO as T
import qualified System.Info as I
import qualified Text.Regex.PCRE as RE
import qualified Text.Regex.PCRE.ByteString.Lazy as RE

import Koneko.Data
import Koneko.Misc (prompt)

import qualified Paths_koneko as P

-- TODO
initCtx :: Context -> (Identifier -> IO ()) -> Evaluator -> Evaluator
        -> Evaluator -> (Block -> Evaluator) -> IO ()
initCtx ctxMain load call apply apply_dict callBlock = do
  ctx <- forkContext primModule ctxMain
  traverse_ (defPrim ctx) [
      mkPrim "call" call, mkPrim "apply" apply,
      mkPrim "apply-dict" apply_dict, if' call,
      def, defmulti, defrecord call, mkPair, mkDict, swap,
      show', say, ask, types, type', callable, function,
      defmodule call, modules, moduleGet, moduleDefs, moduleName,
      import', importFrom, loadModule load,
      comp "=" (==), comp "not=" (/=),
      comp "<" (<) , comp "<="   (<=),
      comp ">" (>) , comp ">="   (>=),
      spaceship,
      comp' "eq" (== EQ), comp' "neq" (/= EQ),
      comp' "lt" (== LT), comp' "lte" (/= GT),
      comp' "gt" (== GT), comp' "gte" (/= LT),
      cmp',
      arithI "int+" (+), arithI "int-" (-), arithI "int*" (*),
      arithI "div" div, arithI "mod" mod,
      arithF "float+" (+), arithF "float-" (-),
      arithF "float*" (*), arithF "float/" (/),
      abs', neg,
      floatToInt "trunc" (truncate), floatToInt "round" (round),
      floatToInt "ceil"  (ceiling) , floatToInt "floor" (floor),
      chr', intToFloat, recordToDict,
      recordType, recordVals,
      recordTypeName, recordTypeFields,
      mkThunk callBlock,
      fail', try' callBlock,
      mkIdent, mkQuot, mkBlock, blockParams, blockCode,
      rxMatch, rxSub callBlock,
      par callBlock, sleep,
      version,
      showStack call, clearStack, nya
    ]

-- primitives: important --

if', defrecord :: Evaluator -> Builtin
def, defmulti, mkPair, mkDict, swap :: Builtin

if' call = mkPrim "if" $ \c s -> do
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
              _       -> throwIO $ NameError $ T.unpack k

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

-- primitives: show, I/O & types --

show', say, ask, types, type', callable, function :: Builtin

show' = mkPrim "show" $ pop1push1 $ T.pack . (show :: KValue -> String)

-- NB: uses stdio
say = mkPrim "say!" $ \_ s -> do (x, s') <- pop' s; s' <$ T.putStrLn x

-- NB: uses stdio
ask = mkPrim "ask!" $ \_ s -> do
  (x, s') <- pop' s; prompt x >>= rpush1 s'

types     = mkPrim "types" $ const $ flip rpush1 $ map kwd typeNames
type'     = mkPrim "type"       $ pop1push1 $ typeToKwd . typeOf
callable  = mkPrim "callable?"  $ pop1push1 isCallable
function  = mkPrim "function?"  $ pop1push1 isFunction

-- primitives: modules --

defmodule :: Evaluator -> Builtin
modules, moduleGet, moduleDefs, moduleName, import', importFrom :: Builtin

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

import' = mkPrim "import" $ \c s -> do
  (Kwd m, s') <- pop' s; s' <$ importIn c m

importFrom = mkPrim "import-from" $ \c s -> do
  ((ks, Kwd m), s') <- pop2' s; s' <$ (importFromIn c m =<< unKwds ks)

loadModule :: (Identifier -> IO ()) -> Builtin
loadModule load = mkPrim "load-module" $ \_ s -> do
  (Kwd m, s') <- pop' s; s' <$ load m

-- primitives: Eq, Ord --

comp :: Identifier -> (KValue -> KValue -> Bool) -> Builtin
comp name = mkPrim name . pop2push1

comp' :: Identifier -> (Ordering -> Bool) -> Builtin
comp' name f = mkPrim name $ pop2push1 $ \x y -> f $ _cmp' x y

spaceship, cmp' :: Builtin
spaceship = mkPrim "<=>" $ pop2push1 $ \x y -> _ordToInt $ _cmp  x y
cmp'      = mkPrim "cmp" $ pop2push1 $ \x y -> _ordToInt $ _cmp' x y

_cmp, _cmp' :: KValue -> KValue -> Ordering
_cmp  = compare
_cmp' = cmp

_ordToInt :: Ordering -> Integer
_ordToInt LT = -1
_ordToInt EQ =  0
_ordToInt GT =  1

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

abs', neg :: Builtin

abs' = mkPrim "abs" $ pop1push1 $ either
  (toVal . (abs :: Integer -> Integer))
  (toVal . (abs :: Double  -> Double ))

neg = mkPrim "neg" $ pop1push1 $ either
  (toVal . (negate :: Integer -> Integer))
  (toVal . (negate :: Double  -> Double ))

-- primitives: conversion --

floatToInt :: Identifier -> (Double -> Integer) -> Builtin
floatToInt name f = mkPrim name $ pop1push1 g
  where
    g n = if isNaN n || isInfinite n then nil else int $ f n

chr', intToFloat, recordToDict :: Builtin

chr' = mkPrim "chr" $ \_ s -> do
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

-- primitives: exceptions --

fail' :: Builtin
fail' = mkPrim "fail" $ \_ s -> do
  (msg, _) <- pop' s; throwIO $ Fail $ T.unpack msg

try' :: (Block -> Evaluator) -> Builtin
try' callBlock = mkPrim "try" $ \c s0 -> do
    ((f, g, h), s1) <- pop3' s0
    r <- try $ (return $!!) =<< callBlock f c emptyStack
    s3 <- either (cat (flip callBlock c) g) (return . id) r
    s4 <- (++ s3 ++ s1) <$> callBlock h c emptyStack
    when (isNothing g) $ either throwIO (const $ return ()) r
    return s4
  where
    cat cb g e = maybe (return []) (\b -> cb b $ _errInfo e) g

_errInfo :: KException -> [KValue]                -- NB: reverse order
_errInfo e = [list $ map T.pack $ exceptionInfo e,
              str $ T.pack $ show e, kwd $ T.pack $ show $ toConstr e]

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

-- TODO
rxMatch :: Builtin
rxMatch = mkPrim "rx-match" $ \_ s -> do
    ((x, r), s') <- pop2' s
    rpush1 s' =<< (f . _rxGetMatches . _rxMatch x) <$> _rxCompile r
  where
    f = fmap $ \(_, m, _) -> list m

-- TODO
rxSub :: (Block -> Evaluator) -> Builtin
rxSub callBlock = mkPrim "rx-sub" $ \c s -> do
  ((x, s_b, r, glob), s') <- pop4' s; rx <-_rxCompile r
  let strsub t m = return $ _dollar t m
      blksub b m = do
        l <- callBlock b c $ reverse $ map str m
        unless (length l == 1) $ throwIO $
          expected "rx-sub block to produce exactly 1 value"
        fst <$> pop' l
      sub = either strsub blksub s_b
      sub1 (bf,m,af) = do t <- sub m; return $ bf <> t <> af
  rpush1 s' =<< if glob
    then maybe x id <$> _rxReplaceAll (LE.encodeUtf8 x) (_rxMatchAll x rx) sub
    else maybe (return x) sub1 (_rxGetMatches $ _rxMatch x rx)

-- TODO
_dollar :: Text -> [Text] -> Text
_dollar _ []  = error "WTF"
_dollar t m   = T.pack $ f $ T.unpack t
  where
    -- NB: read & !! are safe!
    f ('$':'$':t_)                              = '$'   : f t_
    f ('$':'&':t_)                              = g 0  ++ f t_
    f ('$':i:j:t_) | otn i && isDigit j && ok n = g n  ++ f t_
      where n = read [i,j]
    f ('$':i:  t_) | otn i &&              ok n = g n  ++ f t_
      where n = read [i]
    f (c:t_)                                    = c     : f t_
    f []                                        =         []
    otn i = isDigit i && i /= '0'
    ok n  = 0 < n && n < l
    g n   = T.unpack $ m !! n
    l     = length m

_rxCompile :: Text -> IO RE.Regex
_rxCompile x = f x >>= either (throwIO . InvalidRx . show) return
  where
    f = RE.compile c RE.execBlank . LE.encodeUtf8
    c = RE.compBlank .|. RE.compUTF8 .|. RE.compDollarEndOnly

_rxMatch
  :: Text -> RE.Regex
  -> Maybe (BL.ByteString, RE.MatchText BL.ByteString, BL.ByteString)
_rxMatch s r = RE.matchOnceText r $ LE.encodeUtf8 s

_rxMatchAll :: Text -> RE.Regex -> [RE.MatchText BL.ByteString]
_rxMatchAll s r = RE.matchAllText r $ LE.encodeUtf8 s

_rxGetMatches
  :: Maybe (BL.ByteString, RE.MatchText BL.ByteString, BL.ByteString)
  -> Maybe (Text, [Text], Text)
_rxGetMatches
  = fmap $ \(b,m,a) -> (LE.decodeUtf8 b, _rxMatches m, LE.decodeUtf8 a)

_rxReplaceAll
  :: BL.ByteString -> [RE.MatchText BL.ByteString]
  -> ([Text] -> IO Text) -> IO (Maybe Text)
_rxReplaceAll _   [] _    = return Nothing
_rxReplaceAll src ms sub  = Just . T.concat . concat <$> f 0 src ms
  where
    f _ s    []   = return [[LE.decodeUtf8 s]]
    f i s (m:mt)  = do
        t <- sub $ _rxMatches m
        ([LE.decodeUtf8 s1, t]:) <$> f (off + len) (BL.drop len s2) mt
      where
        (s1, s2)    = BL.splitAt (off - i) s
        (off, len)  = (toEnum *** toEnum) $ snd (m A.! 0)     -- safe!

_rxMatches :: RE.MatchText BL.ByteString -> [Text]
_rxMatches = map (LE.decodeUtf8 . fst) . A.elems

-- primitives: concurrency --

-- TODO
par :: (Block -> Evaluator) -> Builtin
par callBlock = mkPrim "par" $ \c s -> do
  ((f, g), s') <- pop2' s
  (l1, l2) <- concurrently  (callBlock f c emptyStack)
                            (callBlock g c emptyStack)
  return (l2 ++ l1 ++ s')

sleep :: Builtin
sleep = mkPrim "sleep" $ \_ s -> do
  (n, s') <- pop' s
  let ms = either (1000 *) (round . ((1000 :: Double) *)) n
  s' <$ threadDelay (fromInteger $ 1000 * ms)

-- primitives: miscellaneous --

version :: Builtin
version = mkPrim "version" $ const $ flip rpush1
        $ [list $ map int _koneko_version, kwd("hs"),
           list $ map str _platform]

_koneko_version :: [Integer]
_koneko_version = map toInteger $ versionBranch P.version

_platform :: [Text]
_platform = map T.pack [I.os ++ " " ++ I.arch, c ++ " " ++ v]
  where
    c = I.compilerName; v = showVersion I.compilerVersion

-- repl --

replDef :: Context -> IO ()
replDef ctx = do
    alias ["show-stack!" , "s!"] Nothing primModule
    alias ["clear-stack!", "c!"] Nothing primModule
    alias ["d!"] (Just     "display!")   prldModule
    alias ["D!"] (Just "dup&display!")   prldModule
    defineIn ctx "__repl__" true
  where
    alias new old m = do                            -- safe!
      x <- lookupModule' ctx (maybe (underscored $ head new) id old) m
      traverse_ (flip (defineIn ctx) x) new

-- TODO
showStack :: Evaluator -> Builtin
showStack call = mkPrim "show-stack!" $ \c s -> s <$ do
  p <- lookupModule' c "show" prldModule
  let f x = fst <$> (pop' =<< call c [p, x]) >>= T.putStrLn
  putStrLn "--- STACK ---"
  traverse_ f s
  putStrLn "---  END  ---"

clearStack :: Builtin
clearStack = mkPrim "clear-stack!" $ \_ _ ->
  emptyStack <$ T.putStrLn "*** STACK CLEARED ***"

-- nya --

nya :: Builtin
nya = mkPrim "nya!" $ \_ s -> s <$ do
  nyaD  <- P.getDataFileName "nya"
  cats  <- filter (isSuffixOf ".cat") <$> listDirectory nyaD
  unless (null cats) $ do
    i   <- getStdRandom $ randomR (0, length cats -1)
    (T.readFile $ nyaD </> (cats !! i)) >>= T.putStr

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

--  --                                                          ; {{{1
--
--  File        : Koneko/Eval.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-05
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

                                                              --  {{{1
-- |
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Maybe
-- >>> id = fromJust . ident
-- >>> ctx <- initContext
-- >>> ev x = eval x ctx []
--
-- >>> ev [str "Hello, World!", KIdent $ id "say"]
-- Hello, World!
-- []
-- >>> ev [val 1, val 2, KIdent $ id "-"]
-- [-1]
--
-- >>> ev x = evalText "" x ctx []
--
-- >>> ev "\"Hello, World!\" say"
-- Hello, World!
-- []
-- >>> ev "1 2 +"
-- [3]
--
-- ... TODO ...
--

                                                              --  }}}1

module Koneko.Eval (
  tryK, eval, evalText, evalStdin, evalFile, initContext
) where

import Control.Exception (throwIO, try)
import Control.Monad (when)
import Data.List (genericLength, intercalate)
import Data.Text.Lazy (Text)
import Prelude hiding (lookup)
import Safe (atMay)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Koneko.Data

import qualified Koneko.Read as R

import qualified Koneko.Bltn as Bltn
import qualified Koneko.Prim as Prim
import qualified Koneko.Prld as Prld

tryK :: IO a -> IO (Either KException a)
tryK = try

eval :: [KValue] -> Evaluator
eval []     _ s = return s
eval (x:xt) c s = do
  (s1, deferredCall) <- eval1 x c s
  s2 <- if deferredCall then call c s1 else return s1
  eval xt c s2

evalText :: FilePath -> Text -> Evaluator
evalText name code c s = eval (R.read' name code) c s

evalStdin :: Context -> Stack -> IO ()
evalStdin c s = () <$ do
  code <- T.getContents; evalText "(stdin)" code c s

evalFile :: FilePath -> Evaluator
evalFile f c s = do code <- T.readFile f; evalText f code c s

eval1, eval1_ :: KValue -> Context -> Stack -> IO (Stack, Bool)

eval1 x c s = do
  debug <- getDebug c
  when debug $ do
    putStrLn $ "==> eval " ++ show x
    putStrLn $ "--> " ++ intercalate " " (map show $ reverse s)
  r@(s', _) <- eval1_ x c s
  when debug $
    putStrLn $ "<-- " ++ intercalate " " (map show $ reverse s')
  return r

eval1_ x c s = case x of
  KPrim _         -> (, False) <$> rpush s [x]
  KList (List l)  -> (, False) <$> evalList l c s
  KIdent i        -> (, True ) <$> pushIdent (unIdent i) c s
  KQuot i         -> (, False) <$> pushIdent (unIdent i) c s
  KBlock b        -> (, False) <$> evalBlock b c s
  _               -> throwIO $ EvalUnexpected $ typeToStr $ typeOf x

-- TODO
evalList :: [KValue] -> Evaluator
evalList xs c s = do
  ys <- eval xs c emptyStack
  rpush s [List $ reverse ys]

-- TODO
pushIdent :: Text -> Evaluator
pushIdent i c s = lookup c i >>= maybe err (return . push s)
  where
    err = throwIO $ LookupFailed $ T.unpack i

evalBlock :: Block -> Evaluator
evalBlock b c s = rpush s [b { blkScope = Just $ ctxScope c }]

-- TODO
call :: Evaluator
call c s = do
  debug <- getDebug c
  when debug $ putStrLn "*** call ***"
  (x, s') <- pop' s
  case x of
    KPrim (KStr _)  -> error "TODO"
    KPair p         -> callPair p c s'
    KList l         -> callList l c s'
    KDict _         -> error "TODO"
    KBlock b        -> callBlock b c s'
    KBuiltin b      -> biRun b c s'
    KMulti _        -> error "TODO"
    KRecordT _      -> error "TODO"
    KRecord _       -> error "TODO"
    _               -> throwIO $ UncallableType $ typeToStr $ typeOf x

callPair :: Pair -> Evaluator
callPair Pair{..} _ s = do
  (Kwd k, s') <- pop' s
  case k of
    "key"   ->  rpush s' [key]
    "value" ->  rpush s' [value]
    _       ->  throwIO $ UnknownField (T.unpack k) "pair"

callList :: List -> Evaluator
callList (List l) _ s = do
  (Kwd k, s') <- pop' s
  let o = "list." <> k
      g = when (null l) $ throwIO $ EmptyList $ T.unpack o
  case k of
    "empty?"  ->  rpush s' [null l]
    "head"    ->  g >> rpush s' [head l]                        --safe!
    "tail"    ->  g >> rpush s' [List $ tail l]                 --safe!
    "uncons"  ->  g >> rpush s' [head l, KList $ List $ tail l] --safe!
    "cons"    ->  rpush s' $ (:[]) $ mkPrim o $ \_ s1 -> do
                    (x, s2) <- pop' s1; rpush s2 [List (x:l)]
    "len"     ->  rpush s' [genericLength l :: Integer]
    "get"     ->  rpush s' $ (:[]) $ mkPrim o $ \_ s1 -> do
                    (n, s2) <- pop' s1
                    case atMay l $ fromInteger n of
                      Nothing -> throwIO $ IndexError $ T.unpack o
                      Just x  -> rpush s2 [x]
    _         ->  throwIO $ UnknownField (T.unpack k) "list"

-- TODO
callBlock :: Block -> Evaluator
callBlock Block{..} c s0 = do
    scope       <- maybe err return blkScope
    (s1, args)  <- popArgs [] s0 $ reverse blkArgs
    eval blkCode (forkScope args c scope) s1
  where
    popArgs r s []      = return (s, r)
    popArgs r s (k:kt)  = do
      (v, s') <- pop' s; popArgs ((unIdent k, v):r) s' kt
    err                 = throwIO EvalScopelessBlock

-- initial context --

initContext :: IO Context
initContext = do
  ctx     <- initMainContext
  ctxPrim <- Prim.initCtx ctx call
  ctxBltn <- Bltn.initCtx ctxPrim
  ctxPrld <- Prld.initCtx ctxBltn
  pre     <- Prld.modFile
  ctx <$ evalFile pre ctxPrld emptyStack

-- utilities --

getDebug :: Context -> IO Bool
getDebug c = maybe False (== true) <$> lookup c "__debug__"

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

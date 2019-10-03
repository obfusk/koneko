--  --                                                          ; {{{1
--
--  File        : Koneko/Eval.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-03
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
-- >>> let id = fromJust . ident
-- >>> let ev x = evalList x undefined []
--
-- >>> ev [str "Hello, World!", KIdent $ id "__say__"]
-- Hello, World!
-- []
-- >>> ev [val 1, val 2, KIdent $ id "__-__"]
-- [-1]
--
-- >>> let ev x = evalText "" x undefined []
--
-- >>> ev "\"Hello, World!\" __say__"
-- Hello, World!
-- []
-- >>> ev "1 2 __+__"
-- [3]
--
-- ... TODO ...
--

                                                              --  }}}1

module Koneko.Eval (
  tryK, eval, evalList, evalText, evalStdin, evalFile, replPrims,
  initContextWithPrelude
) where

import Control.Exception (throwIO, try)
import Data.Foldable (traverse_)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Koneko.Data hiding (lookup)
import Paths_koneko (getDataFileName)

import qualified Koneko.Read as R
import qualified Koneko.Data as D
import qualified Koneko.Prim as Prim

tryK :: IO a -> IO (Either KException a)
tryK = try

-- TODO: TCO
eval :: KValue -> Evaluator
eval x c s = case x of
  KPrim _         -> return $ s `push` x
  KPair _         -> throwIO $ EvalUnexpected "pair"
  KList (List l)  -> _evalList l c s
  KDict _         -> throwIO $ EvalUnexpected "dict"
  KIdent i        -> _evalIdent (unIdent i) c s
  KQuot i         -> _pushIdent (unIdent i) c s
  KBlock b        -> _evalBlock b c s
  KCallable _     -> error "TODO"

-- TODO
_evalList :: [KValue] -> Evaluator
_evalList xs c s = do
  ys <- evalList xs c []
  return $ s `push` (KList $ List $ reverse ys)

_evalIdent :: Text -> Evaluator
_evalIdent i c s  = maybe (_pushIdent i c s >>= primCall c)
                    (\p -> p c s) $ lookup i primitives

-- TODO
_pushIdent :: Text -> Evaluator
_pushIdent i c s = D.lookup c i >>= maybe err (return . push s)
  where
    err = throwIO $ LookupFailed $ T.unpack i

_evalBlock :: Block -> Evaluator
_evalBlock b c s
  = return $ s `push` KBlock b { blkScope = Just $ ctxScope c }

-- convenience --

evalList :: [KValue] -> Evaluator
evalList []     _ s = return s
evalList (x:xt) c s = eval x c s >>= evalList xt c

evalText :: FilePath -> Text -> Evaluator
evalText name code c s = evalList (R.read' name code) c s

evalStdin :: Context -> Stack -> IO ()
evalStdin c s = () <$ do
  code <- T.getContents; evalText "(stdin)" code c s

evalFile :: FilePath -> Evaluator
evalFile f c s = do code <- T.readFile f; evalText f code c s

-- primitives --

-- TODO
primitives, __primitives__ :: [(Text, Evaluator)]
primitives = [                                                --  {{{1
    ("def"            , primDef),
    ("__nya__"        , Prim.nya),
    ("__show-stack__" , showStack),
    ("__clear-stack__", clearStack)
  ] ++ __primitives__
__primitives__ = [
  -- NB: these must all match __*__ for preludePrims
    ("__call__"       , primCall),
    ("__if__"         , primIf),
    ("__=>__"         , primMkPair),
    ("__say__"        , primSay),
    ("__+__"          , primIntArith (+)),
    ("__-__"          , primIntArith (-)),
    ("__*__"          , primIntArith (*))
  ]                                                           --  }}}1

preludePrims, replPrims :: Context -> IO ()

preludePrims ctx
  = traverse_ (aliasPrim ctx)
  $ [ T.drop 2 $ T.dropEnd 2 k | (k, _) <- __primitives__ ]

replPrims ctx
  = traverse_ (aliasPrim ctx) ["show-stack", "clear-stack"]

aliasPrim :: Context -> Identifier -> IO ()
aliasPrim ctx name = defineIn ctx name $ blk $ "__" <> name <> "__"
  where
    blk i = KBlock $ Block [] [idt i] $ Just $ ctxScope ctx
    idt   = KIdent . (maybe err id) . ident
    err   = error "INVALID IDENTIFIER"

primDef, primCall, primIf, primMkPair, primSay :: Evaluator

-- TODO: error if primitive
primDef c s = do ((Kwd k, v), s') <- pop' s; s' <$ defineIn c k v

-- TODO
primCall c s0 = do
    (Block{..}, s1) <- pop' s0
    scope           <- maybe err return blkScope
    (s2, args)      <- popArgs [] s1 $ reverse blkArgs
    evalList blkCode (forkScope args c scope) s2
  where
    popArgs r s []      = return (s, r)
    popArgs r s (k:kt)  = do  (v, s') <- pop' s
                              popArgs ((unIdent k, v):r) s' kt
    err = throwIO EvalScopelessBlock

primIf c s = do ((b, tb, fb), s') <- pop' s
                primCall c $ push' s' $ if D.truthy b then tb else fb

primMkPair _ s = do ((k, v), s') <- pop' s; return $ s' `push` pair k v

-- NB: uses stdout implicitly
primSay _ s = do (x, s') <- pop' s; s' <$ T.putStrLn x

primIntArith :: (Integer -> Integer -> Integer) -> Evaluator
primIntArith op _ s = do  ((x, y), s') <- pop' s
                          return $ s' `push` (x `op` y)

-- repl --

showStack, clearStack :: Evaluator

-- TODO
showStack   _ s = s <$ traverse_ (putStrLn . show) s
clearStack  _ _ = return []

-- prelude --

preludeFile :: IO FilePath
preludeFile = getDataFileName "lib/prelude.knk"

initContextWithPrelude :: IO Context
initContextWithPrelude = do
  ctx   <- D.initContext
  ctxP  <- D.forkContext D.preludeModule ctx
  pre   <- preludeFile
  preludePrims ctxP
  ctx <$ evalFile pre ctxP D.emptyStack

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

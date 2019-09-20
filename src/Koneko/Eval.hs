--  --                                                          ; {{{1
--
--  File        : Koneko/Eval.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-09-20
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE OverloadedStrings #-}

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
-- >>> ev [val 1, val 2, KIdent $ id "__int_-__"]
-- [-1]
--
-- >>> let ev x = evalText "" x undefined []
--
-- >>> ev "\"Hello, World!\" __say__"
-- Hello, World!
-- []
-- >>> ev "1 2 __int_+__"
-- [3]
--
-- ... TODO ...
--

                                                              --  }}}1

module Koneko.Eval (
  eval, evalList, evalText, evalStdin, evalFile,
  initContextWithPrelude, truthy
) where

import Data.Text.Lazy (Text)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Koneko.Data hiding (lookup)
import Paths_koneko (getDataFileName)

import qualified Koneko.Read as R
import qualified Koneko.Data as D

-- TODO: Either for errors
type Evaluator = Context -> Stack -> IO Stack

-- TODO:
-- * say, +, -, ... --> defaultScope
eval :: KValue -> Evaluator
eval x c s = case x of
  KPrim _         -> return $ s `push` x
  KPair _         -> error "eval: unexpected pair"
  KList (List l)  -> _evalList l c s
  KIdent i        -> _evalIdent (unIdent i) c s
  KQuot i         -> _pushIdent (unIdent i) c s
  KBlock b        -> _evalBlock b c s

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
    err = error $ "*** lookup failed: " ++ T.unpack i ++ " ***"

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
primitives :: [(Text, Evaluator)]
primitives = [                                                --  {{{1
    ("def"        , primDef),
    ("__call__"   , primCall),
    ("__if__"     , primIf),
    ("__=>__"     , primMkPair),
    ("__say__"    , primSay),
    ("__int_+__"  , primIntArith (+)),
    ("__int_-__"  , primIntArith (-)),
    ("__int_*__"  , primIntArith (*))
  ]                                                           --  }}}1

primDef, primCall, primIf, primMkPair, primSay :: Evaluator

primDef c s = let ((Kwd k, v), s') = pop' s in s' <$ defineIn c k v

primCall = error "TODO"

primIf c s  = let ((b, tb, fb), s') = pop' s
              in primCall c $ push' s' $ if truthy b then tb else fb

primMkPair _ s  = let ((k, v), s') = pop' s
                  in return $ s' `push` pair k v

primSay _ s = let (x, s') = pop' s in s' <$ T.putStrLn x

primIntArith :: (Integer -> Integer -> Integer) -> Evaluator
primIntArith op _ s
  = let ((x,y),s') = pop' s in return $ s' `push` (x `op` y)

-- prelude --

preludeFile :: IO FilePath
preludeFile = getDataFileName "lib/prelude.knk"

initContextWithPrelude :: IO Context
initContextWithPrelude = do
  ctx   <- D.initContext
  ctxP  <- D.forkContext D.preludeModule ctx
  pre   <- preludeFile
  ctx <$ evalFile pre ctxP D.emptyStack

-- utilities --

truthy :: KValue -> Bool
truthy (KPrim KNil)           = False
truthy (KPrim (KBool False))  = False
truthy _                      = True

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

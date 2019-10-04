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
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wwarn #-}

                                                              --  {{{1
-- |
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Maybe
-- >>> let id = fromJust . ident
-- >>> let ev x = eval x undefined []
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
  tryK, eval, evalText, evalStdin, evalFile, replPrims,
  initContextWithPrelude
) where

import Control.Exception (throwIO, try)
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import System.Environment (lookupEnv)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Koneko.Data hiding (lookup)
import Paths_koneko (getDataFileName)

import qualified Koneko.Read as R
import qualified Koneko.Data as D
import qualified Koneko.Prim as Prim

tryK :: IO a -> IO (Either KException a)
tryK = try

-- interface --

eval :: [KValue] -> Evaluator
eval xs c s = fst <$> evaluate xs Top c s

evalText :: FilePath -> Text -> Evaluator
evalText name code c s = eval (R.read' name code) c s

evalStdin :: Context -> Stack -> IO ()
evalStdin c s = () <$ do
  code <- T.getContents; evalText "(stdin)" code c s

evalFile :: FilePath -> Evaluator
evalFile f c s = do code <- T.readFile f; evalText f code c s

-- implementation --

data Level = Top | Nested deriving Show
data CallT = Done | Tail  deriving Show

type TCEvaluator = Context -> Stack -> IO (Stack, CallT)

-- TODO: use __debug__ instead of env!?
evaluate, evaluate_ :: [KValue] -> Level -> TCEvaluator
evaluate xs l c s = do
  debug <- maybe False (== "yes") <$> lookupEnv "KONEKO_DEBUG"
  when debug $ do
    putStrLn "==> evaluate"
    putStrLn $ "  code : " ++ intercalate " " (map show xs)
    putStrLn $ "  level: " ++ show l
    putStrLn $ "  stack: " ++ intercalate " " (map show $ reverse s)
  r@(s', _) <- evaluate_ xs l c s
  when debug $ do
    putStrLn "<== evaluate"
    putStrLn $ "  stack: " ++ intercalate " " (map show $ reverse s')
  return r

evaluate_ []     _ _ s = return (s, Done)
evaluate_ [x]    l c s = do
  r@(s', t) <- eval1 x c s
  case (l, t) of (Top, Tail) -> (, Done) <$> call c s'; _ -> return r
evaluate_ (x:xt) l c s = do
  (s', t) <- eval1 x c s
  s'' <- case t of Tail -> call c s'; Done -> return s'
  evaluate xt l c s''

-- TODO: callable?
eval1 :: KValue -> TCEvaluator
eval1 x c s = case x of
  KPrim _         -> (, Done) <$> return (s `push` x)
  KList (List l)  -> (, Done) <$> evalList l c s
  KIdent i        -> (, Tail) <$> pushIdent (unIdent i) c s
  KQuot i         -> (, Done) <$> pushIdent (unIdent i) c s
  KBlock b        -> (, Done) <$> evalBlock b c s
  _               -> throwIO $ EvalUnexpected $ typeToStr $ typeOf x

-- TODO
evalList :: [KValue] -> Evaluator
evalList xs c s = do
  ys <- fst <$> evaluate xs Top c []
  return $ s `push` (KList $ List $ reverse ys)

-- TODO
pushIdent :: Text -> Evaluator
pushIdent i c s = D.lookup c i >>= maybe err (return . push s)
  where
    err = throwIO $ LookupFailed $ T.unpack i

evalBlock :: Block -> Evaluator
evalBlock b c s
  = return $ s `push` KBlock b { blkScope = Just $ ctxScope c }

-- TODO
call :: Evaluator
call c s = do
  (x, s') <- pop' s
  case x of
    KPair _     -> error "TODO"
    KList _     -> error "TODO"
    KDict _     -> error "TODO"
    KBlock b    -> callBlock b c s'
    KCallable f -> callCallable f c s'
    KMulti _    -> error "TODO"
    KRecordT _  -> error "TODO"
    KRecord _   -> error "TODO"
    _           -> throwIO $ UncallableType $ typeToStr $ typeOf x

callBlock :: Block -> Evaluator
callBlock Block{..} c s0 = do
    scope           <- maybe err return blkScope
    (s1, args)      <- popArgs [] s0 $ reverse blkArgs
    fst <$> evaluate blkCode Top (forkScope args c scope) s1
  where
    popArgs r s []      = return (s, r)
    popArgs r s (k:kt)  = do  (v, s') <- pop' s
                              popArgs ((unIdent k, v):r) s' kt
    err = throwIO EvalScopelessBlock

-- TODO
callCallable :: Callable -> Evaluator
callCallable = error "TODO"

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
--  ("__call__"       , primCall),
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

primDef, primIf, primMkPair, primSay :: Evaluator

-- TODO: error if primitive
primDef c s = do ((Kwd k, v), s') <- pop' s; s' <$ defineIn c k v

primIf c s = do ((b, tb, fb), s') <- pop' s
                call c $ push' s' $ if D.truthy b then tb else fb

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

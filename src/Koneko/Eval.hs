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
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Text.Lazy (Text)
import Prelude hiding (lookup)
import System.Environment (lookupEnv)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Koneko.Data

import qualified Koneko.Read as R

import qualified Koneko.Bltn as Bltn
import qualified Koneko.Prim as Prim
import qualified Koneko.Prld as Prld

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
pushIdent i c s = lookup c i >>= maybe err (return . push s)
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
    KCallable f -> cllRun f c s'
    KMulti _    -> error "TODO"
    KRecordT _  -> error "TODO"
    KRecord _   -> error "TODO"
    _           -> throwIO $ UncallableType $ typeToStr $ typeOf x

-- TODO
callBlock :: Block ->  Evaluator
callBlock Block{..} c s0 = do
    scope       <- maybe err return blkScope
    (s1, args)  <- popArgs [] s0 $ reverse blkArgs
    fst <$> evaluate blkCode Top (forkScope args c scope) s1
  where
    popArgs r s []      = return (s, r)
    popArgs r s (k:kt)  = do  (v, s') <- pop' s
                              popArgs ((unIdent k, v):r) s' kt
    err = throwIO EvalScopelessBlock

-- initial context --

initContext :: IO Context
initContext = do
  ctx     <- initMainContext
  ctxPrim <- Prim.initCtx ctx call
  ctxBltn <- Bltn.initCtx ctxPrim
  ctxPrld <- Prld.initCtx ctxBltn
  pre     <- Prld.modFile
  ctx <$ evalFile pre ctxPrld emptyStack

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

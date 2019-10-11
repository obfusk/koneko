--  --                                                          ; {{{1
--
--  File        : Koneko/Eval.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-10
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
-- >>> ev [int 1, int 2, KIdent $ id "-"]
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
import Data.List hiding (lookup)
import Data.Monoid((<>))
import Data.Text.Lazy (Text)
import Prelude hiding (lookup)
import Safe (atMay)
import System.IO (hPutStrLn, stderr)

import qualified Data.HashMap.Lazy as H
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Koneko.Data

import qualified Koneko.Read as R

import qualified Koneko.Bltn as Bltn
import qualified Koneko.Prim as Prim
import qualified Koneko.Prld as Prld

tryK :: IO a -> IO (Either KException a)
tryK = try

-- eval --

eval :: [KValue] -> Evaluator
eval []     _ s   = return s
eval (x:xt) c s0  = do
  (s1, deferredCall) <- eval1 x c s0
  s2 <- if deferredCall then call c s1 else return s1
  eval xt c s2

evalText :: FilePath -> Text -> Evaluator
evalText name code = eval $ R.read' name code

evalStdin :: Context -> Stack -> IO ()
evalStdin c s = () <$ do
  code <- T.getContents; evalText "(stdin)" code c s

evalFile :: FilePath -> Evaluator
evalFile f c s = do code <- T.readFile f; evalText f code c s

eval1, eval1_ :: KValue -> Context -> Stack -> IO (Stack, Bool)

eval1 x c s = do
  debug <- getDebug c
  when debug $ do
    hPutStrLn stderr $ "==> eval " ++ show x
    hPutStrLn stderr $ "--> " ++ intercalate " " (map show $ reverse s)
  r@(s', _) <- eval1_ x c s
  when debug $
    hPutStrLn stderr $ "<-- " ++ intercalate " " (map show $ reverse s')
  return r

eval1_ x c s = case x of
  KPrim _         -> (,False) <$> rpush1 s x
  KList (List l)  -> (,False) <$> evalList l c s
  KIdent i        -> (,True ) <$> pushIdent (unIdent i) c s
  KQuot i         -> (,False) <$> pushIdent (unIdent i) c s
  KBlock b        -> (,False) <$> evalBlock b c s
  _               -> throwIO $ EvalUnexpected $ typeToStr $ typeOf x

-- TODO
evalList :: [KValue] -> Evaluator
evalList xs c s = do ys <- eval xs c emptyStack; rpush1 s $ reverse ys

-- TODO
pushIdent :: Text -> Evaluator
pushIdent i c s = lookup c i >>= maybe err (return . push s)
  where
    err = throwIO $ LookupFailed $ T.unpack i

evalBlock :: Block -> Evaluator
evalBlock b c s = rpush1 s b { blkScope = Just $ ctxScope c }

-- call --

-- TODO
call :: Evaluator
call c s = do
  getDebug c >>= flip when (hPutStrLn stderr "*** call ***")
  (x, s') <- pop' s
  case x of
    KPrim (KStr _)  -> throwIO $ NotImplementedError "call str"
    KPair p         -> callPair p c s'
    KList l         -> callList l c s'
    KDict d         -> callDict d c s'
    KBlock b        -> callBlock b c s'
    KBuiltin b      -> biRun b c s'
    KMulti _        -> throwIO $ NotImplementedError "call multi"
    KRecordT _      -> throwIO $ NotImplementedError "call record-type"
    KRecord _       -> throwIO $ NotImplementedError "call record"
    _               -> throwIO $ UncallableType $ typeToStr $ typeOf x

callPair :: Pair -> Evaluator
callPair Pair{..} _ s = do
  (Kwd op, s') <- pop' s
  case op of
    "key"   -> rpush1 s' key
    "value" -> rpush1 s' value
    _       -> throwIO $ UnknownField (T.unpack op) "pair"

-- TODO
callList :: List -> Evaluator
callList (List l) _ s = do
  (Kwd op, s') <- pop' s
  let o = "list." <> op
      g = when (null l) $ throwIO $ EmptyList $ T.unpack o
  case op of
    "head"    ->  g >> rpush1 s' (head l)                     -- safe!
    "tail"    ->  g >> rpush1 s' (tail l)                     -- safe!
    "uncons"  ->  g >> rpush s' [head l, list $ tail l]       -- safe!
    "cons"    ->  rpush1 s' $ mkPrim o $ \_ s1 -> do
                    (x, s2) <- pop' s1; rpush1 s2 (x:l)
    "empty?"  ->  rpush1 s' $ null l
    "len"     ->  rpush1 s' $ len l
    "get"     ->  rpush1 s' $ mkPrim o $ \_ s1 -> do
                    (i, s2) <- pop' s1
                    case atMay l $ fromInteger i of
                      Nothing -> throwIO $ IndexError (T.unpack o) i
                      Just x  -> rpush1 s2 x
    "member?" ->  rpush1 s' $ mkPrim o $ \_ s1 -> do
                    (i, s2) <- pop' s1
                    rpush1 s2 $ 0 <= i && i < len l
    "elem?"   ->  rpush1 s' $ mkPrim o $ \_ s1 -> do
                    (x, s2) <- pop' s1; rpush1 s2 $ x `elem` l
    _         ->  throwIO $ UnknownField (T.unpack op) "list"

-- TODO
callDict :: Dict -> Evaluator
callDict (Dict h) _ s = do
  (Kwd op, s') <- pop' s
  let o = "dict." <> op
  case op of
    "empty?"  ->  rpush1 s' $ H.null h
    "len"     ->  rpush1 s' $ toInteger $ H.size h
    "get"     ->  rpush1 s' $ mkPrim o $ \_ s1 -> do
                    (Kwd k, s2) <- pop' s1
                    case H.lookup k h of
                      Nothing -> throwIO $ KeyError (T.unpack o)
                                                    (T.unpack k)
                      Just x  -> rpush1 s2 x
    "member?" ->  rpush1 s' $ mkPrim o $ \_ s1 -> do
                    (Kwd k, s2) <- pop' s1; rpush1 s2 $ H.member k h
    _         ->  throwIO $ UnknownField (T.unpack op) "dict"

-- TODO
callBlock :: Block -> Evaluator
callBlock Block{..} c s0 = do
    sc          <- getScope blkScope
    (s1, args)  <- popArgs [] s0 $ reverse nargs
    eval blkCode (forkScope (args ++ map (,nil) sargs) c sc) s1
  where
    (sargs, nargs) = partitionSpecial $ map unIdent blkArgs

-- apply --

-- TODO
apply :: Evaluator
apply c s0 = do
  (Block{..}, s1) <- pop' s0; sc <- getScope blkScope
  (l, s2)         <- pop' s1
  let (sargs, nargs)  = partitionSpecial $ map unIdent blkArgs
      sargs'          = delete "&" sargs
      lna             = len nargs
  when (len l < lna) $ throwIO $ ApplyExpected lna
  when (len l > lna && "&" `notElem` sargs) $
    throwIO $ ApplyMissing "&"
  let (l1, l2)  = splitAt (fromInteger lna) l
      args      = zip nargs l1 ++ map (,nil) sargs' ++ [("&", list l2)]
  s3 <- eval blkCode (forkScope args c sc) emptyStack
  return $ s3 ++ s2

-- TODO
applyDict :: Evaluator
applyDict c s0 = do
    (Block{..}, s1) <- pop' s0; sc <- getScope blkScope
    (d, s2)         <- pop' s1
    let (sargs, nargs)  = partitionSpecial $ map unIdent blkArgs
        sargs'          = delete "&&" sargs
        h               = unDict d
    when ("&&" `notElem` sargs) $ throwIO $ ApplyMissing "&&"
    vals <- look h nargs
    let h'    = H.filterWithKey (\k _ -> k `notElem` nargs) h
        args  = zip nargs vals ++ map (,nil) sargs' ++
                [("&&", KDict $ Dict h')]
    s3 <- eval blkCode (forkScope args c sc) emptyStack
    return $ s3 ++ s2
  where
    look h ks   = either (throwIO . KeyError "apply-dict") return
                $ traverse (lookOne h) ks
    lookOne h k = maybe (Left $ T.unpack k) Right $ H.lookup k h

-- initial context --

initContext :: IO Context
initContext = do
  ctx     <- initMainContext
  ctxPrim <- Prim.initCtx ctx call apply applyDict
  ctxBltn <- Bltn.initCtx ctxPrim
  ctxPrld <- Prld.initCtx ctxBltn
  pre     <- Prld.modFile
  ctx <$ evalFile pre ctxPrld emptyStack

-- utilities: block call/apply --

getScope :: Maybe Scope -> IO Scope
getScope = maybe (throwIO EvalScopelessBlock) return

popArgs :: Args -> Stack -> [Identifier] -> IO (Stack, Args)
popArgs r s []      = return (s, r)
popArgs r s (k:kt)  = do
  (v, s') <- pop' s; popArgs ((k, v):r) s' kt

partitionSpecial :: [Identifier] -> ([Identifier], [Identifier])
partitionSpecial = partition (`elem` ["&", "&&"])

-- utilities --

len :: [a] -> Integer
len = genericLength

getDebug :: Context -> IO Bool
getDebug c = maybe False (== true) <$> lookup c "__debug__"

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

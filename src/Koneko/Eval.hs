--  --                                                          ; {{{1
--
--  File        : Koneko/Eval.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-11-26
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE NoMonomorphismRestriction #-}
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
-- >>> ev [str "Hello, World!", KIdent $ id "say!"]
-- Hello, World!
-- []
-- >>> ev [int 1, int 2, KIdent $ id "-"]
-- [-1]
--
-- >>> ev x = evalText "" x ctx []
--
-- >>> ev "\"Hello, World!\" say!"
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

import Control.DeepSeq (($!!))
import Control.Exception (throwIO, try)
import Control.Monad (unless, when)
import Data.Char (ord)
import Data.List hiding (lookup)
import Data.Monoid((<>))
import Data.Text.Lazy (Text)
import Prelude hiding (lookup)
import Safe (atMay)
import System.IO (hPutStrLn, stderr)

import qualified Data.HashMap.Strict as H
import qualified Data.HashTable.IO as HT
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Koneko.Data
import Koneko.Misc (firstJust)

import qualified Koneko.Read as R

import qualified Koneko.Bltn as Bltn
import qualified Koneko.Prim as Prim
import qualified Koneko.Prld as Prld

tryK :: IO a -> IO (Either KException a)
tryK = try

-- eval --

eval, evl :: [KValue] -> Evaluator

eval xs c s = (return $!!) =<< evl xs c s

evl    []  _ s  = return s
evl (x:xt) c s0 = do
  (s1, deferredCall) <- eval1 x c s0
  let f = if deferredCall then call c s1 else return s1
  if null xt then f else f >>= evl xt c   -- tail call!

evalText :: FilePath -> Text -> Evaluator
evalText name code = eval $ R.read' name code

evalStdin :: Context -> Stack -> IO ()
evalStdin c s = () <$ do
  code <- T.getContents; evalText "(stdin)" code c s

evalFile :: FilePath -> Evaluator
evalFile f c s = do code <- T.readFile f; evalText f code c s

eval1, eval1_ :: KValue -> Context -> Stack -> IO (Stack, Bool)

eval1 x c s = do
  debug c $ do
    hPutStrLn stderr $ "==> eval " ++ show x
    hPutStrLn stderr $ "--> " ++ intercalate " " (map show $ reverse s)
  r@(s', _) <- eval1_ x c $!! s
  debug c $
    hPutStrLn stderr $ "<-- " ++ intercalate " " (map show $ reverse s')
  return $!! r

eval1_ x c s = case x of
  KPrim _         -> (,False) <$> rpush1 s x
  KList (List l)  -> (,False) <$> evalList l c s
  KIdent i        -> (,True ) <$> pushIdent (unIdent i) c s
  KQuot i         -> (,False) <$> pushIdent (unIdent i) c s
  KBlock b        -> (,False) <$> evalBlock b c s
  _               -> throwIO $ EvalUnexpected $ typeToStr $ typeOf x

-- TODO
evalList :: [KValue] -> Evaluator
evalList xs c s = do ys <- evl xs c emptyStack; rpush1 s $ reverse ys

-- TODO
pushIdent :: Text -> Evaluator
pushIdent i c s = lookup c i >>= maybe err (return . push s)
  where
    err = throwIO $ LookupFailed $ T.unpack i

evalBlock :: Block -> Evaluator
evalBlock b c s = rpush1 s b { blkScope = Just $ ctxScope c }

-- call --

call :: Evaluator
call c s = do
  debug c $ hPutStrLn stderr "*** call ***"
  (x, s') <- pop' s
  case x of
    KPrim (KStr y)  -> callStr      y c s'
    KPair p         -> callPair     p c s'
    KList l         -> callList     l c s'
    KDict d         -> callDict     d c s'
    KBlock b        -> callBlock    b c s'
    KBuiltin b      -> biRun        b c s'
    KMulti m        -> callMulti    m c s'
    KRecordT r      -> callRecordT  r c s'
    KRecord r       -> callRecord   r c s'
    KThunk t        -> callThunk    t c s'
    _               -> throwIO $ UncallableType $ typeToStr $ typeOf x

-- TODO
callStr :: Text -> Evaluator
callStr x _ s = do
  (Kwd op, s') <- pop' s
  let o = "str." <> op; p = rpush1 s'; pr = p . mkOp o
  case op of
    "ord"     ->  do  unless (T.length x == 1) $ throwIO $
                        stackExpected "str of length 1"
                      p $ toInteger $ ord $ T.head x
    "append"  ->  pr $ pop1push1 (<> x)
    "slice"   ->  pr $ \_ s1 -> do
                    ((i, j, step), s2) <- pop3' s1; let lx = lengthT x
                    i' <- nilToDef i 0; j' <- nilToDef j lx
                    unless (step == 1) $ throwIO $ NotImplementedError
                      $ T.unpack $ op <> ": step other than 1"
                    rpush1 s2 $ slice T.take T.drop i' j' step lx x
    "empty?"  ->  p $ T.null x
    "len"     ->  p $ lengthT x
    "get^"    ->  pr $ \_ s1 -> do
                    (i, s2) <- pop' s1
                    let err = throwIO $ IndexError (T.unpack o) i
                    maybe err (rpush1 s2) $ indexT x i
    "member?" ->  pr $ pop1push1 $ mem lengthT x
    "elem?"   ->  pr $ pop1push1 (`T.isInfixOf` x)
    _         ->  throwIO $ UnknownField (T.unpack op) "str"

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
  let o = "list." <> op; p = rpush1 s'; pr = p . mkOp o
      g = when (null l) $ throwIO $ EmptyList $ T.unpack o
  case op of
    "head^"   ->  g >> p (head l)                             -- safe!
    "tail^"   ->  g >> p (tail l)                             -- safe!
    "uncons^" ->  g >> rpush s' [head l, list $ tail l]       -- safe!
    "cons"    ->  pr $ pop1push1 (:l)
    "sort"    ->  p $ sort l
    "append"  ->  pr $ pop1push1 (++ l)
    "slice"   ->  pr $ \_ s1 -> do
                    ((i, j, step), s2) <- pop3' s1; let ll = len l
                    i' <- nilToDef i 0; j' <- nilToDef j ll
                    unless (step == 1) $ throwIO $ NotImplementedError
                      $ T.unpack $ op <> ": step other than 1"
                    rpush1 s2 $ slice take drop i' j' step ll l
    "empty?"  ->  p $ null l
    "len"     ->  p $ len l
    "get^"    ->  pr $ \_ s1 -> do
                    (i, s2) <- pop' s1
                    let err = throwIO $ IndexError (T.unpack o) i
                    maybe err (rpush1 s2) $ atMay l $ fromInteger i
    "member?" ->  pr $ pop1push1 $ mem len l
    "elem?"   ->  pr $ pop1push1 (`elem` l)
    _         ->  throwIO $ UnknownField (T.unpack op) "list"

-- TODO
callDict :: Dict -> Evaluator
callDict (Dict h) _ s = do
  (Kwd op, s') <- pop' s
  let o = "dict." <> op; p = rpush1 s'; pr = p . mkOp o
  case op of
    "keys"    ->  p $ map kwd $ H.keys h
    "values"  ->  p $ H.elems h
    "pairs"   ->  p [ pair (Kwd k) v | (k, v) <- H.toList h ]
    "merge"   ->  pr $ \_ s1 -> do
                    (Dict h2, s2) <- pop' s1
                    rpush1 s2 $ Dict $ H.union h h2
    "empty?"  ->  p $ H.null h
    "len"     ->  p $ toInteger $ H.size h
    "get^"    ->  pr $ \_ s1 -> do
                    (Kwd k, s2) <- pop' s1
                    let err = throwIO $ KeyError (T.unpack o) (T.unpack k)
                    maybe err (rpush1 s2) $ H.lookup k h
    "member?" ->  pr $ \_ s1 -> do
                    (Kwd k, s2) <- pop' s1; rpush1 s2 $ H.member k h
    _         ->  throwIO $ UnknownField (T.unpack op) "dict"

-- TODO
callMulti :: Multi -> Evaluator
callMulti Multi{..} c s = do
    sig <- map toSig . fst <$> popN' mltArity s
    let f b = callBlock b c s; look = HT.lookup mltTable
    maybe (err sig) f =<< firstJust [look sig, look def]
  where
    toSig (KRecord r) = recordTypeSig $ recType r
    toSig t           = typeToStr $ typeOf t
    def               = replicate mltArity "_"
    err sig           = throwIO $ MultiMatchFailed (T.unpack mltName)
                      $ show $ list $ map kwd sig

callRecord :: Record -> Evaluator
callRecord r _ s = do
    (Kwd k, s') <- pop' s
    let err = throwIO $ UnknownField (T.unpack k) (T.unpack recName)
    maybe err (rpush1 s' . (recValues r !!)) $ elemIndex k recFields -- safe!
  where
    RecordT{..} = recType r

callThunk :: Thunk -> Evaluator
callThunk t _ s = rpush1 s =<< runThunk t

-- apply --

-- TODO
apply :: Evaluator
apply c s = do
  debug c $ hPutStrLn stderr "*** apply ***"
  (x, s') <- pop' s
  case x of
    KBlock b    -> applyBlock b c s'
    KMulti _    -> throwIO $ NotImplementedError "apply multi"
    KRecordT r  -> applyRecordT r c s'
    _           -> throwIO $ UnapplicableType (typeToStr $ typeOf x) "apply"

-- TODO
apply_dict :: Evaluator
apply_dict c s = do
  debug c $ hPutStrLn stderr "*** apply-dict ***"
  (x, s') <- pop' s
  case x of
    KBlock b    -> apply_dictBlock b c s'
    KMulti _    -> throwIO $ NotImplementedError "apply-dict multi"
    KRecordT r  -> apply_dictRecordT r c s'
    _           -> throwIO $ UnapplicableType (typeToStr $ typeOf x) "apply-dict"

-- call & apply: block --

-- TODO
callBlock :: Block -> Evaluator
callBlock Block{..} c s0 = do
    sc          <- getScope blkScope
    (s1, args)  <- popArgs [] s0 $ reverse nparms
    evl blkCode (forkScope (args ++ map (,nil) sparms) c sc) s1
  where
    (sparms, nparms) = partitionSpecial $ map unIdent blkParams

-- TODO
applyBlock :: Block -> Evaluator
applyBlock Block{..} c s0 = do
    sc <- getScope blkScope; (l, s1) <- pop' s0; let ll = length l
    when (ll < lnp) $ throwIO $ expected $ show lnp ++ " arg(s) for apply"
    when (ll > lnp && "&" `notElem` sparms) $ throwIO $ applyMissing False
    let (l1, l2)  = splitAt lnp l
        args      = zip nparms l1 ++ map (,nil) sparms' ++ [("&", list l2)]
    (++ s1) <$> evl blkCode (forkScope args c sc) emptyStack
  where
    (sparms, nparms)  = partitionSpecial $ map unIdent blkParams
    sparms'           = delete "&" sparms
    lnp               = length nparms

-- TODO
apply_dictBlock :: Block -> Evaluator
apply_dictBlock Block{..} c s0 = do
    sc <- getScope blkScope; (d@(Dict h), s1) <- pop' s0
    when ("&&" `notElem` sparms) $ throwIO $ applyMissing True
    vals <- retOrThrow $ dictLookup "apply-dict" d nparms
    let h'    = H.filterWithKey (\k _ -> k `notElem` nparms) h
        args  = zip nparms vals ++ map (,nil) sparms' ++
                [("&&", KDict $ Dict h')]
    (++ s1) <$> evl blkCode (forkScope args c sc) emptyStack
  where
    (sparms, nparms)  = partitionSpecial $ map unIdent blkParams
    sparms'           = delete "&&" sparms

-- call & apply: record-type --

callRecordT :: RecordT -> Evaluator
callRecordT t@RecordT{..} _ s = do
  (l, s') <- popN' (length recFields) s; _pushRec s' $ record t l

applyRecordT :: RecordT -> Evaluator
applyRecordT t _ s = do
  (l, s') <- pop' s; _pushRec s' $ record t l

apply_dictRecordT :: RecordT -> Evaluator
apply_dictRecordT t@RecordT{..} _ s = do
  (d@(Dict h), s') <- pop' s; let uf = H.keys h \\ recFields
  unless (null uf) $ throwIO $ unexpected $
    "key(s) " ++ (T.unpack $ T.intercalate ", " uf)
    ++ " for record " ++ T.unpack recName
  let l = dictLookup "record-type.apply-dict" d recFields
  _pushRec s' $ record t =<< l

_pushRec :: Stack -> Either KException Record -> IO Stack
_pushRec s r = retOrThrow r >>= rpush1 s . KRecord

-- initial context --

initContext :: IO Context
initContext = do
  ctx <- initMainContext
  Prim.initCtx ctx call apply apply_dict callBlock
  Bltn.initCtx ctx
  Prld.initCtx ctx evalFile
  return ctx

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

mkOp :: Identifier -> Evaluator -> Builtin
mkOp = mkPrim . (<> ")") . ("(" <>)

len :: [a] -> Integer
len = genericLength

mem :: (a -> Integer) -> a -> Integer -> Bool
mem l x i = 0 <= i && i < (l x)

lengthT :: Text -> Integer
lengthT = toInteger . T.length

-- TODO
indexT :: Text -> Integer -> Maybe Text
indexT t i = if mem lengthT t i then f i else Nothing
  where
    f = Just . T.singleton . T.index t . fromInteger

-- TODO
slice :: Num a => (a -> b -> b) -> (a -> b -> b)
      -> Integer -> Integer -> Integer -> Integer -> b -> b
slice tak drp i j _ ll l = tak (f j - i') $ drp i' l
  where
    f n = fromInteger $ if n < 0 then ll + n else n; i' = f i

nilToDef :: FromVal a => KValue -> a -> IO a
nilToDef x d = if isNil x then return d else retOrThrow $ fromVal x

debug :: Context -> IO () -> IO ()
debug c act
  = maybe False (== true) <$> lookup c "__debug__" >>= flip when act

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

--  --                                                          ; {{{1
--
--  File        : Koneko/Prim.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-06
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE OverloadedStrings #-}

module Koneko.Prim (initCtx, replDef) where

import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.List (isSuffixOf)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.Random (getStdRandom, randomR)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Koneko.Data
import Paths_koneko (getDataFileName)

-- TODO
initCtx :: Context -> Evaluator -> IO Context
initCtx ctxMain call = do
  ctxPrim <- forkContext primModule ctxMain
  traverse_ (defPrim ctxPrim) [
      mkPrim "call" call, apply call, if_ call,
      def, mkPair, swap, show_, say, type_, callable,
      arithI "__int+__" (+), arithI "__int-__" (-),
      arithI "__int*__" (*),
      arithI "__div__" div, arithI "__mod__" mod,
      arithF "__float+__" (+), arithF "__float-__" (-),
      arithF "__float*__" (*), arithF "__float/__" (/),
      intToFloat,
      not_, and_, or_,
      comp "=" (==), comp "/=" (/=), comp "<" (<),
      comp "<=" (<=), comp ">" (>), comp ">=" (>=),
      showStack, clearStack, nya
      -- ...
    ]
  return ctxPrim

-- primitives: miscellaneous --

apply, if_ :: Evaluator -> Builtin

-- TODO
apply call = mkPrim "apply" $ \c s0 -> do
  ((l, f), s1) <- pop2' s0
  s2 <- call c (f:reverse l)
  return $ s2 ++ s1

if_ call = mkPrim "if" $ \c s -> do
  ((cond, t_br, f_br), s') <- pop3' s
  call c $ push' s' $ if truthy cond then t_br else f_br

def, mkPair, swap, show_, say, type_, callable :: Builtin

def = mkPrim "def" $ \c s -> do
  ((Kwd k, v), s') <- pop2' s; s' <$ defineIn c k v

mkPair = mkPrim "=>" $ pop2push1 pair

-- needed as primitive by read for .foo
swap = mkPrim "swap" $ pop2push $ \x y -> [y, x] :: [KValue]

show_ = mkPrim "show" $ pop1push1 $ T.pack . (show :: KValue -> String)

-- NB: uses stdout
say = mkPrim "say" $ \_ s -> do (x, s') <- pop' s; s' <$ T.putStrLn x

type_ = mkPrim "type" $ pop1push1 $ typeToKwd . typeOf

callable = mkPrim "callable?" $ pop1push1 isCallable

-- primitives: arithmetic, comparison --

arith :: (FromVal a, ToVal a)
      => Identifier -> (a -> a -> a) -> Builtin
arith name op = mkPrim name $ pop2push1 op

arithI :: Identifier -> (Integer -> Integer -> Integer) -> Builtin
arithF :: Identifier -> (Double  -> Double  -> Double ) -> Builtin
arithI = arith
arithF = arith

intToFloat :: Builtin
intToFloat  = mkPrim "int->float"
            $ pop1push1 (fromInteger :: Integer -> Double)

-- primitives: Eq, Ord --

not_, and_, or_ :: Builtin
not_  = mkPrim "not"  $ pop1push1 $ not . truthy
and_  = mkPrim "and"  $ pop2push1 $ \x y -> truthy x && truthy y
or_   = mkPrim "or"   $ pop2push1 $ \x y -> truthy x || truthy y

comp :: Identifier -> (KValue -> KValue -> Bool) -> Builtin
comp name op = mkPrim name $ pop2push1 op

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

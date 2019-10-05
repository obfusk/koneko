--  --                                                          ; {{{1
--
--  File        : Koneko/Prim.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-05
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
      def, mkPair, swap, show_, say,
      arithI "__int+__" (+), arithI "__int-__" (-),
      arithI "__int*__" (*),
      arithI "__div__" div, arithI "__mod__" mod,
      arithF "__float+__" (+), arithF "__float-__" (-),
      arithF "__float*__" (*), arithF "__float/__" (/),
      intToFloat,
      not_, comp "=" (==), comp "/=" (/=), comp "<" (<),
      comp "<=" (<=), comp ">" (>), comp ">=" (>=),
      showStack, clearStack, nya
      -- ...
    ]
  return ctxPrim

defPrim :: Context -> Builtin -> IO ()
defPrim ctx f = defineIn ctx (biName f) $ KBuiltin f

-- primitives: miscellaneous --

apply, if_ :: Evaluator -> Builtin

-- TODO
apply call = mkPrim "apply" $ \c s -> do
  ((List l, f), s1) <- pop' s
  s2 <- call c (f:reverse l)
  return $ s2 ++ s1

if_ call = mkPrim "if" $ \c s -> do
  ((cond, t_br, f_br), s') <- pop' s
  call c $ push' s' $ if truthy cond then t_br else f_br

def, mkPair, swap, show_, say :: Builtin

def = mkPrim "def" $ \c s -> do
  ((Kwd k, v), s') <- pop' s; s' <$ defineIn c k v

mkPair = mkPrim "=>" $ pop2push $ \k v -> [pair k v]

-- needed as primitive by read for .foo
swap = mkPrim "swap" $ pop2push $ \x y -> [y, x] :: [KValue]

show_ = mkPrim "show" $ pop1push $ \x -> [T.pack $ show (x :: KValue)]

-- NB: uses stdout
say = mkPrim "say" $ \_ s -> do (x, s') <- pop' s; s' <$ T.putStrLn x

-- primitives: arithmetic, comparison --

arith :: (Pop a, Push a) => Identifier -> (a -> a -> a) -> Builtin
arith name op = mkPrim name $ pop2push $ \x y -> [x `op` y]

arithI :: Identifier -> (Integer -> Integer -> Integer) -> Builtin
arithF :: Identifier -> (Double  -> Double  -> Double ) -> Builtin
arithI = arith
arithF = arith

intToFloat :: Builtin
intToFloat
  = mkPrim "int->float" $ pop1push $ \x -> [fromInteger x :: Double]

-- primitives: Eq, Ord --

not_ :: Builtin
not_ = mkPrim "not" $ pop1push $ \x -> [not x]

comp :: Identifier -> (KValue -> KValue -> Bool) -> Builtin
comp name op = mkPrim name $ pop2push $ \x y -> [x `op` y]

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

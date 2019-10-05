--  --                                                          ; {{{1
--
--  File        : Koneko/Prim.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-04
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
      showStack, clearStack, nya,
      intArith "+" (+), intArith "-" (-), intArith "*" (*),   --  TODO
      intToFloat,
      eq, neq, lt, lte, gt, gte
      -- ...
    ]
  return ctxPrim

defPrim :: Context -> Builtin -> IO ()
defPrim ctx f = defineIn ctx (biName f) $ KBuiltin f

-- primitives: miscellaneous --

apply, if_ :: Evaluator -> Builtin

-- TODO
apply call = mkPrim "apply" $ \c s -> do
  ((List l, f), s') <- pop' s
  s'' <- call c (f:reverse l)
  return $ s'' ++ s'

if_ call = mkPrim "if" $ \c s -> do
  ((cond, t_br, f_br), s') <- pop' s
  call c $ push' s' $ if truthy cond then t_br else f_br

def, mkPair, swap, show_, say :: Builtin

def = mkPrim "def" $ \c s -> do
  ((Kwd k, v), s') <- pop' s; s' <$ defineIn c k v

mkPair = mkPrim "=>" $ \_ s -> do
  ((k, v), s') <- pop' s; return $ s' `push` pair k v

-- needed as primitive by read for .foo
swap = mkPrim "swap" $ pop2and $ \x y s' -> s' `push` y `push` x

show_ = mkPrim "show" $ \_ s -> do
  (x, s') <- pop' s; return $ s' `push` (T.pack $ show (x :: KValue))

-- NB: uses stdout
say = mkPrim "say" $ \_ s -> do
  (x, s') <- pop' s; s' <$ T.putStrLn x

-- primitives: arith --

intArith :: Identifier -> (Integer -> Integer -> Integer) -> Builtin
intArith name op = mkPrim name $ \_ s -> do
  ((x, y), s') <- pop' s; return $ s' `push` (x `op` y)

intToFloat :: Builtin
intToFloat = mkPrim "int->float" $ \_ s -> do
  (x, s') <- pop' s; return $ s' `push` (fromInteger x :: Double)

-- primitives: Eq, Ord --

eq, neq, lt, lte, gt, gte :: Builtin

eq  = mkPrim "="  $ pop2and $ \x y s' -> s' `push` (x == y)
neq = mkPrim "/=" $ pop2and $ \x y s' -> s' `push` (x /= y)
lt  = mkPrim "<"  $ pop2and $ \x y s' -> s' `push` (x <  y)
lte = mkPrim "<=" $ pop2and $ \x y s' -> s' `push` (x <= y)
gt  = mkPrim ">"  $ pop2and $ \x y s' -> s' `push` (x >  y)
gte = mkPrim ">=" $ pop2and $ \x y s' -> s' `push` (x >= y)

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

clearStack = mkPrim "__clear-stack__" $ \_ _ -> return []

-- nya --

nya :: Builtin
nya = mkPrim "__nya__" $ \_ s -> s <$ do
  nyaD  <- getDataFileName "nya"
  cats  <- filter (isSuffixOf ".cat") <$> listDirectory nyaD
  unless (null cats) $ do
    i   <- getStdRandom $ randomR (0, length cats -1)
    (T.readFile $ nyaD </> (cats !! i)) >>= T.putStr

-- utilities --

pop2and :: (KValue -> KValue -> Stack -> Stack) -> Evaluator
pop2and f _ s = do ((x, y), s') <- pop' s; return $ f x y s'

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

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
import Data.Monoid ((<>))
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.Random (getStdRandom, randomR)

import qualified Data.Text.Lazy.IO as T

import Koneko.Data
import Paths_koneko (getDataFileName)

-- TODO
initCtx :: Context -> TCall -> IO Context
initCtx ctxMain call = do
  ctxPrim <- forkContext primModule ctxMain
  traverse_ (defPrim ctxPrim) [
      mkPrim "call" call, if_ call, def, mkPair, say,
      showStack, clearStack, nya,
      intArith "+" (+), intArith "-" (-), intArith "*" (*),   --  TODO
      intToFloat
      -- ...
    ]
  return ctxPrim

defPrim :: Context -> Builtin -> IO ()
defPrim ctx f = defineIn ctx (biName f) $ KBuiltin f

-- primitives --

-- TODO: arith, eq, ...

if_ :: TCall -> Builtin

-- TODO
if_ call = mkPrim "if" $ \pos c s -> do
  ((cond, t_br, f_br), s') <- pop' s
  call pos c $ push' s' $ if truthy cond then t_br else f_br

def, mkPair, say :: Builtin

def = mkPrim "def" $ noTC $ \c s -> do
  ((Kwd k, v), s') <- pop' s; s' <$ defineIn c k v

mkPair = mkPrim "=>" $ noTC $ \_ s -> do
  ((k, v), s') <- pop' s; return $ s' `push` pair k v

-- NB: uses stdout
say = mkPrim "say" $ noTC $ \_ s -> do
  (x, s') <- pop' s; s' <$ T.putStrLn x

intArith :: Identifier -> (Integer -> Integer -> Integer) -> Builtin
intArith name op = mkPrim name $ noTC $ \_ s -> do
  ((x, y), s') <- pop' s; return $ s' `push` (x `op` y)

intToFloat :: Builtin
intToFloat = mkPrim "int->float" $ noTC $ \_ s -> do
  (x, s') <- pop' s; return $ s' `push` (fromInteger x :: Double)

-- repl --

replDef :: Context -> IO ()
replDef ctx = do
    alias "show-stack"  showStack
    alias "clear-stack" clearStack
  where
    alias n f = defineIn ctx n $ KBuiltin f

showStack, clearStack :: Builtin

-- TODO
showStack = mkPrim "__show-stack__" $ noTC $ \_ s ->
  s <$ traverse_ (putStrLn . show) s

clearStack = mkPrim "__clear-stack__" $ noTC $ \_ _ -> return []

-- nya --

nya :: Builtin
nya = mkPrim "__nya__" $ noTC $ \_ s -> s <$ do
  nyaD  <- getDataFileName "nya"
  cats  <- filter (isSuffixOf ".cat") <$> listDirectory nyaD
  unless (null cats) $ do
    i   <- getStdRandom $ randomR (0, length cats -1)
    (T.readFile $ nyaD </> (cats !! i)) >>= T.putStr

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

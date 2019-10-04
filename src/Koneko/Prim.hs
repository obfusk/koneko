--  --                                                          ; {{{1
--
--  File        : Koneko/Prim.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-03
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

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Koneko.Data
import Paths_koneko (getDataFileName)

-- TODO
initCtx :: Context -> Evaluator -> IO Context
initCtx ctxMain call = do
  ctxPrim <- forkContext primModule ctxMain
  traverse_ (defPrim ctxPrim) [
      Callable "call" call, if_ call, def, mkPair, say,
      showStack, clearStack, nya,
      intArith "+" (+), intArith "-" (-), intArith "*" (*),   --  TODO
      intToFloat
      -- ...
    ]
  return ctxPrim

defPrim :: Context -> Callable -> IO ()
defPrim ctx f = defineIn ctx (cllName f) $ KCallable f

-- NB: do not export
aliasPrim :: Context -> Identifier -> IO ()
aliasPrim ctx name = defineIn ctx name $ blk $ "__" <> name <> "__"
  where
    blk i = KBlock $ Block [] [idt i] $ Just $ ctxScope ctx
    idt   = KIdent . (maybe err id) . ident
    err   = error "INVALID IDENTIFIER"

-- primitives --

-- TODO: arith, ...

if_ :: Evaluator -> Callable

-- TODO
if_ call = Callable "if" $ \c s -> do
  ((cond, t_br, f_br), s') <- pop' s
  call c $ push' s' $ if truthy cond then t_br else f_br

def, mkPair, say :: Callable

def = Callable "def" $ \c s -> do
  ((Kwd k, v), s') <- pop' s; s' <$ defineIn c k v

mkPair = Callable "=>" $ \_ s -> do
  ((k, v), s') <- pop' s; return $ s' `push` pair k v

-- NB: uses stdout
say = Callable "say" $ \_ s -> do
  (x, s') <- pop' s; s' <$ T.putStrLn x

intArith :: Identifier -> (Integer -> Integer -> Integer) -> Callable
intArith name op = Callable name $ \_ s -> do
  ((x, y), s') <- pop' s; return $ s' `push` (x `op` y)

intToFloat :: Callable
intToFloat = Callable "int->float" $ \_ s -> do
  (x, s') <- pop' s; return $ s' `push` (fromInteger x :: Double)

-- repl --

replDef :: Context -> IO ()
replDef ctx = traverse_ (aliasPrim ctx) ["show-stack", "clear-stack"]

showStack, clearStack :: Callable

-- TODO
showStack = Callable "__show-stack__" $ \_ s ->
  s <$ traverse_ (putStrLn . show) s

clearStack = Callable "__clear-stack__" $ \_ _ -> return []

-- nya --

nya :: Callable
nya = Callable "__nya__" $ \_ s -> s <$ do
  nyaD  <- getDataFileName "nya"
  cats  <- filter (isSuffixOf ".cat") <$> listDirectory nyaD
  unless (null cats) $ do
    i   <- getStdRandom $ randomR (0, length cats -1)
    (T.readFile $ nyaD </> (cats !! i)) >>= T.putStr

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

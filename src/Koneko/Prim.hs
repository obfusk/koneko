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

{-# OPTIONS_GHC -Wwarn #-}

module Koneko.Prim (initCtx, preludeDef, replDef, nya) where

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

initCtx :: Context -> Evaluator -> IO Context
initCtx ctxMain call = do
  ctxPrim <- forkContext bltnModule ctxMain
  -- ...
  return ctxPrim

preludeDef, replDef :: Context -> IO ()

preludeDef ctx
  = traverse_ (aliasPrim ctx)
  $ [ T.drop 2 $ T.dropEnd 2 k | (k, _) <- [] ] -- __primitives__

replDef ctx
  = traverse_ (aliasPrim ctx) ["show-stack", "clear-stack"]

-- NB: do not export
aliasPrim :: Context -> Identifier -> IO ()
aliasPrim ctx name = defineIn ctx name $ blk $ "__" <> name <> "__"
  where
    blk i = KBlock $ Block [] [idt i] $ Just $ ctxScope ctx
    idt   = KIdent . (maybe err id) . ident
    err   = error "INVALID IDENTIFIER"

-- primitives --

{-

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

-}

-- repl --

showStack, clearStack :: Evaluator

-- TODO
showStack   _ s = s <$ traverse_ (putStrLn . show) s
clearStack  _ _ = return []

-- nya --

nya :: KValue
nya = callable "nya" $ \_ s -> s <$ do
  nyaD  <- getDataFileName "nya"
  cats  <- filter (isSuffixOf ".cat") <$> listDirectory nyaD
  unless (null cats) $ do
    i   <- getStdRandom $ randomR (0, length cats -1)
    (T.readFile $ nyaD </> (cats !! i)) >>= T.putStr

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

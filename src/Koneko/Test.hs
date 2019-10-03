--  --                                                          ; {{{1
--
--  File        : Koneko/Test.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-10-02
--
--  Copyright   : Copyright (C) 2019  Felix C. Stegerman
--  Version     : v0.0.1
--  License     : GPLv3+
--
--  --                                                          ; }}}1

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Koneko.Test (
  doctest, doctest', testFiles, testKoneko, testMarkdown, testKoneko_,
  testMarkdown_, testKonekoFile, testMarkdownFile, testKonekoFile_,
  testMarkdownFile_
) where

import GHC.IO.Handle (hDuplicate, hDuplicateTo)

import Control.Exception (bracket)
import Control.Monad (unless, when)
import Data.Foldable (foldl', traverse_)
import Data.Monoid((<>))
import Data.Text.Lazy (Text)
import Prelude hiding (exp, fail)
import System.Console.CmdArgs.Verbosity (Verbosity(..), getVerbosity)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (exitFailure)
import System.FilePath (takeExtension)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified System.IO as IO
import qualified System.IO.Silently as S

import qualified Koneko.Data as D
import qualified Koneko.Eval as E
import qualified Koneko.Misc as M
import qualified Koneko.Repl as RE

data Example = Example {
  inputLine   :: Text,
  outputLines :: [Text]
} deriving Show

type ExampleGroup = [Example]
type Examples     = [ExampleGroup]

doctest :: Verbosity -> [FilePath] -> IO Bool
doctest v fs = _testFail <$> testFiles v fs

doctest' :: [FilePath] -> IO ()
doctest' fs
  = getVerbosity >>= flip doctest fs >>= flip unless exitFailure

testFiles :: Verbosity -> [FilePath] -> IO (Int, Int, Int)
testFiles verb files = do
    r@(t,o,f) <- s <$> traverse process files
    when (verb /= Quiet) $ do
      putStrLn "=== Summary ==="
      putStrLn $ "Files: " ++ show (length files) ++ "."
      printSummary t o f
    return r
  where
    process fname = do
      let (what, func)  = typAndFunc fname
          info          = fname ++ " (" ++ what ++ ")"
      when (verb /= Quiet) $
        putStrLn $ "=== Testing " ++ info ++ " ==="
      func verb fname <* when (verb /= Quiet) (putStrLn "")
    typAndFunc fname  = if takeExtension fname == ".md"
                        then ("markdown", testMarkdownFile)
                        else ("koneko", testKonekoFile)
    s = foldl' (\(t,o,f) (t',o',f') -> (t+t',o+o',f+f')) (0,0,0)

testKoneko, testMarkdown :: Verbosity -> [Text] -> IO (Int, Int, Int)
testKoneko    v = testExamples v . parseKoneko
testMarkdown  v = testExamples v . parseMarkdown

testKoneko_, testMarkdown_ :: Verbosity -> [Text] -> IO Bool
testKoneko_   = _test parseKoneko
testMarkdown_ = _test parseMarkdown

_test :: ([Text] -> Examples) -> Verbosity -> [Text] -> IO Bool
_test f v ls = _testFail <$> testExamples v (f ls)

_testFail :: (Int, Int, Int) -> Bool
_testFail (_, _, fail) = fail == 0

testKonekoFile, testMarkdownFile
  :: Verbosity -> FilePath -> IO (Int, Int, Int)
testKonekoFile    = _testFile testKoneko
testMarkdownFile  = _testFile testMarkdown

testKonekoFile_, testMarkdownFile_ :: Verbosity -> FilePath -> IO Bool
testKonekoFile_   = _testFile testKoneko_
testMarkdownFile_ = _testFile testMarkdown_

_testFile :: (Verbosity -> [Text] -> IO a)
          -> Verbosity -> FilePath -> IO a
_testFile f v fname = T.readFile fname >>= f v . T.lines

-- parsing --

parseKoneko, parseMarkdown :: [Text] -> Examples
parseKoneko   = examples . knkCommentBlocks []
parseMarkdown = examples . mdCodeBlocks []

examples :: [[Text]] -> Examples
examples = filter (not . null) . map (exampleGroup [])

-- TODO
exampleGroup :: ExampleGroup -> [Text] -> ExampleGroup
exampleGroup es ls
    = if null ls || null ls' then reverse es
      else exampleGroup (Example{..}:es) ls''
  where
    ls'             = dropWhile (not . isPrompt') ls
    (e, ls'')       = span isSameExample $ tail ls'           -- safe!
    inputLine       = T.drop (T.length prefix + T.length RE.promptText)
                    $ head ls'                                -- safe!
    outputLines     = e
    isSameExample s = maybe False (\x -> not $ isPrompt x || T.null x)
                    $ T.stripPrefix prefix s
    prefix          = T.takeWhile _isSp $ head ls'            -- safe!
    isPrompt'       = isPrompt . T.dropWhile _isSp
    isPrompt        = T.isPrefixOf RE.promptText

-- TODO
knkCommentBlocks :: [[Text]] -> [Text] -> [[Text]]
knkCommentBlocks bs ls
    = if null ls || null ls' then reverse bs
      else knkCommentBlocks (b':bs) ls''
  where
    ls'             = dropWhile (not . isComment) ls
    (b, ls'')       = span isSameComment ls'
    b'              = map (T.drop $ T.length prefix) b
    isComment       = T.isPrefixOf ";" . T.dropWhile _isSp
    isSameComment   = T.isPrefixOf prefix
    prefix          = T.takeWhile _isSp (head ls') <> ";"     -- safe!

-- TODO
mdCodeBlocks :: [[Text]] -> [Text] -> [[Text]]
mdCodeBlocks bs [] = reverse bs
mdCodeBlocks bs ls = mdCodeBlocks (b:bs) $ drop 1 ls''
  where
    ls'             = dropWhile (/= mdCodeStart) ls
    (b, ls'')       = break (== mdCodeEnd) $ drop 1 ls'

mdCodeStart, mdCodeEnd :: Text
mdCodeStart = "```koneko"
mdCodeEnd   = "```"

_isSp :: Char -> Bool
_isSp = M.isSpaceOrComma

-- internal --

-- TODO
testExamples :: Verbosity -> Examples -> IO (Int, Int, Int)
testExamples verb ex = do
    r@(total, ok, fail) <- go 0 0 0 ex
    when (verb == Loud) $ do
      putStrLn "=== Summary ==="
      printSummary total ok fail
    return r
  where
    go total ok fail []     = return (total, ok, fail)
    go total ok fail (g:gt) = do
      (t, o, f) <- testExampleGroup verb g
      go (total+t) (ok+o) (fail+f) gt

-- TODO
testExampleGroup :: Verbosity -> ExampleGroup -> IO (Int, Int, Int)
testExampleGroup verb g = do
    ctx <- E.initContextWithPrelude
    let st = D.emptyStack; total = length g
    (ok, fail, _) <- loop 0 0 g ctx st
    when (verb == Loud) $ do printTTPF total ok fail; putStrLn ""
    return (total, ok, fail)
  where
    loop :: Int -> Int -> ExampleGroup
         -> D.Context -> D.Stack -> IO (Int, Int, D.Stack)
    loop ok fail [] _ s = return (ok, fail, s)
    loop ok fail (e@Example{..}:et) c s = do
      (out, err, s') <- provide (T.unpack inputLine)
                      $ capture $ repl c s
      let olines = asLines out; elines = asLines err
      if compareOutput outputLines olines elines then do
        when (verb == Loud) $ printSucc e
        loop (ok+1) fail et c s'
      else do
        printFail e olines elines
        return (ok, fail+1, s')
    repl = RE.repl' True ""
    asLines x = let l = T.lines $ T.pack x in
                if not (null l) && last l == "" then init l else l

-- TODO: stderr, "...", ...
compareOutput :: [Text] -> [Text] -> [Text] -> Bool
compareOutput exp got err = exp' == got && null err
  where
    exp' = [ if l == "<BLANKLINE>" then "" else l | l <- exp ]

printSummary :: Int -> Int -> Int -> IO ()
printSummary total ok fail = do
  printTTPF total ok fail
  putStrLn $ "Test " ++ if fail == 0 then "passed." else "failed."

printTTPF :: Int -> Int -> Int -> IO ()
printTTPF total ok fail =
  putStrLn $  "Total: "   ++ (show total) ++
            ", Tried: "   ++ (show $ ok + fail) ++
            ", Passed: "  ++ (show ok) ++
            ", Failed: "  ++ (show fail) ++ "."

-- TODO
printSucc :: Example -> IO ()
printSucc Example{..} = do
    p "Trying:"   ; p $ indent inputLine
    p "Expecting:"; traverse_ (p . indent) outputLines
    p "ok"
  where
    p = T.putStrLn

-- TODO
printFail :: Example -> [Text] -> [Text] -> IO ()
printFail Example{..} out err = do
    p "Failed example:" ; p $ indent inputLine
    p "Expected:"       ; traverse_ (p . indent) outputLines
    p "Got:"            ; traverse_ (p . indent) out
    unless (null err) $ do
      p "Errors:"       ; traverse_ (p . indent) err
  where
    p = T.hPutStrLn IO.stderr

indent :: Text -> Text
indent = ("    " <>)

-- stdio --

capture :: IO a -> IO (String, String, a)
capture act = do
  (err, (out, x)) <- S.hCapture [IO.stderr] $ S.capture act
  return (out, err, x)

provide :: String -> IO a -> IO a
provide = hProvide IO.stdin

hProvide :: IO.Handle -> String -> IO a -> IO a
hProvide h str act = withTempFile "provide" f
  where
    f hTmp = do
      IO.hPutStr hTmp str; IO.hFlush hTmp
      IO.hSeek hTmp IO.AbsoluteSeek 0
      withRedirect h hTmp act

withRedirect :: IO.Handle -> IO.Handle -> IO a -> IO a
withRedirect hOrig hRepl act = do
    buf <- IO.hGetBuffering hOrig
    bracket redirect (restore buf) $ const act
  where
    redirect = do
      hDup <- hDuplicate hOrig
      hDuplicateTo hRepl hOrig
      return hDup
    restore buf hDup = do
      hDuplicateTo hDup hOrig
      IO.hSetBuffering hOrig buf
      IO.hClose hDup

withTempFile :: String -> (IO.Handle -> IO a) -> IO a
withTempFile template f = do
    tmpDir <- getTemporaryDirectory
    bracket (IO.openTempFile tmpDir template) cleanup (f . snd)
  where
    cleanup (tmpFile, h) = IO.hClose h >> removeFile tmpFile  -- !!!

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

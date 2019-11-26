--  --                                                          ; {{{1
--
--  File        : Koneko/Test.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2019-11-25
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
import Data.Char (isSpace)
import Data.Foldable (foldl', traverse_)
import Data.Monoid((<>))
import Data.Text.Lazy (Text)
import Prelude hiding (exp, fail)
import System.Console.CmdArgs.Verbosity (Verbosity(..), getVerbosity)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (exitFailure)
import System.FilePath (takeExtension)
import System.IO (Handle)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified System.IO as IO
import qualified System.IO.Silently as S

import Koneko.Data (Context, Stack, emptyStack)
import Koneko.Eval (initContext)

import qualified Koneko.Repl as RE

data Example = Example {
  fileName    :: FilePath,
  lineNo      :: Int,
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
    process fp = do
      let (what, func)  = typAndFunc fp
          info          = fp ++ " (" ++ what ++ ")"
      when (verb /= Quiet) $
        putStrLn $ "=== Testing " ++ info ++ " ==="
      func verb fp <* when (verb /= Quiet) (putStrLn "")
    typAndFunc fp = if takeExtension fp == ".md"
                    then ("markdown", testMarkdownFile)
                    else ("koneko"  , testKonekoFile  )
    s = foldl' (\(t,o,f) (t',o',f') -> (t+t',o+o',f+f')) (0,0,0)

testKoneko, testMarkdown
  :: Verbosity -> FilePath -> [Text] -> IO (Int, Int, Int)
testKoneko    v fp = testExamples v . parseKoneko   fp
testMarkdown  v fp = testExamples v . parseMarkdown fp

testKoneko_, testMarkdown_
  :: Verbosity -> FilePath -> [Text] -> IO Bool
testKoneko_   = _test parseKoneko
testMarkdown_ = _test parseMarkdown

_test :: (FilePath -> [Text] -> Examples)
      -> Verbosity -> FilePath -> [Text] -> IO Bool
_test f v fp ls = _testFail <$> testExamples v (f fp ls)

_testFail :: (Int, Int, Int) -> Bool
_testFail (_, _, fail) = fail == 0

testKonekoFile, testMarkdownFile
  :: Verbosity -> FilePath -> IO (Int, Int, Int)
testKonekoFile    = _testFile testKoneko
testMarkdownFile  = _testFile testMarkdown

testKonekoFile_, testMarkdownFile_ :: Verbosity -> FilePath -> IO Bool
testKonekoFile_   = _testFile testKoneko_
testMarkdownFile_ = _testFile testMarkdown_

_testFile :: (Verbosity -> FilePath -> [Text] -> IO a)
          -> Verbosity -> FilePath -> IO a
_testFile f v fp = T.readFile fp >>= f v fp . T.lines

-- parsing --

parseKoneko, parseMarkdown :: FilePath -> [Text] -> Examples
parseKoneko   fp = examples fp . knkCommentBlocks [] . zip [1..]
parseMarkdown fp = examples fp . mdCodeBlocks     [] . zip [1..]

examples :: FilePath -> [[(Int, Text)]] -> Examples
examples fp = filter (not . null) . map (exampleGroup fp [])

-- TODO
exampleGroup :: FilePath -> ExampleGroup -> [(Int, Text)] -> ExampleGroup
exampleGroup fileName es ls
    = if null ls || null ls' then reverse es
      else exampleGroup fileName (Example{..}:es) ls''
  where
    ls'             = dropWhile (not . isPrompt' . snd) ls
    (e, ls'')       = span (isSameExample . snd) $ tail ls'   -- safe!
    e'              = map (dropPrefix . snd) e
    (c,outputLines) = span isCont e'
    (lineNo, fl)    = head ls'                                -- safe!
    inputLine       = T.concat $ map dropPrompt (dropPrefix fl:c)
    isSameExample s = maybe False (\x -> not $ isPrompt x || T.null x)
                    $ T.stripPrefix prefix s
    dropPrefix      = T.drop $ T.length prefix
    dropPrompt      = T.drop $ T.length RE.promptText
    prefix          = T.takeWhile isSpace fl
    isPrompt'       = isPrompt . T.dropWhile isSpace
    isPrompt        = T.isPrefixOf RE.promptText
    isCont          = T.isPrefixOf "... "                     --  TODO

-- TODO
knkCommentBlocks :: [[(Int, Text)]] -> [(Int, Text)] -> [[(Int, Text)]]
knkCommentBlocks bs ls
    = if null ls || null ls' then reverse bs
      else knkCommentBlocks (b':bs) ls''
  where
    ls'             = dropWhile (not . isComment) ls
    (b, ls'')       = span isSameComment ls'
    b'              = [ (n,T.drop (T.length prefix) x) | (n,x) <- b ]
    isComment       = T.isPrefixOf ";" . T.dropWhile isSpace . snd
    isSameComment   = T.isPrefixOf prefix . snd
    prefix          = T.takeWhile isSpace (snd $ head ls') <> ";" -- safe!

-- TODO
mdCodeBlocks :: [[(Int, Text)]] -> [(Int, Text)] -> [[(Int, Text)]]
mdCodeBlocks bs [] = reverse bs
mdCodeBlocks bs ls = mdCodeBlocks (b:bs) $ drop 1 ls''
  where
    ls'             = dropWhile ((/= mdCodeStart) . snd) ls
    (b, ls'')       = break ((== mdCodeEnd) . snd) $ drop 1 ls'

mdCodeStart, mdCodeEnd :: Text
mdCodeStart = "```koneko"
mdCodeEnd   = "```"

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
    ctx <- initContext
    let st = emptyStack; total = length g
    (ok, fail, _) <- loop 0 0 g ctx st
    when (verb == Loud) $ do printTTPF total ok fail; putStrLn ""
    return (total, ok, fail)
  where
    loop :: Int -> Int -> ExampleGroup
         -> Context -> Stack -> IO (Int, Int, Stack)
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

-- TODO: "...", ...
compareOutput :: [Text] -> [Text] -> [Text] -> Bool
compareOutput exp got err
    = if null err then exp' == got else null got &&
      T.isPrefixOf RE.errorText (head err) && exp' == err     -- safe!
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
    p $ T.pack $ "File " ++ fileName ++ ", line " ++ show lineNo
    p "Failed example:" ; p $ indent inputLine
    p "Expected:"       ; traverse_ (p . indent) outputLines
    p "Got:"            ; traverse_ (p . indent) out
    unless (null err) $ do
      p "Errors:"       ; traverse_ (p . indent) err
  where
    p = T.hPutStrLn IO.stderr

indent :: Text -> Text
indent = ("  " <>)

-- stdio --

capture :: IO a -> IO (String, String, a)
capture act = do
  (err, (out, x)) <- S.hCapture [IO.stderr] $ S.capture act
  return (out, err, x)

provide :: String -> IO a -> IO a
provide = hProvide IO.stdin

hProvide :: Handle -> String -> IO a -> IO a
hProvide h str act = withTempFile "provide" f
  where
    f hTmp = do
      IO.hPutStr hTmp str; IO.hFlush hTmp
      IO.hSeek hTmp IO.AbsoluteSeek 0
      withRedirect h hTmp act

withRedirect :: Handle -> Handle -> IO a -> IO a
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

withTempFile :: String -> (Handle -> IO a) -> IO a
withTempFile template f = do
    tmpDir <- getTemporaryDirectory
    bracket (IO.openTempFile tmpDir template) cleanup (f . snd)
  where
    cleanup (tmpFile, h) = IO.hClose h >> removeFile tmpFile  -- !!!

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :

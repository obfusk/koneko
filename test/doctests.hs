import System.Environment (getArgs)
import Test.DocTest (doctest)

main :: IO ()
main = do args <- getArgs; doctest $ ["-isrc", "src/"] ++ args

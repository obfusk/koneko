#!/usr/bin/runhaskell
main = mapM_ print $ filter (\i -> i `mod` 3 == 0) $ map (\i -> i*i) [1..10000]

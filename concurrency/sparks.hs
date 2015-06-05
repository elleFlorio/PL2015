-- to compile it:
-- ghc -threaded par.hs
-- to run it:
-- ./par +RTS -N

import Control.Parallel
import System.TimeIt

-- non parallel
fib :: Int -> Int
fib 0 = 0 
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- simple (bad) parallelism
nfib0 :: Int -> Int
nfib0 0 = 0
nfib0 1 = 1
nfib0 n = par n1 (n1 + n2)
          where n1 = nfib0 (n-1)
                n2 = nfib0 (n-2)

--  parallel with "threshold"
nfib1 :: Int -> Int
nfib1 n | n <= 12 = fib n
nfib1 n = par n1 (n1 + n2)
          where n1 = nfib1 (n-1)
                n2 = nfib1 (n-2)

--  parallel with "threshold" switched
nfib1' :: Int -> Int
nfib1' n | n <= 12 = fib n
nfib1' n = par n1 (n2 + n1)
          where n1 = nfib1' (n-1)
                n2 = nfib1' (n-2)

--  parallel with "threshold" and pseq
nfib :: Int -> Int
nfib n | n <= 12 = fib n
nfib n = par n1 (pseq n2 (n1 + n2))
         where n1 = nfib (n-1)
               n2 = nfib (n-2)

main = do 
    putStrLn ("Executing non parallel")
    timeIt $ print $ fib 40 
    putStrLn ("Executing simple (bad) parallelism")
    timeIt $ print $ nfib0 40 
    putStrLn ("parallel with 'threshold'") 
    timeIt $ print $ nfib1 40 
    putStrLn ("parallel with 'threshold', switched") 
    timeIt $ print $ nfib1' 40 
    putStrLn ("parallel with 'threshold' and pseq") 
    timeIt $ print $ nfib 40 








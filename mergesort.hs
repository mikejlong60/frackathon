-- The idea here is that we split up this algorithm between a client and a server.
-- Here is the algorithm(stolen from): https://stackoverflow.com/questions/37082925/haskell-merge-sort
-- 1. The server invokes `halve` on some list of any kind resulting in two lists
-- 2. It sends an HTTP post to each client with a half asynchronously.
-- 3. When all responses come back the server calls `merge` which sorts the final list.


-- To run this program as a standalone inside the Haskell repl.:
-- 1. cd to the directory containing this file.
-- 2. ghci
-- 3. :l mergesort.hs
-- 4. msort [2,3,4,5,6,7,8,5,3,2,1,2,34,4]
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys         = ys
merge xs []         = xs
merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

halve :: [a] -> ([a],[a])
halve xs = (take lhx xs, drop lhx xs)
           where lhx = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [x] = [x]
msort  xs = merge (msort left) (msort right)
            where (left,right) = halve xs
-----------------------------------------------------------------------------
--
--    Project Euler 0014
--   
--    The following iterative sequence is defined for the set of positive integers:
--
--    n -> n/2 (n is even)
--    n -> 3n + 1 (n is odd)
--
--    Using the rule above and starting with 13, we generate the following sequence:
--    13, 40, 20, 10, 5, 16, 8, 4, 2, 1
--
--    It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. 
--    Although it has not been proved yet (Collatz Problem), it is thought that all starting 
--    numbers finish at 1.
--
--    Which starting number, under one million, produces the longest chain?
--
--    NOTE: Once the chain starts the terms are allowed to go above one million.
--
--
-----------------------------------------------------------------------------


module Problem0014 (
    run
) where



run :: IO Int
run = return $ longestChain 999999


longestChain :: Int -> Int
longestChain mx = (snd . maximum . map g) [2..mx]
        where g x = (collatzCount x, x)

        
collatzCount :: Int -> Int
collatzCount n = collatzCount' n 1
        -- the helper is just to enforce tail recursion, while hiding the accumulator
        where collatzCount' 1 acc = acc
              collatzCount' n acc = collatzCount' (nextCollatz n) $! (acc+1)
                    

nextCollatz :: Int -> Int
nextCollatz n = if n `mod` 2 == 0 
                    then n `div` 2 
                    else 3 * n + 1 


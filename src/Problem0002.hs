-----------------------------------------------------------------------------
--
--    Project Euler 0002
--   
-- |  Each new term in the Fibonacci sequence is generated by adding the previous two terms. 
--    By starting with 1 and 2, 
--    the first 10 terms will be:
--
--    1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
--
--    By considering the terms in the Fibonacci sequence whose values do not exceed four 
--    million, find the sum of the even-valued terms.
-----------------------------------------------------------------------------

module Problem0002 (run) where

        
run :: IO Int
run = return $ prob2 4000000 


prob2 limit = sumevens fibs 0
    where sumevens (_:x:_:xs) acc  
            | x > limit = acc
            | otherwise = sumevens xs (acc + x)
          fibs = [ fib i | i <- [1..] ]
          fib 1 = 1
          fib 2 = 2
          fib n = fib(n-1) + fib(n-2)

    


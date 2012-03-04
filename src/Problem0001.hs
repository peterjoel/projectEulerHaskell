-----------------------------------------------------------------------------
--
--    Project Euler 0001
--     
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
-- The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.
--
-----------------------------------------------------------------------------

module Problem0001 (interactive, run) where


prob1 n = sum [ x | x <- [1..n], mod x 3 == 0 || mod x 5 == 0]

run :: IO Int
run = return $ prob1 999

interactive = do
        putStrLn "Enter a number"
        input <- getLine
        putStrLn $ outMessage input (show $ prob1 (toInteger $ read input))


outMessage input result = "The sum of all multiples of 3 and 5 below " 
                ++ (show input) ++ " is " ++ (show result)




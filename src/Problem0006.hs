-----------------------------------------------------------------------------
--
--    Project Euler 0006
--   
--    The sum of the squares of the first ten natural numbers is,
--    12 + 22 + ... + 102 = 385
--
--    The square of the sum of the first ten natural numbers is,
--    (1 + 2 + ... + 10)2 = 552 = 3025
--
--    Hence the difference between the sum of the squares of the first ten 
--    natural numbers and the square of the sum is 3025 ‚àí 385 = 2640.
--
--    Find the difference between the sum of the squares of the first one 
--    hundred natural numbers and the square of the sum.
-----------------------------------------------------------------------------

module Problem0006 (
    run
) where


run :: IO Int
run = return $ diffSsSs [1..100]


diffSsSs xs = sum xs ^ 2 - (sum . map (^2)) xs

 
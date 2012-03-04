-----------------------------------------------------------------------------
--
--  Project Euler 24
--
--    A permutation is an ordered arrangement of objects. For example, 3
--      124 is one possible permutation of the digits 1, 2, 3 and 4. If all 
--      of the permutations are listed numerically or alphabetically, we call 
--      it lexicographic order. The lexicographic permutations of 0, 1 and 2 
--      are:
--
--    012   021   102   120   201   210
--
--    What is the millionth lexicographic permutation of the digits 0, 1, 2, 
--      3, 4, 5, 6, 7, 8 and 9?
--
--
-----------------------------------------------------------------------------

module Problem0024 (
    run
) where


import Data.List


run :: IO String
run = return calc


calc = (permute ['0'..'9']) !! 999999


permute [] = []
permute [a] = [[a]]
permute as = concat $ map p (divsr as)
      where p (as,b:bs) = map (b:) (permute (as++bs))
                   

divsr [] = []
divsr xs = d [] xs
        where d bs (x:[]) = [(bs,[x])]
              d bs (x:xs) = (bs,x:xs) : d (bs++[x]) xs


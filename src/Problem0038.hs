    -----------------------------------------------------------------------------
--
--  Project Euler 38
--
--  Take the number 192 and multiply it by each of 1, 2, and 3:
--  
--  192 × 1 = 192
--  192 × 2 = 384
--  192 × 3 = 576
--  By concatenating each product we get the 1 to 9 pandigital, 192384576. 
--  We will call 192384576 the concatenated product of 192 and (1,2,3)
--  
--  The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 
--  4, and 5, giving the pandigital, 918273645, which is the concatenated 
--  product of 9 and (1,2,3,4,5).
--  
--  What is the largest 1 to 9 pandigital 9-digit number that can be formed 
--  as the concatenated product of an integer with (1,2, ... , n) where n > 1?
--  
-----------------------------------------------------------------------------

module Problem0038 (
    run
) where

import Num.Digits
import Control.Applicative
import Data.List
import Data.Maybe

run :: IO Int
run = return . fromDigitsD . head . filter isPandigital $ candidates

isPandigital :: [Int] -> Bool
isPandigital ds = sort ds == [1..9]

candidates :: [[Int]]
candidates = do 
                    s <- [9999,9998..1]
                    return . filter isJust . nineDigits [] 0 $ (*s) <$> [1..9] 
                    where 
                        nineDigits _ _ [] = Nothing
                        nineDigits c l (n:ns)| newLen == 9 = Just newDigits
                                             | newLen > 9  = Nothing
                                             | otherwise   = nineDigits newDigits newLen ns
                                             where newLen    = l + length digits
                                                   newDigits = c ++ digits
                                                   digits    = toDigitsD n





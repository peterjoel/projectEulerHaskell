-----------------------------------------------------------------------------
--
--  Project Euler 33
--
--
--    The fraction 49/98 is a curious fraction, as an inexperienced 
--    mathematician in attempting to simplify it may incorrectly believe 
--    that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
--
--    We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
--
--    There are exactly four non-trivial examples of this type of fraction, 
--    less than one in value, and containing two digits in the numerator and 
--    denominator.
--
--    If the product of these four fractions is given in its lowest common 
--    terms, find the value of the denominator.
--
--
-----------------------------------------------------------------------------

{- This is ugly ocde. But I don't like this problem much so I'm not 
    going to improve on it just yet.
-}
module Problem0033 (
    run
) where

import Data.Char
import Data.List( (\\))
import Data.Ratio
import Data.Function

run :: IO Int
run = return $ denominator $ foldl1 (*) curiousList

curiousList = map (uncurry (%)) $ filter isCurious [ (n,d) | n <- [11..99], d <-[11..99], n < d]


isCurious (n,d) = if anyInCommon
                    then n % d == reduce
                    else False
    where 
          n'                = digits n
          d'                = digits d
          reduce            = (head (n' \\ inCommon)) % (head (d' \\ inCommon))
          inCommon          = filter (\x -> x /=0 && x `elem` n') d'
          anyInCommon       = length inCommon == 1 && 0 `notElem` d'
          digits            = map digitToInt . show
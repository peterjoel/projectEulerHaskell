-----------------------------------------------------------------------------
--
--  Project Euler 36
--
--
--    The decimal number, 585 = 1001001001 (binary), is palindromic in 
--    both bases.
--
--    Find the sum of all numbers, less than one million, which are 
--    palindromic in base 10 and base 2.
--
--    (Please note that the palindromic number, in either base, may not 
--    include leading zeros.)
--
--
-----------------------------------------------------------------------------
--
-- I wanted to use pointfree, applicative style for the main part of the
-- calculation, as an exercise. Got there in the end..
--
-----------------------------------------------------------------------------
module Problem0036 (
    run
) where

import Num.Digits
import Control.Applicative

run :: IO Integer
run = return $ calc 999999

calc :: Integer -> Integer
calc n = (sum . doublyPalindromics) [1..(ceiling $ sqrt $ fromIntegral n)]

doublyPalindromics :: [Integer] -> [Integer]
doublyPalindromics = filter isBinaryPalindrome . decimalPalindromes

isBinaryPalindrome :: Integer -> Bool
isBinaryPalindrome = (==) <$> (fromDigitsB . reverse . digitsB) <*> id

decimalPalindromes :: [Integer] -> [Integer]
decimalPalindromes = map fromDigitsD . oddsAndEvens . map digitsD
        where oddsAndEvens  = (++) <$> (map oddDigits) <*> (map evenDigits)
              evenDigits    = (++) <$> id              <*> reverse
              oddDigits     = (++) <$> reverse . tail  <*> id
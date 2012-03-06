-----------------------------------------------------------------------------
--
-- Module      :  Num.Primes
-- Copyright   :  
-- License     :  AllRightsReserved
--
-- Maintainer  :  
-- Stability   :  
-- Portability :  
--
-- | Some utilities to do with primes and prime factorization
--
-----------------------------------------------------------------------------

module Num.Primes (
              isPrime
             ,prime 
             ,allPrimes
             ,primesSA
             ,factorize
             ,divisors
             ,properDivisors
)
where

import Data.Array.Unboxed
import Data.Maybe
import Data.List
import Control.Applicative


-- I didn't write this. It's quite fast 
primesSA = 2 : prs
  where 
    prs = 3 : sieve prs 3 []
    sieve (p:ps) x fs = [i*2 + x | (i,e) <- assocs a, e] 
                        ++ sieve ps (p*p) fs'
     where
      q     = (p*p-x)`div`2                  
      fs'   = (p,0) : [(s, rem (y-q) s) | (s,y) <- fs]
      a     :: UArray Int Bool
      a     = accumArray (\ b c -> False) True (1,q-1)
                         [(i,()) | (s,y) <- fs, i <- [y+s, y+s+s..q]]
                         

isPrime :: (Integral n) => n -> Bool
isPrime n = isPrime' (2:[3,5..h]) n
            where h = (ceiling . sqrt . fromIntegral) n

isPrime' filter n
    | n == 0 = False
    | n == 1 = False
    | n < 0 = isPrime (-n)
    | n < 4 = True
    | n `mod` 2 == 0 = False
    | n `mod` 3 == 0 = False
    | any ((==0) . mod n) filter = False
    | otherwise = True
   

allPrimes :: [Int]
allPrimes = primesSA

prime :: Int -> Int
prime n = allPrimes !! (n-1)


-- | produce a list of tuples. The first item in the tuple is the 
--   factor and the second is the power
factorize :: Int -> [(Int,Int)]
factorize n = reduce n primesSA 0 []
    where reduce n (p:ps) pwr factors
            | n `mod` p == 0    = reduce (div n p) (p:ps) (pwr+1) factors
            | pwr > 0           = (p, pwr) : reduce n ps 0 factors
            | p <= n            = reduce n ps 0 factors
            | otherwise         = factors



-- divisors include the original number. This doesn't
properDivisors n = (divisors n) \\ [n]


divisors n = foldl' combine [1] primePowers
             where primePowers    = map pwrs $ factorize n
                   pwrs (n, pwr)  = [ n^p | p <- [0..pwr] ]
                   combine a b    = (*) <$> b <*> a


    
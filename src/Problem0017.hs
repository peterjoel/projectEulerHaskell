-----------------------------------------------------------------------------
--
--    Project Euler 0017
--   
--    If the numbers 1 to 5 are written out in words: one, two, three, 
--      four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
--
--    If all the numbers from 1 to 1000 (one thousand) inclusive were written 
--      out in words, how many letters would be used?
--
--    NOTE: Do not count spaces or hyphens. For example, 342 (three hundred 
--      and forty-two) contains 23 letters and 115 (one hundred and fifteen) 
--      contains 20 letters. The use of "and" when writing out numbers is in 
--      compliance with British usage.
--
--
-----------------------------------------------------------------------------
--
--    This isn't really nice code, but it isn't an interesting problem either
--
-----------------------------------------------------------------------------

module Problem0017 (
    run
) where


run :: IO Int
run = return $ sumAll 1000

sumAll n = foldl1 (+) $ map thouLen [1..n]


thouLen 1000 = length "onethousand"
thouLen n    = hunLen n

hunLen 0 = 0
hunLen n
    | n < 100 = tenLen n
    | otherwise = hundreds + tenLen r + joiner
    where 
          hundreds = unitLen d + length "hundred"
          joiner = if r == 0 then 0 else length "and"
          (d,r) = divMod n 100

tenLen n 
    | n < 10 = unitLen n
    | n < 20 = teenLen n
    | otherwise = length wordDigit + unitLen r
    where wordDigit = (words "twenty thirty forty fifty sixty seventy eighty ninety") 
                          !! (d-2)
          (d,r) = divMod n 10

teenLen n = length $ 
    (words "ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen") 
        !! (n-10)

unitLen 0 = 0
unitLen n = length $ 
    (words "one two three four five six seven eight nine") !! (n-1)







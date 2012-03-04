-----------------------------------------------------------------------------
--
--    Project Euler 0019
--   
--    You are given the following information, but you may prefer to do 
--      some research for yourself.
--
--        1 Jan 1900 was a Monday.
--        Thirty days has September,
--        April, June and November.
--        All the rest have thirty-one,
--        Saving February alone,
--        Which has twenty-eight, rain or shine.
--        And on leap years, twenty-nine.
--        A leap year occurs on any year evenly divisible by 4, but not 
--      on a century unless it is divisible by 400.
--
--    How many Sundays fell on the first of the month during the twentieth 
--      century (1 Jan 1901 to 31 Dec 2000)?
--
--
-----------------------------------------------------------------------------

module Problem0019 (
    run
) where

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate


run :: IO Int
run = return $ countSundays 1901 2000




countSundays :: Int -> Int -> Int
countSundays fromYr toYr = count isSunday $ yrRange monthStarts

        where yrRange         = to . from
              -- offset from 1/1/1900 because that's the data I have been given
              from            = drop $ (fromYr - 1900) * 12
              to              = take $ (toYr - fromYr + 1) * 12
              
              isSunday day    = day `mod` 7 == 0
              months          = concat $ map monthLengths [1900..]
              jan1st1900      = 1 -- Monday
              -- first day of each month
              monthStarts     = scanl (+) jan1st1900 months
              
              
monthLengths yr = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    where feb | yr `mod` 400 == 0 = 29
              | yr `mod` 100 == 0 = 28
              | yr `mod` 4 == 0   = 29
              | otherwise         = 28 


count p = length . filter p




-- run = return $ countSundaysWithCal
-- alternative solution with built-in calendar functions
countSundaysWithCal = count isSunday [ fromGregorian y m 1 | y <- [1901..2000], m <- [1..12] ]
        where isSunday d = let (_,_,n) = toWeekDate d in n == 7


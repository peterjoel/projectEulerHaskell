-----------------------------------------------------------------------------
--
-- Project Euler 0022
--
--
--    Using names.txt (right click and 'Save Link/Target As...'), a 46K text
--    file containing over five-thousand first names, begin by sorting it 
--    into alphabetical order. Then working out the alphabetical value for 
--    each name, multiply this value by its alphabetical position in the list 
--    to obtain a name score.
--
--    For example, when the list is sorted into alphabetical order, COLIN, 
--    which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the 
--    list. So, COLIN would obtain a score of 938 × 53 = 49714.
--
--    What is the total of all the name scores in the file?
--
--
-----------------------------------------------------------------------------

module Problem0022 (
    run
) where


import Paths_projectEuler

import Control.Applicative
import Data.List
import Data.Function (on)

run :: IO Int
run = calc <$> input


calc :: [String] -> Int 
calc names = sum $ map (\(n,i)-> score n * i ) $ zip (sort names) [1..]
        where score word = (sum . map lscore) word
              lscore c = fromEnum c - 64
 
input :: IO [String]
input = do file <- readFile =<< getDataFileName "problem0022.txt"
           let stripEmpty = filter (/="")
               stripPunc  = map (filter (`notElem` "\","))
               onCommas   = groupBy ((==) `on` (==','))
           return $ (stripEmpty . stripPunc . onCommas ) file

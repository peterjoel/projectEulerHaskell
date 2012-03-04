-----------------------------------------------------------------------------
--
--    Project Euler 0011
--   
--
--    In the 20×20 grid below, four numbers along a diagonal line have been marked in red.
--
--    08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
--    49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
--    81 49 31 73 55 79 14 ... (data/problem0011.txt)
--
--    The product of these numbers is 26 × 63 × 78 × 14 = 1788696.
--
--    What is the greatest product of four adjacent numbers in any direction (up, down, left, right, or diagonally) in the 20×20 grid?
-----------------------------------------------------------------------------

module Problem0011 (
    run
) where

import Paths_projectEuler (getDataFileName)

run :: IO Int
run = do d <- input
         return $ maximum (map mmaximum [convolveprod d p| p <- patterns])


input :: IO [[Int]]
input = do file <- readFile =<< getDataFileName "problem0011.txt"
           return $ (map (map read)) $ map words $ lines file

patterns = [ 
        [[1,1,1,1]],
        [[1],[1],[1],[1]],
        [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]],
        [[0,0,0,1],[0,0,1,0],[0,1,0,0],[1,0,0,0]]
        ]



mslice :: [[a]] -> Int -> Int -> Int -> Int -> [[a]]
mslice a x y w h = map (\a -> slice a x w) ( slice a y h )


slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice a start len = map (a!!) [ start.. start + len -1]		
	

convolveprod :: (Num n) => [[n]] -> [[n]] -> [[n]]
convolveprod d m = 
	[ calcrow j | j<- yrange ]
	where
		xrange = [0.. (mwidth d - mwidth m) ]
		yrange = [0.. (mheight d - mheight m) ]
		calcrow j = map ( mmult . mprod m) [ mslice d i j (mwidth m) (mheight m) | i<- xrange ]
		mmult m = foldr( (*) . foldr (\a b -> if a==0 then b else a * b) 1 ) 1 m

mheight :: [[a]] -> Int	
mheight [[]] = 0
mheight m = length m
mwidth :: [[a]] -> Int
mwidth [[]] = 0
mwidth m = length ( m !! 0 )

mmaximum :: (Num a, Ord a) => [[a]] -> a
mmaximum m = maximum ( map maximum m )
	
msum :: (Num n) => [[n]] -> n
msum m = sum $ map sum m

mprod :: (Num n) => [[n]] -> [[n]] -> [[n]]
mprod =  zipWith $ zipWith (*)




	
	



-----------------------------------------------------------------------------
--
--    Project Euler 0003
--   
--    The prime factors of 13195 are 5, 7, 13 and 29.
--
--    What is the largest prime factor of the number 600851475143 ?
--
-----------------------------------------------------------------------------

module Problem0003 ( run

) where


run :: IO Integer
run = return $ largestPrimeFactor 600851475143

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor a
	| a <= 3 = a
	| otherwise = 
		let (_,factors) = reduce a [] 2 in last factors
		where
			reduce a f n
				| a<3 || n<2 = (a,f)
				| fromIntegral n > sqrt(fromIntegral a) = (1,f++[a])
				| n == 2 = r 3
				| otherwise = r (n+2)
				where r = reduce a' f' 
					where (a',f') = reduceBy a f n


reduceBy a f n
	| n < 2 = (a,f)
	| (a `mod` n) == 0 = 
		reduceBy( floor( fromIntegral a / fromIntegral n)) appendlist n
	| otherwise = (a, appendlist)
	where appendlist
		| n `elem` f = f
		| otherwise = f++[n]




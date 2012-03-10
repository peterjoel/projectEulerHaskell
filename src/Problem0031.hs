-----------------------------------------------------------------------------
--
--  Project Euler 31
--
--
--    In England the currency is made up of pound, £, and pence, p, and there 
--    are eight coins in general circulation:
--
--        1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
--
--    It is possible to make £2 in the following way:
--
--        1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
--
--    How many different ways can £2 be made using any number of coins?
--
--
-----------------------------------------------------------------------------

module Problem0031 (
    run
) where


type Coin = Int

run :: IO Int
run = return $ length (ways 200 coins)


coins = [200,100,50,20,10,5,2,1]

ways :: Int -> [Coin] -> [[(Coin,Int)]]
ways amount [c] = [[(c, amount `div` c)]]
ways amount (c:coins) = concat $ map drill [0 .. amount `div` c]
    where drill n = map ((c,n):) (ways (amount-c*n) coins)


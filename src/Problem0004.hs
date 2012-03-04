-----------------------------------------------------------------------------
--
--    Project Euler 0004
--   
--    A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 √ó 99.
--
--    Find the largest palindrome made from the product of two 3-digit numbers.
--
-----------------------------------------------------------------------------

module Problem0004 ( run

) where


run :: IO Int
run = return $ maximum [ a*b | a<-[100..999], b<-[a..999], show(a*b) == reverse(show $ a*b)]

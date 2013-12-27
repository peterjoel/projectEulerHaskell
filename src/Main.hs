-----------------------------------------------------------------------------
--
-- Project Euler problems runner
--
--
--  All the problems have a `run` function that is in IO and accepts no 
--      arguments. That's so I can invoke them all the same way from here,
--      and just swap the import.
--
-----------------------------------------------------------------------------

module Main (
    main
) where


import Problem0037
import Data.Time (diffUTCTime, getCurrentTime)


main = do t0 <- getCurrentTime
          print =<< run
          t1 <- getCurrentTime
          putStrLn $ " in " ++ (show $ diffUTCTime t1 t0)


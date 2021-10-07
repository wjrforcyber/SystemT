-- | This is the entrypoint.

module Main where

import Lang.L1.Syntax
import Common.Types

main :: IO ()
-- main = print "EpsilonX"
-- main = exec "3 4 +"


main = do
  putStr "> \n"
--   a <- getLine
  exec "2+3"
  exec "2"
  exec "3-2"
  exec "2-3"
  exec "2*3"
  
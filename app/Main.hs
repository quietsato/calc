module Main (main) where

import           Lib (parser)

main :: IO ()
main = do
  print "input a equation"
  eq <- getLine
  print $ "input is " ++ eq
  print $ "the equation is " ++ (show . parser) eq

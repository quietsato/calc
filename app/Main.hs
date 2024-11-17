module Main (main) where


import           Lib

main :: IO ()
main = do
  print "input a expression"
  s <- getLine
  print $ "input is " ++ s
  let ex = expr s
  print $ "the equation is " ++ show ex
  print $ "evaluated to " ++ show (eval . fst <$> ex)

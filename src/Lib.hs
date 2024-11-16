module Lib (num, add, sub, mul, div_, parser) where

import           Data.List  (elemIndex)
import           Data.Maybe (listToMaybe, mapMaybe)
import           GHC.Base   (join)
import           Text.Read  (readMaybe)

data Factor =
  Num Int |
  Add Factor Factor |
  Sub Factor Factor |
  Mul Factor Factor |
  Div Factor Factor

instance Show Factor where
  show (Num a)   = "[" ++ show a ++ "]"
  show (Add a b) = "[" ++ show a ++ "+" ++ show b ++ "]"
  show (Sub a b) = "[" ++ show a ++ "-" ++ show b ++ "]"
  show (Mul a b) = "[" ++ show a ++ "*" ++ show b ++ "]"
  show (Div a b) = "[" ++ show a ++ "/" ++ show b ++ "]"

num :: String -> Maybe Factor
num s = Num <$> readMaybe s

add :: String -> Maybe Factor
add s = uncurry Add <$> op '+' s

sub :: String -> Maybe Factor
sub s = uncurry Sub <$> op '-' s

mul :: String -> Maybe Factor
mul s = uncurry Mul <$> op '*' s

div_ :: String -> Maybe Factor
div_ s = uncurry Div <$> op '/' s

op :: Char -> String -> Maybe (Factor, Factor)
op o s = liftA2 (,) x y
  where
    idx = (length s -) <$> elemIndex o (reverse s)
    removeOp i = [take (i - 1) s, drop i s]
    strWithoutOp = sequence . mapM removeOp $ idx
    factors = join (traverse (traverse parser) strWithoutOp)
    x = (!! 0) <$> factors
    y = (!! 1) <$> factors

parser :: String -> Maybe Factor
parser s = listToMaybe (mapMaybe ($ s) [add, sub, mul, div_, num])

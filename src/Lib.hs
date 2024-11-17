{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib (anyChar, eof, chr, is, nt, (~|~), (~~), rep, digit, number, factor, term, expr, eval)  where

import           Data.Char  (isDigit)
import           Data.Maybe (isJust)
import           GHC.Base   (divInt)

type ParseFunc a = String -> Maybe (a, String)


apply :: (a -> b) -> Maybe (a, String) -> Maybe (b, String)
apply f (Just (a, s)) = Just (f a, s)
apply _ _             = Nothing


anyChar :: ParseFunc Char
anyChar (c:s) = Just (c, s)
anyChar _     = Nothing

eof :: ParseFunc ()
eof "" = Just ((), "")
eof _  = Nothing

chr :: Char -> ParseFunc Char
chr c (h:r) = if c == h then Just (c, r) else Nothing
chr _ _     = Nothing

is :: (Char -> Bool) -> ParseFunc Char
is f (h:r) = if f h then Just (h, r) else Nothing
is _ _     = Nothing

nt :: ParseFunc a -> ParseFunc ()
nt p s = if (isJust . p) s then Nothing else Just ((), s)

or :: ParseFunc a -> ParseFunc a -> ParseFunc a
or f g s =
  let
    a = f s
    b = g s
  in
    if isJust a then a else b

(~|~) :: ParseFunc a -> ParseFunc a -> ParseFunc a
(~|~) = Lib.or

cat :: ParseFunc a -> ParseFunc b -> ParseFunc (a,b)
cat f g = u g . f
  where
    u :: ParseFunc b ->  Maybe (a, String) -> Maybe ((a,b), String)
    u g (Just (a, r)) = v a (g r)
    u _ Nothing       = Nothing

    v :: a -> Maybe (b, String) -> Maybe ((a, b), String)
    v a (Just (b, r)) = Just ((a, b), r)
    v _ Nothing       = Nothing

(~~) :: ParseFunc a -> ParseFunc b -> ParseFunc (a,b)
(~~) = cat

rep :: (Maybe Int, Maybe Int) -> ParseFunc a -> ParseFunc [a]
rep range f s = g 0 [] s
  where
    pred (Just min, Nothing) i  = i >= min
    pred (Nothing, Just max) i  = i <= max
    pred (Just min, Just max) i = min <= i && i <= max
    pred (Nothing, Nothing) i   = pred (Just 0, Nothing) i

    doNext (_, Nothing) _  = True
    doNext (_, Just max) i = i <= max

    g n l s = h s $ f s
      where
        h _ (Just (res, r)) = if doNext range n then g (n+1) (l++[res]) r else Nothing
        h s  Nothing = if pred range n then Just (l, s) else Nothing

digit :: ParseFunc Int
digit s = apply (\c -> read [c]) $ is isDigit s

number :: ParseFunc Int
number = apply f . g
  where
    g = rep (Nothing, Just 1) (chr '-') ~~ rep (Just 1, Nothing) (is isDigit)
    f (c1, c2) = read $ c1 ++ c2

expr :: ParseFunc Component
expr = apply f . g
  where
    g = term ~~ rep (Nothing, Nothing) ((chr '+' ~|~ chr '-') ~~ term)
    f (c1, []             ) = c1
    f (c1, ('+', c2):ops)   = f (Op Add c1 c2, ops)
    f (c1, ('-', c2):ops)   = f (Op Sub c1 c2, ops)
    f _                     = error "unreachable"

term :: ParseFunc Component
term = apply f . g
  where
    g = factor ~~ rep (Nothing, Nothing) ((chr '*' ~|~ chr '/') ~~ factor)
    f (c1, []             ) = c1
    f (c1, ('*', c2):ops)   = f (Op Mul c1 c2, ops)
    f (c1, ('/', c2):ops)   = f (Op Div c1 c2, ops)
    f _                     = error "unreachable"

factor :: ParseFunc Component
factor =  num ~|~ paren

num :: ParseFunc Component
num = apply Num . number

paren :: ParseFunc Component
paren = apply f . g
  where
    g = chr '(' ~~ expr ~~ chr ')'
    f ((_, c), _) = Paren c


data Component = Num Int | Paren Component | Op Operation Component Component
data Operation = Add | Sub | Mul | Div

instance Show Component where
  show (Num n)      = "[" ++ show n ++ "]"
  show (Paren c)    = "(" ++ show c ++ ")"
  show (Op o c1 c2) = "[" ++ show c1 ++ show o ++ show c2 ++ "]"

instance Show Operation where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

eval :: Component -> Int
eval (Num n)        = n
eval (Paren c)      = eval c
eval (Op Add c1 c2) = eval c1 + eval c2
eval (Op Sub c1 c2) = eval c1 - eval c2
eval (Op Mul c1 c2) = eval c1 * eval c2
eval (Op Div c1 c2) = divInt (eval c1) (eval c2)
